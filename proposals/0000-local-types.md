---
author: David Feuer
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/273).

# Locally declared types and instances

The `reflection` package offers a solution to the "configurations problem",
allowing instance definitions to depend on run-time values. There are some
performance downsides to the implementation (`unsafeCoerce` is required,
leading to a loss of inlining). Furthermore, the whole API is a tad
awkward for end users. I propose local type and instance declarations as an
alternative, friendlier solution with at least as much power.

## Motivation

Detailed motivation for the core idea behind the `reflection` package can be found in
"[Functional Pearl: Implicit Configurations — or, Type Classes Reflect the Values of Types](http://okmij.org/ftp/Haskell/tr-15-04.pdf)",
by Oleg Kiselyov and Chung-chieh Shan. Much of that paper discusses a very
different (and much less efficient) implementation approach. I hereby incorporate
by reference its general discussion and examples. In determining the importance
of supporting this idea, the committee is urged to consider the
[reverse dependencies of the `reflection` package](http://packdeps.haskellers.com/reverse/reflection).

## Proposed Change Specification

Allow type and instance declarations in `let` expressions and `where` clauses.

### Examples

Suppose we want to serialize and deserialize values differently depending on
command-line arguments. Then we could write

```haskell
import System.Environment
import Data.Aeson

data B s = ...
foo :: forall s. ... B s ... -> IO ...

main = do
  args <- getArgs
  let
    data A
    instance FromJSON (B A) where
      parseJSON = ... -- using args
    instance ToJSON (B A) where
      toJSON = ... -- again using args
  foo @A ...
```

The `reflection` package defines

```haskell
class Reifies s a | s -> a where
  -- | Recover a value inside a 'reify' context, given a proxy for its
  -- reified type.
  reflect :: proxy s -> a

-- | Reify a value at the type level, to be recovered with 'reflect'.
reify :: forall a r. a -> (forall (s :: *). Reifies s a => Proxy s -> r) -> r
```

We can implement `reify` with no difficulty whatsoever:

```haskell
reify :: forall a r. a -> (forall (s :: *). Reifies s a => Proxy s -> r) -> r
reify a f = f (Proxy :: Proxy S)
  where
    data S
    instance Reifies S a where
      reflect _ = a
```

### Type declarations

Local `data` and `newtype` declarations have forms similar to global ones,
but their kind signatures may not mention local type variables, as Richard
Eisenberg believes this would be a significant complication). Local type
declarations are local to their scope and may not escape: all terms must
be typed using only global types and local types in scope.

```haskell
-- OK: *values* involving local types may escape.
data Hide = forall a. Hide a
f :: Int -> Hide
f x = Hide (P x)
  where
    newtype P = P Int

-- Prohibited: the type P escapes its scope.
g x = P x
  where
    newtype P = P Int
```

### Type synonyms

Local type synonym definitions would be allowed, working exactly as they
do elsewhere.

### Class instances

It is well known that allowing arbitrary local instances destroys coherence.
One could write

```haskell
import Data.Constraint
-- data Dict c where
--   Dict :: c => Dict c

f :: forall a. (a -> a -> a) -> Dict (Semigroup a)
f append = Dict
  where
    instance Semigroup a where
      (<>) = append
```

at which point matching on two applications of `f` could reveal conflicting
`Semigroup` instances for the same type. We must not allow that!

I believe the key to maintaining coherence simply is to reject *local* instances
in favor of *locally defined* instances. All instances remain (conceptually)
global, but they may involve local types and values. To keep things simple, we
impose the following requirement: the instance head of a locally defined instance
must mention a local type declared in the *same* `let` or `where`. We could
optionally remove this requirement for instances labeled `INCOHERENT`. But it
doesn't seem at all likely that such extensive incoherence will ever be desirable,
so I don't think we want to allow it.

We could, if we wanted, replace the strict "same `let` or `where`" requirement
by a laxer requirement that it would be possible to *float out* the instance
declaration into said `let` or `where`. But this offers no additional power and
complicates the machinery and documentation.

The type checker must still check for overlap, both between globally and locally
defined instances and between locally defined instances. I believe it makes
sense to allow (annotated) overlap between global and local instances, but
probably not between one local instance and another—that would get just as wild
as self-incoherent local instances would.


```haskell
-- Global/local:
class Foo x y where ...
instance Foo A a where ...

g = let
      data B
      -- Overlaps the global instance, requiring an
      -- {-# OVERLAPPING #-} annotation
      instance Foo A B where ...
    in ...


-- Local/local

class Bar x y where ...

h x =
  let
    data B
    instance Bar B a where ...
  in ...
i =
  let
    data C
    -- Impermissibly overlaps the local instance in h.
    instance Bar a C where ...
  in ...
```

How do we deal with locally bound type variables in instance heads?
Richard Eisenberg believes the idea of matching on those is absurd.
However, as John Ericson points out, it could be quite confusing
if those names were shadowed. His example is

```haskell
sortWith :: forall a b. Ord b => (a -> b) -> Blob a -> Blob a
sortWith f = coerce (sort :: Blob (N a) -> Blob (N a)) where
  newtype N c = N {getN :: c}
  instance Eq (N a) where
    (==) = (==) `on` (f . getN)
  instance Ord (N a) where
    compare = compare `on` (f . getN)
```

So I propose, for now, to *forbid* locally bound type variables in instance
heads, with one exception. Consider the definition of `reify` above:

```haskell
reify :: forall a r. a -> (forall (s :: *). Reifies s a => Proxy s -> r) -> r
reify a f = f (Proxy :: Proxy S)
  where
    data S
    instance Reifies S a where
      reflect _ = a
```

The type variable `a` appears in the instance head! We could avoid this if
necessary using an equality constraint:

```haskell
reify :: forall a r. a -> (forall (s :: *). Reifies s a => Proxy s -> r) -> r
reify a f = f (Proxy :: Proxy S)
  where
    data S
    instance a ~ b => Reifies S b where
      reflect _ = a
```

but that's pretty annoying. I believe, therefore, that we should allow a local
type variable to be used in an instance head when it is determined by a
functional dependency or (in a sufficiently obvious way) by an equality
superclass constraint from concrete types. That is, it should be allowed in an
instance head, but should never be matched on.

### Interpretation

How could we interpret locally defined types and instances in terms of more
familiar globally defined ones? I believe we can do this using a sort of
dependent lambda lifting.  Suppose we have

```haskell
p :: forall a b. C a => Int -> a -> F a b
p i c = ...
  where
    data M x = M Int x
    instance D M where
      m = ...
```

We can lift `M` by capturing its local type *and value* context:

```haskell
data M' (a :: Type) (b :: Type) (i :: Int) (c :: a) (x :: Type) = M Int x
instance forall (a :: Type) (b :: Type) (i :: Int) (c :: a).
           C a => D (M' a b i c) where
  m = ...
```

Why do we need the value context? The definition of `m` may use the `i` and/or
`c` arguments, which are not globally available.

### Type family instances

It's important to be able to include local type family instances, because
classes may rely on them. To ensure coherence, we impose the following
requirement: there must be a type on the left-hand side that is defined at
least as near as the most closely bound type on the right-hand side.

```haskell
type family Foo a

-- OK
f :: forall a. a -> ...
f x = ...
  where
    data A
    type instance Foo A = a  -- `a` is bound in an outer scope.

-- OK
f' :: forall a. a -> ...
f' x = ...
  where
    data A
    data B
    type instance Foo A = B  -- `B` is bound in the same scope.

-- No good
g :: a -> ...
g x = ...
  where
    data B
    type instance Foo Int = B -- `B` is more local than `Int`.
```

Similar overlap checks are required as for class instances.

Why don't we require a locally defined type to appear on the left-hand
side? That would cause a problem for associated types in cases where
the associated type only depends on some of the class variables. We
*could* impose that requirement on standalone type family instance
declarations, but in some cases that could force related code to
separate.

How do we deal with locally bound type variables on the left-hand side?
It would make sense to shadow them, but for now I propose to simply
forbid them for consistency with instance heads.

### Data family instances

Data families can be treated the same as type families. Their constructors
will only be available locally.

### Functional dependencies

Functional dependencies should be treated like type families: when applying
each fundep to an instance, there must be a locally defined type on the left
side of the arrow that's defined at least as close as any type on the right
side of the arrow.

### Class, type family, and data family declarations

We can support these, and therefore probably should. Perhaps surprisingly,
they don't seem to add any additional power, but in the case of classes they
may add considerable convenience. Given a local declaration

```haskell
type family F a b ...
```

we can translate this into a local type declaration, a global type
family declaration, and a local type synonym declaration:

```haskell
-- Local
data FT
type F a b = F' FT a b

-- Global
type family F' ctx a b
```

Data families can be treated similarly.

Translating local class declarations is a bit trickier. Given a local
declaration

```haskell
class superclasses => C a b
```

we may need to augment the global class with extra parameters to accommodate
locally bound or locally defined types mentioned in the constraints. For example,

```haskell
f x =
  let
    data T
    class q T => C a
  in
    let
      class C a => D a
    in ...
```

could be translated into a combination of local and global declarations thus:

```haskell
-- Local
f x =
  let
    data T
    data CT
    type C a = C' CT q T a
  in
    let
      data DT
      type D a = D' DT CT q T a
    in ...

-- Global
class q t => C' ctx q t a
class C' cctx q t a => D' dctx ctx q t a
```

### `Typeable`

Locally defined types will *not* be given `Typeable` instances automatically.
I don't believe there is a sensible way to give them `Typeable`
instances at all. In principle, we could offer a multi-parameter version
of `Typeable`. Imagine a type of "run-time contexts" and a type family
that gets the run-time context of a type:

```haskell
data Context
type family ContextOf (t :: k) :: Context
```

Then we could offer

```haskell
class Typeable' (c :: Context) t where
  typeRep' :: TypeRep c t
```

It would only be possible to compare `TypeRep` values relative to the same
context. The main limitation of this scheme is that I don't believe there
would be any sensible way to serialize or deserialize `TypeRep`s relative
to anything but the global context.

## Effect and Interactions

Detail how the proposed change addresses the original problem raised in the
motivation.

Discuss possibly contentious interactions with existing language or compiler
features.


## Costs and Drawbacks

I have no estimate of the development or maintenance costs. I imagine the
change would violate several assumptions baked into GHC's type checker
and specializer.


## Alternatives

"Coherent Explicit Dictionary Application for Haskell", by Winant and Devriese,
offers a way to construct and use class dictionaries explicitly, which also
provides `reflection`-like power. Their proposal is larger and, in some ways,
more powerful—it allows locally defined instances to be used for *global*
types. To accommodate this power, their safety mechanism must be quite complex,
and fragile in the face of redundant constraints. They have not yet found a way
to accommodate associated types or type families involved in constraints, and
they do not seem entirely confident that they will be able to do so. Of course,
nothing prevents the present proposal from living side by side with theirs
should both prove workable.

## Unresolved Questions

Should instances be allowed to float out before type checking? In some cases
that would allow related code to be kept together. But it would become more
complex to explain the rules. It might be reasonable to allow it but also
offer a warning when an instance can be floated.

## Implementation Plan

(Optional) If accepted who will implement the change? Which other resources
and prerequisites are required for implementation?
