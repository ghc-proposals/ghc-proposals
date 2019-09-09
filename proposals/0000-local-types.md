---
author: David Feuer
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/0>).
**After creating the pull request, edit this file again, update the number in
the link, and delete this bold sentence.**

# Local types and instances

The `reflection` package offers a solution to the "configurations problem",
allowing instance definitions to depend on run-time values. There are some
performance downsides to the implementation (`unsafeCoerce` is required,
leading to a loss of inlining). Furthermore, the whole API is a tad
awkward for end users. I propose local type and instance declarations as an
alternative, equally powerful, solution.

## Motivation

Detailed motivation for the core idea behind the `reflection` package can be found in
"[Functional Pearl: Implicit Configurations — or, Type Classes Reflect the Values of Types](http://okmij.org/ftp/Haskell/tr-15-04.pdf)",
by Oleg Kiselyov and Chung-chieh Shan. Much of that paper discusses a very
different (and much less efficient) implementation approach; only the general
discussion and examples are really relevant. In determining the importance
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

### Coherence

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

We can recover coherence by imposing a simple requirement: the instance
head of an instance in a `let` expression or `where` clause must mention a
local type declared in the *same* `let` or `where`. The type checker must
still check for overlap, both between global and local instances and
between local instances.

```haskell
-- Global/local:
class Foo x y where ...
instance Foo A a where ...

g = let
      data B
      -- Overlaps the global instance, requiring an
      -- {-# OVERLAPPING #-} annotation
      instance Foo B where ...
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
    -- Overlaps the local instance in h, requiring that at
    -- least one have overlap pragmas.
    instance Bar a C where ...
  in ...
```

### Type families

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
the associated type only depends on some of the class variables.

How do we deal with locally bound type variables on the left-hand side?

```haskell
-- ??? valid ???
r :: forall a. a -> ...
r x = ...
  where
    type instance Foo a = Int
```

I don't see any *obvious* way to make sense of matching on such variables,
so I don't think we should try too hard initially. On the other hand, I have the
nagging feeling that we might eventually need to find a way to do so. I
therefore propose that locally bound type variables be *prohibited* on the
left-hand side of a local type family instance.

### Data families

Data families can be treated the same as type families. Their constructors
will only be available locally.

### `Typeable`

Locally defined types will *not* be given `Typeable` instances automatically.
It's not at all obvious to me whether there is a good way to give them `Typeable`
instances at all, and if it is, the costs may be high enough to justify
manual `deriving Typeable`.

## Effect and Interactions

Detail how the proposed change addresses the original problem raised in the
motivation.

Discuss possibly contentious interactions with existing language or compiler
features.


## Costs and Drawbacks

Give an estimate on development and maintenance costs. List how this effects
learnability of the language for novice users. Define and list any remaining
drawbacks that cannot be resolved.


## Alternatives

List existing alternatives to your proposed change as they currently exist and
discuss why they are insufficient.


## Unresolved Questions

Explicitly list any remaining issues that remain in the conceptual design and
specification. Be upfront and trust that the community will help. Please do
not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to
the steering committee.


## Implementation Plan

(Optional) If accepted who will implement the change? Which other resources
and prerequisites are required for implementation?

