---
author: Daniel Smith
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/324).

# Concrete class dictionaries

Currently typeclass dictionaries are second class citizens in Haskell.
They are passed around explicitly in GHC Core but these concrete dictionaries
are not exposed cleanly to the developer.

This has led to a proliferation of extensions that help you define instances
and convert instances between one another. Even with these extensions the
ergonomics are still significantly worse than regular value-level Haskell.

For this reason I think we should instead focus on making the underlying
dictionaries first class and accessible to developers.

This proposal is to extend the syntax and semantics of classes and instances.
Consider for example the (simplified) `Eq` class:

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
```

1. The class will instead be declared as follows, associating it to a record type `EqDict`,
   with a constructor `Eq` whose fields will be named `(==)` and `(/=)`:

    ```haskell
    class Eq a where
        data dict EqDict a = Eq
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool

    {- Implicitly defines

    data EqDict a = Eq
      { (==) :: a -> a -> Bool
      , (/=) :: a -> a -> Bool
      }

    -}
    ```

2. `EqDict` records are regular values, that can be defined and transformed using
   regular Haskell functions.

    ```haskell
    data T = ...

    myEqT :: EqDict T
    myEqT = ...
    ```

3. Instances of the `Eq` type class can then be defined as `EqDict` values.

    ```haskell
    instance Eq T = myEqT
    ```

This proposal thus separates the act of "attaching" an instance to a type,
from the act of "deriving" instances, i.e., code generation.
Once an instance corresponds to a regular value, "deriving" can be
defined as a regular function producing such values.

## Motivation

1. Concrete class dictionaries unify several features related to deriving instances:

    - `DefaultSignatures`
    - `StandaloneDeriving`
    - `DerivingStrategies`
    - `DerivingVia` and `GeneralizedNewtypeDeriving`

    These extensions provide different ways of constructing instances,
    which we can now express directly with regular functions, as the compiler
    already did for us under the hood when instances only became concrete values
    after elaboration.

    On the very long term, we hope that concrete class dictionaries will in fact
    be the standard way of declaring and understanding instances in Haskell.

2. Since they are functions, new deriving strategies can be defined for any class
   **by users**, rather than the compiler or the provider of the
   class. Previously, `DefaultSignatures` and `DerivingVia` were the closest to
   fill that role, but:

    - `DefaultSignatures` are coupled with the class definition, which means
      the provider of the class must bless an arbitrary implementation as the
      "default" (e.g., for generics, do you use `Data.Data`, `GHC.Generics`,
      generics-sop, or kind-generics?)
    - `DerivingVia` is only applicable to "coercible" classes.

    Previously, one could also expose "default implementations" as simple
    functions to be used in instances, but this doesn't scale to large classes.

We illustrate those motivations below.

To clarify the scope of this proposal, it's worth mentioning that this proposal
only makes dictionaries concrete in order to declare instances at the top-level,
and not in any way that affects the current status quo about coherence.
In particular, concrete dictionaries cannot be used to satisfy constraints
locally; this is out of scope for this proposal.

### Default methods, `DefaultSignatures`

Default methods can be expressed using open recursion and record updates.
For example, consider the `Eq` class:

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  x == y = not (x /= y)

  (/=) :: a -> a -> Bool
  x /= y = not (x == y)

instance Eq T where
  (==) = eqT
```

The default implementations define a (possibly partial) record,
and instance declarations update the relevant fields of that record.

```haskell
class Eq a where
  data dict EqDict a = Eq
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

defaultEq :: Eq a => EqDict a
defaultEq = Eq
  { (==) = \x y -> not (x /= y)  -- using (Eq a)
  , (/=) = \x y -> not (x == y)
  }

instance Eq T = defaultEq
  { (==) = eqT }
```

The extension `DefaultSignatures` can also be emulated using the
same pattern: the only difference is that the "default implementation"
`defaultEq` would be allowed to have other constraints than `Eq a` (which
stands for a recursive reference to the final implementation).
This is technically not as expressive as `DefaultSignatures`, since different
methods could have different constraints, but to the best of our knowledge,
that extra expressiveness is not useful in practice.
Even if it is, it can still be expressed as multiple default implementations.

### `StandaloneDeriving` and `DerivingStrategies`

Making instances first-class makes `StandaloneDeriving` redundant,
since it amounts to defining an instance with a "default" value.

Various `DerivingStrategies` are also accounted for by using different
"default" values.

### Stock deriving

```haskell
deriving stock instance Eq T
```

could be rewritten as

```haskell
instance Eq T = stock
```

where `stock` would be a magic constant whose definition depends on the
structure of `T`, embodying the logic currently found in stock deriving.

The point of this example is to show that the "stock deriving" feature can be
factored into two independent features: concrete class dictionaries (this
proposal), and `stock` magic.
`stock` is not actually part of this proposal to limit its scope.
This proposal lays the groundwork for such simplification in the future.

### `DerivingVia` (and `GeneralizedNewtypeDeriving`)

The extension `DerivingVia` can be viewed as coercions of instances from one
type to another.

```haskell
newtype N a = N a
instance Thing a => Eq (N a) where ...

--

deriving via Eq (N T) instance Eq T
```

Once instances are first-class values, the `DerivingVia` extension becomes
redundant since we can use `coerce` explicitly instead:

```haskell
eqN :: forall a. Thing a => EqDict (N a)
eqN = ...

--

instance Eq T = coerce (eqNT @T)
```

Note: `DerivingVia` is actually a little smarter than a simple `coerce`.
But with this proposal, we can just as well replace `coerce` with another
function, and it doesn't have to be baked into the compiler.

### Ad-hoc deriving

We have shown above how concrete dictionaries offer a simplified story for
defining and deriving instances. Let us now show new idioms enabled by this
extension.

It should be mentioned that the currently existing extension `DerivingVia`
is also intended to solve a similar, overlapping class of problems.
The [`DerivingVia` paper](https://www.kosmikus.org/DerivingVia/deriving-via-paper.pdf)
(Sections 2.3 and 4.3) presents use cases similar to the next two examples.

- However, `DerivingVia` is only applicable to classes compatible with coercion;
  for instance, that rules out `Traversable`, and `Unbox` from the vector library.

- Furthermore, the more advanced use cases of `DerivingVia` require cumbersome
  encodings at the type level of what is fundamentally value-level code.

We shall skip a more detailed comparison, as we hope that the simplicity of our
approach speaks for itself: all of the code to set up deriving is regular
value-level programming.

#### Value-level configuration

Consider the problem of defining `Num` instances using modular arithmetic.
Recall `Num`, which is notable for its numerous methods:

```Haskell
class Num a where
    data dict NumDict a = Num
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    negate :: a -> a
    abs :: a -> a
    signum :: a -> a
    fromInteger :: Integer -> a
```

We can define a function to construct a `Num` dictionary parameterized by the
modulus as follows.

```haskell
modularNum :: Integral a => a -> NumDict a
modularNum m = NumDict
    { (+) = \x y -> (x + y) `mod` m
    , (-) = \x y -> (x - y) `mod` m
    , (*) = \x y -> (x * y) `mod` m
    , negate = \x -> negate x `mod` m
    , abs = \x -> x
    , signum = \x -> signum x
    , fromInteger = \n -> fromInteger n `mod` m
    }
```

That function can now be used with varying moduli.
We do need to specialize that function to a type `a` that already has an `Integral`
instance, but then we only need to `coerce` the resulting dictionaries:

```haskell
newtype Int7  = Int7  Int
newtype Int11 = Int11 Int

instance Num Int7 = coerce (modularNum @Int 7)

newtype Int13 = Int13 Int
    deriving (Num = coerce (modularNum @Int 13))
```

#### Classes as functors

Many classes are functors, whether covariant, contravariant, or invariant (most common).
For example, given an instance `Eq a`, and a function `arg :: t -> a`,
we could define an instance `Eq t` by applying `arg` before comparing the results.

With this proposal, we can literally view the dictionary type associated
with `Eq` as a `Contravariant` functor:

```haskell
instance Contravariant EqDict where ...
```

It will also be necessary to reflect an `Eq` constraint down to an `EqDict`
dictionary (automating this shall be the focus of a future proposal):

```haskell
eqDict :: Eq a => EqDict a
eqDict = Eq { (==) = (==), (/=) = (/=) }
```

Given the fairly general code above, we can now construct instances
using `contramap`. The following `Arg` data type has equality thus defined by
comparing its first component (this type can be found `Data.Semigroup`):

```haskell
data Arg a b = Arg { arg :: a, val :: b }

instance Eq a => Eq (Arg a b) = contramap arg eqDict
```

Another example is from a production codebase. Currently we have
several instances that look like the following:

```Haskell
instance PersistStoreWrite Environment where
    insert = contramapReaderT eBackend . insert
    insert_ = contramapReaderT eBackend . insert_
    insertMany = contramapReaderT eBackend . insertMany
    insertMany_ = contramapReaderT eBackend . insertMany_
    insertEntityMany = contramapReaderT eBackend . insertEntityMany
    insertKey = (contramapReaderT eBackend .) . insertKey
    repsert = (contramapReaderT eBackend .) . repsert
    repsertMany = contramapReaderT eBackend . repsertMany
    replace = (contramapReaderT eBackend .) . replace
    delete = contramapReaderT eBackend . delete
    update = (contramapReaderT eBackend .) . update
    updateGet = (contramapReaderT eBackend .) . updateGet
```

Despite how obviously duplicated the above code is, there is basically nothing
the library (persistent) can do to make it more concise for us. At least not
without resorting to TH or hacky type-level shenanigans.

This proposal would allow persistent to make a `Contravariant` functor out of
`PersistStoreWriteDict`:

```Haskell
persistStoreWriteDict :: PersistStoreWrite b => PersistStoreWriteDict b
persistStoreWriteDict = ...

instance Contravariant PersistStoreWriteDict ...
```

Which would allow us to simply write:

```Haskell
instance PersistStoreWrite Environment = contramap eBackend persistStoreWriteDict
```

#### Generic generic deriving

Class dictionaries are regular data types, in particular they can be the target
of generic metaprogramming (of the ilk of `GHC.Generics`, or the more general
kind-generics).
This allows users to define deriving strategies for whole sets of classes.

For example, several classes can in principle have derived instances for product types
(e.g., `Eq`, `Ord`, `Semigroup`, `Monoid`, `Functor`, `Applicative`, `Alternative`,
`Monad`, `Foldable`, `Traversable`, `NFData`). If we can inspect the structure of
classes, that principle can be metaprogrammed once for many such classes.
This idea exploits the generic structure of both classes and data types, which
is why it deserves to be called "generic generic deriving".

Another example is to derive "lifting" instances of MTL classes
(for methods of the right shape: `ask`, `set`, `throw`, `tell`).


## Proposed Change Specification

The proposal introduces a new extension `ConcreteClassDictionaries` that
enables direct manipulation of typeclass dictionaries.

### Classes as record types

First we add a new form of declaration in classes that allows defining
a type and constructor for the underlying dictionary of a class,
introducing a new keyword `dict`:

```
<cdecl> -> data dict <simpletype> = <conid>
        |  newtype dict <simpletype> = <conid>
```

(For context, see the relevant chapter of the
[Haskell 2010 Report](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html).)

A `data dict` or `newtype dict` declaration gives a type name and a constructor
name to be used to implicitly define a record type whose fields have the same
names, types, and order as the corresponding methods in the original class.
If the types are polymorphic, then `RankNTypes` must also be enabled.

This extra record type declaration is the only change. It is not quite
a regular data declaration: the compiler must keep track of the association
between classes and those record types, which will be exploited in the new form
of `instance` declarations described below.
Ignoring that `data dict` clause, type classes have the same meaning as before.

For example:

```Haskell
class Eq a where
    data dict EqDict a = Eq
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

class Semigroup a where
    newtype dict SemigroupDict a = Semigroup
    (<>) :: a -> a -> a
```

The above examples would create the following types:

```Haskell
data EqDict a = Eq
    { (==) :: a -> a -> Bool
    , (/=) :: a -> a -> Bool
    }

newtype SemigroupDict a = Semigroup
    { (<>) :: a -> a -> a
    }
```

The left-hand side of a `data dict` declaration must differ from the `class`
declaration only in the type constructor name. The variables must be the same.

The field names would collide with the class methods if they were generated,
so we apply `NoFieldSelectors` to these data types.

The `newtype dict` variant is only allowed for single-method classes.
Superclasses are not taken into account for this translation.
For example the following `Ord` class declares an `OrdDict` record with two
fields, which does not include the `Eq` superclass.

```haskell
class Eq a => Ord a where
  data dict OrdDict a = Ord
  compare :: a -> a -> Ordering
  (<=) :: a -> a -> Bool
```

### Record types as classes

In the case of a user already having a data type available and wanting to
create a class on top of it, we define the following alternative syntax:

```Haskell
data Fold f = Fold
    { foldr :: forall a b. (a -> b -> b) -> b -> t a -> b 
    , foldl :: forall a b. (b -> a -> b) -> b -> t a -> b
    }

class Foldable f = fold :: Fold f

class Default a = def :: a
```

This syntax does not need to use `NoFieldSelectors` as no class methods are
created besides the chosen `fold :: Foldable a => Fold a` and `def`.

### Instance declarations

We add a new form of top-level declaration:

```
<topdecl> -> instance [<scontext> =>] <qtycls> <inst> = <exp>
```

The left-hand side is the instance head, of the form `Eq (T a b c)` for
example, possibly under some context `X a b c`.
The right-hand side is an arbitrary expression which must have type
`X a b c => EqDict (T a b c)`, where `EqDict` is the record type associated with
the class `Eq` via the `data dict` clause in the declaration of `Eq`.

That new instance declaration desugars to a regular instance declaration.
For each method `meth`, we pattern-match on the RHS expression `<exp>` to
extract the field corresponding to that method; this makes use of the known
type of `<exp>` as required above:

```haskell
  meth = case <exp> of
    CCon { meth = meth } -> meth
```

For an example of that new syntax:

```haskell
instance X a b c => Eq (T a b c) = (myDict :: EqDict (T a b c))
```

That instance then desugars to the following:

```haskell
instance X a b c => Eq (T a b c) where
  (==) = case (myDict :: CDict (T a b c)) of
    CCon { (==) = (==) } -> (==)
  (/=) = case (myDict :: CDict (T a b c)) of
    CCon { (/=) = (/=) } -> (/=)
```

### Deriving clauses

We similarly extend the `deriving` syntax to allow assignments.

```
<dclass> -> <qtycls> = <aexp>
```

(For reference, see the [Haskell 2010 Report, Section 4.2](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-680004.2).)

For example, if `C` is a class associated with a dictionary type `CDict`,
one can write:

```haskell
data T a = ...
    deriving (C = v)
```

where `C` is a type class and `v` is a variable in scope at the top-level.

Only atomic expressions `<aexp>` (variables, constructors, and literals) are
allowed on the right of `=`: this is so that their types, and in particular
their contexts, are easily available before type inference.

If the atomic expression is a variable `v`, it must be associated with an explicit
top-level type signature of the form `v :: [forall ... .] [x =>] t` for some
optional constraint `x` and some type `t`. Otherwise the program is rejected.

Using the `C = v` example for concreteness, the above requirements
guarantee that the type of `v :: x => t` is known before type inference.
Then the above `deriving` clause desugars to:

```haskell
instance X a => C (T a) = v
```

where the constraint `X a` is obtained from simplifying the constraint
`(x, t ~ CDict (T a))`.

The main benefit of `deriving` clauses over `instance` declarations is that
they do not mention the name of the type to which the instance is associated.
Such `deriving` clauses can easily be copied across many types.
Furthermore, being able to name the dictionary makes it easy to customize
deriving within large projects even when they do not control the definition of
the type class (so `DefaultSignatures`+`DeriveAnyClass` would not be an
applicable alternative).


## Effect and Interactions

With these additions, we have the full power of value level Haskell available
to us when creating and transforming typeclass dictionaries.

There is no longer any need to build up complex and verbose type level
utilities in order to define various desired typeclass instances.

We do not explicitly remove any existing typeclass machinery in this proposal.
With that said this should significantly slow any desire to add new machinery
around typeclass instance manipulation. In future we can revisit existing
extensions to see which, if any, are obviated by this extension.

Associated type and data families will largely be ignored by this proposal.
The concrete dictionary types created will not contain the type or data
families themselves, they will simply reference them as before.


## Costs and Drawbacks

I do not have a deep enough understanding of GHC to be able to precisely
estimate the development and maintenance costs.

With that said the above changes are purely syntax level, and should not have
any effect on core or the underlying runtime.

I do not anticipate significant learnability difficulties. I actually prefer
this approach to the status quo for learnability, as it makes it clear that
classes and instances are basically just extensible partial functions from the
type level to the value level.


## Alternatives

* I originally considered making heavier use making heavier use of data families
for the dict types to avoid having to declare a dict type. I ultimately decided
against it on the basis that you would still need to decide on a constructor
name and thus may as well have full control of both the type name and the
constructor name.

* I also considered providing an `IsClass (c :: Constraint)` class with a
`dict :: (IsClass c, c) => Dict c` but decided to do away with it once I decided
I didn't want to use a data family. Particularly since there would be no way for
users to avoid exporting this conversion without specifying extra rules about
how that would work.

* We can encode some of this in Template Haskell. But as argued in the
  Motivation section, we believe this ought to be a core language feature,
  paving the way for simplifying other features related to deriving instances.
  In particular, the requirement of associating every class to a data type is
  difficult to meet through a third-party TH implementation.


## Unresolved Questions

* Is it worthwhile to adjust/extend the first syntax to also allow for defining
a function that converts directly from an instance to the underlying dict?
Equivalent to `fold :: Foldable f => Fold f` from the second syntax.

* Is it worthwhile to add an optional flag to the second syntax that will
automatically create top level class methods for every field of the underlying
type? This would allow you to avoid writing out the fields/methods twice just
like we can do with the first syntax.

* There is currently a blind spot regarding associated type families.
  Should they also be bundled in a type-level record/tuple?
  Or should we leave them alone. In any case, the `instance ... =` syntax
  should be extended to allow type instance clauses:

    ```haskell
    instance MonoFoldable Blah = myMonoFoldableBlah where
       type Elt Blah = Bleh
    ```

## Implementation Plan

No implementation plan at this time.


## Endorsements
