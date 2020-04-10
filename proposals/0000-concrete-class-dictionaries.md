---
author: Daniel Smith
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/324).

# Concrete class dictionaries

Currently typeclass dictionaries are second class citizens in Haskell.
They are passed around explicitly in core but these concrete dictionaries
are not exposed cleanly to the developer.

This has led to a proliferation of extensions that help you define instances
and convert instances between one another. Even with these extensions the
ergonomics are still significantly worse than regular value-level Haskell.

For this reason I think we should instead focus on making the underlying
dictionaries first class and accessible to developers.


## Motivation

There has been a large proliferation of typeclass related extensions, and yet
the ergonomics for manipulating typeclass dictionaries is still poor.

Something fundamentally quite simple such as defining a series of newtype
wrappers over `Int` for modular arithmetic, with each using a different modulo
for `Num`, is very painful.

The specifical example that motivated me to write this proposal is the one
given [here](https://mail.haskell.org/pipermail/haskell-cafe/2020-April/132083.html)
and looks as follows:

```Haskell
{-# LANGUAGE DerivingVia, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

import Data.Function (on)
import Data.Coerce (coerce)

data Foo = Foo deriving Eq via (Foo `InjectedInto` Bar)
data Bar = Bar deriving Eq

foo2bar :: Foo -> Bar
foo2bar Foo = Bar

newtype InjectedInto a b = InjectedInto a

class Injective a b where
    -- Law: to x = to y => x = y
    to :: a -> b

instance Injective Foo Bar where
    to = foo2bar

instance (Eq b, Injective a b) => Eq (a `InjectedInto` b) where
    (==) = (==) `on` (to :: a -> b) . coerce
```

With some follow up discussion about making libraries of various type level
utilities to help with the above.

Everything in the above example is fundamentally value-level code. Writing it
at the type level adds a lot of complexity and verbosity for no real benefit.

I would expect something closer to the following:

```Haskell
{-# LANGUAGE ConcreteClassDictionaries #-}

import Data.Function (on)
import Data.Coerce (coerce)

data Foo = Foo deriving (Eq = eqOn foo2Bar)
data Bar = Bar deriving Eq

foo2bar :: Foo -> Bar
foo2bar Foo = Bar

eqOn :: ???
eqOn = ???
```


## Proposed Change Specification

The proposal introduces a new extension `ConcreteClassDictionaries` that
enables direct manipulation of typeclass dictionaries.

First we add a new syntactic construct that allows for defining a type and
constructor for the underlying dictionary of a class:

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

The field names would collide with the class methods if they were generated,
so we apply `NoFieldSelectors` to these data types.

In the case of a user already having a data type available and wanting to
create a class over the top of it, we define the following alternative syntax:

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

We now provide syntax for defining new instances directly from an appropriate
concrete dictionary:

```Haskell
fromEq :: (a -> a -> Bool) -> EqDict a
fromEq eq = EqDict
    { (==) = eq
    , (/=) = \a b -> not (eq a b)
    }

data Foo = Foo

instance Eq Foo = fromEq $ \_ _ -> True

instance Default Foo = Foo

data Bar = Bar
    deriving (Eq = eqOn foo2Bar, Default = Bar)

eqOn :: Eq b => (a -> b) -> EqDict a
eqOn f = fromEq ((==) `on` f)

foo2Bar :: Foo -> Bar
foo2Bar Foo = Bar
```


## Examples

In the proposed changes I already covered the motivating equality example.
So I will give the `Num` example as well:

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

modularNum :: Num a => a -> NumDict a
modularNum m = NumDict
    { (+) = \x y -> (x + y) `mod` m
    , (-) = \x y -> (x - y) `mod` m
    , (*) = \x y -> (x * y) `mod` m
    , negate = \x -> negate x `mod` m
    , abs = \x -> x
    , signum = \x -> signum x
    }

newtype Int7 = Int7 Int

instance Num Int7 = coerce (modularNum @Int 7)

newtype Int13 = Int13 Int
    deriving (Num = coerce (modularNum @Int 13))
```

Another example I will use is from a production codebase. Currently we have
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
without resorting to TH or hacky type-level shenangians like in the original
motivating example.

This would allow persistent to define:

```Haskell
persistStoreWriteDict :: PersistStoreWrite b => PersistStoreWriteDict b
persistStoreWriteDict = ...

instance Contravariant PersistStoreWriteDict ...
```

Which would allow us to simply write:

```Haskell
instance PersistStoreWrite Environment = contramap eBackend persistStoreWriteDict
```

## Effect and Interactions

With these additions, we have the full power of value level Haskell available
to us when creating and transforming typeclass dictionaries.

There is no longer any need to build up complex and verbose type level
utilities in order to define various desired typeclass instances.

We do not explicitly remove any existing typeclass machinary in this proposal.
With that said this should significantly slow any desire to add new machinary
around typeclass instance manipulation. In future we can revisit existing
extensions to see which, if any, are obviated by this extension.

Associated type and data families will largely be ignored by this proposal.
The concrete dictionary types created will not contain the type or data
families themselves, they will simply reference them as before.


## Costs and Drawbacks

I do not have a deep enough understanding of GHC to be able to precisely
estimate the development and maintainence costs.

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


## Unresolved Questions

* Is it worthwhile to adjust/extend the first syntax to also allow for defining
a function that converts directly from an instance to the underlying dict?
Equivalent to `fold :: Foldable f => Fold f` from the second syntax.

* Is it worthwhile to add an optional flag to the second syntax that will
automatically create top level class methods for every field of the underlying
type? This would allow you to avoid writing out the fields/methods twice just
like we can do with the first syntax.


## Implementation Plan

No implementation plan at this time.


## Endorsements
