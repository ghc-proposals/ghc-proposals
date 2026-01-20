---
author: Daniel Smith
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/286).

# Separate HasField into GetField and SetField

There are a variety of useful instances of `getField` that do not have a
corresponding implementation of `setField`.

These include records with invariants that `setField` could violate, like
`TimeOfDay` or `AsciiString` or `Crossword`. As well as various uses of
virtual fields that are completely impossible to set.

Due to dot-notation likely being built on top of `getField`, supporting this
increased flexibility is of significant importance.


## Motivation

There are two main use cases for having a `getField` without a corresponding
`setField`: read-only physical fields, and virtual fields. Both of these use
cases are compelling and worth supporting.

Read only physical fields:

```haskell
newtype AsciiString = AsciiString
    { contents :: Text
    }

data TimeOfDay = TimeOfDay
    { hour :: Int
    , minute :: Int
    , second :: Pico
    }

data Crossword = Crossword
    { ... variety of data structures that need to be in sync ...
    }

newtype AscList a = AscList
    { list :: [a]
    }
```

The above types all benefit from read-only physical fields because they enforce
invariants on the underlying fields, but at the same time they are not true
abstract types, as it is not problematic for users to be able to see the values
of all these fields.

Virtual fields:

```haskell
newtype TimeOfDay = TimeOfDay
    { seconds :: Pico
    -- hour = seconds `div` 3600
    -- minute = seconds `div` 60 `mod` 60
    -- second = seconds `mod` 60
    }

newtype MyFlags = MyFlags
    { flags :: Word8
    -- foo = testBit flags 0
    -- bar = testBit flags 1
    -- baz = testBit flags 0 || testBit flags 1
    }
```

The above types all benefit from read-only virtual fields because they store
the internal data in a compact format, but want to be have the same ergonomics
as types that store it in fully expanded form. They also have invariants so we
specifically need only a virtual GetField and not a SetField.

Virtual fields (but more magical):

```haskell
data FieldLenses r (f :: Type -> Type) = FieldLenses

l :: FieldLenses r f
l = FieldLenses

type instance FieldType x (FieldLenses r f) = (FieldType x r -> f (FieldType x r)) -> r -> f r

instance (SetField x r, Functor f) => GetField x (FieldLenses r f) where
    getField _ = lens (getField @x) (flip $ setField @x)
```

The above when combined with the dot-syntax would allow for you to write
`foo & l.name .~ 5` instead of `foo & l @"name" .~ 5` or `foo & l #name .~ 5`.

More generally it allows you to define potentially infinite and fairly magical
"records" which you can use clean and intuitive dot-syntax to work with.

Obviously the above has absolutely no chance of supporting a reasonable
equivalent `SetField`.

## Proposed Change Specification

`GHC.Records.HasField` would be adjusted from it's current planned
specification:

```haskell
class HasField x r a | x r -> a where
    hasField :: r -> (a -> r, a)
```

to become

```haskell
type family FieldType (x :: k) r

class GetField (x :: k) r where
    getField :: r -> FieldType x r

class GetField x r => SetField (x :: k) r where
    setField :: FieldType x r -> r -> r
    setField = updateField @x . const

    updateField :: (FieldType x r -> FieldType x r) -> r -> r
    updateField f r = let (g, a) = hasField @x r in g (f a)

    hasField :: r -> (FieldType x r -> r, FieldType x r)
    hasField r = (\a -> setField @x a r, getField @x r)

    {-# MINIMAL setField | updateField | hasField #-}
```


## Effect and Interactions

This change would allow for the definition of various `GetField` instances
that were mentioned above as desirable. This would then enable those instances
to be utilized by dot-notation.

This would not be backwards compatible with the existing implementation,
but neither is the planned implementation. Unlike the planned implementation
it would be a very mechanical rename to fix, as opposed to a potentially
impossible definition of `hasField`.

Currently with this proposal alone you will have to manually define any desired
GetField-only instances, as the derived instances will include an associated
SetField instance.


## Costs and Drawbacks

The drawbacks seem minimal compared to the planned implementation. You can just
treat `SetField` as the proposed `HasField` and everything will work the same.

There is slightly more complexity due to having two separate classes rather
than just one.


## Alternatives

Separate out further into "get-only", "set-only", "get-0-or-more",
"get-and-set" etc. instead of only "get-only" and "get-and-set".

The above makes a lot of sense for libraries like `lens` that deal with sums and
clever multi-target lenses like `traverse`.

However I do not believe it is particularly valuable for these `GHC.Records`
classes, since they are focused on "fields" and "records" and automatically
deriving them, and not on the general concept of lenses.

Whenever you have a record that contains a `Maybe b` or a `[b]`, you will most
likely want (and have derived for you) the single-target field from `a` to
`Maybe b`/`[b]`, and not the multi/partial-target field from `a` to `b`. The
latter you can always define yourself as a top level lens or as an instance of
some other class.


## Unresolved Questions


## Implementation Plan

(Optional) If accepted who will implement the change? Which other resources
and prerequisites are required for implementation?

