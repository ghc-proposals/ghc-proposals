---
author: Matt Parsons
date-accepted: ""
ticket-url: "https://github.com/ghc-proposals/ghc-proposals/pull/512"
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/512>).

# `NoFieldSelector`s as a datatype annotation

GHC recently implemented a feature `NoFieldSelectors` which disables generation of record fields on a per-module basis.
The motivation for this is that `DuplicateRecordFields` and `OverloadedRecordDot` can now be used to access fields on a record in a way that permits sharing field names on records, and even allows polymorphism on the record in question.

I propose that we allow `NoFieldSelectors` as datatype annotation.

## Motivation

The database library `esqueleto` recently implemented support for `OverloadedRecordDot` on the `SqlExpr (Entity rec)` type, which represents a row from a database table.
The syntax for projecting a field in the old and new styles is this:

```haskell
-- Vanilla
foo ^. FooFieldName
maybeFoo ?. FooFieldName

-- OverloadedLabels
foo ^. #fieldName
maybeFoo ?. #fieldName

-- OverloadedRecordDot
foo.fieldName
maybeFoo.fieldName

-- note that we've unified the access for nullable rows and non-nullable rows!
```

`esqueleto` users have long been requesting a feature for the ability to select Haskell records from a type, but this has a problem: we need a means of constructing the record in SQL-land, and then translating those SQL expressions into Haskell expressions.

An approach I'd like to implement uses `TemplateHaskell` to declare a `Sql*` variant of a record, with the appropriate instances for `SqlSelect`.
This record would share the same field names as the source record, so that field access and construction is easy.
The generated code would look like this:

```haskell
data Foo = Foo 
    { name :: Text
    , age :: Int 
    }

deriveSqlRecord ''Foo

data SqlFoo = SqlFoo 
    { name :: SqlExpr (Value Text)
    , age :: SqlExpr (Value Int) 
    }
```

By sharing the same field names, we can very easily construct these records and access their fields without worrying about learning some new API for it.
However, this API requires that `NoFieldSelectors` be enabled for the module in question in order to avoid generating the function accessors.
Since `TemplateHaskell` cannot enable or disable language extensions for the generated code, I have to report this as a warning to the end user.

With per-datatype `NoFieldSelectors`, I'd be able to set this annotation in `TemplateHaskell`, and the end user would not have to worry about it.

While this is the use case that sparked this proposal, I can also see it being useful for other "auxiliary" datatypes, that are related to a main datatype and share some or all field names and types.
Consider the [`SqlBackend` type in `persistent`](https://www.stackage.org/haddock/lts-19.9/persistent-2.13.3.5/Database-Persist-Sql.html#t:SqlBackend).
Instead of exposing the constructor (and forcing a breaking change with each field addition), I expose a separate 'constructor type' called [`MkSqlBackendArgs`](https://www.stackage.org/haddock/lts-19.9/persistent-2.13.3.5/Database-Persist-SqlBackend-Internal-MkSqlBackend.html#t:MkSqlBackendArgs).
This type shares the same field names as `SqlBackend`, but the intent is not ever to *use* them as a function but to use them as constructor labels.
The types are currently defined in separate modules to avoid duplicate field warnings and issues, but if it were possible to do `NoFieldSelectors` on a single type, then I could easily keep them together.

Another motivation is consistency.
The `OverlappingInstances` pragma is used to allow *all* instances in a module to be overlapping or overlappable.
But this is too coarse grained - you usually want to specify a *single* instance as `{-# OVERLAPPABLE #-}` directly in the instance body.

## Proposed Change Specification

Introduce an annotation `{-# NoFieldSelectors #-}` that can appear on a datatype.

This works exactly the same as if `NoFieldSelectors` were present for the module it is defined in, without interacting with the other datatypes.

For completeness and consistency, allowing `{-# FieldSelectors #-}` as a datatype annotation also make sense.

## Examples

Suppose we had a module that did not enable the `NoFieldSelectors` extension.

```haskell
{-# language FieldSelectors #-}

module Foo where

-- This datatype does not have field selectors generated
data 
    {-# NoFieldSelectors #-} 
    Foo 
    = Foo 
        { name :: String
        , age :: Int
        }
```

Meanwhile, with a `{-# FieldSelectors #-}` annotation, we would be able to override the module behavior on a per datatype basis.

```haskell
{-# language NoFieldSelectors #-}

module Point where

-- The following declaration has top-level field selector functions
data 
    {-# FieldSelectors #-}
    Point 
    = Point 
        { x :: Int
        , y :: Int
        }

-- This does not have field selectors
data MkPoint = MkPoint
    { x :: Int
    , y :: Int
    }
```

## Effect and Interactions

This interacts with the `NoFieldSelectors` and `FieldSelectors` language extension by allowing a per-datatype override.
Otherwise, this has the same effects and interactions as the original `NoFieldSelectors` extension.

Syntactically, a common trick is to `grep` for `^(type|newtype|data) TypeName` to find the declaration for a given type name. Putting an annotation between the `data` and type name would break this sort of search.

## Costs and Drawbacks

GHC already has a facility for deciding on whether or not to generate field selectors for a datatype.
Extending this facility to check for a datatype local override should not be particularly thorny.

An additional annotation is more syntax to learn.
The `{-# FieldSelectors #-}` syntax is a bit ugly, though it is consistent with other annotations.

Implementing this proposal may spark desire for further datatype annotations.
The `StrictData` extension comes to mind immediately - being able to write `data X = X {-# Strict #-}` seems like an obvious application.

If the alternative for *field-level* annotations is appealing, then we will need to consider what syntax we may want for this.
`StrictData` annotations on a per-field basis are done with `BangPatterns` (and a laziness annotation can be provided with `~` as a prefix operator).
Using a `{-# Blah #-}` comment as a field annotation introduces a second means of annotating a field, and in particular, it is somewhat noisy.
If we're able to annotate fields, we may also want the ability to say that some fields are "private" or "public" much like other object and record systems permit.
Finally, field annotations could potentially be used in `DerivingVia` - see [this Reddit post by `Iceland_jack`](https://www.reddit.com/r/haskell/comments/pbq9rn/via_fields_finer_granularity_in_deriving/).

## Alternatives

There is currently no way to get record selectors for record construction and record pattern matching without record syntax.

For `OverloadedRecordDot` field access, synthetic `HasField` instances can be defined.

Another potential extension of this design is to apply it to *fields* as well as a type.
The `StrictData` analogy is already implemented here with `BangPatterns`.

```haskell
-- This datatpye generates the `age` field selector, but not the `name`
-- selector.
data Foo = Foo
    { {-# NoFieldSelector #-} name :: String 
    , age :: Int
    }
```

```haskell
{-# language NoFieldSelectors #-}

module X where

data Point = Point
    { x :: Int
    , y :: Int
    , {-# FieldSelector #-} z :: Int 
    }
```

Syntactically, the `{-# Blah #-}` style is lifted from `{-# Overlapping #-}`.
However, it's pretty ugly and verbose.
A further extension could introduce new keywords that are only used in record declaration syntax with the same meaning, allowing you to write, eg,

```haskell
data Point = Point
    { x :: strict selector Int
    , y :: lazy field Int
    }
```

## Unresolved Questions

Since this is a specialization of an existing language behavior, I don't believe there is anything to be resolved here.


## Implementation Plan

The datatype for a record declaration is extended with a field `Maybe FieldSelectorOverride`.
When GHC is deciding whether or not to generate field selectors, this field is checked as a final source of truth.

This change is also propagated to `template-haskell`.

## Endorsements

*crickets are screaming in the distance*
