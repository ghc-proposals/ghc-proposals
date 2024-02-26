---
author: Matt Parsons
date-accepted: 2024-02-25
ticket-url: "https://gitlab.haskell.org/ghc/ghc/-/issues/24481"
implemented: ""
---

This proposal was [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/512).

# 1. `NoFieldSelector`s as a datatype and field annotation

GHC recently implemented a feature `NoFieldSelectors` which disables generation of record fields on a per-module basis.
The motivation for this is that `DuplicateRecordFields` and `OverloadedRecordDot` can now be used to access fields on a record in a way that permits sharing field names on records, and even allows polymorphism on the record in question.

I propose that we allow a datatype and field level override of the module default.

## 1.1 Motivation

There are three main motivations for this proposal:

1. Generating records in `TemplateHaskell` without field selectors
2. Ability to suppress selectors for partial fields
3. Consistency with `OverlappingInstances` pragma being specialized

### 1.1.1 Template Haskell Record Generation

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

### 1.1.2 Suppressing selectors for partial fields

Today, many style guides consider partial functions to be a problem.
You can create partial fields on sum types if all constructors do not share the field.
For example, consider:

```haskell
data T 
    = A { x :: Int, y :: Double }
    | B { x :: Int, z :: Char }
```

The field `x` is a total function with type `x :: T -> Int`. However, the selectors `y :: T -> Double` and `z :: T -> Char` are partial, and will fail at runtime if used on the wrong constructor.
This proposal would allow you to disable field selectors for the partial fields, while retaining them for the total field.

With `NoFieldSelectors`, we can avoid this, but at the cost of turning off field selector generation for the entire module, which we might not want.
Being able to control field selector generation on a per-datatype level and per-field level lets you use this trick while keeping other "normal" records the same.

### 1.1.3 Consistency with `OverlappingInstances`

The `OverlappingInstances` pragma is used to allow *all* instances in a module to be overlapping or overlappable.
But this is too coarse grained - you usually want to specify a *single* instance as `{-# OVERLAPPABLE #-}` directly in the instance body.
This proposal is motivated in a similar manner to specializing `OverlappingInstances` to specific places where you want it.

## 1.2 Proposed Change Specification

* Export the following data type from a new module `ghc-experimental:GHC.Modifiers.Experimental`:

  ```haskell
  data FieldSelectorsModifier = NoFieldSelectors | FieldSelectors
  ```

* Allow the use of ``FieldSelectorsModifier`` as a modifier at the level of
  datatype declarations, data constructor declarations, or record field
  declarations.

* For each record field, determine whether to generate a top-level field selector as follows:

  1. consult the value of the ``FieldSelectorModifier`` at the field level if present; otherwise
  2. consult the value of the ``FieldSelectorModifier`` at the data constructor level if present; otherwise
  3. consult the value of the ``FieldSelectorModifier`` at the data declaration level if present; otherwise
  4. check whether the ``FieldSelectors`` language extension is enabled in the current module.

  In case of conflicting modifiers, the first takes priority (ordered by the list above).

## 1.3 Examples

Suppose we had a module that did not enable the `NoFieldSelectors` extension.

```haskell
{-# language FieldSelectors #-}

module Foo where

-- This datatype does not have field selectors generated
%NoFieldSelectors
data Foo = Foo { name :: String
               , age :: Int }
```

Meanwhile, with a `%FieldSelectors` annotation, we would be able to override the module behavior on a per datatype basis.

```haskell
{-# language NoFieldSelectors #-}

module Point where

-- The following declaration has top-level field selector functions
%FieldSelectors
data Point = Point { x :: Int
                   , y :: Int }

-- This does not have field selectors
data MkPoint = MkPoint { x :: Int
                       , y :: Int }
```

The pragma may apply to a single constructor in a sum type.

```haskell
module OnlyOneSelector where

data CoolSumType 
    = HasFieldSelectors { x :: Int, y :: Int }
    | %NoFieldSelectors Doesn'tHaveSelectors { a :: Char, b :: Char }
```

Field level annotations are also possible.

```haskell
{-# language FieldSelectors #-}

module Foo where

-- This datatype does not have the `secretInformation` field selectors generated
-- but the other two are fine
data Foo = Foo { name :: String
               , age :: Int
               , secretInformation %NoFieldSelectors :: [String] }
```

## 1.4 Effect and Interactions

This interacts with the `NoFieldSelectors` and `FieldSelectors` language extension by allowing a per-datatype or per-field override.
Otherwise, this has the same effects and interactions as the original `NoFieldSelectors` extension.

## 1.5 Costs and Drawbacks

GHC already has a facility for deciding on whether or not to generate field selectors for a datatype.
Extending this facility to check for a datatype local override should not be particularly thorny.

Implementing this proposal may spark desire for further datatype modifiers.
The `StrictData` extension comes to mind immediately - being able to write `data X = %Strict X` seems like an obvious application.

## 1.6 Alternatives

A previous version of this proposal used pragma-based syntax, e.g.

```haskell
data 
    {-# FieldSelectors #-}
    Point 
    = Point 
        { x :: Int
        , y :: Int
        }
```

This is a viable alternative, and it was a judgement call to use modifiers instead of pragmas.

## 1.7 Unresolved Questions

Since this is a specialization of an existing language behavior, I don't believe there is anything to be resolved here.


## 1.8 Implementation Plan

The datatype for a record declaration is extended with a field `Maybe FieldSelectorOverride`.
When GHC is deciding whether or not to generate field selectors, this field is checked as a final source of truth.

This change is also propagated to `template-haskell`.

The implementation of this feature will be sponsored by Mercury.

## 1.9 Endorsements

*crickets are screaming in the distance*
