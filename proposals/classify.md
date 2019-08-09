---
author: Iceland_jack
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/261).

# Classify instances by deriving strategy / declaration

The provenance of an `instance` tells me a lot about it. Whether it was written by hand or derived using one of the avaiable deriving strategies: `stock`, `anyclass`, `newtype`, `via`.

## Motivation

I propose adding an option to the `:info` command or creating a new command called `:classify` (this is not a pun) that breaks instances down by how they came to be:

```haskell
newtype Point f a = P (f a)
 deriving
 stock (Eq, Show)

 deriving
 anyclass X

 deriving
 newtype Functor

 deriving Applicative
 via Compose Identity (Co Point)

 deriving Monad
 via Co Point

instance Representable f => Representable (Point f) where
 type Rep (Point f) = Rep f ..
```

```ghci
>> :classify Point
newtype:
  instance Functor f => Functor (Point f) 

via `Co Point`:
  instance Representable f => Monad (Point f)

via `Compose Identity (Co Point)`:
  instance Representable f => Applicative (Point f)

anyclass:
  instance X (Point f a)

stock:
  instance Show (f a) => Show (Point f a)
  instance Eq   (f a) => Eq (Point f a)

standalone:
  instance Representable f => Representable (Point f)
  type instance Rep (Point f) = Rep f
```

I imagine IDEs could easily take the output and make it collapsable, for example if a 90% of instances are derived using `newtype` and 10% are written by hand (`standalone`) I will focus on the handwritten instances since the majority of them are inhereted from the representing type.

## Proposed Change Specification
Add new ghci command `:classify` that gives the same output as `:info` (warts and all) with grouping depending on how it was defined.

## Examples
See above.

## Effect and Interactions
..

## Costs and Drawbacks
..

## Alternatives
..


## Unresolved Questions
..

## Implementation Plan
..

