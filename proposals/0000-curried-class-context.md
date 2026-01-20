---
author: Baldur Blöndal
date-accepted: ""
ticket-url: https://gitlab.haskell.org/ghc/ghc/-/issues/20778
implemented: ""
---
This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/666).

# Curried class contexts

Everything in Haskell is curried by standard practice, except strangely, constraints.

They are normally written in uncurried form: `(cls, cls1) => ..`:

```haskell
instance
  ( Simplify NoMeta  rep            simp
  , Simplify NoMeta          rep1            simp1 )
 => Simplify NoMeta (rep :+: rep1) (simp :+: simp1) where
```
This proposal allows writing the following in curried form; without tuple syntax: `cls => cls1 => ..`.

1. Contexts in instance declarations
2. Superclasses in class declarations

```haskell
instance
     Simplify NoMeta rep             simp
  => Simplify NoMeta          rep1            simp1
  => Simplify NoMeta (rep :+: rep1) (simp :+: simp1) where
```

## Motivation

A large motivation is to consistently prefer (and allow) currying. The problem with uncurried constraints lies in formatting.

```haskell
ok :: (Eq a, Num a, Enum a) => a -> Bool
ok x = toEnum x == abs x
```

Let's say the constraints were more numerous, more complex and with regularity that benefits from alignment.
How do we split the signature between lines? I find all of these syntax variants clumsy and difficult to remember. The placement of the parentheses and commas gets in the way of formatting intuitively.

```haskell
ok1 :: (Eq a,
        Num a,
        Enum a) => a -> Bool

ok2 :: ( Eq a
       , Num a
       , Enum a ) => a -> Bool

ok3 :: ( Eq a
       , Num a
       , Enum a
       )
    => a -> Bool
```

Written in curried style makes it syntactically straightforward.

```haskell
ok :: Eq a
   => Num a
   => Enum a
   => a -> Bool
```

It turns out GHC supports currying constraints that appear in types, but not constraints that appear in GADT syntax (#[12087](https://gitlab.haskell.org/ghc/ghc/-/issues/12087)), or class contexts (#[20778](https://gitlab.haskell.org/ghc/ghc/-/issues/20778)).

## Proposed Change Specification

This proposal only changes the parser to accept curried forms. It would not create a new language extension.

The parser should accept currying of superclasses, in class declarations:

```haskell
class Num a => Ord a => Real a where
```

The parser should also accept currying of contexts, in instance declarations:

```haskell
instance Monoid a => Monoid b => Monoid c => Monoid (a, b, c) 
```

## Implementation Plan

I can implement this if it is accepted.
