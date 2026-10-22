---
author: Iceland_jack
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/363).
# Standalone StandaloneKindSignatures

Many papers like [A Monadic Framework for Delimited Continuations](https://legacy.cs.indiana.edu/~dyb/pubs/monadicDC.pdf) introduce a library like this

```haskell
data CC a -- Abstract
instance Monad CC

data Prompt a -- Abstract
data SubCont a b -- Abstract

type Obs = ... -- Arbitrary but fixed
```

Given that we have `StandaloneKindSignatures` I think we should be able to write (standalone) standalone kind signatures. 
Currently it gives an error "The standalone kind signature for ‘..’ lacks an accompanying binding":

```haskell
type CC      :: Type -> Type
type Prompt  :: Type -> Type
type SubCont :: Type -> Type -> Type
type Obs     :: Type
```

It ought to work for type classes as well

```haskell
type MyMonad :: (Type -> Type) -> Constraint
```

and get translated into empty data and class declarations

```haskell
data  CC      a
data  Prompt  a
data  SubCont a b
data  Obs     a
class MyMonad m
```

## Motivation

This is useful for development (but also in high-level communication of a library's api, like in the example listed above).
I can start defining my library by its kinds before focusing on definitions and so I can incrementally add to a definition.

## Proposed Change Specification

Allow kind signatures without a declaration, insert "stub" declarations matching the type kind signature.
