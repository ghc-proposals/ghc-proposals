---
author: Alejandro Serrano Mena
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/389).

# Better ambiguous variables

Right now the extension `AllowAmbiguousTypes` is too invasive. This document proposes a more fine-grained annotation `{-# AMBIGUOUS #-}` which can be applied to variable in a type signature.

## Motivation

With the advent of `TypeApplications`, it sometimes make sense to leave some of the type variables in a function signature ambiguous. The programmer would later *fix* the type of that ambiguous variable by means of a type application.

Consider the following example based on the one in GHC's User Guide.

```haskell
data Nat = Z | S Nat

class ToInt (n :: Nat) where
  toInt :: Int
instance ToInt 'Z where
  toInt = 0
instance ToInt n => ToInt ('S n) where
  toInt = 1 + toInt @n
```

The designer of this API may expect consumers of the library to use `TypeApplications` to call this method:

```haskell
toInt @('S ('S 'Z))
```

In order to do so, we should do as the error message tells us: enable the `AllowAmbiguousTypes` extension. That fixes the problem, but the drawback is that the ambiguity check is disabled *for the whole module*. It would be better if we could indicate that we only want *that* variable to be ambiguous.

There is a historical precedent for more fine-grained annotations. The former `OverlappingInstances` extension was replaced by `{-# OVERLAPPING #-}` and `{-# OVERLAPPABLE #-}`.

## Proposed Change Specification

Everytime a type variable is explicitly introduced, either in by a `forall` or in a class signature, each type variable may be annotated with `{-# AMBIGUOUS #-}`. This informs the ambiguity checker to disable the check *only* for that variable.

Note that the ambiguity check is *not* dropped for the kind of the variable. That is, there is a difference between these two declarations:

```haskell
forall {-# AMBIGUOUS #-} (n :: k). ...
forall {-# AMBIGUOUS #-} k {-# AMBIGUOUS #-} (n :: k). ...
```

## Examples

```haskell
-- Usage in a type signature
nextInt :: forall {-# AMBIGUOUS #-} n.
           ToInt n => Int
nextInt = toInt @('S n)

-- Usage in a type class declaration
class ToInt ({-# AMBIGUOUS #-} n :: Nat) where
  toInt :: Int
```

## Effect and Interactions

When performing the ambiguity check, GHC would check whether any of the variables to be reported are marked with the `{-# AMBIGUOUS #-}` pragma. If so, they are ignored.

The current error message suggesting to enable `AllowAmbiguousTypes` should be changed to mention this pragma instead of the aforementioned extension.

## Costs and Drawbacks

The cost of this proposal is not high, as it simply makes an existing feature more fine-grained.

## Alternatives

There are different syntactic variations we could play with:

- A completely separate annotation, listing every variable for which the ambiguity check is expected to fail. One nicety is that we would not need to introduce `forall`, and hence we would not need to list every single type variable.

    ```haskell
    {-# AMBIGUOUS nextInt :: n #-}
    nextInt :: ToInt n => Int
    ```

## Unresolved Questions

- Should this proposal be guarded under an extension? Following the lead of `{-# OVERLAPPING #-}` and friends, the answer would be 'no'.
- Should `AllowAmbiguousTypes` be deprecated once this proposal is implemented?

## Implementation Plan

I (Alejandro) volunteer to implement this when / once the proposal is accepted.

