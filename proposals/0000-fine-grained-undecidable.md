---
author: Alejandro Serrano Mena
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/0>).
**After creating the pull request, edit this file again, update the number in
the link, and delete this bold sentence.**

# Fine-grained pragmas for undecidability

Currently when one needs to "escape" the termination checker, this is only possible by enabling `UndecidableInstances` and `UndecidableSuperClasses` in a per-module basis. However, this means losing those checks for every single type class, family, or instance defined in that module. This proposal introduces new `{-# UNDECIDABLE #-}` pragmas to mark a specific definition, instead of the whole module.

## Motivation

Once a developer starts using type level computation, either in the form of type classes or type families, it's easy to end up in a situation in which the instances go outside of the Paterson conditions from `FlexibleInstances` or the coverage conditions from functional dependencies.

```haskell
{-# language DataKinds, KindSignatures #-}
{-# language FunctionalDependencies, FlexibleInstances #-}

data Nat = Z | S Nat
class Sum (n :: Nat) (m :: Nat) (r :: Nat) | n m -> r
instance Sum 'Z m m
instance (Sum n m r) => Sum ('S n) m ('S r)
```

As GHC's error message point out, one can escape those checks by using `UndecidableInstances`.

```
<interactive>: error:
    • Illegal instance declaration for ‘Sum ('S n) m ('S r)’
        The coverage condition fails in class ‘Sum’
          for functional dependency: ‘n m -> r’
        Reason: lhs types ‘'S n’, ‘m’
          do not jointly determine rhs type ‘'S r’
        Un-determined variable: r
        Using UndecidableInstances might help
    • In the instance declaration for ‘Sum ('S n) m ('S r)’
```

However, this is quite a drastic measure. In order to accept _one_ instance, we need to disable the checks for _every_ instance defined in the module. One could imagine a situation in which that leads to an incorrect instance being accepted, which would have been caught if the check was accepted.

```haskell
instance (Sum 'Z m m) => Sum 'Z m m
```

## Proposed Change Specification

Type classes, instances, or type families can be annotated with `{-# UNDECIDABLE #-}` to signal that the termination and coverage checks should be waived for that particular definition.

This proposal subsumes the job of `UndecidableInstances` and `UndecidableSuperClasses`, but makes it more fine-grained. This has been done in the past with the `OverlappingInstances` extension, which was refined into a series of instance pragmas.

## Examples

The instance in the previous example would be written as follows:

```haskell
instance {-# UNDECIDABLE #-} (Sum n m r) => Sum ('S n) m ('S r)
```

The [example in the documentation](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-UndecidableSuperClasses) for `UndecidableSuperClasses` would now be written:

```haskell
type family F a :: Constraint
class {-# UNDECIDABLE #-} F a => C a where
```

## Effect and Interactions

By using this pragmas the programmer does not have to enable the `UndecidableInstances` or `UndecidableSuperClasses` extensions, but rather mark _each_ undecidable definition explicitly. This makes it harder to accept a definition by mistake.

## Costs and Drawbacks

For modules using a lot of type level computation, there might be a large amount of undecidable instances or type families. In that case marking every single undecidable definition could be hard.

## Unresolved Questions

None as of now.

## Implementation Plan

I (Alejandro) volunteer to implement this proposal once / when this proposal is accepted.

