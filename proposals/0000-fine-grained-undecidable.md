---
author: Alejandro Serrano Mena
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/390).

# Fine-grained pragmas for termination

Currently when one needs to "escape" the termination checker, this is only possible by enabling `UndecidableInstances` and `UndecidableSuperClasses` in a per-module basis. However, this means losing those checks for every single type class, family, or instance defined in that module. This proposal introduces new `%terminating` and `%covered` modifiers to mark a specific definition, instead of the whole module.

## Motivation

When working with type classes and families, sometimes we write definitions which lay outside the [Paterson conditions](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/jfp06.pdf). Roughly speaking, that means that GHC is not able to prove the termination of the resolution / constraint solving process, and thus rejects the definition. One particular case is when the context of an instance mentions a variable more often than its head.

```haskell
data Pair a = Pair (a, a)
instance Show (a, a) => Show (Pair a) where ...
```

As GHC's error message point out, one can escape those checks by using `UndecidableInstances`.

```
<interactive>: error:
    • Variable ‘a’ occurs more often
        in the constraint ‘Show (a, a)’
        than in the instance head ‘Show (Pair a)’
      (Use UndecidableInstances to permit this)
```

However, this is quite a drastic measure. In order to accept _one_ instance, we need to disable the checks for _every_ instance defined in the module. For example, now looping instances are allowed without warning.

```haskell
instance Show (Pair a) => Show (Pair a)
```

### The many faces of `Undecidable`

As it turns out, GHC puts a lot of tweak under the `UndecidableSuperClasses` and `UndecidableInstances` extensions. A few of them are related to termination proper:

- `UndecidableSuperClasses` omits the cycle check for all class declarations in a module. This allows for arbitrary predicates in a superclass constraint.
- `UndecidableInstances` omits the termination check for all local quantified constraints in a module.
- `UndecidableInstances` skips the termination check for class instances.
- `UndecidableInstances` skips the termination check for type family equations (both in open type family instances and closed type family equations).

There are also tweaks to the coverage conditions for coverage / injectivity in type families and functional dependencies.

- `UndecidableInstances` allows a liberalization of the check for injective type family consistency.
- `UndecidableInstances` allows an instance of a class with functional dependencies to use the liberal coverage condition instead of the coverage condition.

## Proposed Change Specification

We introduce new modifiers `%terminating` and `%covered`, which are used _before_ the definition where we want to lift the corresponding restriction.

1. Putting `%terminating` before a class declaration skips its superclass-cycle check.

    ```haskell
    %terminating class C (F a) => C a where ...
    ```

2. Putting `%terminating` before the forall in a quantified constraint skips its termination check. 

    ```haskell
    f :: forall a. (%terminating forall b. C b a => C a a) => a -> a

    -- Note the empty forall to have a target for the modifier
    g :: forall a. (%terminating forall . D a a => C a) => a -> a
    ```

3. Putting `%terminating` before an instance declaration skips its termination check. Putting `%covered` allows for the more liberal coverage condition.

    ```haskell
    %terminating instance Eq (Tree a a) => Eq (Rose a) where ..
    ```

5. Putting `%terminating` before a type instance (or type in an associated type instance) skips the equation's termination check. Putting `%covered` allows for the more liberal injectivity consistency check.

    ```haskell
    %terminating type instance F [a] = G a a

    instance D (Maybe a) where
      %terminating type F (Maybe a) = G a a
    ```

6. Putting `%terminating` before a closed type family declaration skips the termination check for all of its equations. Putting `%covered` allows for the more liberal injectivity consistency check.

    ```haskell
    %terminating type family F a where
      F [a] = G a a
      F Int = Bool
    ```

## Examples

The instance in the previous example would be written as follows:

```haskell
%terminating instance Show (a, a) => Show (Pair a)
```

The [example in the documentation](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-UndecidableSuperClasses) for `UndecidableSuperClasses` would now be written:

```haskell
type family F a :: Constraint
%terminating class F a => C a where
```

## Effect and Interactions

By using this pragmas the programmer does not have to enable the `UndecidableInstances` or `UndecidableSuperClasses` extensions, but rather mark _each_ undecidable definition explicitly. This makes it harder to accept a definition by mistake, but also means that explicit action needs to be taken to upgrade modules to the new syntax.

## Costs and Drawbacks

For modules using a lot of type level computation, there might be a large amount of undecidable instances or type families. In that case marking every single undecidable definition could be hard.

## Unresolved Questions

This proposal overlaps in part with [#374](https://github.com/ghc-proposals/ghc-proposals/pull/374) (_DYSFUNCTIONAL per-instance pragma for selective lifting of the coverage condition_). Note however that this proposal only concerns with enabling the more liberal conditions.

There's another level of flexibility in GHC by allowing to use the more liberal Paterson conditions instead of the restrictive conditions in the Report; one could think of also selectively enabling those. Given that it seems quite plausible that [`GHC2021`](https://github.com/ghc-proposals/ghc-proposals/pull/380) would get `FlexibleInstances` and `FlexibleContexts` in the default mix, I think we should not go that way.

## Implementation Plan

I (Alejandro) volunteer to implement this proposal once / when this proposal is accepted.

