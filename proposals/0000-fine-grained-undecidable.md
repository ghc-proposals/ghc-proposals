---
author: Alejandro Serrano Mena
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/390).

# Fine-grained pragmas for classes, families, and instances

Currently when one needs to "escape" the termination checker, this is only possible by enabling `UndecidableInstances` and `UndecidableSuperClasses` in a per-module basis. However, this means losing those checks for every single type class, family, or instance defined in that module. This proposal introduces new modifiers to mark a specific definition, instead of the whole module. 

One side effect of only adding those changes would be weird syntax for adding `OVERLAPPING` information in addition to these modifiers.

```haskell
%NoTerminationCheck ; instance {-# OVERLAPS #-} ...
```

For that reason, this proposal also regularizes the syntax, turning those pragmas into modifiers `%Overlapping`, `%Overlappable`, and `%Overlaps`.

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

The modifier syntax is taken from the [updated modifiers proposal](https://github.com/goldfirere/ghc-proposals/blob/clarify-modifiers/proposals/0370-modifiers.rst). Since modifiers ought to be types, we introduce the following declarationg in the `GHC.Modifiers` module.

We define a set of (promoted) data types to describe the different cases that may arise:

```haskell
data LiberalCoverageConfig    = NoLiberalCoverage    | LiberalCoverage
data LiberalInjectivityConfig = NoLiberalInjectivity | LiberalInjectivity

data TerminationCheck = PerformTerminationCheck | NoTerminationCheck

data OverlapConfig = Overlap { overlapping :: Bool, overlappable :: Bool }

-- type synonyms to cover the previously-defined pragmas
type NoOverlap    = Overlap False False
type Overlappable = Overlap False True
type Overlapping  = Overlap True  False
type Overlaps     = Overlap True  True
```

The following grammar rules are updated to allow modifiers to appear:

```diff
+modifiers : {- empty -} | ('%' qtycon)* ';'

cl_decl :
- : 'class' tycl_hdr fds where_cls
+ : modifiers 'class' tycl_hdr fds where_cls

inst_decl
- :           'instance' overlap_pragma inst_type where_inst
+ : modifiers 'instance' overlap_pragma inst_type where_inst
- |           'type' 'instance' ty_fam_inst_eqn
+ | modifiers 'type' 'instance' ty_fam_inst_eqn
- |           data_or_newtype 'instance' capi_ctype datafam_inst_hdr ...
+ | modifiers data_or_newtype 'instance' capi_ctype datafam_inst_hdr ...

ty_decl
- |           'type' 'family' type opt_tyfam_kind_sig ...
+ | modifiers 'type' 'family' type opt_tyfam_kind_sig ...


+forall_modifiers : ('%' qtycon)*

forall_telescope : 
- :                  'forall' tv_bndrs '.'
+ : forall_modifiers 'forall' tv_bndrs '.'
- |                  'forall' tv_bndrs '->'
+ | forall_modifiers 'forall' tv_bndrs '->'
```

The modifiers change the behavior of the checks as follows.

1. **Type class declaration**: the `%NoTerminationCheck` modifier before a class declaration makes it skip its superclass-cycle check.

    ```haskell
    %NoTerminationCheck ; class C (F a) => C a where ...
    ```

2. **Type class instance**: the `%NoTerminationCheck` modifier before an type class instance declaration skips its termination check. The `%LiberalCoverage` modifier allows for the more liberal coverage condition.

    ```haskell
    %NoTerminationCheck ; instance Eq (Tree a a) => Eq (Rose a) where ..
    ```

3. **Quantified constraint**: the `%NoTerminationCheck` modifier before the `forall` in a quantified constraint skips its termination check. 

    ```haskell
    f :: forall a. (%NoTerminationCheck forall b. C b a => C a a) => a -> a

    -- Note the empty forall to have a target for the modifier
    g :: forall a. (%NoTerminationCheck forall. D a a => C a) => a -> a
    ```

4. **Open type family**: the `%NoTerminationCheck` modifier before a type family instance (or type in an associated type instance) skips the equation's termination check. The `%LiberalInjectivity` modifier allows for the more liberal injectivity consistency check.

    ```haskell
    %NoTerminationCheck ; type instance F [a] = G a a

    instance D (Maybe a) where
      %NoTerminationCheck ; type F (Maybe a) = G a a
    ```

    The `%Overlapping`, `%Overlappable`, and `%Overlaps` modifiers keep the same meaning of the corresponding pragmas.

5. **Closed type family**: the `%NoTerminationCheck` modifier before a closed type family declaration skips the termination check for all of its equations. The `%LiberalInjectivity` modifier allows for the more liberal injectivity consistency check.

    ```haskell
    %NoTerminationCheck ; type family F a where
      F [a] = G a a
      F Int = Bool
    ```

In addition, the warning messages in GHC should be rephrased to point to this new constructs instead of the per-module extensions.

## Examples

The instance in the previous example would be written as follows:

```haskell
%NoTerminationCheck
instance Show (a, a) => Show (Pair a)
```

The [example in the documentation](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-UndecidableSuperClasses) for `UndecidableSuperClasses` would now be written:

```haskell
type family F a :: Constraint

%NoTerminationCheck
class F a => C a where
```

## Effect and Interactions

By using this pragmas the programmer does not have to enable the `UndecidableInstances` or `UndecidableSuperClasses` extensions, but rather mark _each_ undecidable definition explicitly. This makes it harder to accept a definition by mistake, but also means that explicit action needs to be taken to upgrade modules to the new syntax.

### Backwards compatibility

This proposal does _not_ deprecate neither the `UndecidableInstances` and `UndecidableSuperClasses` extensions, not the `{-# OVERLAP #-}` set of pragmas for type class instances. Those extensions have been present for a long time, and quite some libraries and reading material would be forced to change if they were removed.

Starting two versions from the first one in which this proposal is implemented, GHC will start emitting a warning, informing the programmer of the new modifiers. If possible, that warning will point to the specific modifiers that must be enabled for the code to keep compiling.

```
warning:
  Using 'UndecidableInstances' is discouraged.
  Use the '%NoTerminationCheck' modifiers in front of
  an instance to disable the termination check.
```

## Costs and Drawbacks

For modules using a lot of type level computation, there might be a large amount of undecidable instances or type families. In that case marking every single undecidable definition could be hard.

## Unresolved Questions

Should we introduce a deprecation plan for `{-# OVERLAPS #-}` and related pragmas? We propose to follow the usual 3-release deprecation cycle:

- implement modifiers in version X,
- introduce the warnings about future deprecation in X + 1,
- fully deprecate the old pragmas in X + 3.

## Implementation Plan

I (Alejandro) volunteer to implement this proposal once / when this proposal is accepted.

