---
author: Artyom Kuznetsov
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/270).

# Extend Term-Level Lookup Rules

This proposal introduces `-Wpuns` and `-Wpun-bindings`, deprecates the `'`
syntax of `-XDataKinds`, deprecates the `''` syntax of `-XTemplateHaskell`,
introduces namespace-qualified imports, defines alphanumeric names for built-in
types, and enables referencing type-level entities in a term-level context.

The interplay between these changes leads to a coherent design for namespace
resolution rules in GHC, cleaning up the existing situation and paving the
road for future extensions.

## Motivation

### Background

Haskell has two namespaces: one for types (the type namespace), and one for
terms (the data namespace).

This separation allows us to define data constructors and type constructors
whose names coincide:

```haskell
data T = T
```

The use of identical names for type-level and term-level entities is called
*punning*.

At use sites, GHC infers which `T` is referred to from context:

```haskell
t :: T  -- type-level T
t = T   -- term-level T
```

Haskell makes heavy use of punning in its built-in syntax and common types:

```haskell
data [] a = [] | a : [a]
data (a, b) = (a, b)
data () = ()
data Proxy a = Proxy
newtype Identity a = Identity a
newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}
newtype ExceptT e m a = ExceptT (m (Either e a))
```

However, as Haskell's type system evolves, the distinction between types and
terms becomes blurry. For example, the `-XDataKinds` extension introduces the
`'` syntax to select entities from the data namespace in a type-level context:

```haskell
r :: Rec Const '[ 'T ]   -- vinyl records
r = Const :& RNil
```

Note that we had to qualify both the list syntax and the `T` data constructor
with a tick.

### Problem Statement

Punning becomes a problem which we'll call *the namespace problem*:

* With `-XDataKinds`, namespace resolution is context-dependent, ambiguous, and
  hard to reason about.

* The `'` syntax, if used everywhere, is verbose and conflicts with the name
  quotation syntax of `-XTemplateHaskell`.

* There's no term-level counterpart of the `'` syntax to select names from the
  type namespace in a term-level context.

This issue also applies to term variables and type variables. Currently, the
following code is valid:

```haskell
id :: forall a. a -> a
id a =     (a :: a)
      -- term :: type
```

On the right-hand side of `=`, `a` could refer either to a value or to a type,
depending on context.

In old-school Haskell where terms and types were entirely distinct, punning was
sometimes convenient. Although even then it was a source of beginner confusion:
`[a]` can mean either `a : []` or `[] a` depending on context, and it takes
time to learn to tell them apart at a glance.

With new extensions, especially related to dependent types, the issue is worse
than mere inconvenience. For example, visible dependent quantification
([GHC Proposal #281: "Visible 'forall' in types of terms" proposal](https://github.com/ghc-proposals/ghc-proposals/pull/281/)), a
useful step towards dependent functions (pi-types), requires a solution to the
namespace problem.

As a real-world example, consider `sizeOf` in `Foreign.Storable`:

```haskell
sizeOf :: Storable a => a -> Int
```

This function takes a value of type `a` as an argument whereas actually it
doesn't need this value, it only cares about its type.

Currently, to use it we have to provide it with a value, and the most
convenient way to do it is to use `undefined`:

```
sizeOf (undefined :: MyType)
```

Ideally, we want to be able to write this instead (using visible dependent
quantification):

```haskell
sizeOf :: forall a -> Storable a => Int

x = sizeOf MyType
```

We could also refer to the `forall`-bound type variables in terms:

```haskell
sizeOfArray :: forall a. Storable a => Array a -> Int
sizeOfArray xs = length xs * sizeOf a

y = sizeOfArray [True, False, False]
```

Of course today we have the option of using the `@` syntax to do that:

```haskell
sizeOf :: forall a. Storable a => Int

x = sizeOf @MyType
```

But this approach uses an implicit, ambiguous type variable. Supplying it is
not enforced by GHC, and if the user forgets to specify it, the compiler is
doomed to fail with a bad error message.

### Solution Overview

These problems could be solved by changing the identifier lookup rules in a
non-intrusive way: whenever we look up an identifier at the term level, we can
first search for it in the data namespace, and if it wasn't found there, we
could search in other namespaces as a fallback.

Compare this to today's `-XDataKinds` behavior:

* if a type constructor is not in scope, look up a data constructor (`-XDataKinds` today)
* if a data constructor is not in scope, look up a type constructor (proposed extension)

Unlike `-XDataKinds`, we do *not* introduce a term-level counterpart for the
`'` syntax. Reusing `'` for this purpose isn't possible due to a conflict with
`-XTemplateHaskell` syntax, and introducing yet another syntactic marker (e.g.
`^`) would further increase the syntactic complexity of Haskell.

Instead, we propose to deprecate the `'` syntax of `-XDataKinds` and introduce
namespace-qualified imports and module aliases (guarded behind `-XExplicitNamespaces`
and a new extension `-XModuleAliases`) for compatibility with modules that use punning:

```
module M as ModuleAlias, type as MT, type as MD where

import Data.Proxy type qualified as T   -- import only the type namespace
import Data.Proxy data qualified as D   -- import only the data namespace

data T = T

f :: T.Proxy MT.T
f = D.Proxy
```

To avoid hard to grasp, context-dependent code, we introduce `-Wpuns` and
`-Wpun-bindings` to encourage and enforce a programming style where identifiers
do not clash.

With `-Wpuns` and namespace-qualified imports, `'` can be used exclusively for
`-XTemplateHaskell` name quotation, and `''` is not needed anymore.

Since `[]`, `()`, `(,)`, and so on, type constructors use punning, they will no
longer be always visible in scope and we introduce `Data.BuiltInTypes` module
that exports them and is imported by default (this behavior can be overriden
with `-XNoImplicitBuiltInTypes`).

This leaves us with `(~)` which we also propose to change to a type operator
and add to `Data.BuiltInTypes`. The main motivation to do it as part of this
proposal is to avoid breakage in the future. Consider this example:

```
{-# LANGUAGE NoImplicitBuiltInTypes #-}
id :: a ~ b => a -> b
id x = x
```

Should this code compile?

If we don't add `(~)` to `Data.BuiltInTypes` in this proposal, the answer is initially "yes" and "no" afterwards.

If we handle `(~)` now, the answer is "no" from the get go.

## Proposed Change Specification

1. The name lookup rules are extended: when looking up a term-level identifier
   fails, look for a type-level identifier as fallback.

2. `-XModuleAliases` extension is introduced that adds a syntax for module aliasing:
   ```
   module  -> module modid [exports] where body
            | body
   ```
   is changed to
   ```
   module  -> module modid [aliases] [exports] where body
            | body

   aliases -> alias1, ..., aliasN

   alias   -> as modid
   ```

3. To disambiguate the namespaces of identifiers from modules that use punning,
   the `-XExplicitNamespaces` extension is extended with new syntax:

   ```
   impdecl   -> import [qualified] modid [as modid] [impspec]
   ```
   is changed to
   ```
   impdecl   -> import [qualified] modid [namespace] [as modid] [impspec]

   namespace -> data
              | type
   ```
   and module in alias syntax from `-XModuleAliases`
   ```
   alias  -> as modid
   ```
   is changed to
   ```
   alias  -> as modid
           | as type modid
           | as data modid
   ```
    1. With `data` specified in the import or alias, only identifiers belonging to the data namespace will be brought into the scope.

    2. With `type` specified in the import or alias, only identifiers belonging to the type namespace will be brought into the scope.

4. Add a new module, `Data.BuiltInTypes`, imported by default unless the user
  passes `-XNoImplicitBuiltInTypes` to the compiler.

5. In `GHC.Types`, the `[]` type constructor is renamed to `List`:

   ```haskell
   data List a = [] | a : List a
   ```

   The new type constructor is re-exported from `Data.List`.

   In `Data.BuiltInTypes`, introduce a backwards-compatibility type synonym:

   ```
   type [] = List
   ```

   Users are also able to have their own declarations of `[]`.

6. In `GHC.Tuple`, rename the tuple type constructors to `Tuple<N>` and add a
   new type family, `Tuple`:

   ```haskell
   data Tuple0 = ()
   data Tuple1 a = MkTuple1 a
   data Tuple2 a b = (a, b)
   data Tuple3 a b c = (a, b, c)
   data Tuple4 a b c d = (a, b, c, d)
   {- ... -}

   type Unit = Tuple0

   type family Tuple xs where
     Tuple [] = Tuple0
     Tuple [a] = Tuple1 a
     Tuple [a,b] = Tuple2 a b
     Tuple [a,b,c] = Tuple3 a b c
     {- ... -}
   ```

   Existing `Unit` in GHC.Tuple is renamed to `Tuple1` and `Unit` is now
   aliased to the unit type instead.

   The new type constructors and the type family are re-exported from `Data.Tuple`.

   In `Data.BuiltInTypes`, introduce backwards-compatibility type synonyms:

   ```haskell
   type () = Tuple0
   type (,) = Tuple2
   type (,,) = Tuple3
   type (,,,) = Tuple4
   {- ... -}
   ```

   Just as for lists, users are able to have their own declarations for `()`,
   `(,)` and so on.

7. Change `a ~ b` from special built-in syntax to a type operator, and
   export it from `Data.BuiltInTypes` and `Data.Type.Equality`.

8. The type constructors `[]`, `()`, `(,)`, `(,,)`, and so on, are no longer
   always in scope. Instead, they are looked up as any other constructor, which
   by default coincides with today's behavior, as `Data.BuiltInTypes` is
   imported by default.

9. The data constructors `[]`, `()`, `(,)`, `(,,)`, `(:)`, and so on, continue to be always in scope.

10. The `[a]` syntax is treated as follows:

    1. Look up `[]` according to the scoping rules in the given context.

    2. If `[]` came from the type namespace, treat `[a]` as `[] a`.

    3. If `[]` came from the data namespace, treat `[a]` as `a : []`.

11. The `(a,b)` syntax means `(,) a b`, where `(,)` is resolved according to the new rules.
    This also applies to tuples of other arities.

12. Deprecate the `'` syntax of `-XDataKinds`, reserving this syntax for Template
    Haskell name quotation. Introduce a new warning, `-Wticked-promoted-constructors` which
    warns of the usage of the syntax.

13. Deprecate the `''` syntax in Template Haskell. Introduce a new warning,
    `-Wdouble-tick-template-haskell` which warns about the usage of the syntax.

14. Introduce a new warning, `-Wpun-bindings`, triggered by any name
    binding that would clash with another identifier if Haskell had a single
    unified namespace.

15. Introduce a new warning, `-Wpuns`, triggered by using an identifier
    that would be ambiguous or refer to another entity if Haskell had a single
    unified namespace.

16. The deprecation strategy for the `'` syntax in `-XDataKinds` and `''` syntax in `-XTemplateHaskell` is the following:

    1. In the next release add `-Wpun-bindings`, `-Wpuns`, `-Wticked-promoted-constructors` and `-Wdouble-tick-template-haskell` to `-Wcompat` and deprecate `-Wunticked-promoted-constructors`.

    2. Three releases from after this proposal is implemented add all four warnings to `-Wall`.

    3. Seven releases from after this proposal is implemented deprecate the syntax and enable `-Wticked-promoted-constructors` and `-Wdouble-tick-template-haskell` by default.

    4. Fifteen releases from after this proposal is implemented remove the syntax.

17. Amend [GHC Proposal #65](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0065-type-infix.rst)
    to use `data` instead of `value`.

18. Under `-XPatternSynonyms`, change the `pattern` qualifier in import and export lists to `data`.
    Introduce `-Wpattern-namespace-qualifier` warning that warns when the
    `pattern` namespace qualifier is used.  Add it to `-Wcompat`.

19. Under `-XDataKinds`, in type signatures with an explicit `forall`, type
    variable lookup falls back to term-level variable lookup, as is already the
    case with type constructors. Type signatures with an implicit `forall` are
    not affected to avoid breakage.

## Examples

### Namespace-Qualified Imports

```haskell
import Data.Proxy type as T
import Data.Proxy data as D

type T = T.Proxy Int

f :: T
f = D.Proxy
```

### Punnning Warnings

Recall that `-Wpun-bindings` is triggered at definition sites that use punning,
and `-Wpuns` is triggered at use sites. To see what qualifies as punning, we
will look at the code that works today and analyze the breakage that would
occur if Haskell had a single unified namespace.

#### `-Wpuns`, example #1

```haskell
module A where { data A = T }
module B where { data T = X }

module C where

import A
import B

f = T
```

If Haskell had a single unified namespace, referring to `T` would result in
ambiguity (is it `A.T` or `B.T`?), thus this should trigger the warning.

#### `-Wpuns`, example #2

```haskell
a = 15
f :: a -> a
```

If Haskell had a single unified namespace, `a` instead of referring to
implicitly bound type-variable `a` would refer to `a` on the type-level. This
means that punning is used and should trigger the warning.

On the contrary:

```haskell
a = 15

f :: forall a. a -> a
```

Does not use punning because if Haskell had a single unified namespace, explicitly bound type variable `a` would shadow the top-level `a`.

#### `-Wpuns`, example #3

```haskell
{-# LANGUAGE ScopedTypeVariabels #-}
a = 15

f :: forall a. a -> a
f = \a -> (a :: a)
--              ^ warning here
```

In all of the `a` uses except for the last one there is no punning, because if
Haskell had a single unified namespace, in the type signature, top-level `a`
would be shadowed by explicitly bound type variable `a`, and in the expression
`a` variable bound in the lambda would shadow the type variable. In the very
last case, however, currently the `a` would refer to the type variable, but if
Haskell had a single namespace it would refer to the term-level variable. Thus the
warning is triggered.

#### `-Wpuns`, example #4

```haskell
f :: [] a -- warning
g :: [a]  -- warning
g = []    -- warning
h = [a]   -- warning
x = [a,b] -- no warning
```

Since `Data.BuiltInTypes` is imported by default, all of the cases except the
very last one will emit `-Wpuns` warning because in all of them it is not clear
whether the data constructor or a type constructor is being referred to, except
in the very last case.

#### `-Wpuns`, example #5

```haskell
f :: ()      -- warning
f = ()       -- warning
g :: (a,b)   -- warning
g = (c,d)    -- warning
h :: (,) a b -- warning
h = (,) c d  -- warning
```

Tuples in this case are very much the same as lists except they will emit a
warning in all cases.

Note that for both lists and tuples if `-XNoImplicitBuiltInTypes` is enabled,
the type constructors will not be in scope anymore and no warnings will be
emitted.

#### `-Wpun-bindings`, example #1

(This example uses [GHC Proposal #155](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0155-type-lambda.rst)).

```haskell
id :: t -> t
id @a a = a
```

Here, when term-level `a` is bound it would conflict with the type level `a` if
Haskell had a single namespace, thus triggering the warning. This behavior is
similar to conflicting definition error for `f b b = ...`:

```haskell
Test.hs:1:3: error:
    • Conflicting definitions for 'b'
      Bound at: Test.hs:1:3
                Test.hs:1:5
    • In an equation for 'f'
```

On the contrary, the code below is fine, similarly to `-Wpuns` example #2, the `a` is shadowed instead:

```haskell
f :: t -> ()
f @a = \a -> ()
```

#### `-Wpun-bindings`, example #2

```haskell
data T = T
```

If Haskell had a single unified namespace, type constructor `T` and data
constructor `T` would conflict, thus this should trigger the warning.

#### `-Wpun-bindings`, example #3

```haskell
data T = MkT
data B = T | F
```

Even though type constructor `T` and data constructor `T` are defined in
different declarations, they would still cause a conflict, same as example #2.

#### `-Wpun-bindings`, example #4

```haskell
data J = Bool
```

This should not cause the warning because `Bool` defined here would not
conflict with `Bool` imported from `Prelude`, this declaration is not rejected
by GHC:

```haskell
import Prelude (Bool)
data Bool -- no conflict
```

## Effect and Interactions

* This proposal does not interact with built-in behavior of `(->)` which remains special.

* There is an asymmetry with what `-XDataKinds` does, as `-XDataKinds` only
  promotes data constructors to the type level and doesn't promote variables.
  On the contrary, new lookup rules let users reference type variables at the
  term-level:

  ```haskell
  {-# LANGUAGE ScopedTypeVariables #-}
  f :: forall a. a -> Int
  f _ = sizeOf a -- passes the renamer, type error until VDQ is implemented
  ```

  Compare this to `-XDataKinds`:

  ```haskell
  {-# LANGUAGE DataKinds #-}
  a = 5
  f :: Proxy a -> Proxy a -- 'a' here does not refer to the term-level 'a' and is implicitly quantified
  ```

  Under this proposal, this can be counter-acted by adding an explicit `forall`

  ```haskell
  {-# LANGUAGE DataKinds #-}
  a = 5
  f :: forall. Proxy a -> Proxy a
  ```

  Now `a` in the signature refers to the term-level definition. It passes the
  renamer and triggers a type error until dependent types are implemented.

 * This code will now pass the renamer, but will still be rejected by the type checker:

   ```haskell
   data T = MkT
   f = T
   ```

## Costs and Drawbacks

* This proposal introduces new syntax (namespace-qualified imports), but at the
  same time it obviates other syntax (the `'` namespace qualifier and the `''`
  name quote). The new syntax is easier so discover by searching for the keyword.

* After `'` syntax in `-XDataKinds` is deprecated it becomes impossible to refer to a punned name on type level:
  ```haskell
  data T = T

  f :: T -- Will always refer to the type constructor
  f = T -- Will always refer to the data constructor
  ```
  The user can circumvent this by either not using punning, or if the punned name is imported, using a namespace qualified import:
  ```haskell
  import Data.Proxy type as T
  import Data.Proxy data as D

  f = T.Proxy -- Refers to the type constructor
  h :: D.Proxy -- Refers to the data constructor
  ```

## Alternatives

* We could use `value`, `pattern`, or any other keyword instead of `data` to denote the data namespace.

* Instead of changing the lookup logic, a new syntax could be introduced to
  specify whether an identifier should be looked up in terms or types.
  In fact, this [has been proposed before](https://github.com/ghc-proposals/ghc-proposals/pull/214).
  However, this results in noisy source code that is hard to read. For example: `a data.: b data.: data.[]`.

* [Dependent Haskell page on GHC wiki](https://gitlab.haskell.org/ghc/ghc/wikis/dependent-haskell#parsingnamespace-resolution)
  suggests using `'` to "flip" the default namespace from one to another, for
  example on types it would mean the data namespace and in terms it would mean
  the type namespace. While this is not as noisy as the previous alternative,
  context-dependent syntax is generally more confusing to read and it still
  conflicts with Template Haskell.

* We could supress `-Wpuns` warning for certain kinds of punning. For instance: we could suppress it for `data Foo = Foo`
  (when the data constructor is related to the type constructor, the most common use of punning) and let users disambiguate
  with module aliases. Or we could suppress `-Wpuns` when punning is used for records `Foo { ... }`. However, this doesn't
  help with backwards compatibility much, introduces unintuitive `-Wpuns` warning behavior (sometimes it warns about puns
  and sometimes it doesn't)

## Unresolved Questions

None

## Implementation Plan

I (Artyom Kuznetsov) will implement the change.

There's a merge request with `-Wpuns` warning implementation which can be found [here](https://gitlab.haskell.org/ghc/ghc/merge_requests/2044).
