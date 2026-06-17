---
author: Oleg Grenrus
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at pull request 408](https://github.com/ghc-proposals/ghc-proposals/pull/408)

# Import renaming

We propose to add `-XRenamingImports` to extend import syntax such that it is possible to rename definitions.

```haskell
import Distribution.Simple renaming (defaultMain to main)
```

## Motivation

Names are source of many debates. Whether

```haskell
newtype Fix = Fix { unFix :: f (Fix a) }
```

or

```haskell
newtype Fix = Fix { unfix :: f (Fix a) }
```

Recently implemented `QualifiedDo` extension requires to create dummy modules just for this feature.
Instead we could

```haskell
import qualified Data.Functor.Bind as Semi renaming ((>>-) to (>>=))
```

Another example is `optics`. Some people just like `.` as optical composition operator.

```haskell
import Prelude hiding ((.))
import Optics renaming ((%) to (.))
```

This is a small step towards [*Local modules* discussed in PR#283](https://github.com/ghc-proposals/ghc-proposals/pull/283).
The proposals are not in conflict.

## Proposed Change Specification

```
impdecl	→	import [qualified] modid [as modid] [impspec] [renspec]

renspec →   renaming ( import1 to rename1 , … , importn to rename2 [ , ] )     (n ≥ 0)

rename  →   almost like import, but without namespaces
```


Lexically `renaming` and `to` are `varid` (similarly to `as`, `qualified` and `hiding`) rather than a `reservedid`.
They have special significance only in the context of `import` declaration, it may also be used as a variable.

There is no conflict with `import Foo qualified` syntax.


## Examples

We can hide an rename.

```haskell
import Prelude hiding (tail) renaming (head to unsafeHead)
```

We can import just some names, and rename others

```haskell
import Data.List (foldl') renaming (foldr1 to unsafeFoldr1)
```

Renamed names are always imported, `importspec` specifies
further names to be imported or hidden.

We can rename with `ExplicitNamespaces`

```haskell
import GHC.Generics renaming (type (:*:) to Prod)
```

We can rename import declaration

```haskell
import Data.Maybe renaming (Maybe (Just, Nothing) to Option (Some, None))
```

## Effect and Interactions

TBW

## Costs and Drawbacks

Allowing to *export* renamed names would allow a lot,
but would require GHC interface files to contain
not only bindings but also renamings.

There is interesting interaction with `RecordDotSyntax`-like features,
where constraint solver generates instances based on names in scope.
Yet it wouldn't be possible to rename instances defined by hand.
(TODO Check: if it's forbidden to shadow automatic instance, the renaming will break that invariant).

## Alternatives

An alternative is to do nothing.
The `QualifiedDo` example can be resolved by introducing new (possibly local in the future) modules.
(E.g. https://github.com/ekmett/semigroupoids/pull/103)
The optics example can be resolved by introducing local binding

```haskell
(.) :: ...
(.) = (Optics.%)
```

However, these introduce *new* bindings, so they behave (slightly)
differently regarding RULES, diamond imports etc.
They aren't just the new name for the same thing,
they are different things which are almost equal.

The renaming of data-types and constructors
can be achieved with `type` and pattern synonyms.

Even `type` alias is mostly the same as its RHS,
they have own "identity", i.e. the same `type` alias introduced
in separate modules will cause ambiguous name.
Renaming wouldn't.

Pattern synonyms (consider `Maybe` example) need extra machinery,
e.g. `COMPLETE` pragmas to behave the same.
Similarly to `type` aliases they are *things* themselves, not just names for old things.

## Unresolved Questions

A lot. TBW.
