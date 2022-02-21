---
author: Anselm Schüler
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/0>).
**After creating the pull request, edit this file again, update the number in
the link, and delete this bold sentence.**

# Add `applyWhen` and `nTimes` to `Prelude`

Add `applyWhen` and `nTimes` from `GHC.Utils.Misc` to `Prelude`

## Motivation

`applyThen` and `nTimes` are small and general functions that could be used in a variety of situations. They’re small enough to be easily defined by the user but obvious enough that they should’t have to be.

They’re currently used twice and thrice respectively in the GHC codebase.

## Proposed Change Specification

The library `Prelude` in the package `base` now exports `applyThen` and `nTimes`.

These are imported from `GHC.Utils.Misc`, but this is an implementation detail. After this change is implemented, they may be moved to a different module and persist by bootstrapping.

## Examples

```haskell
f = (>>= g)
h = nTimes 4 f
```


```haskell
x = applyWhen (x /= 0) g x
```

## Effect and Interactions

This may cause code that is currently rejected to be accepted.  
This changes the output of commands listing module contents (such as GHCi’s `:browse`). 

## Unresolved Questions

Should these functions be renamed (e.g. `applyWhen` → `applyIf`)?
Should these functions be integrated into an existing module (e.g. `Data.Function`)?