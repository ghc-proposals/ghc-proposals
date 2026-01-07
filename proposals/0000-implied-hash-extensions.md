---
author: Andreas Klebinger
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/691).

# Introduce a new XLowLevelHaskell meta extension.

When working with low level code and unboxed types there is a whole family of commonly used extensions.
While it might not be desirable to have these be on by default it would be good to normalize a set
of Extensions that can be expected to be available for "low level" code.

I propose we introduce a `XLowLevelHaskell` extension which transitively enables a common extensions
which are useful for writing relatively low level code for this purpose.

## Motivation

When writing unboxed or low level code I almost always end up pairing -XMagicHash with some other
common extensions. These being in order of frequency:

* -XMagicHah
* -XExtendedLiterals
* -XUnboxedTuples
* -XUnboxedSums
* -XUnliftedNewTypes
* -XUnliftedFFITypes

I believe it would be better to have an extension `-XLowLevelHaskell` which enables all those extensions
for better ergonomics. Beyond this adding another extension I don't see a downside to this.

## Proposed Change Specification

Enabling `-XLowLevelHaskell` will imply the following extensions:

* -XMagicHah
* -XExtendedLiterals
* -XUnboxedTuples
* -XUnboxedSums
* -XUnliftedNewTypes
* -XUnliftedFFITypes

`XLowLevelHaskell` should be considered experimental, and could be expanded to include other
extensions in the future.

## Proposed Library Change Specification

None

## Examples

After this change a file like this would compile without error:

```haskell
{-# LANGUAGE LowLevelHaskell #-}
-- {-# LANGUAGE MagicHash #-}
-- {-# LANGUAGE UnboxedTuples #-}
-- {-# LANGUAGE UnliftedNewtypes #-}
-- {-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE DataKinds #-}

module M where

import GHC.PrimOps (Word16#, atomicCasWord16Addr#, TYPE, Addr#, eqWord16# )
import GHC.Types
import Data.Kind
import GHC.Word

-- Newtype to allow defining instances
newtype CounterRef (a :: TYPE AddrRep) = CounterRef a

type Counter = CounterRef Addr#

resetCounter :: Counter -> Word16 -> IO Bool
resetCounter (CounterRef addr) (W16# expected) = IO $ \s ->
    case atomicCasWord16Addr# addr expected 0#Word16 s of
        (# s2, old #) -> (# s2, isTrue# (eqWord16# old expected) #)

```

## Effect and Interactions

With this proposal the need for a whole zoo of extensions every time one needs to work
with primops or unboxed types goes away. Both for compiled code and when working in the interpreter.

I am not aware of any contentious interactions with other language or compiler features.

## Costs and Drawbacks

The implementation of this would naturally be fairly minimal.

## Backward Compatibility

`XLowLevelHaskell` being a new extension there would be no issue with
backwards compatiblitiy. Therefore no migration strategy is required.

## Alternatives

Leaving typical low level functionality gated behind multiple extensions.

## Unresolved Questions

None that I'm aware of.

## Implementation Plan

I (Andreas Klebinger) would be interested in implementing this.

## Endorsements

Non as I've not yet discussed this with anyone.
