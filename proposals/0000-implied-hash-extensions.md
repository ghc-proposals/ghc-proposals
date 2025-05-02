---
author: Andreas Klebinger
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/691).

# Implied -XMagicHash extensions

When working with unboxed types there is a whole family of commonly used together extensions.
I argue that -XMagicHash should implie these extensions by default.

## Motivation

When writing unboxed code I almost always end up pairing -XMagicHash with some other
common extensions. These being in order of frequency:

* -XExtendedLiterals
* -XUnboxedTuples
* -XUnboxedSums
* -XUnliftedNewTypes
* -XUnliftedFFITypes

I believe having some or all of those be implied by -XMagicHash would be a net ergonomic improvement
to working with unlifted types. As I found myself growing tired of enabling those every time.

## Proposed Change Specification

Enabling `-XMagicHash` will imply the following extensions:

* -XExtendedLiterals
* -XUnboxedTuples
* -XUnboxedSums
* -XUnliftedNewTypes
* -XUnliftedFFITypes

## Proposed Library Change Specification

None

## Examples

After this change a file like this would compile without error:

```haskell
{-# LANGUAGE MagicHash #-}
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

With this proposal the need for the additional LANGUAGE specifications goes away.
Both for compiled code and when working in the interpreter.

I am not aware of any contentious interactions with other language or compiler features.

## Costs and Drawbacks

The implementation of this would naturally be fairly minimal.

## Backward Compatibility

The only drawbacks that come to mind for me is that I believe it is possible to conjure
up a program which conflicts with the syntax claimed by `ExtendedLiterals`.

However this is unlikely to happen naturally and would need to happen in code already
using XMagicHash to breack backwards compatible.

So I believe strongly this change would fall under breakage category 1:

1. Breakage only in extremely rare cases (e.g. for specifically-constructed
   examples, but probably no packages published in the Hackage package repository)

Given how unlikely breakage due to this change would be I do not believe this would
warrant any special migration strategy.

## Alternatives

The implication could be made dependend on the language edition used. That is
only with eg. GHC2026 -XMagicHash would imply these other language extensions.

However I think this is more likely to cause confusion than to help.

## Unresolved Questions

I do not see any unresolved question.


## Implementation Plan

I (Andreas Klebinger) would be interested in implementing this.

## Endorsements

Non as I've not yet discussed this with anyone.
