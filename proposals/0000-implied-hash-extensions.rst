.. author: Andreas Klebinger
.. date-accepted: ""
.. ticket-url: ""
.. implemented: ""

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/691>`_.

Introduce a new XLowLevelHaskell meta extension.
=================================================

When working with low level code and unboxed types there is a whole family of commonly used extensions.
While it might not be desirable to have these be on by default it would be good to normalize a set
of Extensions that can be expected to be available for "low level" code.

I propose we introduce a ``XLowLevelHaskell`` extension which transitively enables common extensions
which are useful for writing relatively low level code for this purpose.

Motivation
----------

When writing unboxed or low level code I almost always end up pairing -XMagicHash with some other
common extensions. These being in order of frequency:

* -XMagicHash
* -XExtendedLiterals
* -XUnboxedTuples
* -XUnboxedSums
* -XPatternSynonyms
* -XUnliftedNewTypes
* -XUnliftedFFITypes
* -XUnliftedDatatypes

I believe it would be better to have an extension ``-XLowLevelHaskell`` which enables all those extensions
for better ergonomics. Beyond this adding another extension I don't see a downside to this.

Concrete motivations for every extension are:

* -XMagicHash, XUnboxedTuples, -XUnboxedSums are required for use of low level primops
  which are common in low level haskell code.
* -XExtendedLiterals allows specifying primitive literals like `42#Int8` without introducing conversion
  expressions.
* -XPatternSynonyms allows to abstract pattern matching on at times complex combinations of unboxed tuples and sums
  as if they were constructors.
* -XUnliftedNewTypes is required to introduce safe abstractions over primitive types without runtime overhead.
* -XUnliftedFFITypes for using low level types directly via FFI.
* -XUnliftedDatatypes is required for low level high performance code.

While most of these are obvious, for PatternSynonyms here is an example from the GHC Source code defining
a unboxed Maybe type:

.. code-block:: haskell

    newtype MaybeUB a = MaybeUB (# (# #) | a #)

    pattern JustUB :: a -> MaybeUB a
    pattern JustUB x = MaybeUB (# | x #)

    pattern NothingUB :: MaybeUB a
    pattern NothingUB = MaybeUB (# (# #) | #)

    {-# COMPLETE NothingUB, JustUB #-}

    fromMaybeUB :: a -> MaybeUB a -> a
    fromMaybeUB d NothingUB = d
    fromMaybeUB _ (JustUB x) = x


Proposed Change Specification
------------------------------

Enabling ``-XLowLevelHaskell`` will imply the following extensions:

* -XMagicHash
* -XExtendedLiterals
* -XUnboxedTuples
* -XUnboxedSums
* -XUnliftedNewTypes
* -XUnliftedFFITypes
* -XPatternSynonyms
* -XHexFloatLiterals (Enabled by GHC2021 but for completeness)
* -XUnliftedDatatypes

``XLowLevelHaskell`` should be considered experimental, and could be expanded to include other
extensions in the future.

Proposed Library Change Specification
--------------------------------------

None

Examples
--------

After this change a file like this would compile without error:

.. code-block:: haskell

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

Effect and Interactions
------------------------

With this proposal the need for a whole zoo of extensions every time one needs to work
with primops or unboxed types goes away. Both for compiled code and when working in the interpreter.

I am not aware of any contentious interactions with other language or compiler features.

Costs and Drawbacks
--------------------

The implementation of this would naturally be fairly minimal.

Backward Compatibility
-----------------------

``XLowLevelHaskell`` being a new extension there would be no issue with
backwards compatibility. Therefore no migration strategy is required.

Alternatives
------------

Leaving typical low level functionality gated behind multiple extensions.

Versioned extension
~~~~~~~~~~~~~~~~~~~
Another mentioned alternative was "versioning" the extension similar to the
`LANGUAGE GHC<YEAR>` approach.

I'm not a huge fan of this for this language extension.

The goal for this extension is not to enable features which are likely to conflict with existing
code. But rather to allow writing code which focuses on high performance or low level details of
execution with less boiler plate.

Inclusion in `LANGUAGE GHC` editions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

I believe many of the extensions covered by the proposed extension could be included
in a future GHC edition by default. To a point where in the future the extensions as
proposed would be redundant, and this would be optimal.

In particular XUnboxedTuples, XUnboxedSums, XUnliftedNewTypes, XUnliftedFFITypes
XPatternSynonyms, and XUnliftedDatatypes I think would be very reasonable to include
by default in a future GHC edition. They steal no relevant syntax, their semantics
are fairly clear and have been use for a long time.

XExtendedLiterals and XMagicHash steal `#` as a possible operator. I believe this
could be tackled by tweaking the behaviour of `#` and XMagicHash.

If there is a choice between reworking MagicHash and these extensions being enabled by default
or a new LowLevelHaskell meta extensions I think I prefer the former. However it's unclear
if (or when) this would happen.

For now I will open a proposal on neccesary MagicHash changes. And depending on the
outcome there I might close this proposal as redundant.


Unresolved Questions
---------------------

None that I'm aware of.

Implementation Plan
--------------------

I (Andreas Klebinger) would be interested in implementing this.
