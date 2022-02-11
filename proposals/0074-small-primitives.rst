
Add more fixed size primitive types, like ``Int8#/Word8#``
==========================================================

.. author:: Michal Terepeta
.. date-accepted:: 2018-03-01
.. ticket-url:: https://phabricator.haskell.org/D5006, https://phabricator.haskell.org/D5258
.. implemented:: GHC-8.10
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/74>`_.
.. contents::

This proposes adding new primitive types to GHC:

- ``Int8#``/``Word8#``

- ``Int16#``/``Word16#``

- ``Int32#``/``Word32#``

- ``Int64#``/``Word64#``

along with corresponding primitive operations.

Motivation
------------

GHC currently only has a few machine-level primitives (not counting arrays or
STM stuff): ``Int#``, ``Word#``, ``Float#``, ``Double#`` and ``Char#`` (which is
a 31-bit character). This means that there is no way to express a value that is
exactly one byte in size (or two, etc.), for instance, ``Word8`` is defined in
terms of ``Word#`` (`definition of Word8`_) and thus will always take 64 bits
(on 64-bit arch). This is not only wasteful in terms of space, but also
counter-intuitive, consider:

::

  data MyStruct =
      MyStruct
          {-# UNPACK #-} !Word16
          {-# UNPACK #-} !Word16
          {-# UNPACK #-} !Word16
          {-# UNPACK #-} !Word16

On 64-bit architecture one could expect that this takes just 8 bytes (in this
proposal I'm ignoring the additional object header). However, GHC today will use
32 bytes for this (each field is a full 8-byte word). This can be quite
surprising for people using C/C++, Rust, etc. For instance, in Rust the
following structure would take only 8 bytes.

::

  struct MyStruct {
      a : u16,
      b : u16,
      c : u16,
      d : u16
  }

The only way to achieve this in GHC would be to manually use an unboxed
``Word64`` and use shifts to extract the data, which is both less efficient and
far more error-prone.

.. _definition of Word8: https://github.com/ghc/ghc/blob/b3ae47caf2f23cfd2c22c29dbfca646493ffe469/libraries/base/GHC/Word.hs#L64

Proposed Change Specification
-----------------------------

Add and expose new primitive types:

- ``Int8#``/``Word8#``

- ``Int16#``/``Word16#``

- ``Int32#``/``Word32#``

Along with the set of basic primitive operations (similar to ``Int#`` and
``Word#``) as well as conversions to/from ``Int#``/``Word#``.

GHC could expose them along with ``Int#`` and ``Word#``.

Effect and Interactions
-----------------------

Having the new primitives in GHC would allow us to redefine types like
``Word{8,16,32}`` and ``Int{8,16,32}`` as well as cleanup
``Data.Int``/``Data.Word`` modules and get rid of ``narrow`` primitives (but
note that this is not part of this proposal, I believe it should be a separate
proposal to the libraries committee).

There might be also interesting interactions with unpacked sums (note:
``UNPACK`` doesn't yet work on sum types yet). Consider:

::

  data MyStruct =
      MyStruct {#- UNPACK #-} !Bool
               {#- UNPACK #-} !Bool
               {#- UNPACK #-} !Bool
               {#- UNPACK #-} !Bool

Without small primitive types, the only way to unpack this is to use at least
one word per field (which on 64-bit arch this would correspond to a total of 4 *
8 = 32 bytes for the whole structure).  If we had ``Word8#`` this could go down
to a total of just one word (1 byte per ``Bool`` field, but rounded to a word
due to heap layout).


Costs and Drawbacks
-------------------

Implementation shouldn't require any major changes/redesign and maintenance
costs should be relatively small (nothing is likely to change after
implementation).


Alternatives
------------

The only alternative I can think of is to decide not to do this (but that not a
very satisfactory "solution" to the problem ;)


Unresolved questions
--------------------

I don't see any, but please comment :)


Implementation Plan
-------------------

I would like to implement this (but might need some guidance :).

I believe GHC's current calling convention would not have to change. For any
parameters smaller than full register width, we will only use the bottom bits.
From the implementation perspective, the caller might need to zero-extend the
parameters and the callee to narrow them back.  I believe this is what ``ghccc``
(`LLVM's calling convention for GHC`_) already does.

An alternative would be to create a new calling convention to try to avoid the
widening/narrowing, but so far all my attempts seemed overly complicated (due
to, e.g., 32-bit x86 having the unfortunate limitation that not all registers
have their lower 8-bits for use)

Other than that, this proposal would require changing the following pieces of
GHC (please comment if you know of any more places!):

- Primops file (``compiler/prelude/primops.txt.pp``)

- GHC's wired-in types to expose the new types (``compiler/prelude/TysPrim.hs``,
  ``compiler/prelude/TysWiredIn.hs``)

- Extend ``TyCon.PrimRep`` (``compiler/types``) and `` ``GHC.Types.RuntimeRep``
  (``ghc-prim``) to represent their width. We already have ``Int64``, so we
  could add a new constructor for each of the new primitives. Alternatively, we
  could have a single ``IntRep`` (``WordRep``) constructor parameterized by the
  width (similarly to what ``VecRep`` does).

- Constant folding to support evaluating the new primitive operations
  (``compiler/prelude/PrelRules.hs``)

- Codegen to support express the new operations as ``MachOp``
  (``compiler/codeGen/StgCmmPrim.hs``)

- Backends to actually compile them. LLVM should be quite easy because it
  supports all of this.  Native backend might require more work (e.g., ``imulb``
  uses implicit register operands and is not currently supported by the native
  backend)

Additional context
------------------

- `Initial discussion`_ on ``ghc-devs`` about small primitives

- Ticket about always exposing ``Word64#``/``Int64#``: `#11953`_


.. _Initial discussion: https://mail.haskell.org/pipermail/ghc-devs/2017-August/014462.html

.. _#11953: https://gitlab.haskell.org/ghc/ghc/issues/11953

.. _LLVM's calling convention for GHC: https://github.com/llvm-project/llvm-project-20170507/blob/e11c49f6c12a9646ef77f8781acc626bbfcae9b5/llvm/lib/Target/X86/X86CallingConv.td#L648
