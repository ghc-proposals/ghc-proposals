Sized primitive literals
========================

.. author:: Sylvain Henry
.. date-accepted:: 2022-04-23
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/21422
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/451>`_.
.. contents::

Add syntax for sized primitive literals (``Int8#``, ``Word32#``, etc.).


Motivation
----------

GHC provides sized primitive types -- ``Int[8,16,32,64]#`` and
``Word[8,16,32,64]#`` -- and their operations as primops. However it doesn't
support *literals* for these types in Haskell syntax.

It does however support primitive literals for ``Int#`` and ``Word#`` via the
``MagicHash`` extension: ``123#`` is an ``Int#`` literal and ``123##`` is a
``Word#`` literal.

Internally in Core, literals for sized primitives are supported but the only way
to use them is to rely on constant folding. For example, the following code:

::

  import GHC.Word
  import GHC.Exts
  
  -- from Int# and Word# literals
  foo (# #) = intToInt64#   123#
  bar (# #) = wordToWord32# 123##
  -- from Integer literals
  baz (# #) = let unW16 (W16# w) = w in unW16 (fromInteger 123)

is compiled into the following Core:

::

  foo = \ _ -> 123#64
  
  bar = \ _ -> 123##32
  
  baz = \ _ -> 123##16

Notice the literal suffixes that aren't allowed with Haskell syntax. This
proposal is about adding support for these suffixes (not necessarily in this
form) to Haskell.

Adding support for these literals would make some codes easier to write. In
particular, allowing case expression on sized literals.


Proposed Change Specification
-----------------------------

Syntax was debated and was settled with a vote after discussions on the
previous revisions of this proposal (21 voters):

::

  After a week of voting we have the following (using ranked pairs):
  
  Winner: Hash syntax extension, expecting generalization: 123#Int8, 123#Word64
  Runner up: Mimicking C/Rust syntax: 123#i8, 123#u64
  Third place: Hash syntax extension: 123#8, 123##64
  Fourth place: Type mimickry: 123 :: Int8# 123 :: Word64#

Hence we propose that a new extension (e.g. "ExtendedLiterals") allows the
following literal forms:

::

  123#Int   -- Int# literal
  123#Int8  -- Int8# literal
  123#Int16 -- Int16# literal
  123#Int32 -- Int32# literal
  123#Int64 -- Int64# literal

  123#Word   -- Word# literal
  123#Word8  -- Word8# literal
  123#Word16 -- Word16# literal
  123#Word32 -- Word32# literal
  123#Word64 -- Word64# literal

The lexer has new lexemes for these literals.

The lexer for primitive ``Int#`` and ``Word#`` has to take into account two
extensions (``NegativeLiterals`` and ``BinaryLiterals``) in addition to
``MagicHash``. It is currently defined as follows:

::

  -- Unboxed ints (:: Int#) and words (:: Word#)
  -- It's simpler (and faster?) to give separate cases to the negatives,
  -- especially considering octal/hexadecimal prefixes.
  @decimal                          \# / { ifExtension MagicHashBit }        { tok_primint positive 0 1 decimal }
  0[bB] @numspc @binary             \# / { ifExtension MagicHashBit `alexAndPred`
                                           ifExtension BinaryLiteralsBit }   { tok_primint positive 2 3 binary }
  0[oO] @numspc @octal              \# / { ifExtension MagicHashBit }        { tok_primint positive 2 3 octal }
  0[xX] @numspc @hexadecimal        \# / { ifExtension MagicHashBit }        { tok_primint positive 2 3 hexadecimal }
  @negative @decimal                \# / { negHashLitPred }                  { tok_primint negative 1 2 decimal }
  @negative 0[bB] @numspc @binary   \# / { negHashLitPred `alexAndPred`
                                           ifExtension BinaryLiteralsBit }   { tok_primint negative 3 4 binary }
  @negative 0[oO] @numspc @octal    \# / { negHashLitPred }                  { tok_primint negative 3 4 octal }
  @negative 0[xX] @numspc @hexadecimal \#
                                       / { negHashLitPred }                  { tok_primint negative 3 4 hexadecimal }

  @decimal                       \# \# / { ifExtension MagicHashBit }        { tok_primword 0 2 decimal }
  0[bB] @numspc @binary          \# \# / { ifExtension MagicHashBit `alexAndPred`
                                           ifExtension BinaryLiteralsBit }   { tok_primword 2 4 binary }
  0[oO] @numspc @octal           \# \# / { ifExtension MagicHashBit }        { tok_primword 2 4 octal }
  0[xX] @numspc @hexadecimal     \# \# / { ifExtension MagicHashBit }        { tok_primword 2 4 hexadecimal }

We propose to extend it as follows for sized literals. We only show the
``Int8#`` case to avoid cluttering this proposal: other cases are very similar.
The call to ``tok_primint8`` returns a lexeme corresponding to ``Int8#``
literals.

::

  @decimal                             \#Int8 / { ifExtension ExtendedLiterals }   { tok_primint8 positive 0 1 decimal }
  0[bB] @numspc @binary                \#Int8 / { ifExtension ExtendedLiterals `alexAndPred`
                                                 ifExtension BinaryLiteralsBit }   { tok_primint8 positive 2 3 binary }
  0[oO] @numspc @octal                 \#Int8 / { ifExtension ExtendedLiterals }   { tok_primint8 positive 2 3 octal }
  0[xX] @numspc @hexadecimal           \#Int8 / { ifExtension ExtendedLiterals }   { tok_primint8 positive 2 3 hexadecimal }
  @negative @decimal                   \#Int8 / { negHashLitPred }                 { tok_primint8 negative 1 2 decimal }
  @negative 0[bB] @numspc @binary      \#Int8 / { negHashLitPred `alexAndPred`
                                                  ifExtension BinaryLiteralsBit }  { tok_primint8 negative 3 4 binary }
  @negative 0[oO] @numspc @octal       \#Int8 / { negHashLitPred }                 { tok_primint8 negative 3 4 octal }
  @negative 0[xX] @numspc @hexadecimal \#Int8 / { negHashLitPred }                 { tok_primint8 negative 3 4 hexadecimal }

(This can probably be factored with ``@signed_suffix`` and ``@unsigned_suffix``).

Examples
--------

Example of a case-expression on a ``Word64#``:

::

  case x of
    0#Word64   -> ...
    123#Word64 -> ...
    _          -> ...


Effect and Interactions
-----------------------

Extension: we could use the same syntax for ``Char#`` literals (e.g. ``123#Char``)
or for ``Float#/Double#`` literals, but it is left out of the scope of this
proposal.

``Int#`` and ``Word#`` literals can now be created with two different syntaxes:
``123# / 123#Int`` and ``123## / 132#Word``. We could deprecate former syntax but it
is left out of the scope of this proposal.


Costs and Drawbacks
-------------------
None.

Alternatives
------------

Other syntaxes were suggested:

- Mimicking C/Rust syntax without the hash: ``123i8, 123u64`` (initial proposal)
- Mimicking C/Rust syntax: ``123#i8, 123#u64``
- Hash syntax extension, expecting generalization: ``123#Int8, 123#Word64``
- Hash syntax extension: ``123#8, 123##64``
- Type mimickry: ``123 :: Int8# 123 :: Word64#``


Implementation Plan
-------------------
I [Sylvain Henry] could implement it.

Endorsements
-------------
