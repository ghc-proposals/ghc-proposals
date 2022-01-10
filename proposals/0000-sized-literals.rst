Sized primitive literals
========================

.. author:: Sylvain Henry
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/451>`_.
.. contents::

Add syntax for sized primitive literals (Int8#, Word32#, etc.).


Motivation
----------

GHC provides sized primitive types -- ``Int[8,16,32,64]#`` and
``Word[8,16,32,64]#`` -- and their operations as primops. However it doesn't
support *literals* for these types in Haskell syntax.

It does however support primitive literals for ``Int#`` and ``Word#`` via the
``MagicHash`` extension: ``123#`` is an ``Int#`` literal and ``123##`` is a
``Word#`` literal.

Internally in Core literals for sized primitives are supported but the only way
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

We propose to borrow Rust's syntax for sized literals. A new extension (e.g.
"SizedLiterals") allows the following forms:

::

  123#i   -- Int# literal
  123#i8  -- Int8# literal
  123#i16 -- Int16# literal
  123#i32 -- Int32# literal
  123#i64 -- Int64# literal

  123#u   -- Word# literal
  123#u8  -- Word8# literal
  123#u16 -- Word16# literal
  123#u32 -- Word32# literal
  123#u64 -- Word64# literal

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

  @decimal                             \#i8 / { ifExtension SizedLiterals }      { tok_primint8 positive 0 1 decimal }
  0[bB] @numspc @binary                \#i8 / { ifExtension SizedLiterals `alexAndPred`
                                              ifExtension BinaryLiteralsBit }    { tok_primint8 positive 2 3 binary }
  0[oO] @numspc @octal                 \#i8 / { ifExtension SizedLiterals }      { tok_primint8 positive 2 3 octal }
  0[xX] @numspc @hexadecimal           \#i8 / { ifExtension SizedLiterals }      { tok_primint8 positive 2 3 hexadecimal }
  @negative @decimal                   \#i8 / { negHashLitPred }                 { tok_primint8 negative 1 2 decimal }
  @negative 0[bB] @numspc @binary      \#i8 / { negHashLitPred `alexAndPred`
                                                ifExtension BinaryLiteralsBit }  { tok_primint8 negative 3 4 binary }
  @negative 0[oO] @numspc @octal       \#i8 / { negHashLitPred }                 { tok_primint8 negative 3 4 octal }
  @negative 0[xX] @numspc @hexadecimal \#i8 / { negHashLitPred }                 { tok_primint8 negative 3 4 hexadecimal }

(This can probably be factored with ``@signed_suffix`` and ``@unsigned_suffix``).

Examples
--------

Example of a case-expression on a ``Word64#``:

::

  case x of
    0#u64   -> ...
    123#u64 -> ...
    _       -> ...


Effect and Interactions
-----------------------
None.


Costs and Drawbacks
-------------------
None. It doesn't make the language any harder to learn. On the contrary, it
allows the replacement of some "Magic" (the hashes suffixes for ``Int#`` and
``Word#`` literals) with more meaningful suffixes.

Alternatives
------------

We could also keep the "double-#" syntax instead of introducing "i/u" suffixes:

::

  123#    -- Int# literal
  123#8   -- Int8# literal
  123#16  -- Int16# literal
  123#32  -- Int32# literal
  123#64  -- Int64# literal

  123##   -- Word# literal
  123##8  -- Word8# literal
  123##16 -- Word16# literal
  123##32 -- Word32# literal
  123##64 -- Word64# literal

An earlier revision of this proposal proposed to drop the "#" completely, but it
was considered confusing because there is no hint that literals are unboxed:

::

  123i   -- Int# literal
  123i8  -- Int8# literal
  123i16 -- Int16# literal
  123i32 -- Int32# literal
  123i64 -- Int64# literal

  123u   -- Word# literal
  123u8  -- Word8# literal
  123u16 -- Word16# literal
  123u32 -- Word32# literal
  123u64 -- Word64# literal


Unresolved Questions
--------------------
None for now.


Implementation Plan
-------------------
I could implement it.

Endorsements
-------------
