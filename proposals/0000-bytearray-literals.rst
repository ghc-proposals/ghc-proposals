.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/N>`_.

.. contents::

ByteArray# literals
===================

This is a proposal to introduce ``ByteArray#`` literals. The user
would be able to write code like ``[utf8|Tag Team|]`` and
``[utf16le|Araña|]``, valid as either an expression or a pattern.
This would not require turning on ``TemplateHaskell``.

Motivation
----------

This proposal addresses several shortcomings with string literals in GHC:

* GHC produces suboptimal generated code when using constant ``ByteArray#``
  terms (often wrapped in ``Data.Primitive.ByteArray`` at the top level).
  The ``ByteArray`` constructor that wraps the ``ByteArray#`` gets forced every
  time it is accessed.
* The ``text`` library produces unoptimizable Core that leads to bloated
  binaries when creating ``Text`` literals with ``OverloadedStrings``
  (or equivalently with ``pack``). This is explained in greater detail
  on the `GitHub text library issue`_.
* There is no syntactically simple way to write textual
  non-ASCII ``Addr#`` (e.g. literals in UTF-8 or UTF-16).
  Related ticket: `Trac 5877 <https://ghc.haskell.org/trac/ghc/ticket/5877>`_.
* GHC has no mechanism for generating efficient instructions when
  users want to scrutinize a ``ByteArray#``. Users cannot currently case on
  ``ByteArray#`` at all and must often resort to performing a series
  of equality tests. Independently, various authors have reinvented gross
  hacks to speed this up. Simon Jakobi has `discussed`_ accelerating
  Dhall's decoding of builtins by `checking length before matching`_.
  Andrew Martin (the author of this proposal) has `gone even further`_
  by checking the length and hashing the tokens in a FortiOS log parser.
  Doing this by hand is tedious and error prone. The compiler could
  generate better code with less opportunity for mistakes.

.. _GitHub text library issue: https://github.com/haskell/text/issues/287
.. _discussed: https://github.com/haskell/bytestring/pull/191#issuecomment-633260238
.. _checking length before matching: https://github.com/dhall-lang/dhall-haskell/blob/0cc0e9cf4f967c26f6e03cc0e0209a6e29a19047/dhall/src/Dhall/Binary.hs#L165-L200
.. _gone even further: https://github.com/layer-3-communications/fortios-syslog/blob/2c27f91d58dcc03177ae28e69e5d5b41ac658c95/src/Fortios/Syslog/Unsafe.hs#L362

This proposal introduces provides a mechanism for writing ``ByteArray#``
literals. It does this with without extending the language
syntactically. Consequently, all programs that compile with GHC today will
continue to compile after this proposal is implemented.

Recap: String desugaring currently
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Currently, it is possible to create primitive ``Addr#`` string literals:

.. code-block:: haskell

  "hello"# -- :: Addr#

Such literals encode text as UTF-8. Additionally, arbitrary octets are
supported via hexadecimal and decimal escape sequences. A null byte is
added to the end of the sequence.

``String`` literals like ``"hello"``, ``"Юникод"``, ``"\NUL"`` are desugared
using `Modified UTF-8`_. For example:

.. _Modified UTF-8: https://en.wikipedia.org/wiki/UTF-8#Modified_UTF-8

.. code-block:: haskell

  "hello" ==> unpackCString# "hello"#
  "Юникод" ==> unpackCStringUtf8# "\208\174\208\189\208\184\208\186\208\190\208\180"#
  "\NUL" ==> unpackCStringUtf8# "\192\128"#

Like ``Addr#`` literals, these result in null-terminated byte sequences.
Unlike ``Addr#`` literals, they use Modified UTF-8. Note that Modified
UTF-8 is only used when desugaring ``String`` literals, not when desugaring
``Addr#`` literals. More concretely, ``"\NUL"`` is not desugared to an
expression that involves ``"\NUL"#``.  This proposal does not impact the
desugaring of ``String`` literals or ``Addr#`` literals in any way.

Proposed Change Specification
-----------------------------

Rather than adding new syntax, this proposal leverages an existing GHC
extension: ``QuasiQuotes``. Rather than using ``TemplateHaskell``, these
quasiquoters would be built in to the compiler. Here are some examples of
``ByteArray#`` literals under this scheme::

    [octets|fe01bce8|] -- ByteArray# (four bytes)
    [utf8|Araña|]      -- ByteArray# (UTF-8)
    [utf16|Araña|]     -- ByteArray# (UTF-16, native endian)
    [utf16le|Araña|]   -- ByteArray# (UTF-16, little endian)
    [utf16be|Araña|]   -- ByteArray# (UTF-16, big endian)

The five quasiquoters showcased above would be known-key identifiers
exported by ``GHC.Exts``. The
resulting ``ByteArray#`` literals would not be null-terminated. The
textual quasiquotes (those that start with ``utf``) do not support
escape sequences. The ``octet#`` quasiquoter only supports hexadecimal
characters, and the number of characters must be even. GHC will throw
an error at compile-time if an odd number of hexadecimal characters
are given as the argument to ``octets``.

These literals can be used both as values and as a way to scrutinize a
``ByteArray#`` that has been cased on. Casing would look like this:

.. code-block:: haskell

  readSmallNumber :: ByteArray# -> Int#
  readSmallNumber x = case x of
    [utf8|one|] -> 1#
    [utf8|two|] -> 2#
    [utf8|three|] -> 3#
    _ -> 4#

When compiling STG to cmm, GHC has an opportunity to generate very
good code for case expressions like this. For lengthier case expressions
that test against dozens of strings, GHC could emit code that performs
perfect or nearly-perfect hashing. Currently, that burden is pushed onto
program authors.

Only one optimization is mandated by this proposal: GHC must perform
constant-folding when ``sizeofByteArray#``, ``indexWord8Array#``, or
``isByteArrayPinned#`` is applied to a ``ByteArray#`` literal.

Users in need of other less common textual encodings could use template
haskell to provide additional non-built-in quasiquoters.

Effects and Interactions
------------------------

The template-haskell library
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The data type ``QuasiQuoter`` (currently defined in
``Language.Haskell.TH.Quote``) needs to be moved from ``template-haskell``
to ``base``. It is desirable that the known-key quasiquoters be
available *without* depending on the ``template-haskell`` library.
For this to be possible, their *type* must also be defined in ``base``.

Overloaded Strings
~~~~~~~~~~~~~~~~~~
Future proposals may build on top of this one to improve the desugaring
of string literals. This proposal does not change the way that string
literals are desugared, but it does lay important groundwork that any
future proposal would build on.

Compact Regions
~~~~~~~~~~~~~~~
All ``ByteArray#`` literals are considered pinned, but
unlike explicitly pinned ``ByteArray#`` literals, they can be copied into
a compact regions. Technically, they would not actually be copied. The
compact region is allowed to point to them because they are static data
that cannot be GCed.


Costs and Drawbacks
-------------------

None that the author is aware of.

Unresolved questions
--------------------

None.

Implementation Plan
-------------------

There are three phases for implementation:

1. Add ``ByteArray#`` literals to GHC Core. Support them with built-in
   quasiquoters. An eager student may implement this.
2. Allow casing on values of type ``ByteArray#`` with ``ByteArray#`` literals.
   Desugar this to nearly-perfect hashing in cmm. An eager student may
   implement this.
3. Allow ``ByteArray#`` literals to appear in all other expected places.
   Float them all to the top level. Constant fold ``sizeofByteArray#``,
   ``isByteArrayPinned#``, and ``indexWord8Array#`` when their argument
   byte array is a literal. An eager student may implement this.

Phase 1 and 2 can be merged without phase 3 being completed. There is
plenty of value in being able to case on values of type ``ByteArray#``
even without being able to use literals elsewhere.
