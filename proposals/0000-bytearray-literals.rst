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
would be able to write:

.. code-block:: haskell

  "Tag Team"utf8#    -- Addr# (UTF-8)
  "Tag\x00Team"utf8# -- Addr# (UTF-8 with raw bytes)
  "Foo\x00Bar"utf8## -- ByteArray# (UTF-8 with raw bytes)
  "Юникод"utf8##     -- ByteArray# (UTF-8)
  "Юникод"utf16##    -- ByteArray# (UTF-16, native endian)

Motivation
----------

This proposal addresses several shortcomings with string literals in GHC:

* GHC produces suboptimal generated code when using constant ``ByteArray#``
  terms (often wrapped in ``Data.Primitive.ByteArray`` at the top level).
  The ``ByteArray`` thunk that wraps the ``ByteArray#`` gets forced every
  time it is accessed.
* There is no O(1) way to get the length of a primitive string
  literal. `Trac 5218 <https://ghc.haskell.org/trac/ghc/ticket/5218>`_.
* There is no syntactically simple way to write textual
  non-ASCII ``Addr#`` (e.g. literals in UTF-8 or UTF-16).
  Related ticket: `Trac 5877 <https://ghc.haskell.org/trac/ghc/ticket/5877>`_.
* GHC has no mechanism for generating efficient instructions when
  users want to case on ``ByteArray#``. Users cannot currently case on
  ``ByteArray#`` at all and must often resort to performing a series
  of equality tests.

This proposal introduces provides a mechanism for literals of two types:
``ByteArray#`` and ``Addr#``. It does with without extending the language
syntactically. Consequently, all programs that compile with GHC today will
continue to compile after this proposal is implemented. Their meaning will
not change.

Recap: String desugaring currently
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Currently, it's possible to create primitive ``Addr#`` string literals:

.. code-block:: haskell

  "hello"# -- :: Addr#

Such literals encode text as UTF-8. Additionally, arbitrary octets are
supported via hexadecimal and decimal escape sequences. A null byte is
added to the end of the sequence.

``String`` literals like ``"hello"``, ``"Юникод"``, ``"\NUL"`` are desugared
using <Modified UTF-8 https://en.wikipedia.org/wiki/UTF-8#Modified_UTF-8>_.
For example:

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
quasiquoters would be built in. Here are some examples of ``ByteArray#``
literals under this scheme::

    [octets#|fe01bce8|] -- ByteArray# (four bytes)
    [utf8#|Araña|]      -- ByteArray# (UTF-8)
    [utf16#|Araña|]     -- ByteArray# (UTF-16, native endian)
    [utf16le#|Araña|]   -- ByteArray# (UTF-16, little endian)
    [utf16be#|Araña|]   -- ByteArray# (UTF-16, big endian)

The five quasiquoters showcased above would be built in to GHC. The
resulting ``ByteArray#`` literals would not be null-terminated. The
textual quasiquotes (those that start with ``utf``) do not support
escape sequences. The ``octets#`` quasiquoter only supports hexadecimal
characters, and the number of characters must be even. GHC will throw
an error at compile-time if an odd number of hexadecimal characters
are given as the argument to ``octets#``.

These literals can be used both as values and as a way to scrutize a
``ByteArray#`` that has been cased on. Casing would look like this:

.. code-block:: haskell

  readSmallNumber :: ByteArray# -> Int#
  readSmallNumber x = case x of
    [utf8#|one|] -> 1#
    [utf8#|two|] -> 2#
    [utf8#|three|] -> 3#
    _ -> 4#

When compiling STG to cmm, GHC has an opportunity to generate very
good code for case expressions like this. For lengthier case expressions
that test against dozens of strings, GHC could emit code that performs
perfect or nearly-perfect hashing. Currently, that burden is pushed onto
program authors.

Only one optimization is mandated by this proposal: GHC must perform
constant-folding when ``sizeofByteArray#`` is applied to a ``ByteArray#``
literal.

Effect and Interactions
-----------------------

Future proposals may build on top of this one to improve the desugaring
of string literals. This proposal does not change the way that string
literals are desugared, but it does lay important groundwork that any
future proposal would build on.

Compact regions. All ``ByteArray#`` literals are considered pinned, but
unlike explicitly pinned ``ByteArray#`` literals, they can be copied into
a compact regions. Technically, they would not actually be copied. The
compact region is allowed to point to them because they are static data
that cannot be GCed.

Users in need of other encodings could use template haskell to provide
additional non-built-in quasiquoters.

Costs and Drawbacks
-------------------

None that the author is aware of.

Unresolved questions
--------------------

Should we support all encoding ``iconv`` supports? I think that it is best
to keep the list small.

Implementation Plan
-------------------

There are three phases for implementation:

1. Add ``ByteArray#`` literals to GHC Core. Support them with built-in
   quasiquoters. Andrew Thaddeus Martin will implement this.
2. Allow casing on values of type ``ByteArray#`` with ``ByteArray#`` literals.
   Desugar this to nearly-perfect hashing in cmm. Andrew Thaddeus Martin will
   implement this.
3. Allow ``ByteArray#`` literals to appear in all other expected places.
   Float them all to the top level. It is not known who will implement this.

Phase 1 and 2 can be merged without phase 3 being completed. There is
plenty of value in being able to case on values of type ``ByteArray#``
even without being able to use literals elsewhere.
