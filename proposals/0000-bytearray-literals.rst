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

  "Literals"#         -- Addr# (Modified UTF-8)
  "\xef\xbb\xbf"utf8# -- Addr# (UTF-8)
  "Юникод"utf8##      -- ByteArray# (UTF-8)
  "Юникод"utf16##     -- ByteArray# (UTF-16, native endian)

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

This proposal introduces additional syntax for two types: ``ByteArray#`` and
``Addr#``. It causes GHC to reject some code that is currently accepted. It
does not change the meaning of any programs that continue compiling.

Recap: String desugaring currently
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Currently, it's possible to create primitive ``Addr#`` string literals:

.. code-block:: haskell

  "hello"# -- :: Addr#

These literals are octet sequences. That is, primitive string literals may
only have characters <= '\xFF'. (Most BMP characters cannot be represented).

Strings with non-ASCII characters, like ``"hello"``, ``"Юникод"``, ``"\NUL"``,
are desugared as:

.. code-block:: haskell

  unpackCString# "hello"#
  unpackCStringUtf8# "\208\174\208\189\208\184\208\186\208\190\208\180"#
  unpackCString# "\192\128"#

The current representations are null-terminated and encoded with Modified UTF-8
so that literals may contain the ``NUL`` character.

Proposed Change Specification
-----------------------------

The new syntax for the primitive string literals uses a prefix before the
magic hash. The suffix is the name of an encoding::

  "foo"(utf8#|utf16#|utf8##|utf16##|)

The meaning of this syntax is:

* The number of hashes encodes the type:

  * ``#``: ``Addr#``
  * ``##``: ``ByteArray#``

* The prefix is one of three encodings:

  * ``utf8``
  * ``utf16`` (native endian)
  * Omitted: Modified UTF-8, which is currently used for ``Addr#``.
    This makes the proposal backwards-compatible. This may only be
    used with ``Addr#``, not with ``ByteArray#``. GHC will emit
    an error on this literal ``"foo"##``. The reasoning is that
    Modified UTF-8 is not useful when working with types like
    ``ByteArray#`` that prefix a byte sequence with its length.
    Such a literal would only be written by accident and could
    lead to confusing and difficult-to-diagnose behavior.

GHC will throw an error at compile-time if invalid Unicode
surrogates are present. For example, GHC would reject

.. code-block:: haskell

  "\xd8000"utf16##

with an error message reading:

::

  Invalid character or shift sequence at the end of the buffer.

The encodings other than Modified UTF-8 (UTF-8 and UTF-16) will not
be terminated with a null byte.

To simplify implementation and prevent confusing messages from the
compiler, this proposal goes one step further than just recognizing
the new syntax. When ``MagicHash`` is enabled, ``utf8#``, ``utf8##``,
``utf16#``, and ``utf16##`` are reserved as keywords. Users may no
longer use these as identifiers.

These literals can be used both as values and as a way to scrutize a
``ByteArray#`` that has been cased on. Casing would look like this:

.. code-block:: haskell

  readSmallNumber :: ByteArray# -> Int#
  readSmallNumber x = case x of
    "one"utf8# -> 1#
    "two"utf8# -> 2#
    "three"utf8# -> 3#
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

Costs and Drawbacks
-------------------

Today, with ``MagicHash``, users may write ``"foo"utf8#`` as an expression
that means: apply the function ``"foo"`` to the argument ``utf8#``. This
proposal deprives users of that freedom.

Unresolved questions
--------------------

Should we support all encoding ``iconv`` supports? I think that it is best
to keep the list small.

Implementation Plan
-------------------

There are two phases for implementation:

1. Make the parser recognize the new syntax. Allow casing on values of type
   ``ByteArray#`` with ``ByteArray#`` literals. Desugar this to nearly-perfect
   hashing in cmm. Andrew Thaddeus Martin will implement this.
2. Allow ``ByteArray#`` literals to appear in all other expected places.
   Float them all to the top level. It is not known who will implement this.

Phase 1 can be merged without phase two being completed. There is
plenty of value in being able to case on values of type ``ByteArray#``
even without being able to use literals elsewhere.
