.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/135>`_.

.. contents::

ByteArray# literals
===================

This is a proposal to introduce ``ByteArray#`` and ``(# Word#, Addr# #)``
literals, and slightly change ``Addr#`` literals. In short you'll be able
to write

.. code-block:: haskell

  "Literals"#b           -- ByteArray#
  "\xef\xbb\xbf"#abytes  -- Addr#
  "Юникод"#ucp1251       -- (# Int#, Addr# #)

additionally to current

.. code-block:: haskell

  "primitive"#           -- Addr# in utf8
  "string"               -- String or IsString a => a

Also ordinary ``String`` literals will use new (UTF-8) ``ByteArray#`` literals under the hood (instead of currently used problematic Modified-UTF8 ``Addr#``).

Motivation
------------

This proposal tries to address multiple string literal usage shortcomings in GHC

* There is no way to get the length of a primitive string literal in O(1). `Trac 5218 <https://ghc.haskell.org/trac/ghc/ticket/5218>`_.
* There is no syntactically simple way to write textual non-ASCII ``Addr#`` (e.g. literals in UTF-8 or UTF-16). Related ticket: `Trac 5877 <https://ghc.haskell.org/trac/ghc/ticket/5877>`_.
* ``Addr#`` is compared via pointer equality, which is semantically wrong when working with text. `Trac 11312 <https://ghc.haskell.org/trac/ghc/ticket/11312>`_.
* Bytes and Characters are mixed up in general.
* ``ByteArray#`` literals can be used to implement better ``mkInteger``. `Trac 9719: Improve mkInteger interface <https://ghc.haskell.org/trac/ghc/ticket/9719>`_.

We need the syntax for all three types:
``ByteArray#``, ``Addr#`` and unboxed sum as they are all slightly different,
as demonstrated by following table

======================  ===========  =======  ========  ===========
 Type                    primitive    boxed    lifted    algebraic
======================  ===========  =======  ========  ===========
``Addr#``               Yes          No       No        No
``(# Int#, Addr# #)``   Yes          No       No        Yes
``ByteArray#``          Yes          Yes      No        No
======================  ===========  =======  ========  ===========


Recap: String desugaring currently
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Currently, it's possible to create primitive ``Addr#`` string literals:

.. code-block:: haskell

  "hello"# -- :: Addr#

These literals are ``[Word8]`` literals, *primitive string literal must contain only characters <= '\xFF'*.

Ordinary strings, like ``"hello"``, ``"Юникод"``, ``"\NUL"`` are then desugared as

.. code-block:: haskell

  unpackCString# "hello"#
  unpackCStringUtf8# "\208\174\208\189\208\184\208\186\208\190\208\180"#
  unpackCString# "\192\128"#

Current representations are null-terminated, using *Modified UTF-8* for literals
containing the NUL character.

Proposed Change Specification
-----------------------------

Primitive string literals
~~~~~~~~~~~~~~~~~~~~~~~~~

The new syntax for the primitive string literals uses a suffix after the magic hash:

.. code-block:: haskell

  "foo"#[abu]encoding

where

* the first letter specifies the type of the literal:

  * ``a`` - ``Addr#``
  * ``b`` - ``ByteArray#``
  * ``u`` - Unboxed sum: ``(# Int#, Addr# #)``

  The syntax is extensible, if there will be need for new types of primitive strings.

* the rest of the suffix is the encoding, which is one of following

  * ``Utf8``
  * ``Ascii``
  * ``Latin1``
  * ``Bytes``, which is the synonym for ``Latin1``, but captures the intent better.
  * ``Utf16``, ``Utf16le``, ``Utf16be``, ``Utf32``, ``Utf32le``, ``Utf32be``

  Encoding is matched case-insensitively. Encoding may be omitted, in that case ``Utf8`` is assumed.

If GHC encounters a literal which cannot be faithfully represented in the target encoding an error will be raised. For example

.. code-block:: haskell

  "ÄETSÄ"#bAscii

will fail with an error:

::

  primitive ASCII string literal must contain only characters <= '\xFF

Error will be raised also if invalid Unicode surrogates are present, as
it's an occurrence of the error case above.

.. code-block:: haskell

  "\xd8000"#bUtf16

will fail with an error

::

  invalid character or shift sequence at the end of the buffer.

The representations will still be NUL terminated. There is little reason
to not to add additional byte. Therefore

    "\NUL\NUL"#aASCII

would produce a *three* bytes: 0x00 0x00 0x00.

To summarize, there are various errors which may be raised,

- Characters not representable in character set (e.g. a Unicode character in an ASCII string)
- An unencodable codepoint (e.g. a surrogate codepoint in a UTF-8 string)
- An invalid sequence (e.g. an unpaired surrogate in a UTF-16 string)


*Note:* If you like to play with incorrect encodings try:

::

  perl -e 'print "a\x00b\x00c\x00\x00\xd8"'|iconv -f utf16le -t utf8|hexdump -C

You may append two bytes to the input, try to make correct surrogate pair!


Primitive string without modifier
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The current primitive string

.. code-block:: haskell

  "hello"#

will mean the same as

.. code-block:: haskell

  "hello"#aLatin1

Therefore, code with current primitive strings won't break.

*Unresolved:* Should there be a (``-Wall``) warning in this case, asking user to be explicit?


String syntax desugaring
~~~~~~~~~~~~~~~~~~~~~~~~

The ordinary Haskell ``String``

.. code-block:: haskell

  "hello"
  "unicode ∀"
  "\NUL"

will be desugared as an ASCII literal when possible, otherwise as a *non-modified* UTF8 literal:

.. code-block:: haskell

 unpackAsciiByteArray# "hello"bAscii
 unpackUtf8ByteArray# "unicode ∀"#bUtf8
 unpackAsciiByteArray# "\NUL"#bAscii

where ``unpackAsciiByteArray#`` is defined as

.. code-block:: haskell

 unpackAsciiByteArray# ba = unpackAsciiCStringWithLen#
   (sizeofByteArray# ba) (byteArrayContents# ba)

and similarly for UTF-8 variant. *Note* we don't go through unboxed sum, so `Trac 1257: Bytecode generator can't handle unboxed tuples <https://ghc.haskell.org/trac/ghc/ticket/1257>`_ is not a problem.

Desugaring to ``ByteArray#`` literals will allow GHC to eliminate common (sub)-expressions.

UTF8 literals will use non-modified UTF8 encoding, as literals have known length.

Ben Gamari performed a quick experiment: An additional overhead of using
``ByteArray#`` literals for all strings is acceptably small.
`Trac 5128, comment 95 <https://ghc.haskell.org/trac/ghc/ticket/5218#comment:95>`_.


Conversions
~~~~~~~~~~~

There are (and will be) libraries which don't use UTF8 as internal encoding of
textual data. To avoid unneeded run-time conversions of the literal strings. We
should provide conversions of ``ByteArray#`` from UTF8 to other Unicode
encodings:

.. code-block:: haskell

  recodeUtf8ToUtf16 :: ByteArray# -> ByteArray#
  recodeUtf8ToUtf32 :: ByteArray# -> ByteArray#
  -- etc, note ASCII is valid UTF8

The GHC's constant folding / rewrite mechanisms will be able to avoid creation
of intermediate UTF8 encoded ``ByteArray#``.

The conversion primops are also useful in non-static contexts.

*Note:* this is not possible current ``Addr#`` as a ``String`` literal "backend".

Fow now I don't propose to add recoding primops for ``Addr#``
``Addr#`` is an arbitrary machine address outside the garbage-collected heap:
how to create new ones? (AFAICS there aren't such primops currently).


Effect and Interactions
-----------------------

Library authors may provide `RULES` to avoid looping to get the length of the string (``Prim.Builder``  in ``bytestring`` could simply ``memcpy`` the contents of a literal),
or avoid roundtripping through `String` (also in ``bytestring``):

.. code-block:: haskell

  data ShortByteString = SBS ByteArray#
 
  instance FromString ShortByteString where
    fromString = packChars
 
  {-# RULES packChars (unpackAsciiByteArray# ba) = SBS ba #-}
  {-# RULES packChars (unpackUtf8ByteArray# ba) = SBS ba #-}

  packChars :: ByteArray# -> ShortByteString
  packChars = SBS

This will resolve `Trac 5218 <https://ghc.haskell.org/trac/ghc/ticket/5218>`_.

Similarly, for textual types (`compare with current <http://hackage.haskell.org/package/text-short-0.1.2/docs/src/Data.Text.Short.Internal.html#line-1455>`_)

.. code-block:: haskell

  -- short text in UTF-16
  data ShortText16 = ST16 ByteArray#

  {-# RULES "ShortText literal UTF-8" forall s . fromStringLit (GHC.unpackUtf8ByteArray# s) = fromLitMUtf8Addr# s #-}

  fromLitUtf8Addr# :: ByteArray# -> ShortText
  fromLitUtt8Addr# ba = ST (recodeUtf8ToUtf16# ba)

Other effects and interactions are:

- ``ByteArray#`` is a ``String#`` asked for in `Trac 11312 (comment 3) <https://ghc.haskell.org/trac/ghc/ticket/11312#comment:3>`_,
  the only missing part is way to create statically allocated ``ByteArray#``. This proposal addresses that.

- ``ByteArray#`` literals can be used to implement better ``mkInteger``. `Trac 9719: Improve mkInteger interface <https://ghc.haskell.org/trac/ghc/ticket/9719>`_.

- This proposal will affect ``template-haskell``.
  We'll need to (again) change the type of ``StringPrimL`` constuctor to include the result-type tag.
  Tag type can also be used to implement encoding-tagged bytearray# for safer conversions
  (`Unlifted newtypes <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0013-unlifted-newtypes.rst>`_ will allow that once implemented).

Costs and Drawbacks
-------------------

This would be a potentially breaking change for any code using unboxed string literals.

Alternatives
------------

One proposed alternative is to use double hash syntax ``"foo"##`` to represent
UTF8 ``ByteArray#``.  That variant is very limited in power compared to proposed.

In fact, we might introduce similar syntax for the number literals, e.g. ``120#i8``, if we get `Int8#` primitive type.

Unresolved questions
--------------------

Invalid Unicode
~~~~~~~~~~~~~~~

This proposal explicitly forbits invalid Unicode surrogates.
Currently ``String`` literal

.. code-block:: haskell

  "\xd8000"

is accepted. By this proposal it would desugar into

.. code-block:: haskell

  unpackUtf8ByteArray "\xd8000"bUtf8

which should be rejected, as it's invalid UTF8.

There is a [a Phab diff: *Make ``Char`` & literals Unicode-correct by construction*](https://phabricator.haskell.org/D3818),
which would resolve this.

Tools like ``alex`` or ``happy``, which need to construct ``ByteArray#`` or
``Addr#`` literals can use the direct syntax, as they do now.

List of encodings
~~~~~~~~~~~~~~~~~

Should we support all encoding ``iconv`` supports?

The proposal is conservative and picks only few encodings.
The list is more easily extended than shorten in the future.


Implementation Plan
-------------------

Ben Gamari expressed interest in implementing the proposal.
