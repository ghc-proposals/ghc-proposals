Qualified Strings
=================

.. author:: Brandon Chinn
.. date-accepted:: 2025-10-07
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/26503
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/723>`_.
.. sectnum::
.. contents::

This proposal proposes replicating ``-XQualifiedDo`` for literal strings, to enable more ergonomic and more powerful syntax than ``OverloadedStrings``. Another way to view this proposal would be replicating ``-XRebindableSyntax`` for literal strings, but only within a local scope.

See also:

* `QualifiedLists <https://github.com/ghc-proposals/ghc-proposals/pull/724>`_
* `QualifiedNumerics <https://github.com/ghc-proposals/ghc-proposals/pull/725>`_

Motivation
----------

Problems with Type Class-driven overloading
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``OverloadedStrings`` works by desugaring to a type-class-overloaded function. That leads to certain general shortcomings:

* It is a module-wide setting

  * Anecdotally, people would rather avoid ``OverloadedStrings`` than deal with overloaded strings in the entire module.

  * It's possible that this is one reason this extension isn't a default in GHC202X language editions, despite being in GHC for a long time.

* Type inference ambiguity.

  * Consider the following code:

    ::

      import Data.Text qualified as T

      output :: IsString s => s -> IO ()

      main = do
        -- Rejected by typechecker if OverloadedStrings is enabled
        output "hello"
        output "world"

        -- -- Requires OverloadedStrings
        -- output $ T.replace " " "_" input

    This originally works with no extensions, due to the string literals being typed to concrete ``String``. But if the developer wants to use ``Text`` literals with ``T.replace``, adding ``OverloadedStrings`` would cause ambiguity to the existing locations because they are now no longer concretely ``String``.

This proposal would allow using a module qualifier to say precisely which function to desugar to, rather than using type classes, in a similar manner as ``-XQualifiedDo``. This would allow writing the previous code as

::

  {-# LANGUAGE QualifiedStrings #-}

  main = do
    output "hello"
    output "world"

    output $ T.replace T." " T."_" input

The existing locations would continue working as ``String``, while the new line would unambiguously desugar to ``T.replace (T.pack " ") (T.pack "_") input``.

Proposed Change Specification
-----------------------------

Introduce ``-XQualifiedStrings`` that desugars literal string syntax to function calls in a similar way to ``-XQualifiedDo`` (`docs <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html>`_, `proposal <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0216-qualified-do.rst>`_).

As long as the desugared expressions/patterns type check, users are free to define these functions however they want. No whitespace is allowed between the ``.`` and the module name / literal.

Currently, string literals have the following desugaring:

.. list-table::
    :align: left

    * - **Expression**
      - **Enabled extensions**
      - **Desugared expression syntax**
    * - ``"hello"``
      -
      - ``"hello"``
    * - ``"hello"``
      - ``-XOverloadedStrings``
      - ``GHC.Exts.fromString "hello"``
    * - ``"""hello"""``
      - ``-XMultilineStrings``
      - ``"hello"``

With ``-XQualifiedStrings``, we gain the following syntaxes:

.. list-table::
    :align: left

    * - **New expression syntax**
      - **Additional extensions**
      - **Desugared expression syntax**
    * - ``M."asdf"``
      -
      - ``M.fromString "asdf"``
    * - ``M."""asdf"""``
      - ``-XMultilineStrings``
      - ``M.fromString "asdf"``

.. list-table::
    :align: left

    * - **New pattern syntax**
      - **Additional extensions**
      - **Desugared pattern syntax**
    * - ``M."asdf"``
      -
      - ``((== M.fromString "asdf") -> True)``
    * - ``M."""asdf"""``
      - ``-XMultilineStrings``
      - ``((== M.fromString "asdf") -> True)``

It is highly recommended that all types with ``IsString`` instances include a top-level ``fromString`` function, to enable locally-scoped overloading over ``-XOverloadedStrings``:

::

  module Data.MyString where

  import Data.String qualified as S

  data MyString = ...

  instance S.IsString MyString where
    fromString = ...

  -- Alternatively, this can be defined in aonther
  -- module like Data.MyString.Qualified
  fromString :: String -> MyString
  fromString = S.fromString

Qualified multiline strings are only allowed if ``-XMultilineStrings`` is enabled. Qualified multiline strings are desugared to single line strings first, then desugared as a qualified string literal. See `Multiline Strings <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0569-multiline-strings.rst>`_ for more information.

Parser
~~~~~~

Update `Section 10.5 <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5>`_ of the Haskell 2010 report as follows.

.. code-block:: abnf

  aexp → qvar
       | ...
       | modid . string
       | modid . multiLineString

  apat → var [ @ apat ]
       | ...
       | modid . string
       | modid . multiLineString

Module name resolution
~~~~~~~~~~~~~~~~~~~~~~

Module names are resolved immediately, when parsing a quote. This matches the behavior of resolving modules in normal qualified values in quotes.

::

  module A where

  import OneImpl qualified as M

  -- Immediately resolves to OneImpl."foo"
  -- Errors if M is not in scope
  foo = [| M."foo" |]

Proposed Library Change Specification
-------------------------------------

Template Haskell
~~~~~~~~~~~~~~~~

We'll add the following constructors instead of modifying the existing ``StringL`` constructor, to maintain backwards compatibility:

::

  data Lit
    = ...
    | QualStringL ModName String

Examples
--------

ByteString
~~~~~~~~~~

It's a `known issue <https://github.com/haskell/bytestring/issues/140>`_ that ByteString has surprising ``IsString`` behavior, due to ambiguity in how to handle Unicode characters.

With ``QualifiedStrings``, ``bytestring`` could define the following modules:

::

  module Data.ByteString.Qualified.Ascii where

  -- truncates unicode
  fromString :: String -> ByteString
  fromString = Char8.pack

  module Data.ByteString.Qualified.Utf8 where

  -- encodes unicode
  fromString :: String -> ByteString
  fromString = BS.toStrict . BS.toLazyByteString . BS.stringUtf8

Users would then be forced to decide what behavior they want (and can switch between the two!):

::

  import Data.ByteString qualified as BS
  import Data.ByteString.Qualified.Ascii qualified as Ascii
  import Data.ByteString.Qualified.Utf8 qualified as Utf8

  main = do
    -- [98,108,97,158]
    print $ BS.unpack Ascii."bla語"

    -- [98,108,97,232,170,158]
    print $ BS.unpack Utf8."bla語"

Effect and Interactions
-----------------------

With ``QualifiedStrings``, there's no more typeclass ambiguity; e.g. the ``text`` library could provide a module like:

::

  module Data.Text.Qualified where

  import Data.Text

  fromString :: String -> Text
  fromString = pack

and users can do

::

  import Data.Text.Qualified qualified as T

  main = print T."asdf"

The equivalent code with ``OverloadedStrings`` would have failed to compile with ``-Wall -Werror`` enabled (due to type defaulting).


Interactions with other extensions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Related to `QualifiedLists <https://github.com/ghc-proposals/ghc-proposals/pull/724>`_ and `QualifiedNumerics <https://github.com/ghc-proposals/ghc-proposals/pull/725>`_, but all three proposals are orthogonal to each other.

* Qualified multiline strings are allowed when ``-XMultilineStrings`` is enabled, as mentioned in the specification

* `Allow arbitrary identifiers as fields in OverloadedRecordDot <https://github.com/ghc-proposals/ghc-proposals/pull/668>`_ has similar syntax to the proposed qualified string literal, but as ``M.bar`` is parsed as a qualified identifier even with OverloadedRecordDot, it makes sense that ``M."bar"`` is also parsed as a qualified literal.

* `Allow native string interpolation syntax <https://github.com/ghc-proposals/ghc-proposals/pull/570>`_ proposes adding string interpolation syntax with ``s"..."``. If both proposals are accepted, this syntax could provide a mechanism similar to Javascript's `tagged template literals <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals#tagged_templates>`_. See the other proposal for more details.

Costs and Drawbacks
-------------------

Development and maintenance should be low effort, as the core implementation is in the renamer step, and typechecking would proceed as normal.

The syntax is approachable for novice users and shouldn't be an extra barrier to understand.

Backward Compatibility
----------------------

No breakage, as the new syntax is only enabled with the extension.

Furthermore, turning on the extension will generally not break existing code. Any existing code written as ``M."asdf"`` would be parsed as function composition between a data constructor and a literal, which would only typecheck if someone adds an ``IsString`` instance for a function type.

Alternatives
------------

* Use PatternSynonyms for string literals in patterns

  * The View pattern more closely matches `Section 3.17.2 <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-60015x7>`_ in the 2010 Report

Future work
~~~~~~~~~~~

* Some literals are not supported yet (Chars, unboxed literals) due to lack of use-cases, but could be extended in the future.

* Future work could be done to allow compile time logic, e.g. ``$M."hello"`` => ``$(M.fromString [|"hello"|])``, but that is out of scope of this proposal.

Implementation Plan
-------------------

Brandon Chinn will volunteer to implement.
