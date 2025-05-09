Qualified Literals
==================

.. author:: Brandon Chinn
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/698>`_.
.. sectnum::
.. contents::

This proposal proposes extending ``-XQualifiedDo`` to literals, to enable more ergonomic and more powerful syntax than ``OverloadedStrings`` or ``OverloadedLists``.

Motivation
----------

``OverloadedStrings`` and ``OverloadedLists`` are useful for writing normal string/list syntax that get desugared to non-built-in types. However, they do this via typeclasses, which inherits issues like type inference ambiguity. It's possible these issues are one reason why these extensions, despite being in GHC for a long time, are not defaults in GHC202X language editions. Furthermore, they could provide even more flexibility; one note in the original ``OverloadedLists`` `design <https://gitlab.haskell.org/ghc/ghc/-/wikis/overloaded-lists>`_ mentions the inability to support heterogeneous lists.

These extensions allow extensibility of string and list literals, but there's currently no option to allow extending other literals, like numeric literals. Because the ``fromInteger`` function is a part of ``Num``, there's no way to overload numeric literals without providing a specification for all the ``Num`` operations. Even if we broke out ``fromInteger`` into a separate class, we still inherit the same typeclass issues as overloaded strings/lists.

The initial motivation for this came from `String interpolation <https://github.com/ghc-proposals/ghc-proposals/pull/570>`_, where this syntax could provide a mechanism similar to Javascript's `tagged template literals <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals#tagged_templates>`_. But it applies more generally to other literals as well.

Semi-related to https://github.com/ghc-proposals/ghc-proposals/issues/438

Proposed Change Specification
-----------------------------

High-level Overview
~~~~~~~~~~~~~~~~~~~

``-XQualifiedLiterals`` enables the following syntaxes for expressions

.. list-table::
    :align: left

    * - **New expression syntax**
      - **Desugared expression syntax**
    * - ``Foo.1``
      - ``Foo.fromNatural 1``
    * - ``Foo.(1)``
      - ``Foo.fromNatural 1``
    * - ``Foo.(-1)``
      - ``Foo.fromNegativeInteger (-1)``
    * - ``Foo.(1.2)``
      - ``Foo.fromRational 1.2``
    * - ``Foo."asdf"``
      - ``Foo.fromString "asdf"``
    * - ``Foo."""asdf"""``
      - ``Foo.fromString "asdf"``
    * - ``Foo.[x, y]``
      - ``Foo.buildList (\cons nil -> x `cons` (y `cons` nil))``

And the following syntaxes for patterns

.. list-table::
    :align: left

    * - **New pattern syntax**
      - **Desugared ViewPattern**
    * - ``Foo.1``
      - ``(Foo.matchNatural 1 -> True)``
    * - ``Foo.(1)``
      - ``(Foo.matchNatural 1 -> True)``
    * - ``Foo.(-1)``
      - ``(Foo.matchNegativeInteger (-1) -> True)``
    * - ``Foo.(1.2)``
      - ``(Foo.matchRational 1.2 -> True)``
    * - ``Foo."asdf"``
      - ``(Foo.matchString "asdf" -> True)``
    * - ``Foo."""asdf"""``
      - ``(Foo.matchString "asdf" -> True)``
    * - ``Foo.[x, y]``
      - ``(Foo.listUncons -> Just (x, Foo.listUncons -> Just (y, Foo.listUncons -> Nothing)))``
    * - ``Foo.(x : _)``
      - ``(Foo.listUncons -> Just (x, _))``

As long as the desugared expressions/patterns type check, users are free to define these functions however they want.

Notes:

* Parentheses are required for negative integers and rationals, to avoid ambiguity, both in the lexer and for human readers. Parentheses are optional for positive integers.

* Multiline strings are desugared to single line strings first, then desugared as a qualified string literal.

* Some literals are not supported yet (Chars, unboxed literals) due to lack of use-cases, but could be extended in the future.

* Future work could be done to allow compile time logic, e.g. ``$Foo.1`` => ``$(Foo.fromInteger [|1|])``, but that is out of scope of this proposal.

Parser
~~~~~~

Update `Section 10.5 <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5>`_ of the Haskell 2010 report as follows.

.. code-block:: abnf

  aexp → qvar
       | ...
       | modid . integer
       | modid . ( {-} integer )
       | modid . ( float )
       | modid . string
       | modid . multiLineString
       | modid . [ exp_1 , ..., exp_k ]

  apat → var [ @ apat ]
       | ...
       | modid . integer
       | modid . ( {-} integer )
       | modid . ( float )
       | modid . string
       | modid . multiLineString
       | modid . [ pat_1 , ..., pat_k ]
       | modid . ( pat_1 : ... : pat_k )


Proposed Library Change Specification
-------------------------------------

Template Haskell
~~~~~~~~~~~~~~~~

We'll add the following constructors, to maintain backwards compatibility:

::

  QualListE ModuleName [Exp]

  QualStringL ModuleName String
  QualIntegerL ModuleName Integer
  QualRationalL ModuleName Rational

Examples
--------

Vector
~~~~~~

Currently, if you want to pattern match on vector, you have to use ``OverloadedLists`` (which enables it for list literals in the entire file) or be verbose:

::

  case user of
    -- guard
    User{tags = tags} | ["a", tag2] <- V.toList tags -> _
    -- with ViewPatterns
    User{tags = (V.toList -> ["a", tag2])} -> _

With ``QualifiedLiterals``, ``vector`` could define:

::

  module Data.Vector.Qualified where

  buildList :: ((a -> [a] -> [a]) -> [a] -> [a]) -> Vector a
  buildList f = V.fromList (GHC.List.build f)

  listUncons = V.uncons

And the user could do:

::

  import Data.Vector.Qualified qualified as V

  case user of
    User{tags = V.["a", tag2]} -> _

One scenario this can come up is when parsing ``Aeson.Array``, which stores JSON values in a ``Vector``.

Scientific
~~~~~~~~~~

`Scientific <https://hackage.haskell.org/package/scientific-0.3.8.0/docs/Data-Scientific.html#t:Scientific>`_ represents an arbitrary precision number. It has a ``Num`` instance, but ``+`` and ``-`` are unsafe and can cause OOM. Safety-minded developers might desire to wrap with a newtype that provides ``unsafeAdd`` but not ``+``, to prevent call-sites from accidentally blowing up memory.

::

  newtype BigDecimal = BigDecimal Scientific

  unsafeAdd :: BigDecimal -> BigDecimal -> BigDecimal
  unsafeAdd = coerce (+)

If you want to write ``BigDecimal`` literals (e.g. for tests), you have to use either the ``BigDecimal`` constructor or write a ``big = BigDecimal`` helper, but that's unsafe if accidentally called on a non-literal, as ``Scientific`` throws a runtime error if converting from a repeating decimal.

With QualifiedLiterals, you could write ``Big.123``, which guarantees that ``Big.fromRational`` is only called on literals (e.g. you could configure hlint to ban calling ``BigDecimal.fromRational`` directly and only be used via QualifiedLiterals).

Positive-only literals
~~~~~~~~~~~~~~~~~~~~~~

The following currently fails to compile with ``-Werror`` enabled:

::

  >>> (-1) :: Natural

  error: [GHC-97441] [-Woverflowed-literals, Werror=overflowed-literals]
    Literal -1 is negative but Natural only supports positive numbers

However, this check is hardcoded in the compiler for specific types, e.g. ``Natural``, ``Int*``, and ``Word*``. There's no way to enforce this at compile time for custom types, e.g.

::

  -- Invariant: positive
  newtype UserId = UserId Integer

  UserId (-1) -- works

With QualifiedLiterals, you could just define ``fromNatural`` and not define ``fromNegativeInteger``:

::

  UserId.123 -- works

  UserId.(-1) -- error: UserId.fromNegativeInteger not defined

ByteString
~~~~~~~~~~

It's a `known issue <https://github.com/haskell/bytestring/issues/140>`_ that ByteString has surprising ``IsString`` behavior, due to ambiguity in how to handle Unicode characters.

With QualifiedLiterals, ``bytestring`` could define the following modules:

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

Heterogeneous Lists
~~~~~~~~~~~~~~~~~~~

With QualifiedLiterals, converting list literals are no longer confined to the list type, enabling list literal syntax for heterogenous lists (aka ``HList``):

::

  module Data.HList.Qualified where

  buildList ::
    ( (forall a as. f a -> HList f as -> HList f (a ': as))
      -> HList f '[]
      -> HList f xs
    ) -> HList f xs
  buildList f = f HCons HNil

  class Uncons a where
    type UnconsRet a
    listUncons :: a -> Maybe (UnconsRet a)

  instance Uncons (HList f '[]) where
    type UnconsRet (HList f '[]) = Void
    listUncons _ = Nothing

  instance Uncons (HList f (x ': xs)) where
    type UnconsRet (HList f (x ': xs)) = (f x, HList f xs)
    listUncons (HCons x xs) = Just (x, xs)

Users could then do

::

  import Data.HList.Qualified qualified as HList

  HList.[Just True, Just 1, Nothing] :: HList Maybe '[Bool, Int, String]

  -- hlist matches all the patterns below
  case hlist of
    HList.[Just True, _, Nothing] -> _
    HList.[_, Just 1, Nothing] -> _
    HList.(Just _ : _) -> _

Effect and Interactions
-----------------------

With QualifiedLiterals, there's no more typeclass ambiguity; e.g. the ``text`` library could provide a module like:

::

  module Data.Text.Overloaded where

  import Data.Text

  fromString :: String -> Text
  fromString = pack

and users can do

::

  import Data.Text.Overloaded qualified as T

  main = print T."asdf"

The equivalent code with ``OverloadedStrings`` would have failed to compile with ``-Wall -Werror`` enabled (due to type defaulting).


Interactions with other extensions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Multiline strings are supported, as mentioned in the specification

* `Allow arbitrary identifiers as fields in OverloadedRecordDot <https://github.com/ghc-proposals/ghc-proposals/pull/668>`_ has similar syntax to the proposed qualified string literal, but as ``Foo.bar`` is parsed as a qualified identifier even with OverloadedRecordDot, it makes sense that ``Foo."bar"`` is also parsed as a qualified literal.

* `Allow native string interpolation syntax <https://github.com/ghc-proposals/ghc-proposals/pull/570>`_ proposes adding string interpolation syntax with ``s"..."``. If both proposals are accepted, you could have qualified string interpolations with ``Foo.s"..."``. See the other proposal for more details.


Costs and Drawbacks
-------------------

Development and maintenance should be low effort, as the core implementation is in the renamer step, and typechecking would proceed as normal.

The syntax is approachable for novice users and shouldn't be an extra barrier to understand.

``COMPLETE`` is not possible in this proposal, since it's not possible to mark completeness with ``ViewPatterns``. A future proposal with TH (e.g. ``$Foo.123``) could generate patterns that would be recognized as complete, but that's out of scope of this proposal.

Backward Compatibility
----------------------

No breakage, as the new syntax is only enabled with the extension.

Furthermore, turning on the extension will generally not break existing code, as the expression would be parsed as function composition between a data constructor and a literal, which would only typecheck if someone adds an ``IsString`` or ``Num`` instance for a function type.

Alternatives
------------

Unresolved Questions
--------------------

Implementation Plan
-------------------

Brandon Chinn will volunteer to implement.

Endorsements
------------
