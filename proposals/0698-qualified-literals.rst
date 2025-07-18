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

``OverloadedStrings`` and ``OverloadedLists`` are useful for writing normal string/list syntax that get desugared to non-built-in types. However, they do this via typeclasses, which inherits issues like type inference ambiguity. The extensions are also enabled globally, so if you turn it on, all string literals become overloaded, even if you only want specific locations to be overloaded. It's possible these issues are one reason why these extensions, despite being in GHC for a long time, are not defaults in GHC202X language editions. Furthermore, they could provide even more flexibility; one note in the original ``OverloadedLists`` `design <https://gitlab.haskell.org/ghc/ghc/-/wikis/overloaded-lists>`_ mentions the inability to support heterogeneous lists.

These extensions allow extensibility of string and list literals, but there's currently no option to allow extending other literals, like numeric literals. Because the ``fromInteger`` function is a part of ``Num``, there's no way to overload numeric literals without providing a specification for all the ``Num`` operations. Even if we broke out ``fromInteger`` into a separate class, we still inherit the same typeclass issues as overloaded strings/lists.

The initial motivation for this came from `String interpolation <https://github.com/ghc-proposals/ghc-proposals/pull/570>`_, where this syntax could provide a mechanism similar to Javascript's `tagged template literals <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals#tagged_templates>`_. But it applies more generally to other literals as well.

Semi-related to https://github.com/ghc-proposals/ghc-proposals/issues/438.

Proposed Change Specification
-----------------------------

Introduce ``-XQualifiedNumbers``, ``-XQualifiedStrings``, and ``-XQualifiedLists`` that desugar literals syntax to function calls in a similar way to ``-XQualifiedDo`` (`docs <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html>`_, `proposal <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0216-qualified-do.rst>`_).

General comments:

* As long as the desugared expressions/patterns type check, users are free to define these functions however they want.

* No whitespace is allowed between the ``.`` and the module name / literal.

* Some literals are not supported yet (Chars, unboxed literals) due to lack of use-cases, but could be extended in the future.

* Future work could be done to allow compile time logic, e.g. ``$Foo.1`` => ``$(Foo.fromNumeric [|1|])``, but that is out of scope of this proposal.

QualifiedNumbers
~~~~~~~~~~~~~~~~

With ``-XQualifiedNumbers``, we gain the following syntaxes:

.. list-table::
    :align: left

    * - **New expression syntax**
      - **Desugared expression syntax**
    * - ``Foo.1``
      - ``Foo.fromNumeric (1 :: Natural)``
    * - ``Foo.(1)``
      - ``Foo.fromNumeric (1 :: Natural)``
    * - ``Foo.(-1)``
      - ``Foo.fromNumeric (-1 :: Integer)``
    * - ``Foo.(1.2)``
      - ``Foo.fromNumeric (1.2 :: Rational)``

.. list-table::
    :align: left

    * - **New pattern syntax**
      - **Desugared pattern syntax**
    * - ``Foo.1``
      - ``((== Foo.fromNumeric (1 :: Natural)) -> True)``
    * - ``Foo.(1)``
      - ``((== Foo.fromNumeric (1 :: Natural)) -> True)``
    * - ``Foo.(-1)``
      - ``((== Foo.fromNumeric (-1 :: Integer)) -> True)``
    * - ``Foo.(1.2)``
      - ``((== Foo.fromNumeric (1.2 :: Rational)) -> True)``

We use one ``fromNumeric`` function to simplify implementation for any numeric types. Use-cases requiring different implementations for different types may use a type class with instances for ``Natural``, ``Integer``, and/or ``Rational``. See *Section 4.3 Multiple numeric types* for examples.

We distinguish between ``Natural`` and ``Integer`` so that use-cases that want non-negative guarantees can do so. See *Section 4.4 Positive-only literal* for an example.

Parentheses are required for negative integers and rationals, to avoid ambiguity, both in the lexer and for human readers. Parentheses are optional for positive integers.

``Foo.10e6`` will desugar to ``Foo.fromNumeric (10e6 :: Natural)`` if ``NumDecimals`` is enabled, or ``Foo.fromNumeric (10e6 :: Rational)`` otherwise.

QualifiedStrings
~~~~~~~~~~~~~~~~

With ``-XQualifiedStrings``, we gain the following syntaxes:

.. list-table::
    :align: left

    * - **New expression syntax**
      - **Desugared expression syntax**
    * - ``Foo."asdf"``
      - ``Foo.fromString "asdf"``
    * - ``Foo."""asdf"""``
      - ``Foo.fromString "asdf"``

.. list-table::
    :align: left

    * - **New pattern syntax**
      - **Desugared pattern syntax**
    * - ``Foo."asdf"``
      - ``((== Foo.fromString "asdf") -> True)``
    * - ``Foo."""asdf"""``
      - ``((== Foo.fromString "asdf") -> True)``

Multiline strings are desugared to single line strings first, then desugared as a qualified string literal. See `Multiline Strings <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0569-multiline-strings.rst>`_ for more information.

QualifiedLists
~~~~~~~~~~~~~~

With ``-XQualifiedLists``, we gain the following syntaxes:

.. list-table::
    :align: left

    * - **New expression syntax**
      - **Desugared expression syntax**
    * - ``Foo.[]``
      - ``Foo.buildList (\cons nil -> nil)``
    * - ``Foo.[x, y]``
      - ``Foo.buildList (\cons nil -> x `cons` (y `cons` nil))``
    * - ``Foo.[x ..]``
      - ``Foo.buildList (Foo.enumFrom x)``
    * - ``Foo.[x, y ..]``
      - ``Foo.buildList (Foo.enumFromThen x y)``
    * - ``Foo.[x .. y]``
      - ``Foo.buildList (Foo.enumFromTo x y)``
    * - ``Foo.[x, y .. z]``
      - ``Foo.buildList (Foo.enumFromThenTo x y z)``

.. list-table::
    :align: left

    * - **New pattern syntax**
      - **Desugared pattern syntax**
    * - ``Foo.[x, _, y]``
      - ``Foo.FromListCons x (Foo.FromListCons _ (Foo.FromListCons y Foo.FromListNil))``
    * - ``x Foo.: y``
      - ``Foo.FromListCons x y``

One might wonder why this doesn't align more closely with the interface of ``-XOverloadedLists``, e.g. ``Foo.fromListN 3 [x, y, z]``. The reason is to avoid the intermediate list, which would need to typecheck as a list. Similar reason for defining new ``enumFrom`` functions instead of reusing Prelude's. See *Section 4.6 Heterogeneous Lists* for a use-case.

We also decide to do ``Foo.buildList`` instead of something like ``Foo.fromList (x `Foo.cons` Foo.nil)`` so that there's one definition to jump to (e.g. with IDE integrations) instead of three.

To use as patterns, the implementor should define ``FromListCons`` and ``FromListNil`` pattern synonyms, typically with the ``COMPLETE`` pragma specified. We choose to do this instead of ``toList -> [x, _, z]`` because that would also disallow heterogeneous lists.

Future work could be done to allow list comprehensions, e.g. ``Foo.[x * 10 | x <- [1..10]]`` => ``[1..10] `Foo.listCompBind` \x -> Foo.listCompReturn (x * 10)``, but that is out of scope of this proposal.

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
  QualNaturalL ModuleName Natural
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

With ``QualifiedLists``, ``vector`` could define:

::

  module Data.Vector.Qualified where

  buildList :: ((a -> [a] -> [a]) -> [a] -> [a]) -> Vector a
  buildList f = V.fromList (GHC.List.build f)

  pattern FromListCons a b <- (V.uncons -> Just (a, b))
  pattern FromListNil <- (V.uncons -> Nothing)
  {-# COMPLETE FromListCons, FromListNil #-}

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

With ``QualifiedNumbers``, you could write ``Big.123``, which guarantees that ``Big.fromNumeric`` is only called on literals (e.g. you could configure hlint to ban calling ``BigDecimal.fromNumeric`` directly and only be used via ``QualifiedNumbers``).

::

  -- only called on literals, can be used with any numeric literal: naturals, integers, rationals
  fromNumeric :: Real a => a -> BigDecimal
  fromNumeric = BigDecimal . realToFrac

Multiple numeric types
~~~~~~~~~~~~~~~~~~~~~~

Here's an example of an implementation that's generic to all three numeric types:

::

  module StringInc (fromNumeric) where

  fromNumeric :: (Num a, Show a) => a -> String
  fromNumeric x = "StringInc (" ++ show (x + 1) ++ ")"

* ``StringInc.10`` => ``"StringInc (11)"``
* ``StringInc.(-10)`` => ``"StringInc (-9)"``
* ``StringInc.(10.5)`` => ``"StringInc (23 % 2)"``

Here's an example of an implementation that's different for each numeric type:

::

  module MyMod (fromNumeric) where

  class Impl a where
    fromNumeric :: a -> String

  instance Impl Natural where
    fromNumeric n = "NAT:" ++ show n

  instance Impl Integer where
    fromNumeric n = "NEGATIVE:" ++ show n

  instance Impl Rational where
    fromNumeric n = "FLOAT:" ++ show (realToFrac n :: Double)

* ``MyMod.10`` => ``"NAT:10"``
* ``MyMod.(-10)`` => ``"NEGATIVE:-10"``
* ``MyMod.(10.5)`` => ``"FLOAT:10.5"``

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

With ``QualifiedNumbers``, you could define ``fromNumeric`` only for ``Natural`` and not ``Integer``:

::

  -- In UserId module
  fromNumeric :: Natural -> UserId
  fromNumeric = UserId

  UserId.123 -- works

  UserId.(-1) -- error: (-1) has type Integer, expected Natural

See https://github.com/ghc-proposals/ghc-proposals/issues/438 for more details.

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

Heterogeneous Lists
~~~~~~~~~~~~~~~~~~~

With ``QualifiedLists``, converting list literals are no longer confined to the list type, enabling list literal syntax for heterogenous lists (aka ``HList``):

::

  module Data.HList.Qualified where

  buildList ::
    ( (forall a as. f a -> HList f as -> HList f (a ': as))
      -> HList f '[]
      -> HList f xs
    ) -> HList f xs
  buildList f = f HCons HNil

  pattern FromListCons :: () => xs ~ (x0 ': xs0) => f x0 -> HList f xs0 -> HList f xs
  pattern FromListCons a b = HCons a b

  pattern FromListNil :: () => xs ~ '[] => HList f xs
  pattern FromListNil = HNil

  {-# COMPLETE FromListCons, FromListNil #-}

Users could then do

::

  import Data.HList.Qualified qualified as HList

  HList.[Just True, Just 1, Nothing] :: HList Maybe '[Bool, Int, String]

  -- hlist matches all the patterns below
  case hlist of
    HList.[Just True, _, Nothing] -> _
    HList.[_, Just 1, Nothing] -> _
    Just _ HList.: _ -> _

Effect and Interactions
-----------------------

With ``QualifiedStrings``, there's no more typeclass ambiguity; e.g. the ``text`` library could provide a module like:

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

Backward Compatibility
----------------------

No breakage, as the new syntax is only enabled with the extension.

Furthermore, turning on the extension will generally not break existing code, as the expression would be parsed as function composition between a data constructor and a literal, which would only typecheck if someone adds an ``IsString`` or ``Num`` instance for a function type.

Alternatives
------------

* Use PatternSynonyms for all the patterns, not just lists

  * This makes defining the corresponding pattern for ``fromNumeric :: Real a => a -> Foo`` difficult

* Use ViewPatterns for lists

  * This prevents marking list patterns as COMPLETE

* Avoid explicitly annotating type of numeric literals

  * In the scenario where you only want to allow natural numbers, you could implement ``fromNumeric`` to take in a ``Natural``, but you'd still be relying on GHC to warn that ``-1`` is an overflowed literal.

Unresolved Questions
--------------------

Implementation Plan
-------------------

Brandon Chinn will volunteer to implement.

Endorsements
------------
