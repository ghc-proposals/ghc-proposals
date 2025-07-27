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

Problems with Type Class-driven overloading
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Haskell98 numeric literals, ``OverloadedLists``, and ``OverloadedStrings`` all work by desugaring to a type-class-overloaded function. That leads to certain general shortcomings:

* They are module-wide settings

  * Anecdotally, people would rather avoid ``OverloadedLists`` and ``OverloadedStrings`` than deal with overloaded lists/strings in the entire module.

  * It's possible that this is one reason these extensions aren't defaults in GHC202X language editions, despite being in GHC for a long time.

* Type inference ambiguity.

  * Consider the following code:

    ::

      output :: IsString s => s -> IO ()

      main = do
        output "hello"
        output "world"

        -- output $ T.replace " " "_" input

    This originally works with no extensions, due to the string literals being typed to concrete ``String``. But say the developer wants to add a call to ``T.replace`` and use ``Text`` literals; adding ``OverloadedStrings`` would cause ambiguity to the existing locations because they are now no longer concretely ``String``.

    This is less of an issue with numeric literals due to defaulting, but that still causes compilation errors with ``-Wall -Werror``.

This proposal would allow using a module qualifier to say precisely which function to desugar to, rather than using type classes, in a similar manner as ``-XQualifiedDo``. This would allow writing the previous code as

::

  {-# LANGUAGE QualifiedStrings #-}

  main = do
    output "hello"
    output "world"

    output $ T.replace T." " T."_" input

The existing locations would continue working as ``String``, while the new line would unambiguously desugar to ``T.pack " "``.

No granular typeclass for numeric literals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``fromInteger`` and ``fromRational`` are part of ``Num`` and ``Fractional``, so there's no way to use numeric literal syntax for custom types that shouldn't implement operators like ``+``.

Related: https://github.com/ghc-proposals/ghc-proposals/issues/438

Inability to distinguish natural numbers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In Haskell98, ``13`` desugars to ``fromInteger 13`` and ``2.7`` desugars to ``fromRational 2.7``. If a type ``T`` does not wish to support rationals, one could simply fail to provide an instance for ``Fractional T``, then ``fromRational 2.7 :: T`` will be statically rejected. But if ``T`` does not want to support negative integers, there is no way to reject it statically.

This proposal would desugar natural numbers separately from negative integers so that implementations that wish to distinguish between the two (e.g. support only natural numbers) may do so.

Inability to use heterogeneous lists
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With ``-XOverloadedLists`` we can never write the literal ``[4, "foo", True]``, becuase that desugars to ``fromList [4, "foo", True]`` which is ill-typed regardless of ``fromList``. That is annoyingly restrictive, because with heterogeneous lists, it's perfectly fine to write

::

  4 `HCons` "foo" `HCons` True `HCons` HNil :: HList [Int, String, Bool]

and it would be convenient to use list literals instead. This was even explicitly listed as a restriction in the original ``OverloadedLists`` `design <https://gitlab.haskell.org/ghc/ghc/-/wikis/overloaded-lists>`_.

This proposal would desugar list literals to a build-like form instead, so that ``M.[4, "foo", True]`` desugars to

::

  M.buildList 3 (\cons nil -> 4 `cons` ("foo" `cons` (True `cons` nil)))

For a suitable ``M.buildList``, this is enough to support heterogenous list literals: see *Section 4.6 Heterogeneous Lists*.

Proposed Change Specification
-----------------------------

Introduce ``-XQualifiedNumbers``, ``-XQualifiedStrings``, and ``-XQualifiedLists`` that desugar literals syntax to function calls in a similar way to ``-XQualifiedDo`` (`docs <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html>`_, `proposal <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0216-qualified-do.rst>`_).

General comments:

* As long as the desugared expressions/patterns type check, users are free to define these functions however they want.

* No whitespace is allowed between the ``.`` and the module name / literal.

* Some literals are not supported yet (Chars, unboxed literals) due to lack of use-cases, but could be extended in the future.

* Future work could be done to allow compile time logic, e.g. ``$Foo.1`` => ``$(Foo.fromNatural [|1|])``, but that is out of scope of this proposal.

QualifiedNumbers
~~~~~~~~~~~~~~~~

With ``-XQualifiedNumbers``, we gain the following syntaxes:

.. list-table::
    :align: left

    * - **New expression syntax**
      - **Desugared expression syntax**
    * - ``Foo.1``
      - ``Foo.fromNatural 1``
    * - ``Foo.(1)``
      - ``Foo.fromNatural 1``
    * - ``Foo.(-1)``
      - ``Foo.fromNegativeInt (-1)``
    * - ``Foo.(1.2)``
      - ``Foo.fromRational 1.2``

.. list-table::
    :align: left

    * - **New pattern syntax**
      - **Desugared pattern syntax**
    * - ``Foo.1``
      - ``((== Foo.fromNatural 1) -> True)``
    * - ``Foo.(1)``
      - ``((== Foo.fromNatural 1) -> True)``
    * - ``Foo.(-1)``
      - ``((== Foo.fromNegativeInt (-1)) -> True)``
    * - ``Foo.(1.2)``
      - ``((== Foo.fromRational 1.2) -> True)``

We distinguish between ``Natural`` and negative ``Integer`` so that use-cases that want non-negative guarantees can do so. If we only had one ``fromInteger``, you could type it as ``fromInteger :: Natural -> ...``, but it would be relying on the hardcoded ``-Woverflowed-literals`` compiler check.

Parentheses are required for negative integers and rationals, to avoid ambiguity, both in the lexer and for human readers. Parentheses are optional for positive integers.

``Foo.10e6`` will desugar to ``Foo.fromNatural 10e6`` if ``NumDecimals`` is enabled, or ``Foo.fromRational 10e6`` otherwise.

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

Qualified multiline strings are only allowed if ``-XMultilineStrings`` is enabled. Qualified multiline strings are desugared to single line strings first, then desugared as a qualified string literal. See `Multiline Strings <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0569-multiline-strings.rst>`_ for more information.

QualifiedLists
~~~~~~~~~~~~~~

With ``-XQualifiedLists``, we gain the following syntaxes:

.. list-table::
    :align: left

    * - **New expression syntax**
      - **Desugared expression syntax**
    * - ``Foo.[]``
      - ``Foo.buildList 0 (\cons nil -> nil)``
    * - ``Foo.[x, y]``
      - ``Foo.buildList 2 (\cons nil -> x `cons` (y `cons` nil))``
    * - ``Foo.[x ..]``
      - ``Foo.buildListEnum (Foo.enumFrom x)``
    * - ``Foo.[x, y ..]``
      - ``Foo.buildListEnum (Foo.enumFromThen x y)``
    * - ``Foo.[x .. y]``
      - ``Foo.buildListEnum (Foo.enumFromTo x y)``
    * - ``Foo.[x, y .. z]``
      - ``Foo.buildListEnum (Foo.enumFromThenTo x y z)``

.. list-table::
    :align: left

    * - **New pattern syntax**
      - **Desugared pattern syntax**
    * - ``Foo.[x, _, y]``
      - ``Foo.FromListCons x (Foo.FromListCons _ (Foo.FromListCons y Foo.FromListNil))``
    * - ``x Foo.: y``
      - ``Foo.FromListCons x y``

One might wonder why this doesn't align more closely with the interface of ``-XOverloadedLists``, e.g. ``Foo.fromList [x, y, z]``. The reason is to avoid the intermediate list, which would need to typecheck as a list. Similar reason for defining new ``enumFrom`` functions instead of reusing Prelude's. See *Section 4.6 Heterogeneous Lists* for a use-case.

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

  data Exp
    = ...
    | QualListE ModuleName [Exp]

  data Lit
    = ...
    | QualStringL ModuleName String
    | QualNaturalL ModuleName Natural
    | QualIntegerL ModuleName Integer
    | QualRationalL ModuleName Rational

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

  buildList :: Integer -> ((a -> [a] -> [a]) -> [a] -> [a]) -> Vector a
  buildList n f = V.fromListN n (GHC.List.build f)

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

With ``QualifiedNumbers``, you could write ``Big.123``, which guarantees that ``Big.fromNatural`` is only called on literals (e.g. you could configure hlint to ban calling ``BigDecimal.fromNatural`` directly and only be used via ``QualifiedNumbers``).

::

  -- only called on literals
  fromNatural :: Natural -> BigDecimal
  fromNatural = BigDecimal . realToFrac

  fromNegativeInt :: Integer -> BigDecimal
  fromNegativeInt = BigDecimal . realToFrac

  fromRational :: Rational -> BigDecimal
  fromRational = BigDecimal . realToFrac

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
    Integer ->
    ( (forall a as. f a -> HList f as -> HList f (a ': as))
      -> HList f '[]
      -> HList f xs
    ) ->
    HList f xs
  buildList _ f = f HCons HNil

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

ByteArray
~~~~~~~~~

Example of a ``ByteArray`` implementation, which requires knowing the length of the list in advance.

::

  type Builder s = Int -> MutableByteArray s -> ST s ()

  buildList ::
    forall a. Prim a =>
    Integer ->
    ( forall s.
      (a -> Builder s -> Builder s)
      -> Builder s
      -> Builder s
    ) ->
    ByteArray
  buildList n f = createByteArray (n * sizeOfType @a) $ f cons nil
    where
      nil :: Builder s
      nil = \_ _ -> pure ()

      cons :: Prim a => a -> Builder s -> Builder s
      cons x next = \i arr -> writeByteArray arr i x >> next (i + 1) arr

  -- [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40]
  print ByteArray.[1, 2]

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

* Qualified multiline strings are allowed when ``-XMultilineStrings`` is enabled, as mentioned in the specification

* `Allow arbitrary identifiers as fields in OverloadedRecordDot <https://github.com/ghc-proposals/ghc-proposals/pull/668>`_ has similar syntax to the proposed qualified string literal, but as ``Foo.bar`` is parsed as a qualified identifier even with OverloadedRecordDot, it makes sense that ``Foo."bar"`` is also parsed as a qualified literal.

* `Allow native string interpolation syntax <https://github.com/ghc-proposals/ghc-proposals/pull/570>`_ proposes adding string interpolation syntax with ``s"..."``. If both proposals are accepted, this syntax could provide a mechanism similar to Javascript's `tagged template literals <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals#tagged_templates>`_. See the other proposal for more details.

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

  * The View pattern more closely matches `Section 3.17.2 <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-60015x7>`_ in the 2010 Report

* Use ViewPatterns for lists

  * This prevents marking list patterns as COMPLETE

* Don't split up ``fromNatural`` and ``fromNegativeInt``; just have one ``fromInteger`` function that can be defined as only taking in ``Natural``.

  * You'd still be relying on compiler support to warn that ``-1`` is an overflowed literal.

Unresolved Questions
--------------------

Implementation Plan
-------------------

Brandon Chinn will volunteer to implement.

Endorsements
------------
