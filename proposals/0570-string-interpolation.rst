Add native string interpolation syntax
======================================

.. author:: Brandon Chinn
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/570>`_.
.. sectnum::
.. contents::

Most languages have support for interpolating variables and (usually) arbitrary expressions:

* Python

  .. code-block:: python

    f"Expected: {x + y}, got: {result}"

* Rust

  .. code-block:: rust

    format!("Expected: {sum}, got: {result}")

* Scala

  .. code-block:: scala

    s"Expected: ${x + y}, got: ${result}"

* Javascript/Typescript

  .. code-block:: javascript

    `Expected: ${x + y}, got: ${result}`

This proposal proposes adding S-strings (like Scala's syntax) to Haskell.

Motivation
----------

Most non-trivial projects build strings at some point: printing out logs, rendering exceptions, generating code, pretty-printing. There are currently multiple ways to do this:
::

  -- concatenation + show
  error $ "Expected: " <> show (x + y) <> ", got: " <> show result

  -- printf
  error $ printf "Expected: %d, got: %d" (x + y) result

  -- safer printf, e.g. the `formatting` package
  error $ format ("Expected: " % int % ", got: " % int) (x + y) result

  -- quasiquoters, e.g. `string-interpolate` using `haskell-src-exts`
  error [i|Expected: #{x + y}, got: #{result}|]

But each of these options leave much to be desired:

* Manual interpolation (e.g. ``<>``, ``show``, ``unwords``, etc.) is annoying, especially for strings with a lot of interpolation. It's hard to see the overall structure of the string, especially when building up a ``Text``:
  ::

    let
      name1 = _ :: Text
      age1 = _ :: Int
      name2 = _ :: Text
      age2 = _ :: Int

      textExample1 = name1 <> " (age: " <> T.pack (show age1) <> ") encountered " <> name2 <> " (age: " <> T.pack (show age2) <> ")"

      textExample2 = T.pack $ T.unpack name1 <> " (age: " <> show age1 <> ") encountered " <> T.unpack name2 <> " (age: " <> show age2 <> ")"

      textExample3 = T.unwords
        [ name1
        , "(age: " <> T.pack (show age1) <> ")"
        , "encountered"
        , name2
        , "(age: " <> T.pack (show age2) <> ")"
        ]

* ``printf`` is partial and unsafe, which especially safety-conscious people might always stay away from anyway. Using a safer ``printf`` like ``formatting`` induces a third-party dependency, which is admittedly lightweight, but isn't as seamless of an integration as native string interpolation would be

* Quasiquotes induces a dependency on Template Haskell, which a lot of people avoid out of principle. Most QuasiQuoters also add a dependency on ``haskell-src-exts`` to parse arbitrary Haskell expressions, which could technically be avoided by using something like ``ghc-meta`` (`repo <https://github.com/noughtmare/ghc-meta>`_, `GHC issue <https://gitlab.haskell.org/ghc/ghc/-/issues/20862>`_), but this isn't in wide use yet.

If Haskell had native string interpolation, it would have the benefit and safety of the current third-party quasiquotes without the need for Template Haskell, and be able to take advantage of features like `multiline strings <https://github.com/ghc-proposals/ghc-proposals/pull/569>`_.
::

  error s"Expected: ${x + y}, got: ${result}"

  let textExample = s"${name1} (age: ${age1}) encountered ${name2} (age: ${age2})"

Proposed Change Specification
-----------------------------

This proposal introduces the ``-XStringInterpolation`` extension, which enables ``s"Name: ${name}"`` syntax. It synergizes well with ``-XOverloadedStrings``, ``-XMultilineStrings``, and ``-XQualifiedStrings``.

High-level Overview
~~~~~~~~~~~~~~~~~~~

At its core, this proposal proposes the following functionality:

* The syntax ``s"age: ${age}"`` expands to a built-in implementation using a new ``Interpolate`` type class to interpolate values such as ``age``

* The syntax ``M.s"age: ${age}"`` expands to a user-defined implementation where the implementor of the module ``M`` has total control over the implementation

  * Same technique as ``-XQualifiedStrings``

* ``s"..."`` is exactly equivalent to ``Data.String.Experimental.s"..."``, where ``Data.String.Experimental`` is a new module in ``ghc-experimental``.

Concretely, ``-XStringInterpolation`` enables the following syntax:

::

  s"a ${x + 1} b"

  -- Desugars to:
  interpolateFinalize $
    interpolateRaw "a "      `interpolateAppend`
    interpolateValue (x + 1) `interpolateAppend`
    interpolateRaw " b"      `interpolateAppend`
    interpolateEmpty

These definitions will be provided by ``Data.String.Experimental``, which will be initially implemented in ``ghc-experimental``. See `Section 2.4 Machinery <#24machinery>`_ for details.

Lexical Structure
~~~~~~~~~~~~~~~~~

Update `Section 10.2 <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17700010.2>`_ of the Haskell 2010 report as follows.

Add ``istring*`` patterns to ``lexeme`` (not ``literal``, because they're not literals):

.. code-block:: abnf

  lexeme  → qvarid | qconid | qvarsym | qconsym
          | literal | special | reservedop | reservedid
          | istringBegin
          | istringRaw
          | istringExprOpen
          | istringExprClose
          | istringEnd
          | istringMultilineBegin
          | istringMultilineRawStartLine
          | istringMultilineRawMidLine
          | istringMultilineEnd

  istringBegin → 's"' | modid . 's"'
  istringRaw → {graphic⟨'\' | '"' | '${'⟩ | space | escape | gap}
  istringExprOpen → '${'
  istringExprClose → '}'
  istringEnd → '"'

  istringMultilineBegin → 's"""' | modid . 's"""'
  istringMultilineRawStartLine → {whitechar} istringMultilineRawMidLine
  istringMultilineRawMidLine → {graphic⟨'\' | '"""' | '${'⟩ | space | escape | gap}
  istringMultilineEnd → '"""'

Also add ``$`` to ``charesc``:

.. code-block:: abnf

  charesc → a | b | f | n | r | t | v | \ | " | ' | & | $

With ``$`` added to ``charesc``, interpolation can be avoided by escaping the dollar sign; e.g. ``s"\${foo}" == "${foo}"``.

This grammar enables interpolating expressions with nested braces. Concretely, ``istringExprOpen`` and ``istringExprClose`` are only lexed within the ``istring`` and ``istringMultiline`` grammar productions, using Alex start codes. See `Section 3.1 Parsing <#31parsing>`_ for examples.

Context-Free Syntax
~~~~~~~~~~~~~~~~~~~

Update `Section 10.5 <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5>`_ of the Haskell 2010 report as follows.

.. code-block:: abnf

  aexp → qvar
       | ...
       | istring
       | istringMultiline

  istring →
    istringBegin
      {istringRaw | istringExprOpen exp istringExprClose}
      istringEnd

  istringMultiline →
    istringMultilineBegin
      {istringMultilineRawStartLine | istringExprOpen exp istringExprClose istringMultilineRawMidLine}
      istringMultilineEnd

Machinery
~~~~~~~~~

The following code will live in ``ghc-experimental`` under ``Data.String.Experimental``. After the API has stablized, these might eventually live in ``base`` under ``Data.String``, alongside ``IsString``.

::

  {----- Implementation of s"..." -----}

  interpolateRaw :: String -> StringBuilder
  interpolateRaw = fromString

  interpolateValue :: Interpolate a => a -> StringBuilder
  interpolateValue = interpolate

  interpolateAppend :: StringBuilder -> StringBuilder -> StringBuilder
  interpolateAppend = mappend

  interpolateEmpty :: StringBuilder
  interpolateEmpty = mempty

  interpolateFinalize :: StringBuilder -> String
  interpolateFinalize = buildString

  {----- StringBuilder -----}

  newtype StringBuilder = StringBuilder (Endo String)
    deriving newtype (Semigroup, Monoid)
  instance IsString StringBuilder where
    fromString s = StringBuilder (Endo (s <>))

  buildString :: StringBuilder -> String
  buildString (StringBuilder (Endo f)) = f ""

  {----- Interpolation of values -----}

  class Interpolate a where
    interpolate :: (IsString s, Monoid s) => a -> s

  instance Interpolate String where
    interpolate = fromString
  instance Interpolate Char where
    interpolate c = fromString [c]

  instance Interpolate Int where
    interpolate = fromString . show
  instance Interpolate Double where
    interpolate = fromString . show
  instance Interpolate Bool where
    interpolate = fromString . show

Types may implement ``Interpolate`` using ``IsString`` or ``Monoid``; see `Section 3.2 Composite types <#32composite-types>`_ for an example. The default interpolator will only ever use this as ``s ~ StringBuilder``, but this allows other qualified interpolators to reuse the built-in ``Interpolate`` class and avoid roundtripping through ``String`` in certain instances.

Expansion
~~~~~~~~~

With the machinery defined above, the following interpolated string desugars to the below expression:

::

  -- original string
  s"foo ${f a b} bar ${g x} baz ${name}"

  -- desugared
  interpolateFinalize $
    interpolateRaw "foo "   `interpolateAppend`
    interpolateValue (f a b)`interpolateAppend`
    interpolateRaw " bar "  `interpolateAppend`
    interpolateValue (g x)  `interpolateAppend`
    interpolateRaw " baz "  `interpolateAppend`
    interpolateValue name   `interpolateAppend`
    interpolateEmpty

Namely:

* An ``istringRaw`` component expands to ``interpolateRaw "<istringRaw>"``

  * The string literal passed to ``interpolateRaw`` is a strict ``String`` literal, unaffected by ``-XOverloadedStrings``

* An ``istringExprOpen exp istringExprClose`` component expands to ``interpolateValue (<exp>)``

* The list of ``istring`` components between ``istringBegin`` and ``istringEnd`` expands to the expansion of the components, with ``interpolateAppend`` as "list cons" and ``interpolateEmpty`` as "list nil".

  * ``interpolateAppend`` is always right-associative

  * Related: `Section 2.7 OverloadedStrings <#27overloadedstrings>`_ and `Section 2.8 QualifiedStrings <#28qualifiedstrings>`_

``ghc-experimental`` modules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This proposal would be adding the following modules to ``ghc-experimental``, which would potentially be promoted to a proper library like ``base`` once the feature is stablized.

.. list-table::
    :align: left

    * - **Module**
      - **Details**
    * - ``Data.String.Experimental``
      - Re-exports ``Data.String.Interpolate.Class.Experimental`` and ``Data.String.Interpolate.Default.Experimental``
    * - ``Data.String.Interpolate.Class.Experimental``
      - Defines the ``Interpolate`` class and instances as written in `Section 2.4 Machinery <#24machinery>`_
    * - ``Data.String.Interpolate.Default.Experimental``
      - Defines the classes and functions for the default ``s"..."`` syntax, as written in `Section 2.4 Machinery <#24machinery>`_
    * - ``Data.String.Interpolate.Basic.Experimental``
      - Defines an interpolator that's the same as the default except interpolates values directly without automatic conversion with ``Interpolate`` (See `Section 10.3 Provided interpolator: Basic <#103provided-interpolator-basic>`_)
    * - ``Data.String.Interpolate.ShowS.Experimental``
      - Defines an interpolator useful for implementing ``showsPrec`` (See `Section 10.4 Provided interpolator: ShowS <#104provided-interpolator-shows>`_)

OverloadedStrings
~~~~~~~~~~~~~~~~~

When ``-XOverloadedStrings`` is enabled, a final ``fromString`` is added after the ``interpolateFinalize`` call. This still constructs the string with ``StringBuilder`` -> ``String``, so users might prefer using QualifiedStrings instead.

QualifiedStrings
~~~~~~~~~~~~~~~~

When ``-XQualifiedStrings`` is enabled, you may qualify string interpolation, where ``[modid.]s"..."`` desugars to the same expressions, except resolving the ``interpolate*`` functions as ``[modid.]interpolate*``. Some examples:

::

  Text.s"hello world"

  -- Desugars to:
  Text.interpolateFinalize $
    Text.interpolateRaw "hello world"

::

  SQL.s"select * from users where name = ${Text.toUpper name} and age = ${age}"

  -- Desugars to:
  SQL.interpolateFinalize $
    SQL.interpolateRaw   "select * from users where name = " `SQL.interpolateAppend`
    SQL.interpolateValue (Text.toUpper name)                 `SQL.interpolateAppend`
    SQL.interpolateRaw   " and age = "                       `SQL.interpolateAppend`
    SQL.interpolateValue age                                 `SQL.interpolateAppend`
    SQL.interpolateEmpty

It's highly recommended that every string type with an ``IsString`` instance provides at least one string interpolator reusing the built-in ``Interpolate`` class. That way, there's always an option to use ``MyString.s"..."`` if the user does not wish to globally enable ``-XOverloadedStrings``. This interpolator could simply be a monomorphized version of the default interpolator:

::

    module Data.MyString (
      module X,
      interpolateFinalize,
    ) where

    import Data.String.Experimental as X hiding (interpolateFinalize)
    import Data.String.Experimental qualified as S

    interpolateFinalize :: StringBuilder -> MyString
    interpolateFinalize = S.interpolateFinalize

Or it could reuse the built-in ``Interpolate`` class using its own ``Builder`` type:

::

    module Data.MyString where

    interpolateRaw :: String -> MyStringBuilder
    interpolateRaw = fromString

    interpolateValue :: Interpolate a => a -> MyStringBuilder
    interpolateValue = interpolate

    interpolateAppend :: MyStringBuilder -> MyStringBuilder -> MyStringBuilder
    interpolateAppend = mappend

    interpolateEmpty :: MyStringBuilder
    interpolateEmpty = mempty

    interpolateFinalize :: MyStringBuilder -> MyString
    interpolateFinalize = buildMyString

The only recommendation here is that ``MyString`` provide a module implementing string interpolation using the built-in ``Interpolate`` type class. Of course, ``MyString`` is free to implement more string interpolators, potentially using its own ``MyString.Interpolate`` type class for more performant interpolations.

The following laws should hold, if the expression compiles:

* ``M."str" == M.s"str"``
* ``Data.String.fromString "str" == M.s"str"``

MultilineStrings
~~~~~~~~~~~~~~~~

When ``-XMultilineStrings`` is enabled, string interpolation may be used with multiline strings. Multiline string interpolations resolve the multiline string first, then do the string interpolation. This means that qualified string interpolations work with multiline strings for free.

::

  let x = "hello"

  -- original string
  let str0 =
        s"""
        ${x} world
        world ${x}
        ${x} world
        """

  -- resolve multiline string
  let str1 = s"${x} world\nworld ${x}\n${x} world"

  -- resolve interpolation
  let str2 = "hello world\nworld hello\nhello world"

Template Haskell
~~~~~~~~~~~~~~~~

We are intentionally not adding anything to Template Haskell, as one could just build the expression themselves. String interpolation is still supported in quotes, which will be desugared when translating to TH.

Examples
--------

Parsing
~~~~~~~

.. list-table:: **Valid expressions**
    :align: left

    * - ``s"a ${x} b"``
      - Simple expressions
    * - ``s"a ${x + 1} b"``
      - Complex expressions
    * - ``s"a ${'{'} ${'}'} b"``
      - Expressions containing braces (char)
    * - ``s"a ${User{a = 1}} b"``
      - Expressions containing braces (record)
    * - ``s"a ${s"c ${x} d"} b"``
      - Nested interpolation
    * - ``s"a ${1 :: Int}"``
      - Inline type annotation
    * - ``s"a ${x {- a -}} b"``
      - Inline comment
    * - ``s"Name: ${user.name}"``
      - OverloadedRecordDot

.. list-table:: **Invalid expressions**
    :align: left

    * - ``s"a ${} b"``
      - Expression is missing
    * - ``s"a ${=} b"``
      - Not a valid expression
    * - ``s"a ${let x =} b"``
      - Incomplete expression
    * - ``s"a ${{b} c"``
      - The second ``{`` is not a valid character to start an expression
    * - ``s"a ${b -- asdf} c"``
      - The rest of the string is commented out

Composite types
~~~~~~~~~~~~~~~

``Interpolate`` specifies a generic ``IsString s, Monoid s``, which allows composite types to stay within ``s``. If it were monomorphized to ``String``, instances that would only use ``fromString`` and ``<>`` would make unnecessary roundtrips through ``String``.

::

    data SrcLoc = SrcLoc
      { file :: FilePath
      , line :: Int
      , col :: Int
      }

    instance Interpolate SrcLoc where
      interpolate SrcLoc{..} =
        interpolate file <>
        fromString ":" <>
        interpolate line <>
        fromString ":" <>
        interpolate col

Effect and Interactions
-----------------------

An existing program containing ``s"..."`` will break when ``-XStringInterpolation`` is enabled. While there's precedent for this (Template Haskell splices make ``$(...)`` different from ``$ (...)``), this is the first instance where whitespace matters for an alphanumeric identifier. But this is not a big deal:

#. It's unlikely for someone to be naming a function as ``s`` in the first place
#. Easily mitigatable: just add a space, which improves readability anyway
#. Prefixing string literals like ``s"..."`` is common in other languages: Python, Scala, Javascript/Typescript, etc. so it shouldn't be a big hurdle for newcomers

Interacts nicely with ``-XOverloadedStrings``, ``-XQualifiedStrings``, and ``-XMultilineStrings``. See `Section 2 Proposed Change Specification <#2proposed-change-specification>`_ above.

Costs and Drawbacks
-------------------

Development and maintenance is of moderate effort. Learnability for novice users will go up, since novice users probably expect string interpolation to be available, and might be frustrated at the lack of support currently.

One minor drawback is the whitespace sensitivity of ``s"``, as discussed in "Effect and Interactions".

Alternatives
------------

* Status quo (discussed in the "Motivation" section)

* Don't implicitly convert values when interpolating

  * ``s"a ${x}"`` would instead translate to ``fromBuilder (toBuilder "a " <> toBuilder x)``
  * Pro: no more ``Interpolate`` class
  * Pro: more explicit, e.g. the way you have to explicitly convert before calling ``+``
  * Pro: less likely to encounter type inference issues
  * Con: adds more noise to interpolate non-string values
  * This is what ``neat-interpolation`` does
  * See `Section 10.1 Community Survey <#101community-survey>`_

* Reuse ``PrintfArg``

  * The bulk of its API deals with format specifiers, which is not applicable to this proposal. ``Interpolate`` is much simpler

* Define ``Interpolate`` as a multi param type class

  * More complex
  * Introduces M*N instances problem
  * ``-XQualifiedStrings`` is available for any more advanced use cases

* Desugar to a function

  * like ``printf``: ``s"a %s b %s" foo bar => (\x0 x1 -> "a " <> interpolate x0 <> " b " <> interpolate x1) foo bar``
  * or like ``formatting``: ``s"a {text} b {int}" foo bar => (\x0 x1 -> "a " <> text x0 <> " b " <> int x1) foo bar``
  * This defeats the purpose of string interpolation making it easy to see the exact location a variable gets injected. If you're interpolating a lot of values into a large string (e.g. with multiline strings), it's extremely difficult to match up which expression to which interpolation position.

* Allow passing a String representation of the interpolated expression to ``interpolate``, e.g. to support something like ``Dbg.s"foo | ${x + 1}"`` returning ``"foo | x + 1 = 11"``

  * I don't think this has any uses outside of debugging; if it's just that one use-case, quasiquotation should be sufficient
  * https://github.com/brandonchinn178/ghc-string-interpolation-prototypes/issues/8

* Do something like `Python's new t-string feature <https://peps.python.org/pep-0750/>`_

  * This doesn't translate easily to Haskell, since the point of t-string is to return a list of strings and a list of "anything" that was interpolated
  * The ``QualifiedStrings`` part of the proposal should be able to handle any functionality here

* Instead of interpolating primitives via ``String``, define a finally tagless API to allow interpolating certain other blessed types, like ``Integral`` or ``RealFloat``

  * Would allow performant interpolation of primitives for ``Text.Builder`` using the same ``Interpolate`` class
  * Wouldn't solve the issue of ``Text.Builder`` interpolating to itself via ``String``

* Instead of interpolating primitives via ``String``, define a bytearray writer enabling primitives to explicitly write bytes, which could be a useful lowest-common-denominator for both ``String`` and ``Text.Builder``

  * Would be slightly slower for finally outputting ``String`` compared to ``ShowS``
  * Writing a byte array is pretty low-level, perhaps more low-level than we'd like here

Expansion-related Alternatives
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Hardcode to Monoid's ``mappend`` and ``mempty``

  * Would remove potential use-cases needing type-changing ``append``, e.g. ``a -> [a] -> [a]`` or ``f a -> f as -> f (a ': as)``
  * It's likely that most custom interpolators will implement ``interpolateAppend``/``interpolateEmpty`` with ``Monoid``, but we should avoid restricting the interface here

* Use ``M.fromString`` instead of ``interpolateRaw``, to more tightly connect ``StringInterpolation`` with ``QualifiedStrings``

  * While ``QualifiedStrings`` and ``StringInterpolation`` are closely related, and implementions *ought* to implement them consistently, the language feature should not enforce it, in the same way that typeclass laws are not enforced by the language
  * Even if we hardcoded ``fromString``, one could still devise a custom string interpolator that's inconsistent with ``M.fromString``; e.g. ``interpolateFinalize _ = "bad"``

* Hardcode a wired-in ``Interpolate`` class with ``interpolateValue`` (and potentially ``interpolateRaw``)

  * Redundant with the rebindable functionality with ``-XQualifiedStrings``

* Add an ``InterpolateBuilder`` type family to specify a builder type for the interpolator ``s`` and define ``interpolateFinalize`` with that type family

  * Allows the default ``s"..."`` to build more performantly for non-``String``
  * Type inference should be unambiguous, since ``s`` should be known, and ``Builder s`` is unique for a given ``s``
  * Adds complexity; probably better to just use a qualified interpolator for this

Delimiter-related Alternatives
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Allow ``$foo`` in addition to ``${foo}``

  * This would complicate the syntax, and would also require interpolated string to escape bare ``$``.

* Different quote delimiter

  * ``s"..."`` was taken from Scala's interpolation syntax
  * Could use ``f"..."`` like Python, with ``f`` for format, but ``f`` is a common variable name for functions and if the user forgets to enable ``-XStringInterpolation``, ``f"..."`` would parse as ``f "..."`` which is likely to be valid.
  * Could use ``i"..."``, with ``i`` for interpolate.
  * Could reuse QuasiQuote syntax, e.g. ``[s|`` or ``[fmt|``, except it would be special and NOT use Template Haskell.
  * Could do ``''...''``, since ``''`` is invalid Haskell syntax today. However, code highlighters that aren't updated for ``-XStringInterpolation`` yet would not gracefully handle this.

* No quote delimiter, always interpolate

  * e.g. if we switch the delimiter to `\{...}`, which is currently invalid in a string
  * Used by `jq <https://jqlang.org/manual/#string-interpolation>`_
  * Downside is that you have to parse the string before figuring out how it should be desugared

* Different interpolation delimiter, e.g. ``#{foo}``

  * ``${`` is familiar to most developers

* No interpolation delimiter, e.g. ``{foo}``, like Python/Rust, escape with ``\{``

  * One less character for the "common" case where interpolation typically doesn't happen in strings containing ``{``
  * It would make interpolating into JSON, LaTeX, etc. more annoying
  * It would make interpolating into code more annoying (e.g. shell, C-like languages, Haskell code with records)
  * ``{`` is more likely to come up than ``${``

* Allow custom delimiters, which could be defined with Template Haskell or some other approach

  * See `Section 10.1 Community Survey <#101community-survey>`_

Unresolved Questions
--------------------

Implementation Plan
-------------------

I have a prototype started `here <https://gitlab.haskell.org/ghc/ghc/-/compare/master...wip%2Finterpolated-strings>`_

Endorsements
------------

Appendix
--------

Community Survey
~~~~~~~~~~~~~~~~

I sent out multiple community surveys, the last one being open 2025-04-21 to 2025-04-30. Raw data and analysis can be found here: https://github.com/brandonchinn178/ghc-string-interpolation-prototypes/tree/main/results

Performance consideration
~~~~~~~~~~~~~~~~~~~~~~~~~

Strings are notorious for O(n^2) concatenations, but the current proposal builds with ``ShowS``, so it should remain linear. The only case where it might be O(n^2) is when nesting interpolated strings inside interpolated strings (although benchmarking still shows this to be linear in practice).

Benchmarks: https://github.com/brandonchinn178/ghc-string-interpolation-prototypes/tree/main/bench

Provided interpolator: Basic
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As part of the feature, ``ghc-experimental`` will provide ``Data.String.Interpolate.Basic.Experimental``, which provides an interpolator that does not implicitly convert values and stays in ``s`` the whole time.

::

  module Data.String.Interpolate.Basic.Experimental where

  interpolateRaw :: IsString s => String -> s
  interpolateRaw = fromString

  interpolateValue :: s -> s
  interpolateValue = id

  interpolateAppend :: Monoid s => s -> s -> s
  interpolateAppend = mappend

  interpolateEmpty :: Monoid s => s
  interpolateEmpty = mempty

  interpolateFinalize :: s -> s
  interpolateFinalize = id

This is particularly useful for ``Builder``, where users could explicitly convert values and avoid the penalty of going through ``String`` with the default ``Interpolate`` class.

::

  import Data.String.Interpolate.Basic.Experimental qualified as B
  import Data.Text.Lazy.Builder qualified as B

  render :: Person -> B.Builder
  render Person{..} = B.s"Person(name = ${B.fromLazyText name}, age = ${B.decimal age})"

Provided interpolator: ShowS
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As part of the feature, ``ghc-experimental`` will provide ``Data.String.Interpolate.ShowS.Experimental``, which provides an interpolator that makes it easier to implement ``showsPrec``:

::

  module Data.String.Interpolate.ShowS.Experimental where

  interpolateRaw = showString
  interpolateValue = shows
  interpolateAppend = (.)
  interpolateEmpty = id
  interpolateFinalize = id

  data P a = P !Int !a
  instance Show a => Show (P a) where
    showsPrec _ (P p a) = showsPrec p a

Users could then write:

::

  instance Show a => Show (MyTree a) where
    showsPrec d (MyTree l v r) =
      showParen (d > 10) $
        ShowS.s"MyTree ${ShowS.P 11 l} ${v} ${ShowS.P 11 r}"

Text
~~~~

When ``OverloadedStrings`` is enabled, the default interpolation builds up with ``StringBuilder`` then converts to ``Text`` with a final ``fromString``. As mentioned in `Section 2.8 QualifiedStrings <#28qualifiedstrings>`_, ``text`` should provide interpolators that reuse the built-in ``Interpolate`` class, probably using ``Builder`` to be as performant as possible:

::

  interpolateRaw = fromString
  interpolateValue = interpolate
  interpolateAppend = mappend
  interpolateEmpty = mempty
  interpolateFinalize = LazyText.toStrict . Builder.toLazyText

With this support, users can write the following:

::

  {-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE QualifiedStrings #-}
  {-# LANGUAGE StringInterpolation #-}

  import Data.Text qualified as T
  import Data.Text.Interpolate qualified as T

  main = do
    let name = "Alice"
    let age = 10

    -- with qualified strings
    print $ T.toUpper T.s"Name: ${name}, Age: ${age}"

Custom interpolator: SqlQuery
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Imagine a library implements a ``SqlQuery`` type like:

::

  data SqlQuery = SqlQuery
    { sqlText :: Text
    , sqlValues :: [SqlValue]
    }
    deriving (Show, Eq)

  instance IsString SqlQuery where
    fromString s = SqlQuery{sqlText = T.pack s, sqlValues = []}
  instance Semigroup SqlQuery where
    q1 <> q2 =
      SqlQuery
        { sqlText = sqlText q1 <> sqlText q2
        , sqlValues = sqlValues q1 <> sqlValues q2
        }
  instance Monoid SqlQuery where
    mempty =
      SqlQuery
        { sqlText = ""
        , sqlValues = []
        }

  data SqlValue
    = SqlText Text
    | SqlInt Int
    deriving (Show)

  class ToSqlValue a where
    toSqlValue :: a -> SqlValue
  instance ToSqlValue String where
    toSqlValue = SqlText . T.pack
  instance ToSqlValue Text where
    toSqlValue = SqlText
  instance ToSqlValue Int where
    toSqlValue = SqlInt

The library would also define a module for use with ``-XStringInterpolation`` + ``-XQualifiedStrings``:

::

  module Data.SQL.Interpolate where

  import Data.String qualified as S
  import Data.String.Experimental qualified as S

  interpolateRaw = fromString
  interpolateValue = interpolate
  interpolateAppend = mappend
  interpolateEmpty = mempty
  interpolateFinalize = id

  class Interpolate a where
    interpolate :: a -> SqlQuery
  instance Interpolate SqlQuery where
    interpolate = id
  instance {-# OVERLAPPABLE #-} ToSqlValue a => Interpolate a where
    interpolate a = SqlQuery{sqlText = "?", sqlValues = [toSqlValue a]}

And gain access to safe string interpolation without SQL injection:

::

  let age = 10 :: Int
  let name = "Robert'); DROP TABLE Students;--" :: String

  SQL.s"SELECT * FROM tab WHERE age = ${age} AND name ILIKE ${name}"
    == SqlQuery
        { sqlText = "SELECT * FROM tab WHERE age = ? AND name ILIKE ?"
        , sqlValues = [SqlInt 10,SqlText "Robert'); DROP TABLE Students;--"]
        }

  let
    -- e.g. from user input
    isAdult = True
    nameFilter = SqlText "A%"

    -- build where clause
    whereClauses =
      concat
        [ ["age > 18" | isAdult]
        , [SQL.s"name ILIKE ${nameFilter}"]
        ]
    conjoin cs = mconcat $ intersperse " AND " (cs :: [SqlQuery])

  SQL.s"SELECT * FROM tab WHERE ${conjoin whereClauses}"
    == SqlQuery
        { sqlText = "SELECT * FROM tab WHERE age > 18 AND name ILIKE ?"
        , sqlValues = [SqlText "A%"]
        }

The library could also define an implementation to support failure states:

::

  module Data.SQL.Compile.Interpolate (
    module X,
    interpolateFinalize,
  ) where

  import Data.SQL.Interpolate as X hiding (interpolateFinalize)

  interpolateFinalize :: SqlQuery -> Either ParseError CompiledSqlQuery
  interpolateFinalize = compileQuery

::

  import Data.SQL.Compile.Interpolate qualified as SQL

  main = do
    let name = "Alice"
    query <-
      either (fail . show) pure $
        SQL.s"SELECT * FROM users WHERE name = ${name}"

    print query

Custom interpolator: HTML
~~~~~~~~~~~~~~~~~~~~~~~~~

Imagine a library implements a new ``Html`` type like:

::

  newtype Html = Html Text
    deriving newtype (Show, IsString, Semigroup, Monoid)

  escapeHtml :: Text -> Text
  escapeHtml = Text.replace "<" "&lt;" . Text.replace ">" "&gt;"

That library could define the module:

::

  module Data.HTML.Interpolate where

  import Data.HTML as HTML
  import Data.String.Experimental qualified as S

  interpolateRaw = fromString
  interpolateValue = interpolate
  interpolateAppend = mappend
  interpolateEmpty = mempty
  interpolateFinalize = id

  class Interpolate a where
    interpolate :: a -> Html
  instance Interpolate Html where
    interpolate = id
  instance Interpolate String where
    interpolate = interpolate . T.pack
  instance Interpolate Text where
    interpolate = Html . escapeHtml
  instance {-# OVERLAPPABLE #-} S.Interpolate a => Interpolate a where
    interpolate = interpolate @Text . S.interpolate

And gain access to safe string interpolation with HTML escaping by default:

::

  let title = "Why is 1 > 0?" :: Text
  let body = "<p>Hello world</p>" :: Text

  Html.s"<h1>${title}</h1>${Html.raw body}"
    == Html "<h1>Why is 1 &gt; 0?</h1><p>Hello world</p>"

Custom interpolatable type: BigDecimal
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Imagine a library implements a new ``BigDecimal`` type:

::

  data BigDecimal = BigDecimal Integer Int

  renderBigDecimal :: (IsString s) => BigDecimal -> s
  renderBigDecimal (BigDecimal digits scale) =
    let (int, frac) = splitAt scale (show digits)
     in fromString $ int <> "." <> frac

That library could define:

::

  instance Interpolate BigDecimal where
    interpolate = renderBigDecimal

And be able to use it in interpolated strings:

::

  let n = BigDecimal 123456 3
  s"123456 / 10^3 = ${n}" == "123456 / 10^3 = 123.456"

If ``text`` provided an interpolator using the built-in ``Interpolate`` class, ``BigDecimal`` could interpolate into ``Text.s"..."`` for free.

Format specifiers
~~~~~~~~~~~~~~~~~

One notable feature Python supports in string interpolation is specifying format specifiers:

.. code-block:: python

  x = 1.2
  f"{x:.3f}" == "1.200"

This could be provided by libraries with the proposed machinery, and the design and implementation of those libraries is left as an exercise for the reader. But from a user perspective, here's one possible way such a library could be used:

::

  {-
  class Formattable a where
    fmt :: String -> a -> String
  -}

  let today = fromGregorian 2024 08 12 :: Day
   in s"Today's date is ${fmt "%a, %d %b %Y" today}."

  let earned = -13.2 :: Float
      total = 127.978 :: Float
   in s"""
        Points earned: ${fmt "+8.2" earned}
        Current total: ${fmt "+8.2" total}
      """

Where these would return the strings:

::

  Today's date is Mon, 12 Aug 2024.

  Points earned:   -13.20
  Current total:  +127.98
