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

This proposal introduces the ``-XStringInterpolation`` extension, which includes syntax similar to ``-XQualifiedStrings`` (`proposal <https://github.com/ghc-proposals/ghc-proposals/pull/723>`_).

High-level Overview
~~~~~~~~~~~~~~~~~~~

At its core, this proposal proposes the following functionality:

* The syntax ``s"age: ${age}"`` expands to a built-in implementation hardcoded to ``String`` and a new ``Interpolate`` type class with an ``interpolate :: a -> String`` function.

    * ``-XOverloadedStrings`` does not affect this expression at all

* The syntax ``M.s"age: ${age}"`` expands to a user-defined implementation where the implementor of the module ``M`` has total control over the implementation

    * Same technique as ``-XQualifiedStrings``

* ``s"..."`` is exactly equivalent to ``Data.String.Interpolate.Experimental.s"..."``, where ``Data.String.Interpolate.Experimental`` is a new module in ``ghc-experimental``.

Concretely, ``-XStringInterpolation`` enables the following syntax:

::

  s"a ${x + 1} b"

  -- Desugars to:
  interpolateFinalize $
    interpolateRawString "a " `interpolateAppend` 
    interpolateConvertValue (x + 1)  `interpolateAppend` 
    interpolateRawString " b" `interpolateAppend` 
    interpolateEmpty

These definitions will be provided by ``Data.String.Interpolate.Experimental``, which will be initially implemented in ``ghc-experimental``. See *Section 2.4 Machinery* for details.

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

This grammar enables interpolating expressions with nested braces. Concretely, ``istringExprOpen`` and ``istringExprClose`` are only lexed within the ``istring`` context, using Alex start codes. See *Section 3.1* for examples.

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

The following code will live in ``ghc-experimental`` under ``Data.String.Interpolate.Experimental``. After the API has stablized, these might eventually live in ``Data.String`` alongside ``IsString``.

::

  interpolateRawString :: String -> ShowS
  interpolateRawString = showString

  interpolateConvertValue :: Interpolate a => a -> ShowS
  interpolateConvertValue = interpolateS

  interpolateAppend :: ShowS -> ShowS -> ShowS
  interpolateAppend = (.)

  interpolateEmpty :: ShowS
  interpolateEmpty = id

  interpolateFinalize :: ShowS -> String
  interpolateFinalize f = f ""

  class Interpolate a where
    {-# MINIMAL interpolate | interpolateS #-}

    interpolate :: a -> String
    interpolate x = interpolateS x ""

    interpolateS :: Int -> a -> ShowS
    interpolateS _ x s = interpolate x <> s

Instances will be provided as well, for example:

::

  instance Interpolate String where
    interpolateS _ = showString
  instance Interpolate Char where
    interpolateS _ = showChar

  instance Interpolate Int where
    interpolateS = showsPrec
  instance Interpolate Double where
    interpolateS = showsPrec
  instance Interpolate Bool where
    interpolateS = showsPrec

  instance Interpolate a => Interpolate (Maybe a) where
    interpolateS _ Nothing = showString "Nothing"
    interpolateS d (Just a) = showString "Just " . showParen (d > 10) (interpolateS 11 a)

Expansion
~~~~~~~~~

With the machinery defined above, the following interpolated string desugars to the below expression:

::

  -- original string
  s"foo ${f a b} bar ${g x} baz ${name}"

  -- desugared, where D.S.I.E = Data.String.Interpolate.Experimental.
  D.S.I.E.interpolateFinalize $
    D.S.I.E.interpolateRawString "foo "     `D.S.I.E.interpolateAppend`
    D.S.I.E.interpolateConvertValue (f a b) `D.S.I.E.interpolateAppend`
    D.S.I.E.interpolateRawString " bar "    `D.S.I.E.interpolateAppend`
    D.S.I.E.interpolateConvertValue (g x)   `D.S.I.E.interpolateAppend`
    D.S.I.E.interpolateRawString " baz "    `D.S.I.E.interpolateAppend`
    D.S.I.E.interpolateConvertValue name    `D.S.I.E.interpolateAppend`
    D.S.I.E.interpolateEmpty

Namely:

* An ``istringRaw <str>`` component expands to ``interpolateRawString "<str>"``

    * The string literal passed to ``interpolateRawString`` is a strict ``String`` literal, unaffected by ``-XOverloadedStrings``

* An ``istringExprOpen exp istringExprClose`` component expands to ``interpolateConvertValue (<exp>)``
* The right-associated list of ``istring`` components between ``istringBegin`` and ``istringEnd`` expands to the expansion of the components, with ``interpolateAppend`` as "list cons" and ``interpolateEmpty`` as "list nil".

Qualified string interpolation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Similar to ``-XQualifiedStrings``, you may qualify string interpolation:

::

  Text.s"hello world"

  -- Desugars to:
  Text.interpolateFinalize $
    Text.interpolateRawString "hello world"

::

  SQL.s"select * from users where name = ${Text.toUpper name} and age = ${age}"

  -- Desugars to:
  SQL.interpolateFinalize $
    SQL.interpolateRawString "select * from users where name = " `SQL.interpolateAppend`
    SQL.interpolateConvertValue (Text.toUpper name)              `SQL.interpolateAppend`
    SQL.interpolateRawString " and age = "                       `SQL.interpolateAppend`
    SQL.interpolateConvertValue age                              `SQL.interpolateAppend`
    SQL.interpolateEmpty

This syntax does not require ``-XQualifiedStrings``, only ``-XStringInterpolation``. It doesn't enable ``-XQualifiedStrings`` for normal string literals, only interpolation strings. While asymmetrical, since ``s"..."`` only works with ``String``, it's highly likely that most uses will use qualified syntax, so we enable it with only ``-XStringInterpolation``.

The following laws should hold, if the expression compiles:

* ``M."str" == M.s"str"``
* ``Data.String.fromString "str" == M.s"str"``

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

Multiline strings
~~~~~~~~~~~~~~~~~

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

Effect and Interactions
-----------------------

An existing program containing ``s"..."`` will break when ``-XStringInterpolation`` is enabled. While there's precedent for this (Template Haskell splices make ``$(...)`` different from ``$ (...)``), this is the first instance where whitespace matters for an alphanumeric identifier. But this is not a big deal:

#. It's unlikely for someone to be naming a function as ``s`` in the first place
#. Easily mitigatable: just add a space, which improves readability anyway
#. Prefixing string literals like ``s"..."`` is common in other languages: Python, Scala, Javascript/Typescript, etc. so it shouldn't be a big hurdle for newcomers

When ``-XStringInterpolation`` is enabled, ``M.s"..."`` syntax is enabled, as described above. This is enabled whether ``-XQualifiedStrings`` is enabled or not, but the two extensions should not conflict.

Interpolation is also supported with ``-XMultilineStrings``, as described above.

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
  * See *Section 10.1 Community Survey*

* Reuse ``PrintfArg``

  * The bulk of its API deals with format specifiers, which is not applicable to this proposal. ``Interpolate`` is much simpler

* Define ``Interpolate`` as a multi param type class, instead of only going to ``String``.

  * More complex
  * Introduces n^2 instances problem
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

Expansion-related Alternatives
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Also see *Section 10.9 Alternative - Polymorphic interpolable expansion*

* Hardcode to Monoid's ``mappend`` and ``mempty``

  * Would remove potential use-cases needing type-changing ``append``, e.g. ``a -> [a] -> [a]`` or ``f a -> f as -> f (a ': as)``
  * It's likely that most custom ``interpolateString`` will use ``Monoid``, but we should avoid restricting the interface here

* Use ``M.fromString`` instead of ``interpolateStringRaw``, to more tightly connect ``StringInterpolation`` with ``QualifiedStrings``

  * While ``QualifiedStrings`` and ``StringInterpolation`` are closely related, and implementions *ought* to implement them consistently, the language feature should not enforce it, in the same way that typeclass laws are not enforced by the language
  * Even if we hardcoded ``fromString``, one could still devise a custom string interpolator that's inconsistent with ``M.fromString``; e.g. ``interpolateFinalize _ = "bad"``

* Hardcode a wired-in ``Interpolate`` class with ``interpolateConvertValue`` (and potentially ``interpolateRawString``)

  * Redundant with the rebindable functionality with ``-XQualifiedStrings``

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

  * Would require any use of ``${...}`` to be escaped.
  * No other language does this; even Bash has single quoted strings to avoid escaping

* Different interpolation delimiter, e.g. ``#{foo}``

  * ``${`` is familiar to most developers

* No interpolation delimiter, e.g. ``{foo}``, like Python/Rust, escape with ``\{``

  * One less character for the "common" case where interpolation typically doesn't happen in strings containing ``{``
  * It would make interpolating into JSON, LaTeX, etc. more annoying
  * It would make interpolating into code more annoying (e.g. shell, C-like languages, Haskell code with records)
  * ``{`` is more likely to come up than ``${``

* Allow custom delimiters, which could be defined with Template Haskell or some other approach

  * See *Section 10.1 Community Survey*

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

Text
~~~~

``text`` has a few options for string interpolation, potentially even providing multiple modules for each variant of string interpolator. All the interpolators would probably be implemented with ``Builder``, for performance, but one could implement variants for:

* The final type to return

  * ``interpolateFinalize :: Builder -> Builder; interpolateFinalize = id``
  * ``interpolateFinalize :: Builder -> LazyText; interpolateFinalize = toLazyText``
  * ``interpolateFinalize :: Builder -> Text; interpolateFinalize = toStrict . toLazyText``

* How to interpolate values

  * Reuse built-in ``Interpolate``: ``interpolateConvertValue = fromString . interpolate``
  * Provide a new ``Interpolate`` class with ``interpolate :: a -> Builder``

Here's an example implementing the ``Builder`` + ``Text`` interpolators:

::

  module Data.Text.Interpolate.Builder where

  import Data.String.Interpolate.Experimental (interpolate)

  interpolateFinalize = id
  interpolateConvertValue = fromString . interpolate
  interpolateRawString = fromString
  interpolateAppend = mappend
  interpolateEmpty = mempty

::

  module Data.Text.Interpolate where

  import Data.Text.Interpolate.Builder qualified as B

  interpolateFinalize = toStrict . toLazyText
  interpolateConvertValue = B.interpolateConvertValue
  interpolateRawString = B.interpolateRawString
  interpolateAppend = B.interpolateAppend
  interpolateEmpty = B.interpolateEmpty

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

The library would also define a module for use with qualified string interpolations:

::

  module Data.SQL.Interpolate where

  import Data.String qualified as S
  import Data.String.Interpolate.Experimental qualified as S

  interpolateFinalize = id
  interpolateConvertValue = interpolate
  interpolateRawString = fromString
  interpolateAppend = mappend
  interpolateEmpty = mempty

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

  newtype RawHtml = RawHtml {unRawHtml :: Text}

  raw :: Text -> RawHtml
  raw = RawHtml

That library could define the module:

::

  module Data.HTML.Interpolate where

  import Data.HTML as HTML
  import Data.String.Interpolate.Experimental qualified as S

  interpolateFinalize = id
  interpolateConvertValue = interpolate
  interpolateRawString = HTML.raw
  interpolateAppend = mappend
  interpolateEmpty = mempty

  class Interpolate a where
    interpolate :: a -> Html
  instance Interpolate String where
    interpolate = interpolate . T.pack
  instance Interpolate Text where
    interpolate = Html . escapeHtml
  instance Interpolate RawHtml where
    interpolate = Html . unRawHtml
  instance {-# OVERLAPPABLE #-} S.Interpolate a => Interpolate a where
    interpolate = interpolate . S.interpolate

And gain access to safe string interpolation with HTML escaping by default:

::

  let title = "Why is 1 > 0?" :: Text
  let body = "<p>Hello world</p>" :: Text

  Html.s"<h1>${title}</h1>${raw body}"
    == Html "<h1>Why is 1 &gt; 0?</h1><p>Hello world</p>"

Custom interpolator: ShowS
~~~~~~~~~~~~~~~~~~~~~~~~~~

String interpolation could also make it easier to implement `shows`, in a module potentially provided by `base`:

::

  module Data.ShowS.Interpolate (P (..), interpolateString) where

  interpolateFinalize = id
  interpolateConvertValue = shows
  interpolateRawString = showString
  interpolateAppend = (.)
  interpolateEmpty = id

  data P a = P !Int !a
  instance Show a => Show (P a) where
    showsPrec _ (P p a) = showsPrec p a

Users could then do

::

  instance Show a => Show (MyTree a) where
    showsPrec d (MyTree l v r) =
      showParen (d > 10) $
        ShowS.s"MyTree ${ShowS.P 11 l} ${v} ${ShowS.P 11 r}"

Custom interpolatable type: BigDecimal
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Imagine a library implements a new ``BigDecimal`` type:

::

  data BigDecimal = BigDecimal Integer Int

  renderBigDecimal :: BigDecimal -> String
  renderBigDecimal (BigDecimal digits scale) =
    let (int, frac) = splitAt scale (show digits)
     in int <> "." <> frac

That library could define:

::

  instance Interpolate BigDecimal where
    interpolate = renderBigDecimal

And be able to use it in interpolated strings:

::

  let n = BigDecimal 123456 3
  s"123456 / 10^3 = ${n}" == "123456 / 10^3 = 123.456"

If ``text`` is implemented as described in *Section 10.3 Custom interpolator: Text*, ``BigDecimal`` can interpolate into ``Text.s"..."`` for free.

Format specifiers
~~~~~~~~~~~~~~~~~

Python is famous for being able to specify format specifiers when interpolating values:

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

Alternative - Polymorphic interpolable expansion
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Alternate 1: Built-in ``s"..."`` is hardcoded and is only polymorphic on ``IsString``:

::

    -- Desugared expression for M.s"age: ${age}"
    --
    -- s"..." is equivalent to Data.String.Interpolate.s"..."
    M.finalize $
                 M.raw "age: "
      `M.append` M.convert age
      `M.append` M.empty

    {----- Data.String.Interpolate -----}

    finalize :: Monoid s => [s] -> s
    finalize = mconcat

    raw :: IsString s => String -> s
    raw = fromString

    convert :: Interpolate a => a -> s
    convert x = fromString $ interpolateS x ""

    append :: s -> [s] -> [s]
    append = (:)

    empty :: [s]
    empty = []

    -- One instance for all user-defined types; e.g. the `url` library
    -- would just need to implement `Interpolate URL`
    class Interpolate a where
      interpolateS :: a -> ShowS

    {----- Text library: s"..." :: Text -----}

    -- Doesn't have to do anything; built-in works for free.
    -- Could optionally implement a more performant interpolation with
    -- qualified interpolation.

    {----- User-defined SimpleText: SimpleText.s"..." -----}

    -- | Text, except only support interpolating Text values
    newtype SimpleText = SimpleText Text
      deriving newtype (Semigroup, Monoid)

    convert :: Text -> SimpleText
    convert = SimpleText

    finalize = mconcat
    raw = SimpleText . Text.pack
    append = (:)
    empty = []

Alternate 2: Built-in ``s"..."`` with polymorphic builder:

::

    -- Desugared expression for M.s"age: ${age}"
    --
    -- s"..." is equivalent to Data.String.Interpolate.s"..."
    M.finalize $
                 M.raw "age: "
      `M.append` M.convert age
      `M.append` M.empty

    {----- Data.String.Interpolate -----}

    class Interpolable s where
      type Builder s = b | b -> s
      raw      :: String -> Builder s
      append   :: Builder s -> Builder s -> Builder s
      empty    :: Builder s
      finalise :: Builder s -> s

    class Interpolate a builder where
      convert :: a -> builder

    instance Interpolable String where
      type Builder String = ShowS
      raw = fromString
      append = (.)
      empty = id
      finalise = ($ "")

    instance Interpolate String ShowS where
      convert = fromString
    instance Interpolate Int ShowS where
      convert = shows
    -- ...

    {----- Text library: s"..." :: Text -----}

    instance Interpolable Text where
      type Builder Text = Text.Builder
      raw = Text.Builder.fromText
      append = mappend
      empty = mempty
      finalise = Text.toStrict . Text.Builder.toLazyText

    instance Interpolate String Text.Builder where
      convert = Text.Builder.fromString
    instance Interpolate Int Text.Builder where
      convert = Text.Builder.decimal
    -- ...

    {----- User-defined SimpleText: SimpleText.s"..." -----}

    -- | Text, except ONLY support interpolating Text values.
    --
    -- Avoid `Interpolate` class, since we can't forbid someone
    -- from adding a new `Interpolate Foo SimpleText` instance.
    newtype SimpleText = SimpleText Text
      deriving newtype (Semigroup, Monoid)

    convert = Text.Builder.fromText
    raw = Text.Builder.fromText
    append = mappend
    empty = mempty
    finalize = SimpleText . Text.toStrict . Text.Builder.toLazyText

Downsides of Alternate 2 compared to Alternate 1:

* All types that can be interpolated (e.g. a ``URL`` type) must have an instance for each string-like type it can be interpolated within:

    * ``Interpolate URL String``
    * ``Interpolate URL Text``
    * ``Interpolate URL ByteString``
    * ...

  Alternate 1 only needs to implement one ``Interpolate URL``. May be less performant, since we're always going via ``String`` instead of a potentially more efficient conversion (e.g. going straight to ``Text``), but it's a good default.

* All string-like types _must_ implement their own instance, e.g. ``Interpolable Text``, and _must_ implement ``Interpolate`` instances for all built-in types.

  Alternate 1 will automatically work for any ``IsString`` type. Types _may_ implement their own custom interpolation with qualified string interpolation, but that's not a requirement (although recommended, to monomorphize the default interpolation).
