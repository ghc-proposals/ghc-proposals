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

This proposal introduces the ``-XStringInterpolation`` extension, with support for ``-XOverloadedStrings`` and ``-XQualifiedStrings`` (`proposal <https://github.com/ghc-proposals/ghc-proposals/pull/723>`_).

High-level Overview
~~~~~~~~~~~~~~~~~~~

At its core, this proposal proposes the following system:

* The syntax ``s"age: ${age}"`` expands to a built-in implementation hardcoded to ``IsString`` and a new ``Interpolate`` type class with an ``interpolate :: a -> String`` function.

* The syntax ``M.s"age: ${age}"`` expands to a user-defined implementation where the implementor of the module ``M`` has total control over the implementation

    * Same technique as ``-XQualifiedStrings``

* ``s"..."`` is exactly equivalent to ``Data.String.Interpolate.Experimental.s"..."``, where ``Data.String.Interpolate.Experimental`` is a new module in ``ghc-experimental``.

Concretely, ``-XStringInterpolation`` enables the following syntax:

::

  s"a ${x + 1} b"

  -- Desugars to:
  Data.String.Interpolate.Experimental.interpolateString
    ( \convert raw append empty ->
                 raw "a "
        `append` convert (x + 1)
        `append` raw " b"
        `append` empty
    )
    :: String

``Data.String.Interpolate.Experimental`` will be initially provided by ``ghc-experimental``, containing the following:

::

  -- See *Section 2.4* for the type of this definition
  interpolateString f = mconcat $ f (fromString . interpolate) id (:) []

  class Interpolate a where
    {-# MINIMAL interpolate | interpolateS #-}

    interpolate :: a -> String
    interpolate x = interpolateS x ""

    interpolateS :: a -> ShowS
    interpolateS x s = interpolate x <> s

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

The following code will live in ``ghc-experimental`` under ``Data.String.Interpolate.Experimental``. After the API has stablized, these would eventually live in ``Data.String`` alongside ``IsString``.

::

  type SimpleStringInterpolator s =
    ( forall ss.
      (forall a. Interpolate a => a -> s)
      -> (s -> s)
      -> (s -> ss -> ss)
      -> ss
      -> ss
    )
    -> s

  interpolateString :: (IsString s, Monoid s) => SimpleStringInterpolator s
  interpolateString f = mconcat $ f (fromString . interpolate) id (:) []
  {-# INLINE interpolateString #-}

  class Interpolate a where
    {-# MINIMAL interpolate | interpolateS #-}

    interpolate :: a -> String
    interpolate x = interpolateS x ""

    interpolateS :: a -> ShowS
    interpolateS x s = interpolate x <> s

``interpolateS`` is necessary in order to interpolate recursive data structures in linear time, but ``interpolate`` is more straightforward for simple data types.

Instances will be provided as well, for example:

::

  instance Interpolate String where
    interpolateS = showString
  instance Interpolate Char where
    interpolateS = showChar

  instance Interpolate Int where
    interpolateS = shows
  instance Interpolate Double where
    interpolateS = shows
  instance Interpolate Bool where
    interpolateS = shows

  instance Interpolate a => Interpolate (Maybe a) where
    interpolateS Nothing = showString "Nothing"
    interpolateS (Just a) = showString "Just (" . interpolateS a . showChar ')'

Expansion
~~~~~~~~~

With the machinery defined above, the following interpolated string desugars to the below expression:

::

  -- original string
  s"foo ${f a b} bar ${g x} baz ${name}"

  -- desugared
  Data.String.Interpolate.Experimental.interpolateString
    ( \convert raw append empty ->
                 raw "foo "
        `append` convert (f a b)
        `append` raw " bar "
        `append` convert (g x)
        `append` raw " baz "
        `append` convert name
        `append` empty
    )
    :: String

Namely:

* An ``istringRaw <str>`` component expands to ``raw "<str>"``
* An ``istringExprOpen exp istringExprClose`` component expands to ``convert (<exp>)``
* The right-associated list of ``istring`` components between ``istringBegin`` and ``istringEnd`` expands to the expansion of the components, with ``append`` as "list cons" and ``empty`` as "list nil".

If ``-XOverloadedStrings`` is enabled, the string literals passed to ``raw`` are overloaded and the ``:: String`` annotation is removed. The ``:: String`` annotation is included when ``-XOverloadedStrings`` is disabled; it's redundant in most cases, since ``raw "..."`` would resolve the return type to ``String``, but it's necessary if there are no ``raw`` calls (e.g. ``s"${a}${b}"``).

QualifiedStrings
~~~~~~~~~~~~~~~~

When ``-XQualifiedStrings`` is enabled, you may qualify string interpolation as well:

::

  Text.s"hello world"

  -- Desugars to:
  Text.interpolateString $ \_ raw _ _ -> raw "hello world"

  SQL.s"select * from users where name = ${Text.toUpper name} and age = ${age}"

  -- Desugars to:
  SQL.interpolateString $ \convert raw append empty ->
             raw "select * from users where name = "
    `append` convert (Text.toUpper name)
    `append` raw " and age = "
    `append` convert age
    `append` empty

The desugaring will NOT include a ``:: String`` annotation, whether ``-XOverloadedStrings`` is enabled or not.

It is highly recommended that any type with an ``IsString`` instance provide the below definition for use with ``-XQualifiedStrings``. This allows using locally-scoped string interpolation for non-String types without enabling it globally with ``-XOverloadedStrings``.

::

  interpolateString :: SimpleStringInterpolator MyString
  interpolateString = Data.String.Interpolate.Experimental.interpolateString

The following laws should hold, if the expression typechecks:

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

When ``-XOverloadedStrings`` is enabled, string interpolation can be used for any type with an ``IsString`` instance. Otherwise, it will only ever build Strings.

When ``-XQualifiedStrings`` is enabled, ``M.s"..."`` syntax is enabled, as described above. ``M`` should define ``interpolateString`` with concrete ``String`` inputs, so that the string literals concretize when ``-XOverloadedStrings`` is enabled as well. For example, defining ``interpolateString`` for an ``IsString`` type should be implemented as:

::

    import Data.String.Interpolate.Experimental qualified as S

    interpolateString :: S.SimpleStringInterpolator MyText
    interpolateString = S.interpolateString

Interpolation is also supported with ``-XMultilineStrings``, as described in "Proposed Change Specification".

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
  * Most custom ``interpolateString`` might simply use Monoid, but at least the default ``interpolateString`` does not, in order to use a potentially more efficient ``mconcat`` (e.g. for ``Text``)

* Use ``M.fromString`` instead of ``raw``, to more tightly connect ``StringInterpolation`` with ``QualifiedStrings``

  * While ``QualifiedStrings`` and ``StringInterpolation`` are closely related, and implementions *ought* to implement them consistently, the language feature should not enforce it, in the same way that typeclass laws are not enforced by the language
  * Even if we hardcoded ``fromString``, one could still devise a custom ``M.interpolateString`` that's inconsistent with ``M.fromString``; e.g. a law-breaking ``Monoid`` instance such that ``x `mappend` mempty /= x``

* Hardcode a wired-in ``Interpolate`` class with ``convert`` (and potentially ``raw``)

  * Eliminates the whole benefit of a rebindable ``interpolateString``; having a blessed ``Interpolate`` class with a ``convert :: something`` function means everyone has to use that ``something`` type. The whole benefit of a rebindable ``interpolateString`` is, just like ``QualifiedDo``, as long as it typechecks, you can make its type whatever you want.

* Replace the callback ``M.interpolateString $ \raw convert append empty -> raw "age: " `append` convert age `append` empty`` with the expression directly, e.g. ``M.fromString "age: " `M.mappend` M.convert age `M.mappend` M.mempty``.

  * This would remove the major benefit of having one function to link to when clicking into an interpolated string, in an IDE / using a language server
  * This would also remove the ability to add a "finalizer", where perhaps it's more efficient to append in one type, but the final type should be "compiled"/"built" in some fashion
    * You could add ``M.finalizeInterpolatedString`` at the end, but then that's becoming more complicated than the current approach
  * See also the other alternatives to hardcode the operations

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

* No delimiter, always interpolate

  * Would require any use of ``${...}`` to be escaped.
  * No other language does this; even Bash has single quoted strings to avoid escaping

* Different interpolation delimiter, e.g. ``#{foo}``

  * Most languages use ``$``, and I see no reason to deviate

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

Strings are notorious for O(n^2) concatenations, but this only happens if you left-associate the concatenations. The desugaring here is always right-associated, so it should remain linear. The only case where it might be O(n^2) is when nesting interpolated strings inside interpolated strings (although benchmarking still shows this to be linear in practice).

Benchmarks: https://github.com/brandonchinn178/ghc-string-interpolation-prototypes/tree/main/bench

Builder
~~~~~~~

A performance-minded person might want to take advantage of ``interpolateS`` and defer realizing the string until the very end.

::

  module Data.String.Builder.Interpolate where

  import Data.String.Interpolate.Experimental qualified as S

  newtype Builder = Builder (Endo String)
    deriving newtype (Monoid, Semigroup)

  build :: Builder -> String
  build (Builder (Endo f)) = f ""

  interpolateString f = f convert raw mappend mempty
    where
      convert = Builder . Endo . interpolateS
      raw = Builder . Endo . showString

With this definition, one can nest interpolated strings with linear performance:

::

  {-# LANGUAGE QualifiedStrings #-}
  {-# LANGUAGE StringInterpolation #-}

  import Data.String.Builder.Interpolate qualified as B

  main = do
    let name = "Alice"
    let age = 10

    let s1 = B.s"Name: ${name}!"
    let s2 = B.s"Age: ${age}!"

    print $ B.build B.s"${s1} + ${s2}"

Text
~~~~

``text`` would not need any specific work, since it already supports ``IsString``. But it would be highly recommended for ``text`` to define a module for use with ``QualifiedStrings``:

::

  module Data.Text.Interpolate where

  import Data.String.Interpolate.Experimental qualified as S

  interpolateString ::
    ( (forall a. Interpolate a => a -> Text)
        -> (String -> Text)
        -> (Text -> Text -> Text)
        -> Text
        -> Text
    )
    -> Text
  interpolateString = S.interpolateString

With this support, users can write the following:

::

  {-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE QualifiedStrings #-}
  {-# LANGUAGE StringInterpolation #-}

  import Data.Text.Interpolate qualified as T

  main = do
    let name = "Alice"
    let age = 10

    -- with overloaded strings
    print $ T.toUpper s"Name: ${name}, Age: ${age}"

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

The library would also define a module for use with ``QualifiedStrings``:

::

  module Data.SQL.Interpolate where

  import Data.String qualified as S
  import Data.String.Interpolate.Experimental qualified as S

  interpolateString ::
    ( (forall a. Interpolate a => a -> SqlQuery)
      -> (String -> SqlQuery)
      -> (SqlQuery -> SqlQuery -> SqlQuery)
      -> SqlQuery
      -> SqlQuery
    )
    -> SqlQuery
  interpolateString f = f convert raw mappend mempty
    where
      convert = interpolate
      raw = S.fromString

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

  module Data.SQL.Compile.Interpolate where

  import Data.SQL.Interpolate qualified as SQL

  interpolateString ::
    ( (forall a. ToSqlValue a => a -> SqlQuery)
      -> (String -> SqlQuery)
      -> (SqlQuery -> SqlQuery -> SqlQuery)
      -> SqlQuery
      -> SqlQuery
    )
    -> Either ParseError CompiledSqlQuery
  interpolateString = compileQuery . SQL.interpolateString

::

  import Data.SQL.Interpolate qualified as SQL

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

  import Data.String.Interpolate.Experimental qualified as S

  interpolateString ::
    ( (forall a. Interpolate a => a -> Html)
      -> (String -> Html)
      -> (Html -> Html -> Html)
      -> Html
      -> Html
    )
    -> Html
  interpolateString f = f interpolate raw mappend mempty

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

If ``text`` is implemented as described in *Section 10.4 Custom interpolator: Text*, ``BigDecimal`` can interpolate into ``Text`` for free.

Format specifiers
~~~~~~~~~~~~~~~~~

Python is famous for being able to specify format specifiers when interpolating values:

.. code-block:: python

  x = 1.2
  f"{x:.3f}" == "1.200"

This would be provided by libraries, and the design and implementation of those libraries is not specified here. But from a user perspective, here's one possible way such a library could be used:

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

Current proposal - built-in ``s"..."`` is hardcoded and is only polymorphic on ``IsString``:

::

    -- Desugared expression for M.s"age: ${age}"
    --
    -- s"..." is equivalent to Data.String.Interpolate.s"..."
    M.interpolateString $ \convert raw append empty ->
               raw "age: "
      `append` convert age
      `append` empty

    {----- Data.String.Interpolate -----}

    -- Conceptually, think of this as a more performant version of:
    --   fromString $ raw "age: " <> convert age
    interpolateString f = mconcat $ f convert raw (:) []
      where
        convert = fromString . interpolate
        raw = id

    -- One instance for all user-defined types; e.g. the `url` library
    -- would just need to implement
    --   Interpolate URL
    class Interpolate a where
      interpolate :: a -> String

    {----- Text library: s"..." :: Text -----}

    -- Doesn't have to do anything; built-in works for free.
    -- Could optionally implement a more performant interpolation with
    -- qualified interpolation.

    {----- User-defined SimpleText: SimpleText.s"..." -----}

    -- | Text, except only support interpolating Text values
    newtype SimpleText = SimpleText Text

    interpolateString f = f convert raw mappend mempty
      where
        convert = SimpleText
        raw = SimpleText . Text.pack

Built-in ``s"..."`` with polymorphic builder:

::

    -- Desugared expression for M.s"age: ${age}"
    --
    -- s"..." is equivalent to Data.String.Interpolate.s"..."
    M.finalize
      (            M.raw "age: "
        `M.append` M.convert age
        `M.append` M.empty
      )

    {----- Data.String.Interpolate -----}

    class Interpolable s where
      type Builder s
      raw      :: String -> Builder s
      append   :: Builder s -> Builder s -> Builder s
      empty    :: Builder s
      finalise :: Builder s -> s

    class Interpolate a builder where
      convert :: a -> builder

    instance Interpolable String where
      type Builder String = String
      raw = id
      append = (++)
      empty = []
      finalise = id

    instance Interpolate String String where
      convert = id
    instance Interpolate Int String where
      convert = show
    -- ...

    {----- Text library: s"..." :: Text -----}

    instance Interpolable Text where
      type Builder Text = TextBuilder
      raw = Text.Builder.fromText
      append = mappend
      empty = mempty
      finalise = Text.toStrict . Text.Builder.toLazyText

    instance Interpolate String Text where
      convert = Text.pack
    instance Interpolate Int Text where
      convert = Text.pack . show
    -- ...

    {----- User-defined SimpleText: SimpleText.s"..." -----}

    -- | Text, except ONLY support interpolating Text values.
    -- Avoid `Interpolate` class, since we can't forbid someone
    -- from adding a new `Interpolate Foo SimpleText` instance.
    newtype SimpleText = SimpleText Text

    convert = Text.Builder.fromText
    raw = Text.Builder.fromText
    append = mappend
    empty = mempty
    finalize = SimpleText . Text.toStrict . Text.Builder.toLazyText

Downsides of alternative approach:

* All types that can be interpolated (e.g. a ``URL`` type) must have an instance for each string-like type it can be interpolated within:

    * ``Interpolate URL String``
    * ``Interpolate URL Text``
    * ``Interpolate URL ByteString``
    * ...

  The current proposal only needs to implement one ``Interpolate URL``. May be less performant, since we're always going via ``String`` instead of a potentially more efficient conversion (e.g. going straight to ``Text``), but it's a good default.

* All string-like types _must_ implement their own instance, e.g. ``Interpolable Text``, and _must_ implement ``Interpolate`` instances for all built-in types.

  The current proposal will automatically work for any ``IsString`` type. Types _may_ implement their own custom interpolation with qualified string interpolation, but that's not a requirement (although recommended, to monomorphize the default interpolation).
