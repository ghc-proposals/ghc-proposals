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

But each of these leave things to be desired:

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

Grammar
~~~~~~~

Add a new ``-XStringInterpolation`` extension, which, when enabled, adds a new expression to `Chapter 3 of the Haskell 2010 Report <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-220003>`_ after do-expressions in the ``exp`` rule:

.. code-block:: abnf

  exp → ...
      | case exp of { alts }                    (case expression)
      | do { stmts }                            (do expression)
      | s"   interpolatedString "               (interpolated strings)
      | s""" interpolatedStringMultiline """    (interpolated multiline strings)
      | fexp

Note that no space is allowed between the ``s`` and ``"``.

The ``interpolatedString`` and ``interpolatedMultilineString`` rules are new rules defined as follows:

.. code-block:: abnf

  interpolatedString               → {graphic⟨\ | "   | ${⟩ | space | escape | gap} [ ${ exp } interpolatedString ]
  interpolatedStringMultiline      → {{whitechar} interpolatedStringMultilineLine}
  interpolatedStringMultilineLine  → {graphic⟨\ | """ | ${⟩ | space | escape | gap} [ ${ exp } interpolatedStringMultilineLine ]

Note: We will always require interpolation to be of the form ``${foo}``, unlike other languages where ``$foo`` is allowed for single-variable interpolation. This simplifies the syntax and avoids the need for escaping bare ``$``. Interpolation may be avoided by escaping the dollar sign; e.g. ``s"\${foo}"`` is equivalent to ``"${foo}"``.

The following expressions are lex errors, according to the above grammar:

* ``s"a ${} b"``

  * Expression is missing

* ``s"a ${=} b"``

  * Not an expression

* ``s"a ${let x =} b"``

  * Incomplete expression

* ``s"a ${{b} c"``

  * The second ``{`` is not a valid character to start an expression

See the "Semantics" and "Examples" sections to see examples of valid interpolated strings.

Machinery
~~~~~~~~~

An interpolated string expression desugars to calls to ``fromBuilder``, ``toBuilder``, and ``interpolate``, which are defined as:

::

  -- | Laws:
  --     * fromBuilder . toBuilder === id
  --     * toBuilder . fromBuilder === id
  class Monoid (Builder s) => Buildable s where
    type Builder s = r | r -> s
    toBuilder :: s -> Builder s
    fromBuilder :: Builder s -> s

  class Buildable s => Interpolate a s where
    interpolate :: a -> Builder s

Instances for ``String`` will be defined as well:

::

  newtype StringBuilder = StringBuilder (Endo String)
    deriving newtype (Semigroup, Monoid)

  instance Buildable String where
    type Builder String = StringBuilder
    toBuilder s = StringBuilder (Endo (s ++))
    fromBuilder (StringBuilder (Endo f)) = f []

  instance Interpolate String String where
    interpolate = toBuilder
  instance {-# OVERLAPPABLE #-} Show a => Interpolate a String where
    interpolate = StringBuilder . Endo . shows

This design allows interpolating anything, even user-defined types, into a ``String`` with ``Show``, but can be overridden for specific types. See the "Examples" section for more details.

These definitions would initially be implemented in ``ghc-experimental`` under ``Data.String.Interpolate``. After the API has stablized, these would eventually live in ``GHC.Exts`` alongside ``IsString``.

Semantics
~~~~~~~~~

With the machinery defined above, the following interpolated string desugars to the below expression:

::

  -- original string
  s"foo ${f a b} bar ${g x} baz ${name}"

  -- desugared
  fromBuilder $
    toBuilder "foo "
    <> interpolate (f a b) <> toBuilder " bar "
    <> interpolate (g x)   <> toBuilder " baz "
    <> interpolate name    <> toBuilder ""

The string literals there will be handled by ``-XOverloadedStrings`` as usual, if enabled.

Interpolated multiline strings should desugar into a single-line string `as usual <https://github.com/brandonchinn178/ghc-proposals/blob/master/proposals/0569-multiline-strings.rst>`_ first, before interpolating expressions.

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

Examples
--------

Examples were tested with `this gist <https://gist.github.com/brandonchinn178/4d35ed189d7018ca34535ac85442790b>`_ (after desugaring the string interpolation).

Edge cases
~~~~~~~~~~

The following interpolated string expressions are also valid:

::

  let x = "hello"

  -- nested string interpolation
  s"a ${x ++ s" world ${x}"} b" == "a hello world hello b"

  -- seemingly duplicated closing bracket is valid, as the first one closes the expression
  -- and the second is a character in the string literal
  s"${x}} world" == "hello} world"

  -- inline type annotation
  s"a ${1 :: Int} b" == "a 1 b"

Text
~~~~

These instances will be provided in ``Data.Text``. This adds a dependency on ``ghc-experimental``, but IMO it should be fine, since ``ghc-experimental`` is a boot library. If the ``text`` maintainers are not okay with that, we could also hide it behind a Cabal flag.

::

  instance Buildable Text where
    type Builder Text = Text.Builder
    toBuilder = Text.Builder.fromText
    fromBuilder = Text.Lazy.toStrict . Text.Builder.toLazyText

  instance Interpolate Text Text where
    interpolate = toBuilder
  instance {-# OVERLAPPABLE #-} Show a => Interpolate a Text where
    interpolate = interpolate . show

  instance Interpolate String Text where
    interpolate = interpolate . Text.pack
  instance Interpolate Text String where
    interpolate = interpolate . Text.unpack

This is fairly similar to String, with one addition: we also need to define ``Interpolate`` for interpolating between String and Text. Text would probably also be the one to implement interpolation with ByteString, as Text depends on ByteString, not vice versa.

Similar instances can also be implemented for lazy Text.

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
    fromString s = SqlQuery{sqlText = Text.pack s, sqlValues = []}
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

That library could define the following instances:

::

  newtype SqlQueryBuilder = SqlQueryBuilder (Endo SqlQuery)
    deriving newtype (Semigroup, Monoid)

  instance Buildable SqlQuery where
    type Builder SqlQuery = SqlQueryBuilder
    toBuilder q = SqlQueryBuilder (Endo (q <>))
    fromBuilder (SqlQueryBuilder (Endo f)) = f mempty

  instance Interpolate SqlQuery SqlQuery where
    interpolate = toBuilder
  instance Interpolate Text SqlQuery where
    interpolate s = toBuilder SqlQuery{sqlText = "?", sqlValues = [SqlText s]}
  instance Interpolate String SqlQuery where
    interpolate = interpolate . Text.pack
  instance Interpolate Int SqlQuery where
    interpolate x = toBuilder SqlQuery{sqlText = "?", sqlValues = [SqlInt x]}

And gain access to safe string interpolation without SQL injection:

::

  let age = 10 :: Int
  let name = "Robert'); DROP TABLE Students;--" :: String

  s"SELECT * FROM tab WHERE age = ${age} AND name ILIKE ${name}"
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
        , [s"name ILIKE ${nameFilter}"]
        ]
    conjoin cs = mconcat $ intersperse " AND " (cs :: [SqlQuery])

  s"SELECT * FROM tab WHERE ${conjoin whereClauses}"
    == SqlQuery
        { sqlText = "SELECT * FROM tab WHERE age > 18 AND name ILIKE ?"
        , sqlValues = [SqlText "A%"]
        }

Custom interpolator: HTML
~~~~~~~~~~~~~~~~~~~~~~~~~

Imagine a library implements a new ``Html`` type like:

::

  newtype Html = Html Text
    deriving newtype (Show, IsString, Semigroup, Monoid)

  escapeHtml :: Text -> Text
  escapeHtml = Text.replace "<" "&lt;" . Text.replace ">" "&gt;"

  newtype RawHtml = RawHtml {unRawHtml :: Text}

That library could define the following instances:

::

  newtype HtmlBuilder = HtmlBuilder (Endo Html)
    deriving newtype (Semigroup, Monoid)

  instance Buildable Html where
    type Builder Html = HtmlBuilder
    toBuilder s = HtmlBuilder (Endo (s <>))
    fromBuilder (HtmlBuilder (Endo f)) = f mempty

  instance Interpolate String Html where
    interpolate = interpolate . Text.pack
  instance Interpolate Text Html where
    interpolate = toBuilder . Html . escapeHtml
  instance Interpolate RawHtml Html where
    interpolate = toBuilder . Html . unRawHtml
  instance {-# OVERLAPPABLE #-} Show a => Interpolate a Html where
    interpolate = interpolate . show

And gain access to safe string interpolation with HTML escaping by default:

::

  let title = "Why is 1 > 0?" :: Text
  let body = "<p>Hello world</p>" :: Text

  s"<h1>${title}</h1>${RawHtml body}"
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

That library could define the following instances:

::

  instance Interpolate BigDecimal String where
    interpolate = interpolate . renderBigDecimal
  instance Interpolate BigDecimal Text where
    interpolate = interpolate . Text.pack . renderBigDecimal

And be able to use it in interpolated strings:

::

  let n = BigDecimal 123456 3
  s"123456 / 10^3 = ${n}" == "123456 / 10^3 = 123.456"

Effect and Interactions
-----------------------

When ``-XOverloadedStrings`` is enabled, string interpolation can be used for any ``Buildable`` type. Otherwise, it will only ever build Strings.

Interpolation is also supported with ``-XMultilineStrings``, as described in "Proposed Change Specification".

Costs and Drawbacks
-------------------

Development should be low-effort, maintenance should be low-effort. Learnability for novice users will go up, since novice users probably expect string interpolation to be available, and might be frustrated at the lack of support currently.

The major drawback of this approach is the typeclass instances problem:

#. A new interpolator type (e.g. ``SqlQuery``) needs to define ``Builder`` and ``Interpolate`` for all known interpolatable types
#. A new interpolatable type (e.g. ``BigDecimal``) needs to define ``Interpolate`` for all known interpolator types

This is worse than ``IsString`` or ``Show`` due to the multi-param ``Interpolate`` type class. This makes ``Interpolate`` much more susceptible to orphan instances.

One minor drawback is that whitespace is now important with this syntax, with ``s"foo"`` semantically different from ``s "foo"``. While there's precedent for this (Template Haskell splices make ``$(...)`` different from ``$ (...)``), this is the first instance where whitespace matters for an alphanumeric identifier. But IMO this isn't that big of a deal:

#. It's unlikely for someone to be naming a function as ``s`` in the first place
#. Prefixing string literals like ``s"..."`` is common in other languages: Python, Scala, Javascript/Typescript, etc.
#. Easily mitigatable: just add a space, which improves readability anyway

Alternatives
------------

* Status quo (discussed in the "Motivation" section)

* Different delimiter

  * Could use ``f"`` like Python, with ``f`` for format. ``s`` for "String" seems a bit ad-hoc, but it does "look better" for some reason. ``s`` is also a bit better if the user forgets to enable ``-XStringInterpolation`` because ``f`` is a not-uncommon name for functions and ``f"asdf"``, being parsed as ``f "asdf"``, would work more often than ``s"asdf"`` would.
  * Could reuse QuasiQuote syntax, e.g. ``[s|`` or ``[fmt|``, except it would be special and NOT use Template Haskell.

* Different interpolation delimiter, e.g. ``#{foo}``

  * Most languages use ``$``, and I see no reason to deviate

Unresolved Questions
--------------------

Implementation Plan
-------------------

I can implement

Endorsements
------------
