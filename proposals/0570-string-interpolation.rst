Add native string interpolation syntax
======================================

.. author:: Brandon Chinn
.. date-accepted:: 2026-07-26
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/work_items/27592
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/570>`_.
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

This proposal adds S-strings (like Scala's syntax) to Haskell.

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

  -- find-and-replace
  error $
    Text.replace "${x + y}" (Text.show $ x + y) $
    Text.replace "${result}" (Text.show result) $
    "Expected: ${x + y}, got: ${result}"

But each of these options leaves much to be desired:

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

* ``printf`` is partial and unsafe, which especially safety-conscious people may prefer to avoid entirely. Using a safer ``printf`` like ``formatting`` induces a third-party dependency, which is admittedly lightweight, but isn't as seamless as native string interpolation would be

* Quasiquotes induces a dependency on Template Haskell, which a lot of people avoid out of principle. Most QuasiQuoters also add a dependency on ``haskell-src-exts`` to parse arbitrary Haskell expressions, which could technically be avoided by using something like ``ghc-meta`` (`repo <https://github.com/noughtmare/ghc-meta>`_, `GHC issue <https://gitlab.haskell.org/ghc/ghc/-/issues/20862>`_), but this isn't in wide use yet.

* Find-and-replace is much more verbose and a bit less performant. It can also replace too much (e.g. ``Text.replace "${a}" a . Text.replace "${b}" "${a}" $ "${a}${b}"``), and it's possible to get out of sync (e.g. ``Text.replace "${a}" a "${a}${b}"``)

If Haskell had native string interpolation, it would have the benefit and safety of the current third-party quasiquotes without the need for Template Haskell, and be able to take advantage of features like `multiline strings <https://github.com/ghc-proposals/ghc-proposals/pull/569>`_.
::

  error s"Expected: ${x + y}, got: ${result}"

  let textExample = s"${name1} (age: ${age1}) encountered ${name2} (age: ${age2})"

.. _proposed-spec:

Proposed Change Specification
-----------------------------

This proposal introduces the ``-XStringInterpolation`` extension, which enables ``s"Name: ${name}"`` syntax. It synergizes well with ``-XOverloadedStrings``, ``-XMultilineStrings``, and ``-XQualifiedStrings``.

High-level Overview
~~~~~~~~~~~~~~~~~~~

At this proposal's core, the following functionality is added:

* The syntax ``s"age: ${age}"`` expands to a built-in implementation using a new ``Interpolate`` type class to interpolate values such as ``age``

* The syntax ``M.s"age: ${age}"`` expands to a user-defined implementation where the implementor of the module ``M`` has total control over the implementation

  * Same technique as ``-XQualifiedStrings``

* ``s"..."`` expands to ``Data.String.Experimental.s"..."``, where ``Data.String.Experimental`` is a new module in ``ghc-experimental``.

  * The expansion is slightly modified when ``-XOverloadedStrings`` is enabled; see :ref:`overloaded-strings`

Concretely, ``-XStringInterpolation`` enables the following syntax:

::

  s"a ${x + 1} b"

  -- Desugars to:
  interpolateFinalize $
    interpolateRaw "a "      `interpolateAppend`
    interpolateValue (x + 1) `interpolateAppend`
    interpolateRaw " b"      `interpolateAppend`
    interpolateEmpty

These definitions will be provided by ``Data.String.Experimental``, which will be initially implemented in ``ghc-experimental``. See :ref:`machinery` for details.

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

This grammar enables interpolating expressions with nested braces. Concretely, ``istringExprOpen`` and ``istringExprClose`` are only lexed within the ``istring`` and ``istringMultiline`` grammar productions, using Alex start codes. See :ref:`parsing` for examples.

.. _context-free-syntax:

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

.. _machinery:

Machinery
~~~~~~~~~

The following code will live in ``ghc-experimental`` under ``Data.String.Experimental``. After the API has stabilized, these might eventually live in ``base`` under ``Data.String``, alongside ``IsString``.

::

  newtype InterpolateBuilder = InterpolateBuilder {
    unInterpolateBuilder :: forall s. (IsString s, Monoid s) => s
  }

  instance IsString InterpolateBuilder where
    fromString s = InterpolateBuilder (fromString s)
  instance Semigroup InterpolateBuilder where
    InterpolateBuilder s1 <> InterpolateBuilder s2 = InterpolateBuilder (s1 <> s2)
  instance Monoid InterpolateBuilder where
    mempty = InterpolateBuilder mempty

  {----- Implementation of s"..." -----}

  interpolateRaw :: IsString s => String -> s
  interpolateRaw = fromString

  interpolateValue :: (Interpolate a, IsString s, Monoid s) => a -> s
  interpolateValue = unInterpolateBuilder . interpolate

  interpolateAppend :: Monoid s => s -> s -> s
  interpolateAppend = mappend

  interpolateEmpty :: Monoid s => s
  interpolateEmpty = mempty

  interpolateFinalize :: (forall s. (IsString s, Monoid s) => s) -> String
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
    interpolate :: a -> InterpolateBuilder

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

To be more precise, the tokens parsed in :ref:`context-free-syntax` will be expanded as follows:

* An ``istringRaw`` component expands to ``interpolateRaw "<istringRaw>"``

  * ``<istringRaw>`` refers to the string literal stored in the ``istringRaw`` token

  * The string literal passed to ``interpolateRaw`` is a strict ``String`` literal, unaffected by ``-XOverloadedStrings``

* An ``istringExprOpen exp istringExprClose`` component expands to ``interpolateValue (<exp>)``

* The list of ``istring`` components between ``istringBegin`` and ``istringEnd`` expands to the expansion of the components, with ``interpolateAppend`` as "list cons" and ``interpolateEmpty`` as "list nil".

  * The expansion is generated as right-associative; i.e. it will desugar to ``x `interpolateAppend` (y `interpolateAppend` (z `interpolateAppend` ...))`` (parentheses omitted in all the examples for clarity)

  * Related: :ref:`overloaded-strings` and :ref:`qualified-strings`

The desugaring here respects ``RebindableSyntax``, so a project that wishes to use a different desugaring for the default ``s"..."`` syntax may rebind the interpolator bindings as desired.

.. _overloaded-strings:

OverloadedStrings
^^^^^^^^^^^^^^^^^

When ``-XOverloadedStrings`` is enabled, ``s"..."`` expands to ``fromString (Data.String.Experimental.s"...")`` instead. Note this still constructs the string via ``StringBuilder`` -> ``String`` before converting, so string-like types should provide rewrite rules targeting ``fromString (interpolateFinalize f)``; see :ref:`rewrite-rules-for-performant-interpolation` for more details.

Note that the only requirement here is ``fromString``; using string interpolation with ``-XOverloadedStrings`` does not require a ``Monoid`` instance.

.. _qualified-strings:

QualifiedStrings
^^^^^^^^^^^^^^^^

When ``-XQualifiedStrings`` is enabled, you may qualify string interpolation, where ``[modid.]s"..."`` desugars to the same expressions, except resolving the ``interpolate*`` functions as ``[modid.]interpolate*``. ``[modid.]s"..."`` is unaffected by ``-XOverloadedStrings`` because the latter applies only to the non-qualified form ``s"..."``.

Some examples:

::

  Text.s"hello world"

  -- Desugars to:
  Text.interpolateFinalize $
    Text.interpolateRaw "hello world" `Text.interpolateAppend`
    Text.interpolateEmpty

::

  SQL.s"select * from users where name = ${Text.toUpper name} and age = ${age}"

  -- Desugars to:
  SQL.interpolateFinalize $
    SQL.interpolateRaw   "select * from users where name = " `SQL.interpolateAppend`
    SQL.interpolateValue (Text.toUpper name)                 `SQL.interpolateAppend`
    SQL.interpolateRaw   " and age = "                       `SQL.interpolateAppend`
    SQL.interpolateValue age                                 `SQL.interpolateAppend`
    SQL.interpolateEmpty

It's highly recommended that every type with an ``IsString`` instance provides an interpolator that's the monomorphized version of the default interpolator. That way, there's always an option to use ``MyString.s"..."`` if the user does not wish to globally enable ``-XOverloadedStrings``. For example:

::

    module Data.MyString (
      module X,
      interpolateFinalize,
    ) where

    import Data.String.Experimental as X hiding (interpolateFinalize)
    import Data.String.Experimental qualified as S

    interpolateFinalize :: (forall s. (IsString s, Monoid s) => s) -> MyString
    interpolateFinalize = fromString . S.interpolateFinalize

Of course, ``MyString`` is free to implement more string interpolators, but a monomorphized default interpolator should be provided at minimum.

The following laws should hold, if the expression compiles:

* ``M."str" == M.s"str"``

    * That is, a ``-XQualifiedStrings`` string literal and a ``-XStringInterpolation`` string expression with no interpolated values should be equivalent

* ``Data.String.fromString "str" == M.s"str"``

MultilineStrings
^^^^^^^^^^^^^^^^

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

``ghc-experimental`` modules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This proposal would be adding the following modules to ``ghc-experimental``, which would potentially be promoted to a proper library like ``base`` once the feature is stabilized.

.. list-table::
    :align: left

    * - **Module**
      - **Details**
    * - ``Data.String.Experimental``
      - Re-exports ``Data.String.Interpolate.Class.Experimental`` and ``Data.String.Interpolate.Default.Experimental``
    * - ``Data.String.Interpolate.Class.Experimental``
      - Defines the ``Interpolate`` class and instances as written in :ref:`machinery`
    * - ``Data.String.Interpolate.Default.Experimental``
      - Defines the classes and functions for the default ``s"..."`` syntax, as written in :ref:`machinery`
    * - ``Data.String.Interpolate.Builder.Experimental``
      - Defines the default interpolator monomorphized for ``InterpolateBuilder`` for use with ``-XQualifiedStrings``
    * - ``Data.String.Interpolate.ShowS.Experimental``
      - Defines an interpolator useful for implementing ``showsPrec`` (See :ref:`shows-interpolator`)

Template Haskell
~~~~~~~~~~~~~~~~

We are intentionally not adding anything to Template Haskell, as one could just build the expansion directly. String interpolation is still supported in quotes, which will be desugared when translating to TH.

Examples
--------

.. _parsing:

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

.. _rewrite-rules-for-performant-interpolation:

Rewrite rules for performant interpolation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The default interpolator always builds via ``String``, even with ``-XOverloadedStrings`` enabled. String-like types like ``Text`` should define rewrite rules to make interpolation performant. There are two extension points needing rewrite rules:

* ``fromString (interpolateFinalize x)``

  * By default, finalizes with ``StringBuilder`` and lifts with ``fromString``
  * A rewrite rule is needed to finalize with a more efficient builder for the string-like type

* ``interpolateValue``

  * By default, invokes ``interpolate`` which ultimately requires converting through ``String``
  * Rewrite rules are needed for each type that can be converted into the builder type more effeciently than through ``String``

Here are example rewrite rules ``Text`` might write:

::

  {-# RULES
    "interpolateFinalize/Text"
      forall (x :: forall s. (IsString s, Monoid s) => s).
      Text.pack (interpolateFinalize x) = Text.Lazy.toStrict (Text.Builder.toLazyText (x @Text.Builder))

    "interpolateValue/Text.Builder/Text"
      interpolateValue = Text.Builder.fromText
    "interpolateValue/Text.Builder/Int"
      interpolateValue = Text.Builder.decimal
    #-}

Note that the ``interpolateFinalize`` rule needs to target the implementation of ``fromString`` since it's typically inlined before rules fire.

.. _writing-interpolate-instances:

Writing Interpolate instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``Interpolate`` instances will typically be implemented in one of three ways:

* Converting value to ``String`` and calling ``fromString``

* Invoking ``interpolate`` after preprocessing the type into an interpolatable value

* Using string interpolation syntax

  * Requires ``-XOverloadedStrings`` or ``-XQualifiedStrings`` with ``Data.String.Interpolate.Builder.Experimental``

An example using string interpolation:

::

    data SrcLoc = SrcLoc
      { file :: FilePath
      , line :: Int
      , col :: Int
      }

    instance Interpolate SrcLoc where
      interpolate SrcLoc{..} = s"${file}:${line}:${col}"

Because ``InterpolateBuilder`` is a rank-2 type, it keeps the interpolation polymorphic and doesn't incur any performance penalties roundtripping through ``String``.

Effect and Interactions
-----------------------

An existing program containing ``s"..."`` will break when ``-XStringInterpolation`` is enabled. While there's precedent for this (Template Haskell splices make ``$(...)`` different from ``$ (...)``), this is the first instance where whitespace matters for an alphanumeric identifier. But this is not a big deal:

#. It's unlikely for someone to be naming a function as ``s`` in the first place
#. Easy to mitigate: just add a space, which improves readability anyway
#. Prefixing string literals like ``s"..."`` is common in other languages: Python, Scala, JavaScript/TypeScript, etc. so it shouldn't be a big hurdle for newcomers

Interacts nicely with ``-XOverloadedStrings``, ``-XQualifiedStrings``, and ``-XMultilineStrings``. See :ref:`proposed-spec` above.

Costs and Drawbacks
-------------------

Development and maintenance are of moderate effort. Learnability for novice users will go up, since novice users probably expect string interpolation to be available, and might be frustrated at the lack of support currently.

One minor drawback is the whitespace sensitivity of ``s"``, as discussed in "Effect and Interactions".

Prior to this proposal, ``fromString`` had the implicit assumption that it was intended to run on string literals in a program. With this proposal, ``fromString`` may now also be called on the full string interpolation, which may include user input and may introduce vulnerabilities. We deem this low risk, however, as one could always call ``fromString`` oneself.

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
  * See :ref:`community-survey`

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

* Instead of string-specific logic, allow a general syntax for variadic functions

  * String interpolation could then just be a variadic function that takes multiple arguments of type ``Interpolate a => a``
  * Would be a nice language construct on its own, but its unfamiliarity would be a major loss for this feature
  * Also doesn't compose well with other features e.g. with multiline strings

Expansion-related Alternatives
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Hardcode to Monoid's ``mappend`` and ``mempty``

  * Would remove potential use-cases needing type-changing ``append``, e.g. ``a -> [a] -> [a]`` or ``f a -> f as -> f (a ': as)``
  * It's likely that most custom interpolators will implement ``interpolateAppend``/``interpolateEmpty`` with ``Monoid``, but we should avoid restricting the interface here

* Use ``M.fromString`` instead of ``interpolateRaw``, to more tightly connect ``StringInterpolation`` with ``QualifiedStrings``

  * While ``QualifiedStrings`` and ``StringInterpolation`` are closely related, and implementations *ought* to implement them consistently, the language feature should not enforce it, in the same way that typeclass laws are not enforced by the language
  * Even if we hardcoded ``fromString``, one could still devise a custom string interpolator that's inconsistent with ``M.fromString``; e.g. ``interpolateFinalize _ = "bad"``

* Hardcode a wired-in ``Interpolate`` class with ``interpolateValue`` (and potentially ``interpolateRaw``)

  * Redundant with the rebindable functionality with ``-XQualifiedStrings``

* Add an ``InterpolateBuilder`` type family to specify a builder type for the interpolator ``s`` and define ``interpolateFinalize`` with that type family

  * Pro: eliminates the use of ``fromString`` for the final string (see Cost and Drawbacks)
  * Con: String interpolation requires adding a new instance; in the current proposal, anything with ``IsString`` gets string interpolation for free

* Put the ``fromString`` inside ``interpolateFinalize`` and add ``@String`` if non-``OverloadedStrings``

  * Pro: ``OverloadedStrings`` and `QualifiedStrings`` now have the same expansion (modulo the module specified)
  * Con: In the current proposal, ``OverloadedStrings`` adds a final ``fromString`` in both a normal string literal and a string interpolation. Doing this option would remove that symmetry

Delimiter-related Alternatives
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Allow ``$foo`` in addition to ``${foo}``

  * This would complicate the syntax, and would also require interpolated strings to escape bare ``$``.

* Different quote delimiter

  * ``s"..."`` was taken from Scala's interpolation syntax
  * Could use ``f"..."`` like Python, with ``f`` for format, but ``f`` is a common variable name for functions and if the user forgets to enable ``-XStringInterpolation``, ``f"..."`` would parse as ``f "..."`` which is likely to be valid.
  * Could use ``i"..."``, with ``i`` for interpolate.
  * Could reuse QuasiQuote syntax, e.g. ``[s|`` or ``[fmt|``, except it would be special and NOT use Template Haskell.
  * Could do ``''...''``, since ``''`` is invalid Haskell syntax today. However, code highlighters that aren't updated for ``-XStringInterpolation`` yet would not gracefully handle this.

* No quote delimiter, always interpolate

  * e.g. if we switch the delimiter to ``\{...}``, which is currently invalid in a string
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

  * See :ref:`community-survey`

Unresolved Questions
--------------------

Implementation Plan
-------------------

I have a prototype started `here <https://gitlab.haskell.org/ghc/ghc/-/compare/master...wip%2Finterpolated-strings>`_

Endorsements
------------

Appendix
--------

.. _community-survey:

Community Survey
~~~~~~~~~~~~~~~~

I sent out multiple community surveys, the last one being open 2025-04-21 to 2025-04-30. Raw data and analysis can be found here: https://github.com/brandonchinn178/ghc-string-interpolation-prototypes/tree/main/results

Performance consideration
~~~~~~~~~~~~~~~~~~~~~~~~~

Strings are notorious for O(n^2) concatenations, but the current proposal builds with ``ShowS``, so it should remain linear. The only case where it might be O(n^2) is when nesting interpolated strings inside interpolated strings (although benchmarking still shows this to be linear in practice).

Benchmarks: https://github.com/brandonchinn178/ghc-string-interpolation-prototypes/tree/main/bench

.. _shows-interpolator:

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

The ``text`` library should provide rewrite rules as described in :ref:`rewrite-rules-for-performant-interpolation`, which would allow performant interpolation with the default interpolator.

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

  import Data.HTML
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

  HTML.s"<h1>${title}</h1>${HTML.raw body}"
    == Html "<h1>Why is 1 &gt; 0?</h1><p>Hello world</p>"

Custom interpolator: Ascii
~~~~~~~~~~~~~~~~~~~~~~~~~~

The existing machinery is general enough to support ``-XRequiredTypeArguments``, to support interpolators with compile-time validations:

::

    interpolateRaw :: forall (s :: Symbol) -> (KnownSymbol s, AsciiOnly s) => String
    interpolateRaw s = symbolVal (Proxy @s)

    interpolateValue :: InterpolateAscii a => a -> String
    interpolateValue = interpolateAscii

    interpolateAppend = (<>)
    interpolateEmpty = ""
    interpolateFinalize = id

    class InterpolateAscii a where
      interpolateAscii :: a -> String
    instance InterpolateAscii Int where
      interpolateAscii = show

    -- | A usable constraint that raises a nice TypeError on failure.
    type AsciiOnly :: Symbol -> Constraint
    type family AsciiOnly s where
      AsciiOnly s = Unless (IsAscii s) (
        TypeError ('Text "Symbol " ':<>: 'ShowType s ':<>: 'Text " is not ASCII-only")
        )

    type Unless :: Bool -> Constraint -> Constraint
    type family Unless s b where
      Unless 'True _ = ()
      Unless 'False c = c

    -- | Walk the Symbol one Char at a time via UnconsSymbol.
    type IsAscii :: Symbol -> Bool
    type family IsAscii s where
      IsAscii s = IsAsciiGo (UnconsSymbol s)

    type IsAsciiGo :: Maybe (Char, Symbol) -> Bool
    type family IsAsciiGo m where
      IsAsciiGo 'Nothing            = 'True
      IsAsciiGo ('Just '(c, rest))  = (CharToNat c <=? 127) && IsAscii rest

::

    Ascii.s"this is valid: ${123 :: Int}"

    -- Ascii.s"this is a type error: 🙂"

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
