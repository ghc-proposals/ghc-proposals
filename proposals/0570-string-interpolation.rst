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

When a new ``StringInterpolation`` extension is enabled, the following syntax:
::

  s"foo ${f a b} bar ${g x} baz ${name}"

will be syntax sugar for:
::

  ("foo " ++)
  . interpolatePrec 0 (f a b)
  . (" bar " ++)
  . interpolatePrec 0 (g x)
  . (" baz " ++)
  . interpolatePrec 0 name
  $ ""

where:
::

  class Interpolate a where
    {-# MINIMAL interpolate | interpolatePrec #-}

    interpolate :: a -> String
    interpolate a = interpolatePrec 0 a ""

    interpolatePrec :: Int -> a -> ShowS
    interpolatePrec _ a s = interpolate a ++ s

    -- personally I would rather leave this out and just use
    -- OVERLAPPING + TypeSynonymInstances to define
    -- `Interpolate String`, but I'll mimic Show entirely for now
    interpolateList :: [a] -> ShowS
    interpolateList xs = ('[':) . go True xs . (']':)
      where
        go _ [] = id
        go isStart (x:xs) = (if isStart then id else (',' :)) . interpolatePrec 0 x . go False xs

with instances mostly mirroring ``Show``, except for things like ``Char`` or ``String``, which would not include the quotes.

This proposal adds a new class instead of ``Show`` because ``Show`` is intended to return the Haskell source code used to create the value. A quick search indicates that nothing on Stackage currently defines an ``Interpolate`` type class (alternatively, the class + functions could be named ``Display``, which is only in use by ``rio``).

Could optionally provide a newtype for easy derivation with ``DerivingVia``, or even make ``Interpolate`` one of the built-in class that can be derived (like ``Show``):
::

  newtype InterpolateFromShow a = InterpolateFromShow a

  instance Interpolate (InterpolateFromShow a) where
    interpolatePrec = showsPrec

  data Foo = Foo Int Bool
    deriving (Show)
    deriving (Interpolate) via InterpolateFromShow Foo

We will always require interpolation to be of the form ``${foo}``, unlike other languages where ``$foo`` is allowed for single-variable interpolation. This simplifies the syntax and avoids the need for escaping bare ``$``. Interpolation may be escaped by escaping the dollar sign; e.g. ``"\${foo}"`` is equivalent to ``['$', '{', 'f', 'o', 'o', '}']``.

Any Haskell expression may go inside the braces, but an unmatched ``}`` will immediately end processing. This restriction simplifies the implementation and isn't a big deal in practice, since one could always break it out into a helper variable.

When ``OverloadedStrings`` is enabled, calls ``fromString`` with the result.

If the `multiline proposal gets accepted <https://github.com/ghc-proposals/ghc-proposals/pull/569>`_, interpolation syntax will also be enabled for multiline syntax, e.g.
::

  -- "Line 1: 100\nLine 2: 200\n"
  let s =
        s"""
        Line 1: ${x}
        Line 2: ${y}
        """
      x = 100
      y = 200

Examples
--------

An ``Interpolate`` instance different from ``Show``:
::

  data TestFailure = TestFailure { message :: String, file :: FilePath, lineNumber :: Int }
    deriving (Show)

  instance Interpolate TestFailure where
    interpolate TestFailure{..} = s"${message} (at ${file}:${lineNumber})"

  let failure = TestFailure "Bad value" "test.hs" 10

  -- "TestFailure { message = \\\"Bad value\\\" file = \\\"test.hs\\\", lineNumber = 10 }"
  let s1 = show failure

  -- "Bad value (at test.hs:10)"
  let s2 = interpolate failure

  -- "Got a failure: Bad value (at test.hs:10)"
  let s3 = s"Got a failure: ${failure}"

Interpolating expressions including braces:
::

  -- "A record: <Person Bob>"
  let s = s"A record: ${Person{name = "Bob"}}"

  -- parse error: `['` isn't a valid Haskell expression
  let s2 = s"A brace: ${['}']}"

Effect and Interactions
-----------------------

No interactions with existing language features.

Costs and Drawbacks
-------------------

Development should be low-effort, maintenance should be low-effort. Learnability for novice users will go up, since novice users probably expect string interpolation to be available, and might be frustrated at the lack of support currently.

One drawback is that whitespace is now important with this syntax, with ``s"foo"`` semantically different from ``s "foo"``. However, there's precedent for this (Template Haskell splices make ``$(...)`` different from ``$ (...)``), and it's also unlikely for someone to use a one-letter function name, especially naming that function ``s``.

Other common criticisms to including string interpolation in general:

* Why should ``String`` be special?

  * It's a common enough use-case that I think warrants nice-to-haves like this proposal. e.g. we already have ``""`` syntax sugar instead of using list of ``Char`` everywhere for ease of use, why not include interpolation for ease of use?

  * It could be made not-special, at the cost of more complex machinery. See "Alternatives"

* String interpolation makes things like SQL injection easier

  * I would argue that someone unaware of injection attacks (and libraries designed to prevent these attacks) would be just as likely to manually concatenate a string as interpolate

Alternatives
------------

* Status quo (discussed in the "Motivation" section)

* Generalize to any type class outputting ``String`` (not just ``Interpolate``)

  * Change the syntax to enable specifying any module defining an ``Interpolate``-compatible interface, similar to ``QualifiedDo`` e.g.
    ::

      module MyInterpolate where

      class MyInterpolate a where
        interpolatePrec :: Int -> a -> ShowS

      module Main where

      -- desugars to `("Example " ++) . interpolatePrec 0 foo $ ""`
      original :: String
      original = [s|Example ${foo}|]

      -- desugars to `("Example " ++) . MyInterpolate.interpolatePrec 0 foo $ ""`
      custom :: String
      custom = [s.MyInterpolate|Example ${foo}|]

* Generalize to any output type

  * Break out a type class representing "Type that an interpolation can result in" and enable specifying any module defining a compatible interface, similar to ``QualifiedDo`` (would also enable swapping out the ``Interpolate`` type class, like the previous bulletpoint). e.g.
    ::

      {----- built-in to GHC -----}

      class Interpolate s where
        interpolateRaw :: String -> s -> s
      class Interpolate s => InterpolateValue s a where
        interpolatePrec :: Int -> a -> s -> s

      instance Interpolate String where
        interpolateRaw = (++)

      -- the usual instances
      instance InterpolateValue String String
      instance InterpolateValue String Int

      {----- user-defined -----}

      module MySqlQuery where

      data SqlQuery = SqlQuery { queryText :: String, queryVals :: [SqlVal] }
      instance Interpolate SqlQuery where
        interpolateRaw q1 (SqlQuery q2 vs) = SqlQuery (q1 <> q2) vs
      instance InterpolateValue SqlQuery String where
        interpolatePrec _ s (SqlQuery text vs) = SqlQuery text (SqlString s : vs)
      instance InterpolateValue SqlQuery Int where
        interpolatePrec _ x (SqlQuery text vs) = SqlQuery text (SqlInt x : vs)

      module Main where

      -- desugars to `interpolateRaw "SELECT * FROM foo WHERE id = " . interpolatePrec 0 id $ ""`
      pwnable :: Int -> String
      pwnable id = [s|SELECT * FROM foo WHERE id = ${id}|]

      -- desugars to `MySqlQuery.interpolateRaw "SELECT * FROM foo WHERE id = " . MySqlQuery.interpolatePrec 0 id $ ""`
      safe :: Int -> SqlQuery
      safe id = [s.MySqlQuery|SELECT * FROM foo WHERE id = ${id}|]

  * Benefit: one more metaprogramming technique, except this wouldn't require Template Haskell
  * Drawback: requires ``MultiParamTypeClasses``, probably has worse type inference, not sure how to integrate ``OverloadedStrings``
  * Perhaps this should just be left to third-party QuasiQuoters using something like ``ghc-meta``?

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
