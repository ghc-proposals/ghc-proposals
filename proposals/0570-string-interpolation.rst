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

  interpolateRaw "foo "
  . doInterpolate (f a b)
  . interpolateRaw " bar "
  . doInterpolate (g x)
  . interpolateRaw " baz "
  . doInterpolate name
  $ interpolateEmpty

where:
::

  class Interpolate s where
    interpolateRaw :: String -> s -> s
    default interpolateRaw :: (IsString s, Semigroup s) => String -> s -> s
    interpolateRaw s = (fromString s <>)

    interpolateEmpty :: s
    default interpolateEmpty :: IsString s => s
    interpolateEmpty = fromString ""

  class Interpolate s => InterpolateValueRunner s a where
    doInterpolate :: a -> s -> s

  class Interpolate s => InterpolateValue flag s a where
    {-# MINIMAL interpolate | interpolatePrec #-}

    interpolate :: Proxy flag -> a -> s
    interpolate proxy a = interpolatePrec proxy 0 a interpolateEmpty

    interpolatePrec :: Proxy flag -> Int -> a -> s -> s
    default interpolatePrec :: Semigroup s => Proxy flag -> Int -> a -> s -> s
    interpolatePrec proxy _ a s = interpolate proxy a <> s

  data InterpolateDefault
  data InterpolateOverride

  class InterpolateValueFlag s a flag | s a -> flag
  instance {-# OVERLAPPABLE #-} (flag ~ InterpolateDefault) => InterpolateValueFlag s a flag

  instance
    ( Interpolate s
    , InterpolateValueFlag s a flag
    , InterpolateValue flag s a
    ) => InterpolateValueRunner s a where
    doInterpolate = interpolatePrec (Proxy :: Proxy flag) 0

We will always require interpolation to be of the form ``${foo}``, unlike other languages where ``$foo`` is allowed for single-variable interpolation. This simplifies the syntax and avoids the need for escaping bare ``$``. Interpolation may be escaped by escaping the dollar sign; e.g. ``"\${foo}"`` is equivalent to ``['$', '{', 'f', 'o', 'o', '}']``.

Any Haskell expression may go inside the braces, but an unmatched ``}`` will immediately end processing. This restriction simplifies the implementation and isn't a big deal in practice, since one could always break it out into a helper variable.

Examples
--------

See the `brandonchinn178/string-syntax <https://github.com/brandonchinn178/string-syntax>`_ GitHub repo for a working prototype, including:

* The default instances that would be provided
* Custom instances
* Interpolation for an HTML library that auto-escapes interpolated strings
* Interpolation for a SQL library that prevents SQL injection

Effect and Interactions
-----------------------

No interactions with existing language features outside of the ``OverloadedStrings`` behavior mentioned in the Specification.

With the new `multiline string proposal <https://github.com/ghc-proposals/ghc-proposals/pull/569>`_ being accepted, interpolation syntax will also be enabled for multiline syntax, e.g.
::

  -- "Line 1: 100\nLine 2: 200\n"
  let s =
        s"""
        Line 1: ${x}
        Line 2: ${y}
        """
      x = 100
      y = 200

Costs and Drawbacks
-------------------

Development should be low-effort, maintenance should be low-effort. Learnability for novice users will go up, since novice users probably expect string interpolation to be available, and might be frustrated at the lack of support currently.

The major drawback of this approach is the machinery needed behind the scenes. To support instances like:
::

  -- implementation of Bool for any s"..." context
  instance InterpolateValue s String => InterpolateValue s Bool where
    interpolate = interpolate . show

  -- implementation of any ToSqlValue for the SqlQuery s"..." context
  instance ToSqlValue a => InterpolateValue SqlQuery a where
    interpolate b = SqlQuery "?" [SqlBool b]

we need to use the auxiliary overlapping instances technique described in https://wiki.haskell.org/GHC/AdvancedOverlap (See `this comment <https://github.com/brandonchinn178/string-syntax/pull/2#discussion_r1082171539>`_ for more information). This is invisible for basic usage, but it's painfully visible in two instances:

1. Library authors who want to write a new interpolation type (e.g. ``SqlQuery``)
2. Anyone providing a type they want to interpolate (e.g. ``MyUser``)

One minor drawback is that whitespace is now important with this syntax, with ``s"foo"`` semantically different from ``s "foo"``. However, there's precedent for this (Template Haskell splices make ``$(...)`` different from ``$ (...)``), and it's also unlikely for someone to use a one-letter function name, especially naming that function ``s``.

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
