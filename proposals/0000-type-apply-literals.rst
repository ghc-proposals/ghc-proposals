TypeApplications for Overloaded Literals
========================================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/129>`_.
.. sectnum::
.. contents::

Currently, types are unable to be applied to overloaded literals via
``TypeApplications``. This proposal slightly changes the behavior of applied
types in the presence of overloaded literals allowing us to do so.


Motivation
------------
``TypeApplications`` is a fantastic quality-of-life extension that helps to
avoid writing expression-level type signatures in a variety of cases. However,
its usefulness is severely reduced when dealing with overloaded literals, where
type applications fail to typecheck. As a result, users wishing to guide the
type-checker for their overloaded literal must fall back upon an explicit type
signature.

Attempting to simultaneously deal with multiple types of strings with
``OverloadedStrings`` enabled is a particularly painful experience.

This existing behavior is due to an implementation detail in the standard that
says ``5`` be desugared as ``fromInteger (5 :: Integer)``. However, this
transformation is unfaithful in the presense of type applications as it subtly
lies about the types of overloaded literals:

::

  Prelude> :t 5
  5 :: Num t => t


  Prelude> :t 5 @Double

  <interactive>:1:1: error:
      • Cannot apply expression of type ‘t0’
        to a visible type argument ‘Double’
      • In the expression: 5 @Double

The trifecta of improving quality of life, hiding a leaky implementation
detail, and not being very invasive implementation-wise suggests that this is
a change worth making.


Proposed Change Specification
-----------------------------

Because it is a class member, ``fromInteger`` (and its friends, analogously)
has the type ``Num a => Integer -> a``. Due to the current desugaring of eg.
``5`` as ``fromIntegral (5 :: Integer)``, any type applications on ``5`` come
"too late" to fill in the ``a`` type.

Instead, the proposal is to desugar ``5`` into ``integerLit (5 :: Integer)``
where ``integerLit`` is defined as:

::

  integerLit :: Integer -> forall a. Num a => a
  integerLit = fromInteger
  {-# INLINE integerLit #-}

This desugaring's only purpose is to swizzle around the type variable so it can
be applied after the ``Integer``.

There will be corresponding functions for the other varieties of overloaded
literals:

::

  rationalLit :: Rational -> forall a. Fractional a => a
  rationalLit = fromRational
  {-# INLINE rationalLit #-}

  labelLit :: forall (x :: Symbol) a. IsLabel x a => a
  labelLit = fromlabel @x @a
  {-# INLINE labelLit #-}

  stringLit :: String -> forall a. IsString a => a
  stringLit = fromString
  {-# INLINE stringLit #-}

  listLit :: [i] -> [i]
  listLit a = a
  {-# INLINE listLit #-}

  overListLit :: [i] -> forall a. (IsList a, i ~ Item a) => a
  overListLit = fromList
  {-# INLINE overListLit #-}

  overListNLit :: Int -> [i] -> forall a. (IsList a, i ~ Item a) => a
  overListNLit = fromListN
  {-# INLINE overListNLit #-}


The rules for desugaring work as follows:

**Integers:** Expressions of the form ``1`` will be desugared into:

1. ``integerLit (0 :: Integer)`` if ``fromInteger = Prelude.fromInteger``
2. ``fromInteger (1 :: Integer)`` otherwise

where ``fromInteger = Prelude.fromInteger`` means either ``NoRebindableSyntax``
OR ``RebindableSyntax`` and the ``fromInteger`` in scope is equal to
``Prelude.fromInteger``.

**Rationals**: Completely analogous to the integer case.

**Labels**: Completely analogous to the integer case.

**Strings**: Expressions of the form ``"hello"`` will be desugared into:

1. ``stringLit ("hello" :: String)`` if ``OverloadedStrings`` and ``fromString
   = GHC.Exts.fromString``
2. ``fromString ("hello" :: String)`` if ``OverloadedStrings`` and ``fromString
   /= GHC.Exts.fromString``
3. ``"hello"`` (no desugaring) otherwise

**Lists**: Expressions of the form ``[a, b]`` will be desugared into:

1. ``listLit [a, b]`` if ``NoOverloadedLists``
2. ``overListNLit (2 :: Int) [a, b]`` if ``OverloadedLists`` and ``fromListN
   = GHC.Exts.fromListN``
3. ``fromListN (2 :: Int) [a, b]`` otherwise

Expressions of the form ``[a..b]`` will be desugared into:

1. ``listLit [a..b]`` if ``NoOverloadedLists``
2. ``overListLit [a..b]`` if ``OverloadedLists`` and ``fromList
   = GHC.Exts.fromList``
3. ``fromList [a..b]`` otherwise


Effect and Interactions
-----------------------
The proposed change means type now "do the right thing" by default when applied
to overloaded literals.

::

  Prelude> :t 5 @Double
  5 @Double :: Double


  Prelude> :t [5]
  [5] :: Num a => [a]

  Prelude> :t [5] @Int
  [5] @Int :: [Int]


  Prelude> :set -XOverloadedLists
  Prelude> :t [5]
  [5] :: (Num (GHC.Exts.Item l), GHC.Exts.IsList l) => l

  Prelude> :t [5] @[Int]
  [5] @[Int] :: [Int]


This last example is to point out a possible "gotcha", that the type to apply
to a list is different depending on whether or not ``OverloadedLists`` is
enabled.  However, such a difference is correctly described by the types.


Costs and Drawbacks
-------------------
As best I can tell, there are no drawbacks to this proposal. The new desugaring
logic is invisible to users, and its implementation can draw heavily upon the
existing logic for desugaring in terms of ``RebindableSyntax``.

The development cost of this proposal is unlikely to be significant. I have
a mostly-working implementation of it already which is roughly 50 SLOC. The
maintenance burden is likely to be correspondingly small.


Alternatives
------------

**One alternative**  is `a previous draft
<https://github.com/isovector/ghc-proposals/blob/a57f500cab6a7d3a71aaebfaf51b3ed5e757c966/proposals/0000-type-apply-literals.rst>`_
of this proposal which suggested special desugaring rules for type applied
directly to overloaded literals, which would get reshuffled to the correct
location on the ``fromInteger`` call. Feedback from the community suggested
this to be more complicated than it was worth. Furthermore this approach
doesn't provide any immediate solutions for how to type-apply lists.


**A second alternative**  is to do nothing, and write `id @Int 5` intead of `5
@Int`. This works today, but is clearly the lowest-cost workaround to the
motivating problem of this proposal.


**Another alternative** is to wait until #99, which allows giving
``fromInteger`` and ``fromRational`` the correct types without desugaring,
although doesn't seem to directly permit us to do so for other
overloaded literals.


Unresolved questions
--------------------
None.

Implementation Plan
-------------------
If accepted, I (isovector) will implement the change. `There is already an
existing draft implementation of it
<https://github.com/ghc/ghc/compare/master...isovector:typelits3?expand=1>`_ .

