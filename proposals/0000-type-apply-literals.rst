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

This desugaring's only purpose is to swizzle around the type variable so it can
be applied after the ``Integer``.

There will be corresponding functions for the other varieties of overloaded
literals:

::

  rationalLit :: Rational -> forall a. Fractional a => a
  rationalLit = fromRational

  stringLit :: String -> forall a. IsString a => a
  stringLit = fromString

  listLit :: [i] -> forall a. (IsList a, i ~ Item a) => a
  listLit = fromList

  listNLit :: Int -> [i] -> forall a. (IsList a, i ~ Item a) => a
  listNLit = fromListN

This desugaring rule will only apply when ``NoRebindableSyntax`` is enabled,
because type applications work as expected when syntax is rebound.

Effect and Interactions
-----------------------
The proposed change means type now "do the right thing" by default when applied
to overloaded literals.

::

  Prelude> :t 5 @Double
  5 @Double :: Double


Costs and Drawbacks
-------------------
As best I can tell, there are no drawbacks to this proposal.

The development cost of this proposal is minimal; I have a working
implementation for the ``Num``, ``Rational``, ``IsString`` and ``IsList`` cases
already, which is roughly 50 SLOC. Adding labels to this is unlikely to be
significantly more costly. The maintenance burden is likely to be
correspondingly small.


Alternatives
------------

**One alternative**  is `a previous draft
<https://github.com/isovector/ghc-proposals/blob/a57f500cab6a7d3a71aaebfaf51b3ed5e757c966/proposals/0000-type-apply-literals.rst>`_
of this proposal which suggested special desugaring rules for type applied
directly to overloaded literals, which would get reshuffled to the correct
location on the ``fromInteger`` call. Feedback from the community suggested
this to be more complicated than it was worth.


**A second alternative**  is to completely bypass the issue, and write `id @Int
5` intead of `5 @Int`. This works today, but is clearly the lowest-cost
workaround to the motivating problem of this proposal.


Unresolved questions
--------------------
None.

Implementation Plan
-------------------
If accepted, I (isovector) will implement the change. `There is already an
existing draft implementation of it
<https://github.com/ghc/ghc/compare/master...isovector:typelits3?expand=1>`_ .

