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

Instead, the proposal is to slightly delay desugaring overloaded literals until
after they have been type applied. As an example, under the proposal,
desugaring of type-applied overloaded literals will be done according to the
following examples:

::

  1 @a          desugars to   fromInteger @a        (1 :: Integer)
  2 @a @b       desugars to   fromInteger @a @b     (2 :: Integer)
  3 @a ... @z   desugars to   fromInteger @a ... @z (3 :: Integer)
  (((4))) @a    desugars to   fromInteger @a        (4 :: Integer)
  (5 @a) @b     desugars to   fromInteger @a @b     (5 :: Integer)

To be pedantic, every expression of the form ``LIT_APP`` in the following
grammar will be desugared:

::

  LIT_APP := LIT
           | '(' LIT_APP ')'
           | LIT_APP TY_APP

such that the result is equivalent to ``fromLiteral (TY_APP...) LIT`` for an
appropriate ``fromLiteral`` corresponding to ``LIT``. The ``TY_APP``s will be
applied in the same order after desugaring that they were before.

This change in desugaring will apply to all overloaded literals (``Num``,
``Fractional``, ``IsString``, ``IsList``, ``IsLabel``).


Effect and Interactions
-----------------------
The proposed change means type now "do the right thing" by default when applied
to overloaded literals.

::

  Prelude> :t 5
  5 :: Num t => t


  Prelude> :t 5 @Double
  5 @Double :: Double


There is an unfortunate interaction here for code in the wild that is already
using both ``RebindableSyntax`` and ``TypeApplications`` simultaneously, which
is type-applying its overloaded literals. In such a case, there will be
a **change in behavior.**

Fortunately the situation is less dire than it seems; `this combination of
extensions is exceedingly rare in public code
<https://github.com/search?l=Haskell&q=TypeApplications+RebindableSyntax&type=Code>`_.
At time of writing, there are 127 Haskell files on Github that mention both
``RebindableSyntax`` and ``TypeApplications``. Approximately half of which are
forks of GHC, 13 are obviously unrelated, and a cursory glance at the others
did not obviously depend on type applying overloaded literals.


Costs and Drawbacks
-------------------
The primary drawback of this change is the user-visible change in existing code
described in `Effect and Interactions <#effect-and-interactions>`_.

The development cost of this proposal is minimal; I have a working
implementation for the ``Num``, ``Fractional`` and ``String`` cases already,
which is roughly 50 SLOC. Adding lists and labels to this is unlikely to be
significantly more costly. The maintenance burden is likely to be
correspondingly small.


Alternatives
------------

**One alternative** as suggested by SPJ on `the trac issue
<https://ghc.haskell.org/trac/ghc/ticket/11409#comment:3>`_ is to define the
following helper function:

::

  integerLit :: Integer -> forall a. Num a => a
  integerLit n = fromInteger n

and then perform desugaring in terms of ``integerLit`` rather than
``fromInteger``. However, it's not clear how such an approach would generalize
to the ``RebindableSyntax`` case.


**Another alternative**  is `a previous draft
<https://github.com/isovector/ghc-proposals/blob/a57f500cab6a7d3a71aaebfaf51b3ed5e757c966/proposals/0000-type-apply-literals.rst>`_
of this proposal which suggested differentiating between `5 @Int` and `(5)
@Int`. Feedback from the community suggested this to be more complicated than
it was worth.


**A third alternative**  is to completely bypass the issue, and write `id @Int
5` intead of `5 @Int`. This works today, but is clearly the lowest-cost
workaround to the motivating problem of this proposal.


Unresolved questions
--------------------
**Question:** Should this new behavior be hidden behind an opt-in flag so as to avoid
potential interference with existing users of both ``RebindableSyntax`` and
``TypeApplications`` who are already type applying their overloaded literals?

**Answer:** `SPJ suggests
<https://github.com/ghc-proposals/ghc-proposals/pull/129#issuecomment-385529471>`_
that this rare enough to not worry about breaking, and I am inclined to agree.


Implementation Plan
-------------------
If accepted, I (isovector) will implement the change. `There is already an
existing implementation of it
<https://github.com/isovector/ghc/tree/typelits2>`_ .

