Type-level type applications
============================

.. author:: Richard Eisenberg
.. date-accepted:: 2018-03-01
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/12045
.. implemented:: 8.8
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/80>`_.
.. contents::

.. _`#12045`: https://gitlab.haskell.org/ghc/ghc/issues/12045

Allow the use of type applications at the type level. For example,
we could write::

  'Just @Nat

instead of::

  'Just :: Nat -> Maybe Nat


Motivation
------------
There are two major motivations:

1. To allow users to get the power and convenience of explicit type
   applications at the type level as well as the term level.

2. To allow ``-fprint-explicit-kinds`` and the ``Show`` instance for
   ``TypeRep`` to produce more readable output. Currently,
   ``show (typeRep @('Just 3))`` produces ``"'Just Nat 3"``, making
   no distinction between levels. With the proposed change, we could
   quite legitimately produce ``"'Just @Nat 3"``, which seems much clearer.


Proposed Change Specification
-----------------------------
Allow visible type application in types as well as terms. In precisely
the same way that it currently is used to reduce ``forall``\s in terms,
it will reduce ``forall``\s in types. This new behavior will be controlled
by the ``-XTypeApplications`` extension.

Specifically:

1. Add new parsing rules to types allowing ``@`` to appear before a type argument.

2. Currently, GHC tracks three *visibilities*: *required*, *specified*, and *inferred*.
   A required argument must be applied at every call site. These are normal arguments,
   like the ``a`` argument to ``Maybe a``. Specified arguments are arguments that are
   normally omitted but can be supplied explicitly with the use of ``@``. For example,
   the ``k`` in ``data Proxy :: forall k. k -> Type`` is specified. Inferred arguments
   are not available for explicit application, like the kind of ``a`` in ``data Proxy2 a``.

   With visible type application in types, users could provide explicit instantiations
   of specified arguments.

   In an extension to existing rules, a variable must be mentioned somewhere in the Haskell
   source to be specified. A variable that is never written in the source is inferred.

3. Promoted data constructor arguments have the same visibilities as the unpromoted data
   constructor.

4. GHC's current behavior of implicitly quantifying over type variables used in type
   signatures is unaffected. If a type variable is mentioned only in a visible type
   application, it is still implicitly quantified.

Effect and Interactions
-----------------------

This new feature would work in pattern signatures (that is, a type signature ascribing a type to a term-level pattern) and would have the capability of binding a scoped type variable. Viz::

  data Ex where
    MkEx :: forall k (a :: k). Proxy (a :: k) -> Ex

  foo (MkEx (p :: Proxy @k a)) = ... k is in scope here (along with a) ...

Costs and Drawbacks
-------------------
As a user, I was
quite surprised to find that this didn't work already, so I don't think
the learning cost will be high.

The development costs should be relatively low. Instantiation in types is
already lazy, and so type application in types will be much easier to implement
than type application in terms was.

Alternatives
------------
I am not aware of any existing alternatives.


Unresolved questions
--------------------
Should we change the behavior of ``:kind`` to match that of ``:type``? Currently, the latter
does instantiation while the former does not. This means that there is no screaming need
to introduce a ``:kind +v``, because ``:kind`` is already analogous to ``:type +v``. Perhaps
this is confusing though.


Implementation Plan
-------------------
Richard Eisenberg (@goldfirere) is happy to advise someone who wants to take this on. Or he
will implement himself someday.
