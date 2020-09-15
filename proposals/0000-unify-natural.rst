Unify ``Nat`` and ``Natural``
=============================

.. author:: Richard Eisenberg, inspired by Rinat Striungis
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

GHC currently uses ``GHC.TypeLits.Nat`` to describe compile-time natural numbers
but ``Numeric.Natural.Natural`` to describe runtime ones. This proposal unifies
the two by making ``GHC.TypeLits.Nat`` a synonym for ``Numeric.Natural.Natural``.

Motivation
----------
GHC should not have two different types to represent natural numbers. One is
enough.

Proposed Change Specification
-----------------------------
* Add ``type Nat = Natural`` in ``GHC.TypeNats``.

* Re-export ``Natural`` from ``GHC.TypeLits`` (along with the current re-export
  of ``Nat``)

* Assign numbers in types to have kind ``Natural`` instead of kind ``Nat``.

Effect and Interactions
-----------------------
* Instances written about ``Nat`` may now need ``TypeSynonymInstances``, which
  is implied by ``FlexibleInstances``. Or, they could be written to use ``Natural``
  instead of ``Nat``.

Otherwise, this change should be backward compatible.


Costs and Drawbacks
-------------------
* We might be painting ourselves into a corner, having not yet worked out the
  way we will support other types of compile-time literals. But I (Richard)
  think this will be OK.


Alternatives
------------
* Do nothing.


Unresolved Questions
--------------------
* None at this time.

Implementation Plan
-------------------
* This is already `implemented <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3583>`_,
  by Rinat Striungis.

