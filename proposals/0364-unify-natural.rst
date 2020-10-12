Unify ``Nat`` and ``Natural``
=============================

.. author:: Richard Eisenberg, inspired by Rinat Striungis
.. date-accepted:: 2020-10-12
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/364>`_.
.. contents::

GHC currently uses ``GHC.TypeLits.Nat`` to describe compile-time natural numbers
but ``Numeric.Natural.Natural`` to describe runtime ones. This proposal unifies
the two by making ``GHC.TypeLits.Nat`` a synonym for ``Numeric.Natural.Natural``.

Motivation
----------

GHC should not have two different types to represent natural numbers. One is
enough. The existence of the separate kind ``Nat`` and the impossibility of
using ``Natural`` at compile time (and data constructors with ``Natural`` fields) is
an unnecessary complication. It forces us to create
redundant types with ``Nat`` fields in their constructors (making these types
uninhabited at runtime) only for use at compile time. This is more cumbersome
than the promotion of ordinary data types.

Consider this data type with fields of type ``Natural``::

  data Point = MkPoint Natural Natural

``Point`` is inhabited by terms, but not by types::

  p = MkPoint 3 5       -- ok
  type P = MkPoint 3 5  -- not ok

Alternatively, we could declare it with fields of type ``Nat``::

  data Point = MkPoint Nat Nat

Then it would have the opposite issue::

  p = MkPoint 3 5        -- not ok
  type P = MkPoint 3 5   -- ok

To avoid declaring two incompatible data types, we could add a parameter to ``Point``::

  data Point n = MkPoint n n
  type PointT = Point Natural  -- inhabited by terms
  type PointK = Point Nat      -- inhabited by types

However, this is a roundabout way to go about it, and in more involved
scenarios requires additional machinery to support it (such as the ``Demote``
type family in the ``singletons`` package). By unifying ``Nat`` and
``Natural``, we avoid this issue entirely.

Proposed Change Specification
-----------------------------
* Add ``type Nat = Natural`` in ``GHC.TypeNats``.

* Re-export ``Natural`` from ``GHC.TypeNats`` and
  ``GHC.TypeLits`` (along with the current re-export
  of ``Nat``).

* Assign numbers in types to have kind ``Natural`` instead of kind ``Nat``.

Effect and Interactions
-----------------------
* Instances written about ``Nat`` may now need ``TypeSynonymInstances``, which
  is implied by ``FlexibleInstances``. Or, they could be written to use ``Natural``
  instead of ``Nat``.

* It is possible for someone to have separate instances today for ``Nat`` and
  ``Natural``, though it is unclear how an instance on ``Nat`` would be useful.
  Having two separate instances for these types will not be possible after this
  proposal is implemented.

Otherwise, this change should be backward compatible. In particular, this proposal
does *not* change parsing or other aspects of numbers written in types. For example,
even though ``(-5 :: Natural)`` parses today (and throws a runtime error), that
expression will *not* parse in types.


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

