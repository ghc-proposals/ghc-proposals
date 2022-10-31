Make Symbol a newtype over String
=================================

.. author:: Oleg Grenrus
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/546>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

I propose changing ``data Symbol`` definition
to be

::

    newtype Symbol = MkSymbol String

and exporting this fact from ``GHC.TypeLits``.


Motivation
----------

The motivation is similar to accepted and implemented
`Unify Nat and Natural proposal <https://github.com/ghc-proposals/ghc-proposals/pull/364>`_.

Consider the data type with fields of type ``String``::

  data Fields = Fields [String]

``Fields`` is inhabitaed by terms, and since recently (`The Char Kind <https://github.com/ghc-proposals/ghc-proposals/pull/387>`_) also by 
types. However type literals produce ``Symbol``, not ``String`` ::

  fs = Fields ["foo", "bar"]           -- ok
  type FS = Fields '[ "foo", "bar" ]   -- not ok

Alternatively, we could declare the type with fields of type ``Symbol``::

  data Fields = Fields [Symbol]

Then it would have the opposite issue::

  fs = Fields ["foo", "bar" ]         -- not ok
  type FS = Fields '[ "foo", "bar" ]  -- ok

As with ``Nat`` and ``Natural`` proposal, it's possible to go roundabout
this (and e.g. ``singletons-base`` does). However, if ``Symbol`` were
an ordinary ``newtype`` over ``String``, we would avoid this issue entirely.

(Note: ``singletons-base`` demotes ``Symbol`` to ``Text`` which is
incorrect, as ``Text`` cannot represent all ``String`` values)

Proposed Change Specification
-----------------------------

* Change ``data Symbol`` to ``newtype Symbol = MkSymbol String``

* Re-export full ``Symbol`` definition from ``GHC.TypeLits``.

We don't propose any further changes in ``GHC.TypeLits`` interface.
For example ``symbolVal`` will continue to return ``String`` value.
A possible future ``GHC.Symbol`` interface mirroring
``GHC.TypeLits`` functionlity, but using ``Symbol`` also for the term level
can be made as CLC proposal, as that would be only a library change.

Technically this is a library only change, but maybe it's only
a happy coincidence that GHC internals don't need to be changed,
when ``Symbol`` definition is changed.

Effect and Interactions
-----------------------

This change is completely backwards-compatible.

Costs and Drawbacks
-------------------

This proposal introduces "yet another string" type into Haskell ecosystem.
Yet, it serves as specific purpose.

Alternatives
------------

An alternative is to make ``String`` promote to ``Symbol``.
That won't work out well, because lists of characters (i.e. ``String``)
can be promoted already.

The opposite alternative is to make ``type Symbol = String``.
It was also meentioned as an alternative in The Char Kind proposal.
It was rejected then, and the reasoning is still valid:
First of all, we keep ``Symbol`` for type-checking efficiency.
Moreover, we would also handle type families inside cons cells when solving
``HasField`` constraints. For example, ``HasField T ('x' : F y : G z) ty``.

It's possible to use some other type for ``Symbol`` "backend" than ``String``,
however ``Text`` doesn't qualify as ``String`` (and type-level ``Symbol``)
can contain values ``Text`` cannot represent. For example single surrogate codepoints.
``SNat`` and ``KnownSymbol`` are backed by ``String``, so it is a natural
choice.

Unresolved Questions
--------------------

None.

Implementation Plan
-------------------

The patch is very small. I'll write it.

Endorsements
-------------

None atm.
