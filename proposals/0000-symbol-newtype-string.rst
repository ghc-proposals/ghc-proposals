Make Symbol a newtype over String
=================================

.. author:: Oleg Grenrus (with amendments by Chris Dornan and Adam Gundry)
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/562>`_.
.. sectnum::
.. contents::

I propose changing ``data Symbol`` definition
to be

::

    newtype Symbol = MkSymbol String

Motivation
----------

The motivation is similar to accepted and implemented
`Unify Nat and Natural proposal <https://github.com/ghc-proposals/ghc-proposals/pull/364>`_.

Consider the data type with fields of type ``String``::

  data Fields = Fields [String]

``Fields`` is inhabited by terms, and since recently (`The Char Kind <https://github.com/ghc-proposals/ghc-proposals/pull/387>`_) also by 
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

(Note: An API for working with Symbol at the term level is out of scope for this proposal,
but may be the subject of a subsequent CLC proposal.)

Proposed Change Specification
-----------------------------

* Change ``data Symbol`` to ``newtype Symbol = MkSymbol String``

The definition being changed lives in ``GHC.Types``, and thus accessing the newtype constructor
requires importing ``ghc-prim``.

We don't propose any changes to ``GHC.TypeLits`` interface.
Firstly the implementation of ``Symbol`` stays internal.
Also ``symbolVal`` will continue to return ``String`` value.
In future the ``GHC.Symbol`` interface mirroring ``GHC.TypeLits`` functionality
could be created , but using ``Symbol`` also for the term level can be made as
CLC proposal, as that would be only a library change.

Technically this is a library only change, but maybe it's only
a happy coincidence that GHC internals don't need to be changed,
when ``Symbol`` definition is changed.

Effect and Interactions
-----------------------

This change is generally backwards-compatible, but:
 * code might rely on ` ``Symbol`` being an empty datatype, e.g. via an empty case analysis on a ``Symbol`` value; or
 * code might import ``GHC.Types`` and introduce a name collision on ``MkSymbol``.

Costs and Drawbacks
-------------------

This proposal introduces "yet another string" type into Haskell ecosystem.
Yet, it serves as specific purpose.

Alternatives
------------

An alternative is to export ``Symbol`` definition from ``GHC.TypeLits``.
This however raises questions like whether ``MkSymbol '[] ~ ""``.
For now we pretend that ``MkSymbol`` should not be promoted,
until that can be resticted.

It is also possible to prevent ``MkSymbol`` usage on the type level by
defining ``Symbol`` like::

  data Symbol = MkSymbol (# String #)

As unlifted types cannot be promoted, it won't be possible to successfully
use ``MkSymbol`` on the type level. (No more success than with ``NS`` and
``NB`` constructors of type ``Natural``).A

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

The patch is very small.

Endorsements
-------------

None atm.
