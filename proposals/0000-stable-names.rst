Remove an undocumented `StableName` guarantee
=============================================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

The ``hashStableName`` function in ``System.Mem.Weak`` is explicitly
documented as not being injective. However, under certain circumstances
users actually *can* rely on it being injective. I propose to remove
this de facto guarantee.

Motivation
------------

At present, stable names are implemented using two tables: a hash table and a
"stable name table". The stable name table uses three words per stable name,
and we need a non-trivial amount of code to keep the hash table, the stable
name table, and the actual ``StableName#`` objects synced up, particularly
during garbage collection. I realized yesterday that we should actually be able
to implement all the *documented* guarantees of ``StableName``\s without using
a stable name table at all. We can, I believe, get away with just one hash
table per GC generation and a radically simpler garbage collection strategy.

What the stable name table really lets us do is make an *undocumented*
guarantee: if ``sn1`` and ``sn2`` are stable names, that are currently
*live*, and ``hashStableName sn1 = hashStableName sn2``, then
``sn1 = sn2``. As a consequence, we could implement a map from stable
names to values like so: ::

 newtype SNMap k v = SNMap (IntMap (StableName k, v))

where the ``IntMap`` keys are ``hashStableName`` values for their
entries. Since the map keeps the stable names alive, there's no need
to worry about collisions.

Proposed Change Specification
-----------------------------

Reimplement ``StableName``\s in a simpler, more efficient way that
does not make the described de facto guarantee.

Effect and Interactions
-----------------------

I don't foresee any significant interactions.

Costs and Drawbacks
-------------------

Any code relying on the de facto guarantee will break, probably
silently, and probably intermittently.

Someone will actually have to write the simplified implementation.
If I understand what's going on correctly, this shouldn't take more
than a day or so for someone who's familiar with the GHC garbage
collector, or somewhat longer for someone who's not.

Alternatives
------------

The obvious alternative is to *document* the de facto guarantee.

Unresolved Questions
--------------------

Is anyone currently relying on the de facto guarantee?

How does the performance of code relying on the de facto guarantee compare to
its performance once it's modified not to rely on the de facto guarantee and
the ``StableName`` implementation has been streamlined?

Implementation Plan
-------------------
I'd be happy to work on it myself, but I'd need some help from the
GC masters.
