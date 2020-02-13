Remove an undocumented `StableName` guarantee
=============================================

.. author:: David Feuer
.. date-accepted:: 2018-09-30
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/163>`_.
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
guarantee: if ``sn1`` and ``sn2`` are stable names, ``sn1`` was alive
when ``sn2`` was created by ``makeStableName``, and
``hashStableName sn1 = hashStableName sn2``, then ``sn1 = sn2``.

Proposed Change Specification
-----------------------------

Reimplement ``StableName``\s in a simpler, more efficient way that
does not make the described de facto guarantee. Collisions will remain
quite rare (and will almost never happen on 64-bit architectures),
but there will no longer be any way to guarantee that they absolutely
will not occur.

Effect and Interactions
-----------------------

I don't foresee any significant interactions.

Costs and Drawbacks
-------------------

* Any code relying on the de facto guarantee will break, probably
  silently, and probably intermittently.

* Someone will actually have to write the simplified implementation.
  If I understand what's going on correctly, this shouldn't take more
  than a day or so for someone who's familiar with the GHC garbage
  collector, or somewhat longer for someone who's not.

Alternatives
------------

The obvious alternative is to *document* the de facto guarantee. This would
allow some (very carefully written) code to be simpler and/or more efficient.
For example, we could implement a map from stable names to values like so: ::

 touchStableName :: StableName a -> IO ()
 touchStableName (StableName sn) =
   IO $ \s -> (# touch# sn s, () #)

 newtype SNMap k v = SNMap (IntMap (StableName k, v))

 empty :: SNMap k v
 empty = SNMap (IM.empty)

 insert :: k -> v -> SNMap k v -> IO (SNMap k v)
 insert k v (SNMap im) = do
   snk <- makeStableName k
   pure $! SNMap (IM.insert (hashStableName snk) (snk, v) im)

 lookup :: k -> SNMap k v -> IO (Maybe v)
 lookup k (SNMap im) = do
   snk <- makeStableName k
   case lookup (hashStableName snk) im of
     Nothing -> pure Nothing
     Just (sn, v) -> do
       touchStableName sn
       pure (Just v)

We don't need to worry about hash collisions on lookup because
we ensure (using ``touchStableName``) that ``sn`` was alive when ``snk``
was created, and therefore the equality of their hashes implies
their equality.

There is a clear trade-off here between complexity of code using
stable names and complexity of the code implementing them. There's
also a balance in where we pay performance prices. At the moment,
we're getting the worst of both worlds, paying the price to implement
conditional injectivity but not letting users reap any benefits.
I think we should definitely do one or the other.

Unresolved Questions
--------------------

Is anyone currently relying on the de facto guarantee?

Implementation Plan
-------------------
I'd be happy to work on it myself, but I'd need some help from the
GHC garbage collection experts.
