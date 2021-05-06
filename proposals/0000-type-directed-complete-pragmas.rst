Type-directed COMPLETE pragmas
==============================

.. author:: Cale Gibbard and Dan Bornside on behalf of Obsidian Systems and MIRI
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

Here you should write a short abstract motivating and briefly summarizing the proposed change.

Motivation
----------
We can presently obtain polymorphic pattern synonyms, for example::

  class AsEmpty a where
    empty :: a
    isEmpty :: a -> Bool

  pattern Empty :: AsEmpty a => a
  pattern Empty <- (isEmpty -> True) where
    Empty = empty

It's also possible, at present, to write a misleading ``COMPLETE`` pragma::

  instance AsEmpty (Proxy a) where
    empty = Proxy
    isEmpty Proxy = True

  instance AsEmpty [a] where
    empty = []
    isEmpty xs = null xs

  {-# COMPLETE Empty :: Proxy #-}

The thing which makes this misleading is that this pragma does not simply apply to ``Proxy``, the "type signature" currently does nothing, and the following pattern match
will not warn::

  foo :: [a] -> Int
  foo Empty = 5

In the past, the "type signature" (actually just the name of a type constructor) specified in a COMPLETE pragma was used to specify which TyCon to attach the COMPLETE set to,
in cases where GHC couldn't figure that out from the ConLikes. Now that the COMPLETE sets are stored separately from the TyCon, this information is no longer required, but we also lose any ability to refine which type our COMPLETE set was meant to apply to.

So, it would be nice if rather than ignoring the thing following the double colon, we instead could specify the type of scrutinee that the COMPLETE pragma was meant to apply to,
and in cases where the type of a scrutinee doesn't unify with whatever type is provided, the COMPLETE set would be ignored.

There is an outstanding related GHC issue `#14422<https://gitlab.haskell.org/ghc/ghc/-/issues/14422>`_.

Proposed Change Specification
-----------------------------

Rather than simply the name of a TyCon following the :: we will have GHC accept a type (possibly involving type variables). Any scrutinee whose type does not unify with the type
of the COMPLETE pragma will exclude that COMPLETE pragma from consideration in checking whether the match is complete.

Examples
--------

To start, the example above would have to be modified::

  {-# COMPLETE Empty :: Proxy a #-}

The type variable in the specified type would unify with the argument to Proxy in the following::

  bar :: Proxy t -> Int
  bar Empty = 5

and so the COMPLETE pragma would be used to avoid reporting a warning in this case.

Moreover, in the original example::

  foo :: [a] -> Int
  foo Empty = 5

The type of the pattern doesn't unify with the type of the COMPLETE pragma, and so we should get a warning.

We could extend this example with an additional polymorphic constructor::

  class AsCons a as | as -> a where
    cons :: a -> as -> as
    asCons :: as -> Maybe (a,as)

  instance AsCons a [a] where
    cons = (:)
    asCons (x:xs) = Just (x,xs)
    asCons _ = Nothing

  pattern Cons :: AsCons a as => a -> as -> as
  pattern Cons x xs <- (asCons -> Just (x,xs)) where
    Cons x xs = cons x xs

  {-# COMPLETE Empty, Cons :: [a] #-}

and this would inform GHC that a pattern match such as::

  quux :: [a] -> Int
  quux Empty = 0
  quux (Cons x xs) = 1

is complete, and not have any effect on the Proxy case at all.

Effect and Interactions
-----------------------
There may be some interesting considerations needed surrounding higher-rank types, since we'd end up trying to match polytypes with one another. This might be easy though,
we'll have to see whether it presents any concern during implementation. If all else fails, we could just forbid specifying polytypes, and/or not have COMPLETE pragmas that
specify their scrutinee's type to apply to polytyped scrutinees.

Costs and Drawbacks
-------------------
There might be some worry that the syntax we're proposing here overlaps with the old syntax which is still parsed by GHC but does nothing at present, and the double colon is
still being used in a somewhat quirky way here, as we're not providing the type of the ConLikes being listed, but rather the type of the scrutinee under consideration.

Alternatives
------------
One alternative would be to simply allow only specifying the name of a type constructor, as before, and then decompose the type of the scrutinee to see if it is constructed
by that TyCon in order to check that the COMPLETE pragma should apply. This would be more backwards compatible, but also somewhat less flexible.

We also may wish to alter the syntax in some surface-level way to prevent overlap with the old deprecated one.

Unresolved Questions
--------------------
Should any of the alternatives above be taken?

Implementation Plan
-------------------
Obsidian Systems will implement whatever is needed here on behalf of our client MIRI.

Endorsements
------------
