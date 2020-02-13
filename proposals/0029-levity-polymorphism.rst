Revise Levity Polymorphism
==========================

.. author:: Richard Eisenberg
.. date-accepted:: 2017-02-04
.. ticket-url:: https://phabricator.haskell.org/D2842
.. implemented:: 8.2.1
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/29>`_.
.. contents::

GHC 8 introduced *levity polymorphism*, where a variable can be polymorphic in
its representation. (This is also known as "representation polymorphism", but
levity polymorphism surely has a better ring to it. This proposal also
solidifies this name for the feature.) However, levity polymorphism still has
some rough edges; this proposal smoothes them out by parameterizing
``UnboxedTupleRep`` and ``UnboxedSumRep``. This proposal also renames some
definitions to make them shorter.

Motivation
----------

This is useful in the types of ``undefined`` and ``error`` as well as in
simplifying type inference a good deal, with the abolishment of sub-kinding.

As @Iceland_jack has pointed out, levity polymorphism also allows us to
overload operations like ``+`` over unlifted types. (See `this bug report`_.)

.. _this bug report: https://gitlab.haskell.org/ghc/ghc/issues/12708

Further motivation is available in Simon and my `paper on the subject`_.

.. _paper on the subject: http://cs.brynmawr.edu/~rae/papers/2017/levity/levity.pdf

Motivating this change is the fact that unboxed tuples and sums don't really
work. For example, GHC accepts this nonsense::

    type family F a where
      F Int  = (# Int, Bool #)
      F Bool = (# Int# #)

It's nonsense because the two return types actually have different
representations! With the changes proposed here, such a type family would be
safe.

Proposed Change
---------------

Here is the current state of affairs::

    TYPE :: RuntimeRep -> Type   -- super-magical constant
    data RuntimeRep = PtrRepLifted
                    | PtrRepUnlifted
                    | VoidRep
                    | IntRep
                    | FloatRep
                    | ...
                    | UnboxedTupleRep
                    | UnboxedSumRep
    type Type = TYPE PtrRepLifted

With these definitions, all "normal" types have type ``TYPE PtrRepLifted``
(that is, ``Type``). Unboxed types are kinded similarly, with
``Int# :: TYPE IntRep`` and ``Array# :: Type -> TYPE PtrRepUnlifted``.

The problem is that all unboxed tuples have the same kind,
``TYPE UnboxedTupleRep``.
This violates the intent of levity polymorphism, wherein
the kind of a type tells you its representation (and calling convention).

I propose changing this to become::

    TYPE :: RuntimeRep -> Type   -- same super-magical constant
    data RuntimeRep = LiftedRep
             	    | UnliftedRep
             	    | IntRep
             	    | FloatRep
             	    | ...
             	    | TupleRep [RuntimeRep]
             	    | SumRep [RuntimeRep]
    type Type = TYPE LiftedRep

Note the name changes and the new parameters to ``TupleRep`` and ``SumRep``.
These parameters mean that different unboxed tuples/sums have *different*
kinds. Hooray!

Also, this change removes ``VoidRep`` in favor of ``TupleRep []``, a small
simplification.

Drawbacks
---------

The renaming is indeed gratuitous. But it still seems early enough in the
adoption of this feature that we can do this. Note that the changed names are
exported only by ``GHC.Types`` and ``GHC.Exts``, so programs using these names
have opted into experimental features.

Alternatives
------------

I argue that the status quo is untenable, because it fails to fulfill the
promise of levity polymorphism.

One alternative is presented in Simon and my `paper on the subject`_, where
``TYPE :: [RuntimeRep] -> Type`` (note the list!). The list contains the
representations of all components of an unboxed tuple. Anything other than an
unboxed tuple has a singleton list. This alternative is more elaborate than
what is proposed here, and it would allow, for example ::

    foo :: forall (a :: TYPE '[IntRep, FloatRep]). a -> a
    foo x = x

to be instantiated, say, at both ``(# Int#, Float# #)`` and
``(# Int#, (# (# #), Float# #) #)``,
because these both have the same representation. This is
all well and type-safe, but no one is really asking for this feature, and it
complicates the type system.

A fully broken partial implementation of this is available `here`__.

__ https://github.com/goldfirere/ghc/tree/wip/runtime-rep-lists

Unresolved Questions
--------------------

I don't have any at the moment.

Related Concerns
----------------

The original introduction of levity polymorphism generated much consternation
around, e.g., the type of ``($)``, which suddenly became quite complicated.
(To wit:
``($) :: forall (r :: RuntimeRep) a (b :: TYPE r). (a -> b) -> a -> b``)
This was fixed by adding a new flag to GHC,
``-fprint-explicit-runtime-reps``, without which the levity polymorphic bits
get defaulted to `PtrRepLifted`, making everything look non-scary again. This
proposal makes *no* change to this behavior.
