Pointer Rep
==============

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

GHC's ``RuntimeRep`` allows programmers to deal polymorphically with types of a
common runtime representation. It distinguishes between lifted pointer
representations, unlifted pointer representations, and various unlifted
non-pointer reprenestations. There is a shortcoming of the way this
distinction is currently drawn. There are several primitives types
(``MutVar#``, ``MVar#``, ``Array#``, ``TVar#``, ``SmallArray#``, etc.)
that are polymorphic in such a way that they only accept lifted types.
The runtime actually allows them to accept unlifted pointer types as
well, but the data constructors of ``RuntimeRep`` are not expressive
enough to capture this possibility.

Here it is proposed that the definition of ``RuntimeRep`` be changed
from::

    data RuntimeRep
      = LiftedRep
      | UnliftedRep
      | IntRep
      | ...

to::

    data Levity = Lifted | Unlifted
    data RuntimeRep
      = PtrRep Levity
      | IntRep
      | ...

The aforementioned primitives, along with their relevant corresponding
functions, would accept levity-polymorphic (but not
representation-polymorphic) types. This proposal obsoletes the
`Unlifted Array proposal`_, which was accepted but has not yet
been implemented, since it accomplishes more with even fewer
primitives.

.. _Unlifted Array proposal: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0021-unlifted-array.rst

Motivation
------------
All of the reasons for which the `Unlifted Array proposal`_ proposal were
accepted are applicable here as well. Those reasons are not reiterated
here, so please please read the motivation section of that proposal for
a discussion of the deficiencies of ``ArrayArray#``.

.. _Unlifted Array proposal: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0021-unlifted-array.rst

What else do we get? In the discussion of the Unlifted Array proposal,
David Feuer and Michal Terepeta `expressed interest`_ in a variant of
``SmalArray#`` that could hold unlifted values. That proposal was
accepted without the addition, but this proposal gives us the ability 
to express this. Furthermore, it does this without adding any new
primitive types. Elsewhere, David has `suggested an UnliftedRef type`_.
This proposal gives us a way to express that without ``unsafeCoerce#``.

.. _expressed interest: https://github.com/ghc-proposals/ghc-proposals/pull/112#issuecomment-385773161
.. _suggested an UnliftedRef type: https://github.com/haskell/primitive/issues/198

Where does the need for this arise? We can reduce some needless boxing
in the event manager if we are able to store unlifted types in ``MutVar#``.
Consider functions like::

    eventManager :: IORef (IOArray Int (Maybe (ThreadId, EventManager)))

If we could store the unlifted ``Array#`` type directly in a ``MutVar#``,
we could remove the wrapper that ``IOArray`` imposes. (Actually, this
example is more complicated. It is also necessary to replace the use of
``IOArray`` with an array type that doesn't store the lower and upper
bounds.)

Proposed Change Specification
-----------------------------
The ``RuntimeRep`` data type is redefined as::

    data Levity = Lifted | Unlifted
    data RuntimeRep
      = PtrRep Levity
      | IntRep
      | ...

The following primitive types are given new kinds::

    data Array# :: forall (v :: Levity). TYPE ('PtrRep v) -> Type
    data MutableArray# :: forall (v :: Levity). Type -> TYPE ('PtrRep v) -> Type
    data SmallArray# :: forall (v :: Levity). TYPE ('PtrRep v) -> Type
    data SmallMutableArray# :: forall (v :: Levity). Type -> TYPE ('PtrRep v) -> Type
    data MutVar# :: forall (v :: Levity). Type -> TYPE ('PtrRep v) -> Type
    data TVar# :: forall (v :: Levity). Type -> TYPE ('PtrRep v) -> Type
    data MVar# :: forall (v :: Levity). Type -> TYPE ('PtrRep v) -> Type

Functions operating on the aforementioned types are given new kinds. The ``Levity``
argument is marked as inferred. For example::

    readArray# :: forall {v :: Levity} (s :: Type) (u :: TYPE ('PtrRep v)). MutableArray# s u -> Int# -> State# s -> (#State# s, u#)

Additionally, the functions ``mkWeak#``, ``mkWeakNoFinalizer#``,
``touch#``, and ``with#`` are given more accurate, more constrained,
types::

    mkWeak# :: forall {v :: Levity} (u :: TYPE ('PtrRep v)) (b :: Type) (c :: Type).
      u -> b -> (State# RealWorld -> (#State# RealWorld, c#)) -> State# RealWorld -> (#State# RealWorld, Weak# b#)
    mkWeakNoFinalizer# :: forall {v :: Levity} (u :: TYPE ('PtrRep v)) (b :: Type).
      u -> b -> State# RealWorld -> (#State# RealWorld, Weak# b#)
    touch# :: forall {v :: Levity} (u :: TYPE ('PtrRep v)).
      u -> State# RealWorld -> State# RealWorld
    with# :: forall {v :: Levity} (u :: TYPE ('PtrRep v)) (s :: Type) (r :: Type).
      u -> (State# s -> (# State s, r #)) -> State# s -> (# State# s, r #)

The parser for ``primops.txt.pp`` is tweaked to assigned levity-polymorphic
kinds to type variables starting with ``u``. There is already a similar
hack in place that gives representation-polymorphic kinds to type variables
starting with ``o``.

This proposal does not loosen any of the restrictions around where
representation polymorphism can be used. Consequently, all of the
functions dealing with levity-polymorphic arguments are marked as
having a compulsory unfolding. It is left for a future proposal to
loosen these restrictions, making the compulsory unfoldings unneeded.

The ``ArrayArray#`` type and its functions are shimmed out in
``GHC.Exts``. This strategy was discussed in the Unlifted Array
proposal and is essentially the same here.

Effect and Interactions
-----------------------
The proposed changes give users greater flexibility with several primitive
types. Judicious use of ``Inferred`` means that even the rare bird
who uses ``TypeApplications`` with GHC's primops would be unaffected.
Anyone trying to write things like ``touch 5#`` would start getting
compile errors instead of having the compiler silently accept this
nonsense construction.

Costs and Drawbacks
-------------------
The type signatures of primops become a little harder to read.


Alternatives
------------
The Unlifted Newtypes proposal eschews levity polymorphism in favor
of monomorphism. This leads to more types and more functions.


Unresolved Questions
--------------------
Can we do this for ``Weak#``, ``StablePtr#``, and ``StableName#``. Probably
yes, but I do not understand how the runtime treats these, so I'm not sure.


Implementation Plan
-------------------
I, Andrew Martin, will implement this proposal.
