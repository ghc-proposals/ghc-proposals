Pointer Rep
==============

.. author:: Andrew Martin
.. date-accepted:: 2019-04-17
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/17526
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/203>`_ and amended in `#301 <https://github.com/ghc-proposals/ghc-proposals/pull/301>`_.
.. contents::

GHC's ``RuntimeRep`` allows programmers to deal polymorphically with types of a
common runtime representation. It distinguishes between lifted pointer
representations, unlifted pointer representations, and various unlifted
non-pointer representations. There is a shortcoming of the way this
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
      = BoxedRep Levity
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
      = BoxedRep Levity
      | IntRep
      | ...

The following primitive types are given new kinds::

    data Array# :: forall (v :: Levity). TYPE ('BoxedRep v) -> Type
    data MutableArray# :: forall (v :: Levity). Type -> TYPE ('BoxedRep v) -> Type
    data SmallArray# :: forall (v :: Levity). TYPE ('BoxedRep v) -> Type
    data SmallMutableArray# :: forall (v :: Levity). Type -> TYPE ('BoxedRep v) -> Type
    data MutVar# :: forall (v :: Levity). Type -> TYPE ('BoxedRep v) -> Type
    data TVar# :: forall (v :: Levity). Type -> TYPE ('BoxedRep v) -> Type
    data MVar# :: forall (v :: Levity). Type -> TYPE ('BoxedRep v) -> Type
    data Weak# :: forall (v :: Levity). TYPE ('BoxedRep v) -> Type
    data StableName# :: forall (v :: Levity). TYPE ('BoxedRep v) -> Type
    data StablePtr# :: forall (v :: Levity). TYPE ('BoxedRep v) -> Type

Functions operating on the aforementioned types are given new kinds. The ``Levity``
argument is marked as inferred. For example::

    readArray# :: forall {v :: Levity} (s :: Type) (u :: TYPE ('BoxedRep v)). MutableArray# s u -> Int# -> State# s -> (#State# s, u#)
    makeStableName# :: forall {v :: Levity} (a :: TYPE ('BoxedRep v)). a -> State# RealWorld -> (#State# RealWorld, StableName# a#)

The functions ``mkWeak#``, ``mkWeakNoFinalizer#``,
``touch#``, and ``with#`` are more constrained in a type argument that was
previously accepted types of any representation (``ua`` and ``u`` below)::

    mkWeak# :: forall {va :: Levity} {vb :: Levity} (ua :: TYPE ('BoxedRep va)) (ub :: ('BoxedRep vb)) (c :: Type).
      ua -> ub -> (State# RealWorld -> (#State# RealWorld, c#)) -> State# RealWorld -> (#State# RealWorld, Weak# ub#)
    mkWeakNoFinalizer# :: forall {va :: Levity} {vb :: Levity} (ua :: TYPE ('BoxedRep v)) (ub :: TYPE ('BoxedRep v)).
      ua -> ub -> State# RealWorld -> (#State# RealWorld, Weak# ub#)
    touch# :: forall {v :: Levity} (u :: TYPE ('BoxedRep v)).
      u -> State# RealWorld -> State# RealWorld
    with# :: forall {v :: Levity} {rep :: RuntimeRep} (u :: TYPE ('BoxedRep v)) (s :: Type) (r :: TYPE rep).
      u -> (State# s -> (# State s, r #)) -> State# s -> (# State# s, r #)

The parser for ``primops.txt.pp`` is tweaked to assigned levity-polymorphic
kinds to type variables starting with ``u``. There is already a similar
hack in place that gives representation-polymorphic kinds to type variables
starting with ``o``.

This proposal does not loosen any of the restrictions around where
representation polymorphism can be used. From the
`levity polymorphism paper`_, the fundamental rule is:

    Never move or store a levity-polymorphic value.

The two restrictions that enforce this rule are:

    1. Disallow levity-polymorphic binders. Every bound term variable
    in a Haskell program must have a type whose kind is fixed
    and free of any type variables...
    2. Disallow levity-polymorphic function arguments. Arguments
    are passed to functions in registers. During compilation, we
    need to know what size register to use.

Neither of these are changed. That is, every bound term variable
must still have a type whose kind is fixed (that is, no ``RuntimeRep``
variables or ``Levity`` variables are permitted to show up in
the kind of the type of a bound variable). It is
`possible to loosen the binder restriction`_, but this proposal does
include such a change since it would make implementation more
difficult.

Consequently, all of the
functions dealing with levity-polymorphic arguments are marked as
having a compulsory unfolding. It is left for a future proposal to
loosen the binder restriction, making the compulsory unfoldings unneeded.

.. _levity polymorphism paper: https://cs.brynmawr.edu/~rae/papers/2017/levity/levity-extended.pdf
.. _possible to loosen the binder restriction: https://gitlab.haskell.org/ghc/ghc/issues/15532

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
The type signatures of primops become a little harder to read. Users of
``'LiftedRep`` and ``'UnliftedRep`` would be required to changed these
to ``'BoxedRep 'Lifted`` and ``'BoxedRep 'Unlifted`` respectively. It is
possible for a backwards-compatibility package to introduce::

    type LiftedRep = 'BoxedRep 'Lifted
    type UnliftedRep = 'BoxedRep 'Unlifted

However, this only half-way work. GHC encourages user (with warning
messages) to tick promoted data constructors, and these type synonyms
can only be used without ticks. Backward compatible code using these
is guaranteed to emit warnings when build on older GHCs with ``-Wall``.
For this reason, this proposal recommends that these type synonyms
not be included with ``base`` or ``ghc-prim``.

All code using ``'LiftedRep`` or ``'UnliftedRep`` will break. This
includes the ``primitive`` library, which explicitly mentions
``UnliftedRep`` in ``Data.Primitive.UnliftedArray``. It is trivial to
patch with ``CPP``, and there is already some ``CPP`` in there for the
``RuntimeRep`` data constructor rename between GHC 8.0 and GHC 8.2.


Alternatives
------------
The Unlifted Newtypes proposal eschews levity polymorphism in favor
of monomorphism. This leads to more types and more functions.

The version of this proposal originally accepted by the commitee used
the name ``PtrRep`` instead of ``BoxedRep``. In subsequent discussion,
it was decided that ``BoxedRep`` better conveyed the intent of the
data constructor.

Unresolved Questions
--------------------
None.

Implementation Plan
-------------------
I, Andrew Martin, will implement this proposal.
