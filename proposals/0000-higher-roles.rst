Simulate higher-order roles via cleverer role inference
=======================================================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/233>`_.
.. sectnum::
.. contents::

If we write ``data T f a = MkT (f a)``, GHC assigns a nominal role to ``a``, because it appears
as the argument of a type variable, and we cannot know how this argument will be used. This proposal
describes a way to assign a representational role to ``a`` via the use of a quantified constraint.

Motivation
----------
Consider::

  -- in a library
  data GList :: (Type -> Type)   -- concrete representation
             -> Type             -- element type
             -> Type where
    Mk :: Metadata      -- some metadata; details irrelevant
       -> v a           -- contents
       -> GList v a

  newtype Linked a = MkLinked (GList [] a)

  -- in a client
  newtype Age = MkAge Int
  change :: Linked Age -> Linked Int
  change = coerce

It would be nice if this could work. The problem is that we have ``type role
GList representational nominal``, with the ``a :: Type`` parameter getting a
nominal role. This happens because we can't know the role of ``v``\'s argument,
and so GHC (correctly) conservatively chooses nominal.

The only solution I could offer is to make ``GList`` a newtype (holding a tuple)
so that GHC could look through the newtype to its representation and then
discover that ``Linked Age ~R (Metadata, [Age]) ~R (Metadata, [Int]) ~R Linked
Int``. This is sad, though, because we don't want to export the constructor of
``GList``, which is necessary in order to make those representational
equalities. And it's also sad that we need a newtype here.

This issue was directly inspired by a need in real software, as reported by
Lennart Augustsson of Google Research.

Proposed Change Specification
-----------------------------
When role inference inspects the type of a data constructor, it will assume both the
data constructor and datatype context as a givens.

Then, when inferring the role of the argument of a type ``ty`` (which may be a type
variable or some other type):

1. Try to solve ``forall a b. Coercible (ty a) (ty b)``. If solving succeeds, then
   add no constraints on the role of the argument of ``ty``.

2. Try to solve ``forall a b. Coercible a b => Coercible (ty a) (ty b)``. If solving
   succeeds, then mark the argument of ``ty`` as having at least a representational
   role.

3. Otherwise, mark the argument of ``ty`` as having a nominal role.

Effect and Interactions
-----------------------
1. We can now write::

     data GList v a where
       Mk :: (forall c d. Coercible c d => Coercible (v c) (v d)) => Metadata -> v a -> GList v a
     type role GList representational representational

2. You might be worried (I was) that the fact that datatype contexts are broken would
   make this unsound. That is, even if we have ``G :: Type -> Type`` with ``type role G nominal``, one
   can still talk about the type ``GList G Int``. However, we would never have a value
   of that type, and so no threat to soundness can come of the type's existence.

3. It would appear that the ``Mk`` constructor in this example would now have to hold
   evidence for the implication constraint. However, it seems quite possible to detect this
   shape of dictionary and eliminate it.

4. Unlike previous versions of this proposal, this version does *not* change the status
   of ``-XDatatypeContexts``.
   
Costs and Drawbacks
-------------------
1. This is potentially non-performant, requiring running the solver a lot during role inference.
   However, it will happen only on types that have a datatype context. If we're worried about
   performance, we could require users to write a role annotation to confirm that they want GHC
   to do the extra work.

2. It has not been proved sound. Even so, I would be comfortable adding this to GHC without a proof.
   There are already 4 publications about roles!

3. There is a remote chance that this would relax some roles in existing code, meaning that a library-writer's
   abstraction barrier might be compromised. There is also a remote chance that I will quantum-tunnel
   through the floor and break my leg on the floor below. Neither of these is likely.

4. Newtypes will have to use datatype contexts, as newtype
   constructors cannot have contexts.

Alternatives
------------
* Instead of using a quantified constraint and invoking the solver, we could come up with a more
  restrictive syntax for this idea, which expands out to a quantified constraint on every constructor.
  This would mean we wouldn't run the solver during role inference. However, such a feature seems
  sadly non-orthogonal when we have the features to express this idea already.

* Previous versions of this proposal had the constraints either in the datatype context (only) or
  in the constructor contexts (only).
  In this version of the proposal, allowing constraints in both places, users can make their own decisions. This seems strictly better than either previous
  proposal.

* An `alternative proposal <https://github.com/ghc-proposals/ghc-proposals/pull/248>`_
  introduces an entirely new mechanism for ``Coercible``. At this point, that proposal
  has yet to become convincingly sound, and it is *much* more invasive than this one.
  It is worth reading as a counterpoint, but I don't think it's yet ready to be counted
  as a viable.
  
Unresolved Questions
--------------------
None at this time.

Implementation Plan
-------------------
I suppose I could implement.
