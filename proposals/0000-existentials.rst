First-class existentials
========================

.. author:: Richard Eisenberg
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

This proposal introduces first-class existentials into GHC, where type
inference figures out where to pack and unpack existentials, with no need
for user annotations. It is based on the ICFP'21 paper `An Existential
Crisis Resolved: Type inference for first-class existential types <TODO>`_.

.. _paper: TODO

Motivation
----------

* Richly typed programming invariably uses its share of existential types,
  and this proposal makes it vastly easier to work with existentials.
  Currently, every existential must be encoded using its own datatype,
  which is laborious. Furthermore, packing and unpacking these datatypes
  must be done by hand, which is cluttersome.

* A simple example, clearly aided by existentials is ``filter`` over
  length-indexed vectors::

    data Nat = Zero | Succ Nat

    type Vec :: Nat -> Type -> Type
    data Vec n a where
      VNil :: Vec Zero a
      (:>) :: a -> Vec n a -> Vec (Succ n) a
    infixr 5 :>

    filter :: (a -> Bool) -> Vec n a -> Vec ???? a

  Because we cannot know the behavior of the filtering function on every
  element in the input, we cannot know the length of the output of ``filter``.
  We thus need to existentially quantify that length: there exists some length,
  but we do not know what it is.

  Here is how we could write this with this proposal::

    filter :: (a -> Bool) -> Vec n a -> exists m. Vec m a
    filter p VNil = VNil
    filter p (x :> xs)
      | p x       = x :> filter p xs
      | otherwise = filter p xs

  This implementation is simple -- it is exactly what we would write
  without worrying about the type-level index, beyond the use of ``exists``
  in the type.

  A point expanded in the `paper`_ is that this implementation is *lazy*,
  just like ``filter`` should be. I do not believe a lazy implementation
  of ``filter`` over length-indexed vectors is possible in GHC's current
  type system.

* Suppose we have a pretty-printer based around the following class::

    class Pretty a where
      ppr :: a -> Doc

  We would naturally have ::

    instance Pretty a => Pretty [a] where ...

  Yet, if I have ``woz :: Woz`` and ``wiz :: Wiz`` (with instances for
  both types), I cannot ``ppr [woz, wiz]``, because that creates a
  heterogeneous list.

  With this proposal, I could write ::

    pprList :: [exists a. Pretty a /\ a] -> Doc
    pprList = sep . map ppr

  and then have ``pprList [woz, wiz]``.

  It would be even better to have ``ppr [woz, wiz]``, but that seems
  beyond the abilities of type inference at the moment.

* The refinement types of Liquid Haskell often look something like this::

    plusNat :: { x :: Nat } -> { y :: Nat } -> { v :: Nat | v >= x && v >= y }

  where the result type has a refinement making a claim about the result
  of running the function.

  It would amplify the power of Liquid Haskell to have its refinement types
  interact with other type system features in Haskell. Accordingly, we might
  want to represent the inputs as pi-types and the output as a sigma-type --
  which is essentially the same as an existential. Here might be one rendering::

    plusNat :: foreach (x :: Nat) (y :: Nat) -> exists (v :: Nat). Proof (v >= x && v >= y)

  Yet we do not want to manually pack and unpack the existential in the
  definition for ``plusNat`` -- and thus need the inference capabilities proposed
  here.

  Note that this proposal does not go "all the way" toward this encoding of
  refinement types, in that we would not be able to write the type above with
  this proposal. Nevertheless, the automatic inference of packing and unpacking
  described here seems necessary if we are to integrate Liquid Haskell with the
  rest of GHC's type system.

Proposed Change Specification
-----------------------------

1. Introduce a new extension ``-XExistentialTypes``.

#. With ``-XExistentialTypes``, ``exists`` is a keyword in both
   types and terms.

#. With ``-XExistentialTypes``, introduce a new type, according to
   the following grammar (baseline: GHC's parser)::

     ctype → forall_telescope ctype
           | context '=>' ctype
           | exists_telescope ctype   -- NEW!
           | ctype
           | ...

       -- just for comparison
     forall_telescope → 'forall' tv_bndrs '.'
                      | 'forall' tv_bndrs '->'

     exists_telescope → 'exists' tv_bndrs '.'

   An existential is a new form of type, not equal to any current form.

#. In a type ``exists tv_bndrs . ty``, the ``tv_bndrs`` are in scope
   in the ``ty``.

#. In a type ``exists tv_bndrs . ty``, the ``ty`` must have kind
   ``TYPE rep`` for some ``rep``. The type ``exists tv_bndrs. ty`` itself
   has the same kind. (This is just like how ``forall`` is kinded.)

#. Introduce a new module in ``base`` called ``GHC.Exists``.

#. ``GHC.Exists`` exports a type operator ``/\ :: Constraint -> Type -> Type``;
   ``/\`` is injective and generative, like a datatype. It may appear partially
   applied.

#.


Examples
--------
This section illustrates the specification through the use of examples of the
language change proposed. It is best to exemplify each point made in the
specification, though perhaps one example can cover several points. Contrived
examples are OK here. If the Motivation section describes something that is
hard to do without this proposal, this is a good place to show how easy that
thing is to do with the proposal.

Effect and Interactions
-----------------------
Your proposed change addresses the issues raised in the motivation. Explain how.

Also, discuss possibly contentious interactions with existing language or compiler
features. Complete this section with potential interactions raised
during the PR discussion.


Costs and Drawbacks
-------------------
Give an estimate on development and maintenance costs. List how this effects
learnability of the language for novice users. Define and list any remaining
drawbacks that cannot be resolved.


Alternatives
------------
List alternative designs to your proposed change. Both existing
workarounds, or alternative choices for the changes. Explain
the reasons for choosing the proposed change over these alternative:
*e.g.* they can be cheaper but insufficient, or better but too
expensive. Or something else.

The PR discussion often raises other potential designs, and they should be
added to this section. Similarly, if the proposed change
specification changes significantly, the old one should be listed in
this section.

Unresolved Questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and
specification. Be upfront and trust that the community will help. Please do
not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to
the steering committee.


Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other resources
and prerequisites are required for implementation?

Endorsements
-------------
(Optional) This section provides an opportunity for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.
