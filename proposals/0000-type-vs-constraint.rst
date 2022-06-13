Separating Type from Constraint
==============================

.. author:: Richard Eisenberg and Simon Peyton Jones
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/518>`_.
.. sectnum::
.. contents::

Since version 8.0, GHC has supported a rich structure of inhabited types through the
new primitive ``TYPE``, allowing
unlifted types to be treated as first-class citizens, leading to the possibility
of unlifted datatypes and newtypes, already available.

This proposal extends this treatment to constraints, whose inhabitants
can usefully be viewed as implicit parameters (typically type class dictionaries).
Just like how the existence of ``TYPE`` directly opened the door to new innovations
in datatype and newtype definitions, we expect the similar treatment of constraints
to allow for strict classes and unlifted and unboxed implicit parameters.

In addition, the new treatment proposed here allows GHC's internal language, Core,
to more properly tell the difference between types and constraints, eliminating a
troubling class of bugs that keeps recurring.

Motivation
----------

1. With this proposal, we immediately allow unlifted and unboxed implicit parameters
   using the existing ``(?x :: Int#)`` syntax. This allows programs to abstract over
   implicit parameters without suffering inefficiencies due to boxing.

2. This proposal allows a fix to several type-checker bugs that are otherwise elusive.
   With this proposal, we can eliminate this entire class of bugs, that we otherwise
   believe will continue to haunt GHC.  Currently in GHC, `Type` and `Constraint` are
   *distinct* in the typechecker, but *identical* in Core.  That leads to massive duplication;
   many, many functions over types have two versions, one for the typechecker and one for Core.
   It also leads to intellectual complexity, with two subtly-different forms of type equality.
   Worse, nothing maintains the separation, which leads to those elusive bugs.

3. Although we do not propose unlifted constraints in this proposal, we believe that
   this paves the way toward new innovations in constraint representations.

There is a lot of discussion about the gnarly Type-vs-Constraint problem in
`GHC issue #11715 <https://gitlab.haskell.org/ghc/ghc/-/issues/11715>`_, which is six
years old (gnarly!) and in turn lists a raft of other tickets blocked on this issue.

Background
----------

This section describes key aspects of the status quo.

The current type structure
:::::::::::::::::::::::::::

The current root of our type system contains these definitions::

  -- Primitive type constructors
  type TYPE       :: RuntimeRep -> Type
  type Constraint :: Type    -- The kind of constraints
  type Symbol     :: Type    -- The kind of compile-time strings
  type IP         :: Symbol -> Type -> Constraint   -- Implicit parameters

  type FUN :: forall (m :: Multiplicity) ->
              forall (r1 :: RuntimeRep) (r2 :: RuntimeRep).
              TYPE r1 -> TYPE r2 -> Type

  -- Data type declarations, used only at the type level
  data Multiplicity = Many | One
  data Levity       = Lifted | Unlifted
  data RuntimeRep   = BoxedRep Levity | IntRep | FloatRep | ...

  -- Type synonyms
  type LiftedRep   = BoxedRep Lifted
  type UnliftedRep = BoxedRep Unlifted
  type Type        = TYPE LiftedRep
  type (->)        = FUN Many

  -- (=>) is not something that can be written unsaturated;
  --      rat

NB: in GHC, implicit parameters are internally represented as a special class,
but that is not user-visible.

Type and Constraint are not apart
:::::::::::::::::::::::::::::::::::

GHC has an optimization for one-element classes (where the element
is either a superclass or a method), defining these in like a newtype, not a datatype.
For example, if we have ::

  class Default a where
    def :: a

the Core of the program will have a definition like ::

  newtype Default a = MkDefault a

In turn, this newtype gives rise to an axiom (coercion), like so::

  axDefault :: Default a ~R# a

where ``~R#`` represents primitive representational equality. Note that
``axDefault`` is *heterogeneous*: the kind of ``Default a`` is ``Constraint``,
whereas the kind of ``a`` is ``Type``.

GHC allows us to extract out an equality relationship between *kinds* from an
equality relationship on *types* -- and kind equalities are always nominal. To
wit, Core allows ::

  KindCo axDefault :: Constraint ~# Type

Now, suppose that you could write this::

  type family F a
  type instance F Type = Int
  type instance F Constraint = Bool

If these instances were allowed, GHC could
produce a coercion between ``Int`` and ``Bool``, thus::

  Bool  ~#  F Constraint   -- By type instance F Constraint (backwards)
        ~#  F Type         -- By KindCo axDefault
        ~#  Int            -- By type instance F Type

That would be Very, Very Bad.  So, although ``Type`` and ``Constraint`` are built
with different (un-equal) primitive type constructors,

* **GHC's type checker treats `Type` and `Constraint` as *not apart*.**

That in turn makes GHC complain that the above instances overlap, and are hence illegal.


Proposed Change Specification
-----------------------------

We propose the following new setup, not repeating any types that remains unchanged::

  -- Primitive type constructors
  type SORT :: TypeOrConstraint -> RuntimeRep -> Type
  type IP   :: forall (r :: RuntimeRep). Symbol -> TYPE r -> CONSTRAINT r

  type (=>)  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep).
                CONSTRAINT r1 -> TYPE r2 -> Type  -- primitive
  type (==>) :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep).
                CONSTRAINT r1 -> CONSTRAINT r2 -> Constraint

  -- Data types
  data TypeOrConstraint = TypeLike | ConstraintLike

  -- Synonyms
  type TYPE       = SORT TypeLike
  type CONSTRAINT = SORT ConstraintLike
  type Constraint = CONSTRAINT LiftedRep


Changes to the type structure
:::::::::::::::::::::::::::::

This proposal introduces ``(=>)`` and ``(==>)`` as proper type constructors, just like
any other. Just like ``(->)``, they have kinds and can be abstracted over.
Unlike ``FUN``, they do not take a ``Multiplicity`` argument; implicitly, it is ``Many``.

The ``==>`` arrow is used in two places:

* In instance heads, like ``instance Eq a => Eq (Maybe a)``
* In quantified constraints, like ``forall x. c x => Eq x``

In order to be backward compatible,
we allow writing ``=>`` instead of ``==>`` in instance heads and in quantified constraints.
That is, the *concrete* syntax for the type construtor ``==>`` can still use ``=>``.
However, users may also choose to write ``==>`` (imported from ``GHC.Exts``) if they choose.

Implicit parameters
:::::::::::::::::::::::::::::

Now that constraints can have varying runtime representation (via ``CONSTRAINT rep``),
the door is open to having unlifted constraints, or constraints whose representation is
an unboxed type like ``Int#``.  In this proposal we exploit this opportunity only in a
limited way, by generalising the kind of ``IP``, thus::

  type IP   :: forall (r :: RuntimeRep). Symbol -> TYPE r -> CONSTRAINT r

So now this is accepted::

  f :: (?x :: Int#) => Int# -> Int#
  f y = ?x +# y


Type and Constraint are not apart
:::::::::::::::::::::::::::::::::

It remains the case that ``Type`` must not be apart from ``Constraint``, because
making them apart is unsound in the presence of the current ``newtype`` optimization for
one-element classes.
Accordingly, under this proposal,

  * ``TypeLike`` and ``ConstraintLike`` will be considered not *apart*.

As a consequence, ``Type`` and ``Constraint`` are also not *apart*, just as today.
This a wart, but it is an *existing* wart, and one that is not easy to fix.

Future stability
:::::::::::::::::::::::::::::::::

We anticipate that the kind of ``SORT`` may change again, for example to accommodate the ideas
of `Kinds are calling conventions <https://simon.peytonjones.org/kinds-are-calling-conventions/>`_.
Accordingly:

* ``Data.Kind`` exports: ``Symbol``, ``Type``, ``TYPE``, ``Constraint``,
  ``CONSTRAINT``, ``RuntimeRep``, ``Multiplicity``, ``Levity``, ``(->)``, ``(=>)``, and ``(==>)``.
  ``Data.Kind`` should have a stable API; the kinds of these type
  constructors will not change.

* ``GHC.Prim`` exports ``SORT``, ``TypeOrConstraint``, ``IP``.
  Users may import them from ``GHC.Prim``, but they should not complain if they change in future.

Implementation notes
:::::::::::::::::::::::::::::::::

The fully-applied types ``FUN m r1 r2 t1 t2``, ``(=>) r1 r2 t1 t2``, and ``(==>) r1 r2 t1 t2`` can
all be represented inside GHC by ``FunTy m t1 t2`` (where ``m`` is ``Many`` for ``(=>)`` and ``(==>)``),
just as today.  That is, the proposal does not impose
a new burden on GHC's internal representations.

Examples
--------
This is now accepted::

  f :: (?x :: Int#) => Int# -> Int#
  f y = ?x +# y

So is this::

  g :: (=>) (Eq a) (a -> Bool)
  g x = x == x

along with other abstractions over ``(=>)``.

Effect and Interactions
-----------------------
* We can fix type-checker tickets that have proved resistant to principled fixes.

* The door is open to new innovations in strict classes.

* This proposal is fully backward compatible.

* This proposal is forward compatible with more glorious updates to the type/constraint
  system we might imagine in the future, as detailed at TODO.

Costs and Drawbacks
-------------------
* This adds complexity to the root of our type system. However, we have learned
  how to manage this complexity and protect users from seeing it. We do not expect
  routine users to notice this change, but users who specify ``-fprint-explicit-runtime-reps``
  will see some changes.

Alternatives
------------
* This seems to be the best way to support unlifted and unboxed constraints.

* We have another putative fix to the type-checker bugs, but it would take a
  major sea-change to the compiler, requiring a full separation between Haskell
  types and Core types.
  (Currently, GHC uses the same representation for both,
  a considerable simplification, as only one type needs to be e.g. written to
  interface files.) This other fix would likely require several solid weeks
  of implementation work, and would have consequences (both implementation and end-user) that are hard to anticipate.
  In constrast, the one presented here is simple, and we have a clear grasp of its consequences.

Unresolved Questions
--------------------
* How should unlifted or unboxed constraints interact with constraint tuples?
  Right now, we simply wouldn't allow unlifted constraints (implicit parameters only)
  in a tuple.

Implementation Plan
-------------------
* Simon or Richard will implement.

Endorsements
-------------
