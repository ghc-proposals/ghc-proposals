Generalising Type and Constraint
==============================

.. author:: Richard Eisenberg and Simon Peyton Jones
.. date-accepted:: 2022-07-28
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/21623
.. implemented:: 9.6.1
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/518>`_.
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

  -- (=>) is not something that can be written unsaturated

NB: in GHC, implicit parameters are internally represented as a special class,
but that is not user-visible.

Type and Constraint are not apart
:::::::::::::::::::::::::::::::::::

GHC has an optimization for one-element classes (where the element
is either a superclass or a method), defining these like a newtype, not a datatype.
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

* **GHC's type checker treats ``Type`` and ``Constraint`` as not "apart".**

That in turn makes GHC complain that the above instances overlap, and are hence illegal.
You can read more about what "apart" means in
`Closed type families with overlapping equations <https://simon.peytonjones.org/closed-type-families/>`_,
sections 3.2 and 3.3.

Apartness affects type-class instances as well as type-family instances.
Suppose we have::

  instance {-# OVERLAPPABLE #-} C Int a  where ...
  instance {-# OVERLAPPING #-}  C Int Bool where ...

and we are trying to solve the constraint ``C Int (F gamma)``, where ``F`` is a type family and ``gamma``
is an as-yet-unknown unification variable. Since ``(F gamma)`` is not
apart from ``Bool``, it could be that (once we know ``gamma``) the second
instance should be chosen. So GHC declines to commit to the first.

If ``(F gamma)`` later simplifes to, say ``Char`` (which *is* apart from ``Bool``),
then and only then GHC can commit to the first instance.

Similarly with ``Type`` and ``Constraint``. Suppose we have::

  instance D Type where...
  instance D Constraint where ...

and try to solve ``C Int Type``. This matches the first instance, but
since ``Type`` is not apart from ``Constraint`` GHC thinks "oh, the
second one could match in the future" and declines to commit to the
first.  In fact, you can have either of these instances separately,
but if they occur together neither will ever be chosen: they overlap
irretrievably.


Proposed Change Specification
-----------------------------

We propose the following new setup, not repeating any types that remain unchanged::

  -- Primitive type constructors
  type CONSTRAINT :: RuntimeRep -> Type
  type IP   :: forall (r :: RuntimeRep). Symbol -> TYPE r -> CONSTRAINT r

  type (=>)  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep).
                CONSTRAINT r1 -> TYPE r2 -> Type  -- primitive
  type (==>) :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep).
                CONSTRAINT r1 -> CONSTRAINT r2 -> Constraint
  type (-=>) :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep).
                TYPE r1 -> CONSTRAINT r2 -> Constraint

  -- Synonyms
  type Constraint = CONSTRAINT LiftedRep


Changes to the type structure
:::::::::::::::::::::::::::::

This proposal introduces ``(=>)``, ``(==>)``, ``(-=>)`` as proper type constructors, just like
any other, with the kinds specified above.
Just like ``(->)``, they have kinds and can be abstracted over.
Unlike ``FUN``, they do not take a ``Multiplicity`` argument; implicitly, it is ``Many``.
Internally, the new arrows are used as follows:

* ``(=>)`` is used for type-class-overloaded types, just as in Haskell, e.g.
  ``f :: forall a. Num a => a -> a``

* ``(==>)`` is used for the dictionary function that arise from an instance declaration such::

      instance Eq a => Eq [a] where ...

  This instance declaration gives rise to a dictionary function ``$fEqList :: forall a. Eq a ==> Eq [a]``.

* ``(-=>)`` is used in the type of the data type for a dictionary.  For example, the data constructor for an ``Eq`` dictionary has the type ``forall a. (a->a->Bool) -=> (a->a->Bool) -=> Eq a``.

The concrete syntax of types and instance declarations is unchanged.
In particular:

* In instance heads we continue to write::

     instance Eq a => Eq (Maybe a) where ...

  and not::

     instance Eq a ==> Eq (Maybe a) where ...

* In quantified constraints we continue to write::

     f :: (forall x. Eq x => Eq (c x)) => c Int -> c Bool

  and not::

     f :: (forall x. Eq x ==> Eq (c x)) => c Int -> c Bool

The new arrow type constructors are exported by ``ghc-prim:GHC.Types``, but
are not part of GHC's stable API, and might be subject to future change: see Section 3.4.

So for users who do not import GHC's unstable API, there is no visible change.

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

* ``TYPE`` and ``CONSTRAINT`` will be considered not *apart*.

As a consequence, ``Type`` and ``Constraint`` are also not *apart*, just as today.
This a wart, but it is an *existing* wart, and one that is not easy to fix.

As before, nothing prevents writing instances like::

  instance C (Proxy @Type a) where ...

In particular, ``TYPE``, ``CONSTRAINT``, ``Type`` and ``Constraint``
are all allowed in instance heads. It's just that
``TYPE`` is not apart from ``CONSTRAINT``
so that instance would irretrievably overlap with::

  instance C (Proxy @Constraint a) where ...

But this is just the status quo; it is not a change (see Sectionn 2.2).

Future stability
:::::::::::::::::::::::::::::::::

In the past it has not been very clear which parts of GHC's API are stable and which
are unstable:

* By "stable" we mean that efforts will be made to avoid change, and any changes should require a GHC proposal.

* By "unstable" we mean that the API should be considered part of GHC's internal
  implementation.  Changes may be made to the unstable API without a proposal.
  Clients are not prevented from importing GHC's unsable API, but they are explicitly
  using parts of GHC's internal implementation, which is subject to change.

Other proposals aim for formalise this stable/unstable distinction, including

.. _`#524`: https://github.com/ghc-proposals/ghc-proposals/pull/524

But, pending a more systematic approach,
this proposal makes a modest start on clarifying the distinction.  In particular:

* The unstable API includes:

  * The new type constructors ``CONSTRAINT``, ``(=>)``, ``(==>)``, and ``(-=>)``;
    exported by ``GHC.Types``.

  * The existing type constructors ``FUN`` and ``IP``; also exported by ``GHC.Types``.

* The stable API includes:

  * ``Symbol``, ``Type``, ``TYPE``, ``Constraint``, ``RuntimeRep``, ``Multiplicity``, ``Levity``, and ``(->)``; all exported by ``Data.Kind``

We keep ``CONSTRAINT`` in the unstable API for now, exposing it only though the possiblity
of having unlifted implicit paramters.

We anticipate that the definition of ``TYPE`` or ``CONSTRAINT``
(currently specified as primitive) may change again,
for example to accommodate the ideas
of `Kinds are calling conventions <https://simon.peytonjones.org/kinds-are-calling-conventions/>`_. For example, we might define::

    type TYPEC :: Maybe Convention -> RuntimeRep -> Type
    type TYPE = TYPEC Nothing
    data Convention = Eval levity | Call ArityDescription
    data ArityDecription = ACons RuntimeRep ArityDescription | AZero | AConv Convention

where ``TYPE`` becomes a type synonym for ``TYPEC``, where the latter embodies
information about arity.  All this is for the future, however, and does not form part of
this proposal.

Implementation notes
:::::::::::::::::::::::::::::::::

The fully-applied types ``FUN m r1 r2 t1 t2``, ``(=>) r1 r2 t1 t2``,
``(==>) r1 r2 t1 t2`` and ``(-=>) r1 r2 t1 t2`` can
all be represented inside GHC by ``FunTy m t1 t2`` (where ``m`` is ``Many`` for ``(=>)``, ``(==>)``, and ``(-=>)``),
just as today.  That is, the proposal does not impose
a new burden on GHC's internal representations.

Examples
--------
This is now accepted::

  f :: (?x :: Int#) => Int# -> Int#
  f y = ?x +# y


Effect and Interactions
-----------------------
* We can fix type-checker tickets that have proved resistant to principled fixes.

* The door is open to new innovations in strict classes.

* This proposal is fully backward compatible.

* This proposal is forward compatible with more glorious updates to the type/constraint
  system we might imagine in the future, as we've [detailed elsewhere](https://gitlab.haskell.org/ghc/ghc/-/issues/21623).

Costs and Drawbacks
-------------------
* This adds complexity to the root of our type system. However, we have learned
  how to manage this complexity and protect users from seeing it. We do not expect
  routine users to notice this change, but users who specify ``-fprint-explicit-runtime-reps``
  will see some changes.

Alternatives
------------

Two different type constructors
::::::::::::::::::::::::::::::::::

Instead of two distinct primitive type constructors, ``TYPE`` and ``CONSTRAINT``,
we considered having just one, ``SORT``, with an argument to distinguish ``TYPE`` from ``CONSTRAINT``::

  type SORT :: TypeOrConstraint -> RuntimeRep -> Type

  data TypeOrConstraint = TypeLike | ConstraintLike
  type TYPE       = SORT TypeLike
  type CONSTRAINT = SORT ConstraintLike

However, experience with a draft implementation convinced us to have two distinct constructors.  With the ``SORT`` approach we would have to worry what ``SORT a`` might mean, where ``a :: TypeOrConstraint`` is a type variable; or ``SORT (F Int)``, where ``F`` is a type function.  These questions could be resolved in a similar way that we ensure concrete runtime-reps for lambdas and applicatdions, but this seems like a sledgehammer to crack a nut.  We do not seek type-or-constraint polymorphism, and it seems simplest to rule it out by construction.

However:, in Core we have to say that if ``e :: ty :: ki`` then ``ki`` must be ``TYPE rr`` or ``CONSTRAINT rr``.  Similarly for the types of binders. That "or" isn't really a problem, but it's a bit inelegant.


Fully separate Haskell types from Core types
::::::::::::::::::::::::::::::::::::::::::::

An alternative approach would be to fully separate Haskell types from Core types,
where ``Type`` and ``Constraint`` are distinct in the former but the same in the latter.
(Currently, GHC uses the same representation for both,
a considerable simplification, as only one type needs to be e.g. written to
interface files.)
This would be a major sea-change to the compiler, requiring weeks of effort, considerable code duplication,
and knock-on effects (both implementation-wise and end-user-visible) that are hard to predict and might be unwelcome.

In constrast, the one presented here is simple, and we have a clear grasp of its consequences.

Tuples
:::::::::::::

This proposal does not address the problem of tuples, although is tempting to do so.  In GHC today we have::

      (,)   :: Type -> Type -> Type
      (%,%) :: Constraint -> Constraint -> Constraint

except that the latter is written, in source Haskell, as ``(c1,c2)``.  That leads to syntactic ambiguity. If I write::

      type S a b a = ((a,b), c)

do I intend to define a Constraint synonym, or a Type synonym?  It's tempting to give (,) a polymorphic type::

      (,) :: forall (v:TypeOrConstraint). SORT v LiftedRep -> SORT v LiftedRep -> SORT v LiftedRep

But, tempting as it is, we do not propose this.  The tuples problem
is all about concrete syntax. It just so happens that we use the
same parentheses and commas for normal tuples and constraint
tuples. No one has asked for polymorphism here. And it seems
overwrought to complexify our type system just to allow for this
little concrete-syntax convenience. Letting tuple syntax drive the
type system is the tail wagging the dog.  We leave this for the future.

Unresolved Questions
--------------------
* How should unlifted or unboxed constraints interact with constraint tuples?
  Right now, we simply wouldn't allow unlifted constraints (implicit parameters only)
  in a tuple.

Implementation Plan
-------------------

Simon or Richard will implement.

Endorsements
-------------
