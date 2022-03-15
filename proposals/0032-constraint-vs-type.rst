Declare ``Constraint`` is not apart from ``Type``
=================================================

.. author:: Richard Eisenberg
.. date-accepted:: 2018-04-24
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/21092
.. implemented:: 9.4.1
.. highlight:: haskell
.. header::
   This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/32>`_.
.. contents::

Since at least GHC 7.4, there has been an uneasy relationship between ``Constraint`` and ``Type`` (formerly known as ``*``). These
kinds were considered distinct in Haskell but indistinguishable in Core. This strange arrangement causes oddities in the
type system, as explained in `#11715 <https://gitlab.haskell.org/ghc/ghc/issues/11715>`_.

An earlier version of this proposal suggested a fix for this by separating ``Constraint`` and ``Type``.
However, that solution is quite heavy (it is listed in the `Old proposal`_ section). So, this
proposal now is simply to declare that ``Type`` and ``Constraint`` are not *apart* in Haskell.

Motivation
------------
Consider ::

       type family F a
       type instance F Constraint = Int
       type instance F Type       = Bool

Such a definition is accepted. Yet it allows a proof that ``Int ~ Bool`` in Core.
It's unclear to me whether this can be used to implement ``unsafeCoerce``, but it should scare us all.

(Earlier versions of this proposal mentioned other motivations around ``Typeable`` getting confused
between ``Constraint`` and ``Type``. However, these are properly interpreted simply as bugs. I've
moved them to the bottom of this document for posterity.)

Proposed Change Specification
-----------------------------

GHC uses a notion of *apartness* (as defined in the `Closed Type Families paper <https://repository.brynmawr.edu/cgi/viewcontent.cgi?article=1006&context=compsci_pubs>`_) to determine what open type family instances to accept and how to
reduce closed type families. In brief, two types are *apart* if they can never represent
the same type, even after type family reductions and variable instantiations. ``Int``
and ``Bool`` are apart, while ``F Int`` and ``Bool`` are not (for a type family ``F``).

Under this proposal, ``Type`` and ``Constraint`` would no longer be apart. And that's the
only change!

Effect and Interactions
-----------------------

The effect of this would be that the following pairs of instances would be considered
overlapping::

  class C a where ...
  instance C Type where ...
  instance C Constraint where ...

  type family F a
  type instance F Type = Int
  type instance F Constraint = Bool

That is, the pair of instances would be rejected.

In addition, suppose we had this type family::

  type family Equals a b where
    Equals a a = True
    Equals a b = False

If a user tested ``Equals Type Constraint``, that type family application would fail
to reduce either to ``True`` or to ``False``. (``Equals Type Type`` and ``Equals Constraint Constraint``
would still both remain ``True``.)

Motivators (2)-(3) are all to do with ``Typeable``. We could fix this by teaching the ``Typeable`` solver
to offer up one ``TypeRep`` for ``Type`` and a different one for ``Constraint``. These ``TypeRep``\s would
compare as different. Indeed, in retrospect, I'm not sure why we haven't already done this.

Note that it's terrible to have the same ``TypeRep`` for two types that are distinct in Core but OK
to have two different ``TypeRep``\s for two types that are equal in Core.

A motivator on earlier versions of this proposal was about inferring the difference between
``() :: Constraint`` and ``() :: Type``.
I've come to view this as a red herring. Some possible ways forward here would indeed make it easier
to implement better type inference around ``()``, but that shouldn't be a primary goal here. After all,
this is really about sorting out a mess in Core, and we shouldn't be overly swayed by type inference.
For example, it's perfectly possible to come up with a scheme where empty tuples are decorated with
some solvable parameter during type inference, only to have desugaring (after everything has been solved
for and/or defaulted) look at that parameter to select the right ``TyCon``.

Costs and Drawbacks
-------------------

This should be dead easy to implement.

The drawback is that ``Constraint`` and ``Type`` really are distinct in Haskell, and so it's
quite odd that these types not be *apart*. This would be counterintuitive to users, and could
be explained only by discussing Haskell's elaboration to Core.

Maintaining the distinction between ``Constraint`` and ``Type`` (while representing them internally
as the same thing) adds some burden to the implementation. This is a burden we have been carrying
for some time.

Alternatives
------------

1. Instead of saying that ``Type`` and ``Constraint`` are not apart, we could have the instance
   lookup machinery treat them interchangeably. That means that an ``instance C Type`` would match
   a need for an ``instance C Constraint`` and that ``Equals Type Constraint`` would be ``True``.
   Note that this would affect only instance-lookup. A user would still not be able to prove
   ``Type ~ Constraint``, which goes via a different mechanism.

2. Adopt one of the heavy solutions listed in `Old proposal`_. In particular, that describes an
   Alternative (3) that seems much better than anything here. Unfortunately, it requires significant
   amounts of type-theory research to sort out what roles in kinds might mean, so is inaccessible
   for some time.

Implementation Plan
-------------------

I or a close collaborator volunteers to implement. Offers of help are welcome.

Old proposal
------------

The (unedited) text below is from an older version of this proposal. In the end, this was
deemed too heavy.

This proposal separates ``Constraint`` from ``Type`` in Core by defining these as separate
datatypes. In order for the type system to hold together, we must have four different
arrow types now, one for each possible combination of a function taking/returning types
of kind ``Constraint`` and ``Type``. An advantage of this arrangement is that ``(=>)``
becomes a first-class type. All the arrows are representationally equal to ``(->)``
and can be coerced. This last bit has the further advantage that the idiom used
in the ``reflection`` library can use ``coerce`` where it currently uses ``unsafeCoerce``.

**User-facing changes**: The ``Typeable`` mechanism can tell the difference between ``Constraint`` and ``Type``.
``(=>)`` becomes a first-class type.
Users can reach into ``GHC.Exts`` to get ``(==>)`` and ``(-=>)``, but I can't imagine how these would be used
correctly in Haskell. And that's it! This is really all quite internal!

**Internal changes**:

Any typing rules in here fit into the various typing judgments as presented
`here <https://github.com/ghc/ghc/blob/master/docs/core-spec/core-spec.pdf>`_.

1. In ``GHC.Prim``::

       data (->) :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep). TYPE r1 -> TYPE r2 -> Type
       data (=>) :: forall (r :: RuntimeRep). Constraint -> TYPE r -> Type
       data (==>) :: Constraint -> Constraint -> Constraint
          -- internal, used in dfun types
       data (-=>) :: forall (r :: RuntimeRep). TYPE r -> Constraint -> Constraint
          -- internal, used in dfun data constructors

         -- these last two are never seen in normal Haskell or in error messages,
         -- but I suppose they wouldn't hurt anyone to have exported from GHC.Prim

       instance Coercible ((=>) @r) ((->) @LiftedRep @r)
       instance Coercible (==>) ((->) @LiftedRep @LiftedRep)
       instance Coercible ((-=>) @r) ((->) @r @LiftedRep)
         -- These instances are little white lies, as Coercible isn't a class. Really,
         -- we'll have axioms saying these are representationally equal.

   The original two arrows, ``(->)`` and ``(=>)``, will remain as built-in syntax, always in scope.
   The new arrows will not be built-in syntax, but will be exported from ``GHC.Exts``.

   The "instances" above will be new axioms (``CoAxiom``\s) relating the three fancier arrows to ``(->)``.
   Coercions between the arrows themselves will be built up using transitivity.

2. In ``GHC.Types``::

       type Type = TYPE LiftedRep   -- as today
       data Constraint              -- new and wonderful

3. New typing rules for Core lambdas::

       G, x:t1 |- e : t2
       G |- t1 : TYPE r1
       G |- t2 : TYPE r2
       ------------------------------------ (TyTyLam)
       G |- \ x:t1 . e : (->) @r1 @r2 t1 t2

       G, x:t1 |- e : t2
       G |- t1 : Constraint
       G |- t2 : TYPE r
       ------------------------------------ (CtTyLam)
       G |- \ x:t1 . e : (=>) @r t1 t2

       G, x:t1 |- e : t2
       G |- t1 : Constraint
       G |- t2 : Constraint
       ------------------------------------ (CtCtLam)
       G |- \ x:t1 . e : (==>) t1 t2

       G, x:t1 |- e : t2
       G |- t1 : TYPE r
       G |- t2 : Constraint
       ------------------------------------ (TyCtLam)
       G |- \ x:t1 . e : (-=>) @r t1 t2

   These rules will have to be accommodated in ``exprType`` and in Core Lint.
   Note that we do *not* need any additional annotation on lambdas (the ``Lam``
   constructor) to make this work, because it's always possible to get the
   kinds of the types involved.

4. We similarly need more rules for Core expression applications (only Core, not Haskell)::

       G |- e1 : t1 -> t2
       G |- e2 : t1
       -------------- (TyTyApp)
       G |- e1 e2 : t2

       G |- e1 : t1 => t2
       G |- e2 : t1
       -------------- (CtTyApp)
       G |- e1 e2 : t2

       G |- e1 : t1 ==> t2
       G |- e2 : t1
       -------------- (CtCtApp)
       G |- e1 e2 : t2

       G |- e1 : t1 -=> t2
       G |- e2 : t1
       -------------- (TyCtApp)
       G |- e1 e2 : t2

   These changes shouldn't affect ``exprType`` but will affect Lint.

5. We need to allow term variables whose type has kind constraint (in addition to a
   similar rule about ``TYPE r``)::

       G ok
       x # G
       G |- t : Constraint
       ------------- (CtVar)
       |- G, x:t ok

   This last change will affect Lint, but not ``exprType``.

6. We have to generalize slightly the regularity lemma:

   **Lemma (Regularity).** If ``G |- x : t``, then ``G |- t : TYPE r`` for some ``r`` or
   ``G |- t : Constraint``.

   This change to the regularity lemma affects functions like ``classifiesTypeWithValues``
   and maybe elsewhere in GHC.


7. The old rules for ``NthCo`` no longer work to decompose arrows in the push rules.
   The key question here is how to reduce ``(e1 |> co) e2``. Suppose ``e1 :: t => t2``
   with
   ``t :: Constraint`` but ``e2 :: (t' :: Type)``. This can really happen, when dealing
   with newtype-classes (classes with only one method). In this case,
   ``co :: (t => t2) ~R (t' -> t2)``. (It's representational because all coercions in ``|>``
   constructs are representational, and we're keeping ``t2`` the same on both sides for
   simplicity.) To make progress, we need to
   rewrite this expression to ``e1 (e2 |> co') |> co''``. This has been done for ages,
   but we need some way of building ``co'`` and ``co''`` from ``co``. We can see that
   ``co' :: t' ~R t``. But to get this from ``co``, we need to decompose ``co``.
   Historically, this has been done with ``NthCo``, which has the following (unchanged)
   rule::

       G |- co : T t1 .. tn ~ T s1 .. sn
       ---------------------------------- (NthCo)
       G |- NthCo i co : ti ~ si

   The real rule is a bit more complicated
   (see the `core-spec <https://github.com/ghc/ghc/blob/master/docs/core-spec/core-spec.pdf>`_
   for the gory details), but this is the essence. Note that the ``T``\s in the premise
   are the *same*. So, we can't use ``NthCo`` to decompose our ``co`` from above.

   Instead, we need this new beast::

       G |- co : arrow1 t1 t2 ~R arrow2 s1 s2
       isArrowTy arrow1
       isArrowTy arrow2
       -------------------------------- (ArrowNthCo)
       G |- ArrowNthCo i co : ti ~R si


   where ::

       ----------------------- (ArrowTyTy)
       isArrowTy ((->) r1 r2)

       ----------------------- (ArrowCtTy)
       isArrowTy ((=>) r)

       ----------------------- (ArrowCtCt)
       isArrowTy (==>)

       ----------------------- (ArrowTyCt)
       isArrowTy ((-=>) r)

   That works nicely. This differs from ``NthCo`` in two ways:

   1. It allows different tycons on the two sides of ``co``\'s kind.

   2. It ignores ``RuntimeRep`` arguments when counting. This is important, because
      otherwise, it would be impossible to relate ``t`` and ``t'`` in ``(->) r1 r2 t t2``
      and ``(=>) r t' t2``.

   The push rules (as implemented in the simplifier) will need to create these new
   ``ArrowNthCo``\s.

8. Currently, GHC has ``KindCo``, with this rule::

       G |- co : (t1 : k1) ~r (t2 : k2)
       -------------------------------- (KindCo)
       G |- KindCo co : k1 ~N k2

   Note that ``co`` can have any role, but the output role is nominal. This nominal
   output role is due to the fact that the coercion in ``ty |> co`` is always nominal
   (i.e., no roles in kinds). However, such a rule is disastrous if we have ``(=>) ~R (->)``
   and similar. (It's also disastrous with newtype-classes.) So, we weaken it to ::

       G |- co : (t1 : k1) ~N (t2 : k2)
       -------------------------------- (KindCo)
       G |- KindCo co : k1 ~N k2

   The only difference is the nominal requirement on ``co``. There is discussion below
   as to why this change shouldn't affect anyone except type theorists.

9. The constraint solver must be taught to be aware of the representational
   equalities among the different arrows. This will happen at the same time as newtype-unwrapping
   during canonicalizing representational equality constraints.

Effect and Interactions
-----------------------

This change should have no effect on 99% of Haskell code out there. It's mostly an internal
reorganization, affecting only power users and type theorists. See the motivation for examples
of where this comes up.

Speaking of type theory: There is no proof that the new system is consistent. I believe
strongly that it is, but I have not proved it. I believe this because the new arrows really
*are* representationally equal, in that they have the same runtime representation (a closure).
And the arrows really are injective w.r.t. representational equality in their arguments
and results. Thus, the new ``ArrowNthCo`` coercion seems to be safe. Weakening ``KindCo``
can't destroy consistency, as it's making a coercion *weaker*. My tiny argument in this
paragraph is nowhere near a proof, which is left as an exercise for the reader.

One likely non-effect is the weakening of ``KindCo``. This makes Core a tad bit less
expressive, but I don't think anyone can write Haskell code that needs this corner of
Core expressiveness.  In order to see the lost expressiveness, you would need to have
a heterogeneous representational coercion. The user-accessible ``Coercible`` class is
*homogeneous*, so creating
one seems impossible in user code.
(GHC certainly could internally. But it doesn't.) So we should be OK here.

Another non-effect is that this version of this proposal is fully compatible with
the generalized kind of ``(->)``. Earlier versions of this proposal were not
(see `this comment <https://phabricator.haskell.org/D2038?id=10783#inline-25457>`_).
Essentially, we could not weaken ``KindCo`` without destroying the type system.
In this version, because the arrows are different tycons, the subtle interplay
of features that caused problems previously doesn't arise. (Essentially, the new
``ArrowNthCo`` fills the gap left by the missing functionality of ``KindCo``. It's
a long story.)

A happy consequence of this proposal is that, I believe, the ``reflection`` library
will no longer have to use ``unsafeCoerce`` to get from ``C a => b`` to ``a -> b``.
The only missing step is to teach the solver to reduce ``C a`` to ``a`` (when
we have ``class C a where meth :: a``). That's not part of this proposal, but it
would be very easy to do once this proposal is fully implemented.

Costs and Drawbacks
-------------------
This is both a simplification and a complication to the type system.

It's a simplification in that GHC will no longer have to maintain a separate ``tcEqType``
(which says that ``Constraint`` and ``Type`` are distinct) from ``eqType`` (which considers
them the same). There are knock-on effects, too, like no longer needing a separate
``coreView`` and ``tcView``.

It's a complication in that we have to add a lot of baggage to pull this off. This
is a fairly steep cost, when viewed in its entirety, above. But we trade a hacky, wonky
approach for a more principled one.

Alternatives
------------

1. Two earlier versions of this proposal argued for tinkering with ``TYPE`` and ``RuntimeRep``, either adding
   a new parameter to ``TYPE`` (representing constraintiness) or a new constructor to ``RuntimeRep`` (``ConstraintRep``).
   These were more subtle (in my opinion) than the current proposal (which is straightforward, if a bit heavy).
   They also had the disadvantage of allowing polymorphism where no one was asking for it -- tinkering with
   ``TYPE`` and ``RuntimeRep`` is good if we want constraintiness-polymorphism, but no one does. Those proposals
   required us to restrict the polymorphism, anyway. Those earlier proposals also were incompatible with
   newtype-classes, a problem this one sidesteps.

2. @int-index has argued very cogently and patiently for an alternative solution, whereby we allow ``Constraint ~ Type``
   in Haskell code, resolving the discrepancy between Haskell and Core in the opposite direction. This idea
   was originally proposed by Simon PJ `here <https://gitlab.haskell.org/ghc/ghc/issues/11715#note_117916>`_, but he
   has since changed his mind on the idea. It's hard to summarize @int-index's arguments here beyond Simon's original
   proposal, but they are worthwhile reading if you're keen. The main drawbacks to the
   alternative proposal might be written by Edward Kmett `here <https://gitlab.haskell.org/ghc/ghc/issues/11715#note_123424>`_.
   I confess I have not liked this idea much, but it's more from a language-design standpoint than from a type-safety
   standpoint (the alternative proposal appears type-safe to me). (@int-index has since backed off this point of view,
   as seen on the pull request)

3. Some potential future will allow roles in kinds. This is in contrast to today, where all kind casts ``(ty |> co)`` use
   a *nominal* coercion. (This is also in contrast to term-level casts ``(exp |> co)`` which use *representational*
   coercions.) @sweirich and collaborators are working on the theory behind this currently. Once this theory is complete,
   it seems we could introduce ``Constraint`` and have an axiom saying ``Constraint ~R Type``. Here, "representation"
   is fairly meaningless, but here is the intuition: nominal equalities should be inferred by GHC. That is, Haskell types
   that are nominally equal are considered interchangeable in a Haskell program. On the other hand, representational
   equalities are never inferred; a programmer must include some annotation saying where to use them. Currently, these
   annotations take the form of ``coerce``, a newtype constructor, or a newtype pattern-match. But it would also make sense
   to have ``(=>)`` be an "annotation" saying to cast a ``Constraint`` into a ``Type`` usable by ``(->)``. If it weren't
   for the fact that the theory isn't ready yet, this would seem to be the most appealing option.

Unresolved questions
--------------------

1. Is ``ArrowNthCo`` necessary. At one point, Simon PJ thought we could mimic its behavior by using transitivity
   and ``NthCo``. I initially agreed, but upon reflection have changed my mind. Here is the case at hand:

   From ``co :: (t1 => t2) ~R (t3 -> t4)`` (where ``t1 :: Constraint`` and ``t2, t3, t4 :: Type``), we need to
   derive ``co' :: t1 ~R t3``. Simon's suggestion was to build this as a first step: ``co0 = (sym axCtTy) <t1> <t2>``,
   where ``axCtTy`` is the axiom proving that ``(=>) ~R (->)`` (let's ignore ``RuntimeRep`` arguments here; they're
   not the problem). Thus, ``co0 :: (t1 -> t2) ~R (t1 => t2)`` and ``co0 ;; co`` (where ``;;`` denotes transitivity)
   would prove ``(t1 -> t2) ~R (t3 -> t4)``. Now, just proceed using a standard ``NthCo``.

   This process went wrong with the construction of ``co0``: it's not well-typed. Specifically, ``t1 -> t2`` is
   ill-kinded, because ``t1 :: Constraint``. You might think that we can just cast ``t1`` to have kind ``Type``,
   but we certainly don't have a coercion that proves ``Constraint ~N Type`` (we need a *nominal* coercion to
   cast types), so we're a bit dead in the water. So I don't think this is possible and that we need ``ArrowNthCo``.
   But perhaps I'm missing something.

2. Is this whole idea type safe? I don't know for sure. The challenge has to do with the interaction between roles and
   kind coercions, something yet to be studied in the literature. (My thesis cleverly avoids broaching the subject.)
   When I hesitated on this point in a recent interaction with Simon, he rightly pointed out that we don't have
   a proof for the status quo, so this new proposal doesn't make things any worse. My future hopefully holds
   a mechanized proof of this all, but let's not wait for that future to arrive before making progress here.

Implementation Plan
-------------------

Some implementation thoughts:

1. The existing ``FunTy`` constructor of ``Type`` will be used to represent
   all four saturated arrow constructors, just like it works now to
   represented a saturated ``(->)``. When decomposing (in, say, ``splitTyConApp``),
   GHC will have to check the kinds of the arguments to determine the right
   ``TyCon`` (and, perhaps, ``RuntimeRep`` arguments) to produce.

2. The existing ``FunCo`` constructor of ``Coercion`` will be used to represent
   coercions involving any of the four arrows. It's even possible that a ``FunCo``
   will relate two different arrows. For example, if we have a newtype-class leading
   to ``axC : C a ~R a`` (where ``C a :: Constraint``), then we can build
   ``FunCo Representational axC <a> :: (C a => a) ~R (a -> a)``. This use of ``FunCo``
   overlaps with the new axioms relating the arrow types, but that's OK; it's a
   representation optimization. At one point, I was worried that this cross-arrow
   ``FunCo`` would be problematic at a nominal role, but such a thing is impossible
   to build, because we will never have ``ty1 ~N ty2`` where ``ty1 :: Type`` and
   ``ty2 :: Constraint``. (At least, we won't if we're typesafe!)

Old ``Typeable`` motivators
---------------------------

2. Printing::

      main = do
        print $ typeRep (Proxy :: Proxy Eq)
        print $ typeOf (Proxy :: Proxy Eq)

   This prints ::

      Eq
      Proxy (* -> *) Eq

   But of course ``Eq`` doesn't have kind ``* -> *``. It has kind ``* -> Constraint``! Except that Core can't tell the difference.


3. Order sensitivity::

      main = do
        print $ typeOf (Proxy :: Proxy (Eq Int))
        print $ typeOf (Proxy :: Proxy Eq)

   prints ::

      Proxy Constraint (Eq Int)
      Proxy (Constraint -> Constraint) Eq

   but if you print them in the opposite order, you get ::

      Proxy (* -> *) Eq
      Proxy * (Eq Int)

   Ew.

