Coercions without Type Constructor Roles
==============

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

The rules for coercions for parameterized data types currently are inflexible.
Assigning roles to type parameters has all the same limitations as restricting instances to ``Haskell98``-allowed.
This proposal allows writing the rules using standalone deriving syntax, and piggybacking on extensions effecting instances to limit what coercible pseudo-instanes are allowed.

Motivation
------------
Give a strong reason for why the community needs this change. Describe the use case as clearly as possible and give an example. Explain how the status quo is insufficient or not ideal.

Types are erased. Decomposing applied type constructors to that on the pameraters is on the face of it silly be cause we are tying to push coercions up through erased applications, when coercibility is the preimage of equality post erasure.a

GADTS
~~~~~

Given a definition like::

  data Foo a b (c :: Bool) where
    A :: Foo a   () True
    B :: Foo ()  b  False

We want

::
  Coercible (Foo l  () True)  (Foo r () True)
  Coercible (Foo () l  False) (Foo () r False)

Neither of the first two variables is absolutely phantom, but given enough about the others, the conditional phantom reveals itself.

We will call these parameters "werewolf" instead "phantom" because they are only supernatural sometimes.

Proposed Change Specification
-----------------------------

Formalism follow [paper16]_ unless otherwise noted.

Core language
~~~~~~~~~~~~~

No more ``nth`` for data
^^^^^^^^^^^^^^^^^^^^^^^^

The ``nth`` decomposition rule to derive coercions between parameters from coersions on type constructor application is almost no more.

Instead of decomposition to *paremeters*, we decompose to *fields*. We have the following rules:

::
  Γ ⊢ K ∀(a:κ)*. ∀(b:κ′)*. (σ_l ∼_ρ σ_r)* ⇒ τ → T a*
  Γ ⊢ γ : T υ_l* ~R# T υ_r*
  ------------------------------------------------ CO_liftNthFieldTo
  Γ (γ' : σ_l[υ_l*/a*] ∼_ρ σ_r[υ_l*/a*])*
    ⊢ liftNthField^n K co σ1* σ1* : ∀b* : κ′. τ^n[υ_r*/a*] ~_R τ^n[υ_r*/a*]

::
  Γ ⊢ K ∀(a:κ)*. ∀(b:κ′)*. (σ_l ∼_ρ σ_r)* ⇒ τ → T a*
  Γ ⊢ γ : T υ_l* ~R# T υ_r*
  ------------------------------------------------ CO_liftNthFieldFrom
  Γ (γ' : σ_l[υ_r*/a*] ∼_ρ σ_r[υ_r*/a*])*
    ⊢ liftNthField^n K co σ1* σ1* : ∀b* : κ′. τ^n[υ_l*/a*] ~_R τ^n[υ_l*/a*]

::
  Γ ⊢ K ∀(a:κ)*. ∀(b:κ′)*. (σ_l ∼_ρ σ_r)* ⇒ τ → T a*
  Γ ⊢ γ : T υ_l* ~R# T υ_r* a
  ------------------------------------------------ CO_liftNthCoercionTo
  Γ (γ' : σ_l[υ_l*/a*] ∼_ρ σ_r[υ_l*/a*])*
    ⊢ liftNthField^n K co σ1* σ1* : ∀(b*:κ′)*. (σ_l[υ_r*/a*] ∼_ρ σ_r[υ_r*/a*])*

::
  Γ ⊢ K ∀(a:κ)*. ∀(b:κ′)*. (σ_l ∼_ρ σ_r)* ⇒ τ → T a*
  Γ ⊢ γ : T υ_l* ~R# T υ_r* a
  ------------------------------------------------ CO_liftNthCoercionFrom
  Γ (γ' : σ_l[υ_r*/a*] ∼_ρ σ_r[υ_r*/a*])*
    ⊢ liftNthField^n K co σ1* σ1* : ∀(b:κ′)*. (σ_l[υ_l*/a*] ∼_ρ σ_r[υ_l*/a*])*

The idea is that given a coercion between (non-newtype) data types, and a constructor for that new type, one can push that coerion into the coercions for each regular field and coercion field.
Note that the existential type parameters are not substituted, so we can't conclude spurious things about them.
The ``n`` superscript indicates which coercion or field we wish to project the coercion for.

For types types like ``(=>)`` and ``((->))``, ``nth`` remains, because those are not (generalized) abstract data types.

TODO ``Co_TyConApp``. which is now exploded into a family of rules whose only constraint is that the ``liftNth*`` rules are admissible given the ``Co_TyConApp``-replaced axioms.

Surface language
~~~~~~~~~~~~~~~~

#. We steal the ``deriving instance`` syntax for ``Coercible``::

     data T ... = ...

     deriving instance ... => Coercible (T ...) (T ...)

   There are no fields of Coercible, but instead the compiler checks that all the projections from the ``liftNth`` rules are satisfied from the instance constraints.

#. These pseduo-isnstances are subject to the rules of normal instance heads and contexts, and relaxed by:

   - ``FlexibleInstances``
   - ``FlexibleContexts``
   - ``UndecidableInstances``
   - ``QuantifiedConstraints``

   But not ``MultiParmTypeClasses``.

#. Absent an explicit "instance", we derive the first order axiom we effectively do today.
   ::
     data T ... = ...
       deriving (Coercible)

   gives a warning because we derive ``Coercible`` by default, and this cannot be prevented.
   (Or else ``sub (γ : t ~_N s) : t ~_R s`` would not be admissible.)

#. Explicit role applications are deprecated.

Effect and Interactions
-----------------------

Detail how the proposed change addresses the original problem raised in the motivation.

Discuss possibly contentious interactions with existing language or compiler features.

Proof Sketch
~~~~~~~~~~~~

Core language
~~~~~~~~~~~~~

In ``[paper16]``, ``nth`` is only used for the push rules.
This is because we only need to decompose coercions when we push them deeper in expressions.
The paper uses ``nth`` to project out a coercion on type parameters, but then uses the lifting meta-function / lemma to rebuild the constraint for each field.
With the formalisms in this proposal, all the lifting + "nth"-ing in the proof steps involving ``S_KPUSH`` instead use these new rules.
I hope this is a fairly wrote transformation of those proofs".

When a given constructor is not in scope, the relevant ``LiftNth*`` usages become impossible, but this is fine because we also won't to use ``S_KPUSH`` with that constructor.
In core we less often don't think about constructors as going out of scope, but this property becomes important for the surface language where we wish to contextually allow or disallow the use of sketchy coercions.

There is another difference in that the coercions of the data constructor are put into the context for the conclusion.
The point of this is subtle.
In the paper, to "coerce the coersions", it does::

  sym (σ[nth η/a]ρ) ; γ ; σ[nth η/a]ρ

This is fine, but doesn't allow us to take advantage of ``γ`` being absurd.
By extending the context, we take a more "black box" approach which, at the very least, allows ``contra`` to be used when ``γ`` is absurd.
This is useful to discharge obligations on impossible variants in GADTs, which is what makes our "werewolf param" example in the motivation work.

The cost to the above is that we need twice as many rules, as we have to be able to start with the coercisons of either constructor application, and derive the other.
This is stupid as the proofs "should" be basically identical in both directions, but I do not want to invent a calculus of bi-implications and drift further from the paper.
So, 2x blow up for now.

The new ``Co_TyConApp`` family is never weaker than the old ``Co_TyConApp`` with inferred roles, so we can be sure that any proof step involving it is still valid.
The old lift axiom in fact describes exactly how to transform premises for the old ``Co_TyConApp`` invocation into premises for the new ``Co_TyConApp`` family invocations.

Surface language
~~~~~~~~~~~~~~~~

First of all, note that this proposal does *not* introduce "quantified coersions" / "high-order coersions" / internal implication to the constraint language.
When we write::

  deriving Coercible Sigma => Coercible Tau

this refers to boxed ``Coercible`` not magic ``~#R``.

Because the obligations of the instances are the results of the ``LiftNth*`` rules, rather than the end ``~#R`` superclass, we ensure the instances are in a sense productive.
This ensures that the coinduction the constraint solver does is actually valid, even in the case of undecidable instances.
For example::

  instance Coercible (X a) => Coercible (X a)

looks very dangerous, but is in fact the most natural, if not minimal, instance for::

  newtype X a = X (X a)

Even

Costs and Drawbacks
-------------------

The biggest drawback is error messages.
The roles are a pleasant special case, but without them we will be back to undifferentiating types class resolution errors, and blaming the whole axiom.


Alternatives
------------

Earlier proposals had pseudo-classes to speak of the roles of higher-kinded type parameters, so we still had one rich-role per argument.
This seems overwrought (too many pseudo-classes), and _still_ is less expressive:
The higher-kinded roles speak to the entire domain, rather than the types we actually apply the type constructor to.
This is similar to the issues with ``Foo1`` classes.

If we required the axioms be complete, we could get rid of the no newtype restriction for ``Co_Premise``.
The reason for the restriction against ``coNth`` against newtypes is that the unwrapping axioms can generate more ``H ... ~ H ...`` coercions than ``Co_TyConAp`` alone.
This is unavoidable with the old system: type constructor roles just aren't expressive enough.
But with the new system we can require that newtype's new axioms generate all valid ``H ... ~ H ...`` coercions.
The new axioms are thus "lossless", since they can generate any coersion in the right form that the unwrapping ones can.
That means by the same proof technique the axiom application can always be pulled to the head, so ``Co_Premise`` stays the duel of ``Co_TyConAp``.
This however is a big breaking change.

Unresolved Questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and specification. Be upfront and trust that the community will help. Please do not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to the steering committee.


Implementation Plan
-------------------

No idea yet.

.. [paper16] "Safe zero-cost coercions for Haskell"
  https://repository.brynmawr.edu/cgi/viewcontent.cgi?article=1010&context=compsci_pubs
