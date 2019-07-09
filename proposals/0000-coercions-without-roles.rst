Coercions without Roles
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

Roles effectively dictate that the coercion language is first order.
In a language with higher kinds, this means that we are missing coercions.
The "higher order role" proposal fundamentally doesn't change this.
It instead allows restricting the domain of type constructors so that the still-first-order assumptions can be relaxed.
This proposal instead piggybacks on quantified constraints to dispense with roles entirely.

Motivation
------------
Give a strong reason for why the community needs this change. Describe the use case as clearly as possible and give an example. Explain how the status quo is insufficient or not ideal.


Proposed Change Specification
-----------------------------

Core Coercions
~~~~~~~~~~~~~~~~

#. The language of constraints propositions is considered higher-order.
   This is not an implementation change because GHC already lumps everything under ``Type``, and can already be tricked with ``unsafeCoerce`` into inhabiting these today.
   ::
     phi ::= t0 ~_r t1 -- old
          -- new
          | forall (a : k)*. phi
          | '(' phi, phi (, phi)* ')'
          | phi => phi

#. Instead of roles, we associate axioms with regular (non-newtype) data types.
   newtypes and type families also create axioms, though we must keep these axioms separate as only they are eligible for ``nthCo``.
   The axiom is admissible under these conditions.

   Shape
     It must be in the form ``forall a* b*. phi => H a* ~_R H b*``.
     In other words it, has head of representational equality constraint of same type constructor applied to some variable arguments on each side.

   Saturation
     The ``H`` must be saturated on both sides.
     Likewise, one cannot just skip assigning a role today.

   Soundness
     If one looks at the type of each field under the two applications, they will accumulate per-field ``f_n ~_R f'_n`` constraints.
     The axiom's premise must be sufficient to derive all of those constraints.

   Not-too-incompleteness
     The axiom can't be stricter than all nominal roles today.
     Formally:
     ::
       (forall a* b*. (a ~_N b)* => H a* ~_R H b*) => C

     This allows us to continue with roughly the current treatment of unsaturated ``H`` in the typing rules.

#. ``coTyConApp``, the first typing judgement involving roles that must be rewritten
   ::
     C : forall a* b*. phi => H a* ~_R H b*
     |- G
     (G |- t : k)*
     G (r : k)* |- c : phi[t* / a*][r* / a*][s* / b*][r* / b*]
     -------------------------------------------------------------------------------- Co_TyConAp'
     G |- C(t*, s*) : H t* ~_R H s*

   ``r`` is the remaining arguments to saturate the axiom.
   ``r*`` appears twice because we are applying the same fresh vars twice for each side.
   Because of _Not-too-incompleteness_ these duplicated abstract types are never "to blame" if the premises cannot be derived.
   The user-supplied arguments are to be blamed instead.

#. ``coNth``, the second such judgement is replaced with ``coPremise``.
   The name is changed because we no longer take a parameter index, but just get the whole premise back in one lump.
   ::
     C : forall a* b*. phi => H a* ~_R H b*
     G |- c : H t* ~_R H s*
     H is not a newtype
     -------------------------------------------------------------------------------- Co_Premise
     G |- c : forall r*. phi[t* / a*][r* / a*][s* / b*][r* / b*]

Surface language
~~~~~~~~~~~~~~~~

#. We steal the ``deriving instance`` syntax for ``Coercible``:
   ::
     data T ... = ...

     deriving instance ... => Coercible (T ...) (T ...)

   These "instaces" are subject to analogous restrictions as the core axioms.

#. Absent an explicit "instance", we derive the first order axiom we affectively do today.
   ::
     data T ... = ...
       deriving (Coercible)

   gives a warning because we derive ``Coercible`` by default, and this cannot be prevented.
   (Or else ``t ~_N s => t ~_R s`` would not be true.)

#. Explicit role applications are deprecated.

Effect and Interactions
-----------------------

Detail how the proposed change addresses the original problem raised in the motivation.

Discuss possibly contentious interactions with existing language or compiler features.

Proof Sketch
~~~~~~~~~~~~

``Co_TyConAp'`` and ``Co_Premise`` are nearly dual.
The key difference is the ``C(t*, s*)`` use of the axiom in the former, gets replaced with an abstract constraint ``c`` in the latter.
This is OK because we can rewrite any closed term with the correct type into one where the Axiom is at it's head!
All the other coercersion type formers distribute over the axiom application, so we can always push the other formers deeper, and pull axiom applications to the front.
\[One can think of the preexisting prohibition against newtypes in terms of this two. The rewrite is not possible if ``H`` is a newtype.]

I should rigorously show that this is true, but for now intution

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
The idea then is appealing to coercions on the underlying type never lead to anything more premissive because the axioms are "lossless".
That means by the same proof technique the axiom application can always be pulled to the head, so ``Co_Premise`` stays the duel of ``Co_TyConAp``.
This however is a big breaking change.

Unresolved Questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and specification. Be upfront and trust that the community will help. Please do not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to the steering committee.


Implementation Plan
-------------------

I think I can implement this.
However I've started a bunch of project mucking around with GHC lately and I don't want to bite off more than I can chew and get none of them done.
