Strengthen newtype decomposition
================================

.. author:: Simon Peyton Jones
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/549>`_.
.. sectnum::
.. contents::

This proposal would strengthen the CO-NTH rule for coercions, thereby making type
inference succeed more often.

Motivation
----------

The problem
:::::::::::

Consider::

  type family F a
  newtype T x = MkT (F x)

and suppose we typecheck the function::

  f :: T Int -> T Bool
  f x = coerce x

Then we end up trying to prove ``[W] (T Int) ~R (T Bool)``.  If ``MkT`` is in scope, it's easy: just
unwrap the newtype.  But if not, GHC today will decomposing this to
``[W] Int ~R Bool``, and complain that it can't prove that::

    Foo.hs:12:7: error: [GHC-18872]
        • Couldn't match type ‘Int’ with ‘Bool’
            arising from a use of ‘coerce’
        • In the expression: coerce x
          In an equation for ‘f’: f x = coerce x
       |
    12 | f x = coerce x
       |       ^^^^^^

But it's totally wrong to claim that
we need ``Int ~R Bool``, because of the type family.  (E.g. suppose ``F Int = Char`` and ``F Bool = Char``.)

So the error message is bogus.  In more complicated situations we might unify type variables
that should not be unified, causing more misleading, far-away errors.

In short,

* the decomposition step for newtypes is always *sound* (if typechecking succeeds, all is well),
* but it is not always *complete* (typechecking may fail when it shouldn't).

So you may say "do not decompose newtype wanteds".  But then suppose
we are trying to prove ``[W] (IO Int) ~R (IO Age)``, where::

   newtype Age = MkAge Int
   newtype IO a = MkIO (State -> (State,a))

and ``MkAge`` is in scope, but ``MkIO`` is not (being rightly abstract).
If we do not decompose newtypes, GHC would be totally stuck, which is not good for the programmer.

So GHC has a weird special case, where it says "as a last resort, decompose the newtype unless there are
in-scope Given equalities".  The "unless" part is an ad-hoc heuristic,
intended to make incompleteness harder to trigger.
(Interested parties can look at
``can_decompose`` in ``GHC.Tc.Solver.Canonical.canTyConApp``, and the
discussion on `this merge request <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/9282>`_.

But even with the horrid special case, we still get the bogus ``Int ~ Bool`` error message.


The CO-NTH rule
:::::::::::::::

For a newtype ``T``, given ``[W] (T t1) ~R (T t2)``
can we ever decompose to ``[W] t1 ~R t2``, without losing completeness?

The relevant paper is `Safe zero-cost coercions for Haskell (journal version) <https://simon.peytonjones.org/safe-coercions-2016/>`_.
Here's the CO-NTH rule in the paper, Fig 5::

  G |- g : H taus ~R H sigmas    rs is a prefix of roles(H)     H is not a newtype
  ---------------------------------------------------------------------------------------    CO-NTH
     G |- Nth(i) G : taus(i) ~rs(i) sigmas(i)

The CO-NTH rule governs completeness of decomposition for wanted constraints: if CO-NTH holds
for each argument i, it means that nothing is lost by decomposing.

But note "H is not a newtype"; that means **we can never decompose a newtype** except
at risk of incompleteness.
But in the case of the newtype ``IO`` above, it is easy to see that
if we know ``(IO t1) ~R (IO t2)`` then we certainly know ``t1 ~R t2``, and vice versa.
**Nothing is lost by decomposing ``IO``!**  CO-NTH is too conservative.

The proposal below (see Proposed Change Specification) strengthens CO-NTH to allow
this decomposition.



Proposed Change Specification
-----------------------------

There are two proposed changes.

Change 1: CO-NTH Rule
:::::::::::::::::::::

First, change the CO-NTH rule as follows::

  G |- g : H taus ~R H sigmas    rs is a prefix of roles(H)
  H is not a newtype or rs(i) is representational
  ---------------------------------------------------------------------------------------    CO-NTH
     G |- Nth(i) G : taus(i) ~rs(i) sigmas(i)

Note the extra "or rs(i) is representational".  That says that if we know ``(T ty1) ~R (T ty2)``
then we know ``ty1 ~R ty2`` *if ``T``'s argument has representational role*. When is that?
Here's an example::

  type family F
  newtype N a b c = MkN ([a], F c)

  -- Inferred roles    a            b      c
  -- type role N representational phantom nominal

Here
* ``b`` has phantom role: it is not even mentioned in the RHS
* ``c`` has nominal rule: it appears in the RHS but only in the argument of a type family
* ``a`` has reprsentational role: it appears in the RHS, and *not* under a type family

Representational role means "if you know the RHS type, then you know ``a``"

Change 2: user role signatures
::::::::::::::::::::::::::::::

In GHC today, the user can declare explicit roles::

  newtype P a = MkP Int
  type role P representational

Here the argument of ``P`` is not mentioned in its RHS, so its role would be inferres as ``phantom``.  But GHC today allows you to override a phantom role
to ``representational``.

If we continue to allow this, the new CO-NTH rule would be unsound.  Clearly, knowing ``(P t1) ~R (P t2)`` does **not** imply that ``t1 ~R t2``.

Hence change 2: **In a newtype declaration, we do not allow a user to give a representational role for a phantom argument.**  Any such
attempt would simply be rejected.

With that change, CO-NTH is sound.

It is perfectly OK to specify a ``nominal`` role for a ``phantom`` argument, however.  Thus, returning to the example ``N`` in the
"Change 1" section:

* ``type role N representational nominal nominal`` is OK: we have changed ``phantom`` into ``nominal``.
* ``type role N nominal phantom nominal`` is OK: we have changed ``representational`` into ``nominal``.
* ``type role N representational reprsentational nominal`` is BAD: we have changed ``phantom`` into ``representational``.

So far as role ascription is concerned, ``representational`` is the most informative role of the three: it guarantees that the
type variable appears, in at least guaranteed-injective position in the RHS.

Effect and Interactions
-----------------------

With the new rule, we can decompose ``(T s1 s2 s3) ~R (T t1 t2 t3)``,
where ``T`` is a newtype, if *all three parameters are at representational role*.
That strengthens type inference without introducing incompleteness.

Moreover, arguments with representational role are very common: they are arguments that
are mentioned, and not under type families.  The ``IO`` example above is a case in point.


Costs and Drawbacks
-------------------

Implementation is easy.

The only cost is Change 2: you can't give a user-ascribed representational role to a phantom argument.
I hypothesise that this is vanishingly rare, and undesirable anyway.


Alternatives
------------
Do nothing.



Implementation Plan
-------------------

I will implement.

