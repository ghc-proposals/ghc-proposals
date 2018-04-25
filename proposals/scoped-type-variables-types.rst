Allow ScopedTypeVariables to refer to types
===========================================

.. proposal-number::
.. trac-ticket:: [#15050](https://ghc.haskell.org/trac/ghc/ticket/15050)
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

The ``ScopedTypeVariables`` extension has a restriction that type variables in patterns can only refer to type variables, not
full types. This proposal lifts this restriction.


Motivation
------------

Consider this code, with ``ScopedTypeVariables`` enabled. Can you tell which lines of ``foo`` typecheck::

    P a = P
    data T1 a where
      MkT1 :: forall a.              P a               -> T1 a
      MkT2 :: forall a.              P a               -> T1 (a,a)
      MkT3 :: forall a b. b ~ Int => P a -> P b        -> T1 a
      MkT4 :: forall a b.            P a -> P b        -> T1 a
      MkT5 :: forall a b c. b ~ c => P a -> P b -> P c -> T1 a

    foo :: Int -> T1 (Int, Int) -> ()
    foo 1 (MkT1 (P::P (Int,Int)))    = ()
    foo 2 (MkT1 (P::P (Int,x)))      = (() :: x ~ Int => ())
    foo 3 (MkT1 (P::P x))            = (() :: x ~ (Int,Int) => ())
    foo 4 (MkT2 (P::P x))            = (() :: x ~ Int => ())
    foo 5 (MkT3 P (P::P Int))        = ()
    foo 6 (MkT4 P (P::P b))          = ()
    foo 7 (MkT5 P (P::P b) (P::P b)) = ()
    
All lines but line 2 and 3 typecheck. The reason is an restriction in ``ScopedTypeVariables``. The documentation says
  
  When a pattern type signature binds a type variable in this way, GHC insists that the type variable is bound to a rigid, or fully-known, type variable. This means that any user-written type signature always stands for a completely known type.
  
I found this rule always a bit opaque and confusing. Which type variables are there to be bound to? (It turns out, it’s the
type variables mentioned in the type of the data constructor, for example.) Only recently, and helped by some reading
of the type checker code, did I gain a more complete grasp of this restriction.

Simon explains the motivation behind this restriction:

   I agree this is a questionable choice. At the time I was worried that it'd be confusing to have a type variable that was just an alias for ``Int``; that is not a type variable at all. But in these days of GADTs and type equalities we are all used to that. We'd make a different choice today. 

So let’s do this!


Proposed Change Specification
-----------------------------
The above sentence in the documentation for ``ScopedTypeVariables`` is repaced with

  When a pattern type signature binds a type variable in this way, GHC insists that the type variable is bound to a rigid, or fully-known, type. This means that any user-written type signature always stands for a completely known type.

(a one-word-deletion!)

No separate pragma is needed for this behaviour, as we are expanding the set of programs accepted by ``ScopedTypeVariables``, but do not change any behaviour with regard to Haskell2010.

Effect and Interactions
-----------------------
With the restriction lifted, all lines of the function above typecheck.

Proposal #126 has the same restriction for type applications in patterns. If we adopt this proposal, then the restriction
ought to also be lifted for that feature.


Costs and Drawbacks
-------------------
Development is small; there is a working prototype in ``wip/T15050`` – after all, we are removing a check! The code that implements this check (which is spread in many places) unfortunately cannot be just removed, as the restriction still applies to kind variables in data type definitions.

Alternatives
------------
If scoped type variables bind only variables, then we should rename  ``ScopedTypeVariables`` to ``ScopedTypeVariablesVariables``.

Doing nothing is an option.

Unresolved questions
--------------------
none yet

Implementation Plan
-------------------
Brush up ``wip/T15050`` and be done with.
