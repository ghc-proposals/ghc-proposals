Allow ScopedTypeVariables to refer to types
===========================================

.. author:: Joachim Breitner
.. date-accepted:: 2018-08-04
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/15050
.. implemented:: 8.8
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/128>`_.
.. contents::

The ``ScopedTypeVariables`` extension has a restriction that type variables in patterns can only refer to type variables, not
full types. This proposal lifts this restriction.


Motivation
------------

``-XScopedTypeVariables`` allows us to write code like this::

    f :: Maybe a -> Int
    f (Just (x :: b)) = <body>

This is clearly fine.  ``b`` scopes over ``<body>``.   The variable `a` is not in scope at all (no explicit forall). We can also write::

    f :: forall a. Maybe a -> Int
    f (Just (x :: b)) = <body>

Now both ``a`` and ``b`` are in scope in ``<body>``, as aliases. Great. So if that all works, isn't it strange to reject this::

    f :: Maybe Int -> Int
    f (Just (x :: a)) = <body>

If this was allowed ``a`` would have to be an alias for ``Int``.  Currently that is not allowed: a lexically scoped type variable can only be bound to a type *variable*.  The whole and sole point of the proposal is to lift that restriction.

The restriction is documented as follows:

  When a pattern type signature binds a type variable in this way, GHC insists that the type variable is bound to a rigid, or fully-known, type variable. This means that any user-written type signature always stands for a completely known type.

Simon explains the motivation behind this restriction:

   I agree this is a questionable choice. At the time I was worried that it'd be confusing to have a type variable that was just an alias for ``Int``; that is not a type variable at all. But in these days of GADTs and type equalities we are all used to that. We'd make a different choice today.

Let’s do this!


One reason for making the change is that it's not really clear what being "bound to a type variable" means in the presence of type equalities.  For example::

    f1 :: (a ~ Int) => Maybe a -> Int
    f1 (Just (x :: b)) = <body>

    f2 :: (a ~ Int) => Maybe Int -> Int
    f2 (Just (x :: a)) = <body>

Which of these should be accepted under the current rules?   (SPJ says: “I don't even know; I'd have to try it.”)  An advantage of the proposal is that such questions become irrelevant.

Here are more type-checking puzzles. Can you tell which lines of ``foo`` typecheck::

    data P a = P
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

All lines but line 2 and 3 typecheck, but arguably all could.


Another motivation for this proposal is to use ``ScopedTypeVariables`` as abbreviations for long types::

    f :: ReallyReallyReallyReallyLongTypeName -> T
    f (x :: a) = … (read "" :: a) …
    -- Instead of f x = … (read "" :: ReallyReallyReallyReallyLongTypeName) …


Proposed Change Specification
-----------------------------
The sentence

  When a pattern type signature binds a type variable in this way, GHC insists that the type variable is bound to a rigid, or fully-known, type variable. This means that any user-written type signature always stands for a completely known type.

in the documentation for ``ScopedTypeVariables`` is removed.

No separate pragma is needed for this behaviour, as we are expanding the set of programs accepted by ``ScopedTypeVariables``, but do not change any behaviour with regard to Haskell2010.

Effect and Interactions
-----------------------
With the restriction lifted, all lines of the function above typecheck.

Proposal `#126 <https://github.com/ghc-proposals/ghc-proposals/pull/126>`_ has the same restriction for type applications in patterns. If we adopt this proposal, then the restriction
ought to also be lifted for that feature.

At the moment, a type variable may occur multiple times in multiple pattern signatures in the same pattern. These do not shadow each other, but rather refer to the same type. For example::

  foo1 :: Int -> Bool -> ()
  foo1 (_ :: a) (_ :: a) = () -- Type error, because a can not be both int and bool

  foo2 :: Int -> Int -> ()
  foo2 (_ :: a) (_ :: a) = () -- Ok, binds a to Int

This behaviour is unchanged by the current proposal.

The paper `Type variables in patterns <https://arxiv.org/abs/1806.03476>`_ (Haskell'18) has typing rules that describe this the proposed behaviour.

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
