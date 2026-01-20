Drop support for ``-XNoPolyKinds``
==================================

.. author:: Richard Eisenberg
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/476>`_.
.. contents::

Since version 7.4, GHC has supported *kind polymorphism*, where a definition like ``data App f a = MkApp (f a)`` works
with a variety of kinds. Kind polymorphism is a natural extension of the type
polymorphism Haskell has had since its inception. In the light of experience
with kind polymorphism, a mode where GHC *rejects* kind
polymorphism has become increasingly hard to specify. This proposal thus suggests
to have kind polymorphism be permanently on, much like type polymorphism is
permanently on.

Motivation
----------
Consider the following program::

  data App f a = MkApp (f a)
  data AtInt f = MkAtInt (f Int)

  unApp :: App AtInt Maybe -> Maybe Int
  unApp (MkApp (MkAtInt m)) = m

Should that be accepted?

The key question is: what is ``App``\ 's kind? After analyzing its definition,
we see that ``f :: kappa -> Type`` and ``a :: kappa`` for some kind ``kappa``.
In Haskell98, GHC insists that every datatype have one fixed, monomorphic kind.
Thus GHC must choose how to default ``kappa``, and it chooses ``kappa := Type``,
assigning ``App :: (Type -> Type) -> Type -> Type``. This means that the use
``App AtInt Maybe`` is rejected.

On the other hand, with ``-XPolyKinds``, GHC is allowed to *generalize* the kind
of ``App``, assigning ``App :: forall k. (k -> Type) -> k -> Type``, and the program
above is accepted. This is more general, composition behavior.

Note that, with ``-XNoPolyKinds``, we could force acceptance of the above program,
by defining e.g. ``data App f a = MkApp (f a) | Don'tUseThisConstructor (f Maybe)``.
The extra, never-used ``Don'tUseThisConstructor`` tells GHC to choose ``kappa := Type -> Type``,
but the kind is still monomorphic.

Defaulting ``kappa`` is not so hard. But there are thornier cases. ::

  {-# LANGUAGE PolyKinds #-}
  module A where

  data App f a = MkApp (f a)   -- infer App :: forall k. (k -> Type) -> k -> Type

  ---------------------------------------

  {-# LANGUAGE NoPolyKinds #-}
  module B where
  import A

  type MyApp = App

What kind should we assign to ``MyApp``? It can't be ``forall k. (k -> Type) -> k -> Type``,
because that's kind-polymorphic, and ``-XNoPolyKinds`` has forbidden us from doing this.
So GHC must again default, choosing ``MyApp :: (Type -> Type) -> Type -> Type``.

A bit annoying, but still doable. It gets worse. ::

  {-# LANGUAGE PolyKinds, ... #-}
  module C where

  data Nat = Zero | Succ Nat

  type Vec :: Nat -> Type -> Type
  data Vec n a where
    Nil  :: Vec Zero a
    (:>) :: a -> Vec n a -> Vec (Succ n) a
  infixr 5 :>

  type HList :: forall (n :: Nat). Vec n Type -> Type
  data HList ts where
    HNil  :: HList Nil
    (:>>) :: t -> HList ts -> HList (t :> ts)

  ---------------------------------

  {-# LANGUAGE NoPolyKinds #-}
  module D where
  import C

  type MyHList = HList

Now what? ``HListN`` has a kind that is polymorphic in a variable
whose kind is not even ``Type``. So we cannot default the variable to ``Type``, because
that is ill-kinded. (Recall that ``Type :: Type``.) If we try this today, GHC issues
an error, telling you not to engage in such shenanigans::

  • Cannot default kind variable ‘n0’
    of kind: Nat
    Perhaps enable PolyKinds or add a kind signature
  • In the type synonym declaration for ‘MyHList’

As we continue to explore exactly when such an error gets triggered, it becomes
harder and harder to do so. This point drives some of the complexity in
`#20686 <https://gitlab.haskell.org/ghc/ghc/-/issues/20686>`_.

I thus propose to eliminate the option of specifying ``-XNoPolyKinds``.

Proposed Change Specification
-----------------------------
1. Add ``-XPolyKinds`` to the list of extensions implied by ``-XHaskell98``
   and ``-XHaskell2010``.

#. Have ``-XNoPolyKinds`` produce an error saying that support for ``-XNoPolyKinds``
   has been dropped from GHC.

#. Six years after the acceptance of this proposal, specifying ``-XPolyKinds``
   explicitly produces a warning (controlled by the existing ``-Wdeprecated-flags``).

#. Eight years after the acceptance of this proposal, remove support for
   an extension named ``-XPolyKinds`` from GHC.

Effect and Interactions
-----------------------
1. The specification and implementation in GHC becomes simpler.

#. Inherent support for kind polymorphism makes the feature not seem exotic. Just
   as ``apply f x = f x`` should infer a polymorphic type, ``data App f a = MkApp (f a)``
   should, too.

#. The only programs that would be newly rejected with ``-XPolyKinds`` were ones
   relying on the defaulting behavior. I believe that this is possible only when
   a ``-XNoPolyKinds`` module imports a kind-polymorphic definition and then uses
   it in a context where the defaulting is required to select a class or type
   family instance. Here is an example::

     {-# LANGUAGE PolyKinds, StandaloneKindSignatures #-}
     module A where

     import Data.Kind
     import Data.Proxy

     data App f a = MkApp (f a)

     type C :: k -> Constraint
     class C a where
       meth :: Proxy a -> Int

     -------------------------------------
     {-# LANGUAGE NoPolyKinds, KindSignatures, FlexibleInstances #-}

     module B where

     import A
     import Data.Kind
     import Data.Proxy

     instance C (a :: (Type -> Type) -> Type -> Type) where
       meth _ = 5

     x :: Int
     x = meth (Proxy :: Proxy App)

   This is accepted today, and ``x`` evaluates to ``5``. However, if we
   enable ``-XPolyKinds`` in ``B``, GHC rejects, because it cannot figure
   out which ``C`` instance to use.

   I do not know of a simpler way to cause ``-XPolyKinds`` to lead to a rejection,
   and I imagine the scenario above does not arise in practice. One step of implementing
   this proposal should be a check of `head.hackage <https://ghc.gitlab.haskell.org/head.hackage/>`_
   to see if there is any breakage. Any breakage would be able to be fixed with a kind
   annotation; this fix would be fully backward compatible.

Costs and Drawbacks
-------------------
Other than the remote possibility of breakage, I do not see any drawbacks. I see
the language supporting kind polymorphism as more uniform and indeed simpler than
the one without, given the need for an extra defaulting step in the non-polymorphic
language.

Alternatives
------------
We do not have to do this, but there is a real cost to inaction in complexity
within GHC's specification and implementation. GHC developers are likely not
to completely eliminate kind polymorphism in the ``-XNoPolyKinds`` case in
tricky scenarios, as there seems little incentive to hunting down and killing
all uses of kind polymorphism.

Unresolved Questions
--------------------
None at this time.


Implementation Plan
-------------------
I will happily implement.

Endorsements
-------------
Add yourself here!