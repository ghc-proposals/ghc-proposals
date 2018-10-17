Simple Constrained Type Families
================================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/177>`_.
.. sectnum::
.. contents::

Constrained Type Families allow the amelioration of a key deficit with the current implementation of type families, which is the assumption of totality. The problems with this assumption are `well documented <https://arxiv.org/abs/1706.09715>`_ by Richard Eisenberg (@goldfirere) and J. Garrett Morris (@jgbm).  

It would be lovely, of course, if we had a way to represent something that looks like a function and has a limited domain relative to what the signature would imply. Thankfully, Haskell provides an excellent mechanism for this sort of constrained polymorphism, typeclasses. Typeclasses can already contain type families, but this currently doesn't do much to impact the type family included, and it isn't an error to apply an associated type family to a type that isn't an instance of the type class in question.

At the outset, I would like to thank Vladislav Zavialov (@int-index) for the help they provided crystallizing the ideas presented below.

Motivation
------------

A lack of constrained type families leaves many "obviously injective" type families unable to be declared as such, and leaves other families open to misuse. For an example of each, we will use a `Pred` family for inductive type level natural numbers and polymorphic arithmetic on type level numbers. For more examples of the former case, see § 4.1 in `the paper introducing injective type families <http://ics.p.lodz.pl/~stolarek/_media/pl:research:stolarek_peyton-jones_eisenberg_injectivity.pdf>`_.

::

    data Nat = Zero | Succ Nat

    type family Pred nat = pred | pred -> nat where
        Pred (Succ nat) = nat

Currently, this is an error, because even for a closed type family, GHC is unable to recognize that a type family can have a partial domain. 

:: 

    • Type family equation violates injectivity annotation.
      RHS of injective type family equation is a bare type variable
      but these LHS type and kind patterns are not bare variables: ‘'Succ nat’
        Pred ('Succ nat) = nat -- Defined at <interactive>:4:9
    • In the equations for closed type family ‘Pred’
      In the type family declaration for ‘Pred’

This is preferable to the second type of error, where meaningless code can be well-typed.

::

    class TNum k where
        type (a :: k) + (b :: k) :: k
        type (a :: k) - (b :: k) :: k
        type (a :: k) * (b :: k) :: k
        type FromInteger (a :: Integer) :: k

This is accepted by GHC, but it provides an interface that leaves much to be desired. While it is not possible to write an instance for any of these type families independently of an instance for TNum k, there is nothing stopping the programmer from using these type families nonsensically.

::

    newtype Const t a = Const a

    pairWithProduct :: a -> b -> Const (a * b) (a, b)
    pairWithProduct a b = Const (a, b)

    what :: Const (Integer * String) (Integer, String)
    what = pairWithProduct 42 "foo"

What is the meaning of ``Integer * String``? There are arguable definitions that could be used, but it is unlikely to actually be what the programmer would like to express, at least if they haven't provided an explanation for what doing arithmetic with arbitrary types is supposed to mean by writing an instance ``TNum Type``.

Proposed Change Specification
-----------------------------

There is a very simple way to reuse the currently existing mechanisms to give the desired behavior, with two changes to current behavior:

Promote Typeclass Dictionaries
++++++++++++++++++++++++++++++

Currently, typeclass instances are desugared into the creation of constant values in a special namespace with a "secret" dictionary type that shares the name of the typeclass that contains fields for each value-level member of the typeclass, or for typeclasses without any value-level members, as a unit type. For example, using the ``TNum k`` example and ``-ddump-simpl``, it can be seen that we generate the following dictionary for a declaration of ``TNum Int``.

::

    -- RHS size: {terms: 1, types: 1, coercions: 0, joins: 0/0}
    interactive:Ghci2.$fTNumInt [InlPrag=CONLIKE] :: TNum Int
    [GblId[DFunId], Caf=NoCafRefs]
    interactive:Ghci2.$fTNumInt = interactive:Ghci1.C:TNum @ Int

This has a very simple constructor ``C:TNum`` and it is easy to promote it, but this doesn't help typeclasses that contain both type and value level members. What should GHC do with a typeclass such as the following?

::

    class IsList l where
        type family Item l :: *
        fromList :: [Item l] -> l

Of course, we could only promote classes that don't have any methods, but that is a very limiting solution to the problem. Instead, I propose that we promote every class as if it has no methods, which does create another case where the original and promoted type differ, but this is hardly new. ``Type`` is uninhabited at the term level but contains ``Int``, ``Bool``, ``Char`` and many more at the type level.

There is one further wrinkle of how typeclass instances work that must be addressed. Instance declarations for classes with a superclass requirement include the superclass' dictionary, and promoted dictionaries function the same way.

::

    class (TNum k) => TIntegral k where
        -- ...

``C:TIntegral``, once promoted, will have kind ``forall (k :: Type). TNum k -> TIntegral k``.

For Associated Type Families, Require Promoted Dictionaries to Reduce
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Let us return to the ``TNum k`` class above. What does the kind of ``(+)`` look like?

::

    λ> :k (+)
    (+) :: (TNum k) => k -> k -> k

This is about what we would expect, and it functions exactly the same way that a constrained term level function works. As the code is simplified, ``=>`` still degrades into ``->``, and an implicit argument (now of **kind** ``TNum k``) is supplied. If there is no such implicit dictionary in scope, this is a type error.

As a minor note, for obvious reasons of symmetry, the same requirement is present for instantiation of associated data families.

::

    class C2 (a :: k) where
        data D (a :: k)

    instance C2 a => C2 (Maybe a) where
        data D (Maybe a) = DMaybe (D a)

``D`` now has kind ``D :: forall (a :: k) -> C2 a => D a`` and ``DMaybe`` now has type ``DMaybe :: (C2 a) => D a -> D (Maybe a)``.

Effect and Interactions
-----------------------
It is obvious that this solves the issue raised by example 2, because it creates a kind that expresses the constraint that is intended and allows the type system to provide the same guarantees that we provide to term level functions to type families.

By itself, however, it does nothing to resolve the issue with example 1. For that purpose, it is important to extend the injectivity checker to consider the injectivity over the domain of types with instances rather than all well-kinded types, allowing example 1 to be written as:

::

    data Nat = Zero | Succ Nat

    class NatPred (nat :: Nat) where
        type Pred (nat :: Nat) = (pred :: Nat) | pred -> nat

    instance NatPred (Succ nat) where
        type Pred (Succ nat) = nat

``Pred`` would now have the kind ``forall (nat :: Nat) -> NatPred nat => Nat``, which is an example of visible dependent quantification. There is an existing GHC proposal to add this syntax to the source language, but this feature has existed in the compiler since GHC 8.0.

The other reliance on accepted-yet-unimplemented functionality is to allow type families and other similar contextless syntax forms to use constrained type families.

::

    class C a where
        type T a

    type family S a where
        S a = T a

As implemented now, if this proposal were to be accepted, it would not be possible to write ``S``, because there is no way of stating the ``C a`` constraint. Top-level kind signatures solve this issue handily.

::

    type S :: forall (a :: Type) -> C a => Type
    type family S a where
        S a = T a


There are no substantial conflicts with other compiler features, because it is a simple extension of existing functionality with fairly minimal potential for conflict.

Costs and Drawbacks
-------------------
It is true that we can conceive of code that will be broken by these changes, but that's the whole point! I would proffer that the amount of actual code that would be harmed by these changes is fairly minimal, because how often does the use of an associated type family not either require other use of the typeclass? I'd be especially skeptical that correct code is ever using a type family at a type that has no instance.

I don't have a rigorous test for this, but it seems unlikely that much correct code will be dinged, and when it is made erroneous it is a very minimal and entirely backwards compatible change to make it work correctly with the new feature. 

If anything, it makes the language easier to learn, especially when it comes to learning new libraries, since it will make it so it is obvious where an associated type family is "coming from" and prevents a class of error that is currently possible.

Alternatives
------------
The most prominent alternative is to implement the full system proposed in the Closed Type Families paper by Eisenberg and Morris, but it is unclear what substantial benefits it offers that we are losing by using this simple extension of current functionality. Even if it is lacking in some way, it seems to be entirely forwards compatible with the system that is proposed in that paper.

Additionally, there's always the option to do nothing, with the obvious tradeoff of being "free" (from an effort perspective) but not resolving the issue.

Unresolved questions
--------------------
- What is lost relative to implementing the full CTF paper system in GHC?
- How much existing code is actually going to be broken by these changes?
- Are there any hidden asymmetries between the type and term level that make the duplication of the term-level system not provide the same level of soundness?

Implementation Plan
-------------------
I'm happy to try to implement it myself, but I'd likely need some amount of help from those who have more experience with the guts of the type system, at least in a mentorship-like role.
