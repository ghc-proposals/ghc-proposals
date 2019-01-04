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

In summary, typeclass dictionaries are promoted to the type level, but ignoring their members, either as a unit type or as a type that simply contains promoted dictionaries for the superclass.

For Associated Type Families, Require Promoted Dictionaries to Reduce
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Let us return to the ``TNum k`` class above. What does the kind of ``(+)`` look like?

::

    λ> :k (+)
    (+) :: (TNum k) => k -> k -> k

This is about what we would expect, and it functions exactly the same way that a constrained term level function works. As the code is simplified, ``=>`` still degrades into ``->``, and an implicit argument (now of **kind** ``TNum k``) is supplied. If there is no such implicit dictionary in scope, this is a type error.

For obvious reasons of symmetry, the same requirement is present for instantiation of associated data families.

::

    class C1 (a :: k) where
        data D1 (a :: k)

    instance C1 Int where
        data D1 Int = D1Int Int

    class C2 (a :: k) where
        data D2 (a :: k)

    instance C1 a => C2 [a] where
        data D2 [a] = D2List [D1 a]

    instance C2 a => C2 (Maybe a) where
        data D2 (Maybe a) = D2Maybe (D2 a)

``D2`` now has kind ``D2 :: forall (a :: k) -> C2 a => Type`` and ``D2Maybe`` now has type ``D2Maybe :: (C2 a) => D2 a -> D2 (Maybe a)``. This is not limited to the same class, and is simply based on the instance's givens.

Formal Description   
++++++++++++++++++

The above is a series of illustrative examples, but a proper specification for this new feature is clearly required:

1. Promote typeclass dictionaries

For every class declaration `(C1 a, C2 b) => C a b c`, a new type-level data constructor is introduced `CDict :: C1 a -> C2 b -> C a b c`. That is, the type-level data constructor produces a type of kind `C a b c`, taking dictionaries of any superclasses as arguments. Nothing changes if `C` does or does not have methods.
 
For every instance declaration `C Nat Bool (Maybe a)`, a new type synonym is introduced `type CDictNatBoolMaybea = (CDict C1DictNat C2DictBool :: C Nat Bool (Maybe a))`.

2. Associated type and data families now have required constraints:

Attempting to use an associated type/data family in any way without the appropriate class constraint (that is, if GHC does not have the appropriate promoted dictionary in scope) is an error. This is true even if it does not need to be reduced, because the dictionary is an argument to the Core level representation of a constrained type family.

Explicitly, a typeclass's associated type family would be kinded as follows:

::

    -- Typeclass
    class TypeLevel (a :: Type) where
        type AType a :: Type
    
    -- old:
    AType :: Type -> Type
    -- new:
    AType :: forall (a :: Type) -> TypeLevel a => Type

    -- Kindclass
    class KindLevel k where
        type ATypeK (a :: k) :: k

    -- old:
    ATypeK :: k -> k
    -- new:
    ATypeK :: (KindLevel k) => k -> k

The distinction rests on if the variables of the class appear in the kind that the type family would have without these changes.

At the Core level, just as with term-level typeclass methods, ``=>`` degrades into ``->`` and the promoted dictionary created above is given to satisfy this newly required visible argument.

::

    -- Current term level +, in Haskell
    increment :: (Num a) => a -> a
    increment a = a + 1

    usage :: Int
    usage = increment (3 :: Int)

    -- Current term level +, in Core
    increment :: forall a -> Num a -> a -> a
    increment = \(@ a) ($dNum :: Num a) (a :: a) -> + @a $dNum a (fromInteger @a $dNum 1)

    usage :: Int
    usage = increment @Int $fNumInt (I# 3#)

    -- New type level +, in Haskell (notional syntax)
    type Increment :: TNum k => k -> k
    type Increment a = a + 1

    type Usage :: Nat
    type Usage = Increment 3

    -- New type level +, in Core (notional syntax)
    type Increment :: forall k -> TNum k -> k -> k
    type Increment k ($dTNum :: TNum k) (a :: k) = + k $dTNum a (FromInteger k $dTNum 1)

    type Usage :: Nat
    type Usage = Increment Nat $FTNumNat (3 :: Nat)

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
The Backwards Compatibility Story
+++++++++++++++++++++++++++++++++

It seems as if this behavior is going to break enough existing code that the sensible thing to do is to gate it behind an extension. However, this is the wrong way to go, because if it can be turned off, it would require a separate version of any library that uses associated type/data families for use with and without the extension enabled. There is another way to ensure backwards compatibility without simply turning off the feature completely, as will be explained in the remainder of this section.

GHC can infer the constraint we'd expect if one uses an associated type family without an appropriate one. To find the constraint we need, it should be possible to just take the same variables given as an argument to the associated type and line them up with the class that contains it. GHC will emit a warning every time it has to do this.

Let us now consider an actual example:

::

    class Collection c where
        type Elem c
    instance Collection [a] where
        type Elem [a] = a

    foo :: a -> Elem a
    foo = undefined

``foo`` is in a very real sense incorrect, because it is given a type signature that implies constraints that are not listed. To operationalize this correctness check, each time GHC sees an associated type used in a type, it generates the constraint required for the use by looking up the class that defines the associated type and instantiating a constraint from it using the parameters given for the associated type. If this constraint (or a constraint that subsumes it) is either given directly or otherwise known (such as from a GADT pattern match), the use of the associated type is lawful. If no such constraint is known, the type is unlawful.

While it may be natural to think that the correct solution is to error out and leave fixing it to the programmer, we already have a way to find the constraint we need to keep such previously correct code compiling. Assuming that the code is in reality correct, it is safe for GHC to emit a warning and then *add the inferred constraint to the type specified by the programmer*. However, if an error arises involving this constraint or any of the types that are mentioned inside of it, we give a modified error that gives the inferred constraint, the follow-on error from it, and the associated type that lead it to be generated.

Here's how it would work in practice:

1. GHC sees that ``foo`` references an associated type family, ``Elem``.
2. GHC looks up the class that contains ``Elem``, then instantiates it with the same type given as a parameter to ``Elem``, creating the constraint ``Collection a``. If the class had more parameters than the ones for the associated type, new free type variables would be generated and used to fill the empty space.
3. GHC checks to see if this constraint is either part of ``foo``'s type or ambiently known.
4. Because it is not, GHC adds it to the provided type for ``foo``, making it ``foo :: (Collection a) => a -> Elem a``. GHC then prints a warning referencing the associated type that caused GHC to infer a new constraint and the constraint it inferred, with a suggestion that it be added to the file.

In my ideal world, this would only stand for a time, perhaps governed by an extension that is initially on by default when type families are enabled and would be disabled after a few GHC major versions, turning the warning into an error.

Because this backwards compatibility system is somewhat complicated and does something somewhat unexpected (changing a programmer-supplied type signature) it may be wise to implement the feature with the warning as an error, and only enable/add the fix-up if the amount of code to be broken is substantial enough.

The Performance Story
+++++++++++++++++++++

The performance implications do not seems significant. In fact, the only case where there appears to be the possibility of a regression is as follows.

::

    class C a where
        type F a

    data FPack a where
        FPack :: F a -> FPack a

This is currently valid code, but with these changes, ``FPack`` (the data constructor) would no longer typecheck. Instead, the programmer would be required to write:

::

    data FPack a where
        FPack :: C a => F a -> FPack a

Which now adds a dictionary's burden. While this may have performance implications, any regression from this change will be undone once dependent quantifiers are implemented: that will bring with it the ability to discuss relevancy in types, allowing the erasure of the constraint if it is written as:

::

    data FPack a where
        FPack :: forall (_ :: C) => F a -> FPack a

Another, simpler solution would be to change how datatype contexts work, giving them the required constraint semantics that are truly desired here. Then, it can be written simply as:

::

    data (C a) => FPack a where
        FPack :: F a -> FPack a

Still, this is a major enough change that it is being neglected here.

The New Haskeller Story
+++++++++++++++++++++++

If anything, it makes the language easier to learn, especially when it comes to learning new libraries, since it will make it so it is obvious where an associated type family is "coming from" and prevents a class of error that is currently possible. Perhaps not likely to have much effect on those who are entirely new to the language, but even at the level I'm at now, I find myself using typeclasses as a way to "explore" libraries when they expose that type of interface, and bringing this to the type level would therefore help increase discoverability.

Alternatives
------------
The most prominent alternative is to implement the full system proposed in the Closed Type Families paper by Eisenberg and Morris, but it is unclear what substantial benefits it offers that we are losing by using this simple extension of current functionality. Even if it is lacking in some way, it seems to be entirely forwards compatible with the system that is proposed in that paper.

Additionally, there's always the option to do nothing, with the obvious tradeoff of being "free" (from an effort perspective) but not resolving the issue.

Unresolved questions
--------------------
- What is lost relative to implementing the full CTF paper system in GHC?
- How much existing code is actually going to be broken by these changes?
    - This is likely unknowable until an implementation exists.
- Are there any hidden asymmetries between the type and term level that make the duplication of the term-level system not provide the same level of soundness?

Implementation Plan
-------------------
I'm happy to try to implement it myself, but I'd likely need some amount of help from those who have more experience with the guts of the type system, at least in a mentorship-like role.
