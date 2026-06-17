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

Currently, we accept a level of sloppiness at the type level that would be an immediate, stop-the-presses bug if it snuck in at the value level: Meaningless code can be well-typed.

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

This is the equivalent of GHC accepting ``"a" + "b"`` without an instance for ``Num String`` and simply producing an error at runtime.

In the future, with some major changes (but still less than the original Constrained Type Families paper), this system is a start towards recovering its results but with more minor changes to the compiler. This proposal only makes the compiler able to reject bad programs of the family (pardon the pun) described above, however. More information on this future path of development is in the Effects and Interactions section.

Proposed Change Specification
-----------------------------

Admittedly, this specification does go somewhat deep into what would normally be considered implementation details, but this is done to show that the results claimed are possible without any changes to Core; indeed, not changing Core is the entire reason this proposal exists.

Promote typeclass dictionary constructors
+++++++++++++++++++++++++++++++++++++++++

For every class declaration ``(C1 a, C2 b) => C a b c``, a new type-level data constructor is introduced ``CDict :: C1 a -> C2 b -> C a b c``. That is, the type-level data constructor produces a type of kind ``C a b c``, taking dictionaries of any superclasses as arguments. Nothing changes if ``C`` does or does not have methods.


Generate type-level dictionaries at every instance declaration
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

For every instance declaration ``C Nat Bool (Maybe a)``, a new type synonym is introduced ``type CDictNatBoolMaybea = (CDict C1DictNat C2DictBool :: C Nat Bool (Maybe a))``.

To demonstrate, let us consider the ``TNum`` example above. This is the value level dictionary for an instance of ``TNum``:

::

    $fTNumInt :: TNum Int
    $fTNumInt = C:TNum @ Int

The promoted dictionary would be much the same:

::

    type $FTNumInt :: TNum Int
    type $FTNumInt = 'C:TNum @ Int

Additionally, superclass dictionaries are given as argument to the dictionary constructor, just as with value-level dictionaries. Consider the following ``TIntegral`` class:

::

    class (TNum k) => TIntegral k where
        -- ...

    -- Promoted dictionary generated for `TIntegral Int`:
    type $FTIntegralInt :: TIntegral Int
    type $FTIntegralInt = 'C:TIntegral @ Int $FTNumInt

Associated type and data families have constrained kinds
++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Attempting to use an associated type or data family in any way without the appropriate class constraint (that is, if GHC does not have the appropriate promoted dictionary in scope) is an error. This is true even if it does not need to be reduced, because the dictionary is an argument to the Core level representation of a constrained type family or of a constrained data family's type constructor.

Explicitly, a typeclass's associated type/data families would be kinded as follows:

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

Require associated type and data families to cover their constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An important issue arises here: an associated type family currently may not provide sufficient information to unambiguously refer to a required instance. Consider the following example:

::

    class Vague (a :: j) (b :: k) where
        type Underspecified (b :: k)

Currently, ``Underspecified :: k -> Type``. If we try to constrain this in the obvious way, we get ``Underspecified :: forall (a :: j). forall (b :: k) -> Vague a b => Type`` which is not only a lot more complicated, but is also going to be ambiguous unless the programmer adds type applications to set ``a``!

Thus, we now require that the variables used in an associated type declaration must cover all of the class variables, in that choices for the associated type arguments must uniquely determine the choice of class instance. This might be done via, e.g., functional dependencies or superclass equality constraints. This will lead to code breakage, but in almost all cases, there is an implicit dependency between the variables that can be made explicit. In the few cases where no such dependency exists, the associated type may be factored out into a superclass over only the relevant variables.

Core-level example
~~~~~~~~~~~~~~~~~~

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
    type Usage = Increment Nat TNumDictNat (3 :: Nat)

Constraining data constructors
++++++++++++++++++++++++++++++

The new constraints added to the kinds of associated types have implications for the types of relevant data constructors.

1. Associated data constructors *require* the availability of the appropriate instance of their parent class.

   Considering the example below, it is clear that a binding ``x :: D Bool`` is not generally valid; ``D Bool`` is not even a valid type without the backwards compatibility elaborations! We must write ``x :: C Bool => D Bool`` instead.
   
   The same logic applies to the data constructor itself. ``DBool :: D Bool`` is not valid by itself, so instead the constructor must have the type ``DBool :: C Bool => D Bool``, but because ``C Bool`` must be in scope to pattern match on ``DBool``, we cannot *provide* it.

2. A constructor that contains an associated type in a field *provides* the appropriate instance of its parent class.

   This is implied by the backwards compatibility section below. When matching on ``MkE`` from the example below, for its existential field to have a valid type we must be provided an instance ``C a``, and thus the constructor must carry the dictionary.

To provide a concrete example:

::

    class C a where
        -- D :: forall (a :: Type) -> C a => Type
        data D a
    
    instance C a => C (Maybe a) where
        -- DMaybe :: C (Maybe a) => C a => D a -> D (Maybe a)
        --           ^^^^^^^^^^^    ^^^
        --           required       provided
        data D (Maybe a) = DMaybe (D a)
    
    instance C Int where
        -- DInt :: C Int => () => D Int
        --         ^^^^^    ^^
        --         req.     prov.
        data D Int = DInt
    
    data E where
        -- MkE :: C a => D a -> E
        --        ^^^
        --        prov.
        MkE :: D a -> E

In both of the above cases, if irrelevant constraints were implemented, it would be entirely reasonable to mark the constraints as irrelevant.

Datatype contexts are available during kind checking
++++++++++++++++++++++++++++++++++++++++++++++++++++

Previously, non-equality constraints in kinds were prohibited and thus this does not truly represent a change in the semantics of datatype contexts, but is an important point to consider for performance reasons. Because "stupid theta" constraints are not provided, they let the programmer use associated types in datatype fields without adding a new field to hold the dictionary we newly demand. This is especially important because it preserves the ability to wrap associated types in ``newtype``\s.

Thus, ``DatatypeContexts`` are undeprecated, and are now permitted in conjunction with GADT syntax. GHC's parser already recognizes them correctly (in order to report an error stating that they are not supported). I cannot find a rigorous statement of the 'idealized' GADT syntax to give a precise change to the BNF, but in practice the change consists of simply removing the check.

Backwards compatibility
+++++++++++++++++++++++

It seems as if this behavior is going to break enough existing code that the sensible thing to do is to gate it behind an extension. However, if it can be turned off, it would require a separate version of any library that uses associated type/data families for use with and without the extension enabled. Instead, GHC can infer the constraints needed and add them to pre-existing code.

Because (if this proposal is accepted) each associated type's variables cover the instance variables, it is trivial for GHC to infer the appropriate constraint that would make such a usage legal, adding it to the programmer-supplied type and emitting a warning. 

Let us now consider an actual example:

::

    class Collection c where
        type Elem c
    instance Collection [a] where
        type Elem [a] = a

    foo :: a -> Elem a
    foo = undefined

``foo`` is in a very real sense incorrect, because it is given a type signature that implies constraints that are not listed. To operationalize this correctness check, each time GHC sees an associated type used in a type, it generates the constraint required by instantiating the class variables appropriately. If this constraint (or a constraint that subsumes it) is required by the existing type or otherwise known (such as from a GADT pattern match), the use of the associated type is lawful. If no such constraint is known, the type is unlawful.

While it may be natural to think that the correct solution is to error out and leave fixing it to the programmer, we already have a way to find the constraint we need to keep such previously correct code compiling. Assuming that the code is in reality correct, it is safe for GHC to emit a warning and then *add the inferred constraint to the type specified by the programmer*. However, if an error arises involving this constraint or any of the types that are mentioned inside of it, we give a modified error that gives the inferred constraint, the follow-on error from it, and the associated type that lead it to be generated.

Here's how it would work in practice:

1. GHC sees that ``foo`` references an associated type family, ``Elem``.
2. GHC looks up the class that contains ``Elem``, then instantiates it with the same type given as a parameter to ``Elem``, creating the constraint ``Collection a``. If the class had more parameters than the ones for the associated type, new free type variables would be generated and used to fill the empty space.
3. GHC checks to see if this constraint is either part of ``foo``'s type or ambiently known.
4. Because it is not, GHC adds it to the provided type for ``foo``, making it ``foo :: (Collection a) => a -> Elem a``. GHC then prints a warning referencing the associated type that caused GHC to infer a new constraint and the constraint it inferred, with a suggestion that it be added to the file.

Analogously, data constructors that contain an associated type or data family but do not have the constraints necessary are modified to provide them, just as with a user-written GADT.

This behavior would only stand for a time, governed by a new extension that is initially implied by ``-XTypeFamilies`` and that would be disabled after two GHC major versions, turning the warning into an error.

Effect and Interactions
-----------------------

Improves the Type Safety of Associated Families
++++++++++++++++++++++++++++++++++++++++++++++++
It is obvious that this solves the issue raised by the example in the Motivation section, because it creates a kind that expresses the constraint that is intended and allows the type system to provide the same guarantees that we provide with term level functions to type families.

Future Development Will Simplify Type Family Injectivity
++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Consider the simple type family ``ListElems`` drawn from the Constrained Type Families paper (§ 3.3). For more examples, see § 4.1 in `the paper introducing injective type families <http://ics.p.lodz.pl/~stolarek/_media/pl:research:stolarek_peyton-jones_eisenberg_injectivity.pdf>`_.

::

    type family ListElems a = b | b -> a where
        ListElems [a] = a

Currently, this is an error, because even for a closed type family, GHC is unable to recognize that a type family can have a partial domain. 

:: 

    • Type family equation violates injectivity annotation.
      RHS of injective type family equation is a bare type variable
      but these LHS type and kind patterns are not bare variables: ‘[a]’
        ListElems [a] = a -- Defined at <interactive>:4:9
    • In the equations for closed type family ‘ListItems’
      In the type family declaration for ‘ListItems’

This is because ``ListElems [ListElems Int] ~ ListElems Int`` by the declaration given, and by injectivity as defined in the Injective Type Families paper (Definition 1)

    Definition 1 (Injectivity). A type family F is n-injective (i.e. injective in its nth argument) iff ∀σ,τ : F σ ∼ F τ ⇒ σ n ∼ τ n

we recover the equation ``[ListElems Int] ~ Int``.

This is an obvious problem, but constrained type families provide a solution. There is an existing pairwise-apartness test for injective type families, which requires that no two RHSes are able to unify. This would clearly prevent any other instances of ``ListElems`` from being written, including some ``ListElems Int``, because any type is unifiable with ``a``. Thus, this heinous equality is safely guarded behind an unsatisfiable ``ListElems Int`` constraint.

To get the maximum improvement, there are several follow-on changes that would need to be made:

* Add support for closed type classes (whether exposed or not) 
* Change top-level type families into syntactic sugar for (constrained) associated types, including changing closed type families into closed typeclasses:
    This would effectively change this

    ::

        type family Pred :: Nat -> Nat where
            Pred (S n) = n

    into syntactic sugar for something morally equivalent to this (using notional syntax for closed type classes)

    ::

        class {-# CLOSED #-} Pred (n :: Nat) where
            type Pred n :: Nat

            instance Pred (S n) where
                type Pred (S n) = n
* Once all (non-total) type families are constrained, we can eliminate the assumption of totality.
    * This means we no longer need to use infiniary unification for the closed type family apartness check, because any infinite type family would require satisfying an infinite constraint (something in the form ``Loop => Loop``), which is plainly impossible.
    * This will make type families in general more closely match the intuition of them as potentially partial type-level functions.
    * Obvious caveat: Determining totality is a difficult (indeed, unsolvable) but well-understood problem, and a rubicon that GHC will have to cross at some point as it moves towards being a dependently typed language.
* By constraining type families to the domains over which they are defined, we solve "Awkward Case 2" in the Injective Type Families paper.
* Additionally, as mentioned above, infinite constrainted type families would also be hidden behind an unsatisfiable constraint, so it would solve "Awkward Case 3" as well.

Top-Level Kind Signatures Are Necessary
+++++++++++++++++++++++++++++++++++++++

The proposal does have reliance on allowing type families and other similar contextless syntax forms to use constrained type families, as specified in `proposal #36 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0036-kind-signatures.rst>`_, ``-XTopLevelKindSignatures`` and implemented in GHC 8.10.

::

    class C a where
        type T a

    type family S a where
        S a = T a

Without these signatures, it would not be possible to write ``S a``, but an explicit signature allows the programmer to specify the needed kind.

::

    type S :: forall (a :: Type) -> C a => Type
    type family S a where
        S a = T a

Costs and Drawbacks
-------------------
The Performance Story
+++++++++++++++++++++

The performance implications do not seem significant. In fact, the only case where there appears to be the possibility of a regression is as follows.

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

Indeed, adding constrained type families provides a motivation to add irrelevant binders, at least for constraints, without needing dependent types fas a justification, but that is beyond the scope of this proposal.


The New Haskeller Story
+++++++++++++++++++++++

If anything, it makes the language easier to learn, especially when it comes to learning new libraries, since it will make it so it is obvious where an associated type family is "coming from" and prevents a class of error that is currently possible. Perhaps not likely to have much effect on those who are entirely new to the language, but even at the level I'm at now, I find myself using typeclasses as a way to "explore" libraries when they expose that type of interface, and bringing this to the type level would therefore help increase discoverability.

Alternatives
------------
The most prominent alternative is to implement the full system proposed in the Constrained Type Families paper by Eisenberg and Morris, but it is unclear what substantial benefits it offers that we are losing by using this simple extension of current functionality, other than closed type classes (which are fairly orthogonal) and constraining top-level type/data families, which would be easy to add if this works well in practice. Even if it is lacking in some way, it seems to be entirely forwards compatible with the system that is proposed in that paper.

Additionally, there's always the option to do nothing, with the obvious tradeoff of being "free" (from an effort perspective) but not resolving the issue.

Unresolved questions
--------------------
* What is lost relative to implementing the full CTF paper system in GHC?
* How much existing code is actually going to be broken by these changes?
    * This is likely unknowable until an implementation exists.
    * If the breakage would be minimal, perhaps GHC should simply produce a type error when the currently-proposed backwards compatibility fix would be needed.
    * If the backwards compatibility extension is implemented, what should it be named? I would suggest ``-XInferKindConstraints``.

Implementation Plan
-------------------
I'm happy to try to implement it myself, but I'd likely need some amount of help from those who have more experience with the guts of the type system, at least in a mentorship-like role.
