Generalised Type Family Injectivity
===================================

.. author:: Cale Gibbard and Dan Bornside on behalf of Obsidian Systems on behalf of MIRI
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/381>`_.
.. contents::

Type families presently enjoy a very restricted form of functional dependency / injectivity constraint,
namely, one is only allowed to specify that the result of the type family uniquely determines some of its
arguments. We propose to extend this to allow arbitrary functional dependencies.

There was an attempt to implement this 4 years ago, and we're interested in reviving the effort. There's
`a GHC wiki page <https://gitlab.haskell.org/ghc/ghc/-/wikis/injective-type-families#user-content-type-c-injectivity-aka-generalized-injectivity>`_
as well as an existing `GHC issue (#10832)<https://gitlab.haskell.org/ghc/ghc/-/issues/10832>`_ regarding
the aim of this proposal.

Motivation
----------

Consider the following small example with sized-vectors, often one of the first examples of dependently-typed programming::

  {-# LANGUAGE TypeFamilies #-}
  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE GADTs #-}
  {-# OPTIONS_GHC -Wall -Werror -Wno-unticked-promoted-constructors #-}

  data Nat = Zero | Succ Nat

  type family Add (n :: Nat) (m :: Nat) :: Nat where
    Add Zero b = b
    Add (Succ a) b = Succ (Add a b)

  data Vec (n :: Nat) a where
    Nil :: Vec Zero a
    Cons :: a -> Vec n a -> Vec (Succ n) a

  data SNat (n :: Nat) where
    SZero :: SNat Zero
    SSucc :: SNat n -> SNat (Succ n)

  concatV :: Vec n a -> Vec m a -> Vec (Add n m) a
  concatV Nil ys = ys
  concatV (Cons x xs) ys = Cons x (concatV xs ys)

  dropV :: SNat n -> Vec (Add n m) a -> Vec m a
  dropV SZero ys = ys
  dropV (SSucc k) (Cons _ ys) = dropV k ys

  takeV :: SNat n -> Vec (Add n m) a -> Vec n a
  takeV SZero ys = Nil
  takeV (SSucc k) (Cons y ys) = Cons y (takeV k ys)

Everything apart from ``takeV`` here is fine, but takeV itself runs into the following error::

    * Couldn't match type `Add n m' with `Add n m0'
      Expected type: SNat n -> Vec (Add n m) a -> Vec n a
        Actual type: SNat n -> Vec (Add n m0) a -> Vec n a
      NB: `Add' is a non-injective type family
      The type variable `m0' is ambiguous
    * In the ambiguity check for `takeV'
      To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
      In the type signature:
        takeV :: SNat n -> Vec (Add n m) a -> Vec n a

Given that the results of ``Add n m`` and ``Add n m0`` are the same, we presently can't determine
that ``m`` and ``m0`` must be.

Presently, type families allow restricted functional dependencies of the form::

  type family Foo a b ... z = r | r -> ...

where the left hand side of the functional dependency arrow must be simply the result type of the
family, and in particular, we can't write something along the lines of::

  type family Add (n :: Nat) (m :: Nat) = (r :: Nat) | r n -> m, r m -> n

(we implicitly have ``m n -> r`` due to the fact that type families are essentially functions).

We'd like for this syntax to be supported, with similar machinery as supports functional dependencies
on class declarations, but adapted to the case of type families.

Proposed Change Specification
-----------------------------

See the `Injective Type Families section of the GHC User's Guide <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#injective-type-families>`_
and the `Injective Type Families for Haskell paper <http://ics.p.lodz.pl/~stolarek/_media/pl:research:stolarek_peyton-jones_eisenberg_injectivity_extended.pdf>`_
for some impression of the existing syntax and semantics.

We propose that the current syntax for injective type families be unrestricted
so that arbitrary collections of variables are allowed on both the left and right of the arrow,
and a comma-separated sequence of such functional dependency specifications is
permitted, for example, as in the final version of the ``Add`` type family above.

Each such constraint::

  v_1 ... v_n -> w_1 ... w_m

expresses the property that given any particular combination of instantiations for v_1, ..., v_n, 
there is at most one instantiation of w_1, ..., w_n for which an instance of the type family exists.

The result type variable may occur on either side of the arrow, or not at all.

This will additionally impact unification: by looking up instances of the type family knowing the instantiations of the variables on the
left hand side of a functional dependency, we can unify the variables on the right hand side according to the instance.

Examples
--------

The following are valid::

  type family Foo a b = r | r -> a, a -> b

  type family Bar a b | a -> b, b -> a

Given the above, the following combinations of instances are invalid::

  type instance Foo Int Bool = String
  type instance Foo Int Char = [String] -- The a -> b fundep is violated, because there can be at most one choice of b when a ~ Int.

  type instance Bar (Maybe t) [t] = t
  type instance Bar Char [t] = String -- There can be at most one choice of a having an instance when b ~ [t].

Effect and Interactions
-----------------------

I don't know whether there's any interaction with the work on partially-applied type families, but it is worth consideration.

There is also probably some sensible way in which it ought to interact with associated type families. Perhaps functional dependencies
on the type class should be translated to constraints on any associated type families?

Costs and Drawbacks
-------------------

There's some degree of potential to reduce future maintenance costs by sharing machinery between class and type family functional
dependencies, but this is pure speculation, and in practice, we may end up with two separate pieces of code. Depending on how involved
that code is, it may not be a huge issue either way.

With regard to learnability, I think this removes an inconsistency given the similarity of syntax -- most users already familiar with
functional dependencies might not expect the restriction on injectivity specifications for type families. That said, it enables clever
applications of these constraints to discover the instantiations of type variables from type family instances in a way that might be
tricky to do in one's head. Haskell users will need to exercise some care that they're not making life any harder for others who read their 
code. (This concern could already be levelled against tricky uses of functional dependencies in general though.)

Alternatives
------------

One question is about whether we ought to allow the result of the type family not to occur, as it is in some ways special. However,
at present we feel that unless there are technical reasons why this can't be as general as stated, it seems most useful not to impose
any restriction of that sort.

Unresolved Questions
--------------------

We could perhaps do a better job at specifying more formally which instances are permitted/forbidden by a given functional dependency.
The explanation provided is informal and might be somewhat open to interpretation. The intention is that these things work as similarly
as possible to class functional dependencies.

In addition to that, formally specifying the change to unification might be helpful.

Consider the type family and instance::

  type family F a b = c | a -> b
  type instance F Int Char = Bool

Given::

  x :: F Int b

we know that there can be no other instance than the one for F Int Char, so from this, we might deduce that b ~ Char and x :: Bool.
However, an interesting thing about this is that a similar-looking signature::

  y :: forall b. F Int b

must surely be invalid, as no other b will do. There are some questions about when exactly these constraints are generated and how to apply them in all cases.

Consider as well::

  z :: forall b. F Bool b

where there is no instance matching F Bool b, but the functional dependency seems to indicate that at most one type b would do.
Presumably if the signature for y is invalid, then so should be the signature for z, so perhaps looking at instances is irrelevant, and only the functional
dependency itself restricts our ability to quantify the variable.

Existential quantification may be fine though::

  data SomeF a = forall b. MkSomeF (F a b)
  
  e :: SomeF Int

and we can presumably recover the quantified variable when pattern matching::

  case e of
    MkSomeF (u :: F Int b) -> {- b ~ Char here because of the functional dependency on F -}
      Refl :: (b :~: Char)

While this looks sensible, we're not entirely sure how to specify things in a way that makes it entirely clear what is valid.

Implementation Plan
-------------------

Obsidian Systems intends to implement this proposal on behalf of our client MIRI, who requested a solution to the ambiguity with ``Add`` 
above.

Endorsements
-------------
