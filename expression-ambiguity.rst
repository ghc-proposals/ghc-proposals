Expression Ambiguity
==============

This is a proposal for changing the concept of ambiguity in Haskell,
and also to define the notion of resolved overloading.

Instead of considering that a type is ambiguous --- namely, instead of
considering that C⇒τ is ambiguous if there exists a type variable in
the set of constraints C that does not occur in the simple type τ ---
we propose to consider that:

 • overloading of a constraint π ∈ C in type C⇒τ is resolved if there
   exists a variable in π that is unreachable from the set of type
   variables in τ.

   A variable is reachable from the set of type variables in τ if it
   occurs in τ or if occurs in a constraint where another reachable
   type variable occurs. For example c in constraint H c is reachable
   from {a} if H c is a constraint on type (F a b, G b c, H c) ⇒ a .

 • When overloading of a constraint π is resolved, but only then, it
   must be checked for satisfiability: it can be ambiguous if there
   exist two or more instances that entail π, unsatisfiable if there
   exist no instances that entail π, otherwise the constraint is
   entailed by a single instance, and it can then be removed and the
   constrained type "improved" (simplified).

---


Motivation
------------
The reasons for this change are the following:

 • Allow multi-parameter type classes (MPTCs) to be introduced in the
   language without the need of extra mechanisms for dealing with
   ambiguity problems that arise in the presence of MPTCs.

 • Define resolved overloading, via resolved constraint, by
   considering not the syntactic property that type variables in a
   constraint π ∈ C do not occur in the simple type τ of a constrained
   type C⇒τ, but the property that type variables in a constraint π ∈
   C are unreachable from the set of type variables of τ.

   When overloading is resolved, and only then, unifying instances
   determine if resolved constraint π is ambiguous (if there are 2 or
   more instances that unify with π), unsatisfiable (if there is no
   unifying instance) or can be removed from the constrained type (if
   there is a single unifying instance).

   This is enough to solve the root cause of problems: first it must
   be considered whether overloading is resolved or not (no ambiguity
   can occur when overloading is not resolved), and only after
   overloading is resolved, it must be checked whether there is
   ambiguity or not, unsatisfiability or not, or whether resolved
   constraints can be removed from the constrained type.

We present next some examples.

Example 1

============================================

class ShowLike a where 
  showLike :: a -> String

class ReadLike a where 
  readLike :: String -> a

instance ShowLike Bool where
  showLike = show

instance ReadLike Bool where
  readLike = read

sr = showLike . readLike

main = print $ sr "True"

=============================================

With expression ambiguity, this program is well-typed (does not cause
an ambiguity error). I explain why next. The non-improved type of rs
is: 
    (ShowLike a, ReadLike a) ⇒ String → String

Constraints (ShowLike a) and (ReadLike a) are resolved in this
type. It must be consider, then, for each constraint (ShowLike
a) and (ReadLike a), which instances are visible. Since there is only
one instance for each one, each of them can be instantiated,
respectively to ShowLike Bool and ReadLike Bool. Then, each constraint
can be eliminated, and the type improved to String→String.

Summary: when a constrain is resolved, it must be checked whether
there exist unifying instances, and if there is only one unifying
instance there is no ambiguity (the constraint can be removed).

Example 2

=============================================

class Conv a b where
  conv:: a -> b

instance Conv Char Bool where
  conv '0' = False
  
  conv _   = True

main = print (conv '1')

=============================================

A similar situation occurs here: with expression ambiguity, this
program is well-typed (prints True), whereas currently in Haskell the
program is not well-typed because of an "ambiguity" type error. An
explanation follows.

The type of (conv '1') is: Conv Char b ⇒ b

Constraint (Conv Char b) in this type is not yet resolved. It becomes
resolved in:

   print (conv '1')

The non-simplified type of (print (conv '1')) is

  (Show b, Conv Char b) => IO()

Overloading of (Conv Char b) in this type is resolved. Since there is
a single instance of (Conv Char b) visible, namely Conv Int String,
then b is instantiated to String, and the type of (print (conv '1'))
becomes IO().

Example 3

============================================

{-# LANGUAGE MultiParamTypeClasses #-}

module Ex3 where

class Sum a b c where
  (<+>):: a->b->c
    
class NumLit a where
  zero:: a

data Nat = Zero | Suc Nat

instance NumLit Nat where
  zero = Zero

instance Sum Nat Nat Nat where
  (<+>) Zero    b = b
  
  (<+>) (Suc n) b = Suc ((<+>) n b)

i = (<+>) Zero

=============================================

Similar situation here. The non-simplified type of i is:

   (Sum Nat b c, NumLit Nat) ⇒ b→c

which can be simplified to:
 
    Sum Nat b c ⇒ b→c

Since overloading is not yet resolved for Sum Nat b c, no
satisfiability checking is needed.

Finally, 2 variants of Richard's polymonads example, taken from
https://ghc.haskell.org/trac/ghc/ticket/8634, illustrate the advantage
of expression ambiguity of not considering non-occurrence in the
simple type as ambiguity (again: expression ambiguity considers that
the fact that a variable in a constraint does not occur in the simple
type does not mean that overloading is resolved and, even if it has
been resolved, i.e. even if it the variables in it are unreachable, we
may not have ambiguity). Both variants compile ok with expression
ambiguity, because overloading is not yet resolved.

Example 4: variant 1

==========================================================

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module PolyMonad where

class (Monad m1, Monad m2) => Morph m1 m2 where
  morph :: m1 a -> m2 a

class PolyMonad m1 m2 m3 where
  (|>>=|) :: m1 a -> (a -> m2 b) -> m3 b

instance  (Morph m1 m2) => PolyMonad m1 m2 m2 where
  ma |>>=| fmb = morph ma >>= fmb

f:: (PolyMonad m1 m2 m2, PolyMonad m2 m3 m3) => m1 a -> (a -> m2 b) -> (b -> m3 c) ->  m3 c
f x g h = x |>>=| \a -> g a |>>=| \b -> h b

Example 4: variant 2

===============================================================================

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

module PolyMonad where

class (Monad m1, Monad m2) => Morph m1 m2 where
  morph :: m1 a -> m2 a

class PolyMonad m1 m2 m3 where
  (|>>=|) :: m1 a -> (a -> m2 b) -> m3 b

instance  (Morph m1 m3, Morph m2 m3) => PolyMonad m1 m2 m3 where
  ma |>>=| fmb = morph ma >>= morph . fmb

f:: (PolyMonad m1 m2 m3, PolyMonad m3 m4 m5) => m1 a -> (a -> m2 b) -> (b -> m4 c) ->  m5 c
f x g h = x |>>=| \a -> g a |>>=| \b -> h b

===============================================================================

For more examples see e.g.:
 [1] Ambiguity and Constrained Polymorphism, 
     Carlos Camarão, Lucília Figueiredo, Rodrigo Ribeiro,
     Science of Computer Programming 124(1), 1--19, 2016.


Proposed Change Specification
-----------------------------

The proposal involves no change to the grammar, and no change to the
semantics of any construct.

The only change is related to the ambiguity rule:

 • instead of considering C⇒τ as ambiguous when ∃a ∈ tv(C): a ∉ tv(τ),
   where tv computes the set of type variables of its argument,
   consider: overloading is resolved for π ∈ C when ∃a ∈ tv(π):
   unreachable(a,tv(τ))

 • when overloading is resolved for a constraint π ∈ C, in a type C⇒τ,
   then check satisfiability of π:

   - if π is satisfiable by two or more instances, we have a type
     error: π is ambiguous

   - if π is not satisfiable, we have also a type error: π is unsatisfiable

   - if π is satisfiable by a single instance, we can simplify
     (improve) C⇒τ, by removing the resolved constraint π.


Effect and Interactions
-----------------------

Note that:

 1. ambiguity can only occur if overloading is resolved and there
    exists for the resolved constraint more than one unifying instance
    for the resolved constraint.
   
    When there exists a single unifying instance, the resolved
    constraint can be removed: this fact can originate a compiler
    warning, that: if another instance that entails the constraint is
    introduced the program will no longer be type correct. In this
    case, to prevent a module from becoming type-incorrect by the
    insertion of an additional unifying instance for this resolved
    constraint, a default clause should be introduced in the program
    (that needs defaulting to become more general than it is today in
    Haskell, allowing, for example, "default (Monad m) []"). 
   
 2. The situation that the introduction of a new instance causes a
    well-typed program to become type-incorrect (because of an
    ambiguity error) results from a program that would have been
    considered, with the old ambiguity rule, not well typed before the
    introduction of the new instance.
   
 3. The situation that the introduction of a new instance causes a
    well-typed program to become type-incorrect can already occur for
    orphan instances: if a program P uses modules A,B,O, module A uses
    an (orphan) instance defined in module O (for example an additive
    Monoid instance for type Integer) and another instance is included
    in module B (for example, a multiplicative Monoid instance for
    Integer) this causes P to be become type incorrect.

Costs and Drawbacks
-------------------

Development and maintenance costs are expected to be small.

A drawback of the proposal is that Haskell programmers need to change
their view on ambiguity. Firslty, they need to become aware of the
fundamental notion of resolved overloading: a resolved constraint need
not only variables that do not occur in the simple type, it need
unreachable variables (if there is a single reachable variable in a
constraint, all its variables are reachable). Secondly, ambiguity can
only occur for resolved constraints and means the existence of two or
more unifying instances for the resolved constraint. A consequence is
the fact that the number of instances that entail a resolved
constraint is significant.

This view of ambiguity represents its common, natural
understanding. Thus, learnability and usage of the language should be
enhanced.

Alternatives
------------

Alternatives to the proposed change are the introduction of functional
dependencies and/or type families to the language. In my view type
families in particular can be useful, but for purposes other than
dealing with ambiguity.

Unresolved questions
--------------------

Implementation Plan
-------------------
