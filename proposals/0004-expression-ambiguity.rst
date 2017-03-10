.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull requst <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_. **After creating the pull request, edit this file again, update the number in the link, and delete this bold sentence.**

.. contents::

Proposal title
==============

Expression Ambiguity

This is a proposal for changing the concept of ambiguity in Haskell,
and also to define the notion of resolved overloading.

Instead of considering that a type is ambiguous --- namely, instead of
considering that C⇒τ is ambiguous if there exists a type variable in
the simple type τ tahat does not occur in the set of constraints C ---
we propose to consider that:

 • overloading of a constraint π ∈ C in type C⇒τ is resolved if there
   exists a variable in π that is unreachable from the set of type
   variables in τ.

   A variable is reachable from the set of type variables in τ if it
   occurs in τ or if occurs in a constraint where another reachable
   type variable occurs. For example c in constraint H c is reachable
   from {a} if H c is a constraint on type (F a b, G b c, H c) ⇒ a .

 • Resolved overloading of a constraint π must be checked for
   satisfiability: it can be ambiguous, if there exist two or more
   instances that entail π, unsatisfiable, if there exist no instances
   that entail π, or, otherwise, if the constraint is entailed by a
   single instance, it can be removed and the constrained type
   "improved" (simplified).

---


Motivation
------------
The reasons for this change are the following:

 • ambiguity is given its intuitive, common sense meaning, as a
   property of a name in an expression, that represents the existence
   of two or more instances of the name in the expression.
 • overloading resolution is defined and distinguished from ambiguity
 • it allows the introduction of multi-parameter type classes with no
   additional concepts such as functional dependencies being required
   to avoid ambiguity.
 • more programs become well-typed, that previoulsy caused ambiguity,
   and currently well-typed expressions remain well-typed.
 • Haskell open-world defined can be defined in a simple way, as
   meaning: satisfiability is checked if and only if overloading is
   resolved
 • type directed name resolution is enabled
 • overloaded of Ref instead of IORef and STRef is enabled

For examples and more detailed explanations of the advantages, see e.g.:
 [1] Ambiguity and Constrained Polymorphism, 
     Carlos Camarão, Lucília Figueiredo, Rodrigo Ribeiro,
     Science of Computer Programming 124(1), 1--19, 2016.


Proposed Change Specification
-----------------------------

The proposal involves no change to the grammar, and no change to the
semantics of any construct.

The only change is related to the ambiguity rule:
 • instead of considering C⇒τ as ambiguous when ∃a ∈ tv(τ): a ∉ tv(C),
   where tv computes the set of type variables of its argument, consider:
    overloading is resolved for π ∈ C when ∃a ∈ tv(π): unreachable(a,tv(τ))
 • when overloading is resolved for a constraint π ∈ C, in a type C⇒τ, then
   check satisfiability of π:
   - if π is satisfiable by two or more instances, we have a type error: π is ambiguous
   - if π is not satisfiable, we have also a type error: π is unsatisfiable
   - if π is satisfiable by a single instance, we can simplify (improve) C⇒τ, by removing
     the resolved constraint π.


Effect and Interactions
-----------------------

Note that:

 1. ambiguity can only occur if overloading is resolved and there
    exists for the resolved constraint a single instance that caused
    unreachable type variables to be instantiated.
   
    This can originate a compiler warning, that: if another instance
    that entails the constraint is introduced the program will no
    longer be type correct.
   
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
their view on ambiguity, and view ambiguity as meaning the existence
of two or more instances when overloading is resolved. A consequence
is the fact that the number of instances that entail a resolved
constraint is significant.

Note that this view of ambiguity represents its common, natural
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
