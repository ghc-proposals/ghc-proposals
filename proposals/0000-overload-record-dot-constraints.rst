Native Typechecker Fallback for OverloadedRecordDot on Constrained Fields
=========================================================================

.. author:: Gautier DI FOLCO
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/761>`_.
.. sectnum::
.. contents::

We propose modifying the typechecker to selectively fall back to native record selectors when using ``OverloadedRecordDot`` on fields with higher-rank types or typeclass constraints. This allows developers to use record dot syntax on all fields without requiring changes to the constraint solver.

Motivation
==========

Currently, ``OverloadedRecordDot`` is broken when records have a constraint, which is tracked as GHC Issue `#22267 <https://gitlab.haskell.org/ghc/ghc/-/issues/22267>`_. Whenever a user tries to reference a function with a constraint using ``OverloadedRecordDot`` syntax, they encounter a compiler error.

The root of this issue lies in the design of the ``HasField`` class, which is defined with a functional dependency ``x r -> a``. When dot-syntax is used, it desugars into a ``HasField`` constraint. However, bidirectional type inference requires that instantiations of ``forall``-bound variables be determined while traversing the term, prior to the constraint solver being invoked. Consequently, ``HasField`` constraints involving fields with higher-rank types or typeclass constraints cannot be solved automatically.

Because of this limitation, users are forced to abandon dot-syntax and revert to traditional record accessors when working with constrained fields.

Proposed Change Specification
=============================

We propose a modification to the desugaring behavior of ``OverloadedRecordDot`` within the typechecker. Instead of uniformly emitting a ``HasField`` constraint for every dot-syntax access, the typechecker will first inspect the inferred type of the record.

The behavior is specified as follows:

1. If the concrete type of the record is known during typechecking, and the accessed field is a polytype (e.g., it contains a typeclass constraint or a higher-rank ``forall``), the typechecker will directly desugar the expression to a native record selector application.
2. If the field is a monotype, or the record type is opaque/virtual, the typechecker will emit the standard ``HasField`` constraint.

This completely bypasses the functional dependency limitation by preventing the constraint solver from ever seeing the polytype. It requires no changes to the constraint solver itself or the ``GHC.Records`` module.

Proposed Library Change Specification
=====================================

No changes to library interfaces, ``base``, or ``ghc-experimental`` are required. This is purely a syntactic desugaring change within the compiler's typechecker.

Examples
========

Given the following data type containing a constrained field:

::

  data MyRecord a = MyRecord
    { myField :: Eq a => a -> Int
    }

Under the status quo, the following expression fails to compile because the constraint solver refuses to unify the polytype with the ``HasField`` functional dependency:

::

  useRecord :: MyRecord a -> a -> Int
  useRecord rec val = rec.myField val

Under this proposal, the typechecker observes that ``rec`` has a known type ``MyRecord a`` and that ``myField`` is a polytype. It then desugars ``rec.myField`` directly to the native selector ``myField rec``, allowing the code to compile successfully.

Effect and Interactions
=======================

This change resolves GHC Issue `#22267 <https://gitlab.haskell.org/ghc/ghc/-/issues/22267>`_. It interacts seamlessly with existing ``OverloadedRecordDot`` usages because it only intervenes in cases where the compiler currently throws an error. It aligns with the user's mental model that dot-syntax should be an exact syntactic equivalent to native record selection.

Costs and Drawbacks
===================

The development cost involves modifying the typechecker to peek at the record type before emitting a ``HasField`` constraint. There are no significant maintenance costs.
A minor drawback is a slight divergence in how ``OverloadedRecordDot`` operates internally (native application vs. typeclass resolution), but this divergence is entirely invisible to the user and drastically improves the learnability of the language by removing an edge-case error.

Backward Compatibility
======================

This proposal evaluates to **0. No breakage** on the impact scale.

It strictly expands the set of valid programs. Programs that previously failed to compile due to the higher-rank field limitation will now compile. No existing valid code will change behavior or stop working.

Alternatives
============

Associated Type Family Redesign
-------------------------------

Instead of using a functional dependency, it is also possible to express the relationship between the record type and the field type using a type family, such as ``type FieldType x r :: Type``.

* This type family approach ends up with unsolved constraints and equalities, which can complicate type inference.
* Supporting representation polymorphism with the type family approach would introduce extra complexity, because we would need another type family to determine the ``RuntimeRep`` of the field.

Impredicativity-Aware Constraint Solving
----------------------------------------

Integrate the built-in ``HasField`` solver directly with Quick Look impredicativity. This would permit the unifier to bind a type variable to a polytype strictly when the equality arises from the ``HasField`` functional dependency. This is mathematically complex and risks disrupting the stability of the constraint solver.

Unresolved Questions
====================

None yet.

Implementation Plan
===================

If accepted, I can rework `#14391 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/14391>`_.
