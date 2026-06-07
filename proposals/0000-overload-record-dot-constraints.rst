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

We propose modifying the desugaring of ``OverloadedRecordDot`` so that, when the record type is concretely known and the field has a polymorphic type (involving ``forall`` or typeclass constraints), the typechecker uses the native record selector directly instead of going through ``HasField``. This resolves GHC Issue `#22267 <https://gitlab.haskell.org/ghc/ghc/-/issues/22267>`_ without any changes to the constraint solver.

Motivation
==========

The problem
-----------

With ``OverloadedRecordDot``, ``r.field`` desugars to ``getField @"field" r``, which invokes the ``HasField`` typeclass. This works well for monomorphic fields:

.. code:: haskell

   {-# LANGUAGE OverloadedRecordDot #-}

   data R = R { myFunc :: String -> Int }

   myUsage :: R -> Int
   myUsage r = r.myFunc "42"      -- OK: desugars to getField @"myFunc" r "42"

But adding a constraint to the field causes a compile error:

.. code:: haskell

   {-# LANGUAGE OverloadedRecordDot #-}

   data R = R { myFunc :: HasCallStack => String -> Int }

   myUsage :: R -> Int
   myUsage r = r.myFunc "42"

This produces:

::

   error:
       • No instance for (GHC.Records.HasField "myFunc" R (String -> Int))
           arising from selecting the field 'myFunc'
       • In the expression: r.myFunc
         In the expression: r.myFunc "42"
         In an equation for 'myUsage': myUsage r = r.myFunc "42"

The same failure occurs with any constrained field type:

.. code:: haskell

   data S = S { showable :: forall a. Show a => a -> String }

   useS :: S -> String
   useS s = s.showable 42       -- ERROR: HasField cannot be solved

.. code:: haskell

   data T a = T { eqField :: Eq a => a -> Bool }

   useT :: T a -> a -> Bool
   useT t x = t.eqField x       -- ERROR: HasField cannot be solved

In every case, the **native** record selector works perfectly:

.. code:: haskell

   myUsage r = myFunc r "42"     -- OK
   useS s     = showable s       -- OK
   useT t x   = eqField t x      -- OK

Yet the user is forced to abandon dot syntax and write these by hand.

Why does ``HasField`` fail?
---------------------------

The ``HasField`` class is defined as:

.. code:: haskell

   class HasField (x :: Symbol) r a | x r -> a where
     getField :: r -> a

The functional dependency ``x r -> a`` says that the result type ``a`` is uniquely determined by the field name and the record type. For a constrained field like ``HasCallStack => String -> Int``, the type checker would need to unify ``a`` with a polytype, but the constraint solver is not designed to do this. It therefore reports ``No instance for HasField``.

This is an inherent limitation of the ``HasField`` design: the functional dependency ties the result type to a single monomorphic type, which cannot accommodate the implicit quantification introduced by constraints or explicit ``forall``\ s.

Goal
----

Dot syntax should work on all fields of a record whose type is concretely known, regardless of whether the field type is monomorphic or polymorphic. When the record type is known, the compiler has all the information needed to resolve the field without going through ``HasField``.

Proposed Change Specification
=============================

Desugaring rules
----------------

We modify the desugaring of field selection expressions ``e.fld`` when ``OverloadedRecordDot`` is enabled. The new behaviour is:

1. **Native fallback.** If, at the point where ``e.fld`` is desugared, the type of ``e`` is a **concretely known record type** ``T p1 ... pn`` (i.e., ``T`` is a data type constructor, not a type variable or type family), and ``fld`` is a field of ``T`` whose type is a **polytype** (its top-level structure contains a ``forall`` or a constraint arrow ``=>``), then ``e.fld`` desugars to the native record selector: ``fld e``.

2. **Standard path.** In all other cases (the field type is a monotype, or the type of ``e`` is not concretely known), ``e.fld`` desugars to ``getField @"fld" e`` as before.

We define:

- A **concretely known record type** is a type whose outermost constructor is a data type or newtype constructor applied to its arguments (not a type variable, type family application, or ``forall``-quantified type).
- A **polytype** is a type whose top-level structure is ``forall a. tau`` or ``Ctx => tau``, as opposed to a monomorphic ``tau``.
- A field selector ``fld`` is considered available if the record type has a field with that name in scope (i.e., it would be a valid native record selector application).

What changes, what stays the same
---------------------------------

- **No changes** to the ``HasField`` class, its instances, or the constraint solver.
- **No changes** to the behaviour of ``OverloadedRecordDot`` when the field is monomorphic: ``e.fld`` still goes through ``HasField``.
- **No changes** when the record type is not concretely known (e.g., ``e`` is polymorphic): ``e.fld`` still goes through ``HasField``.
- **The only change** is that when the record type is known and the field is polymorphic, the compiler uses the native selector instead of ``HasField``.

Proposed Library Change Specification
-------------------------------------

No changes to library interfaces, ``base``, or ``ghc-experimental`` are required. This is purely a desugaring change within the compiler's typechecker.

Examples
========

HasCallStack-constrained field
------------------------------

.. code:: haskell

   {-# LANGUAGE OverloadedRecordDot #-}

   data R = R { myFunc :: HasCallStack => String -> Int }

   myUsage :: R -> Int
   myUsage r = r.myFunc "42"

**Before:** Compile error: ``No instance for (HasField "myFunc" R (String -> Int))``.

**After:** The typechecker sees that ``r :: R`` is concretely known, and ``myFunc`` has type ``HasCallStack => String -> Int``, which is a polytype. It desugars ``r.myFunc`` to ``myFunc r``, which compiles successfully. The ``HasCallStack`` constraint is propagated to the call site as usual.

forall-constrained field
------------------------

.. code:: haskell

   {-# LANGUAGE OverloadedRecordDot #-}

   data S = S { showable :: forall a. Show a => a -> String }

   useS :: S -> String
   useS s = s.showable 42

**Before:** Compile error: ``No instance for (HasField "showable" S (Int -> String))``.

**After:** The typechecker sees that ``s :: S`` is concretely known, and ``showable`` has a polytype. It desugars ``s.showable`` to ``showable s``, and the ``Show Int`` dictionary is passed at the call site.

Eq-constrained field with a type parameter
------------------------------------------

.. code:: haskell

   {-# LANGUAGE OverloadedRecordDot #-}

   data T a = T { eqField :: Eq a => a -> Bool }

   useT :: T a -> a -> Bool
   useT t x = t.eqField x

**Before:** Compile error.

**After:** ``t :: T a`` is concretely known (the outermost constructor is ``T``), and ``eqField`` is a polytype. Desugars to ``eqField t x``. The ``Eq a`` constraint is propagated from the record selector to the call site.

Monomorphic field: unchanged behaviour
--------------------------------------

.. code:: haskell

   {-# LANGUAGE OverloadedRecordDot #-}

   data U = U { name :: String }

   useU :: U -> String
   useU u = u.name

**Before and after:** ``name`` is a monotype, so this goes through ``HasField`` as before. No change in behaviour.

Polymorphic record variable: unchanged behaviour
------------------------------------------------

.. code:: haskell

   {-# LANGUAGE OverloadedRecordDot #-}

   genericAccess :: (HasField "name" r String) => r -> String
   genericAccess r = r.name

**Before and after:** The type of ``r`` is not concretely known (it is a type variable), so this goes through ``HasField`` as before. No change in behaviour.

Effect and Interactions
=======================

This change resolves GHC Issue `#22267 <https://gitlab.haskell.org/ghc/ghc/-/issues/22267>`_. It interacts seamlessly with existing ``OverloadedRecordDot`` usages because the native fallback only triggers in cases where the compiler currently throws an error. When the field is monomorphic or the record type is not concretely known, behaviour is identical to the status quo.

It aligns with the user's mental model that dot syntax should be a transparent shorthand for record selection.

Costs and Drawbacks
===================

The development cost involves modifying the typechecker to inspect the record type and field type before emitting a ``HasField`` constraint. There are no significant maintenance costs.

A minor drawback is a slight internal divergence in how ``OverloadedRecordDot`` operates (native selector vs. ``HasField`` resolution depending on the field type), but this divergence is entirely invisible to the user and improves the learnability of the language by removing an edge-case error.

Backward Compatibility
======================

This proposal evaluates to **0. No breakage** on the impact scale.

It strictly expands the set of valid programs. Programs that previously failed to compile due to the constrained-field limitation will now compile. No existing valid code will change behaviour or stop working. In particular:

- Monomorphic field access continues to use ``HasField``.
- Polymorphic record variables continue to use ``HasField``.
- Only the previously-errored case (concretely-known record type + polytype field) changes.

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
