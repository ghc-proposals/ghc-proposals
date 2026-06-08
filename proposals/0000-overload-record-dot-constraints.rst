Built-in HasField Solver Support for Polytype Fields under OverloadedRecordDot
==============================================================================

.. author:: Gautier DI FOLCO
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/761>`_.
.. sectnum::
.. contents::

We propose modifying the built-in ``HasField`` constraint solver so that, when the record type is concretely known and the field has a polymorphic type (involving ``forall`` or typeclass constraints), the solver constructs proper evidence by decomposing the selector type and emitting the field's constraints as separate subgoals. This resolves GHC Issue `#22267 <https://gitlab.haskell.org/ghc/ghc/-/issues/22267>`_ without any changes to the renamer or desugaring.

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

More precisely, the built-in ``HasField`` solver (``matchHasField`` in ``GHC.Tc.Instance.Class``) locates the record selector and creates an equality constraint between the selector's type and ``r -> a``. For a constrained field, the selector has a type like ``R -> (HasCallStack => String -> Int)``, which in GHC Core is ``R -> HasCallStack -> String -> Int`` (a three-argument function). But the expected type from the ``HasField`` constraint is ``R -> String -> Int`` (a two-argument function). The equality ``R -> HasCallStack -> String -> Int ~ R -> String -> Int`` fails because the types have different arities.

For ``forall``-quantified fields, the situation is different: the check ``hasNoForallsTy`` detects the ``ForAllTy`` in the selector type and the solver falls back to looking for user-defined instances, of which there are none.

In both cases, the native record selector works correctly, but the solver cannot bridge the gap between the selector's polytype and the monomorphic ``a`` expected by ``HasField``.

Goal
----

Dot syntax should work on all fields of a record whose type is concretely known, regardless of whether the field type is monomorphic or polymorphic. The fix should be localized to the built-in ``HasField`` solver, with no changes to the renamer, desugaring, or the ``HasField`` class itself.

Proposed Change Specification
=============================

We modify the built-in ``HasField`` solver (``matchHasField`` in ``GHC.Tc.Instance.Class``). The renamer continues to desugar ``e.fld`` to ``getField @"fld" e`` as before. The change is entirely in how the solver constructs evidence when it finds a polytype field selector.

Phase 1: Constraint-only fields
-------------------------------

When the solver locates a record selector whose result type has constraint arrows (``=>``) not present in the expected type ``r -> a``, instead of emitting a single equality between the full selector type and ``r -> a`` (which fails due to arity mismatch):

1. **Decompose** the instantiated selector type into the record-accepting prefix (``r ->``), the constraint arguments, and the body type.

2. **Emit** each constraint argument as a separate wanted constraint.

3. **Emit** an equality between the body type and ``a``.

4. **Build evidence** by eta-expansion: ``\r -> sel r ?dict1 ... ?dictN``, where each ``?dictN`` is the evidence for the corresponding constraint wanted.

For example, given ``HasField "myFunc" R (String -> Int)`` where the selector is ``myFunc :: R -> (HasCallStack => String -> Int)``:

- Decompose: record arg ``R``, constraint ``HasCallStack``, body ``String -> Int``
- Emit wanted: ``HasCallStack``
- Emit equality: ``String -> Int ~ String -> Int`` (trivially solved)
- Evidence: ``\r -> myFunc r ?hasCallStack`` of type ``R -> String -> Int``

Phase 2: ``forall``-quantified fields (stretch goal)
----------------------------------------------------

When the selector's result type contains a ``forall``, the solver additionally:

1. **Instantiates** the ``forall``-bound type variables with the types determined by the expected result type ``a`` (e.g., if ``a = Int -> String`` and the field is ``forall x. Show x => x -> String``, instantiate ``x := Int``).

2. **Emits** the resulting constraints as wanteds (e.g., ``Show Int``).

3. **Builds evidence** by type application of the selector to the instantiated types, with dictionary arguments filled by the constraint evidence.

Standard path (unchanged)
-------------------------

When the field type is a monotype, the solver behaves exactly as it does today: it emits a single equality ``sel_ty ~ (r -> a)`` and builds evidence by casting the selector. No change.

We define:

- A **concretely known record type** is a type whose outermost constructor is a data type or newtype constructor applied to its arguments (not a type variable, type family application, or ``forall``-quantified type). This is already checked by ``lookupHasFieldLabel`` via ``tcSplitTyConApp_maybe``.
- A **polytype** is a type whose top-level structure is ``forall a. tau`` or ``Ctx => tau``, as opposed to a monomorphic ``tau``.
- A field selector ``fld`` is considered available if the record type has a field with that name in scope (i.e., it would be a valid native record selector application). This is already checked by ``lookupHasFieldLabel``.

What changes, what stays the same
---------------------------------

- **No changes** to the renamer or desugaring: ``e.fld`` still desugars to ``getField @"fld" e`` in all cases.
- **No changes** to the ``HasField`` class, its instances, or its functional dependency.
- **No changes** to the behaviour of ``OverloadedRecordDot`` when the field is monomorphic: the solver uses the same equality-based evidence as before.
- **No changes** when the record type is not concretely known (e.g., ``e`` is polymorphic): the solver cannot locate a selector and falls back to user instances as before.
- **The only change** is in the built-in solver's evidence construction: when the record type is concretely known and the field is a polytype, the solver decomposes the selector type and builds eta-expanded evidence with separate constraint subgoals, instead of emitting a doomed equality or falling back.

Proposed Library Change Specification
-------------------------------------

No changes to library interfaces, ``base``, or ``ghc-experimental`` are required. This is purely a change within the compiler's built-in ``HasField`` solver.

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

**After:** The solver finds selector ``myFunc :: R -> (HasCallStack => String -> Int)``. It decomposes the type: record arg ``R``, constraint ``HasCallStack``, body ``String -> Int``. It emits ``HasCallStack`` as a wanted, checks ``String -> Int ~ String -> Int`` (trivial), and builds evidence ``\r -> myFunc r ?hasCallStack``. The ``HasCallStack`` constraint is propagated to the call site as usual.

forall-constrained field
------------------------

.. code:: haskell

   {-# LANGUAGE OverloadedRecordDot #-}

   data S = S { showable :: forall a. Show a => a -> String }

   useS :: S -> String
   useS s = s.showable 42

**Before:** Compile error: ``No instance for (HasField "showable" S (Int -> String))``.

**After (Phase 2):** The solver finds selector ``showable :: S -> (forall a. Show a => a -> String)``. It instantiates ``a := Int`` from the expected result type ``Int -> String``, emits ``Show Int`` as a wanted, and builds evidence with the instantiated selector. The ``Show Int`` dictionary is resolved at the call site.

Eq-constrained field with a type parameter
------------------------------------------

.. code:: haskell

   {-# LANGUAGE OverloadedRecordDot #-}

   data T a = T { eqField :: Eq a => a -> Bool }

   useT :: T a -> a -> Bool
   useT t x = t.eqField x

**Before:** Compile error.

**After:** ``t :: T a`` is concretely known (the outermost constructor is ``T``), and ``eqField`` has a constrained type. The solver finds selector ``eqField :: T a -> (Eq a => a -> Bool)``, decomposes it, emits ``Eq a`` as a wanted, and builds eta-expanded evidence. The ``Eq a`` constraint is propagated from the record selector to the call site.

Monomorphic field: unchanged behaviour
--------------------------------------

.. code:: haskell

   {-# LANGUAGE OverloadedRecordDot #-}

   data U = U { name :: String }

   useU :: U -> String
   useU u = u.name

**Before and after:** ``name`` is a monotype, so the solver uses the same equality-based evidence as today. No change in behaviour.

Polymorphic record variable: unchanged behaviour
------------------------------------------------

.. code:: haskell

   {-# LANGUAGE OverloadedRecordDot #-}

   genericAccess :: (HasField "name" r String) => r -> String
   genericAccess r = r.name

**Before and after:** The type of ``r`` is not concretely known (it is a type variable), so the solver cannot locate a selector and falls back to the user-provided ``HasField`` constraint. No change in behaviour.

Effect and Interactions
=======================

This change resolves GHC Issue `#22267 <https://gitlab.haskell.org/ghc/ghc/-/issues/22267>`_. It interacts seamlessly with existing ``OverloadedRecordDot`` usages because the solver change only affects cases where the compiler currently throws an error. When the field is monomorphic or the record type is not concretely known, behaviour is identical to the status quo. All field access continues to go through ``HasField`` uniformly -- the change is in how evidence is constructed, not in whether ``HasField`` is used.

It aligns with the user's mental model that dot syntax should be a transparent shorthand for record selection.

Costs and Drawbacks
===================

The development cost involves modifying the built-in ``HasField`` solver in ``GHC.Tc.Instance.Class`` to decompose the selector type, emit constraint subgoals, and build eta-expanded evidence. There are no significant maintenance costs.

A minor drawback is that the evidence construction in ``matchHasField`` becomes more complex, as it must now handle the decomposition of polytype selector types rather than relying on a single equality constraint.

Phase 2 (``forall``-quantified fields) introduces additional complexity: the solver must instantiate ``forall``-bound variables with types determined from the expected result, which touches on impredicative territory. This is why Phase 2 is designated as a stretch goal.

Backward Compatibility
======================

This proposal evaluates to **0. No breakage** on the impact scale.

It strictly expands the set of valid programs. Programs that previously failed to compile due to the constrained-field limitation will now compile. No existing valid code will change behaviour or stop working. In particular:

- Monomorphic field access continues to use ``HasField``.
- Polymorphic record variables continue to use ``HasField``.
- Only the previously-errored case (concretely-known record type + polytype field) changes.

Alternatives
=============

Renamer-Level Native Selector Fallback
---------------------------------------

An earlier version of this proposal suggested modifying the desugaring of ``e.fld`` so that, when the record type is concretely known and the field is a polytype, ``e.fld`` desugars to the native record selector ``fld e`` instead of ``getField @"fld" e``.

This approach has a fundamental problem: the desugaring of ``OverloadedRecordDot`` happens in the **renamer** (``GHC.Rename.Expr``), where no type information is available. Determining whether the type of ``e`` is "concretely known" and whether the field is a "polytype" requires type information that only exists in the typechecker. Ad-hoc type guessing in the renamer was previously attempted for ``DuplicateRecordFields`` and proved fragile (see `proposal 0366 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0366-no-ambiguous-field-access.rst>`_).

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

- For Phase 2 (``forall``-quantified fields), what is the precise strategy for instantiating ``forall``-bound variables from the expected result type? Should this use Quick Look impredicativity, or a simpler targeted instantiation within ``matchHasField`` only?

Implementation Plan
===================

The change is localized to ``matchHasField`` in ``compiler/GHC/Tc/Instance/Class.hs``. The existing ``lookupHasFieldLabel`` function already performs the "concretely known record type" check and returns the selector name, so the modification is in the evidence construction after the lookup succeeds:

1. After ``tcInstType`` produces the instantiated selector type ``sel_ty``, check whether ``sel_ty`` has invisible (constraint) arguments beyond what ``r -> a`` expects.
2. If so, decompose ``sel_ty`` into its constraint prefix and body, emit the constraints as wanteds, and build eta-expanded evidence.
3. If not (monomorphic field), proceed with the existing equality-based evidence.

Phase 2 would add ``forall``-instantiation logic in the same function.

If accepted, I can rework `#14391 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/14391>`_.
