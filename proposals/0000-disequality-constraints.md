Extending Constraint Syntax to Include Disequality Constraints (/~)
====================================================================

.. author:: Art Watkins
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

This proposal introduces a new constraint syntax ``a /~ b`` in GHC to represent disequality constraints, asserting that the types ``a`` and ``b`` must be distinct. This complements the existing equality constraint ``a ~ b`` and enhances the expressiveness of Haskell's type system for type-level programming, including type classes, GADTs, and type families.

Motivation
----------
Haskell’s type system currently supports equality constraints (``a ~ b``) to enforce that two types are identical, but it lacks a direct mechanism to specify that two types must differ. This forces developers to rely on indirect workarounds, such as auxiliary type classes or GADTs, which are often less intuitive and more verbose. The proposed ``a /~ b`` syntax provides a natural and concise way to express disequality, addressing use cases like:

- **Type Class Instances**: Enabling precise control over overlapping instances by distinguishing cases where types are equal versus distinct.
- **GADTs**: Allowing constructors to depend on types being different, enhancing type safety and expressiveness.
- **Type Families**: Offering finer control over type family instances by restricting them to distinct types.

For example, consider a scenario where a type class should behave differently when types are equal versus distinct:

::

    class Bar a b where
      bar :: a -> b -> String

    instance Bar a b where a ~ b => Bar a b where
      bar x y = "Equal types"

    -- Without /~, we’d need a workaround for the distinct case
    instance Bar a b => Bar a b where
      bar x y = "Different types"  -- Overlaps and is imprecise

With ``a /~ b``, the distinct case becomes explicit and clear, improving both usability and type system symmetry.

Proposed Change Specification
-----------------------------
This proposal extends GHC with the following precise changes:

1. **Syntax Extension**:
   - Introduce ``a /~ b`` as a new constraint syntax, parsed analogously to ``a ~ b``, applicable in type class contexts, GADT constructors, and type family instances.

2. **Internal Representation**:
   - Extend the ``Ct`` data type in GHC’s type checker with a new constructor, such as ``CNotEqCan``, to represent disequality constraints between types ``a`` and ``b``.

3. **Constraint Solving**:
   - Collect disequality constraints during type checking.
   - After applying the final type substitution, verify that ``a`` and ``b`` in each ``a /~ b`` do not unify. If they unify, raise a type error.

4. **Integration with Existing Features**:
   - Ensure compatibility with type families, GADTs, and type class resolution, allowing disequality constraints to influence instance selection and constructor applicability.

**Formal Specification**:

- **Syntax**:
  - Type class context: ``class Foo a b where { ... } where a /~ b``
  - GADT constructor: ``data Bar where { MkBar :: a /~ b => a -> b -> Bar }``
  - Type family instance: ``type instance F a b = ... where a /~ b``

- **Semantics**:
  - The constraint ``a /~ b`` is satisfied if and only if ``a`` and ``b`` are distinct types after all substitutions are applied.
  - If ``a`` and ``b`` unify to the same type, the constraint fails, resulting in a type error.

- **Type Checking**:
  - Treat ``a /~ b`` as a "wanted" constraint, evaluated after unification using GHC’s type equality test.
  - If ``a`` equals ``b``, report an error: "Type error: Constraint ``a /~ b`` violated: ``a`` and ``b`` are both ``<type>``."

This aligns with the **Syntactic Unification Principle (SUP)** by extending constraints consistently with existing syntax and respects the **Lexical Scoping Principle (LSP)** by avoiding scoping ambiguities.

Proposed Library Change Specification
-------------------------------------
This proposal does not modify the ``base`` library or other libraries under the Core Libraries Committee’s purview. It is a pure language extension, introducing no new types or functions requiring library changes. Any future extensions leveraging this feature can be proposed separately for ``ghc-experimental`` or ``base`` as needed.

Examples
--------
1. **Type Class Instances**:
   ::

     class Bar a b where
       bar :: a -> b -> String

     instance Bar a b where a ~ b => Bar a b where
       bar x y = "Equal types"

     instance Bar a b where a /~ b => Bar a b where
       bar x y = "Different types"

     test1 :: String
     test1 = bar True False  -- "Different types" (Bool /~ Int)

     test2 :: String
     test2 = bar True True   -- "Equal types" (Bool ~ Bool)

2. **GADTs**:
   ::

     data Foo where
       MkFoo :: a /~ b => a -> b -> Foo

     foo :: Foo
     foo = MkFoo 'a' True  -- Valid: Char /~ Bool

     -- Invalid:
     -- bar :: Foo
     -- bar = MkFoo 'a' 'b'  -- Error: Char ~ Char violates a /~ b

Effect and Interactions
-----------------------
The ``a /~ b`` constraint directly addresses the motivation by providing a clear, symmetric way to enforce type distinctness. It enhances:

- **Type Class Resolution**: Enables precise instance selection, reducing overlap ambiguity.
- **GADTs**: Allows constructors to encode type-level conditions more naturally.
- **Type Families**: Restricts instances to distinct types, improving type safety.

It integrates with existing features by being checked post-unification, ensuring no disruption to the current solving process while adding expressive power. No contentious interactions are anticipated, though community feedback may highlight edge cases.

Costs and Drawbacks
-------------------
- **Development Cost**: Moderate. Requires updates to the parser, type checker, and solver, but leverages existing infrastructure for equality constraints.
- **Maintenance Cost**: Low. The feature is self-contained and aligns with current type system design.
- **Learnability**: Minimal impact. The syntax mirrors ``~``, and disequality is a familiar concept, though novices may need examples to grasp its use.
- **Drawbacks**: Risk of confusing type errors if misused, mitigated by descriptive error messages.

Backward Compatibility
----------------------
This proposal adheres to the **Opt-In Principle (OIP)**, as ``a /~ b`` is optional and unused in existing code unless explicitly adopted. It introduces no breakage (level 0), as it does not alter the behavior of programs not using the new syntax. It complies with the **General Rule (GR1)** for stability, ensuring no impact on stable Haskell packages.

Alternatives
------------
- **Status Quo**: Use indirect encodings (e.g., type classes or GADTs), which are less concise and intuitive.
- **Type-Level Negation**: A broader negation mechanism could be introduced, but it would be more complex and less focused on disequality.

The ``/~`` syntax was chosen for its similarity to ``~``, ensuring consistency. Alternatives like ``/=`` or ``≁`` were considered but rejected as less intuitive or potentially conflicting.

Unresolved Questions
--------------------
- **Syntax Choice**: Is ``/~`` the best symbol, or should alternatives be revisited?
- **Type Family Interaction**: How do disequality constraints interact with type family reductions in complex cases?
- **Instance Resolution**: How do they affect overlapping instance resolution in edge cases?

Implementation Plan
-------------------
1. **Parser Extension**: Add ``a /~ b`` to the parser.
2. **Internal Representation**: Introduce a ``CNotEqCan`` constructor to ``Ct``.
3. **Constraint Solver**: Update the solver to check disequality post-substitution.
4. **Testing**: Create test cases for syntax, semantics, and errors.
5. **Documentation**: Update the GHC User’s Guide.

The proposer will implement this, with GHC team support.

Endorsements
-------------
(Optional) Community members can add endorsements here to demonstrate demand for the feature.
