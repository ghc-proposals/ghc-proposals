Structured module exports/imports
=================================

.. proposal-number:: 
.. trac-ticket:: 
.. implemented:: Not yet
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/205>`_.
.. sectnum::
.. contents::

The inability to assign basic structure in the module import/export language leads to currently inavoidable cognitive costs at the module boundary.  A semantic extension at the module interface level, coupled with syntactic extension on both export and import sides would give language users a tool to control these costs.


Motivation
----------
Preludes (meaning, broadly, all modules with reexports) allow us to (partially) centralise namespace formation, which serves several purposes:

* compressing the total set of import statements in the overarching program
* providing authoritative decisions on the direct availability of names -- essentially providing a common language

This centralisation, however, is deficient in that it precludes any form of imported *module alias sharing*, forcing every module to define their own, *local* aliases, which unnecessarily increases costs of module abstraction.

* for the reader, there are no commons to learn and refer to -- every module is a potential snowflake regarding the structure of its namespace (in the alias part)
* for the writer, growing the program and splitting it into modules brings super-linear expenditures for the namespace maintenance aspect (again, in the alias part)

Providing the package author with some controlled way to define a common core of importable module aliases would allow to address both of those problems.

Proposed Change Specification
-----------------------------
1. Semantics of module interface files need to be extended from the current status-quo of a flat set of (regular, *level-0*) names, to allow a single level of nesting, to describe the subsets of names available through *exported module aliases*, aka *level-1 names*.  Note, that the possibility and potential applications of deeper nesting are out of scope of this proposal.

2. The export list entries for modules (`clause 5 in section 5.2 "Export Lists" of the Haskell 2010 report <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1000005.2>`_) should allow an ``as`` qualifier to mean that the particular subset of *level-0* names associated with the partially re-exported module (that would otherwise normally be appended to the flat set of the exports of the module being defined), shall instead be available in the importing module through the alias directly following the ``as`` qualifier (essentially mimicking the syntax of the import statement). Modules with export lists *not posessing* any module exports with ``as`` qualifiers are considered as having empty *exported module alias sets* (sets of *level-1* names, alternatively speaking).

3. The ``import`` statement should be extended with ways to opt into the structural exports:

   1. ``import Module.Name aliases``, for a blanket import of all aliases exported by the module, intended for a more Prelude-like semantic.
   2. ``import Module.Name aliases (..module-alias-explicit-list..)``, for a selective module alias import.  Explicit import of an alias not exported by the module being imported is a compile-time error.
   3. ``import Module.Name aliases-hiding (..module-alias-hide-list..)``, same, as previous, but negated.

   Note, that specifying the ``aliases`` keyword is orthogonal to the regular (*level-0*) imported names, and should not affect the regular abilities:

   1. ..to specify non-alias names to import, in the same ``import`` statement,
   2. ..to specify an additional, *local* alias for module carrying these names, using the normal ``as`` keyword,
   3. ..to restrict the imports for non-alias names to only their ``qualified`` form.

4. All of the above to be guarded, naturally by a language pragma, such as ``StructuredImports``, or ``SmugglingAliases``.

Effect and Interactions
-----------------------
Package author will be have an option of conveniently setting up coherent namespaces for their entire packages (or their desired subsets), by potentially specifying the entire shared namespace structure in a single file.

The natural divergences and ambiguities of things like ``T`` meaning ``Data.Text`` or ``Data.Text.Lazy``, ``Map`` meaning ``Data.Map`` or ``Data.Map.Strict`` -- all those will have a concise and effective way of being addressed by a policy that will become expressible.

It could be that the user might opt to implicitly (and potentially confusingly for themselves) request overlapping imports for a given alias, either through a pair of un-restricted ``aliases``-augmented import statements, or through a coincidence of one such statement with another import statement carrying a local alias declaration, leading to the same module alias being implicitly populated by different modules.  To this possibility, it's worth noting that:

  1. the feature is strictly opt-in, on both import and export sides,
  2. the language user community is already prepared to deal with a similar problem in context of regular unrestricted imports,
  3. we provide an option for restricting the structured imports, in case the individual user perceives it concerning.

No known interactions with other features.

Costs and Drawbacks
-------------------
One unavoidable downside is the necessary complication in the module interface machinery -- we're now assigning structure to the previously unstructured set of names exchanged between modules, and that structure needs a material carrier.

There appear to be no semantic costs for the non-users (``StructuredImports`` not enabled in either module will result in simple, predictable, customary behavior).

There appears to be no runtime cost whatsoever associated with handling of the modules compiled without the extension enabled.

The costs regarding processing of modules with the extension enabled should be:

1. Constrained to the module processing (compilation/linking) time,
2. Proportional to the complexity of the namespaces defined.

Alternatives
------------
A widely used alternative is disciplined copy-pasting of module import statements across modules.  But that is specifically part of the problem we're trying to solve.

Unresolved questions
--------------------
1. It could be that we might assign some useful meaning to hierarchies deeper than 0 and 1, but that currently lacks obvious motivation.

2. The ``aliases`` keyword, while reusing a customary term which appears quite appropriate, misses the larger point of us introducing structure to the import/export language.  Perhaps a better name for this semantic is worth thinking of.

Implementation Plan
-------------------
1. ``HscTypes.ModIface`` will have to be extended to allow a shallowly hierarchical structure, possibly at the ``mi_exports`` field.
2. It's unclear what, but some internal types (those tracking module composition before it gets serialised) will also need to be changed.
3. Parser changes are inevitable.
4. Something else?
