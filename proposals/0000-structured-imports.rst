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

This centralisation, however, is deficient in that it precludes any form of imported *module alias sharing*, forcing every module to define their own, *local* aliases (essentially, forcing them to repeatedly re-create any desired namespace structure), which unnecessarily increases costs of module abstraction.

* for the reader, there are no commons to learn and refer to -- every module is a potential snowflake regarding the structure of its namespace (in the alias part)
* for the writer, growing the program and splitting it into modules brings super-linear expenditures for the namespace maintenance aspect (again, in the alias part)

Providing the package author with some controlled way to define a common core of importable module aliases would allow to address both of those problems:

  * defining module::

      module LocalPrelude
        ( module Data.Text      () as T         -- The entire module will be available via the alias
                                                -- ..but only through the alias (level-0 names suppressed)
        , module Data.Text.Lazy    as TL (Text, fromStrict)
                                                -- Only the two names specified will be available via
                                                -- the alias, but everything will be available directly.
        , module SOPPrelude (SOP) aliases (SOP) -- Re-export SOP-the-alias _and_ SOP-the-constructor
        )
      where
      import Data.Text
      import Data.Text.Lazy (Text, pack, unpack, fromStrict)
      import SOPPrelude aliases

  * re-exportable module::

      module SOPPrelude
        ( module Generics.SOP    () as SOP  -- Combining structure into a single level-1 name,
        , module Generics.SOP.NP () as SOP  -- ..while suppressing the level-0 names.
        , SOP, POP, NP, NS, (:.:)           -- Making level-0 names also available.
        )
      where
      import Generics.SOP
      import Generics.SOP.NP

  * user module::

      module User
      where

      import LocalPrelude ((:.:)) aliases -- add '(T, TL, SOP)' to restrict imported aliases

      someFn :: T.Text -> (IO :.: []) TL.Text
      ...

Proposed Change Specification
-----------------------------
1. Semantics of module interface files need to be extended from the current status-quo of a flat set of (regular, *level-0*) names, to allow a single level of nesting, to describe the subsets of names available through *exported module aliases*, aka *level-1 names*.  Note, that the possibility and potential applications of deeper nesting are out of scope of this proposal.

2. The export list entries for modules (`clause 5 in section 5.2 "Export Lists" of the Haskell 2010 report <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1000005.2>`_) should allow an ``as`` keyword to mean that the particular subset of *level-0* names associated with the partially re-exported module (that would otherwise normally be appended to the flat set of the exports of the module being defined), shall instead be available in the importing module through the alias directly following the ``as`` keyword (essentially mimicking the syntax of the import statement). Further:

   1. Modules with export lists *not possessing* any module exports with ``as`` qualifiers are considered as having empty *exported module alias sets* (sets of *level-1* names, alternatively speaking).
   2. It should be entirely possible to have two modules' exports to contribute to the set of names exported through a given alias -- syntactically, by having two more re-exports with the same alias, with a seemingly straightforward accompanying operational representation.
   3. The alias identifier can be optionally followed by a name list further narrowing down the set of names available through the alias.  This again mirrors semantics of the import statement on the export side.
   4. The extended *level-1* export specification is entirely orthogonal to the normal *level-0* export specification, (as per clause 5 in section 5.2 of Haskell 2010).

3. The ``import`` statement should be extended with ways to opt into the structural exports:

   1. ``import Module.Name aliases``, for a blanket import of all aliases exported by the module, intended for a more Prelude-like semantic.
   2. ``import Module.Name aliases (..module-alias-explicit-list..)``, for a selective module alias import.  Explicit import of an alias not exported by the module being imported is a compile-time error.
   3. ``import Module.Name aliases_hiding (..module-alias-hide-list..)``, same, as previous, but negated.

   Note, that specifying the ``aliases`` keyword is orthogonal to the regular (*level-0*) imported names, and should not affect the regular abilities:

   1. ..to specify non-alias names to import, in the same ``import`` statement,
   2. ..to specify an additional, *local* alias for module carrying these names, using the normal ``as`` keyword,
   3. ..to restrict the imports for non-alias names to only their ``qualified`` form.

4. In a similar vein, the export list entries shall be *additionally* extended to allow ``aliases`` and ``aliases_hiding`` keywords to signify a request to re-export a subset of aliases previously imported from another module. The keyword is (optionally, in cases of ``aliases``) followed by a name subset specification list.  This extension allows for a controlled, but non-obstructed flow of level-1 names across modules.

5. All of the above to be guarded, naturally by a language pragma, such as ``StructuredImports``, or ``SmugglingAliases``.

Effect and Interactions
-----------------------
Package author will be have an option of conveniently setting up coherent namespaces for their entire packages (or their desired subsets), by potentially specifying the entire shared namespace structure in a single file.

The natural divergences and ambiguities of things like ``T`` meaning ``Data.Text`` or ``Data.Text.Lazy``, ``Map`` meaning ``Data.Map`` or ``Data.Map.Strict`` -- all those will have a concise and effective way of being addressed by a policy that will become expressible.

It could be that the user might opt to implicitly (and potentially confusingly for themselves) request overlapping imports for a given alias, either through a pair of un-restricted ``aliases``-augmented import statements, or through a coincidence of one such statement with another import statement carrying a local alias declaration, leading to the same module alias being implicitly populated by different modules.  To this possibility, it's worth noting that:

  1. the feature is strictly opt-in, on both import and export sides,
  2. the language user community is already prepared to deal with a similar problem in context of regular unrestricted imports,
  3. we provide an option for restricting the structured imports, for cases where a particular situation makes it concerning.

No known interactions with other features.

Costs and Drawbacks
-------------------
One unavoidable downside is the necessary complication in the module interface machinery -- we're now assigning structure to the previously unstructured set of names exchanged between modules, and that structure needs a material carrier.

There appear to be no semantic costs for the non-users (``StructuredImports`` not enabled in either module will result in simple, predictable, customary behavior).

There appears to be no cost whatsoever associated with handling of the modules compiled without the extension enabled.

Introduction of the new stolen keywords (``aliases`` and ``aliases_hiding``) is an opt-in cost for new users of the extensions.

The costs regarding processing of modules with the extension enabled should be:

1. Constrained to the module processing (compilation/linking) time,
2. Proportional to the complexity of the namespaces defined.

Alternatives
------------
A widely used alternative is disciplined copy-pasting of locally-aliased module imports between modules.  But avoiding reliance on human perfection is specifically part of our goal.

Unresolved questions
--------------------
1. It could be that we might assign some useful meaning to hierarchies deeper than 0 and 1, but that currently lacks obvious motivation.

2. The ``aliases`` and derived ``aliases_hiding`` keywords, while reusing a customary term which appears quite appropriate, misses the larger point of us introducing structure to the import/export language.  Perhaps a better name for this semantic is worth thinking of.

3. The ``aliases_hiding`` keyword is a bit ugly.

Implementation Plan
-------------------
1. ``HscTypes.ModIface`` will have to be extended to allow a shallowly hierarchical structure, possibly at the ``mi_exports`` field.
2. It's unclear what, but some internal types (those tracking module composition before it gets serialised) will also need to be changed.
3. Parser changes are inevitable.
4. Something else?
