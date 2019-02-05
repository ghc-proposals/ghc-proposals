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
Simplifed understanding of the inter-module name flow
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
To establish a compact mental model of the inter-module name flow *status quo*, and by extension, the proposal, it's useful to get a hold of two facts:

Exports are mediated by a flat set
*************************************
The inter-module name flow is not only affected by the various combinations of import and export statements, but is also instrumentally limited by what the serialised module interface can express, as represented by ``.hi`` files and the ``HscTypes.ModIface`` type.  While the latter needs not *necessarily* directly correspond to anything at language semantics level, it does have a pretty direct correspondence to the notions of the inter-module name flow.

To establish that, one simply needs to make the following observations regarding the namespace due for export by a module:

1. The namespace due for export is a flat set (which is our problem), which is established by *Section 5.2 of Haskell2010*:

       Exports lists are cumulative: the set of entities exported by an export list is the union of the entities exported by the individual items of the list.
       ...
       The *unqualified* names of the entities exported by a module must all be distinct (within their respective namespace).

2. This flat set is essentially serialised into the module interface as the ``mi_exports`` field of ``HscTypes.ModIface``, which is a list.
3. Requirements of separate compilation necessitate that the inbound name flow is entirely defined by the information available from this serialised module interface.

Furthermore, *Section 5.2 of Haskell2010* underscores that:

   It makes no difference to an importing module how an entity was exported. For example, a field name f from data type T may be exported individually (f, item (1) above); or as an explicitly-named member of its data type (T(f), item (2)); or as an implicitly-named member (T(..), item(2)); or by exporting an entire module (module M, item (5)).

As a result, we are free not to care during import, how exactly the names were exported by the module being imported.  The entirety of relevant information is the *flat export set*.

Name grouping
    Note, however, that it is also true that the exported names still retain natural *grouping*:

    * methods and associated types within type classes
    * constructors and field names within ADTs

    This grouping, though, does change nothing in our calculations.

The export list refers to the local namespace structure
**********************************************************
The final result of the inter-module flow is the local namespace structure of the importing module.  And it is this local namespace structure that the export statements refer to.

This can be established by analysis of the five clauses of *Section 5.2. of Haskell2010*, that enumerate entries allowed in the export list.  All of those entries refer to the structured local namespace, and none of them make any distinction on how the elements of that namespace came to be -- all that matters is that the relevant names are in scope.

As a result, we are free not to care during export, how exactly the names being exported were imported by the module.  The entirety of relevant information is the *local namespace structure*.

Terminology and status quo of the inter-module name flow
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Building on the established simplifications, let's describe the status quo.

First, it's clear that we can separate discussion of exports from the discussion of imports:

1. how ``export`` declarations transform the locally-structured namespace names into the flat set of exported names, and
2. how ``import`` declarations transform the flat set of imported names into the locally-structured namespace.

Exports
*******

.. list-table:: Exports in module Z.Y.X
   :header-rows: 1

   * - #
     - Export list entry
     - Effect on export set
   * - 1
     - ``names``..
     - append the specified set of names to ``Z.Y.X``'s export set
   * - 2
     - ``module A.B.C``
     - of the potentially restricted set of names under the multi-component module name ``A.B.C``, append all to ``Z.Y.X``'s export set
   * - 3
     - ``module M`` (no dots in ``M``)
     - of the set of names composed under the single-component module name ``M``, append all to ``Z.Y.X``'s export set

.. sidebar:: Composed single-component module names

   Due to the mechanics of imports discussed later, the single-component module names are different from multi-component module names in that they can contain a sum of exports from different modules.

   This fact, though, does change nothing in our calculations.

Imports
*******
.. list-table:: Imports in module W
   :header-rows: 1

   * - #
     - Import declaration
     - Effect on W's namespace
   * - 1
     - ``import           Z.Y.X``
     - Append all of ``Z.Y.X``'s export set to the top level of the namespace, and also under ``Z.Y.X``
   * - 2
     - ``import           Z.Y.X        (adds..)``
     - Append the specified subset of ``Z.Y.X``'s export set to the top level of the namespace, and also under ``Z.Y.X``
   * - 3
     - ``import           Z.Y.X hiding (subs..)``
     - Append the ``Z.Y.X``'s export set, with ``subs`` subtracted, to the top level of the namespace, and also under ``Z.Y.X``
   * - 4
     - ``import qualified Z.Y.X``
     - Append all of ``Z.Y.X``'s export set under ``Z.Y.X``
   * - 5
     - ``import qualified Z.Y.X          as W``
     - Append all of ``Z.Y.X``'s export set under ``W``
   * - 6
     - ``import           Z.Y.X          as W``
     - Append all of ``Z.Y.X``'s export set to the top-level, and also under ``W``
   * - 7
     - ``import           Z.Y.X (adds..) as W``
     - Append the specified subset of ``Z.Y.X``'s export set to the top-level, and also under ``W``

Subsetting imports
    Note that in interests of brevity, we only illustrated import subsetting (with ``(adds..)`` and  ``hiding (subs..)``) for the unqualified/unaliased case -- while it unambiguously extends to the rest of the cases.

Semantics of the ``qualified`` keyword
    It's worth underscoring the effect of the ``qualified`` keyword -- it is strictly negative, as it suppresses population of the top level of the namespace.

Proposed change
^^^^^^^^^^^^^^^
Terminology
***********
Level-0 names
  Intra-module names (regardless of introduction), which reside at the top level of its structured namespace, and which therefore cannot be subject to the *"dot operator"* of the module system.  The only names that used to be able to travel across module boundaries.

Level-1 names
  Intra-module names (regardless of introduction), that have a single component (no dots), and carry a set of `level-0 names`_, that are individually accessible by the *"dot operator"* of the module system.  It is these names that we propose allow travelling across module boundaries, along with their associated content.

Higher-level names
  Intra-module names (regardless of introduction), that carry non-level-0 names accessible by the dot syntax.  Note that while the heading section of Chapter 5 of the *Haskell 2010 Language Report* says:

    Module names can be thought of as being arranged in a hierarchy in which appending a new component creates a child of the original module name. For example, the module Control.Monad.ST is a child of the Control.Monad sub-hierarchy.

  ..it also says:

    This is purely a convention, however, and not part of the language definition; in this report a modid is treated as a single identifier occupying a flat namespace.

  It is indeed this *"thought of"* angle that we're referring to here -- the structure of higher-level names has no effect on semantics, but merely gives us a chance to establish a hopefully more enlightening terminology.

New syntax summary: exports
***************************
.. list-table:: Exports in module Z.Y.X
   :header-rows: 1

   * - #
     - Export list entry
     - Effect on structured export namespace
   * - 1
     - ``module A.B.C                 as D``..
     - Append the entire set of *level-0* names available in the scope through the local namespace entry ``A.B.C`` to the set under the ``D`` alias (*level-1 name*) in the ``Z.Y.X``'s structured export namespace.
   * - 2
     - ``module A.B.C        (adds..) as D``..
     - Append the specified subset of *level-0* names available in the scope through the local namespace entry ``A.B.C`` to the set under the ``D`` alias (*level-1 name*) in the ``Z.Y.X``'s structured export namespace.
   * - 3
     - ``module A.B.C hiding (subs..) as D``..
     - Append the set of *level-0* names available in the scope through the local namespace entry ``A.B.C``, with ``subs`` subtracted, to the set under the ``D`` alias (*level-1 name*) in the ``Z.Y.X``'s structured export namespace.
   * - 4
     - ``module A.B.C aliases``..
     - Append all *level-1 names* carried by the structured export namespace of the ``A.B.C`` module as *level-1 names* in the ``Z.Y.X``'s structured export namespace.
   * - 5
     - ``module A.B.C aliases        (adds..)``..
     - Append the specified subset of *level-1 names* carried by the structured export namespace of the ``A.B.C`` module as *level-1 names* in the ``Z.Y.X``'s structured export namespace.
   * - 6
     - ``module A.B.C aliases_hiding (subs..)``..
     - Append all *level-1 names* carried by the structured export namespace of the ``A.B.C`` module, with the ``subs`` set subtracted, as *level-1 names* in the ``Z.Y.X``'s structured export namespace.

Export ``as`` targets
    The *level-1* name (alias) following the ``as`` keyword must have a single component under this proposal.  Multi-component *level-1* names are explicitly out of scope.

Export ``as`` sources
    While we used a multi-component module name in the example of the ``as`` export source, it doesn't matter, in principle, and a single-component module name would do as well.  What matters is that it is brought into scope by an import declaration as a non-level-0 name.

Role of the ``qualified`` keyword
    As mentioned in the *status quo* section, the ``qualified`` keyword has strictly negative semantics in the non-extended semantics: it prevents *level-0* names from being made available at the top level of the local namespace.  In this light, a natural meaning for this keyword in the context of *level-1* name introduction does not appear to exist.

New syntax summary: imports
***************************
.. list-table:: Imports in module W
   :header-rows: 1

   * - #
     - Import declaration
     - Effect on W's namespace
   * - 1
     - ``import           Z.Y.X aliases``
     - Append all of ``Z.Y.X``'s exported *level-1 names* (aliases) to the importing module's local namespace as *level-1 names* (aliases), with the entirety of their *level-0* name content available via *"dot operator"* of the module system.
   * - 2
     - ``import           Z.Y.X aliases        (names..)``
     - Append the specified subset of ``Z.Y.X``'s exported *level-1 names* (aliases) to the importing module's local namespace as *level-1 names* (aliases), with the entirety of every alias's *level-0* name content available via the *"dot operator"* of the module system.
   * - 3
     - ``import           Z.Y.X aliases_hiding (names..)``
     - Append the ``Z.Y.X``'s exported *level-1 names* (aliases) to the importing module's local namespace as *level-1 names* (aliases), without the *level-1* ``names``, with the entirety of every alias's *level-0* name content available via the *"dot operator"* of the module system.

Changes to the operational semantics
************************************
Semantics of module interface files need to be extended from only allowing the current status-quo of a flat set of (regular, *level-0*) exported names, to allow a introduction of recording *level-1 names*, along with their associated *level-0* content.

More specifically, in the ``mi_exports`` field of ``HscTypes.ModIface`` we're going from ``[IfaceExport]`` to something like ``Map ModuleName IfaceExport`` (while also enforcing that ``ModuleName`` corresponds to a *level-1* name, i.e. has no dots).

NOTE, implementation options
  1. Keeping the type and semantics of the ``mi_exports`` field as-is, and adding the new semantics to a new field, such as ``mi_exports_level1`` -- which would be less disruptive, but also less clean in the long run.
  2. Changing the semantics as described above.

Gating the functionality
************************
The new semantics are to be guarded by a language pragma, such as ``StructuredImports``, or ``SmugglingAliases``.

Effect and Interactions
-----------------------
Package author will be have an option of conveniently setting up coherent namespaces for their entire packages (or their desired subsets), by potentially specifying the entire shared namespace structure in a single file.

The natural divergences and ambiguities of things like ``T`` meaning ``Data.Text`` or ``Data.Text.Lazy``, ``Map`` meaning ``Data.Map`` or ``Data.Map.Strict`` -- all those will have a concise and effective way of being addressed by a policy that will become expressible.

It could be that the user might opt to implicitly (and potentially confusingly for themselves) request overlapping imports for a given alias, either through a pair of un-restricted ``aliases``-augmented import statements, or through a coincidence of one such statement with another import statement carrying a local alias declaration, leading to the same module alias being implicitly populated by different modules.  To this possibility, it's worth noting that:

1. the feature is strictly opt-in, on both import and export sides,
2. the language user community is already prepared to deal with a similar problem in context of regular unrestricted imports,
3. we provide an option for restricting the structured imports, for cases where a particular situation makes it concerning.

The implementation cases seem to incur a serialisation of module interface that is incompatible with non-extended functionality, regardless of the use of the extended functionality by the compiled module.

Costs and Drawbacks
-------------------
One unavoidable downside is the necessary complication in the module interface machinery -- we're now assigning structure to the previously unstructured set of names exchanged between modules, and that structure needs a material carrier.  The effect is two-fold, regardless of the use of the extended functionality:

1. Modules compiled by the extended compiler will be impossible to link using older compilers,
2. Linkability of modules produced by older compilers, if desired, will be restricted by the implementation of compatibility handling, that would assume empty exports sets of *level-1* names.

There appear to be no language-level costs for the non-users: ``StructuredImports`` not enabled in either module will result in simple, predictable, customary behavior.

There appears to be no compile-time cost whatsoever associated with handling of the modules compiled without the extension enabled.

The newly introduced keywords (``aliases`` and ``aliases_hiding``) are only assigned meaning locally to the import/export declarations and are not stolen from the overall syntax, similar to how it's handled in Haskell2010 (section 5.3):

   Lexically, the terminal symbols “as”, “qualified” and “hiding” are each a varid rather than a reservedid. They have special significance only in the context of an import declaration; they may also be used as variables.

Compile-time costs regarding processing of modules with the extension enabled should be:

1. Constrained to the module processing (compilation/linking) time,
2. Proportional to the complexity of the namespaces defined.

Implementation costs appear to include:

1. Parser changes
2. Namespace management changes
3. Serialised module interface changes

Alternatives
------------
A widely used alternative is disciplined copy-pasting of locally-aliased module imports between modules.  But avoiding reliance on human perfection is specifically part of our goal.

Additional extensions
^^^^^^^^^^^^^^^^^^^^^
During discussion of this proposal various further suggestions for extension came up:

1. ``import A.B.C aliases (D(..))`` to splice the set (or a subset) of *level-0* names exported by ``A.B.C`` under the ``D`` name into the top level of the local namespace.

On those, further discussion is needed to gauge the potential interest of the wider community.

Prior work
^^^^^^^^^^
* 2005 Coutts, `as` in export lists: https://mail.haskell.org/pipermail/libraries/2005-March/003390.html . Salient points:
  * `letting modules export other modules' contents qualified with the module name`
* 2006 Wallace, explicit namespaces for module names: https://ghc.haskell.org/trac/ghc/wiki/Commentary/Packages/PackageNamespacesProposal . Salient points:
  * `The declaration import namespace brings into availability the subset of the hierarchy of module names rooted in the package "foo-1.3", at the position Data.Foo`
* 2013 de Castro Lopo, qualified exports: https://wiki.haskell.org/GHC/QualifiedModuleExport
  * `qualified module T` in export list

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
