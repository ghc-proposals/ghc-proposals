Structured module exports/imports
=================================

.. author:: Kosyrev Serge
.. date-accepted::
.. proposal-number::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/205>`_.
.. sectnum::
.. contents::

The inability to assign basic structure in the module import/export language leads to currently inavoidable cognitive costs at the module boundary.  A semantic extension at the module interface level, coupled with syntactic extension on both export and import sides would give language users a tool to control these costs.

Motivation
----------
Preludes (meaning, broadly, all modules with reexports) give us a tool to centralise namespace formation, which serves several purposes:

* compressing the total set of import statements in the overarching program
* providing authoritative decisions on the direct availability of names -- essentially providing a common language

This centralisation use case, however, is deficient in that it isn't supported by any form of sharing of *qualified names*, forcing explicit redefinition of structure established by the qualifiers across module collections. The effect is an unnecessary increase in the costs of module abstraction:

* for the reader, there are no commons to learn and refer to -- every module is a potential snowflake regarding the structure of its namespace (in its name qualification aspect)
* for the writer, growing the program and splitting it into modules brings super-linear expenditures for the namespace maintenance aspect (again, in the qualification aspect)

We propose to provide module authors with a way of importing and exporting sets of qualified names.

For the examples, please see the `Examples`_ section.

For some potential additions/tweaks to this proposal, please see the `Alternatives`_ section.

Proposed Change Specification
-----------------------------
Overview of changes to the export side
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We review the set of export use cases, organised along three axes:

- origin of exported name(s) -- *local* versus *imported*,
- specification for the set of exports -- *pointwise* versus *wholesale*,
- qualification at the export boundary -- the key point of this proposal

.. list-table:: Exports: intent vs. syntax
   :header-rows: 1

   * - #
     - Feature set
     - Origin: local or imported
     - Pointwise or wholesale
     - Qualified?
     - Export declaration
     - Added exports
     - Comments
   * - 1
     - *Haskell2010*
     - local
     - point
     - unqual
     - ``module M (a) where a = 1``
     - ``a``
     -
   * - 2
     - *Haskell2010*
     - imported
     - point
     - unqual
     - ``module M (a) where import N (a)``
     - ``a``
     -
   * - 3
     - *Haskell2010*
     - local
     - whole
     - unqual
     - ``module M (module M) where a = 1``
     - ``a``
     -
   * - 4
     - *Haskell2010*
     - imported
     - whole
     - unqual
     - ``module M (module N) where import N``
     - ``N`` 's exports, flat set
     -
   * - 5
     - *Structured Imports*
     - local
     - point-set
     - qual
     - ``module M (module N qualified) where import N``
     - All of ``N`` unqualified exports, re-qualified as ``N.x``.
     - Names ``N.x`` are only created locally in ``M``, so we add them to the export list, qualified.
   * - 6
     - *Structured Imports*
     - imported
     - point-set
     - qual
     - ``module M (module O qualified) where import N``
     - A subset of ``N`` exports, which is qualified as ``O.x``, verbatim.
     - Assuming that module ``N`` exports a set of names qualified with ``O``.
   * - 7
     - **out of scope**
     - local
     - whole
     - qual
     - Would've been ``module M (module M) where import N``
     -
     - This is controversial -- while ``N`` is a locally-introduced qualifier,
       ``N.x`` are not local names, so we decide not to allow this, retaining
       normal interpretation.
   * - 8
     - *Structured Imports*
     - imported
     - whole
     - qual
     - ``module M (module N) where import N``
     - All of ``N`` 's qualified and unqualified exports, verbatim.
     - This is reinterpretation of #4 enabled by the proposed extension.

Export lists
^^^^^^^^^^^^
In section 5.2, "Export lists", extend the *export* non-terminal to accept an extra clause::

    |	module *modid* qualified

With regards to the the list of cases (starting with "Entities in an export list may be named as follows:"):

Reword the fifth entry as follows:

    The form “module M” names two sets of entities:

      1. The set of all entities that are in scope with both an unqualified name “e” and a qualified name “M.e”. This set may be empty. For example:

          module Queue( module Stack, enqueue, dequeue ) where
               import Stack
               ...

         Here the module Queue uses the module name Stack in its export list to abbreviate all the entities imported from Stack.

         These entities will be re-exported unqualified.

      2. The set of all entities that are in scope with a qualified name “M.e”.  Again this set may be empty.

         These entities will be re-exported with their qualified names.

    A module can name its own local definitions in its export list using its own name in the “module M” syntax, because a local declaration brings into scope both a qualified and unqualified name (Section 5.5.1). For example:
      module Mod1( module Mod1, module Mod2 ) where
      import Mod2
      import Mod3

Add a sixth entry:

   The form ``module M qualified`` names the set of all entities that are in scope with a qualified name ``M.e``.
   Those entities will be advertised by the module as exported with their qualified name.

   It is an error to use ``module M qualified`` in an export list unless ``M`` is established either as an alias or a module name, by at least one import declaration.

The same section of Haskell2010 describes a restriction:

   The unqualified names of the entities exported by a module must all be distinct (within their respective namespace).

With regards to the qualified name exports, this restriction only applies to the individual sets of exports with individual qualifiers -- it is naturally a name clash to export different entities with the same qualified name.

The same section says:

   If the export list is omitted, all values, types and classes defined in the module are exported, but not those that are imported.

This is to be extended to cover the qualified names -- none of them are exported in case of an omitted export list.

Import lists
^^^^^^^^^^^^
In section 5.3, "Import lists", extend the *import* non-terminal to accept an extra clause::

    |	module *modid*

This clause stands for a set of names qualified with ``modid``.

The leading part of the section 5.3 should is to be extended with:

   Imported names might be already qualified, if the module being imported exports them as qualified.

The third entry of the list in section 5.3.1 should be reworded as:

   Finally, if impspec is omitted then all the entities exported by the specified module are imported, including all of the entities exported with qualified names.

Changes to the operational semantics
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Semantics of module interface files need to be extended from the status-quo of only allowing a flat set of regular names in the exports, to also admitting qualified names.

More specifically, in the ``mi_exports`` field of ``HscTypes.ModIface`` we're going from ``[IfaceExport]`` to something morally equivalent to ``[(ModuleName, IfaceExport)]``.

Note: Implementation options
  1. Changing ``mi_exports`` to carry a list of pairs, as described above.
  2. Keeping the type and semantics of the ``mi_exports`` field as-is, and adding the new semantics to a new field, such as ``mi_exports_aliases`` -- which would be less disruptive (and more conducive towards maintaining backwards compatibility), but also less clean in the long run.

Gating the functionality
^^^^^^^^^^^^^^^^^^^^^^^^
The new semantics are to be guarded by a language pragma, such as:

  - ``StructuredImports``     -- because that's what we want, ultimately,
  - ``FirstClassModuleNames`` -- because that's what it is, conceptually.

Examples
--------

* Defining module::

    {-# LANGUAGE StructuredImports #-}

    module Containers
      ( module Map qualified          -- Export the set of names qualified with 'Map' and 'Set', qualified.
      , module Set qualified          -- ..and the same for 'Set'.
      , Map, Set                      -- And the 'Map' and 'Set' types, unqualified.
      )
    where

    import qualified Data.Map as Map  -- We construct the classic names for containers..
    import qualified Data.Set as Set
    import           Data.Map (Map)
    import           Data.Set (Set)

* User module::

    {-# LANGUAGE StructuredImports #-}

    module M where

    import Containers                 -- We bring in both the unqualified *and* qualified names.

    import Containers ( module Map    -- Or, alternatively,
                      , module Set)   -- ..if we want to be explicit about the qualified names.
    import Containers hiding
                      ( module Map    -- ..or, even, explicitly negative.
                      , module Set)

    foo :: Map Int String
    foo = Map.empty

Effect and Interactions
-----------------------
Package author will gain an option of conveniently setting up coherent namespaces for their entire packages (or their desired subsets), by potentially specifying the entire shared namespace structure in a single file.

The natural divergences and ambiguities of things like ``T`` meaning ``Data.Text`` or ``Data.Text.Lazy``, ``Map`` meaning ``Data.Map`` or ``Data.Map.Strict`` -- all those will have a concise and effective way of being addressed by a policy that will become expressible.

The implementation cases incurs a serialisation of module interface that is incompatible with non-extended functionality, regardless of the use of the extended functionality by the compiled module.

Costs and Drawbacks
-------------------
Complication of the module interface
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
One unavoidable downside is the necessary complication in the module interface machinery -- we're now assigning structure to the previously unstructured set of names exchanged between modules, and that structure needs a material carrier.  The effect is two-fold, regardless of the use of the extended functionality:

1. Modules compiled by the extended compiler will be impossible to link using older compilers,
2. Linkability of modules produced by older compilers, if desired, will be restricted by the implementation of compatibility handling, that would assume empty exports sets of *level-1* names.

Language-level costs
^^^^^^^^^^^^^^^^^^^^
There appear to be no language-level costs for the non-users: ``StructuredImports`` not enabled in either module will result in simple, predictable, customary behavior (except for the backward compatibility cost).

There appears to be no compile-time cost whatsoever associated with handling of the modules compiled without the extension enabled.

Compile-time costs regarding processing of modules with the extension enabled should be:

1. Constrained to the module processing (compilation/linking) time,
2. Proportional to the complexity of the namespaces defined.

Implementation costs
^^^^^^^^^^^^^^^^^^^^
Implementation costs appear to include (according to a proof-of-concept implementation):

1. Parser changes
2. Renamer changes
3. Serialised module interface changes
4. Minor changes to the desugarer/simplifier, due to data forwarding necessities.

Alternatives
------------
A widely used alternative is disciplined copy-pasting of locally-aliased module imports between modules.  But avoiding reliance on human perfection is specifically part of our goal.

Prior work
^^^^^^^^^^
* 2005 Coutts, `as` in export lists: https://mail.haskell.org/pipermail/libraries/2005-March/003390.html . Salient points:
  * `letting modules export other modules' contents qualified with the module name`
* 2006 Wallace, explicit namespaces for module names: https://ghc.haskell.org/trac/ghc/wiki/Commentary/Packages/PackageNamespacesProposal . Salient points:
  * `The declaration import namespace brings into availability the subset of the hierarchy of module names rooted in the package "foo-1.3", at the position Data.Foo`
* 2013 de Castro Lopo, qualified exports: https://wiki.haskell.org/GHC/QualifiedModuleExport
  * `qualified module T` in export list

Implementation Plan
-------------------
A prototype implementation exists:  https://github.com/deepfire/ghc/tree/structured-imports
