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

For the relevant chapter of the *Haskell2010* specification, please see: https://www.haskell.org/onlinereport/haskell2010/haskellch5.html

For the examples, please see the `Examples`_ section.

For some potential additions/tweaks to this proposal, please see the `Alternatives`_ section.

Proposed Change Specification
-----------------------------
Export side changes
^^^^^^^^^^^^^^^^^^^
Syntax
++++++
In section 5.2, "Export lists", we extend the **export** non-terminal to accept an extra clause::

    |	qualified *modid*

Semantics
+++++++++
Set of qualified names selected for export
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Module re-export entry: reinterpretation
''''''''''''''''''''''''''''''''''''''''
In section 5.2, "Export lists", *Haskell2010*, with regards to the the list of cases
starting with "Entities in an export list may be named as follows:", reword the fifth entry as follows:

    The form ``module M`` names two sets of entities:

    1. The set of all entities that are in scope with both an unqualified name ``e`` and a qualified name ``M.e``. This set may be empty. For example::

          module Queue( module Stack, enqueue, dequeue ) where
          import Stack
          ...

       Here the module Queue uses the module name Stack in its export list to abbreviate all the entities imported from Stack.

       These entities will be re-exported unqualified.

    2. The combination of sets of entities that are in scope under qualified names (regardless of the particular names),
       that are made available due to imports of modules (with due respect to the import specifiers) that are imported with either:

       - ``import M [*impspec*]``, or
       - ``import [qualified] ... as M [*impspec*]``

       These entities will be re-exported with their qualified names.

    A module can name its own local definitions in its export list using its own name in the ``module M`` syntax, because a local declaration brings into scope both a qualified and unqualified name (Section 5.5.1). For example::

       module Mod1( module Mod1, module Mod2 ) where
       import Mod2
       import Mod3

This constitutes a re-interpretation of module re-export statements, that
essentially allows qualified names to flow between modules in a transparent manner,
the same way regular names are allowed to.

Module self re-export: unchanged
''''''''''''''''''''''''''''''''
Considering module self-re-exports::
   module M (module M) where ...

Such self re-export does not add any qualified names to the export list.

The rationale is that the local qualified namespace is often quite populous,
and so reinterpretation to include it would hinder the established usage.

Export entry with a 'qualified' keyword: qualified names only
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
In section 5.2, "Export lists", *Haskell2010*, with regards to the the list of cases
starting with "Entities in an export list may be named as follows:", add a sixth entry:

   The form ``qualified M`` names the set of all entities that are in scope with a qualified name ``M.e``.
   Those entities will be advertised by the module as exported with their qualified name.

   It is an error to use ``module M qualified`` in an export list,
   unless the local module namespace has a non-empty set of names qualified with ``M``.
   That is, such names must have been introduced by either:

   - regular import statements, with or without the ``as`` keyword,
   - imports of qualified names (subject of this proposal).

This allows for introduction of individual sets of locally-established qualified
names into the export list, the same way regular names are allowed to.

Injectivity: preserve
~~~~~~~~~~~~~~~~~~~~~
The same section describes a restriction:

   The unqualified names of the entities exported by a module must all be distinct (within their respective namespace).

With regards to the qualified name exports, this restriction only applies
to the individual sets of exports with individual qualifiers -- it is naturally
a name clash to export different entities with the same qualified name.

Omitted export list: no qualified exports
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The same section says:

   If the export list is omitted, all values, types and classes defined in the module are exported, but not those that are imported.

This is to be extended to cover the qualified names -- none of them are exported in case of an omitted export list.

Comparative case analysis
~~~~~~~~~~~~~~~~~~~~~~~~~
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
     - ``module M (qualified N) where { import N; import P as N; }``
     - All of locally-scoped names qualified with ``N``.
     -
   * - 6
     - *Structured Imports*
     - imported
     - point-set
     - qual
     - ``module M (qualified O) where { import N; import P; }``
     - A subset of ``N`` 's and/or ``P`` 's exports, which is qualified as ``O.x``, verbatim.
     - Assuming that the combination of modules ``N`` and ``P``  exports a non-clashing set of names qualified with ``O``. It is an error otherwise.
   * - 7
     - **out of scope**
     - local
     - whole
     - qual
     - Would've been ``module M (module M) where import N``
     -
     - This is controversial -- while ``N`` is a locally-introduced qualifier,
       ``N.x`` are not names defined locally, so we decide not to allow this, retaining
       customary Haskell98 restriction for local module reexports.
   * - 8
     - *Structured Imports*
     - imported
     - whole
     - qual
     - ``module M (module N) where { import N; import qualified P as N; }``
     - All of ``N`` 's and ``P`` 's exports qualified and unqualified exports, verbatim, unless a name clash induces an error.
     - This is reinterpretation of #4 enabled by the proposed extension.

Import side changes
^^^^^^^^^^^^^^^^^^^
Syntax
++++++
In section 5.3, "Import lists", extend the **import** non-terminal to accept an extra clause::

    |	module *modid* [as *modid*] [*impspec*]

Semantics
+++++++++
Specification of imported names
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We might consider the import process as a combination of two steps:

1. *Selection* of exported names, and,
2. *Introduction* of local names to those selected.

Import statement without an import spec: reinterpretation
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Considering import statements of the form::
   import M

The third entry of the list in section 5.3.1 should be reworded as:

   Finally, if impspec is omitted then all the entities exported by the specified
   module are imported, including all of the entities exported with qualified names.

This constitutes a reinterpretation of the normal import statement for the sake
of a low-friction flow of qualified names between modules.

Import statement with an import spec, without hiding
''''''''''''''''''''''''''''''''''''''''''''''''''''
Considering import statements of the form::
   import M (module *modid* [as *modid*] [*impspec*], ...)

Assuming the context of **import** non-terminal from the above "Syntax" subsection,
this clause *selects* a subset of qualified names exported by module ``M`` -- and,
specifically, the subset that has the **modid** qualifier.

This subset can be further restricted by the normal intepretation of **impspec**,
if it has been provided (see section 5.3.1, "Import lists" of *Haskell2010*).

The qualifier of locally-*introduced* names can be changed to an alternative **modid**,
by an ``as`` clause.

The same **modid** can appear in several ``module`` import entries, and their
effect would be strictly cumulative, as per section 5.3:

   The effect of multiple import declarations is strictly cumulative: an entity is
   in scope if it is imported by any of the import declarations in a module. The
   ordering of import declarations is irrelevant.

Import statement with an import spec, with hiding
'''''''''''''''''''''''''''''''''''''''''''''''''
Considering import statements of the form::
   import M hiding (module *modid* [*impspec*], ...)

The hiding import extends the normal interpretation of the un-extended language semantic,
to the *selected* names that have a qualifier.

Whatever the set of qualified names that is *selected* for *introduction* by an
**import** non-terminal (as specified in the previous subsection "Import statement
with an import spec, without hiding"), addition of the ``hiding`` keyword to the
top-level of the ``import`` statement designates the same set to be un-*selected*
from the entire set of qualified exports of the module being imported.

Note that this has an implication of assigning a double-negation interpretation
to the import statements of a form similar to the following::

  import M hiding (module N hiding (a))

This example would take a meaning of introducing exactly the name ``N.a`` to the
local namespace.

Import statement with a renaming import spec, with hiding: forbid
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Considering import statements of the form::
   import M hiding (module *modid* as *modid* [*impspec*], ...)

Such statements are forbidden, since they appear to have no useful meaning.

Import statement with renaming at top level: indifference
'''''''''''''''''''''''''''''''''''''''''''''''''''
Considering import statements of the form::
   import M as N (module *modid* ..., ...)

The addition of a renaming ``as`` modifier only has effect on the regular imports,
not on the qualified names introduced by the ``module`` import list entry.

This allows us to avoid the need for a separate import statement in case we want
to combine an explicit import list and renaming of the qualifier for the regular imports.

Import statements with/without 'qualified' keyword: indifference
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Considering import statements of the form::
   import qualified M [(module ..., ...)]

Neither *selection* of qualified names for import, nor their *introduction* into
the local namespace is affected by the presence or absence of the ``qualified``
modifier to the import statement.

Note, that this form has an effect of allowing *import* (which is distinct from
*local introduction*) of qualified names only, which might cater to the policy
preference of unqualified name avoidance.

Import-side name self-clash: a new curious kind
'''''''''''''''''''''''''''''''''''''''''''''''
Note that the new ability to *import* qualified names introduces a new,
previously unavailable kind of import-time conflict -- one between names imported
from a single module.

To elucidate this matter, let's assume an import statement of the form
``import M [as N] [(...)]``.

In this context we have to draw a distinction between:

- qualified names coming as a regular Haskell98 import, due to regular qualification.
- qualified names coming from the exports of module ``M`` chosen for import.

Indeed, it is possible for those names to refer to different entities, and also coincide.

Some of those conflicts can be avoided by preventing them on the export side,
and all of them can be avoided on the import side through use of local qualifier renaming.

Generally, though, such conflicts should be relatively rare.

Comparative case analysis
~~~~~~~~~~~~~~~~~~~~~~~~~
We review the set of import use cases, organised along three axes:

- origin of qualified name(s) -- *local* versus *imported* -- the key point of this proposal
- specification for the set of imports -- *pointwise* versus *wholesale*,
- renaming of the qualifier

For the sake of examples, we assume availability of a module defined as follows::

   -- | A module in extended semantics.
   {-# LANGUAGE StructuredImports #-}
   module C
     ( qualified Map
     )
   where

   import qualified Data.Map as Map
   import           Data.Map (map)

For each import statement we provide two sets of *newly introduced names* -- both
for un-extended *Haskell2010* language, and for *Structured Imports*.

Note that we don't specifically consider ``import qualified`` statements, since
presence of the ``qualified`` keyword is specified not to incur a difference in
effect on imports of names that are exported with qualified names.

.. list-table:: Import: intent vs. syntax
   :header-rows: 1

   * - #
     - Origin: local or imported
     - Pointwise or wholesale
     - Renamed?
     - Import declaration
     - *Haskell 2010*
     - *Structured Imports*
     - Comments
   * - 1
     - local
     - point
     - unchanged
     - ``import C (map)``
     - ``C.map, map``
     - ``C.map, map``
     - Extension has no effect, because the explicit import spec doesn't mention qualified imports.
   * - 2
     - local
     - point
     - renamed
     - ``import C as LC (map)``
     - ``LC.map, map``
     - ``LC.map, map``
     - *Same as above*.
   * - 3
     - local
     - whole
     - unchanged
     - ``import C``
     - ``C.map, map``
     - ``C.map, map, Map.map``
     - Incompatible change. Reinterpretation of the import statement to also implicitly include the qualified exports.
   * - 4
     - local
     - whole
     - renamed
     - ``import C as LC``
     - ``LC.map, map``
     - ``LC.map, map, Map.map``
     - *Same as above*.
   * - 5
     - imported
     - point
     - unchanged
     - ``import C (module Map (map))``
     - *unavailable*
     - ``Map.map``
     - Unqualified imports not brought in by the explicit import spec.
   * - 6
     - imported
     - point
     - renamed
     - ``import C (module Map as LMap (map))``
     - *unavailable*
     - ``LMap.map``
     -
   * - 7
     - imported
     - whole
     - unchanged
     - ``import C (module Map)``
     - *unavailable*
     - ``Map.map``
     - Outside of this example, it is a warning, not an error, if ``C`` does not export names qualified with ``Map``.
   * - 8
     - imported
     - whole
     - renamed
     - ``import C (module Map as LMap)``
     - *unavailable*
     - ``LMap.map``
     - Outside of this example, it is a warning, not an error, if ``C`` does not export names qualified with ``Map``.

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
      ( module Map qualified         -- Export the set of names qualified with 'Map' and 'Set', qualified.
      , module Set qualified         -- ..and the same for 'Set'.
      , Map, Set                     -- And the 'Map' and 'Set' types, unqualified.
      )
    where

    import qualified Data.Map as Map -- We construct the classic names for containers..
    import qualified Data.Set as Set
    import           Data.Map (Map)
    import           Data.Set (Set)

* User module::

    {-# LANGUAGE StructuredImports #-}

    module M where

    import Containers                 -- We bring in both the unqualified *and* qualified names.

    import Containers ( module Map as LMap  -- Or, alternatively,
                      , module Set as LSet) -- ..if we want to be explicit about the qualified names.
    import Containers hiding
                      ( module Map    -- ..or, even, explicitly negative.
                      , module Set)

    foo :: Map Int String
    foo = Map.empty

Effect and Interactions
-----------------------
Effect
^^^^^^
Package author will gain an option of conveniently setting up coherent namespaces for their entire packages (or their desired subsets), by potentially specifying the entire shared namespace structure in a single file.

The natural divergences and ambiguities of things like ``T`` meaning ``Data.Text`` or ``Data.Text.Lazy``, ``Map`` meaning ``Data.Map`` or ``Data.Map.Strict`` -- all those will have a concise and effective way of being addressed by a policy that will become expressible.

The implementation cases incurs a serialisation of module interface that is incompatible with non-extended functionality, regardless of the use of the extended functionality by the compiled module.

Interactions
^^^^^^^^^^^^
Backpack
++++++++
There might be potential interactions with the Backpack module system extension.

Deprecating Exports
+++++++++++++++++++
There is an interaction with the ``DEPRECATED`` pragma::

   A symbol exported by a module is deprecated if all export specifiers for that symbol have a DEPRECATED pragma

This meaning is to be extended to include export specifiers for qualified exports.

Qualified Imports
+++++++++++++++++
Relationship with the discussed ``Qualified Imports`` extension (https://github.com/ghc-proposals/ghc-proposals/pull/220 ):

- ``StructuredImports`` deals with:

  1. Expressivity of the inter-module boundary:

     - increasing the amount of namespace structure that can cross the inter-module boundary.

  2. Expressivity of the intra-module namespace formation language:

     - new way of forming local namespace structure -- by import of qualified names.

- ``QualifiedImports``

  1. Expressivity of the intra-module namespace formation language:

     - language extension as a toggle for whether names come qualified by default.

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

A widely used alternative is disciplined copy-pasting of locally-aliased module
imports between modules.  But avoiding reliance on human perfection is
specifically part of our goal.

Options
^^^^^^^
Unqualified imports
+++++++++++++++++++
Assuming a certain future after the proposal is accepted, some situations might call for
mixing preference for allowing and disallowing qualified imports from different modules.

The current proposal only allows either:

1. specifying this preference at module level, by applying the language pragma,
2. explicitly hiding every set of qualified names in a hiding import.

Neither of those options is particularly flexible.

To cater to a more exquisite, fine-grained taste, we might want to introduce the
``unqualified`` keyword to the top level of the import statements.

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
