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
Preludes (meaning, broadly, all modules with reexports) give us a tool to centralise namespace formation, which serves several purposes:

* compressing the total set of import statements in the overarching program
* providing authoritative decisions on the direct availability of names -- essentially providing a common language

This centralisation use case, however, is deficient in that it isn't supported by any form of sharing of *module aliases* (module names, introduced specifically via the 'as' keyword), forcing explicit redefinition of these aliases across module collections. The effect is an unnecessary increase in the costs of module abstraction:

* for the reader, there are no commons to learn and refer to -- every module is a potential snowflake regarding the structure of its namespace (in the alias part)
* for the writer, growing the program and splitting it into modules brings super-linear expenditures for the namespace maintenance aspect (again, in the alias part)

NOTE: sharing of qualified module names introduced by import statements without the 'as' keyword is another side of the namespace replication issue, and one which is admittedly harder to tackle.  This side is explicitly not part of the proposal, although the basic rationale is briefly touched upon later.

NOTE: while we discuss importing and exporting *aliases*, what we *really* mean is performing these operations on the *sets of names available as qualified by those aliases*.  We'll clarify usage where this distinction becomes important.

We propose to provide module authors with a way of importing and exporting sets of names qualified by locally-established aliases, in their qualified form:

* alias definition module::

    module C
      ( aliases                       -- Complete export of all aliases available in the current module
      , aliases (ALIAS..)             -- Export of the available alias set intersected with the specified list
      , aliases hiding (ALIAS..)      -- Export of the available alias set, modulo the specified list
      )
    where
    import A.B.C as A.B.C
    import D.E.F as DEF

* alias user module::

    module B
    where
    import C aliases                  -- Complete import of all aliases available in the imported module
    import C aliases (ALIAS..)        -- Import of a subset of aliases available in the imported module
    import C aliases hiding (ALIAS..) -- ...

For some potential additions/tweaks to this proposal, please see the `Additional extensions`_ section.

Proposed Change Specification
-----------------------------
Export lists
^^^^^^^^^^^^
In section 5.2, "Export lists", extend the *export* non-terminal to accept extra clauses::

    |	aliases
    |	aliases (*modid1*, .., *modidN*)
    |	aliases hiding (*modid1*, .., *modidN*)

Extend the list of cases (starting with "Entities in an export list may be named as follows:") with the a seventh entry:

7. A set of aliased names in scope under a set of aliases may be referred by one of the following three forms:

   1. The form ``aliases`` denotes the entire set of entities available under aliased names.
   2. The form ``aliases (*modid1*, .., *modidN*)`` denotes the set of entities available under names qualified with specified aliases.
   3. The form ``aliases hiding (*modid1*, .., *modidN*)`` denotes the set of entities available under names qualified by all aliases, except those specified.  For example, the following module will carry the sum of sets of names exported by modules ``GH.I`` and ``G.HI`` in own its exports, but qualified by the alias ``GHI``::

        module M ( aliases hiding (ABC, DEF) ) where
          import AB.C as ABC
          import D.EF as DEF
          import GH.I as GHI
          import G.HI as GHI

      It is an error to use module A in an alias export list unless A is established as an alias by at least one import declaration.

The same section of Haskell2010 describes a restriction:

   The unqualified names of the entities exported by a module must all be distinct (within their respective namespace).

With regards to the aliased name exports, this restriction only applies to the individual sets of exports under individual alias names -- it is naturally a name clash to export different entities under the same name, within the set of names under the same alias.

Import lists
^^^^^^^^^^^^
In section 5.3, "Import lists", extend the *impdecl* non-terminal to accept extra clauses::

    |	import *modid* aliases
    |	import *modid* aliases (*modid1*, .., *modidN*)
    |	import *modid* aliases hiding (*modid1*, .., *modidN*)

Lexically, the terminal symbol ``aliases`` is a varid rather than a reservedid, has special significance only in the context of an import declaration and may also be used as variable.

We extend the list under section 5.3.1 ("What is imported") as follows:

   4. Entities can be imported from modules under qualified names (should the respective modules export them in fashion described in the previous section) as follows:

      1. ``import *modid* aliases`` marks the entire set of ``*modid*``'s names exported under aliases to be made available qualified by those corresponding aliases.
      2. ``import *modid* aliases (*modid1*, .., *modidN*)`` marks the subset of ``*modid*``'s names exported under aliases *modid1*.. *modidN* to be made available qualified by those corresponding aliases.
      3. ``import *modid* aliases hiding (*modid1*, .., *modidN*)`` marks the entire subset of ``*modid*``'s names exported under aliases (but those under aliases *modid1*.. *modidN*) to be made available qualified by those corresponding aliases.  It is an error to hide an alias that is not, in fact, exported by the imported module.

Examples
^^^^^^^^
To clarify the above import rules, suppose the module A has the following import/export structure::

   module A
     ( aliases (ABC, DEF)
     )
   where
   import A.B.C as ABC (a,b,c)
   import D.E.F as DEF (d,e,f)

Then this table shows what names are brought into scope by the specified import statement:

.. list-table:: Effects of import statements
   :header-rows: 1

   * - #
     - Import declaration
     - Names brought into scope
   * - 1
     - ``import A aliases``
     - ``ABC.a, ABC.b, ABC.c, DEF.d, DEF.e, DEF.f``
   * - 2
     - ``import A aliases (ABC)``
     - ``ABC.a, ABC.b, ABC.c``
   * - 3
     - ``import A aliases hiding (ABC)``
     - ``DEF.d, DEF.e, DEF.f``

In all cases, all instance declarations in scope in module A are imported.

Changes to the operational semantics
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Semantics of module interface files need to be extended from the status-quo of only allowing a flat set of regular names in the exports, to also admitting qualified names.

More specifically, in the ``mi_exports`` field of ``HscTypes.ModIface`` we're going from ``[IfaceExport]`` to something like ``[(ModuleName, IfaceExport)]``.

Note: Implementation options
  1. Changing ``mi_exports`` to carry a list of pairs, as described above.
  2. Keeping the type and semantics of the ``mi_exports`` field as-is, and adding the new semantics to a new field, such as ``mi_exports_aliases`` -- which would be less disruptive (and more conducive towards maintaining backwards compatibility), but also less clean in the long run.

Gating the functionality
^^^^^^^^^^^^^^^^^^^^^^^^
The new semantics are to be guarded by a language pragma, such as ``StructuredImports`` or ``SmugglingAliases``.

Effect and Interactions
-----------------------
Package author will gain an option of conveniently setting up coherent namespaces for their entire packages (or their desired subsets), by potentially specifying the entire shared namespace structure in a single file.

The natural divergences and ambiguities of things like ``T`` meaning ``Data.Text`` or ``Data.Text.Lazy``, ``Map`` meaning ``Data.Map`` or ``Data.Map.Strict`` -- all those will have a concise and effective way of being addressed by a policy that will become expressible.

The implementation cases incurs a serialisation of module interface that is incompatible with non-extended functionality, regardless of the use of the extended functionality by the compiled module.

Costs and Drawbacks
-------------------
One unavoidable downside is the necessary complication in the module interface machinery -- we're now assigning structure to the previously unstructured set of names exchanged between modules, and that structure needs a material carrier.  The effect is two-fold, regardless of the use of the extended functionality:

1. Modules compiled by the extended compiler will be impossible to link using older compilers,
2. Linkability of modules produced by older compilers, if desired, will be restricted by the implementation of compatibility handling, that would assume empty exports sets of *level-1* names.

There appear to be no language-level costs for the non-users: ``StructuredImports`` not enabled in either module will result in simple, predictable, customary behavior (except for the backward compatibility cost).

There appears to be no compile-time cost whatsoever associated with handling of the modules compiled without the extension enabled.

The newly introduced keyword (``aliases``) is only assigned meaning locally to the import/export declarations and are not stolen from the overall syntax, similar to how it's handled in *Section 5.3 of Haskell2010*:

   Lexically, the terminal symbols “as”, “qualified” and “hiding” are each a varid rather than a reservedid. They have special significance only in the context of an import declaration; they may also be used as variables.

Compile-time costs regarding processing of modules with the extension enabled should be:

1. Constrained to the module processing (compilation/linking) time,
2. Proportional to the complexity of the namespaces defined.

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
