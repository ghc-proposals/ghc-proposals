.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_. **After creating the pull request, edit this file again, update the number in the link, and delete this bold sentence.**

.. contents::

Hierarchical modules
====================

Module hierarchies help in organizing modules in Haskell packages. Sadly there
is almost no hierarchy into GHC's codebase: most modules have unqualified names
which are sometimes short and cryptic. This proposal consists in renaming GHC's
modules to introduce a hierarchy.

Motivation
------------

Most arguments for using a hierarchical structure for modules are non technical
but fall into the following categories: "best practices" or conventions,
documentation, communication, etc. As only present and future GHC developers and
GHC-API users are concerned, there is still some hope that we can converge to
something acceptable for everyone that is not the status quo.

* Documentation: currently the `generated haddock
  <https://www.stackage.org/package/ghc>`_ is hard to navigate. The codebase
  uses a one-level hierarchy (e.g., nativeGen, typecheck, simplCore) but it
  doesn't appear in Haddock which makes the documentation less organized than
  the codebase. It's much easier to navigate `when a hierarchy is used
  <http://hsyl20.fr/ghc_doc/>`_.

* Discoverability: a clear module structure is self-documenting in that it makes
  it easier to know:

  1. Where to find something (least surprise): e.g., the demand analyser on Core
  representation is in GHC.IR.Core.Analyser.Demand

  2. What each module is about: even if we know nothing about Cmm and sinking,
  we can guess that GHC.IR.Cmm.Transformer.Sinker contains code about a
  "sinking" transformation on the Cmm representation.

  3. What the compiler does: as there is a module named
  GHC.IR.Stg.Transformer.CommonSubExpr, it seems like GHC performs CSE on STG
  IR.

  [As a personal example, when I wanted to improve the constant folding in Core,
  I was a bit surprised to have to modify prelude/PrelRules.hs instead of
  something in simplCore/. I understand the logic now that I know that "prelude"
  contains GHC's knowledge about builtin types and operations and that constant
  folding in Core is implemented with builtin rewrite rules, but it wasn't
  obvious.]

* Modularity: using a hierarchy helps in automatically finding dubious
  interconnections and keeping the codebase in order. For instance, I have
  `automatically generated some graphs <http://hsyl20.fr/ghc_module_deps/>`_ and
  we can see that the X86 codegen module (of the Cmm to Asm compiler) imports
  the module defining the Core syntax, which is weird [in this specific example
  we should put the Tickish data type somewhere else].

* Ambiguous names: some module names used by GHC are common (e.g., Parser,
  Lexer, Name, Types, Core, Format, Debug). They could easily lead to name
  clashes when used with GHC-API. Additionally, it is not obvious that GHC
  provides some of them (Bag, Llvm, Check, FastString, Bitmap, Encoding).
  Using at least a "GHC" namespace would be more conventional and would avoid
  these ambiguities.


Proposed Change Specification
-----------------------------

The proposal wouldn't be complete without an actual hierarchy proposal. Remember
that it's only renaming: in my opinion some modules should be split into smaller
more homogeneous modules but it's left for future work.

Here are the principles guiding the proposed hierarchy:

1. Discoverability and self-documentation as mentioned above. In particular,
   avoid acronyms (SAT, UniqDFM), avoid ambiguous names (e.g., codegen vs
   native codegen, Convert) and ambiguous truncations (e.g., TyCoRep), avoid
   codenames or nicknames when possible (backpack, hoopl, desugarer). I.e. be
   unequivocal, even if it's more verbose.

2. Make the GHC pipeline explicit in the module structure: make it obvious that
   there are several intermediate representations (IR) and several compilers.
   Make explicit the operations (transformations, analyses) that apply to each
   IR.


Proposed hierarchy:

.. code:: haskell

   GHC --  Everything is in the top-level GHC namespace.

      IR --  the different representations

         Haskell
            Syntax
            Lexer
            Parser
            Printer
            TypeChecker
            Renamer
            Deriver
            Analyser
               Stats

         Core
            Syntax
            Printer
            Analyser
               Arity
               CallArity
               Demand
               FreeVars
               Lint
               Occurence
               Stats
            Transformer
               CommonSubExpr
               ConstantFolder
               FloatIn
               FloatOut
               LevelSetter
               CaseLiberator
               Rules
               Simplifier
               Specialiser
               StaticArgument
               Substitution
               Tidier
               Vectoriser
               WorkerWrapper
         
         Cmm
            Syntax
            Parser
            Printer
            Analyser
               Lint
               Liveness
            Transformer
               CommonBlockElim
               ConstantFolder
               DataFlow
               Shortcutter
               Sinker
               Switch
               ProcPoint

         Stg
            Syntax
            Analyser
               Lint
               Stats
            Transformer
               CommonSubExpr
               CostCentreCollector
               Unariser

         ByteCode
            Syntax
            Assembler
            Linker

         Interface
            Syntax
            Loader
            Renamer
            TypeChecker
            Transformer
               Tidier

         Llvm
            Syntax
            Printer


      Compiler --  converters between IRs or to machine code (Asm)

         HaskellToCore
         CoreToStg
         StgToCmm
         CmmToAsm
         CmmToLlvm
         CoreToByteCode
         CoreToInterface
         CmmToC
         TemplateToHaskell

      Entity --  entities that we find everywhere in the compiler
         Class
         Coercion
         DataConstructor
         Id
         Name
         Kind
         Literal
         Module
         Type
         Var
         ...

      Builtin --  primitive and wired-in stuff
         Primitive
            Operations
            Types
         TypeNats
         Types
         Names
         Uniques

      Program -- GHC-the-program stuff
         CmdLineParser
         Mode     -- program execution modes
            BackPack
            MakeDepend
            Make
         Pipeline -- management of the compilation pipeline
            Phases

      Interactive -- interactive features
         Debugger
         DynamicLoader
         Interpreter
         Linker
         ...

      Config
         Constants      -- GHC version, max tuple size, etc.
         Build          -- build constants (generated)
         Flags          -- DynFlags
         Hooks
         HostPlatform   -- host platform constants

      Data -- data structures or helpers for some data types (char encoding, etc.)
         Bag
         FastString
         Graph
         List
         Tree

      Packages -- package management stuff
         PackageConfig

      RTS   -- runtime system constants or helpers
         InfoTable
         Storage
         
      Utils -- various utility stuff
         Binary
         Elf
         Error
         FileCleanup
         Finder
         Json
         Outputable
         Panic
         ...

      Plugin -- helpers for plugins (reexports, etc.)


Effect and Interactions
-----------------------

Renaming GHC's modules only impacts GHC developers and GHC-API users. GHC users
are not affected.

Newcomers (GHC devs and GHC-API users)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The proposal should only be positive for newcomers (cf. Motivation).

GHC devs
~~~~~~~~

The proposal should be beneficial in the medium term to current GHC developers.
We could automatically detect some issues (e.g., layering) which would help
keeping the code in order. It could also help in future refactoring such as
removing some dependency loops (DynFlags, etc.).

[We could also follow guidelines (e.g., "Don't use IO in GHC.IR.* and in
GHC.Compiler.*") that would help in making the compiler parallel or in writing
`interactive frontends <https://www.youtube.com/watch?v=sPu5UOYPKUw>`_.]

However, there are also a few drawbacks:

* some developers would have to change years of habits, leading to cognitive
  overhead during the transition period
* current branches and patches would need to be rebased (the conflicts, however,
  are easy to fix as they are only in import lists and in comments)
* module naming could become a new topic for infinite debates


GHC-API users
~~~~~~~~~~~~~

Users of the GHC-API would need to adapt their codes to the new API.

Switching to the new API is simple: only import lists have to be adapted. We
could provide a script to perform most of it automatically.

It is more problematic for packages that need to be compatible with several GHC
releases: if they perform the renaming, they can't be compiled against previous
GHC versions. One way to alleviate this issue is to provide a package that
"undoes" the refactoring and provides compatibility with some previous GHC
releases. For this proposal, the `compatibility package
<https://github.com/hsyl20/ghc-api-compat>`_ would consist in a Cabal file using
the ``reexported-modules`` feature.

[Such ``ghc-compat`` package could also be used for future refactoring by
providing type and function aliases. Thinking out loud, maybe we could abuse
version numbers for this package so that 8.2.8.4 would mean "the 8.2 API
interface on top of the GHC 8.4 release". Packages with version x.y.x.y would
reexport the modules of GHC x.y release so that no ``ifdef`` would be necessary in
the Cabal file to choose between ``ghc`` and ``ghc-compat`` packages: GHC-API
clients just need to depend on ``ghc-compat ==x.y.*``.]


Costs and Drawbacks
-------------------

Most of the cost is for GHC-API users and for GHC developers used to the actual
module naming.

Performing the actual renaming has a one time cost alleviated with ``sed``
scripts. Some references to former module names that are not checked by the
compiler (e.g., in comments) could slip through the renaming, especially when
the module name is also the name of a type. They would need to be fixed manually
later on when they are found.

References to modules outside of the codebase will become invalid. In
particular, on the wiki (Commentary, etc.), on Trac, in commit messages, in blog
posts, etc. Maybe we could use a script to fix the ``compiler/*`` links on the
wiki?

Several modules use the same ``GHC.`` prefix: ``ghc``, ``ghc-boot``,
``ghc-prim``, ``base``. While it is not critical, I figured I would mention it
as it can be confusing. Especially for ``base``: maybe we should have a
``ghc-base`` package and use Cabal's ``reexported-modules`` feature in ``base``
to reexport these modules. In the ``ghc-base`` package, modules would be
prefixed with ``GHC.Base``.

[As an anecdote, I had first prefixed ``basicTypes/*`` modules with
``GHC.Types`` as suggested in the `initial 9-year old proposal
<https://ghc.haskell.org/trac/ghc/wiki/ModuleDependencies/Hierarchical>`_. It
turned out to be a bad idea because ``GHC.Types`` is already taken in ``ghc-prim``
package and it made it more complicated to move basic types into ``GHC.Entity``
later on.]

Alternatives
------------

A proposed alternative has been to use Cabal's ``reexported-modules`` feature
the other way around: we could build a ``ghc-new-api`` package that would
provide the new hierarchical module scheme without modifying GHC's codebase.
While it could be interesting for GHC-API users if the proposal is rejected, it
doesn't make the GHC codebase better organized (obviously). Still it could be
useful to try alternative hierarchies.


Unresolved questions
--------------------

None at the moment.

Implementation Plan
-------------------

Once we agree on a hierarchy, I (Sylvain Henry) volunteer to implement the
proposal (there is a `preliminary patch on Phabricator
<https://phabricator.haskell.org/D3647>`_ already which makes renaming the
renaming easier) and the `compatibility package
<https://github.com/hsyl20/ghc-api-compat>`_.

I can also write the script to convert imports (using ``sed``), except if
someone with better ``sed`` skills (or ``ghc-exactprint`` skills) wants to
volunteer. It is a bit tricky because some module names are also data type names
(which are of course not renamed), hence the script would have to modify the
import lists only (e.g., we can't
``s/HsExpr/GHC.IR.Haskell.Syntax.Expression/g``). To be conservative, the script
would need to alias new module names with the old ones as some imports
(qualified or not) can be used qualified in the code (e.g., replace ``import
[qualified] HsExpr`` with ``import [qualified] GHC.IR.Haskell.Syntax.Expression
as HsExpr``).

