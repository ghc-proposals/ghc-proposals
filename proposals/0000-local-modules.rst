Local Modules
=============

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/283>`_.
.. sectnum::
.. contents::

Haskell currently has three related restrictions:

* *Modules*, a set of declarations that share a namespace and perhaps are
  surrounded by an abstraction barrier, coincide with *source files*, the
  `.hs` files on disk. This is limiting if one region of a source file has
  local definitions that the programmer wishes to restrict. It is also
  limiting if a user wants to declare multiple types with the same, e.g.,
  constructors and record names in the same file.

* When a user declares an algebraic datatype or a class, all the
  constructors, record selectors, methods, associated types, associated
  datatype instance constructors, and associated datatype instance record
  selectors get *bundled* with the type name, allowing you to export, e.g.,
  ``T(..)``. Pattern synonyms allow for a small, ad-hoc extension to
  bundling, but the user has no way to expand this feature. This is limiting
  if a user wants to, say, change a class into a type family that returns a
  constraint; downstream users have to change their import statements for
  what was meant to be a local refactoring.
  
* Despite common idioms like ``import qualified Data.Set as S``, if a user
  wants to have ``Data.Set`` imported this way throughout their project, they
  must repeat this line in every file. There is no way to abstract over this
  idiom.

This proposal describes a mechanism for *local modules* which lifts all the
above restrictions through a backward-compatible generalization to the
namespacing and import/export mechanism. This proposal is an alternative to
`#205`_ (Structured module exports/imports), `#273`_ (local types), and
`#295`_ (first-class mdoules), and it
provides a tempting way toward a resurrection of rejected proposal `#40`_
("context fixes").

This proposal affects only naming, and the impact within GHC would be
entirely in the renamer (and, naturally, the AST and interface-file format).

.. _`#205`: https://github.com/ghc-proposals/ghc-proposals/pull/205
.. _`#273`: https://github.com/ghc-proposals/ghc-proposals/pull/273
.. _`#40`: https://github.com/ghc-proposals/ghc-proposals/blob/context-fixes/proposals/0000-context-fixes.rst
.. _`#160`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0160-no-toplevel-field-selectors.rst
.. _`#88`: https://github.com/ghc-proposals/ghc-proposals/pull/88
.. _`#234`: https://github.com/ghc-proposals/ghc-proposals/pull/234
.. _`#282`: https://github.com/ghc-proposals/ghc-proposals/pull/282
.. _`#243`: https://github.com/ghc-proposals/ghc-proposals/pull/243
.. _`#295`: https://github.com/ghc-proposals/ghc-proposals/pull/295

Motivation
----------
1. It is common for a Haskell library to export several modules with the same
   (or similar) sets of exported symbols, or for a library's defined names to
   clash with those in the Prelude. For example, the commonly used libraries
   ``bytestring``, ``containers``, and ``text`` all exhibit both of these
   patterns. This means that users must write code like this::

     import qualified Data.ByteString.Lazy as BL
     import qualified Data.ByteString as BS
     import Data.Set ( Set )
     import qualified Data.Set as Set

   In a library where these three types are in common use, these ``import``
   statements must appear in every module: there is no way to write a custom
   ``MyPrelude`` that exports *qualified* symbols. This is annoying.

   With this proposal, the user will be able to write ::

     module MyPrelude ( module qualified BL
                      , module qualified BS
                      , Set
                      , module qualified Set ) where

     import qualified Data.ByteString.Lazy as BL
     import qualified Data.ByteString as BS
     import Data.Set ( Set )
     import qualified Data.Set as Set

   Now, a module with ``import MyPrelude`` will gain access to the qualified
   names.

2. It is impossible to write multiple declarations in the same file that use
   the same names. For example, I might want to have ::

     data Nat = Zero | Succ Nat

     data Fin :: Nat -> Type where
       Zero :: Fin (Succ n)
       Succ :: Fin n -> Fin (Succ n)

     data Elem :: a -> [a] -> Type where
       Zero :: Elem x (x : xs)
       Succ :: Elem x xs -> Elem x (y : xs)

   All three of these are, essentially, encodings of the natural numbers, and
   thus the names ``Zero`` and ``Succ`` apply well to each. This is
   impossible today, as the names clash.

   With this proposal, these declarations would be accepted. The constructors
   would be disambiguated with module prefixes, like ``Nat.Zero`` and
   ``Elem.Succ``. (The ``Fin`` declaration would need to say ``Nat.Succ`` in
   place of ``Succ``.) An unqualified use of a constructor would be an error.
   Alternatively, to prevent ``Fin``\'s and ``Elem``\'s constructors from
   being available unqualified in the global scope, the user could declare
   ``data qualified Fin ...`` and
   ``data qualified Elem ...``.

3. When a function ``f`` needs a helper ``h``, we can declare ``h`` in a
   ``where`` clause. However, suppose both ``f`` and ``g`` need ``h``. Now,
   ``h`` must be declared at the top level, meaning all the rest of the
   definitions in the module can see ``h``, even if ``h`` is really very
   specific to ``f`` and ``g``.

   With this proposal, we can model this situation nicely::

     import module (f, g) where
       f :: ...
       f = ...

       g :: ...
       g = ...

       h :: ...
       h = ...

   Unfortunately, there is no way for ``h`` to access arguments passed to
   ``f`` or ``g`` without declaring these as arguments to ``h`` and passing
   them explicitly. Fixing this was the subject of `#40`_; see `Future Work`_.

4. If a function or group of functions needs to work with a datatype or class
   locally, there is no way to do this without polluting the namespace of the
   entire file.

   With this proposal, we can do this easily::

     import module (f) where
       f :: ...
       f = ...

       data SpecialDataType = ...

       class LocalClass a b c where ...

5. When we expect users to import our library qualified, we have no way of
   signaling this beyond documentation; we also have no way of making it easy
   to import the module qualified correctly.

   With this proposal, we can do this easily::

     -- top of file:
     module Data.Set ( Set, qualified module Set ) where

       module Set ( Set, fromList ) where
         data Set = ...
         fromList = ...

   An importer who days ``import Data.Set`` will get access to ``Set`` (the
   type) and ``Set.fromList``, the function. The fact that the module and type
   have the same name is inconsequential here, but it is permitted.

6. Suppose our library exports class ``C`` with method ``meth``. Our users
   will frequently import ``C(..)`` and get ``meth`` in scope. Now I wish to
   refactor ``C`` without changing my users' import behavior. There is no way
   to do this currently.

   With this proposal, we can do this::

     -- top of file:
     module MyLibrary ( module C(meth) ) where

       class C a where ...

       import module C where
         meth :: ...

   This example shows that modules may be *extended*. The ``class C``
   declaration implicitly creates module ``C``, which is then extended below.

   An import specifier of ``C(..)`` will not import ``meth`` after this
   change. Instead, importers must say ``import module C``. However, this new
   import statement is a drop-in replacement for ``C(..)`` and may become
   preferable (as it is customizable in the way demonstrated here). Thus,
   it is reasonable that Haskellers would learn to write ``import module C``
   in import lists instead of ``C(..)``, as the former (new form) is
   extensible.

Background
----------

In order to precisely specify local modules, we must fix a number of items
of vocabulary. All descriptions of Haskell are true today; this is not part of the
proposed change.

* **Occurrence name** (sometimes just called "name"):
  An occurrence name is an identifier written in a program. Names can be
  alphanumeric or symbolic. Symbolic names, such as ``+`` or ``!@!`` can
  be written in parentheses (``(+)`` or ``(!@!)``); a symbolic name in
  parentheses is treated identically to an alphanumeric name. Because of
  this correspondence, we do not treat symbolic identifiers specially in this
  proposal.

* **Module name**: A dot-separated list of identifiers starting with a
  capital letter (e.g. ``A.B.C``). A module name may or may not refer
  to an extant module. For instance in

  ::

      module Q( x ) where { import P as P2; foo = P2.x }

  the module name ``P2`` is valid but no such module need exist.

* **Original name**:
  The original name of an entity is a pair of the module
  (e.g. ``A.B.C``) that defines it, and the occurrence
  name (e.g. ``x``) by which it is defined in that module. The
  original name uniquely identifies an entity.
  
* **Qualified name**: A qualified name is a name prepended with a module name
  followed by a ``.``, with no intervening whitespace. Both alphanumeric and
  symbolic names can be qualified.
  
* **Entity**: An entity is a definition that can be exported and imported.
  An entity is uniquely identified by its original names name. Entities include variables, classes, datatypes, and
  constructors, among a few other constructs.

* **Environment**: An environment is mapping of unqualified and qualified
  names to original names. It defines what is in scope.

* **in scope**: A name is said to be **in scope** when it is in the current
  environment.
  
* **Scope**: A scope is a lexical region of a program where all name occurrences are
  looked up in an environment (or a superset of an environment).

* **Module**: A module is a set of declarations of entities, along with a set
  of exported names.
  
* **Exports**: The exports from a module is a set of original names, such
  that no two exports can have the same occurrence name.
  (These names can be written with a qualification in an export
  to disambiguate, if the unqualified name has multiple entries in the environment
  of the exporting module. But the exported name itself is just the original name;
  it does not "remember" the qualification.)

* **Import specifiers**: An import specification, or *impspec*, can be listed
  in parentheses in an ``import`` statement to restrict what is imported.
  Formally, an impspec is set of occurrence names. (This set may be specified
  via complement, using ``hiding``; this proposal will not worry about this
  detail.)

* **Import item**:  An element in an import specifier is an *import item*.
  
* **Imports**: An ``import`` statement names a module *M* and (optionally) an 
  import-specifier *impspec*.

  If *impspec* is omitted, then *impspec* is considered to include
  the occurrence names of all the original names exported from *M*.

  Let the *impspec*\-environment be a mapping from all the (unqualified) occurrence
  names in *impspec* to corresponding original names exported from *M*.
  Call the creation of an *impspec*\-environment from an *impspec* the *interpretation*
  of the *impspec*. Note that interpretation happens in the context of a
  exported set of original names.

  It is an error if *impspec* contains any occurrence name that does not
  correpsond to an original name exported from *M*.

  Let the *module alias A* be the module name appearing after the ``as`` in
  the ``import`` statement. If there is no ``as``, then the module alias *A*
  is just *M*.

  For each mapping *occname* ``|->`` *origname* in the *impspec*\-environment,
  add *A.occname* ``|->`` *origname* to the global environment of the file
  being compiled.

  If the import does not use ``qualified``, additionally add the
  entire *impspec*\-environment to the global environment of the file
  being compiled.

Core Proposal Change Specification
----------------------------------

This proposal's specification is divided into pieces. This core piece is a necessary
component of the overall proposal. Later pieces can be chosen piecemeal.

1. An original name is now a triple, containing the top-level (file) module
   where an entity is declared, a list of local module names within which
   an entity is declared, and the occurrence name of the entity. (Entities
   from a module that does not use this extension will have an empty list of local
   module names.)

#. A qualified name can now have any number of module qualifications, each
   separated by a ``.`` and with no intervening whitespace. (This is a change
   to the definition *qualified name*.) This can lead
   to potential ambiguity: is the name ``Data.List.length`` qualified by
   ``Data.List`` (a well-formed module name) or qualified both by ``Data``
   and by ``List``? If ``length`` is in scope qualified both by ``Data.List``
   and by both ``Data`` and ``List``, the occurrence ``Data.List.length`` is
   an error. There is no way to disambiguate locally without using module
   aliases or other renamings. (Programmers should seek to avoid this scenario.)

#. Introduce a new extension ``-XLocalModules``.

#. Introduce a new concept *export-module*, which is a set of original names
   and export-modules. Export-modules have module names.
   Instead of exporting a set of original names, modules now export a set of
   original names and export-modules.

   An export-module *M* may be interpreted into an environment:
   
     * For each original name *O.n* exported by *M*, add *n* ``|->`` *O.n* to the result
       environment.
     * For each export-module *Sub* exported by *M*, interpret *Sub* into an
       environment *E*. For each mapping *qualname* ``|->`` *origname* in *E*,
       add a mapping *Sub.qualname* ``|->`` *origname* to the result environment.

#. Export-modules may be imported with this import item::

     import ::= ... | 'module' modid [ impspec ]

   The interpretation of this import item is a subset of the environment obtained
   by interpreting the export-module *modid* (where the subsetting is controlled
   by *impspec*).
   The *impspec*\-environment produced here is then merged with the
   intepretation of its other sibling import items.
     
   It is an error if the export-module in which we are working
   does not export an export-module named *modid*.

   If *impspec* is omitted, consider it to contain the occurrence names
   corresponding to all original names exported by the export-module *modid*,
   as well as ``module`` import items corresponding to all export-modules exported
   by the export-module *modid*.

   This new form of import is allowed only with ``-XLocalModules``.

   (We do not yet have a way of exporting an export-module; this comes later in this proposal.)
   
#. Introduce a new declaration form (allowed only at the top level of a
   module -- i.e. not in a ``let`` or ``where``)
   to declare a new module called a *local module*. Here is the BNF::

     topdecl ::= ... | 'module' [ 'qualified' ] modid [ exports ] 'where' decls

   This declaration form is allowed only with ``-XLocalModules``.

   The ``module`` keyword in a local module declaration may not be the first
   lexeme in a file. (This is to avoid ambiguity with top-level, file-sized modules.)

#. The environment within the local module is an extension (superset) of the
   environment of the enclosing module. That is, all entities in scope in the
   enclosing module are in scope in the local module, but some entities may
   be in scope within the local module that are not in scope beyond it.

#. Definitions in a local module may be mutually recursive with definitions
   in other local modules or outside of any local module. That is, local
   modules influence scoping only, but not type-checking or dependency
   (which remain constrained by compilation units, as they are today).
   
#. A local module exports a set of original names and export-modules. This set
   of exports is interpreted into an environment (as described under "export-modules");
   that environment is merged with the environment in which the local module
   declaration is written.

   A. The local module may include an export list; if so, this export list
      defines the set of exports (original names and export-modules)
      from the local module.

   B. If the local module *M* omits an export list, it exports the names of all declarations
      made inside *M*. For each nested local module *N*, an export-module *N*
      is exported; this export-module contains all names in *M*\'s environment
      qualified by *N*.

   C. If the keyword ``qualified`` is missing, then all names exported by
      the local module are also added to the enclosing scope unqualified.
      That is, take the environment built by interpreting the exports
      of the local module *M*; the mappings in this environment will have
      the form *M.qualname* ``|->`` *origname*. Create a new environment
      with mappings dropping the *M.* qualification, and merge that with
      the ambient environment outside of *M*.

#. Modules may be extended via the declaration or importing of another
   local module of the same name. If local modules with the same name are in
   scope at the same time (either through importation or declaration) their
   contents are simply merged. Individual identifiers that are multiply
   defined will be an error if used ambiguously.

   This behavior is directly implied by the idea that scopes are simply
   environments mapping qualified names to original ones.

   In other words, local modules are not entities of themselves: module
   names are simply prefixes used in qualified names. This merging behavior
   is thus a natural consequence of the overall design and agrees with
   the behavior of standard Haskell (which allows multiple module aliases
   to be the same).
   
#. A new declaration form is introduced with the following BNF::

     decl ::= ... | 'import' 'module' modid [ impspec ]

   A. Without an ``impspec``: Consider all qualified names in scope with a
      *modid*\ ``.`` qualification. That is, find all mappings *modid.qualname* ``|->``
      *origname* in the environment. These names are brought into scope (that is,
      added to the local environment) without this *modid*\ ``.`` qualification.

   #. With an *impspec*: Interpret the *impspec* into an *impspec*\-environment.
      Each mapping will be of the form *qualname* ``|->`` *origname*. For each
      such mapping, it is an error if *modid.qualname* is not in the environment.
      Merge the *impspec*\-environment with the outer environment, bringing all
      such names into scope without the *modid* qualification.

   The declaration is allowed in ``let`` and ``where`` clauses.

   Note that the declaration form includes the word ``module`` to distinguish
   it from a normal ``import`` which induces a dependency on another file. An
   ``import module`` declaration cannot induce a dependency.

   If the declaration brings no new identifiers into scope (because, for example,
   the module name is not used to qualify any identifiers), it is an error.

   This declaration form is allowed only with ``-XLocalModules``.

#. Local modules may be exported with this export item::

     export ::= ... | 'module' [ 'qualified' ] modid [ exports ]

   The meaning of this export item depends on the presence of the ``qualified``
   keyword in the export item.

   A. With ``qualified``: This exports an export-module containing all names
      in scope with the *modid*\ ``.`` qualification. If qualified names are in scope
      further qualified by *modid*\ ``.``, export-modules for those further qualifications
      are added to the export-module for *modid*. If an *exports* is specified, only those entities
      are exported. Names in the *exports* can be written unqualified.

      The new behavior is allowed only with ``-XLocalModules``.

   #. Without ``qualified``: As specified in the `Haskell Report <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1000005.2>`_,
      point (5), this also exports (unqualified) all identifiers in scope both with
      and without the *modid*\ ``.`` prefix. If an *exports* is included, then
      only those identifiers are included. (This is not a change from the current behavior.)

Optional Change Specifications
------------------------------

Each numbered item in this section can be considered separately.

1. A local module declaration can omit the module name,
   making an anonymous local module. The names exported
   by an anonymous module are not added to the enclosing environment qualified,
   as there is no name to qualify by.

   New BNF::

     topdecl ::= ... | 'module' [ 'qualified' ] [ modid ] [ exports ] 'where' decls

   It is an error to omit a module's name and
   include the ``qualified`` keyword, unless there is also an empty export list.
   (Anonymous qualified modules with an
   empty export list but are still useful
   as a way of declaring instances that use local definitions.)

#. A module import can be preceded by the keyword ``import``::

     import ::= ... | [ 'import' ] 'module' modid [ impspec ]

   A module import with the ``import`` keyword additionally brings the named
   entities into scope without the *modid*\ ``.`` qualification.
   
#. Every ``class``, ``data``, ``newtype``, ``data instance``, and ``newtype
   instance`` declaration with an alphanumeric name implicitly creates a new local module. The name of
   the local module matches the name of the declared type. All entities (e.g.,
   method names, constructors, record selectors) brought into scope within the
   declaration, including the type itself, are put into this local module.
   
   If the pseudo-keyword ``qualified`` appears directly after the keyword(s)
   that begin the declaration, these internal definitions are not brought into
   the outer scope. Otherwise, they are (just like usual). Exception: the type
   itself is always brought into scope unqualified. This feature is enabled
   only when ``-XLocalModules`` is in effect, and it changes the BNF as follows
   (cf. the Haskell 2010 Report; this ignores other extensions, but it is easy
   to map this BNF to a more realistic one)::

     topdecl ::= 'data' ['qualified'] [context '=>'] simpletype ['=' constrs] [deriving]
               | 'newtype' ['qualified'] [context '=>'] simpletype '=' newconstr [deriving]
               | 'class' ['qualified'] [scontext '=>'] tycls tyvar ['where' cdecls]
               | 'data' 'instance' ['qualified'] [context '=>'] type ['=' constrs] [deriving]
               | 'newtype' 'instance' ['qualified'] [context '=>'] type '=' newconstr [deriving]
               | ...

   Associated ``data`` and ``newtype`` instances create modules at the level
   of the enclosing ``instance`` declaration: the ``data``\/\ ``newtype``
   module is *not* nested within the class module.

#. The ``import module`` declaration can unqualify multiple levels of qualification
   at once. This changes the BNF to ::

     decl ::= ... | 'import' 'module' modid { '.' modid } [ impspec ]

   where, following the Haskell Report, braces mean "0 or more". The semantics
   is the same as specified above, but looking under multiple levels of qualification
   at once.

   (Recall that ``modid`` contains module names like ``Data.Set``, which is
   considered as one qualification, just as it is currently.)
   
Further Examples
----------------

::

   module A ( module M1, module M2, module qualified M3, module qualified M4, module A ) where

   import Import1 as M1
   import qualified Import2 as M3

   module M2 ( m2a, m2b ) where
     m2a = ...
     m2b = ...

   module qualified M4 ( m4a, m4b ) where
     m4a = ...
     m4b = ...

* The ``module M1`` export item exports all the identifiers from ``Import1``.

* The ``module M2`` export item exports ``m2a`` and ``m2b``.

* The ``qualified module M3`` export item exports an export-module ``M3``, containing all the exports from ``Import2``.

* The ``qualified module M4`` export item exports and export-module ``M4`` (containing ``m4a`` and ``m4b``).

* The ``module A`` export item exports an export-module ``M2`` (containing ``m2a`` and ``m2b``),
  ``m2a``, ``m2b``, and an export-module ``M4`` (containing ``m4a`` and ``m4b``).

::

   module B where

   import A ( module M2, module M3, module M4, m4a )

* The import of ``module M2`` is an error: no export-module named ``M2`` is exported
  from ``A``.

* The import of ``module M3`` makes all the names exported by ``Import2`` available, qualified
  by ``M3.``. (As usual, an additional qualification of ``A.`` is allowed.)

* The import of ``module M4`` makes ``M4.m4a`` and ``M4.m4b`` available, along with ``A.M4.m4a``
  and ``A.M4.m4b``.

* The import of ``m4a`` is an error; ``A`` does not export ``m4a``.
    
Effect and Interactions
-----------------------

* Modules can now be defined inside other modules.

* The examples in the Motivation_ section are accepted.

* There is a potential ambiguity between local modules and top-level modules. In particular, this might
  happen between the implicit local module of a type declaration and a top-level module. For example::

    -- top of file:
    {-# LANGUAGE LocalModules #-}
    module A where

    import qualified T ( x )

    data T = MkT { x :: Int }

    y = T.x

  There will be two identifiers ``T.x`` in scope: both the one imported from ``T`` and the record selector
  in the type ``T``. This situation will lead to an error, as do other sources of ambiguity.

* The ability to detect dependencies of a module by parsing only a prefix of the module is retained.
  Local modules are always imported only by ``import module``, never plain ``import``. Plain ``import``
  statements remain at the top of the file.

* Other than corner cases around ambiguity, this proposal is backward compatible; it is not "fork-like".

* Proposal `#160`_ allows users to suppress field selectors, thus ameliorating a small part
  of what has motivated this proposal.

* This proposal does not appear to interact with Backpack. It does not address ``signature``\s,
  the key feature in Backpack. Perhaps the ideas here could be extended to work with ``signature``\s.

* Note that the new ``import module`` syntax works with traditional ``import qualified`` imports. For example::

    -- top of file:
    module A where

    import qualified B ( wiz, woz )
    import qualified C ( wiz, woz )

    x = if wiz then woz else error "blargh"
      where
        import module B

    y = woz + wiz
      where
        import module C

* If you have ``module M`` in an export list, and ``M`` contains local modules, then those local
  modules are exported.

* Other proposals and features in GHC move toward allowing duplicate record field names without
  qualification: `#160`_ suppresses top-level field selectors, `#282`_ proposes a new ``.``\-syntax
  for record access, and GHC already has ``-XDuplicateRecordFields`` and ``-XDisambiguateRecordFields``.
  This proposal would allow a different way to crack this nut, by giving users fine control
  over the scope of the selectors. This proposal might obviate ``-XDuplicateRecordFields``, but
  ``-XDisambiguateRecordFields`` is still useful with this proposal.

* A Template Haskell declaration splice can occur within a local module. Just as a top-level
  splice marks a scope boundary (declarations above the splice cannot refer to declarations
  below the splice), declaration splices within local modules do, too. The guideline here
  is that the local module system affects only the names that are in scope (and their qualifications
  and import/export), not any other aspect of the program.

* This proposal does not introduce any hierarchy into module names as the currently exist.
  For example, if we have ::

    import qualified Data.Set ( Set )
    import qualified Data.Map ( Map )

    import module Data

  will not bring ``Set.Set`` or ``Map.Map`` into scope. Those identifiers are qualified
  by ``Data.Set`` and ``Data.Map`` respectively.
  
Costs and Drawbacks
-------------------

* This is a significant new bit of implementation and specification, and it should require the
  requisite level of support from the community to be accepted.

* As highlighted in the "potential ambiguity" effect, above, this extension will rule out
  a few existing programs, when an import whose identifiers are used with qualifications
  shares a name with a locally defined type. The problem only arises with ``-XLocalModules``,
  though, and is easily remedied through a (local) renaming.

* This proposal does not really make a module into a first-class entity. Instead, it
  interprets a module essentially as the set of names that can be written qualified
  by the module name. This design is keeping in the spirit of the existing language,
  where we can have multiple ``import`` statements with the same ``qualified``
  abbreviation. But perhaps a different design, making modules more self-aware would
  be better. (Credit to @michaelpj for pointing this out.)

  See `#295`_ for a more complete treatment of the idea of making a module into a
  first-class entity.
  
Related Work
------------

There is much prior art. The list below is shamelessly cribbed from `#205`_.

* Proposal `#205`_. That proposal essentially tweaks the ``qualified`` feature to
  become more flexible and exportable. It has proved hard to digest (from the commentary),
  though, and solves fewer problems than this proposal. On the other hand, it is likely
  easier to implement.

  This proposal is inspired by some of the topics that came up in the conversation
  for `#205`_, and I'm grateful for @deepfire's efforts on that proposal.

* 2005 Coutts, ``as`` in export lists: `<https://mail.haskell.org/pipermail/libraries/2005-March/003390.html>`_ . Salient points:
  letting modules export other modules' contents qualified with the module name`
  
* 2006 Wallace, explicit namespaces for module names: `<https://ghc.haskell.org/trac/ghc/wiki/Commentary/Packages/PackageNamespacesProposal>`_ . Salient points:
  The declaration import namespace brings into availability the subset of the hierarchy of module names rooted in the package "foo-1.3", at the position ``Data.Foo``
  
* 2013 de Castro Lopo, qualified exports: `<https://wiki.haskell.org/GHC/QualifiedModuleExport>`_
  ``qualified module T`` in export list and is essentially a subset of this proposal.

* A worthwhile counter-proposal can be found at `#295`_. That proposal re-casts modules as
  entities proper; this one, in contrast, continues the historical treatment of modules
  simply as qualifications to identifiers. In my view, `#295`_ needs to do too much work
  to keep backward compatibility due to its more fundamental approach. Given a fresh start,
  I would likely prefer something like `#295`_ than what I have written here, but we're not
  making a fresh start.
  
Alternatives
------------

Beyond the `Related Work`_, there is wiggle room within this proposal for alternatives.

A. This proposal does not allow the export of a qualified local module such that
   importers get the identifiers unqualified. We could imagine a new export item
   ``import module M`` that exports all identifiers in scope with a ``M.`` prefix
   unqualified. I don't find this feature necessary, but it would fit with the
   rest of this proposal.

   For example, if a local module introduces ``M.x`` and ``M.y``
   into the top-level scope (but not ``x`` or ``y``), then this proposal offers no
   way of exporting ``M.x`` and ``M.y`` by listing only something about module ``M``
   such that importers get ``x`` and ``y`` unqualified. This is because exporting
   ``module M`` would *not* export ``x`` or ``y`` (because they are not in scope unqualified)
   and exporting ``qualified module M`` would give importers access to ``M.x`` and
   ``M.y``, but not ``x`` and ``y`` (unless an importer also said ``import module M``).
   A hypothetical ``import module M`` export item could satisfy this need.

#. Disallow local modules to be mutually recursive. The current proposal says
   that the local module system affects scoping only. However, we could instead declare
   that a mutual-dependency strongly-connected component (SCC) cannot include definitions
   in more than one module. This would disable mutual recursion between modules, but open
   the possibility of using local modules to explicitly stage compilation. This might
   allow, for example, the definition of a function and its usage in a Template Haskell
   splice in the same file (as long as they were in different local modules).

   I prefer the proposal as-is in this regard: modules should be about abstraction and
   naming, not about compilation dependencies. Compilation dependencies should be handled
   via a mechanism specifically suited for compilation dependencies, such as explicit
   staging like `#243`_.

#. Counter-proposal `#295`_ rightly observes that the export specifiers in this proposal
   are complicated. At the risk of making this proposal fork-like (that is, by changing the
   meaning of legacy constructs), these specifiers can be simplified. Here is an alternate
   formulation, replacing the last point in the specification section:

     * With ``-XLocalModules``, add this export item::

         export_item ::= ... | 'module' modid [ exports ]

       This exports an export-module *modid*, containing all entities in scope
       with the *modid*\ ``.`` prefix. If an *exports* is included, all entities
       listed are exported unqualified; these entities must be in scope with
       the *modid*\ ``.`` prefix. An *exports* of ``..`` exports all entities
       with the *modid*\ ``.`` prefix unqualified.

       This interpretation replaces the meaning of a ``module`` export item
       in standard Haskell.

   This new formulation is essentially a mix of the ``qualified`` and unqualified
   versions of the ``module`` export item in the main proposal. It exports the module
   (call it ``M``) itself in a way that importing modules will have to access entities
   with ``M.`` syntax. But anything listed in the export specifier will be exported
   unqualified. This allows users to write ``module M(..)`` as an export item to export
   unqualified all entities in scope with a ``M.`` prefix. If you want to export a thinned
   module without exporting anything unqualified, just make a shim local module.

   I am agnostic on whether I prefer the meaning for ``module`` export items above,
   or whether I prefer this alternative.

#. Counter-proposal `#295`_ includes module aliases, stating that its approach
   makes such a definition possible. Module aliases work under this proposal, too:
   A declaration ``module New = Old`` could simply allow ``New.`` to work as a qualifier
   for any entity in scope with an ``Old.`` prefix. I don't find module aliases to
   be useful, but they could be added to this proposal if there is a desire to.

#. When exporting a module ``qualified``, we may also want to rename it. Here is an
   example, thanks to @evincarofautumn::

     module Data.Set (Set, qualified module Data.Set as Set) where

     data Set = ...
     fromList = ...

   Note the ``as Set`` in the export list. Adding this feature to the proposal would be
   easy, at the risk of further complicating export items.

#. Anonymous modules are written by omitting the module name:
   ``import module (x, y) where ...``. An earlier version of this proposal denoted
   an anonymous module by using an ``_``: ``import module _ (x, y) where``. After
   seeing the suggestion, I have a mild preference for the "omit module name" version,
   but perhaps others feel differently.

#. This proposal automatically imports all names from a local module, unless that
   module specifies the ``qualified`` keyword. Alternatively, we could have local
   modules default to the ``qualified`` behavior, requiring an ``import`` to
   get the names unqualified. (Earlier versions of this proposal indeed defaulted
   to ``qualified``.)
   
Future Work
-----------

I see a few future directions along these lines, but I leave it to others to flesh these out.

1. We can imagine *parameterized local modules*, where all the functions defined therein share
   a sequence of parameters. This would resurrect the ideas behind `#40`_. This would bring
   us close to ML-style functors.

#. Haskell currently requires three distinct concepts to coincide: *compilation units* are the
   chunks that go through the compiler all at once, *source files* are distinct files on disk,
   and *modules* are groups of related definitions and can define an abstraction barrier.

   This proposal allows modules to become smaller than these other two. By writing a module to
   collect others, modules can also be larger than the other two (as is true today).

   However, it would be nice to separate the treatment of compilation units and source files,
   as well. This would allow, for example, the inliner and specializer to make decisions with
   respect to more definitions (if the compilation unit is larger than the source file). It would
   also allow for easy mutual dependency between files: just put the SCC of definitions into
   a multi-file compilation unit.

#. Some language extensions and other compiler settings, such as warning flags,
   might make sense on a per-module basis. We can imagine setting these on local
   modules instead of only at the top-level module in a file. With such an extension
   to this design, we might nab abandoned proposal `#88`_ on language extensions
   or tabled proposal `#234`_ on warning flags.

#. Formalise all of this along the lines of `A Formal Specification of the Haskell 98 Module
   System <https://web.cecs.pdx.edu/~mpj/pubs/hsmods.pdf>`_, by Diatchki, Jones, and Hallgren.
   
Unresolved questions
--------------------

1. Should ``-XLocalModules`` be required to *import* a local module? Paraphrased from
   a `comment <https://github.com/ghc-proposals/ghc-proposals/pull/283#issuecomment-548804545>`_
   by @maralorn:

   Consider a user having the following import::

     import qualified Foo

   Right now, the user can be sure that this will never include something like
   ``Foo.Bar.baz``. With this extension that is going to be possible. If a
   local module can be imported without extensions, then this proposal changes
   the possible meanings of an import statement in Haskell quite a bit.
   Someone not familiar with this change might get very confused by a e.g.
   ``Text.Encoding.decode`` in the code when there is no import statement for
   something called ``Text.Encoding``.

   I'm personally split on this point. Requiring ``-XLocalModules`` to import a local
   module goes against the general ethos of extensions ("necessary at definitions but
   not usages"), but the point above is a good one. I'm happy to let the committee
   decide on this point.

#. What is the grand plan here? There are several other proposals that interact
   with this one (such as local types `#273`_ and ``-XNoFieldSelectors`` `#160`_)
   and possibilities of future proposals addressing further breaking up the triple
   confluence of (file = module = compilation unit). This current proposal is just
   one step, but perhaps with a larger plan, we would see that this proposal is
   not future-compatible. I don't have such a plan to offer, but concerns have
   been raised (chiefly by @Ericson2314) about the lack of such a plan before
   accepting this proposal.
   

Implementation Plan
-------------------
I do *not* volunteer to implement, but I wanted to write this down, as it seems like
a nice way to solve these problems.
