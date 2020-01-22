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
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
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
`#205`_ (Structured module exports/imports), `#273`_ (local types) and
provides a tempting way toward a resurrection of rejected proposal `#40`_
("context fixes").

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

     module MyPrelude ( qualified module BL
                      , qualified module BS
                      , Set
                      , qualified module Set ) where

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
   Alternatively, the user could declare ``data qualified Fin ...`` and
   ``data qualified Elem ...`` to make ``Nat``\'s constructors available in
   the global scope but not the others.

3. When a function ``f`` needs a helper ``h``, we can declare ``h`` in a
   ``where`` clause. However, suppose both ``f`` and ``g`` need ``h``. Now,
   ``h`` must be declared at the top level, meaning all the rest of the
   definitions in the module can see ``h``, even if ``h`` is really very
   specific to ``f`` and ``g``.

   With this proposal, we can model this situation nicely::

     import module _ (f, g) where
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

     import module _ (f) where
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

       import module Set ( Set )
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

   Import statements *do* have to change here: instead of ``C(..)``, they must
   become ``import module C``. However, this new import statement is a drop-in
   replacement for ``C(..)`` and may become preferable (as it is customizable
   in the way demonstrated here).

Proposed Change Specification
-----------------------------

1. Introduce a new extension ``-XLocalModules``.

2. Introduce a new declaration form (allowed only at the top level of a
   module -- i.e. not in a ``let`` or ``where``)
   to declare new modules called *local modules*. Here is the BNF::

     decl ::= ... | [ 'import' ] 'module' modname [ export_spec ] 'where' decls
     modname ::= conid | '_'

   Using ``_`` as the module name indicates an *anonymous local module*.

   This declaration form is allowed only with ``-XLocalModules``.
     
3. All
   declarations in scope at the declaration point of a local module (that is,
   outside the module itself) are in scope for the entire local module.

4. Definitions in a local module may be mutually recursive with definitions
   in other local modules or outside of any local module. That is, local
   modules influence scoping only, but not type-checking or dependency.
   
5. A local module declaration brings into scope names listed in its export
   list. These names are always brought into scope qualified by the local
   module name, unless that module name is ``_``. If the declaration includes
   the ``import`` keyword, the names are also brought into scope unqualified
   by the local module name. (In the case of nested local modules, the names
   might be qualified by inner module names.)

6. It is an error to specify a non-empty export list, name a module ``_``, and
   not include the ``import`` keyword. (Modules named ``_`` without an
   non-empty export list but without the ``import`` keyword are still useful
   as a way of declaring instances that use local definitions.)

7. Every ``class``, ``data``, ``newtype``, ``data instance``, and ``newtype
   instance`` declaration implicitly creates a new local module. The name of
   the local module matches the name of the declared type. All entities (e.g.,
   method names, constructors, record selectors) brought into scope within the
   declaration, including the type itself, are put into this local module.
   
   If the pseudo-keyword ``qualified`` appears directly after the keyword(s)
   that begin the declaration, these internal definitions are not brought into
   the outer scope. Otherwise, they are (just like usual). Exception: the type
   itself is always brought into scope unqualified. This feature is enabled
   only when ``-XLocalModules`` is in effect.

   Associated ``data`` and ``newtype`` instances create modules at the level
   of the enclosing ``instance`` declaration: the ``data``\/\ ``newtype``
   module is *not* nested within the class module.

8. Local modules may be extended via the declaration or importing of another
   local module of the same name. If local modules with the same name are in
   scope at the same time (either through importation or declaration) their
   contents are simply merged. Individual identifiers that are multiply
   defined will be an error if used ambiguously.

9. A new declaration form is introduced with the following BNF::

     decl ::= ... | 'import' 'module' conid [ import_spec ]

   This declaration takes all entities (or those entities named in the
   ``import_spec``) in scope with a *conid*\ ``.`` and brings them into scope
   into the local context without that qualification. (They may be more deeply
   qualified, if *conid* exports local modules.)

   The declaration is allowed in ``let`` and ``where`` clauses.

   Note that the declaration form includes the word ``module`` to distinguish
   it from a normal ``import`` which induces a dependency on another file. An
   ``import module`` declaration cannot induce a dependency.

   This declaration form is allowed only with ``-XLocalModules``.

10. Local modules may be imported with this import item::

      import_item ::= ... | [ 'import' ] 'module' conid [ import_spec ]

    This brings the local module named *conid* into scope. It is an error if
    this import item appears in a list of imports from a module that does not
    export the local module *conid*.

    Also brought into scope are the entities listed in the *import_spec*;
    these are brought into scope qualified with the *conid*\ ``.`` prefix. If
    the *import_spec* is not provided, then all entities in *conid* are
    brought into scope (qualified).

    If the ``import`` keyword is included, then all entities brought into
    scope qualified are also brought into scope unqualified.

    This new form of import is allowed only with ``-XLocalModules``.

11. Local modules may be exported with this export item::

      export_item ::= ... | [ 'qualified' ] 'module' conid [ export_spec ]

    The meaning of this export item depends on the presence of the ``qualified``
    keyword in the export item.

    With ``qualified``: This exports the local module *conid* and all entities
    in scope qualified with the *conid*\ ``.`` prefix. If no local module
    *conid* exists but entities are in scope with the *conid*\ ``.`` prefix
    (because a top-level module *conid* exists or *conid* is after the ``as`` in
    a top-level module import declaration), then a local module *conid* is created
    for export. In all cases, if an *export_spec* is specified, only those entities
    are exported. Names in the *export_spec* can be written unqualified.

    Without ``qualified``: This exports the local module *conid* (if one exists).
    As specified in the `Haskell Report <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1000005.2>`_,
    point (5), this also exports (unqualified) all identifiers in scope both with
    and without the *conid*\ ``.`` prefix. If an *export_spec* is included, then
    only those identifiers are included. Note that, because this exports identifiers
    unqualified, if this form is used with a local module, those identifiers become
    both available unqualified and within the local module in importing modules
    (depending on whether they import the local module).

    The new behavior is allowed only with ``-XLocalModules``.

Further Examples
----------------

::

   module A ( module M1, module M2, qualified module M3, qualified module M4, module A ) where

   import Import1 as M1
   import qualified Import2 as M3

   module M2 ( m2a, m2b ) where
     m2a = ...
     m2b = ...

   import module M2 ( m2a )

   import module M4 ( m4a, m4b ) where
     m4a = ...
     m4b = ...

* The ``module M1`` exports all the identifiers from ``Import1``.

* The ``module M2`` exports ``m2a``, ``M2`` (as a local module), ``M2.m2a``, and ``M2.m2b``.

* The ``qualified module M3`` exports ``M3`` (as a local module) and all the identifiers from ``Import2``, qualified with ``M3.``.

* The ``qualified module M4`` exports ``M4`` (as a local module), ``M4.m4a``, and ``M4.m4b``.

* The ``module A`` exports ``M2`` (as a local module), ``m2a``, ``M4`` (as a local module), ``m4a``, and ``m4b``.

::

   module B where

   import A ( module M2, import module M3, m4a )

* The following identifiers will be available: ``M2`` (as a local module), ``M2.m2a``, ``M2.m2b``, ``M3`` (as a local module), all
  the identifiers from ``Import2`` (which was exported as ``M3``) both qualified with ``M3.`` and unqualified, and ``m4a``.
    
Effect and Interactions
-----------------------

* Modules are now compositional.

* The examples in the Motivation_ section are accepted.

* There is a potential ambiguity between local modules and top-level modules. In particular, this might
  happen between the implicit local module of a type declaration and a top-level module. For example::

    -- top of file:
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
  
  
Costs and Drawbacks
-------------------

* This is a significant new bit of implementation and specification, and it should require the
  requisite level of support from the community to be accepted.

* This proposal does not really make a module into a first-class entity. Instead, it
  interprets a module essentially as the set of names that can be written qualified
  by the module name. This design is keeping in the spirit of the existing language,
  where we can have multiple ``import`` statements with the same ``qualified``
  abbreviation. But perhaps a different design, making modules more self-aware would
  be better. (Credit to @michaelpj for pointing this out.)
  
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

A. Drop specification point (7) making implicit modules for each type declaration. It's
   not required by the rest of the proposal. I like it, though.

B. Drop the possibility of the ``import`` keyword in the ``module`` import
   item. Users can always just write one more ``import module`` statement to
   unqualify the module's contents. Yet, to me, it just seems more ergonomic
   to get to the chase right away here.

C. Drop point (8) allowing extension of existing local modules. That was added so that
   definitions could be mixed in with, e.g., constructors of an implicit local module
   surrounding a type declaration. But it also seems like a nice way of flexibly mixing
   modules together. It's an inessential feature.

D. This proposal does not allow the export of a qualified local module such that
   importers get the identifiers unqualified. We could imagine a new export item
   ``import module M`` that exports all identifiers in scope with a ``M.`` prefix
   unqualified. I don't find this feature necessary, but it would fit with the
   rest of this proposal.

E. Drop the possibility of anonymous local modules; that is, remove ``_`` as a possibility
   of ``modname``. However, I like the idea of anonymous modules, as they serve to reduce
   the scope of some declarations without bothering the programmer to write a name for
   the module.

F. Disallow local modules to be mutually recursive. The current proposal says (point 4)
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

G. Counter-proposal `#295`_ rightly observes that the export specifiers in this proposal
   are complicated. At the risk of making this proposal fork-like (that is, by changing the
   meaning of legacy constructs), these specifiers can be simplified. Here is an alternate
   formulation, replacing the point (11) in the specification section:

     * With ``-XLocalModules``, add this export item::

         export_item ::= ... | 'module' conid [ export_spec ]

       This exports the local module *conid* and all entities in scope
       with the *conid*\ ``.`` prefix. If no local module *conid* exists
       but entities are in scope with the *conid*\ ``.`` prefix
       (because a top-level module *conid* exists or *conid* is after the ``as``
       in a top-level module import declaration), then a local module *conid*
       is created for export. If an *export_spec* is included, all entities
       listed are exported unqualified; these entities must be in scope with
       the *conid*\ ``.`` prefix. An *export_spec* of ``..`` exports all entities
       with the *conid*\ ``.`` prefix unqualified.

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

H. Counter-proposal `#295`_ includes module aliases, stating that its approach
   makes such a definition possible. Module aliases work under this proposal, too:
   A declaration ``module New = Old`` could simply allow ``New.`` to work as a qualifier
   for any entity in scope with an ``Old.`` prefix. I don't find module aliases to
   be useful, but they could be added to this proposal if there is a desire to.

I. When exporting a module ``qualified``, we may also want to rename it. Here is an
   example, thanks to @evincarofautumn::

     module Data.Set (Set, qualified module Data.Set as Set) where

     data Set = ...
     fromList = ...

   Note the ``as Set`` in the export list. Adding this feature to the proposal would be
   easy, at the risk of further complicating export items.
   
Future Work
-----------

I see a few future directions along these lines, but I leave it to others to flesh these out.

1. We can imagine *parameterized local modules*, where all the functions defined therein share
   a sequence of parameters. This would resurrect the ideas behind `#40`_. This would bring
   us close to ML-style functors.

2. Haskell currently requires three distinct concepts to coincide: *compilation units* are the
   chunks that go through the compiler all at once, *source files* are distinct files on disk,
   and *modules* are groups of related definitions and can define an abstraction barrier.

   This proposal allows modules to become smaller than these other two. By writing a module to
   collect others, modules can also be larger than the other two (as is true today).

   However, it would be nice to separate the treatment of compilation units and source files,
   as well. This would allow, for example, the inliner and specializer to make decisions with
   respect to more definitions (if the compilation unit is larger than the source file). It would
   also allow for easy mutual dependency between files: just put the SCC of definitions into
   a multi-file compilation unit.

3. Some language extensions and other compiler settings, such as warning flags,
   might make sense on a per-module basis. We can imagine setting these on local
   modules instead of only at the top-level module in a file. With such an extension
   to this design, we might nab abandoned proposal `#88`_ on language extensions
   or tabled proposal `#234`_ on warning flags.

4. Formalise all of this along the lines of `A Formal Specification of the Haskell 98 Module
   System <https://web.cecs.pdx.edu/~mpj/pubs/hsmods.pdf>`_, by Diatchki, Jones, and Hallgren.
   
Unresolved questions
--------------------

1. Would it be equivalent to say that all traditional ``import`` declarations create
   local modules? That is, after I say ``import M (foo)``, then I can say ``M.foo``.
   Does that mean we have a local module ``M``? I *think* such a treatment is consistent
   with this proposal, but I'm somehow not sure.

2. Should ``-XLocalModules`` be required to *import* a local module? Paraphrased from
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

3. What is the grand plan here? There are several other proposals that interact
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
