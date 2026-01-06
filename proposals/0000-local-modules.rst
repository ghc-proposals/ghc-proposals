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

.. _`Haskell 2010 Report`: https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5

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

   With this proposal's optional change 4.3, these declarations would be accepted. The constructors
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

     module (f, g) where
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

     module (f) where
       f :: ...
       f = ...

       data SpecialDataType = ...

       class LocalClass a b c where ...

5. When we expect users to import our library qualified, we have no way of
   signaling this beyond documentation; we also have no way of making it easy
   to import the module qualified correctly.

   With this proposal, we can do this easily::

     -- top of file:
     module Data.Set ( Set, module qualified Set ) where

       module Set ( Set, fromList ) where
         data Set = ...
         fromList = ...

   An importer who says ``import Data.Set`` will get access to ``Set`` (the
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

       module C where
         meth :: ...

   This example shows that modules may be *extended*. The ``class C``
   declaration implicitly creates module ``C``, which is then extended below.

   An import specifier of ``C(..)`` will not import ``meth`` after this
   change. Instead, importers must say ``module C`` in the import list.
   If they want unqualified access to ``C``\ 's contents, they will additionally
   have to write ``import module C`` separately. This new form works to replace
   today's ``C(..)`` and is customizable, and so may become the recommended way
   of importing a type with its auxiliary definitions.

7. When importing a library qualified, there are often stretches of code which
   work with the imported type, performing many operations on values of that type.
   In these stretches, the qualifications can become noisy. ::

     module Main where

     import qualified Data.Set as Set

     frobbleSets :: Set.Set Bool -> Set.Set Int -> Set.Set Char
     frobbleSets = ...

   In ``frobbleSets``, there will be many ``Set.`` qualifications.

   With this proposal, we could add ::

     where
       import module Set

   to the end of the definition of ``frobbleSet`` to bring all the exports
   from ``Data.Set`` into scope unqualified, locally. This reduces noise in
   the implementation of ``frobbleSets`` (though, regrettably, not in its
   type signature) and
   may serve to encourage users to import more identifiers qualified.

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

* **Module name**: A dot-separated list of alphanumeric identifiers starting with a
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

* **Qualifier** and **qualified name**: A qualified name optionally
  begins with a qualifier, where a qualifier is a module name. Because
  the qualifier is optional, all occurrence names are also qualified names.

  A module-name qualification must have no whitespace between it and the ``.``, nor
  between the ``.`` and the occurrence name.

* **Entity**: An entity is a definition that can be exported and imported.
  An entity is uniquely identified by its original name. Entities include variables, classes, datatypes, and
  constructors, among a few other constructs.

* **Environments**: An environment is mapping of names to original names.
  We distinguish two specific types of environment here: a **QEnv** maps
  qualified names to original names, while an **OEnv** maps occurrence
  names to original names.

  At all locations in Haskell source code, some ambient QEnv describes how
  to look up identifier occurrences.

* **in scope**: A name is said to be **in scope** when it is in the current
  QEnv.

* **Scope**: A scope is a lexical region of a program where all name occurrences are
  looked up in a QEnv.

* **Module**: A module is a set of declarations of entities, along with a set
  of exported names.

* **Export specification**: An export specification, or *expspec*, is the list of
  identifiers exported from a module. It is written after the module name.

* **Export item**: An export item is one element of an export specification.
  An export item may be written using a qualified name.

* **Exports**: The exports from a module is a mapping
  from occurrence names to original names *occname ↦ origname* (that is, an OEnv), where
  for each such binding, we require that the occurrence name of *origname*
  is exactly *occname*. We can thus view exports as a set of
  original names, but the presentation of this idea as a map sets the
  stage nicely for the extension proposed.

  To get from an expspec to an export OEnv, we must *interpret* the expspec. This
  is done by looking names up in the ambient QEnv that defines what is in scope.

  In code::

    type ExpSpec = [ExpItem]

    interpretExpSpec :: (ExpSpec, QEnv) -> OEnv
    interpretExpSpec(expspec, ambient_env)
      = concatMap (\item -> interpretExpItem(item, ambient_env)) expspec

    interpretExpItem :: (ExpItem, QEnv) -> OEnv
    interpretExpItem(item, ambient_env)
      = { occ ↦ orig | modid.occ ∈ item
                     , modid.occ ↦ orig ∈ ambient_env }

  We see here that, even though the user might write a qualified name in an
  export item, only its occurrence name makes it into the export OEnv.

  The ``modid.occ ∈ item`` syntax here allows us to retrieve names from
  export items like ``T(..)``; we leave this operation abstract.

  It is an error if multiple export items returns OEnvs with mappings from
  the same occurrence name to distinct original names. We thus need not worry about left- or right-biasing
  of the merge operation on OEnvs.

  (This definition does not cover ``module`` exports.)

* Define the *qualify* operation on an OEnv ``X``::

      qualify :: (ModuleName, OEnv) -> QEnv
      qualify(modid, X) = { modid.occ ↦ orig | (occ ↦ orig) ∈ X }

  In English: ``qualify(modid, X)`` prepends ``modid`` to the domain elements
  of ``X``.

* Define the *strip* operation on a QEnv ``E``::

      strip :: (ModuleName, QEnv) -> OEnv
      strip(modid, E) = { occ ↦ orig | (modid.occ ↦ orig) ∈ E }

  In English: ``strip(modid, E)`` selects all entities whose
  qualified names starts with ``modid``, and removes the
  ``modid`` from their names (yielding a plain occurrence name).

  Note that ``qualify(modid, strip(modid, E))`` is simply a filter, keeping all those names
  in ``E`` which start with ``modid``.

* **Import specification**: An import specification, or *impspec*, can be listed
  in parentheses in an ``import`` statement to restrict what is imported.
  We say we *interpret* an *impspec* with respect to an export OEnv
  to create a different OEnv.
  Interpreting an impspec produces the union of interpreting each import item (defined
  just below)::

    type ImpSpec = [ImpItem]

    interpretImpSpec :: (ImpSpec, OEnv) -> OEnv
    interpretImpSpec(impspec, export_env) =
      concatMap (\item -> interpretImpItem(item, export_env)) impspec

  It is allowed for the merge operation on OEnvs here to encounter the same
  occurrence name more than once; however, it is an invariant that any
  repeated domain occurrence name is mapped to the same original name.
  Accordingly, we need not worry about left- or right-biasing the merge
  operation.

  (An *impspec* may be specified
  via complement, using ``hiding``; this proposal will not worry about this
  detail.)

* **Import item**:  An element in an import specifier is an *import item*.
  Each import item is interpreted by looking it up in the export OEnv of
  the module being imported. If this lookup fails, then report an error. ::

     interpretImpItem :: (ImpItem, OEnv) -> OEnv
     interpretImpItem(item, export_env) = { occ ↦ orig | occ ∈ item
                                                       , occ ↦ orig ∈ export_env }

  Import items are described by the ``import`` nonterminal in the `Haskell 2010 Report`_,
  and we leave the ``occ ∈ item`` operation left abstract here.

* **Imports**: An ``import`` statement names a module ``M`` and (optionally) an
  import-specifier ``impspec``.

  Let ``X`` be the export OEnv of ``M``.

  Let ``I`` be the interpretation of ``impspec`` with respect to ``X``. (That is,
  ``I = interpretImpSpec(impspec, X)``.
  If ``impspec`` is omitted, let ``I = X``.

  Let the *module alias* ``A`` be the module name appearing after the ``as`` in
  the ``import`` statement. If there is no ``as``, then the module alias ``A``
  is ``M``.

  The ``import`` statement brings names into scope by adding entries
  to the global QEnv. Concretely, merge ``qualify(A, I)`` into the global QEnv.
  If the ``import`` statement does not use
  ``qualified``, additionally merge ``I`` into the global QEnv.

Core Proposal Change Specification
----------------------------------

This proposal's specification is divided into pieces. This core piece is a necessary
component of the overall proposal. Later pieces can be chosen piecemeal.

1. **Definitions**

   A. A module name is now just one alphanumeric identifier beginning with a capital
      letter. Previously, a module name could have dot-separated components, but
      no longer.

   #. Use the meta-variable *modids* to refer to a dot-separated list of module names.

   #. A *top-level module* is one that includes an entire file. The initial
      ``module ... where`` syntax at the top of a file defines a top-level module.
      A top-level module's name is a *modids*.
      We also call this structure a *top-level-module-name*, where the hyphenation
      suggests that a top-level-module-name is not just a descriptor on a module
      name. (The former is a list of the latter.)

   #. Qualifiers now are dot-separated lists of module names; that is, just like
      *modids*. Accordingly,
      a qualified name represents the same strings (dot-separated capitalized
      identifiers, followed by final dot and an occurrence name)
      as it did previously, but with now more structure in the qualifier (previously,
      the qualification was a monolithic *modid*, which had lexical structure around
      capitalized words and dots, but was still considered one module name; now,
      we have a dot-separated list of *modid*\ s, which can be split apart by e.g.
      ``strip``).

   #. An original name is now a pair of the name of the top-level-module-name that
      defines it and a qualified name.

      For instance in

      ::

         module Data.Foo where

            module Bar where
              module Baz where
                x = 1

      The original name of the ``x`` definition is ``(Data.Foo, Bar.Baz.x)``

   #. The exports of a module are now a QEnv (not an OEnv): a map from qualified name to
      original names. This is a strict broadening of the current definition of
      an export environment. ::

        interpretExpSpec :: (ExpSpec, QEnv) -> QEnv
        interpretExpSpec = ...  -- unchanged
        interpretExpItem :: (ExpItem, QEnv) -> QEnv
        interpretExpItem = ...  -- see below

   #. The interpretation of an *import item* is now a QEnv, instead of an OEnv.
      The interpretation of an *import specification*, or *impspec*,
      is now also a QEnv, instead of an OEnv. (Recall that a qualified name can omit
      the qualifier
      qualifications, so this change is a broadening of the definition of an *impspec*
      and is backward-compatible.) ::

        interpretImpSpec :: (ImpSpec, QEnv) -> QEnv
        interpretImpSpec = ...  -- unchanged
        interpretImpItem :: (ImpItem, QEnv) -> QEnv
        interpretImpItem = ...  -- see below

   #. Generalize the ``qualify`` and ``strip`` operations to both accept and return
      QEnvs::

         qualify(modids, E) = { modids.qualname ↦ orig | (qualname ↦ orig) ∈ E }

         strip(modids, E) = { qualname ↦ orig | (modids.qualname ↦ orig) ∈ E }

      These operations now also operate on *modids*, not just atomic module names.
      The operations are thus generalizations of the previous versions.

   #. It turns out that we do not need the concept of an OEnv in this proposal.

#. Introduce a new extension ``-XLocalModules``.

#. **Imports**

   A. Import items can now mention qualified names. The `Haskell 2010 Report`_ defines ::

        import ::= var
               |   tycon [ '(' '..' ')' | '(' cname1 ',' ... ',' cnamen ')' ]
               |   tycls [ '(' '..' ')' | '(' var1 ',' ... ',' varn ')' ]

      where literals are put in quotes ``'``, brackets denote an optional sequence
      of tokens, a pipe denotes alternatives, and ``...`` denotes a list.

      This proposal allows qualified names in place of the unqualified names in the
      Report::

        import ::= qvar
               |   qtycon [ '(' '..' ')' | '(' cname1 ',' ... ',' cnamen ')' ]
               |   qtycls [ '(' '..' ')' | '(' var1 ',' ... ',' varn ')' ]

      The interpretation of a qualified-name import is the same as an occurrence-name
      import: just look it up in the export QEnv::

         interpretImpItem(item, export_env) = { qual ↦ orig | qual ∈ item
                                                            , qual ↦ orig ∈ export_env }

      The only difference with the previous interpretation of an import item is
      that we now allow a qualified name instead of an occurrence name.

   #. A new import item is introduced::

        import ::= ... | 'module' modids [ impspec ]

      We must now give the semantics of this new import item; that is, we must
      describe how to interpret a ``module modids impspec`` import item as a
      QEnv with respect to an export QEnv ``X``.

      First, the nested ``impspec`` is interpreted with respect to ``strip(modids, X)``.
      That is, we understand the ``impspec`` in a context where we're looking only
      at exported names qualified by ``modids``.
      This interpretation gives us a QEnv ``I0``.
      The QEnv described by import item
      ``module modids impspec`` is then ``qualify(modids, I0)``.

      In absence of an *impspec*, ``module modids`` denotes
      ``qualify(modids, strip(modids, X))``: that is, it denotes all
      bindings in ``X`` whose domain element has a ``modids.`` prefix.

      In code::

        interpretImpItem('module' modids impspec, export_env)
          = let I0 = interpretImpSpec(impspec, strip(modids, export_env)) in
            qualify(modids, I0)
        interpretImpItem('module' modids, export_env)
          = qualify(modids, strip(modids, export_env))

      This new form of import is allowed only with ``-XLocalModules``.

      For example::

        module A where
          import qualified B (module X (f, g)) as H

      Suppose ``B`` exports ``{ X.f ↦ (B, X.f), X.g ↦ (Q, g) }``. (Those
      pairs are original names. This means that ``f`` is defined within
      a module ``X`` within ``B``, while ``g`` is defined in some other
      module ``Q`` and only re-exported.) Then this example import statement
      adds ``{ H.X.f ↦ (B, X.f), H.X.g ↦ (Q, g) }`` to the global
      environment.

      Note that the import item ``module M (x, y)`` means the same
      as ``M.x, M.y`` in an import specification.

   #. An ``import`` statement adds bindings to the global environment,
      just as described in **Imports** in the Background.
      The only difference is that all OEnvs are now QEnvs.

#. **Exports**

   A. The Haskell 2010 Report defines export items this way::

        export ::= qvar
               |   qtycon [ '(' '..' ')' | '(' cname1 ',' ... ',' cnamen ')' ]
               |   qtycls [ '(' '..' ')' | '(' qvar1 ',' ... ',' qvarn ')' ]
               |   'module' modid


      With ``-XLocalModules`` specified, expand this definition to be ::

        export ::= qvar [ 'qualified' ]
               |   qtycon [ '(' '..' ')' | '(' cname1 ',' ... ',' cnamen ')' ] [ 'qualified' ]
               |   qtycls [ '(' '..' ')' | '(' qvar1 ',' ... ',' qvarn ')' ] [ 'qualified' ]
               |   'module' [ 'qualified' ] modids [ exports ]

      The nonterminal *exports* (from the `Haskell 2010 Report`_)
      denotes an export specification: a list of ``export``.

      We must now give these new export items semantics by saying how they
      are interpreted.

   #. The interpretation of Haskell98-style export items (that is, without ``qualified``)
      is unchanged::

        interpretExpItem(haskell98item, ambient_env)
          = { occ ↦ orig | modids.occ ∈ haskell98item
                         , modids.occ ↦ orig ∈ ambient_env }

      The only difference here from above is now we write ``modids`` for the qualifier.

      (Here, ``haskell98item`` refers to a non-\ ``module`` Haskell98 export item,
      and ``modids`` is any dot-separated sequence of module names.)

      Note that ``occ ↦ orig`` is returned, not the full qualified name. This is
      unchanged from previously, but it was previously a requirement (in order to
      be type-correct) to return an ``occ ↦ orig`` binding, while now the type
      has been generalized to return a QEnv.

   #. If the export item is a ``qvar``, a ``qtycon``, or a ``qtycls`` (henceforth
      called ``qual``) followed
      by ``qualified``, the export
      QEnv from the current module includes ``qual ↦ orig`` (where ``orig``
      is the original name found by looking up ``qual`` in the ambient QEnv).

      If ``qual`` has no qualifier, report a warning controlled by
      ``-Wunqualified-qualified-exports``. In this case, the ``qualified``
      keyword is a no-op.

      If there are bundled identifiers exported (in ``(..)``), these are
      also exported with the same qualification as the root identifier,
      even if the bundled identifiers are written with different qualifications.
      (Example: the export item ``M.T(K1, M.K2, N.K3) qualified`` exports
      ``M.T``, ``M.K1``, ``M.K2``, and ``M.K3``.)

      This behavior is different than the behavior without the ``qualified``
      keyword, which causes the unqualified name to be mapped to the original
      name in the export environment (as is done prior to this proposal).

      In code::

        interpretExpItem(haskell98item@(modids0.occname(..)) 'qualified', ambient_env)
          = { modids0.occ1 ↦ orig | modids1.occ1 ∈ haskell98item
                                  , modids1.occ1 ↦ orig ∈ ambient_env }

      The key part here is that the return is a qualified-name mapping. Note
      that the qualification in the exported mapping is ``modids0``, not the
      ``modids1`` that might be found in the list of bundled qualified names.
      See the ``M.T`` example a few paragraphs above.

   #. For an export item like ``module qualified modids [ exports ]``:

      If ``exports`` is given, we must interpret this export specification into
      an export QEnv. Let ``E`` be the ambient QEnv. Then, ``exports`` is
      interpreted with respect to the QEnv ``strip(modids, E)`` (that is, ``E``
      with ``modids.`` stripped off from every domain element). Call this
      intepretation of ``exports`` to be ``X'``. Let ``X = qualify(modids, X')``.

      If ``exports`` is not given, then let ``X = qualify(modids, strip(modids, E))``. That is,
      ``X`` includes all bindings in ``E``
      whose domain element is qualified by ``modids``.

      This export item exports the QEnv ``X``.

      In this way, the export item ``module qualified M ( f, g )`` means the
      same as ``module qualified M ( f ), module qualified M ( g )`` and
      ``M.f qualified, M.g qualified``.

      In code::

        interpretExpItem('module' 'qualified' modids expspec, ambient_env)
          = let X' = interpretExpSpec(expspec, strip(modids, ambient_env)) in
            qualify(modids, X')
        interpretExpItem('module' 'qualified' modids, ambient_env)
          = qualify(modids, strip(modids, ambient_env))

   #. For an export item like ``module modids [ exports ]``:

      The ambient, in-scope QEnv is ``E``. Define a new QEnv ``E'``
      as follows::

        E' = { qual ↦ orig | qual ↦ orig ∈ E
                           , modids.qual ↦ orig ∈ E }

      If ``exports`` is specified, let ``X`` be the interpretation of ``exports``
      with respect to ``E'``.

      If ``exports`` is not specified, let ``X = E'``.

      This export item exports the environment ``X``.

      In code::

        interpretExpItem('module' modids maybe_exports, ambient_env)
          = let E' = { qual ↦ orig | qual ↦ orig ∈ ambient_env
                                   , modids.qual ↦ orig ∈ ambient_env } in
            case maybe_exports of
              Nothing -> E'
              Just expspec -> interpretExpSpec(expspec, E')

      Other than the new possibility of writing ``exports``, this is not a change
      from current behavior: a ``module modids`` export exports all identifiers
      in scope both with and without the ``modids`` qualification.

      One non-backward-compatible alternative below is not to require that
      names be in scope without qualification; this would make this unqualified
      case more similar to the qualified case.

      (This is still slightly backward-incompatible in an `obscure scenario`_.)

   #. If there is no export list given for a module, all entities declared in that
      module (and only those) are included in the export QEnv. This notion can
      be made formal as follows::

        X = { qual ↦ orig | qual ↦ orig ∈ ambient_env
                          , orig = (MODIDS, _) }

      where ``MODIDS`` is the fully qualified name of the current module.

      This export ``X`` may contain overlapping entries. Here is an example::

        module Top where
          module A where
            x = 5
          module B where
            x = True

      In this example, the ambient QEnv for ``Top`` includes mappings
      ``{ A.x ↦ (Top, A.x), x ↦ (Top, A.x), B.x ↦ (Top, B.x), x ↦ (Top, B.x) }``.
      This QEnv contains two mappings for ``x``. By itself, this does not cause
      a problem: it is only a usage site of ``x`` that would cause an ambiguity
      error (as usual). However, since ``Top`` does not have an export list,
      we would normally export all of the ambient QEnv in ``Top``, causing trouble
      for importing modules.

      We thus exclude from the default export list any qualified name that is mapped to multiple
      original names. More formally::

        X' = X \ { qual ↦ orig | qual ↦ orig ∈ X
                               , qual ↦ orig' ∈ X
                               , orig /= orig' }

      Now, ``X'`` is our final export QEnv.

      When any variable is excluded from the final export QEnv in this way, GHC warns,
      controlled by ``-Wsuppressed-duplicate-exports``.

   #. **Examples**. For instance in

      ::

        module M (module qualified X (f)) where

          module qualified X where
            f = …
            g = …

      the module ``M`` exports ``{X.f ↦ (M, X.f)}``. And in

      ::

       module M (module qualified X) where

         module qualified X where
           f = …
           g = …

      the module ``M`` exports ``{X.f ↦ (M, X.f), X.g ↦ (M, X.g)}``. And in

      ::

         module X where
           module Y where
             f = …
             g = …

         module M (module X) where
           import X

      the export environment of ``M`` is ``{f ↦ (X, Y.f), g ↦ (x, Y.g)}``.

#. **Local Modules**

   A. Introduce a new declaration form
      to declare a new module called a *local module*. Here is the BNF::

        topdecl ::= ... | 'module' [ 'qualified' ] modids [ exports ] 'where' '{' topdecls '}'

      This declaration form is allowed only with ``-XLocalModules``.

      The ``module`` keyword in a local module declaration must not be the first
      lexeme in a file. (This is to avoid ambiguity with top-level,
      file-sized modules.)

      Note that this new form is part of ``topdecl``, meaning that it cannot appear
      in a ``let`` or ``where``. However, because a local module contains ``topdecl``\ s,
      local modules may be arbitrarily nested.

   #. There are now two kinds of modules: top-level modules and local
      modules. Local modules lack import statements, but can define all
      the range of entities definable in a top-level module.

      The term *module* can refer to either a top-level or a local module.

   #. The ambient QEnv within the local module extends the ambient QEnv
      of the enclosing module with its local definitions. This means that
      all qualified names in scope in the enclosing module are in scope in the
      local module.

   #. Definitions in a local module may be mutually recursive with definitions
      in other local modules or outside of any local module. That is, local
      modules influence scoping only, but not type-checking or dependency
      (which remain constrained by compilation units, as they are today).

   #. Let the ambient QEnv of the local module (call it ``M``, located within
      top-level module ``Top`` and under other local modules ``modids``) be ``E``.

      If ``exports`` is given, the export QEnv
      of ``M`` is the result of interpreting ``exports`` with respect to ``E``.

      If ``exports`` is not given, then the export QEnv ``X`` of ``M``
      is the subset of ``E`` containing all mappings to original names that
      appear within ``M``. That is, ::

        X = { qual ↦ orig | qual ↦ orig ∈ E
                          , orig = (Top, modids.M._, _occ) }

   #. A local module definition for ``M`` adds entries to the ambient QEnv
      ``E0`` of the enclosing module.
      Let the export QEnv of ``M`` be ``X``; then ``E0`` is extended
      with the environment ``qualify(M, X)``. If the ``qualified`` keyword is missing from
      the definition of the local module, then ``X`` is additionally added to ``E0``.

      Example::

        module Outer where
          module Inner where
            five = 5

      The export QEnv of ``Inner`` is ``{five ↦ (Outer, Inner.five)}``.
      The ambient QEnv of ``Outer`` is thus extended with ``{five ↦ (Outer, Inner.five),
      Inner.five ↦ (Outer, Inner.five), Outer.Inner.five ↦ (Outer, Inner.five)}``. The
      first of these comes from adding the export QEnv (call it ``X``, as above)
      of ``Inner`` to the ambient QEnv
      in ``Outer``, because the declaration for ``Inner`` does not say ``qualified``.
      The second of these comes from adding ``qualify(Inner, X)``. The third comes
      from the rule in the Haskell 2010 Report (Section 5.5.1) that new names are
      brought into scope both qualified and unqualified.

#. A new declaration form is introduced::

     decl ::= ... | 'import' 'module' modids [ impspec ]

   The declaration is allowed in ``let`` and ``where`` clauses. It
   modifies the environment for the scope of the ``let`` / ``where``
   block. For instance in

   ::

     let
       x = u
       import module A
     in
     v

   The environment is modified for ``u`` and ``v``.

   Let ``E`` be the ambient QEnv, as it exists outside the ``let`` / ``where`` block
   (or, at the top level, the ambient QEnv as it exists after processing all ``import``
   statements, but nothing else).
   Let ``I`` be the interpretation
   of ``impspec`` with respect to the environment ``strip(modids, E)``. (If ``impspec``
   is missing, then let ``I = strip(modids, E)``.)
   The declaration ``import module modids (impspec)`` modifies the ambient QEnv
   to become ``E ∪ I``.

   Note that the declaration form includes the word ``module`` to distinguish
   it from a normal ``import`` which induces a dependency on another file. An
   ``import module`` declaration cannot induce a dependency.

   This form adds bindings to some base QEnv (either the QEnv from outside the ``let``
   / ``where`` or the QEnv built from ``import`` statements only). The reason for this
   design is to avoid the need for a fixpoint computation. If we allowed ``import module``
   statements to build on one another and have no ordering, we would need to consider
   them in all possible orders, including multiple usages of the same one. This would
   require a fixpoint computation. We avoid this complication by restricting the QEnv
   the ``import module`` statements operate on. An alternative would be to make the
   ordering of ``import module`` statements significant, but this seems positively
   anti-Haskellish.

   If the declaration adds no new bindings into the environment (because, for example,
   the module name is not used to qualify any identifiers), it is a warning
   ``-Wredundant-local-import``.

   This declaration form is allowed only with ``-XLocalModules``.

Optional Change Specifications
------------------------------

Each numbered item in this section can be considered separately.

1. A local module declaration can omit the module name,
   making an anonymous local module. The names exported
   by an anonymous module are not added to the enclosing environment qualified,
   as there is no name to qualify by. (More formally: merge the export
   environment of the local module with the ambient environment.)

   New BNF::

     topdecl ::= ... | 'module' [ 'qualified' ] [ modid ] [ exports ] 'where' decls

   It is an error to omit a module's name and
   include the ``qualified`` keyword.

   The original name of an entity declared within an anonymous local module
   includes a special module name ``anonymous`` in place of the name of the
   anonymous module. This special module name is guaranteed not to conflict
   with any other module name, because it begins with a lower-case letter.

   Example::

     module Top where
       x = True
       module where
         x = False

       a = x
       b = Top.x

   In this example, the definition of ``a`` is ambiguous, because ``x`` is
   bound to both ``Top.x`` and ``Top.anonymous.x``. However, ``b`` is OK
   (``b`` will be ``True``), because the inner ``x`` cannot be written
   qualified.

#. Introduce a new export item with the following BNF::

     export ::= ... | 'module' modids '(' '..' ')'

   This export item exports all qualified names in scope with the ``modids`` prefix,
   stripped of that prefix. ::

     interpretExpItem('module' modids '(' '..' ')', ambient_env)
       = strip(modids, ambient_env)

   This new form is really the non-qualified equivalent of the new export
   item ``module qualified modids``. It needs the ``(..)`` syntax to differentiate
   it from ``module modids``, which is designed to be backward-compatible.

   There is no non-qualified equivalent of ``module qualified modids exports``, with
   an explicit export list.

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

   It is an error to use the ``qualified`` keyword with a non-alphanumeric name.
   Module names are always alphanumeric.

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

Let ``I1`` be the export environment from ``Import1`` and ``I2`` be the
export environment from ``Import2``. We now give the export environments
generated from each export item from module ``A``:

* ``module M1``: ``I1``

* ``module M2``: ``{m2a ↦ (A, M2.m2a), m2b ↦ (A, M2.m2b)}``

* ``module qualified M3``: ``M3 . I2``. That is, this export item exports all
  mappings imported from ``Import2``, further qualified by ``M3``.

* ``module qualified M4``: ``{M4.m4a ↦ (A, M4.m4a), M4.m4b ↦ (A, M4.m4b)}``

* ``module A``: ``{m2a ↦ (A, M2.m2a), m2b ↦ (A, M2.m2b), M2.m2a ↦ (A, M2.m2a),
  M2.m2b ↦ (A, M2.m2b), M4.m4a ↦ (A, M4.m4a), M4.m4b ↦ (A, M4.m4b)}``

::

   module B where

   import A ( module M2, module M3, module M4, m4a )

The export environment from ``A`` is the union of the above environments.
We now give the environments imported by each import item in the import statement.
Each import environment is additionally added with the ``A.`` qualification, though
we omit those bindings below.

* ``module M2``: ``{M2.m2a ↦ (A, M2.m2a), M2.m2b ↦ (A, M2.m2b)}``

* ``module M3``: ``M3 . I2``

* ``module M4``: ``{M4.m4a ↦ (A, M4.m4a), M4.m4b ↦ (A, M4.m4b)}``

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
  in the type ``T``. This situation will lead to an error, as do other sources of ambiguity. Note that
  the code above is valid (and unambiguous) today, and so this example shows a small regression compared
  to the status quo, but only when enabling ``-XLocalModules``.

* The ability to detect dependencies of a module by parsing only a prefix of the module is retained.
  Local modules are always imported only by ``import module``, never plain ``import``. Plain ``import``
  statements remain at the top of the file.

* Other than corner cases around ambiguity, this proposal is backward compatible; it does not create a "`fork <https://github.com/ghc-proposals/ghc-proposals/#review-criteria>`_".

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

* This proposal describes *top-level modules* as distinct from *local modules*, but this distinction
  has little import. Here is the full set of differences between these concepts:

  1. The ``module`` keyword for a top-level module is the first lexeme in a file; the ``module``
     keyword for a local module must not be the first lexeme in a file.

  #. A top-level module can have ``import`` statements; a local module can have only ``import module``
     statements.

  #. An ``import`` statement can name only a top-level module, not a local module.

  That's it! If you like, you can think of a ``import X`` statement as a combination of
  ``import qualified X`` and ``import module X`` statement: the first loads the external compilation
  unit named ``X`` (in a file ``X.hs``)
  and brings lots of ``X.blah`` entities into scope, and the second removes the ``X.`` qualification.

  Another way to think about this is that there is really no distinction between top-level modules
  and local modules, but there is a distinction between a compilation unit and a module. An ``import``
  statement names a compilation unit, granting access to any modules it contains. Haskell separately
  has two restrictions:

  A. Every compilation unit must contain exactly one module (which may contain other modules); the
     compilation unit must have the same name as its one module.

  #. ``import`` statements must go before all other statements (except, optionally, for a ``module``
     header) in a compilation unit (in order to
     support finding dependencies while parsing only a prefix of a file).

* This proposal may obviate the existing *bundling* mechanism for pattern synonyms, where a pattern
  synonym may be exported with some type constructor. If we adopt options 4.2 and 4.3, then you can
  export ``module T(..)`` in place of today's ``T(..)``. But
  the exporter could also write code like ::

    module M ( module T(..) ) where

    data T = MkT1 ... | MkT2

    module T where
      pattern MkT3 x = ...

  Now, importers will get all of ``MkT1``, ``MkT2``, and ``MkT3`` with ``import M ( module T(..) )``.
  Indeed, because the ``module T(..)`` form is customizable, it would be preferred over the
  ``T(..)`` import item. We could imagine changing the meaning of ``T(..)`` to mean ``module T(..)``
  or deprecating that form, but this proposal does not do so. This change is not
  backward-compatible, and controlling it by the presence of the ``-XLocalModules``
  extension seems too subtle.

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

  See `#295`_ for a more complete treatment of the idea of making a module into a
  first-class entity.

.. _`obscure scenario`:

* This proposal introduces a backward-incompatibility in an obscure scenario::

    module M ( module L ) where

    import L ( x )
    import L.Internal ( z )
    import L.Internal as Internal ( z )

  The export QEnv from ``M`` would include both ``x`` and ``Internal.z``. This is
  because the ambient QEnv in ``M`` includes both ``L.Internal.z`` and ``Internal.z``,
  both referring to the same original name.

  There is thus the small potential for abstraction leakage.

  It would be possible to introduce a warning to detect this scenario.

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

1. Disallow local modules to be mutually recursive. The current proposal says
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

#. This proposal includes the possibility for a new export item
   ``'module' modids '(' '..' ')'``, whose semantics is very close to
   ``'module' 'qualified' modids``. Instead, we could give these semantics
   to ``'module' modids`` (without the ``(..)``). This would simplify the specification
   a little (and mean that removing the ``qualified`` keyword changes only whether
   the produced QEnv includes names with an additional qualification),
   but it would not be backward compatible. For example::

     module M ( module Data ) where

     import Data
     import Data.Set

   The alternative proposed here would mean that ``M`` would export e.g. ``Set.fromList``
   -- even though this module uses entirely Haskell98 syntax. While we could control
   the new behavior by stating that it applies only when ``-XLocalModules`` is specified,
   this change may be unexpected, and it could potentially lead to abstraction leakage,
   if a module exports more than it thinks it does.

   I favor the proposal as written, because I like backward compatibility. But if
   we were designing from scratch, I would favor this alternative.

#. The proposal offers no non-qualified equivalent of ``module qualified modids exports``,
   where the export QEnv is built without regard to what identifiers are in scope
   without the ``modids`` prefix.
   We could say that ``module modids exports`` behaves like
   ``module qualified modids exports``, instead of the semantics given above.

   There is no backward-compatibility problem here, because including an export-list
   in a module export is new. However, it does mean that the ``module M ( x )`` and
   ``module M`` export items would have strangely different meanings. For example::

     module X ( {- change what goes here -} ) where

     import qualified M ( x )
     import M ( y )

   If the export item is ``module M ( x )``, then ``x ↦ (M, x)`` is exported.
   If the export item is ``module M``, then only ``y ↦ (M, y)`` is exported. This
   is because ``x ↦ (M, x)`` is not in the ambient QEnv.

   If we choose to adopt the backward-incompatible alternative above around this point,
   then I suggest also taking this alternative. Otherwise, skip as too confusing.

#. Counter-proposal `#295`_ rightly observes that the export specifiers in this proposal
   are complicated. At the risk of making this proposal fork-like (that is, by changing the
   meaning of legacy constructs), these specifiers can be simplified. Here is an alternate
   formulation, replacing the relevant point under **Exports** in the specification section:

   * For an export item like ``module modids [ exports ]``:

     If ``exports`` is given, we must interpret this export specification into
     an export QEnv. If ``E`` is the ambient QEnv, ``exports`` is
     interpreted with respect to the environment ``strip(modids, E)`` (that is, ``E``
     with ``modids.`` stripped off from every domain element). Call this
     intepretation of ``exports`` to be ``X``.

     If ``exports`` is not given, then let ``X = strip(modids, E)``. That is,
     ``X`` includes all bindings in ``E``
     whose domain element is qualified by ``modids``, with that qualification
     stripped off.

     This export item exports the QEnv ``X``.

     In this way, the export item ``module M ( f, g )`` means the
     same as ``module M ( f ), module M ( g )``.

   This allows users to write ``module M`` as an export item to export
   unqualified all entities in scope with a ``M.`` prefix.



   I am agnostic on whether I prefer the meaning for ``module`` export items above,
   or whether I prefer this alternative.

#. Counter-proposal `#295`_ includes module aliases, stating that its approach
   makes such a definition possible. Module aliases work under this proposal, too:
   A declaration ``module New = Old`` could simply allow ``New.`` to work as a qualifier
   for any entity in scope with an ``Old.`` prefix. I don't find module aliases to
   be useful, but they could be added to this proposal if there is a desire to.

#. When exporting a module ``qualified``, we may also want to rename it. Here is an
   example, thanks to @evincarofautumn::

     module Data.Set (Set, module qualified Data.Set as Set) where

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

#. This proposal puts the ``qualified`` keyword after export names, like
   ``module M ( Set.fromList qualified )``. It could go before the name,
   like ``module M ( qualified Set.fromList )``. If it goes before, we would
   have to specify where the ``qualified`` goes with respect to the ``type``
   or ``pattern`` namespace selectors allowed in export/import lists.

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
