Allow implicit qualified module import
======================================

.. author:: Tristan de Cacqueray
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/500>`_.
.. contents::

The goal of this proposal is to enable direct access to any available module without requiring an import statement.
This removes an unnecessary restriction, making the language simpler.


Motivation
----------
This proposal's main purpose is to enable direct package use.
This is particularly useful for:

- Functions that are used only once, such as ``System.Environment.getArgs``.
- Temporary type annotations and debugging functions, such as ``Debug.Trace.trace``.
- Portable code samples that can be moved without affecting the module imports.

For example, this propose change would make the following program valid
(assuming a cabal file with build depends and default extensions):

::

 module Main where

 import Servant.API (Get, JSON, (:>))

 type MyApi = "answer" :> Get '[JSON] Integer

 server :: Servant.Server MyApi
 server = pure 42

 main :: IO ()
 main = Network.Wai.Handler.Warp.run 8080 app
   where
     app :: Network.Wai.Application
     app = Servant.serve (Data.Proxy.Proxy @MyApi) server


The status quo is not satisfactory because it requires adding explicit imports:

::

 -- Without this proposal, these imports are required
 import qualified Data.Proxy
 import qualified Network.Wai
 import qualified Network.Wai.Handler.Warp
 import qualified Servant

Managing such imports has the following negative effects:

- It increases the line count and can cause merge conflicts.
- The author needs to jump to the top of the file each time a new module is added or removed.
- It encourages using unqualified imports because unqualified is the default, which can lead to import conflicts.



Proposed Change Specification
-----------------------------
Change 1: add a new ``ImplicitQualifiedImport`` language extension. When the extension is on:

- A qualified name ``M.N.x`` is looked up in the top level environment
  (see `Import Declarations <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1010005.3>`_
  in the Hakell report).

- If the lookup fails, and if there is no user-written import declaration ``import qualified M.N ...`` (see the "Qualified_" example below) or ``import X ... as M.N`` (see the "Renamed_" example),
  then instead of reporting an out-of-scope error, behave as if an extra import declaration is added: ``import qualified M.N(x)``.

User-written import declarations are taken into account following the principle of least surprise: the extension does not interfer with explicit import declarations.
Note that unqualified import declarations, such as ``import M.N(y)``, are not taken into account,
because in those cases, adding the extra import declaration is unambiguous (see the "Unqualified_" example).


Change 2: deprecate the ``-fimplicit-import-qualified`` flag.

GHCi already enables implicit qualified import via this flag, but that does not work with GHC.
Moreover, supporting this feature in source files requires a new language extension because it changes how to interpret the source file, and other tools would have to understand it as well.
Lastly, the current flag implementation does not follow the same principle of least surprise (see the "GHCi_" example below).
Therefore this proposal deprecates the ``-fimplicit-import-qualified`` flag in favor of
``-XImplicitQualifiedImport``, so that the behavior is consistent between GHC and GHCi.


Examples
--------

.. _Qualified:

Qualified Import
~~~~~~~~~~~~~~~~

In this example, the ``Data.List`` and ``Data.Maybe`` modules are imported qualified.

::

 import qualified Data.List hiding (head)
 import qualified Data.Maybe (fromMaybe)

 foo = Data.List.head []
 bar = Data.Maybe.fromJust Nothing

The extension does not enable using names that would contradict the user-written declarations:

- ``Data.List.head`` is explicitely hidden, so we don't add an extra import. This fails with a not-in-scope error (as usual).
- ``Data.Maybe.fromJust`` is explicitly not imported, so we don't add an extra import. This fails with a not-in-scope error (as usual).


.. _Renamed:

Renamed Import
~~~~~~~~~~~~~~

In this example, the module ``A.B`` is imported in place of the module ``C.D``.

::

 module A.B( f, g ) where
   (f, g) = (True, True)

 module C.D( f ) where
   f = False

 module M where
   import A.B as C.D hiding (f)
   foo = (C.D.g, A.B.g)
   bar = C.D.f

The extension does not enable using names that would be ambiguous:

- ``C.D.g`` binds to the ``g`` exported by ``A.B`` (as usual).
- ``A.B.g`` isn't in scope by the usual rules, but we can try adding an extra import ``import qualified A.B(g)``. This is not ambiguous, and binds to the ``g`` exported by ``A.B``.
- ``C.D.f`` isn't in scope by the usual rules, but a module is already renamed as ``C.D``, so we don't try to add an extra import. This fails with a not-in-scope error (as usual).

Trying to resolve ``C.D.f`` would be ambiguous because it can be found through ``import qualified C.D(f)`` or ``import qualified A.B as C.D(f)``.
It is unclear what to do in this situation, therefore we don't add an extra import.


.. _Unqualified:

Unqualified Import
~~~~~~~~~~~~~~~~~~

In this example, the module ``A`` and ``B`` are imported unqualified:

::

 module Main

 import A (a)
 import B hiding (b)

 foo = (A.x, B.b)

The extension enables using qualified names that are unambiguous:

- ``A.x`` isn't in scope by the usual rules, and ``A`` is not imported qualified and it is not a renamed module, so we can try adding an extra import ``import qualified A(x)``.
- Similary for ``B.b``, even though ``b`` is hidden at the top level, we can try adding an extra import ``import qualified B(b)``.


This behavior is particularly useful for such module:

::

 module Demo

 import Data.Text (Text, pack)

 foo = pack "hello" :: Text
 bar = Data.Text.unpack foo


- ``Data.Text.unpack`` isn't in scope by the usual rules, but we can try adding an extra import ``import qualified Data.Text(unpack)``.


.. _GHCi:

GHCi Session
~~~~~~~~~~~~

The following GHCi session is presently valid with ``-fimplicit-import-qualified``:

::

 $ ghci -fimplicit-import-qualified
 Prelude> import qualified Data.List hiding (head)
 Prelude Data.List> Data.List.head [42]
 42
 Prelude> import Data.List as Data.List.NonEmpty
 Prelude Data.List Data.List.NonEmpty> Data.List.NonEmpty.fromList [42]
 42 :| []


With ``-XImplicitQualifiedImport``:

- ``Data.List.head`` is not implicitly imported (because ``Data.List`` is already imported qualified) and the expression fails with a not-in-scope error.
- ``Data.List.NonEmpty.fromList`` is not implicitly imported (because ``Data.List.NonEmpty`` is a renamed module) and the expression fails with a not-in-scope error.



Effect and Interactions
-----------------------
The proposed change enables using any module without requiring an import statement.

Interactions with existing language or compiler features:

Only unknown fully qualified names (that would otherwise throw ``Not in scope`` errors) are affected.
The other language or compiler features are left unchanged.
In particular, typeclass instances are not changed. With ``Data.Generics.Labels.Field'``, the Field instance of Symbol from the generic-lens package is not imported,
and the user still needs to add ``import Data.Generics.Labels ()``.

The extension does not take into account names that are part of a Quasiquotation, e.g. between ``[|`` and ``|]``.

Interactions with `Local modules (under review) <https://github.com/ghc-proposals/ghc-proposals/pull/283>`_ may be implemented similarly to Java, where the first
matching import is used, as demonstrated in this `experiment <https://github.com/ghc-proposals/ghc-proposals/pull/500#issuecomment-1101814148>`_. Or, by following
the principle of least surprise, the extension may refuse to implicitly import a module that can be found in multiple locations.


Costs and Drawbacks
-------------------

Development and Maintenance Costs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To compile a module source file, GHC parses the import declarations in order to get the module's dependencies and define a build plan.
When ``ImplicitQualifiedImport`` is on, the module's dependencies may be implicitly imported anywhere in the module, therefore GHC needs to parse the whole module to get its dependencies.

Thereafter, when the ``implicit-import-qualified`` flag is on, GHCi looks up *all* the available modules before reporting a not-in-scope error.
When ``ImplicitQualifiedImport`` is on, this lookup needs to be restricted to *all* the available modules, except the ones that have been renamed or imported qualified, in order to respect the principle of least surprise (see the Change Specification).

Learnability
~~~~~~~~~~~~
Since this is a syntactic extension, there is a potential cultural fragmentation within the community.
Though I believe that using the proposed extension will be quite clear,
even for those who do not know about the extension.

This extension may improve the language's learnability for novice users by:

- Reducing the length of code samples, and,
- Simplying new module usage, e.g. for one-off experiments and temporary annotations.

Drawbacks
~~~~~~~~~
The extension may reduce a module's readability: its external requirements would no longer be explicitly listed in the import section. To know what a module imports, readers have to read through the entire module.
This may also complicate changing modules dependencies order.


Alternatives
------------
Another helpful solution would be to enable local import, e.g. in a function definition.

Local modules proposed in `#283 <https://github.com/ghc-proposals/ghc-proposals/pull/283>`_ can also enable
using fully qualified names without adding imports. However this requires using a fat prelude to export a
curated list of modules, while ImplicitQualifiedImport enables using any module without relying on such fat prelude.

Imports may be stored in a special ``Mod.imports`` as described in `this comment <https://github.com/ghc-proposals/ghc-proposals/pull/500#issuecomment-1101972865>`_.
However such listing still need to be maintained, and the proposal may be implemented independently.

The extension may work only for modules outside the current package.
The advantage of that would be that implicit imports wouldn't affect the dependency analysis and the implementation is much simpler.
The (admittedly major) disadvantage is that specifying the semantics requires talking about packages, which otherwise are not part of the Haskell standard.

The propose change is enabled by default in some languages such as OCaml, Rust, Elixir and Java.

Unresolved Questions
--------------------
TBD

Implementation Plan
-------------------

I (Tristan de Cacqueray) could implement this change.
