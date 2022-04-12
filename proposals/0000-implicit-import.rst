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
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/497>`_.
.. contents::

The goal of this proposal is to enable direct access to any available module without requiring an import statement.
This removes an unnecessary restriction, making the language simpler.


Motivation
----------
This proposal's main purpose is to enable direct package use.
This is particularly useful for:

- Functions that are used only once, such as ``Servant.serve``.
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

- It increases the file length.
- The author needs to jump to the top of the file each time a new module is added or removed.
- It encourages using unqualified imports because unqualified is the default, which can lead to import conflicts.



Proposed Change Specification
-----------------------------
I propose an extension to enable implicit qualified imports.

GHCi already implements this feature via the ``-fimplicit-import-qualified`` flag,
but it does not seem to work with GHC.
Moreover the flag is not documented in ``ghc --show-options``, but it is accepted.
Therefore this proposal deprecates the ``-fimplicit-import-qualified`` flag in favor of
``-XImplicitQualifiedImport``, so that the behavior is consistent between GHC and GHCi.

When the ``ImplicitQualifiedImport`` extension is enabled:

- The downsweep phase builds the ModuleGraph by looking at the whole module (instead of just the header)
  to collect the dependencies arising from implicit qualified name usage. This may be done by using the
  Lexer to collect ``ITq*`` tokens and looking up matching home modules.
- GHC calls the qualified name lookup function GHCi uses (``GHC.Rename.Env.lookupQualifiedNameGHCi``)
  before throwing a ``Not in scope`` error.


Examples
--------
Here are some example functions that are often used locally and
it would be useful to call them without having to add an import statement:

- ``Control.Concurrent.threadDelay``
- ``Data.Char.isAlpha``
- ``Data.Foldable.traverse_``
- ``Data.Maybe.mapMaybe``
- ``Data.Set.fromList``
- ``Data.Text.pack``
- ``Debug.Trace.trace``
- ``System.Environment.getArgs``
- ``Text.Printf.printf``


Effect and Interactions
-----------------------
The proposed change enables using any module without requiring an import statement.

Interactions with existing language or compiler features:

- Fully qualified imports with hidden declarations are not respected. With ``import qualified Data.Maybe hiding (mapMaybe)``, using ``Data.Maybe.mapMaybe`` is valid.
  If necessary, it might be possible to handle this case by adding extra checks to the new qualified name lookup implementation.
- Only unknown fully qualified names (that would otherwise throw ``Not in scope`` errors) are affected.
  The other language or compiler features are left unchanged.
  In particular, typeclass instances are not changed. With ``Data.Generics.Labels.Field'``, the Field instance of Symbol from the generic-lens package is not imported,
  and the user still needs to add ``import Data.Generics.Labels ()``.


Costs and Drawbacks
-------------------
TBD: estimate development and maintenance costs.

This extension may improve the language's learnability for novice users by:

- Reducing the length of code samples, and,
- Simplying new module usage, e.g. for one-off experiments and temporary annotations.

The drawbacks are:

- It may reduce a module's readability: its external requirements would no longer be explicitly listed in the import section.
- It may complicate changing modules dependencies order.
- It makes it harder to swap out dependencies for ones with similar interfaces but different implementations.


Alternatives
------------
Another helpful solution would be to enable local import, e.g. in a function definition.

Local modules proposed in `#283 <https://github.com/ghc-proposals/ghc-proposals/pull/283>`_ can also enable
using fully qualified names without adding imports. However this requires using a fat prelude to export a
curated list of modules, while ImplicitQualifiedImport enables using any module without relying on such fat prelude.


Unresolved Questions
--------------------
TBD
