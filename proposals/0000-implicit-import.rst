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
The main purpose is to be able to directly use a package.
This is particularly useful for single use functions, such as ``Servant.serve``.
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
- The author needs to jump to the top of the file each time a new module is used.
- It complicates code refactoring when moving a definition requires moving (or copying) its import.


Proposed Change Specification
-----------------------------
I propose an extension to enable implicit qualified imports.

When the ``ImplicitQualifiedImport`` language extension is enabled,
unknown fully qualified symbols are gracefully resolved early in the compilation pipeline by
injecting an implicit ``import qualified`` statement when necessary.
The pipeline might need an extra pass to collect missing imports.

GHCi already implements this feature thanks to the flag ``-fimplicit-import-qualified``,
but the flag does not seem to work with GHC.
Moreover the flag is not documented in ``ghc --show-options``, but it is accepted.
Thus this proposal deprecates the ``-fimplicit-import-qualified`` flag in favor of the
``-XImplicitQualifiedImport`` modifier, so that the behavior is consistent between GHC and GHCi.


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

- Fully qualified import with hidden declarations are not affected: with ``import Data.Maybe hiding (mapMaybe)``, using ``Data.Maybe.mapMaybe`` should not be valid.
- Modules available through multiple package will be disambiguated using the PackageImports extension.
- Only unknown fully qualified names will be affected.


Costs and Drawbacks
-------------------
TBD: estimate development and maintenance costs.

This extension may improve the language's learnability for novice users by:

- Reducing the length of code samples, and,
- Simplying new module usage, e.g. for one-off experiments and temporary annotations.

The main drawback is that the extension may reduce the readability of a module:
its external requirements would no longer be explicitely listed in the import section.


Alternatives
------------
Another helpful solution would be to enable local import, e.g. in a function definition.


Unresolved Questions
--------------------
TBD
