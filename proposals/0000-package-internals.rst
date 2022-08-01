Accessing package internal modules
==================================

.. author:: Richard Eisenberg and Simon Peyton Jones
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/528>`_.
.. sectnum::
.. contents::

Package authors include an ``exposed-modules`` stanza in ``.cabal`` files to
list the modules available for import outside the package. Other modules are
not available for import. Yet it is sometimes useful to access even unexposed
("internal") modules. This proposal offers a way to do so.

Note that internal modules of a package are *not* a part of that package's
published API and thus changes therein would *not* require a major version
bump. These modules do remain internal!

Motivation
----------

1. Suppose package ``pack`` has several internal modules, whose exports are
   not intended for the public API of ``pack``. Yet the author of ``pack``
   still wants to test functions exported by these internal modules.

   The author currently has two options, both bad:

   1. Expose the internal modules, perhaps naming them with ``Internal`` or
      similar.

   2. Structure the project to recompile the entire package in order to run
      the testsuite, instead of using the already-compiled package.

   This proposal allows the modules to remain internal while still being
   available for testing without recompilation.

2. Suppose package ``pack`` exports a very general interface to an particular
   algorithm. Because it is so general, ``pack`` offers no way of, say, using
   real-world data to seed the algorithm. Instead, downstream packages such as
   ``pack-json`` and ``pack-csv`` provide such access.

   If an internal module in ``pack`` needs testing, then there is no choice but
   to expose the internal module, so it can be tested in a downstream package.

3. Suppose two packages ``pack1`` and ``pack2`` are developed together, with
   ``pack2`` depending on ``pack1``. Because the two packages form logically one
   whole (but are separated because, say, ``pack1`` has many fewer dependencies
   than ``pack2`` or other reasons), ``pack2`` needs to import internal modules
   from ``pack1``. Yet these internal modules are not intended to form part of
   any public API.

   This is the case within GHC's boot libraries, where the ``ghc-prim`` package
   is not intended to have a public API. Yet users cannot easily learn that
   ``ghc-prim`` is essentially an implementation detail and should instead depend
   only on ``base`` (which re-exports much, but not all, of ``ghc-prim``). This
   proposal is intended to protect users from unknowingly depending on ``ghc-prim``
   modules, which might change frequently.

Proposed Change Specification
-----------------------------

1. Add a new command-line flag ``-expose-package-internals <pkg>``, where ``<pkg>`` is understood
   in the same way as the argument to the existing ``-package`` flag.

#. It is an error for the ``<pkg>`` named in ``-expose-package-internals`` not to be exposed.

#. Imports of modules from a package named in ``-expose-package-internals`` skip the exposed-module check.

#. The error GHC produces when a non-exposed module is imported warns the user about
   exposing internal modules, with something like ::

    Could not load module 'Data.OldList';
    it is a hidden module in the package 'base-4.15.1.0'.
    If you wish to be able to access an internal module anyway, you may
    specify '-expose-package-internals base-4.15.1.0'; however, be warned
    that the API from internal modules may change between minor version releases.

#. The `Haskell Package Versioning Policy <https://pvp.haskell.org/>`_ specifies that changes in
   internal modules do *not* require a major version change.

#. Haddock output does not change: only exposed modules are included in the documentation.
   Users wishing to depend on internal modules are expected to consult the source code.

#. For the next three releases of GHC, any import of a module exported from ``ghc-prim``
   will emit a warning (controlled by ``-Wghc-prim-exports`` and part of ``-Wcompat``),
   unless ``-expose-package-internals ghc-prim`` is specified. The warning will inform
   users that all modules in ``ghc-prim`` will become internal after three releases.
   When that happens, the warning itself will also be removed.


Examples
--------

*Example.hs*\ ::

  module Example (foldl) where
  import Data.OldList   -- internal module in `base`

::

  > ghc -c Example.hs
  Example.hs:2:1: error:
    Could not load module 'Data.OldList';
    it is a hidden module in the package 'base-4.15.1.0'.
    If you wish to be able to access an internal module anyway, you may
    specify '-expose-package-internals base-4.15.1.0'; however, be warned
    that the API from internal modules may change between minor version releases.

  > ghc -c Example.hs -expose-package-internals base
  > ls
  Example.hs Example.o Example.hi

Effect and Interactions
-----------------------

1. All GHC users can now access internal modules from other packages, improving
   the situations outlined in the Motivation_.

#. The exports from ``ghc-prim`` are now protected; users can discover that
   using names from ``ghc-prim`` is potentially troublesome.

#. There may be a desire to integrate support for ``-expose-package-internals`` in ``.cabal``
   files, but that is beyond the scope of this proposal.

Costs and Drawbacks
-------------------

1. With this proposal, package authors now have no way to prevent access of
   internal modules. However, the primary reason to have internal modules
   is to present a stable, well-designed API to clients. Because internal
   modules remain distinct from this public API, the public API is preserved.

   Note that this does not pose a (new) security hazard, given that Template Haskell
   can be used to access definitions from internal modules.

Alternatives
------------

1. We could imagine a design where the command-line flag is per-module instead
   of per-package, but that seems unnecessarily fine-grained.

Unresolved Questions
--------------------

None at this time.
