Extended Dependency Generation
==============================

.. proposal-number::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/245>`_.
.. sectnum::
.. contents::

Currently GHC does not provide a feature to generate comprehensive build dependencies. This is vital information for external build systems like Cabal. This proposal is about adding a new option, ``-fcompilation-deps <file>``, and a new build mode, ``-precompilation-deps <file>``. This aims to provide external build systems with enough build dependency information to implement correct incremental builds with recompilation avoidance (using GHC's single-shot mode instead of make mode).

Motivation
------------

Currently many build systems use ``ghc``'s ``--make`` to manage builds.  However, there are reasons to believe that GHC's single-shot mode is to be preferred over ``--make``:

* GHC's parallel compilation support (``-j``) scales relatively poorly to high core counts without runtime system tuning

* Build managers may be in a position to do fine-grained recompilation checking beyond what GHC does.

Consequently, while GHC's single-shot does require more work in the form of repeated interface file loads (something which could be optimised in the future), single-shot mode may result in improved overall compilation performance in real-world settings.

However, it is currently challenging for external build managers to provide effective incremental compilation with recompilation avoidance when using single-shot mode. This is because GHC offers no *comprehensive* way for external tools to gain knowledge of dependencies needed by GHC during compilation.

Currently GHC supports `basic dependency generation <https://downloads.haskell.org/~ghc/8.6.5/docs/html/users_guide/separate_compilation.html#dependency-generation>`_ with the ``-M`` option. This outputs:

* The dependence of the object file on the source file.

* For each import X in M, where X is in the home package, a line recording the dependence of M on X.

* For each import X in M, where X is not in the home package, a line recording the dependence of M on X (this does not recurs and is only enabled with `-include-pkg-deps`).

* CPP dependencies discovered after the cpp stage of compilation (enabled with `-include-cpp-deps`)

But ``-M``'s output lacks important dependency information:

* Paths searched for module imports.

* Header files needed by ``foreign import`` s

* Plugins, enabled with the ``-fplugin`` option or with Template Haskell's ``addCorePlugin`` or otherwise.

* Dependencies added by plugins or in Template Haskell splices by ``addDependentFile`` (see `Dependency Tracking <https://gitlab.haskell.org/ghc/ghc/wikis/dependency-tracking>`_)

Much of this is discovered dynamically during compilation. Without this information, external build systems cannot implement correct incremental builds with recompilation avoidance. This proposal aims to solve that limitation by providing *comprehensive* dependency information.


Proposed Change Specification
-----------------------------

Add two new command line options:

* ``-fcompilation-deps <file>`` reports comprehensive dependency information (writing to <file>) discovered dynamically during compilation.

  * This can be used in combination with e.g. the ``-E``, ``-C``, ``-S``, and ``-c`` options to stop compilation and dependency generation early.

  * When only part of the compilation pipeline is run, only dependencies of that part of the pipeline are reported.

  * If compilation fails, dependencies discovered so far are still reported.

  * If compilation failed due to a missing dependency, that dependency will be explicitly reported.

* ``-precompilation-deps <file>`` implies ``-fcompilation-deps <file>`` but stops compilation early after discovering "precompilation dependencies" (see below).

The following dependency information will be collected:

* "Precompilation dependencies"

  * Direct module ``import`` s.

    * There are two cases of imports:

      * Source files (e.g. ``.hs``, ``.lhs``, or ``.hs-boot`` files). In this case the result will include the path where the source file was found.

      * Compiled modules. In this case the result will include the package ID where the module was found and the module name.

    * In both cases, a list of file paths where GHC looked for the import before finding it.

  * CPP ``#include`` paths.

* "Dynamic dependencies"
  * Header files needed by ``foreign import`` s

  * Plugins, enabled with the ``-fplugin`` option or with Template Haskell's ``addCorePlugin`` or otherwise.

  * Dependencies added by plugins or in Template Haskell splices by ``addDependentFile`` (see `Dependency Tracking <https://gitlab.haskell.org/ghc/ghc/wikis/dependency-tracking>`_)


Output format
^^^^^^^^^^^^^

The output will be in JSON. Paths may be absolute or relative to the current working directory (``-M`` also uses paths relative the the current working directory).

.. code-block:: js

    {
        // A list of modules and their dependencies.
        "modules": [
            {
                // Source file of this module.
                "source": "./src/MyModule.hs",
    
                // All dependencies discovered while compiling this module.
                // Entries can be of various types.
                "dependencies": [
    
                    // A source module import.
                    {
                        "type": "sourceImport",
    
                        // The module name.
                        "module": "A"
    
                        // The paths searched before finding the import or after
                        // exhausting all search paths.
                        "query": [
                            "some/relative/path/src/A.hs",
                            "/some/absolute/path/src/A.hs"
                        ]
    
                        // The path of the source import, or false if not found.
                        "path": "/some/absolute/path/src/A.hs"
                    },
    
                    // A compiled module import or plugin.
                    {
                        // Type is either "moduleImport" or "plugin"
                        "type": "moduleImport",
    
                        // The module name given in the import statement.
                        "moduleName": "A"
    
                        // The package if explicitly given (requires the
                        // PackageImports language extension), otherwise false.
                        "package": false
    
                        // The paths searched before finding the plugin module or
                        // after exhausting all search paths. This can contain
                        // file paths, or ghc-pkg database paths.
                        "query": [
                            { "type": "path",  "path": "some/path/MyPlugin.hs"},
                            { "type": "pkgdb", "pkgdb": "build/package.conf.d" }
                        ],
    
                        // The module
                        "module": {
                            // Package ID. Can be "this" for the current package.
                            // Must equal false if missing=true.
                            "packageId": "my-plugin-pkg-1.0.0",
    
                            // Package database path.
                            // Omitted if package ID is "this".
                            "packageDb": "build/package.conf.d"
    
                            // Plugin module name. Due to package thinning and
                            // renaming, this may not be the same module name as
                            // given in the import statement.
                            // See https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#package-thinning-and-renaming.
                            "moduleName": "A"
                        }
                    },
    
                    // "file": the build depends on the existence and contents of
                    // this file. CPP include paths, foreign import header files,
                    // and plugin/Template Haskell ``addDependentFile`` files.
                    {
                        "type": "file",
                        "missing": false,
                        "path": "/path/to/file"
                    },
    
                    ...
                ]
            }
        ]
    }

Effect and Interactions
-----------------------

Incremental builds
^^^^^^^^^^^^^^^^^^

This proposal allows external build managers to implement correct incremental builds with recompilation avoidance using GHC's single-shot mode (``-c``). Consider the concrete case of ``cabal-install``: the tool would start a fresh build by first invoking ``ghc -precompilation-deps`` on all modules in the package to be built (in a single GHC invocation). This allows an initial build plan to be created. Upon compiling each module in single-shot mode, the ``-fcompilation-deps`` will be used to extract complete dependency information to be used in subsequent incremental builds. When a file change is detected by the build system, all modules that (transitively) depend on that file must be rebuilt. The build system should also use ``-fcompilation-deps`` during rebuilding to update the dependency graph.

This still allows for false positives. A false positive is when recompilation is incorrectly deemed necessary. This can happen e.g. when a dependent file changes, the external build system tries to recompile, but GHC does its own checks and avoids recompilation any way. False positives result in unnecessary invocations of ``ghc``, but do not affect the correctness of the build.

With this proposal, unlike with just ``-M``, false negatives can always be avoided. A false negative is when recompilation is incorrectly deemed not necessary. That can result in an incorrect (stale) incremental build.

Missing Dependencies
^^^^^^^^^^^^^^^^^^^^

In the case that compilation fails due to a missing dependency, reporting dependencies so far and explicitly the missing dependency allows the external build system to generate the missing dependency if possible.

Early Termination
^^^^^^^^^^^^^^^^^

GHC allows the build to stop at an earlier phase with the ``-E``, ``-C``, and ``-S`` options. If used, only the dependencies found up to the stopping point will be output.

GHC's Recompilation Avoidance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GHC already does recompilation avoidance checks. If this feature is not disabled with ``-fforce-recomp`` and GHC deems recompilation not necessary then GHC's execution will short circuit. This poses the question if some dynamically discovered dependencies will be omitted due to short circuiting before their discovery. GHC should be able to short circuit and still report such dependencies. This is possible because GHC will recover dependency information from interface files as part of the recompilation check.


Costs and Drawbacks
-------------------

Given that most of the dependency information is already available, it's just a matter of collecting it. Maintenance requires declaring new sources of dependencies when they arise. This could happen when adding a new language feature that reads arbitrary files. Testing for this in advance does not seem plausible, but I expect we do not often add new sources of dependencies.


Alternatives
------------

One option is to expand on ``-M``, but users expect this option to be fast, while some of the dependency information required by this proposal can only be discovered later in the compilation pipeline. In particular dependencies added by plugins or in Template Haskell splices by ``addDependentFile``. Additionally, the output of ``-M`` is a makefile, which cannot include information such as which about search paths or package databases where queried.


Unresolved Questions
--------------------


Implementation Plan
-------------------
If accepted, David Eichmann (`Well-Typed <http://www.well-typed.com/>`_) will implement this change.