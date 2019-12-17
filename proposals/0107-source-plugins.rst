Source plugins
==============

.. author:: Ryan Scott
.. date-accepted:: 2018-04-13
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/14709
.. implemented:: 8.6
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/107>`_.
.. contents::

This document proposes the extension of the already existing `Plugin support <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#compiler-plugins>` in Haskell with plugins that are able to access and modify the representation of the Haskell syntax tree and its environment. This would allow tool developers to base their tools on GHC plugins.


Motivation
------------

I will not argue on how good tool support can help Haskell developers, I think all readers are familiar with this.

When developing tools for the Haskell language that analyze or manipulate the source code, it is necessary to access some representation of the syntax tree. Depending on the actual purpose of the tool, it may require additional information (different representation, the environment of the compilation) to perform its tasks.

Take for example, a development tool that analyses the source code to produces a call graph. The developer wants to build it using GHC as a backend, to guarantee that it keeps up-to date with the latest changes in the compiler. Lets say that it writes out the graph to a file. It's interface should rely on the typechecked representation:

::

 analyze :: TypecheckedSource -> IO ()
 analyze tc = -- analysis of the typechecked source code to write out the nodes and links of the call graph

However there is the problem of the method of accessing that ``TypecheckedSource``. It should be done in a way that is usable for large complex projects. It would be beneficial not to make assumptions about the build system where the tool will be used. The only restriction that the tool's author should make is that it should be compiled using GHC. If we don't want to change the build system of the project, nor make the tool understand and reproduce the build process to some extent, the tool should be integrated into the normal build process. And the most convenient way to do so is to use compiler flags as a way to integrate the tool into the build process. Plugin use is controlled by compiler flags so it is convenient to extend the already existing Plugin support for tooling.


Proposed Change Specification
-----------------------------

The Plugin API is extended with the following fields:

::

 parsedResultAction :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
 typeCheckResultAction :: [CommandLineOption] -> ModSummary -> TcGblEnv -> Hsc TcGblEnv
 spliceRunAction :: [CommandLineOption] -> LHsExpr GhcTc -> TcM (LHsExpr GhcTc)
 interfaceLoadAction :: forall lcl . [CommandLineOption] -> ModIface -> IfM lcl ModIface
 renamedResultAction :: Maybe([CommandLineOption] -> ModSummary -> RenamedSource -> Hsc ())

- ``parsedResultAction`` is called during the compilation when the parser runs successfully. Its third argument is the parsed syntax tree. The result of the function application will be passed to later compilation stages.
- ``renamedResultAction`` is a read-only optional pass that receives the renamed results if the type checker runs successfully. It is optional, because when not needed, the renamed results are thrown away (for performance). It is read-only, because changing the renamed results have no effect on the compilation since renaming and type checking is done in one pass.
- ``typeCheckResultAction`` is called during the compilation when the type checker runs successfully. Its third argument is the type checked syntax tree. The result of the function application will be passed to later compilation stages.
- ``spliceRunAction`` is called on Template Haskell splices and Quasi-Quotes that are about to be evaluated to generated code. This is useful for tools that analyze the source code, since these language elements are not present in the renamed and type checked syntax tree. The result of this function is used to generate code. It should be called for all evaluated metaprogramming elements of the Haskell module.
- ``interfaceLoadAction`` is called every time the compiler loads an interface file. This functionality is useful for source manipulation tools, since they might analyze the environment of the code being compiled. Usually this means that they might know what definitions and instances are in scope in a given module.

The options for the different plugin actions are passed as strings, similarly to existing plugin arguments. Flags are parsed by the plugin itself. Malformed flags could be ignored or could trigger a compilation error. Plugin flags could be given in the form of ``-fplugin-opt=module:args`` as described in the manual.

Each action is performed in the monad that is used in the specific step where the action should be performed. This allows greater freed om for the writer of these plugin actions. This is the reason why some actions return their result in the ``Hsc``, ``TcM`` or ``IfM`` monad.

Effect and Interactions
-----------------------

By using the extended plugins API, tool developers can create tools that can integrate into the already existing build toolchain of a project. By modifying the compiler flags, the user can set up the tool to work.

Using the ``typeCheckResultAction`` it is now easy to implement the example used above as a plugin.

::

 import Plugins

 plugin = defaultPlugin { typeCheckResultAction = \_ _ tc -> analyze (tcg_binds tc) >> return tc }

The user can use the plugin for any project by altering the compilation flags to use the plugin. An example use case would be:

::

 # write GHC_OPTIONS = -fplugin A.Plugin in the appropriate config file
 make

If the build environment contains widely used build tools like cabal or stack, it is trivial for the programmer to setup the GHC flags for the tools.

Since using plugins does not alter the compilation process, the use of plugins does not interfere with other parts of the compiler API.

Costs and Drawbacks
-------------------

The proposal does not change the language itself and should only affect users who choose to use tools that are developed using compiler plugins. No existing functionality is changed.

Development and maintenance is cheap. The proposal only requires a few changes in the compiler. In fact I have an implementation for the basic version of this: `https://phabricator.haskell.org/D4342`.

Currently using plugins forces GHC to recompile every module when plugins are used. While this is not solved it limits the usability of the source plugins as well. For more information see the `ticket <https://gitlab.haskell.org/ghc/ghc/issues/7414>` about that issue.

Giving plugins the possibility to change inner representation of the compiler carries a certain risk of changing the behavior of the compiler in an unexpected way. However since the use of the plugins are requested by the user, it should be evident if a plugin is responsible for the incorrect behavior. This could be mitigated by performing validation after the plugin is executed.

Alternatives
------------

- *Write tools that use third-party libraries for parsing and analyzing Haskell.*

  The drawback of these solutions is that the third-party libraries might not keep up-to-date with GHC. GHC became a de-facto standard of Haskell, so it is important for the tools to keep up with GHC's development

- *Implement tools using the public GHC API.*

  The `GHC API <https://wiki.haskell.org/GHC/As_a_library>` does already provide interface for compiling Haskell modules and accessing their inner representation. Using the API is comfortable for a single Haskell module or a set of modules, but not feasible for large projects with complex build procedure. The reason is that in order to call the API, the tool's developer have to manually analyze the project and decide which Haskell modules belong to the project and how can they be compiled. Although this can be implemented for simple projects using certain libraries as a help, but for a larger project this is not feasible.

- *Use frontend plugins and GHC hooks for accessing this information.*

  `Frontend plugins <https://downloads.haskell.org/~ghc/master/users-guide/extending_ghc.html#frontend-plugins>` add a new programmable major mode to GHC. When the control is passed to the plugin, the plugin's writer receives all the compiler arguments and is able to do whatever is necessary. `GHC Hooks <https://gitlab.haskell.org/ghc/ghc/wikis/ghc/hooks>` are developed for altering how the compiler performs different compilation steps. GHC hooks are primarily meant to help writing different backends for GHC and they are not exposed to the user directly.

  It is important to see that frontend plugins are the most convenient if the developer want to do something else than running the compilation pipeline normally. Frontend plugins are not convenient for running the compiler normally and accessing the inner representations. I have to note that some of the issues can be solved by `creating a wrapper for GHC <http://blog.ezyang.com/2017/02/how-to-integrate-ghc-api-programs-with-cabal/>`.

  It would be possible to define a frontend plugin that install a ``HscFrontendHook`` to access the type checked representation. However this method is insufficient to grant access to parsed and renamed syntax tree as well as splices and interfaces is.


Unresolved questions
--------------------

 - Enable changing the inner representation of the compiler?

   This would remove safety risk from changing the representation, but would also eliminate the possibility of designing tools that extend the language with some clever manipulation of the inner representation.

   We could also put in extra checks in case a plugin modifies some of the representation, keeping the benefits of being able to change the representation and keep the soundness of the compiling process.

 - Implement source plugins separately

   This may be requested out of design considerations. But since type checking plugins are added to the ``Plugin`` API, we cannot say that plugins are reserved for core-to-core transformations.

 - Have another plugin action for compilation errors/warnings?

   This might help writing tools that can automatically correct programmer mistakes. The ability to collect compiler errors could be useful in education as well.

 - Is there any additional parts of the inner representation that should be accessed via plugins?

   I would invite other tool developers to share their ideas if they think some other information should be accessed via the extended plugins.

Implementation Plan
-------------------

The original version of the proposal is already implemented and can be reviewed `here <https://phabricator.haskell.org/D4342>`

Notes
-----

The proposal is based on `Edsko's version <https://gitlab.haskell.org/ghc/ghc/wikis/frontend-plugins-proposal>`

A shorter version of the proposal is available on its `wiki page <https://gitlab.haskell.org/ghc/ghc/wikis/extended-plugins-proposal>`.
