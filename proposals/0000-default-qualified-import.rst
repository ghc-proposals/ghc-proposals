Qualified import by default
===========================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/220>`_.
.. sectnum::
.. contents::

This proposal adds a new extension to GHC ``-XQualifiedImports`` which switches the default behavior of ``import`` from importing modules unqualified to importing module qualified. Unqualified import are still possible using the newly introduced ``open`` modifier.

That is::

  import qualified Data.HashMap as HashMap
  import           Data.Ord

becomes::

  {-# LANGUAGE QualifiedImports #-}

  import Data.HashMap as HashMap
  import Data.Ord open

Motivation
------------

Manipulation of import sections is, in practice, harder than it needs to be. And programmers spend more time fiddling with their import lists, than they perhaps should. It is distracting to interrupt the writing of a program or a refactoring to go back to the import list that often.

One of the main reasons is that so many modules are imported unqualified. Unqualified import mean name conflicts, which in turn mean strategies to cope with conflicts. One can either

* Use import lists, but this means returning to the import declaration section every time a new function is needed in a file. It significantly increases the amount of bureaucracy that the programmer must perform.
* Use hiding lists, which entails much more work, but means that adding a function anywhere in a project may create a conflict and entail a global pass over the project file to hide that new function in many places. Even a package upgrade can cause conflicts, despite the fact that PVP considers adding functions to a package to be a non-breaking change.

Qualified imports protect against both failure modes, hence entail significantly less bureaucratic work.

The reason, in turn, while Haskell programmers turn to unqualified imports so much is that the language communicates that it is the normal thing to do. Part of the issue is the design of the base library which is clearly designed for unqualified imports and is the first interface that programmers have with Haskell. But a deeper-rooted issue is that unqualified import is *the default*. That is, ``import`` means unqualified import, where qualified import need an extra qualifier. It is a clear signal: unqualified import is normal, qualified import is the advanced one. In fact, it is such a strong signal that it is, in the authors' experience, easy to fall for the trap of importing, unqualified, modules designed for qualified import, such as modules from the ``vector`` or ``containers`` libraries (a decision which inevitably will need to be reversed later).

This proposal introduces a way to make qualified import the default, instead. As a first step to correct this unfortunate situation.

Proposed Change Specification
-----------------------------

A new language extension, ``-XQualifiedImports`` is introduced.

When ``-XQualifiedImports`` is enabled

* The qualified-import syntax entry

  ::

    import qualified modid [as modid] [impspec]

  becomes a syntax error. In other words, one mustn't use the ``qualified`` keyword.
* The meaning of

  ::

    import modid [as modid] [impspec]

  is changed to importing the module ``modid`` qualified.
* A new syntax entry is added

  ::

    import modid1 open [as modid2] [impspec]

  It imports ``modid`` unqualified (restricted, as usual, to the ``impsec`` if it is specified)


Effect and Interactions
-----------------------

By making qualified import a convenient default, this proposal lets software designers make their code base explicitly designed for qualified import first. Helping nudge new developments towards qualified import by default, and push for a simplification of module import bureaucracy. This is not a silver bullet as this won't be a true default and will require a conscious decision. But it's a decision which needs to be made far less often than imports (at most once per file, and it could be activated globally for a project, typically in the cabal file). It also paves the way towards qualified imports being the actual default in a future version of the Haskell Report.

The changes are contained to the import declarations, and there is no known interactions with other parts of the system.

Costs and Drawbacks
-------------------

The parser changes only affect module imports. Which are a fairly simple and self-contained part of the parser. Therefore the parser changes are expected to be easy and non-intrusive.

Alternatives
------------

Here are alternative syntax proposals for explicit unqualified imports

- ``unqualified`` can be used in place of ``open``. This will appears more symmetric with the Haskell 98 syntax which uses ``qualified`` for qualified modules import.
- More symmetric with the Haskell 98 syntax, ``open`` could be specified in before the module name: ``import open ModuleName``. However, considering the positive responses to https://github.com/ghc-proposals/ghc-proposals/pull/190 , it does not seem like a good option.
- The position of the `open` keywoard may depends on the activation of the ``-XQualifiedImportsPostpositive`` extension.
- Yet another option is to consider, conceptually, and represent visually that unqualified imports are just qualified imports in a zero-length namespace. Example syntax could be:
    - ``import ModuleName as unqualified``
    - ``import ModuleName as *``
    - ``import ModuleName as .``

    Each time, the right-hand side of the ``as`` is a keyword, which signifies unqualified import.

    It would, however, prevent writing ``import ModuleName open as ImportName`` (which corresponds to the current ``import ModuleName as ImportName``).

Unresolved Questions
--------------------

None.

Implementation Plan
-------------------

@guibou will implement this proposal with mentoring from Tweag I/O's GHC contributors.
