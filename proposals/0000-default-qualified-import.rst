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
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

This proposal adds a new extension to GHC ``-XQualifiedImports`` which switches the default behavior of ``import`` from importing modules unqualified to importing module qualified. This proposal also introduces a notion of *main type* of a module import: the main type of a module import is automatically imported unqualified.

That is::

  import qualified Data.HashMap as HashMap
  import           Data.Ord
  import Data.HashMap (HashMap)


  foo :: HashMap.HashMap -> HashMap.HashMap
  foo = ...

becomes::

  {-# LANGUAGE QualifiedImports #-}

  import Data.HashMap as HashMap
  import Data.Ord unqualified

  foo :: HashMap -> HashMap
  foo = ...


Motivation
------------

Qualified as default
~~~~~~~~~~~~~~~~~~~~

Manipulation of import sections is, in practice, harder than it needs to be. And programmers spend more time fiddling with their import lists, than they perhaps should. It is distracting to interrupt the writing of a program or a refactoring to go back to the import list that often.

One of the main reasons is that so many modules are imported unqualified. Unqualified import mean name conflicts, which in turn mean strategies to cope with conflicts. One can either

* Use import lists, but this means returning to the import declaration section every time a new function is needed in a file. It significantly increases the amount of bureaucracy that the programmer must perform.
* Use hiding lists, which entails much more work, but means that adding a function anywhere in a project may create a conflict and entail a global pass over the project file to hide that new function in many places. Even a package upgrade can cause conflicts, despite the fact that PVP considers adding functions to a package to be a non-breaking change.

Qualified imports protect against both failure modes, hence entail significantly less bureaucratic work.

The reason, in turn, while Haskell programmers turn to unqualified imports so much is that the language communicates that it is the normal thing to do. Part of the issue is the design of the base library which is clearly designed for unqualified imports and is the first interface that programmers have with Haskell. But a deeper-rooted issue is that unqualified import is *the default*. That is, ``import`` means unqualified import, where qualified import need an extra qualifier. It is a clear signal: unqualified import is normal, qualified import is the advanced one. In fact, it is such a strong signal that it is, in the authors' experience, easy to fall for the trap of importing, unqualified, modules designed for qualified import, such as modules from the ``vector`` or ``containers`` libraries (a decision which inevitably will need to be reversed later).

This proposal introduces a way to make qualified import the default, instead. As a first step to correct this unfortunate situation.

Main type
~~~~~~~~~

A practical limitation on qualified imports is that you can end up with referring to ``ByteString.ByteString``, which needlessly occupies screen space. As a consequence, the following idiom has been adopted by many::

  import Data.ByteString (ByteString)
  import qualified Data.ByteString as ByteString

Now one can both refer to ``ByteString`` (the type), while still requiring the name of definitions from the ``ByteString`` module to be qualified (*e.g.* ``ByteString.empty``).

This proposal codifies this idiom by implicitly importing the main type of a module unqualified (see `Proposed Change Specification`_ for the definition of the main type). For example

  import Data.ByteString as ByteString -- This is a qualified import due to this proposal change

will also import ``Data.ByteString.Bytestring`` unqualified as ``ByteString``.


Proposed Change Specification
-----------------------------

A new language extension, ``-XQualifiedImports`` is introduced.

When ``-XQualifiedImports`` is enabled

* The qualified-import syntax entry

  ::

    import qualified modid [as modid] [impspec]

  becomes a syntax error. In other words, one mustn't use the ``qualified`` keyword.
* In


  ::

    import modid1 [as modid2] [impsec]

  The *main type* is defined as the type, if it exists, exported by ``modid1`` whose name coincide with ``modid2``.

  For instance, in ``import Data.Container.Map as Map``, ``Map`` is the main type. But, in ``import Data.Container.Map as M`` there is no main type.

  If ``as modid2`` is omitted, then there is no main type.
* The meaning of

  ::

    import modid [as modid] [impspec]

  is changed to importing the importing the module ``modid`` qualified. In addition the main type, if it exists and is imported, is also imported unqualified. For instance, if ``impsec`` is specified and doesn't mention the main type, then the main type is not imported.

  Note that only the main type is imported unqualified, not his constructors or fields.
* A new syntax entry is added

  ::

    import modid1 unqualified [as modid2] [impspec]

  It imports ``modid`` unqualified (restricted, as usual, to the ``impsec`` if it is specified)


Effect and Interactions
-----------------------

By making qualified import a convenient default, this proposal lets software designers make their code base explicitly designed for qualified import first. Helping nudge new developments towards qualified import by default, and push for a simplification of module import bureaucracy. This is not a silver bullet as this won't be a true default and will require a conscious decision. But it's a decision which needs to be made far less often than imports (at most once per file, and it could be activated globally for a project, typically in the cabal file). It also paves the way towards qualified imports being the actual default in a future version of the Haskell Report.

The changes are contained to the import declarations, and there is no known interactions with other parts of the system.

Costs and Drawbacks
-------------------

The parser changes only affect module imports. Which are a fairly simple and self-contained part of the parser. Therefore the parser changes are expected to be easy and non-intrusive.

The implementation cost of retrieving the main type of a module import is not yet known, but should not have a significant effect on code complexity.

Alternatives
------------

No implicit unqualified import
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We may choose not to implement the implicit unqualified import of the main type. If we did so, we would expect to see a lot of

::

  import Data.Map unqualified (Map)
  import Data.Map as Map

Since it is already a common idiom. This is not a lot of boilerplate to cope with, and this would take away the only non-trivial feature to implement from the proposal.

On the other hand, it does feel awkward to repeat this idiom all over. Therefore, the implicit unqualified import of main types is likely to be a big driver for adoption of the qualified-by-default style.

Alternative definition of the main type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The main type could be defined differently.

- The main type of a module import is the type, if it exists, whose name is the same as the last component of the module's name. For example, ``import Data.Container as Storage`` would import the ``Data.Container.Container`` type unqualified.
- A heavier-weight approach would be to let module specify their main type with a syntax such as

  ::

    module Data.HashMap.Strict (
      main HashMap,
      ...
    )

  A benefit of this proposal is that it may also be extended to let the user specify a list of symbols which will implicitly imported unqualified. For example::

    module Foo.Bar.Baz
      ( symbol
      , TypeA(..)
      , TypeB(..)
      , unqualified TypeB
      , unqualified symbolB
      , unqualified (+)
      )

  In this context, ``import Foo.Bar.Baz as Module`` will always import ``TypeB``, ``symbolB``, and ``(+)`` unqualified. This would be most beneficial for symbols, which typically need to be imported unqualified.

  It is, however, a more complex proposal. It also replicates some of the issues of unqualified-as-default as exposed in the `Motivation
`_ section, albeit less severely (namely: conflicts can easily appear by simply adding functions to an internal module, or upgrading a library). Therefore, such a change would require care, and may be revisited in a dedicated proposal.

The reason why we chose to bind the main type to the name with which the import is qualified are

- It works with existing libraries.
- ``Foo.Foo`` looks very repetitive, ``FooBar.Foo`` feels much less awkward. So really, the former is the one to be avoided.
- It makes visual sense that ``Foo.frobnicate`` is the ``frobnicate`` function which applies to type ``Foo``.
- It is not hard to find examples of modules where the indented main type does not share a name with the module. For instance, in the ``dependent-map`` package, the intended main type of the module ``Data.Dependent.Map`` is ``DMap``. Using the main type convention of this proposal, one would expect the programmers to write

  ::

    import Data.Dependent.Map as DMap

   hence import ``DMap`` unqualified, and the ``member`` function as ``DMap.member``.

Syntax of unqualified imports
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here are alternative syntax proposals for explicit unqualified imports

- More symmetric with the Haskell 98 syntax, ``unqualified`` could be specified in before the module name: ``import unqualified ModuleName``. However, considering the positive responses to https://github.com/ghc-proposals/ghc-proposals/pull/190 , it does not seem like a good option.
- Yet another option is to consider, conceptually, and represent visually that unqualified imports are just qualified imports in a zero-length namespace. Example syntax could be:
    - ``import ModuleName as unqualified``
    - ``import ModuleName as *``
    - ``import ModuleName as .``

    Each time, the right-hand side of the ``as`` is a keyword, which signifies unqualified import.

    It would, however, prevent writing ``import ModuleName unqualified as ImportName`` (which corresponds to the current ``import ModuleName as ImportName``).

Option to deactivate implicit unqualified import of the main type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It may be desirable, in some cases, to disable the implicit unqualified import of the main type. A possible way to achieve that is, instead of making the ``import qualified`` syntax a syntax error, we could use it to mean “qualified without any implicit unqualified imports”. For consistency with the ``unqualified`` syntax, the ``qualified`` keyword would presumably come after the module name, so that

::

   import ByteString qualified as ByteString

would put import ``ByteString.ByteString``, but not the unqualified ``ByteString`` type.

This syntax would imply ``-XQualifiedImportsPostpositive`` (see https://github.com/ghc-proposals/ghc-proposals/pull/190 ).

However, there is no clear motivation for that option. Especially considering that a simple way to avoid the implicit import of the main type is to change the import name: ``import Data.ByteString as LibByteString`` won't import ``ByteString`` unqualified.

Unresolved Questions
--------------------

None.

Implementation Plan
-------------------

@guibou will implement this proposal with mentoring from Tweag I/O's GHC contributors.
