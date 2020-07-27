.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/92>`_.

.. contents::

Library-defined language extensions
===================================

This proposal allows libraries to define a new “language extension” that bundles a number of extensions, together with possibly required other compiler flags and default imports.


Motivation
----------

There is a lack of infrastructure to manage language extensions:

* Programmers who want to get the most out of type-level programming in Haskell have to enable a large number of language extensions in every file. They would like to be able to define a “language extension” ``AdvancedTypes`` once (either in their own library, or provided by a package on Hackage) and use simply ``{-# LANGUAGE AdvancedTypes #-}`` throughout their code.
* Certain libraries require, to be used properly, a number of language extensions to be enabled in using code. Here a quote from the ``singletons`` library:

      The singletons library requires GHC 8.2.1 or greater. Any code that uses the singleton generation primitives needs to enable a long list of GHC extensions. This list includes, but is not necessarily limited to, the following::

          ScopedTypeVariables
          TemplateHaskell
          TypeFamilies
          GADTs
          KindSignatures
          TypeOperators
          FlexibleContexts
          RankNTypes
          UndecidableInstances
          FlexibleInstances
          InstanceSigs
          DefaultSignatures
          TypeInType

  It would be much more convenient if the user could just say ``{-# LANGUAGE Singletons #-}``

* Beyond that, some libraries “extend” the syntax of Haskell using Template Haskell splices and/or GHCPlugins If a library-defined language extensions has the ability to not only enable ``LanguageHaskell``, but plugin flags (``-fplugin=foo``) and a list of modules to be imported by default, similar to the Prelude, then these top-level TH splices would feel indeed like a language extension. So instead of::

      {-# LANGUAGE TemplateHaskell #-}
      {-# OPTIONS_GHC -fplugin=Some.Foo.Plugin #-}
      import Some.Foo.TH (foo)
      
      foo 'id "This is new a plugin"
      
  the user would write::

      {-# LANGUAGE SomeFoo #-}
      
      foo 'id "This is new a plugin"
 
* The combination of bundling language extensions and default imports allows libraries to define alternative Prelude in the shape of language extensions. Instead of::
 
      {-# LANGUAGE NoImplicitPrelude #-}
      import Some.Prelude
    
  the user would write::

      {-# LANGUAGE SomePrelude #-}
* The language extensions ``Haskell98`` and ``Haskell2010`` could be declared in this way, e.g. in the ``ghc-prim`` package.
* People have asked for it in the past: https://ghc.haskell.org/trac/ghc/ticket/9642

Proposed Change Specification
-----------------------------

GHC’s package data base learns a new structured field of the following shape::

  custom-extensions:
    * name: InspectionTesting
      extensions: TemplateHaskell
      flags: -fplugin=Test.Inspection.Plugin
      imports: Test.Inspection.Prelude
  
(I specified this in YAML syntax; the precise syntax still needs to be figured out).

The field ``name`` is required, the rest are optional. All fields are lists; multiple names define aliases (just like ``GeneralizedNewtypeDeriving`` and ``GeneralisedNewtypeDeriving`` is valid.)

When the user specifies ``{-# LANGUAGE InspectionTesting #-}``, and the currently visible packages specify exactly one ``custom-extension`` with that name, then this has the obvious effect of enabling the given language extensions, flags, and treating the given ``imports`` just like the implicit prelude.


Effect and Interactions
-----------------------
* If the custom language extension is used together with conflicting language declarations, e.g::

    {-# LANGUAGE InspectionTesting, NoTemplateHaskell #-}

  then they are toggled in order. In this example, ``NoTemplateHaskell`` is in effect.

* The ``NoImplicitPrelude`` language extenions only disables the ``Prelude`` import, but no implicit imports declared using a library-defined extension.

Costs and Drawbacks
-------------------

* The implementation requires support from ``Cabal``, including an extension of the ``.cabal`` file format, but ``Cabal`` would not have to do much with this.

* Parsing Haskell code reqiures the tools to be able to resolve these language extensions, and humans reading code may have to read about the language extension.

* The namespace is unmanaged, and if you need two libraries who export an extension under the same name, then you cannot use it. (But you can always specify the effect manually.) Do we need package-qualified languages then?


Alternatives
------------
* Doing nothing. Not bad, but as we get more and more language extensions, this gets tedious.
* Doing less, e.g. no custom imports or compiler flags.

Unresolved questions
--------------------
* What will the precise format of language extensions in the package data base be.
* Are there language extensions that should not be toggleable this way?
* Which other flags should be legal there. I expect we want a whitelist, and starting with only ``-fplugin`` and ``-fplugin-opt`` is a good start.
