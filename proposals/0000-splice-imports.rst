.. author:: Matthew Pickering
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/412>`_.
.. contents::
.. sectnum::

Explicit Splice Imports Extension
==============

This proposes a new extension, ``ExplicitSpliceImports``, which modifies the
import syntax so that imports which are only used at compile-time are marked explicitly.


Motivation
----------

The primary goal of this proposal is to distinguish, in the module header, three different ways that
imported module imports are used:

1. Imported modules whose code is executed only at compile time;
2. Imported modules whose code is executed only at runtime;
3. Imported modules whose code is executed both at compile time and runtime.

Distinguishing these 3 different cases has several advantages:

1. Currently, if a module enables ``TemplateHaskell``, then all imported modules
   are compiled to object code before name resolution takes place. The ensures that any top level splices that may be encountered are able to be fully evaluated.
   This is a pessimisation because most of the imported identifiers, which we have taken such pains to ensure we can run, will not
   actually be used in a top-level splice.
   Proposals (such as `#14905 <https://gitlab.haskell.org/ghc/ghc/-/issues/14095>`_) to increase build parallelism are far less effective
   in projects which use ``TemplateHaskell`` because name resolution depends on code generation
   for all dependencies.
   By distinguishing imported modules whose code is executed only at runtime
   (which in common cases will be a small fraction of imported modules), we are
   able to improve this pessimisation.
2. GHC offers an ``-fno-code`` flag that instructs the compiler to parse and
   typecheck Haskell modules, but not to generate code. The intent is to offer
   quick feedback to the user. Any module imports of a module using
   ``TemplateHaskell`` must be compiled to object code, Because of .1 above.
   This despite the fact that we will not generate object code for the module
   itself. By distinguishing imported modules whose code is executed only at
   runtime, we can reduce this unfortunate work significantly, and entirely in many
   cases.
3. Projects such as haskell-language-server face similar problems as 2., where they are interested only in the result of type-checking modules, but when ``TemplateHaskell`` is enabled a large
   number of modules have to be cautiously compiled to bytecode.
4. By using splice imports we can separate the dependencies into those only needed at build-time (1) and
   those only needed at runtime (2). We can then link only against those packages needed at runtime.
5. Currently, when cross-compiling, in modules that use ``TemplateHaskell``, all
   imported modules must be compiled for both host and target.
   By distinguishing imported modules used at compile time(i.e. not used at
   runtime), we can require only those modules to be compiled for the host.
   Similarly, by distinguising imported modules used at runtime(i.e. not used at
   compile time), we can require only those modules to be compiled for the
   target. It can be very hard or impossible to make some packages available on
   some cross-compile target platforms, so this change would significantly
   improve the applicability of ``TemplateHaskell`` in these scenarios.


Definitions
-----------

level
  Each expression exists at a level. The level is increased by 1 when
  inside a quote and decreased by 1 inside a splice. Therefore the level of
  an expression can be calculated as the number of quotes surrounding an expression
  subtract the number of splices.

  example:  ``levels``::

    -- foo is at level 0
    foo = $(let
      -- bar is at level 1
      bar = $(let
        -- baz is at level 2
        baz = [|
        -- qux is at level 1
          qux = [|
            -- quux is at level 0
            quux = [|
              quuz is at level -1
              quuz = 0
            |]
          |]
        |] in baz
      ) in bar
    )


top-level splice
  A splice, where the body is at a negative level or an unadorned
  declaration splice.


home module
  A module from the package that is currently being compiled.


Proposed Change
---------------

The ``splice`` modifier indicates that a module's imports are available at compile time.
The absence of the ``splice`` modifier indicates that a module's imports are available
at runtime.

A ``splice`` imported module allows identifiers to be used at compile time. Compile time
evaluation is indicated by a top-level splice. Therefore the ``splice`` modifier
affects name resolution depending on whether we are inside a top-level splice or not.

1. If a module is only available at compile time then the imports are only available in top-level splices.
2. If a module is only available at runtime then the imports are not available in top-level splices.
3. If a module is available at both runtime and compile time then the imports are available everywhere.

The new language extension ``ExplicitSpliceImports`` adds a
new import modifier to the import syntax. An import is marked as a "splice"
import when it is prefixed with ``splice``::

  {-# LANGUAGE ExplicitSpliceImports #-}
  {-# LANGUAGE TemplateHaskell #-}
  module Main where

  -- (1)
  import splice B

  -- (2)
  import A


The splice modifier indicates to the compiler that module B is only used at compile time
and hence the imports can **only** be used inside top-level splices (1). When the extension is enabled,
imports without the splice modifier are only available at runtime and therefore not available to be used in top-level splices (2).
In this example, identifiers from ``B`` can **only** be used in top-level splices
and identifiers from ``A`` can be used everywhere, apart from in top-level splices.

To make some of the initial motivation explicit:

1. Now when compiling module ``Main``, despite the fact ``TemplateHaskell`` is enabled,
   we know that only identifiers from module ``B`` will be used in top-level splices so
   only ``B`` (and its dependencies) needs to compiled to object code before starting to compile ``Main``.
2. When cross-compiling, only ``A`` needs to be built for the target and ``B``
   only for the host as it is only used at build-time.

If you require scenario (3) then two imports declarations can be used::

  -- (3)
  import C
  import splice C

The syntax for imports is changed in the follow way::

  importdecl :: { LImportDecl GhcPs }
     : 'import' maybe_src maybe_safe optsplice optqualified maybe_pkg modid optqualified maybeas maybeimpspec


The ``splice`` keyword appears before the ``qualified`` keyword but after ``SOURCE``
and ``SAFE`` pragmas.


Intuitive Specification of ``splice``
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Identifiers arising from splice imports can only be used at negative levels, ie, unquoted in a top-level splice::

  -- Accepted, because B is a splice import and B.qux is used at level -1
  foo = $(B.qux)

  -- Rejected, because B is a splice import and B.qux is used at level 0
  foo' =  B.qux


But identifiers from normal imports are rejected::

  -- Rejected, as A is not a splice import and used at level -1
  baz = $(A.zee)

An identifier can appear inside a top-level splice, if it is at a non-negative
level. For example, the following is legal::

  foo = $(B.qid [| A.zee |] )

Because ``A.zee`` is used at level 0 it doesn't need to be imported using a splice import.

Level Specification of ``splice``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Ordinary imports introduce variables at all non-negative levels (>= 0)
* Splice imports introduce variables at all negative levels. (< 0)

Ambiguity of instances and variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Resolution of scopes (often called "renaming") is blind to whether or not an
identifier was imported with ``splice``.

In the case of variables, variables which are splice imported can only be used
inside a top-level quotation but are reported as ambiguous if they clash with any
other variable in scope, for example::

  import A ( x )
  import splice B ( x )

  foo = $( x ) x

In this case, there is no ambiguity because ``A.x`` isn't allowed to be used in
the top-level splice, but we still produce an ambiguity error to prevent any confusing
situations about what is in scope. This position is conservative and allows more
flexibility in the future if it's deemed that the restriction should be relaxed.

For instances, a similar situation applies, splice and non-splice imports must
have a consistent view of imported instances::

  module X where
  data X = MkX

  module Normal where
  import X
  instance Show X where show _ = "normal"

  module Splice where
  import X
  instance Show X where show _ = "splice"

  module Bottom where
  import X (X(..))
  import splice X (X(..))
  import Normal ()
  import splice Splice ()
  import splice Language.Haskell.TH.Lib ( stringE )

  s1 = show MkX
  s2 = $( stringE (show MkX) )

This program is also rejected because the instances defined in ``Normal`` and ``Splice`` overlap.


Other Considerations
~~~~~~~~~~~~~~~~~~~~

When ``TemplateHaskell`` is enabled but NOT ``ExplicitSpliceImports``, then all imports
are implicitly additionally imported as splice imports, which matches the current behaviour.

If the ``Prelude`` module is implicitly imported then it is also imported as a splice module so the following is
allowed::

  zero = $(id [| 0 |])

If ``NoImplicitPrelude`` is enabled then you have to import ``Prelude`` as a splice
module as well in order to use names from ``Prelude`` in negative level splices::

  {-# LANGUAGE NoImplicitPrelude #-}

  import splice Prelude

  -- accepted
  foo = $(id [|"foo"|])

  -- rejected
  foo = id $([|"foo"|])

All exported names are at level 0. Splice imports can't be rexported, unless
they are also imported normally.
Allowing splice imports to be exported would turn a build-time only import into a runtime
export. Maintaining the distinction between things only needed at build-time and
things only needed at runtime allows project dependencies to be separated in the
same way. This is important for cross-compilation.



Drawbacks
---------

* The user has to be aware of the significance of using splice imports.



Alternatives
------------

* ``splice`` imports could also bring identifiers into scope so that they
  can be used everywhere in a module, not **only** in top-level splices as
  the proposal suggest. This approach is not taken because it means that
  build-time only dependencies can't be distinguished from runtime dependencies

* Using a pragma rather than a syntactic modifier would fit in better with
  how ``SOURCE`` imports work and make writing backwards compatible code easier::

    import {-# SPLICE #-} B

* It might be proposed that an alternative would be to work out which modules
  need to be compiled based on usage inside a module. This would compromise the
  principle that we can learn about what's needed for a module just by looking
  at the import list in the module header.

* The extension could only apply to **home** modules, because the benefits of
  splice imports are when using GHC's ``--make`` mode. As the proposal stands,
  for uniformity, any module used inside a top-level splice must be marked as
  a splice module, even if it's an external module.

* Another alternative would be to allow even finer grained control of splice
  imports so that the cases of usage at levels -1 or -2 could be distinguished.
  This could be useful in some cross-compilation situations. This is the approach
  suggested in the `Stage Hygience for Template Haskell proposal <https://github.com/ghc-proposals/ghc-proposals/pull/243>`_.

  The syntax in this proposal can be extended in a natural way to allow for this by adding an optional
  integer component which specifies precisely what level the imported names should be allowed at::

    -- Can be used at -1
    import splice 1 A
    -- Can be used at -2
    import splice 2 A

  Practically, by far the most common situation is 2 stages.


Unresolved Questions
--------------------
