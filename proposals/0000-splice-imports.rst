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

The primary goal of this proposal is to distinguish three different ways that
modules are used:

1. Modules whose code is executed only at compile time
2. Modules whose code is executed only at runtime
3. Modules whose code is executed both at compile time and runtime.

Distinguishing these 3 different cases has several advantages

1. If a module enables ``TemplateHaskell`` then all imported modules are required
   to first be compiled to object code before name resolution can take place. This
   is a major pessimisation because most of the imported identifers will not
   actually be used in a top-level splice.
   Proposals (such as `#14905 <https://gitlab.haskell.org/ghc/ghc/-/issues/14095>`_) to increase build parallelism are far less effective
   in projects which use ``TemplateHaskell`` because name resolution depends on code generation
   for all dependencies.
2. Using ``-fno-code``, any module imported by a module using ``TemplateHaskell`` has to be compiled to object
   code, this causes a significant slow down.
3. Projects such as haskell-language-server face a similar problem where normally
   modules are just typechecked, but when ``TemplateHaskell`` is enabled, a large
   number of modules have to be cautiously compiled to bytecode.
4. By using
   splice imports you can separate the dependencies into those only needed at build-time (1) and
   those only needed at runtime (2). This allows you to omit linking against packages
   only used during build time.
5. When cross-compiling, all packages currently to be compiled for both host and target,
   in a similar way to the previous point, only build dependencies (1) need to be built
   for the host and runtime dependencies for the target (2). If a dependency
   is used at both compile and runtime (3) then it must also be compiled both
   for the host and target.


Definitions
-----------

level
  Each expression exists at a level. The level is increased by 1 when
  inside a quote and decreased by 1 inside a splice. Therefore the level of
  an expression can be calculated as the number of quotes surrounding an expression
  subtract the number of splices.

top-level splice
  A splice, where the body is at a negative level or a unadorned
  declaration splice.


home module
  A module from the package that is currently being compiled.


Proposed Change
---------------

The ``splice`` modifier indicates that a module's imports are available at compile time.
The absense of the ``splice`` modifier indicates that a module's imports are available
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
   we know that only identifers from module ``B`` will be used in top-level splices so
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


But identifers from normal imports are rejected::

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
inside a top-level quotation but reported as ambiguous if they clash with any
other variable in scope, for example::

  import A ( x )
  import splice B ( x )

  foo = $( x ) x

In this case, there is no ambiguity because ``A.x`` isn't allowed to be used in
the top-level splice, but we still produce an ambiguity error to prevent any confusing
situations about what is in scope. This position is conservative and allows more
flexibility in future if it's deemed the restriction should be relaxed.

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
module as well::

  {-# LANGUAGE NoImplicitPrelude #-}

  import splice Prelude


Splice imports can't be rexported, unless they are also imported normally. Allowing
splice imports to be exported would turn a build-time only import into a runtime
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
  build-time only dependencies can't be distinguished

* Using a pragma rather than a syntactic modifier would fit in better with
  how ``SOURCE`` imports work and make writing backwards compatible code easier::

    import {-# SPLICE #-} B

* It might be proposed that an alternative would be to work out which modules
  need to be compiled based on usage inside a module. This would compromise the
  principle that we can learn about what's needed for a module just by looking
  at the import list.

* The extension could only apply to **home** modules, because the benefits of
  splice imports are when using GHC's ``--make`` mode. As the proposal stands,
  for uniformity, any module used inside a top-level splice must be marked as
  a splice module, even if it's an external module.

* Another alternative would be to allow even finer grained control of splice
  imports so that the cases of usage at levels -1 or -2 could be distinguished.
  This could be useful in some cross-compilation situations. This is the approach
  suggested in the `Stage Hygience for Template Haskell proposal <https://github.com/ghc-proposals/ghc-proposals/pull/243>`_.

  The syntax can be extended in a natural way to allow for this by adding an optional
  integer component which specificies precisely what level the import should be allowed at::

    -- Can be used at -1
    import splice 1 A
    -- Can be used at -2
    import splice 2 A

  Practically, by far the most common situation is 2 stages.


Unresolved Questions
--------------------
