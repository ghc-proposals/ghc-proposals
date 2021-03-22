.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

Explicit Splice Imports Extension
==============

This proposes a new extension, ``ExplicitSpliceImports``, which modifies the
import syntax so that imports used inside top-level splices are marked explicitly.


Motivation
----------

If a module enables the ``TemplateHaskell`` then all imported modules are required
to first be compiled to object code before name resolution can take place. This
is a major pessimisation because most of the imported identifers will not
actually be used in a top-level splice.

1. Using ``-fno-code``, any module imported by a module using ``TemplateHaskell`` has to be compiled to object
   code, this causes a significant slow down.
2. IDEs such as haskell-language-server face a similar problem where normally
   modules are just typechecked, but when ``TemplateHaskell`` is enabled, a large
   number of modules have to be cautiously compiled to bytecode.
3. Proposals such as `#14905 <https://gitlab.haskell.org/ghc/ghc/-/issues/14095>`_ to increase build parallelism are far less effective
   in projects which use ``TemplateHaskell`` because renaming depends on code generation
   for all dependencies.
4. When cross-compiling, all packages need to be compiled for both host and target.

Definitions
-----------

level
  Each expression exists at a level. The level is increased by 1 when
  inside a quote and decreased by 1 inside a splice. Therefore the level of
  an expression can be calcated as the number of quotes surrounding an expression
  subtract the number of splices.

top-level splice
  A splice, where the body is at a negative level or a unadorned
  declaration splice.


home module
  A module from the package that is currently being compiled.




Proposed Change
---------------

We would like to distinguish between three ways that an imported identifier can
used.

1. Only available in splices
2. Not available in splices
3. Available everwhere

In order to do this
the new language extension ``ExplicitSpliceImports`` is introduced which adds a
new import modifier to the import syntax. An import is marked as a "splice"
import when it is prefixed with ``splice``::

  {-# LANGUAGE ExplicitSpliceImports #-}
  {-# LANGUAGE TemplateHaskell #-}
  module Main where

  -- (1)
  import A

  -- (2)
  import splice B


The splice modifier indicates to the compiler that identifiers imported from
the module can **only** be used inside splices (1). When the extension is enabled,
imports without the splice modifier are not available to be used in splices (2).
Therefore, in this example, identifiers from ``B`` can **only** be used in top-level splices
and identifiers from ``A`` can be used everywhere, apart from in top-level splices.

This distinction is important for two reasons:

1. Now when compiling module ``Main``, despite the fact ``TemplateHaskell`` is enabled,
   we know that only identifers from module ``B`` will be used in top-level splices so
   only ``B`` needs to compiled to object code before starting to compile ``Main``.
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


Specification of ``splice``
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

Ambiguity of instances and variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The same ambiguity rules that apply to identifiers, type classes and type family
instances are applied to splice imports just as they are for normal imports.

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


Splice imports can't be rexported, unless they are also imported normally.


Drawbacks
---------

* The user has to be aware of the significance of using splice imports.



Alternatives
------------

* Using a pragma rather than a syntactic modifier would fix in better with
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

