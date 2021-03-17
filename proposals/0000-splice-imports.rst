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
2. IDEs such as haskell-language-server face a similar problem where all imports
   have to be compiled.
3. Proposals such as #14905 to increase build parrelism are far less effective
   in projects which use ``TemplateHaskell``.


Proposed Change
---------------

When the new language extension ``ExplicitSpliceImports`` is enabled then a
new import modifier is added to the import syntax. An import is marked as a "splice"
import when it is prefixed with ``splice``.

::
  {-# LANGUAGE ExplicitSpliceImports #-}

  import A

  import splice B

Identifiers arising from splice imports are allowed to be used at negative levels, ie, unquoted in a top-level splice.

::

  foo = $(B.qux)


But identifers from normal imports are rejected

::
  -- Rejected, as A is not a splice import
  baz = $(A.zee)

An identifier can appear inside a top-level splice, if it is at a non-negative
level. For example, the following is legal::

  foo = $(B.qid [| A.zee |] )

Because ``A.zee`` is used at level 0.


When ``TemplateHaskell`` is enabled but NOT ``SpliceImports``, then all imports
are implicitly treated as splice imports, which matches the current behaviour.

The prelude module is implicitly imported as a splice module so the following is
allowed::

  zero = $(id [| 0 |])


Drawbacks
---------

* The user has to be aware of the significance of using splice imports.



Alternatives
------------

* It might be proposed that an alternative would be to work out which modules
  need to be compiled based on usage inside a module. This would compromise the
  principle that we can learn about what's needed for a module just by looking
  at the import list.

* Another alternative would be to allow even finer grained control of splice
  imports so that the cases of usage at levels -1 or -2 could be distinguished.
  This could be useful in some cross-compilation situations.


Unresolved Questions
--------------------

