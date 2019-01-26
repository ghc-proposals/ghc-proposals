Deprecated Entities
==============

.. proposal-number::
.. trac-ticket:: 3427
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/167>`_.
.. sectnum::
.. contents::

This feature would allow to control what sort of entity a ``DEPRECATED`` pragma applies to.

Motivation
------------
It is a very common idiom to have a type and an identically named data constructor.
Sometimes one would want to deprecate the use of constructor.
(for example, when using smart constructors) with a help of ``DEPRECATED`` pragma.
However, according to the user guide, `there is currently no way to deprecate one thing without the other.
<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#warning-deprecated-pragma>`_
The usual workaround would be to have a module that imports one but not the other,
however while that's possible for the type it's not possible for the constructor.

Proposed Change Specification
-----------------------------

* extend ``DEPRECATED`` pragma with a disambiguating specifiers:
  ``pattern`` - for value-level things,
  ``type`` - for types.
* the unqualified case would mean deprecating both entities as it does now.

Although, ``pattern`` seems like a weird choice, we are using it deliberately to be consistent
with other language features(for example, `PatternSynonyms <https://downloads.haskell.org/~ghc/master/users-guide/glasgow_exts.html#patsyn-impexp>`_).

Effect and Interactions
-----------------------
An example of a use case for this is the following. Given the following module: ::

    module A where

    data Foo = Foo
    {-# DEPRECATED Foo "Don't use type and data constructor Foo" #-}

    data Bar = Bar
    {-# DEPRECATED type Bar "Don't use type Bar" #-}

    data Baz = Baz
    {-# DEPRECATED pattern Baz "Don't use data constructor Baz" #-}

When compiling the code which happens to use data constructor or type ``Foo``, we will see the following warnings: ::

    Main.hs:5:8: warning: [-Wdeprecations (in -Wdefault)]
        In the use of type constructor or class ‘Foo’ (imported from A):
        Deprecated: "Don't use type and data constructor Foo"

    Main.hs:6:7: warning: [-Wdeprecations (in -Wdefault)]
        In the use of data constructor ‘Foo’ (imported from A):
        Deprecated: "Don't use type and data constructor Foo"

In the same vein, if we use data constructor or type ``Bar``,
we will be warned **only when we use the type**, but not the constructor,
since we specified which entity we want to deprecate in the above mentioned module ``A``: ::

    Main.hs:8:8: warning: [-Wdeprecations (in -Wdefault)]
        In the use of type constructor or class ‘Bar’ (imported from A):
        Deprecated: "Don't use type Bar"

Same logic applies to ``Baz``, we will be warned **only when its data constructor** is used(but not its type): ::

    Main.hs:12:7: warning: [-Wdeprecations (in -Wdefault)]
        In the use of data constructor ‘Baz’ (imported from A):
        Deprecated: "Don't use data constructor Baz"

When having several entities in one pragma, specifiers can be supplied per entity.
This will work: ::

    {-# DEPRECATED type Qux, pattern Quux "Don't use this" #-}

This feature does not work on ``module`` level.
Module level deprecation already implies the entity - the module itself.

Costs and Drawbacks
-------------------
There are currently no known drawbacks to this feature.

Alternatives
------------
* The usual workaround would be to have a module that imports one but not the other.
  Unfortunately this workaround is limited as it would only work for types, but not for data constructors.
  Another option would be to refactor data constructor names, which is not backward compatible and inefficient.

* Another alternative would be to try to utilize Haddock annotations. Example: ::

    -- | DEPRECATE: This type is deprecated
    data Foo =
        -- | DEPRECATE: This constructor is deprecated
        Foo x

* Another idea is to make ``DEPRECATED`` positional. One says ::

    module M {-# DEPRECATED "blah" #-} where ...

  One could do the same for data constructors, thus ::

    data Baz = Baz {-# DEPRECATED "blah" #-}
             | Boo
             | Bim {-# DEPRECATED "blah" #-} Int

  Or in GADT syntax ::

    data Baz where
      Baz :: Int -> Baz
      Boo :: Baz
      Bim :: Baz
      {-# DEPRECATED Bim, Baz "blah" #-}

  This positional story works well when the deprecation is attached to the definition of the thing. If you want to import something, deprecate it, and re-export it, it would not work so well. But (SPJ thinks) it is not possible to do that anyway today.

Unresolved Questions
--------------------

Implementation Plan
-------------------

If accepted, I (`@nineonine <https://github.com/nineonine>`_) volunteer to implement this change.
`Phab Diff <https://phabricator.haskell.org/D5126>`_
