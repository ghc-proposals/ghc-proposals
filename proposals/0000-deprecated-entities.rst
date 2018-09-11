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
Sometimes one would want to deprecate the use of constructor
(for example, when using smart constructors) with a help of ``DEPRECATED`` pragma.
However, according to the user guide, `there is currently no way to deprecate one thing without the other.
<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#warning-deprecated-pragma>`_
The usual workaround would be to have a module that imports one but not the other,
however while that's possible for the type it's not possible for the constructor.

Proposed Change Specification
-----------------------------

* extend ``DEPRECATED`` pragma with a disambiguating specifier: ``data`` for data constructors and ``type`` for types.
* the unqualified case would mean deprecating both entities as it does now.


Effect and Interactions
-----------------------
An example of a use case for this is the following. Given the following module: ::

    module A where

    data Foo = Foo
    {-# DEPRECATED Foo "Don't use type and data constructor Foo" #-}

    data Bar = Bar
    {-# DEPRECATED type Bar "Don't use type Bar" #-}

    data Baz = Baz
    {-# DEPRECATED data Baz "Don't use data constructor Baz" #-}

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


When specifying several entities in one pragma,
the sort of deprecated entity we've specified will apply to all listed entities.
This will work - warn when these types are used(but not their constructors): ::

    {-# DEPRECATED type Qux, Quux "Don't use this" #-}

This will not work (parse error): ::

    {-# DEPRECATED type Qux, constructor Quux "Don't use this" #-}

This feature does not work on ``module`` level.
Module level deprecation already implies the entity - the module itself.

Costs and Drawbacks
-------------------
There are currently no known drawbacks to this feature.

Alternatives
------------
The usual workaround would be to have a module that imports one but not the other.
Unfortunately this workaround is limited as it would only work for types, but not for data constructors.
Another option would be to refactor data constructor names, which is not backward compatible and inefficient.

Unresolved Questions
--------------------
1) What specifier should be used for data constructors?
`Initial feature request <https://ghc.haskell.org/trac/ghc/ticket/3427>`_ suggested to use `constructor` but
using `specifiers from disambiguation in export list proposal <https://ghc.haskell.org/trac/ghc/wiki/Design/TypeNaming>`_
seems better since it does not require new keywords to be introduced. Another disadvantage of using `constructor`
is that it is quite a widely used identifier so making it a keyword is bad for backward compatibility
(for example, `hsc2hs uses it <https://github.com/haskell/hsc2hs/blob/master/CrossCodegen.hs#L470>`_ ).

Implementation Plan
-------------------
* add new reserved keyword for disambiguating data constructors (?)
* add new datatype to distinguish between different deprecated entities - ``DeprEntity``
* extend ``WarningTxt`` type, namely ``DeprecatedTxt`` constructor with a field of type ``DeprEntity``
* during the renaming phase, in `warnIfDeprecated` do extra check for the deprecated entity
* perform check against ``DeprEntity`` and ``Namespace``

If accepted, I (`@nineonine <https://github.com/nineonine>`_) volunteer to implement this change.
`Phab Diff <https://phabricator.haskell.org/D5126>`_
