Type-level literals as a separate language extension
====================================================

.. author:: Ross Paterson
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/536>`_.
.. sectnum::
.. contents::

Introduce a language extension ``TypeLevelLiterals`` (implied by ``DataKinds``)
to turn on the features in the *Type-Level Literals* section of the
*User's Guide*.

.. _`#106`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0106-type-data.rst

Motivation
----------
Like the ``TypeData`` extension (`#106`_, implemented in 9.6), this
proposal aims to allow some type-level programming without connecting
the constructor and type/class namespaces, as done by ``DataKinds``.

It would allow literals like ``23``, ``'c'`` and ``"abc"`` in both
expressions and types, but not (unless ``DataKinds`` were specified)
constructors like ``False`` and ``Just``.  The difference is that
constructors would conflict with types of the same name, in which case
they would need to be disambiguated with the syntax ``'False``.

This will not be enough for users wanting the full power of ``DataKinds``,
but not all do, and this refactoring does no harm.

Proposed Change Specification
-----------------------------

The extension ``TypeLevelLiterals`` would enable type-level literals,
as described in the *Type-Level Literals* section of the *User's Guide*.
The new extension would be implied by the ``DataKinds`` extension, yielding
backwards compatibility.

Examples
--------

A sized vector type without promoting the constructors to the type level::

  {-# LANGUAGE TypeLevelLiterals, GADTs #-}
  module Vector where

  import Data.Kind (Type)
  import GHC.TypeLits
  import Numeric.Natural

  data Vector :: Natural -> Type -> Type where
    Nil :: Vector 0 a
    Cons :: a -> Vector n a -> Vector (n+1) a

Effect and Interactions
-----------------------
Modules containing::

  {-# LANGUAGE TypeLevelLiterals #-}

will be able to use integer, string and character literals in types,
without having all their data constructors promoted into the type
constructor namespace (unless requested).

Modules containing::

  {-# LANGUAGE DataKinds #-}

will behave as before, because ``DataKinds`` will imply ``TypeLevelLiterals``.

Costs and Drawbacks
-------------------
The cost is an extra language extension name.
It is trivial to implement.

Alternatives
------------
The existing workaround of using ``DataKinds`` may add unwanted names to
the type constructor namespace.

Unresolved Questions
--------------------

Implementation Plan
-------------------
Fairly trivial:

* add the flag
* add the dependency to ``impliedXFlags``
* make the ``HsTyLit`` case of ``rnHsTyKi`` use ``TypeLevelLiterals`` instead of ``DataKinds``

Endorsements
-------------
