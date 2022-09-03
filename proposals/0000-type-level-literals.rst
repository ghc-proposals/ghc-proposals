Separate tag for type-level literals
====================================

.. author:: Ross Paterson
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/536>`_.
.. sectnum::
.. contents::

Introduce a language tag ``TypeLevelLiterals`` (implied by ``DataKinds``)
to turn on the features in the *Type-Level Literals* section of the
*User's Guide*.

.. _`#102`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0106-type-data.rst

Motivation
----------
Currently type-level literals are turned on by the language tag
``DataKinds``, but some programmers want to use them without turning on
datatype promotion.

Indeed, `#102`_ (Define Kinds Without Promotion) will allow programmers to avoid
``DataKinds`` by using ``type data`` definitions to define constructors
at the type level, but programmers using these definitions may also want
to use type-level literals.

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
The cost is an extra language tag.
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
