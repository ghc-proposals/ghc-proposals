Define Kinds Without Promotion
==============================

.. author:: Iavor Diatchki
.. date-accepted:: 2018-09-12
.. ticket-url::
.. implemented::
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/106>`_.
.. highlight:: haskell
.. contents::


This proposal introduces a language construct for defining kinds without
having to promote types.  For example, this is how we would
define a new kind ``Universe``, with three members::

  type data Universe = Character | Number | Boolean

Motivation
----------

Currently, GHC supports defining new kinds using data promotion, which means
that a single ``data`` declaration introduces both a type with value
constructors, and a kind with type constructors.  In some cases this
alleviates the need for duplicated declarations (e.g., ``Bool``), however,
in many common cases using promotion leads to clutter.  Consider, for example,
the following code pattern, which is very common when defining Haskell EDSLs::

  {-# Language DataKinds, GADTs #-}

  data Universe   = Character | Number | Boolean

  type Character  = 'Character
  type Number     = 'Number
  type Boolean    = 'Boolean

  data Type u where
    CharacterRepr :: Type Character
    NumberRepr    :: Type Number
    BooleanRepr   :: Type Boolean

The first issue is the collection of type synonyms, one for each constructor.
They provide proper names for the promoted types, which has a number of
benefits:

- we can refer to the types in export lists,
- we get an ambiguity error if another module happens to define a type with the same name; this is not the case if using the name of a promoted type directly---"normal" types silently "win", and the promoted type is ignored without an error.
- we can refer to the types from other modules without having to enable ``{-# DataKinds #-}``

The second issue is that the value constructors introduced by ``Universe``
are unused, but still clutter up the name space.  As a result,
we have to use different names in the GADT that defines the value-level
representatives for the members of ``Universe``.

Relevant links:

- GHC ticket for the same idea: https://gitlab.haskell.org/ghc/ghc/issues/6024
- An older proposal for the same idea: https://gitlab.haskell.org/ghc/ghc/wikis/ghc-kinds/kinds-without-data
- Example of real code where the clutter is a problem:
  https://github.com/GaloisInc/crucible/blob/master/crucible/src/Lang/Crucible/Types.hs#L351


Proposed Change
---------------

We propose to add a new GHC extension called ``{-# TypeData #-}``.
When this extension is enabled, the ``data`` keyword in a data declaration
may be preceeded by ``type``, which signifies that this declaration affects
only the types:  the LHS introduces a new kind, and the RHS introduces type
constructors that belong to this kind.

Semantically, the new declaration should work in the same way as kinds
introduced by promotion, with the following differences:

- The names of the "promoted" constructors are not prefixed by ``'`` and match
  the names in the declaration exactly.
- The declaration does not introduce any value-level constructors.

This allows a much more direct declaration of the example from the
motivation section::

  {-# Language TypeData, GADTs #-}

  type data Universe = Character | Number | Boolean

  data Type u where
    Character :: Type Character
    Number    :: Type Number
    Boolean   :: Type Boolean

The following table summarizes the names introduced by normal
`data` and `type data` declarations.

================================= =============== ===============
        Declaration               Value Namespace Type Namespace
================================= =============== ===============
``data T = MkT``                     ``MkT``      ``T``
``type data T = MkT``                (nothing)    ``T``,  ``MkT``
================================= =============== ===============


Differences From Ordinary Data Declarations
-------------------------------------------

The `type data` declarations described in this proposal correspond
to a restricted form of `data` declaration.   In particular, here
are some restrictions:

* **No Constructor Name Shadowing**
  Since in GHC types and kinds share the same namespace,
  the following declaration will be rejected::

    type data T = T     // Invalid

  Thus, when using a `type data` the constructors must have different
  names from the kind on the left of the ``=`` sign.

* **No Record Selectors**
  Record selectors are not supported.  One could imagine
  adding some support for that (e.g., by generating selector / updater type functions)
  but at the moment it is not clear that it is neccessary, or what the right design
  should be, so we leave it out of this proposal.

* **No Quantifiers**
  Existential and universal quantifiers in data declarations are not supported at the moment,
  as we don't know how they might work.

* **No Strictness Annotations and UNPACK pragmas**
  These don't really make sense at the type level.

Drawbacks
---------
There are currently no known draw-backs to this feature.

Alternatives
------------

Don't do this, and just keep using data promotion.

Unresolved Questions
--------------------

There are currently no known unresolved questions.
