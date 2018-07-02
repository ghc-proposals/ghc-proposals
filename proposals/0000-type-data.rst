.. proposal-number::
.. trac-ticket::
.. implemented::
.. highlight:: haskell

Define Kinds Without Promotion
==============================

This proposal introduces a language construct for defining kinds without
having to promote types.  For example, this is how we would
define a new kind ``Universe``, with three members::

  data kind Universe = Character | Number | Boolean

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

- GHC trac ticket for the same idea: https://ghc.haskell.org/trac/ghc/ticket/6024
- An older proposal for the same idea: https://ghc.haskell.org/trac/ghc/wiki/GhcKinds/KindsWithoutData
- Example of real code where the clutter is a problem:
  https://github.com/GaloisInc/crucible/blob/master/crucible/src/Lang/Crucible/Types.hs#L351


Proposed Change
---------------

We propose to add a new GHC extension called ``{-# KindDecls #-}``.
When this extension is enabled, the ``data`` keyword in a data declaration
may be followed by ``kind``, which signifies that this declaration introduces
a new kind.  Syntactically, ``kind`` is treated specially only in this context,
much like ``instance`` is in the context of a ``data instance``.

Semantically, the new declaration should work in the same way as kinds
introduced by promotion, with the following differences:

- The name of the "promoted" constructors are not prefixed by ``'`` and match
  the names in the declaration exactly.
- The declaration does not introduce any value-level constructors.

This allows a much more direct declaration of the example from the
motivation section::

  {-# Language KindDecls, GADTs #-}

  data kind Universe = Character | Number | Boolean

  data Type u where
    Character :: Type Character
    Number    :: Type Number
    Boolean   :: Type Boolean

The following table summarizes the names introduces by normal
`data` and `data kind` declarations.

================================= =============== ===============
        Declaration               Value Namespace Type Namespace
================================= =============== ===============
``data T = MkT``                     ``MkT``      ``T``
``data kind T = MkT``                (nothing)    ``T``,  ``MkT``
================================= =============== ===============

Note that since in GHC types and kinds share the same namespace,
the following declaration will be rejected::

  data kind T = T     // Invalid

Thus, when using a `data kind` the constructors must have different
names from the kind on the left of the ``=`` sign.






Drawbacks
---------
There are currently no known draw-backs to this feature.

Alternatives
------------

Don't do this, and just keep using data promotion.

Unresolved Questions
--------------------

There are currently no known unresolved questinos.
