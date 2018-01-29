.. proposal-number::
.. trac-ticket::
.. implemented::
.. highlight:: haskell

Define Kinds Withouth Promotion
===============================

This proposal introduces a language construct for defining kinds without
having to promote types.  For example, this is how we would
define a new kind ``Universe``, with three members:

```haskell
data kind Universe = Character | Number | Boolean
```

Motivation
----------

Currently, GHC supports defining new kinds using data promotion, which means
that a single ``data`` declaration introduces both a type with value
constructors, and a kind with type constructors.  In some cases this
alleviates the need for duplicated declarations (e.g., ``Bool``), however,
in many common cases using promotion leads to clutter.  Consider, for example,
the following code pattern, which is very common when defining Haskell EDSLs::

  data Universe   = Character | Number | Boolean
  
  type Character  = 'Character
  type Number     = 'Number
  type Boolean    = 'Boolean
  
  data TypeRepr u where
    CharacterRepr :: Type Character
    NumberRepr    :: Type Number
    BooleanRepr   :: Type Boolean

The first issue is that the names of the promoted constructors are
derived from the declared constructors by prepending ``'``.
To get the names we actually want, we have to use type synonyms,
one per constructor.  These add nothing but clutter to the program.

The second issue is that the value constructors introduced by ``Universe``
are unused, but still clutter up the name space.  As a result,
we have to use different names in the GADT that defines the value-level
representatives for the members of ``Universe``.

Relevant links:
  - GHC trac ticket for the same idea: https://ghc.haskell.org/trac/ghc/ticket/6024
  - An older proposal for the same idea: https://ghc.haskell.org/trac/ghc/wiki/GhcKinds/KindsWithoutData
  - Example of real code where the clutter is a problem:
    https://github.com/GaloisInc/crucible/blob/master/crucible/src/Lang/Crucible/Types.hs


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

  data kind Universe = Character | Number | Boolean
  
  data TypeRepr u where
    Character :: Type Character
    Number    :: Type Number
    Boolean   :: Type Boolean


Drawbacks
---------

There are currently no known draw-backs to this feature.

Alternatives
------------

Don't do this, and just keep using data promotion.

Unresolved Questions
--------------------

There are currently no known unresolved questinos.
