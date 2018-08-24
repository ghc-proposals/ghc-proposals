Show Types as Symbols
=====================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/164>`_.
.. sectnum::
.. contents::

``ShowType`` has an annoyingly specific kind: ``k -> ErrorMessage``. If it were
instead ``k -> Symbol``, we could use it in many more places.


Motivation
------------
``ShowType`` is unnecessarily restricted to only being useable in custom type
error contexts. Slightly changing its kind would allow it to be used in many
more places.

One prime motiving case is for generating type schema documents of Haskell types
for marshalling data to other languages. Often these languages will use their
own names for primitive types, but non-trivial types should retain their names
after marshalling.

For example, if we wanted to marshall to JSON:

::

  type family ToJSONType (a :: Type) :: Symbol where
    ToJSONType Int       = "integer"
    ToJSONType Integer   = "integer"
    ToJSONType Float     = "number"
    ToJSONType Double    = "number"
    ToJSONType String    = "string"
    ToJSONType Bool      = "boolean"
    ToJSONType [a]       = "array"
    ToJSONType a         = ShowType a  -- kind error! :(


Additionally this would provide a more-principled approach for getting the name
of a type at the term level. Rather than abusing ``TypeRep`` s:

::

  typeName :: forall a. Typeable a => String
  typeName = show $ typeRep @a

we can instead write ``typeName`` as:

::

  typeName :: forall a. KnownSymbol (ShowTypeSymbol a) => String
  typeName = symbolVal $ Proxy @(ShowTypeSymbol a)


Proposed Change Specification
-----------------------------
The proposal is to introduce a new primitive type-family:

::

  type family ShowTypeSymbol (a :: k) :: Symbol

which will expand to a ``Symbol`` for ``a`` equivalent to what ``ShowType``
would emit today for its custom type error. In addition, it will emit a derived
constraint ``KnownSymbol (ShowTypeSymbol a)`` ensuring this symbol can be moved
to the term-level.


Effect and Interactions
-----------------------
The fallback instance ``ToJSONType a`` can now be defined as ``ShowTypeSymbol a``.

This change won't interact with any other existing features.


Costs and Drawbacks
-------------------
Development cost is likely tiny; after all, GHC already prints out the result of
``ShowType`` for custom type errors.

Maintenance cost is similarly small, this primitive would do little more than
use existing machinery to build a ``Symbol``.


Alternatives
------------
Showing things of kind ``Type`` can be somewhat be approximated already in one
of two ways:

::

  showTypeName
      :: forall a r
       . Typeable a
      => (forall name. KnownSymbol name => Proxy name -> r)
      -> r
  showTypeName k =
    case someSymbolVal (show $ typeRep @a) of
      SomeSymbol proxy -> k proxy

``showTypeName`` is annoying to use; it exists at the term-level and relies on a
continuation. Thus it cannot be used in a type family.

Alternatively, this can be provided via a ``Generic`` instance:

::

  type family RepName (x :: Type -> Type) :: Symbol where
    RepName (D1 ('MetaData name _ _ _) _) = name

  type family TypeName (x :: Type) :: Symbol where
    TypeName x = RepName (Rep x)

(thanks to `i-am-tom <https://github.com/i-am-tom>`_ for this alternative)

However this requires a ``Generic`` instance which might not have been derived,
and doesn't show the type parameters without significantly more work.

Neither ``showTypeName`` nor ``TypeName`` support kinds other than ``Type``.


Unresolved Questions
--------------------
Should we redefine ``ShowType`` in terms of ``ShowTypeSymbol``?

::

  type ShowType a = Text (ShowTypeSymbol a)

Possibly not, if ``ShowType`` wants to do context-sensitive pretty printing (eg.
word wrapping and indenting.)


Implementation Plan
-------------------
If accepted, I will implement the change.

