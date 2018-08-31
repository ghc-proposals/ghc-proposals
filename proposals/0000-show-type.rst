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

``ShowType`` has an annoyingly specific kind: ``k -> ErrorMessage``. If we had
something similar whose kind  were instead ``k -> Symbol``, we could use it in
many more places.


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
The proposal is to introduce a new primitive type-family, and a helper:

::

  type family ShowTypeSymbolPrec (i :: Nat) (t :: k) :: Symbol

  type family ShowTypeSymbol (t :: k) :: Symbol where
    ShowTypeSymbol t = ShowTypeSymbolPrec 0 t


which will expand to a ``Symbol`` for ``a`` at precedence ``i``. When ``i
~ 0``, this is equivalent to the source-level name of ``t``. An exact
specification is given below.

* If ``t`` is stuck, ``ShowTypeSymbolPrec i t`` is also stuck.

* If ``t`` is of kind ``Symbol``, return the symbol wrapped in quotes. For
  example:

  ::

    type instance ShowTypeSymbolPrec i (a :: Symbol) =
      AppendSymbol "\"" (AppendSymbol a "\"")

* If ``t`` is of kind ``Nat``, return it as a symbol. For example:

  ::

    type instance ShowTypeSymbolPrec i 17 = "17"

* If ``t`` is of kind ``[a]``, show it element-wise. For example:

  ::

    type instance ShowTypeSymbolPrec i '[a, b] =
      AppendSymbol "'[" (
        AppendSymbol (ShowTypeSymbolPrec 0 a) (
          AppendSymbol ", " (
            AppendSymbol (ShowTypeSymbolPrec 0 b) (
              "]"

* If ``t`` is a promoted tuple AND it is saturated, show it in tuple-form:

  ::

    type instance ShowTypeSymbolPrec i '(a, b) =
      AppendSymbol "'(" (
        AppendSymbol (ShowTypeSymbolPrec 0 a) (
          AppendSymbol ", " (
            AppendSymbol (ShowTypeSymbolPrec 0 b) (
              ")"

  Unsaturated promoted tuples are handled below by the more general rules for
  constructors.

* If ``t`` is a type constructor, return the name of the type constructor. For
  example: ``type instance ShowTypeSymbolPrec i Either = "Either"``

* If ``t`` is a promoted data constructor, return the name of the promoted data
  constructor (including the leading tick.) For example: ``type instance
  ShowTypeSymbolPrec i 'Left = "'Left"``

* If ``t`` is of the form ``a `f` b`` where ``f`` is a type operator, expand as
  follows:

  ::

    type instance ShowTypeSymbolPrec i (a `f` b) =
      NeedsParens i (Precedence f) (
        AppendSymbol (ShowTypeSymbolPrec (Precedence f + 1) a) (
          AppendSymbol " " (
            AppendSymbol (ShowTypeSymbolPrec 0 f) (
              AppendSymbol " " (
                ShowTypeSymbolPrec (Precedence f + 1) b)))))

  where ``Precedence f`` comes from the precedence of the fixity declaration
  for ``f``.

* If ``t`` is of the form ``f a``, expand as follows:

  ::

    type instance ShowTypeSymbolPrec i (f a) =
      NeedsParens i 0 (
        AppendSymbol (ShowTypeSymbolPrec 0 f) (
          AppendSymbol " " (
            ShowTypeSymbolPrec 10 a)))


For completeness, the following definitions are used in the above examples:

::

  type family ShowTypeSymbolPrec (i :: Nat) (t :: k) :: Symbol

  type family ShowTypeSymbol (t :: k) :: Symbol where
    ShowTypeSymbol t = ShowTypeSymbolPrec 0 t

  type family NeedsParens (i :: Nat) (j :: Nat) (s :: Symbol) :: Symbol where
    NeedsParens i j s = PrintParens (CmpNat i j) s

  type family PrintParens (c :: Ordering) (s :: Symbol) :: Symbol where
    PrintParens 'GT s = AppendSymbol "(" (AppendSymbol s ")")
    PrintParens c   s = s

  type family Precedence (f :: k1 -> k2 -> k3) :: Nat


Effect and Interactions
-----------------------
The fallback instance ``ToJSONType a`` can now be defined as ``ShowTypeSymbol a``.

This change won't interact with any other existing features.


Costs and Drawbacks
-------------------
Development cost is likely tiny; after all, GHC already prints out types at
their source-level representation.

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
and doesn't show parameters without significantly more work.

Neither ``showTypeName`` nor ``TypeName`` support kinds other than ``Type``.


Unresolved Questions
--------------------
None


Implementation Plan
-------------------
If accepted, I will implement the change.

