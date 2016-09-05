.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

Type-indexed ``Typeable``
=========================

GHC's ``Typeable`` mechanism provides a means of working with dynamically-typed
values. While it serves this purpose well, using it safely requires care on the
part of the user as type representations carry no type infomation identifying
the represented type.

Here we propose a reimagining of the ``Typeable`` mechanism, adding
indexing the ``TypeRep`` type with the represented type. This additional type
information enables the type system to provde the soundness of many uses of
``Typeable``, allowing many currently unsafe programs to be written in a
completely type-safe manner.

Motivation
----------

Consider the case of ``Data.Dynamic`` which provides a type for working with
dynamically-typed values. Its definition is straightforward Haskell 98,

.. code-block:: haskell

    data Dynamic = Dynamic TypeRep Any

    toDyn :: Typeable a => a -> Dynamic
    toDyn v = Dynamic (typeOf v) (unsafeCoerce v)

    fromDynamic :: Typeable a => Dynamic -> Maybe a
    fromDynamic (Dynamic t v) =
      case unsafeCoerce v of 
        r | t == typeOf r -> Just r
          | otherwise     -> Nothing

    dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
    dynApply (Dynamic t1 f) (Dynamic t2 x) =
      case funResultTy t1 t2 of
        Just t3 -> Just (Dynamic t3 ((unsafeCoerce f) x))
        Nothing -> Nothing

Note how there are several potential bugs here which the type system is unable
to help us avoid,

* there is nothing to ensure that the ``TypeRep`` and the ``Any`` of a
  ``Dynamic`` value are consistent.

* we need to use ``unsafeCoerce`` in a number of places

Here you should describe in greater detail the motivation for the change. This
should include concrete examples of the shortcomings of the current
state of things.

Proposed Change
---------------

The core of the proposal is the introduction of a new type reflection interface,
exposed in the ``Type.Reflection`` module. This interface provides a mechanism
similar to ``Typeable`` but with an indexed representation type,

.. code-block:: haskell

    module Type.Reflection where

    data TypeRep (a :: k)
    instance Show (TypeRep a)

    -- Since TypeRep is indexed by its type and must be a singleton we can trivially
    -- provide these
    instance Eq (TypeRep a)  where (==) _ _    = True
    instance Ord (TypeRep a) where compare _ _ = EQ
    instance TestEquality TypeRepX

    -- | The kind of a type.
    typeRepKind :: TypeRep (a :: k) -> TypeRep k

With a ``Typeable`` constraint we can get a ``TypeRep`` for a (non-kind
polymorphic) type ``a`` with ``typeRep``,

.. code-block:: haskell

    class Typeable (a :: k)

    typeRep :: forall (a :: k). Typeable a => TypeRep a

Note how in contrast to ``Data.Typeable.typeRep`` we needn't provide a ``Proxy``
to ``typeRep``; the desired type propagates through ``TypeRep``\'s index.


We can pattern match on the structure of a ``TypeRep``. For instance, on type
constructors,

.. code-block:: haskell

    -- | A type constructor type. This is a bidirectional pattern.
    pattern TRCon :: forall k (a :: k). TyCon -> TypeRep a

    -- | Information about a type constructor. No means of constructing 'TyCon's
    -- is provided; the only values of this type available are those from
    -- 'TypeRep's.
    data TyCon
    tyConPackage :: TyCon -> String
    tyConModule :: TyCon -> String
    tyConName :: TyCon -> String

Type application can also be decomposed,

.. code-block:: haskell

    -- | A representation of a type application, @a b@. This is a bidirectional pattern.
    pattern TRApp :: forall k2 (fun :: k2). ()
                  => forall k1 (a :: k1 -> k2) (b :: k1). (fun ~ a b)
                  => TypeRep a -> TypeRep b -> TypeRep fun

We can also decompose function types (e.g. ``Int -> String``) in their argument
(e.g. ``Int``) and result types (``String``). Strictly speaking this can be
expressed in terms of ``TRFun`` but it seems like a common enough pattern that
it's worth providing a pattern for it,

.. code-block:: haskell

    pattern TRFun :: forall fun. ()
                  => forall arg res. (fun ~ (arg -> res))
                  => TypeRep arg
                  -> TypeRep res
                  -> TypeRep fun

We can also test for type equality,

.. code-block:: haskell

    -- | Kind-homogenous type equality
    eqTypeRep  :: forall k (a :: k) (b :: k).
                  TypeRep a -> TypeRep b -> Maybe (a :~: b)

    -- | Kind-heterogenous type equality
    eqTypeRep' :: forall k1 k2 (a :: k1) (b :: k2).
                  TypeRep a -> TypeRep b -> Maybe (a :~~: b)

Since ``TypeRep`` is a singleton, we can provide a means of satisfying a
``Typeable`` constraint with a ``TypeRep`` without loss of coherence,

.. code-block:: haskell

    withTypeable :: TypeRep a -> (Typeable a => b) -> b
    
Implementing ``Data.Dynamic``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With this reflection machinery we can implement the ``Data.Dynamic`` type
described in the Motivation section in a perfectly type-safe manner,

.. code-block:: haskell

    data Dynamic where
        Dynamic :: TypeRep a -> a -> Dynamic

    toDyn :: Typeable a => a -> Dynamic
    toDyn v = Dynamic (typeOf v) v

    fromDynamic :: Typeable a => Dynamic -> Maybe a
    fromDynamic (Dynamic t v) =
      case v of 
        r | t `eqTypeRep` typeOf r -> Just r
          | otherwise     -> Nothing
       where

    dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
    dynApply (Dynamic t1 f) (Dynamic t2 x) =
      case funResultTy t1 t2 of
        Just t3 -> Just (Dynamic t3 ((unsafeCoerce f) x))
        Nothing -> Nothing


Preserving ``Data.Typeable``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The existing ``Data.Typeable`` machinery can be expressed in terms of the
primitives provided by ``Type.Reflection``,

.. code-block:: haskell

    module Data.Typeable where

    import qualified Type.Reflection as R

    data TypeRep where
        TypeRep :: R.TypeRep a -> TypeRep

    instance Eq TypeRepX
    instance Ord TypeRepX
    instance Show TypeRepX

Here you should describe in precise terms what the proposal seeks to change.
This should cover several things,

* define the grammar and semantics of any new syntactic constructs
* define the interfaces for any new library interfaces
* discuss how the change addresses the points raised in the Motivation section
* discuss how the proposed approach might interact with existing features  

Note, however, that this section need not describe details of the
implementation of the feature. The proposal is merely supposed to give a
conceptual specification of the new feature and its behavior.

Drawbacks
---------

What are the reasons for *not* adopting the proposed change. These might include
complicating the language grammar, poor interactions with other features, 

Alternatives
------------

Here is where you can describe possible variants to the approach described in
the Proposed Change section.

Unresolved Questions
--------------------

Are there any parts of the design that are still unclear? Hopefully this section
will be empty by the time the proposal is brought up for a final decision.
