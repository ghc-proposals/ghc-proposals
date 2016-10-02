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

In general the current ``Typeable`` mechanism gives us no way to tell the
type-checker about the relationship between the (potentially unknown) type of a
value (say, the ``Any`` in the ``Dynamic`` example above) and the type
represented by a ``TypeRep``.

Proposed Change
---------------

This proposal follows the idea proposed in [PeytonJones2016]_. The core of the
proposal is the introduction of a new type reflection interface, exposed in the
``Type.Reflection`` module. This interface provides a mechanism similar to
``Typeable`` but with an indexed representation type,

.. code-block:: haskell

    module Type.Reflection where

    data TypeRep (a :: k)
    instance Show (TypeRep a)

    -- Since TypeRep is indexed by its type and must be a singleton we can trivially
    -- provide these
    instance Eq (TypeRep a)  where (==) _ _    = True
    instance Ord (TypeRep a) where compare _ _ = EQ
    instance TestEquality TypeRep

.. [PeytonJones2016]
    Peyton Jones, Weirich, Eisenberg, Vytiniotis. "`A Reflection on Types
    <http://research.microsoft.com/en-us/um/people/simonpj/papers/haskell-dynamic/index.htm>`__."
    *Proc. Philip Wadler's 60th birthday Festschrift*. Edinburgh, April 2016.

Like today, the new ``Typeable`` mechanism will only support kind-monomorphic
types. Unlike today's mechanism, we provide a means of extracting the *kind* of
a type representation,

.. code-block:: haskell

    -- | The kind of a type.
    typeRepKind :: TypeRep (a :: k) -> TypeRep k

With a ``Typeable`` constraint we can get a ``TypeRep`` for a (non-kind
polymorphic) type ``a`` with ``typeRep``,

.. code-block:: haskell

    class Typeable (a :: k)

    typeRep :: forall (a :: k). Typeable a => TypeRep a

Note how, in contrast to ``Data.Typeable.typeRep``, we needn't provide a
``Proxy`` to ``typeRep``; the desired type propagates through ``TypeRep``\'s
index.


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
expressed in terms of ``TRApp`` but it seems like a common enough pattern that
it's worth providing a pattern for it,

.. code-block:: haskell

    pattern TRFun :: forall fun. ()
                  => forall arg res. (fun ~ (arg -> res))
                  => TypeRep arg
                  -> TypeRep res
                  -> TypeRep fun

We can also test for type equality,

.. code-block:: haskell

    -- | Kind-homogeneous type equality
    eqTypeRep  :: forall k (a :: k) (b :: k).
                  TypeRep a -> TypeRep b -> Maybe (a :~: b)

    -- | Kind-heterogeneous type equality
    eqTypeRep' :: forall k1 k2 (a :: k1) (b :: k2).
                  TypeRep a -> TypeRep b -> Maybe (a :~~: b)

    -- | Kind-heterogeneous type equality
    data (a :: k1) :~~: (b :: k2) where
        HRefl :: a :~~: a

Since ``TypeRep`` is a singleton, we can provide a means of satisfying a
``Typeable`` constraint with a ``TypeRep`` without loss of coherence,

.. code-block:: haskell

    withTypeable :: TypeRep a -> (Typeable a => b) -> b

We will also see later that it is helpful to quantify over the type index. For
this we introduce,


.. code-block:: haskell

    data SomeTypeRep where
        SomeTypeRep :: forall a. TypeRep a -> SomeTypeRep

    instance Eq SomeTypeRep
    instance Ord SomeTypeRep
    instance Show SomeTypeRep

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
    dynApply (Dynamic (TRFun ta tr) f) (Dynamic ta' x)
      | Just HRefl <- ta `eqTypeRep` ta' = Just (Dynamic tr (f x))
    dynApply _ _                         = Nothing


Preserving ``Data.Typeable``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The existing ``Data.Typeable`` machinery can be expressed in terms of the
primitives provided by ``Type.Reflection``,

.. code-block:: haskell

                {-# LANGUAGE ScopedTypeVariables #-}

    module Data.Typeable where

    import qualified Type.Reflection as R

    type TypeRep = R.SomeTypeRep

    typeOf :: forall a. Typeable a => a -> TypeRep
    typeOf _ = R.SomeTypeRep (R.typeRep :: TypeRep a)

    typeRep :: forall proxy a. Typeable a => proxy a -> TypeRep
    typeRep = R.SomeTypeRep (R.typeRep :: TypeRep a)

    cast :: forall a b. (Typeable a, Typeable b) => a -> Maybe b
    cast x
      | Just HRefl <- ta `R.eqTypeRep` tb = Just x
      | otherwise                         = Nothing
      where
        ta = I.typeRep :: R.TypeRep a
        tb = I.typeRep :: R.TypeRep b

    eqT :: forall a b. (Typeable a, Typeable b) => Maybe (a :~: b)
    eqT
      | Just HRefl <- ta `I.eqTypeRep` tb = Just Refl
      | otherwise                         = Nothing
      where
        ta = I.typeRep :: I.TypeRep a
        tb = I.typeRep :: I.TypeRep b

The remaining existing exports of ``Data.Typeable`` follow easily.

.. code-block:: haskell

    gcast :: forall a b c. (Typeable a, Typeable b) => c a -> Maybe (c b)

    gcast1 :: forall c t t' a. (Typeable t, Typeable t')
           => c (t a) -> Maybe (c (t' a))
           
    gcast2 :: forall c t t' a b. (Typeable t, Typeable t')
           => c (t a b) -> Maybe (c (t' a b))

    typeRepTyCon :: TypeRep -> TyCon

    rnfTypeRep :: TypeRep -> ()

We can also continue to provide the deprecated non-kind-polymorphic ``Typeable``
exports,

.. code-block:: haskell

    typeOf1 :: forall t (a :: *). Typeable t => t a -> TypeRep
    typeOf2 :: forall t (a :: *) (b :: *). Typeable t => t a b -> TypeRep


Defining ``TypeRep``
~~~~~~~~~~~~~~~~~~~~

The heart of ``Type.Reflection`` is the ``TypeRep`` type. It can be defined as a
standard GADT (omitting the ``Fingerprint``\ s used for O(1) comparison), ::

    data TypeRep (a :: k) where
        TrTyCon :: TyCon -> TypeRep k -> TypeRep (a :: k)
        TrApp   :: forall k1 k2 (a :: k1 -> k2) (b :: k1).
                   TypeRep (a :: k1 -> k2)
                -> TypeRep (b :: k1)
                -> TypeRep (a b)

Here a type constructor type consists of a (possibly kind-polymorphic)
type constructor and a ``TypeRep`` of its kind. The kind is necessary to ensure
that we represent only kind-monomorphic types. Type application types are
represented by the representations of the two types of the application.

Since we cannot guarantee that the ``TrTyCon`` couldn't be used to construct
ill-kinded ``TypeRep``\ s we must hide it and instead expose a unidirectional
pattern synonym. In contrast, ``TrApp`` can be exposed bidirectionally.


Serializing type representations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Serializing type representations (with, e.g., the ``binary`` library) is a bit
trickier than it was in the past. Let's look at a few reasons why this is so.

Recursive kind relationships
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Let's start by only considering a naive serializer,

.. code-block:: haskell

    -- TyCon is just plain data, this is trivially provided...
    instance Binary TyCon

    putTypeRep :: TypeRep a -> Put
    putTypeRep (TrTyCon tycon kind) = put 0 >> putTypeRep tycon >> putTypeRep kind
    putTypeRep (TrApp a b) = put 1 >> putTypeRep a >> putTypeRep b

Consider, what happens when we attempt to serialize ``typeRep :: TypeRep Type``.
Recall that ``Type`` is one of the primitive types provided by GHC and that
``Type :: Type``. Forgetting for a moment that ``Type`` is in fact a type
synonym, this means that,

.. code-block:: haskell

    typeTypeRep :: TypeRep Type
    typeTypeRep = TrTyCon typeTyCon typeTypeRep

    typeTyCon :: TyCon
    typeTyCon = {- ... -}

Here we immediately see a problem: the recursive kind relationship of ``Type``
will cause our naive serializer ``putTypeRep`` to loop. Indeed the situation is
a bit more complicated and ``Type`` isn't the only of GHC's primitive types
which has this property. We also have,

.. code-block:: haskell

    type Type = TYPE 'PtrRepLifted

    data TYPE :: RuntimeRep -> Type

    data RuntimeRep :: Type
         = PtrRepLifted
         | {- ... -}

Therefore we have four distinct loops:

* Involving ``(->)``

  * ``(->) :: a -> b -> c``

* Involing ``(->)``, ``Type``, and ``TYPE``

  * ``(->) :: a -> b -> c``
  * ``Type :: TYPE 'PtrRepLifted``
  * ``TYPE :: RuntimeRep -> Type``

* Involving ``TYPE``, ``Type``

  * ``Type :: TYPE 'PtrRepLifted``
  * ``TYPE :: RuntimeRep -> Type``

* Involving ``TYPE``, and ``RuntimeRep``

  * ``TYPE :: RuntimeRep -> Type``
  * ``RuntimeRep :: Type``

* Involing ``RuntimeRep``, and ``'PtrRepLifted``

  * ``RuntimeRep :: Type``
  * ``'PtrRepLifted :: RuntimeRep``

This poses a rather unfortunate safety issue for authors of serializers,
pretty-printers, and other consumers which deeply inspect ``TypeRep`` s. One
option for approaching this would be to restructure the ``TypeRep`` type to draw
particular attention to these cases,

.. code-block:: haskell

    data TypeRep a where
        TRTyCon :: TyCon -> TypeRep k -> TypeRep (a :: k)
        TRApp   :: TypeRep a -> TypeRep b -> TypeRep (a b)
        TRTYPE  :: TypeRep (r :: RuntimeRep) -> TypeRep (TYPE r)
        TRArrow :: TypeRep a -> TypeRep b -> TypeRep (a -> b)
        TRPtrRepLifted :: TypeRep 'PtrRepLifted

This would ensure that consumers who match totally on ``TypeRep``\ s won't face
unexpected loops. This also may have the advantage of in some ways simplifying
production of evidence by the compiler and making the runtime representations of
common types more concise. Unfortunately, in other ways it complicates the
implementation of ``TypeRep`` since it needs to maintain some normalization
invariants.

Another option would be to retain the more simple two-constructor ``TypeRep``
described above but expose pattern synonyms for the special cases seen here,
along with clear documentation instructing library authors to handle them.

Type indexes and deserialization
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Consider the following potential type for a deserializer of ``TypeRep``, 

.. code-block:: haskell

    getTypeRep :: Get (TypeRep a)

Here ``getTypeRep`` claims to be a deserializer for any ``TypeRep a``, where
``a`` is determined by the caller. However, what if the ``TypeRep`` being
deserialized represents a distinct type ``b``? Clearly this deserializer should
fail, but how would it know? Afterall, ``getTypeRep`` does not have access to a
``TypeRep a`` to compare against.

It is easier to think of deserialization of ``TypeRep`` as a two-step operation:

1. First deserialize an unknown ``SomeTypeRep``
2. Then compare the ``SomeTypeRep`` against the type ``a`` expected by the caller

That is,

.. code-block:: haskell

    getSomeTypeRep :: Get SomeTypeRep
    getSomeTypeRep = {- ... -}

    getTypeRep :: forall a. Typeable a => Get (TypeRep a)
    getTypeRep = do
        r <- getSomeTypeRep
        case r of
          SomeTypeRep rep
            | rep `eqTypeRep` expected = expected
            | otherwise                = fail "Type mismatch"
      where
        expected = typeRep @a

Note the ``Typeable`` constraint on ``getTypeRep``. This is crucial since we
need to have ``Typeable`` evidence for the type that the caller *expects*.

Drawbacks
---------

What are the reasons for *not* adopting the proposed change. These might include
complicating the language grammar, poor interactions with other features, 

Alternatives
------------

Type-indexed TyCon
~~~~~~~~~~~~~~~~~~

The design described above does not propagate any type information beyond
``TypeRep``. An alterative would be to also add an index to ``Tycon``,

.. code-block:: haskell

    data TyCon (a :: k)

    data TypeRep (a :: k) where
        TRTyCon :: TyCon a -> TypeRep k -> TypeRep a

However, the benefits to this approach are unclear and it would complicate
evidence generation.

Unresolved Questions
--------------------

Do we want to allow the user to construct ill-kinded type representations? Given
that the the user could never cast with such a representation, it seems like
there is likely no potential for unsafety by doing so.

Implementation Status
---------------------

A variant of this proposal has been implemented and is available in the
`wip/ttypeable <https://github.com/bgamari/ghc/tree/wip/ttypeable>`_ branch.
However, there are a number of limitations elsewhere in the compiler that must
be lifted before this is can be merged. See the `GHC Wiki
<https://ghc.haskell.org/trac/ghc/wiki/Typeable/BenGamari>`_ for details.
