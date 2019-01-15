Make Q (TExp a) into a newtype
==============

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: #16177
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

I propose to modify the typed template Haskell API to make the ``Code`` type
more abstract. In particular we introduce a new data type called ``Code`` which
is abstract such that ``Code m a`` represents an expression of type ``a`` produced
in a monadic context ``m``. The reader should note that this proposal builds on
top of the `overloaded quotations proposal<https://github.com/ghc-proposals/ghc-proposals/pull/246>`_ which was
accepted before this proposal.


Motivation
------------

There are three problems with the current API:

1. It is hard to properly write instances for ``Quote m => m (TExp a)`` as the type is the composition
   of two type constructors. Doing so in your program involves making your own newtype and
   doing a lot of wrapping/unwrapping.

   For example, if I want to create a language which I can either run immediately or
   generate code from I could write the following with the new API. ::

      class Lang r where
        _int :: Int -> r Int
        _if  :: r Bool -> r a -> r a -> r a

      instance Lang Identity where
        _int = Identity
        _if (Identity b) (Identity t) (Identity f) = Identity (if b then t else f)

      instance Lang (Code Q) where
        _int = liftTyped
        _if cb ct cf = [|| if $$cb then $$ct else $$cf ||]

2. When doing code generation it is common to want to store code fragments in
   a map. When doing typed code generation, these code fragments contain a
   type index so it is desirable to store them in one of the parameterised
   map data types such as ``DMap`` from ``dependent-map`` or ``MapF`` from
   ``parameterized-utils``.

   ::

      compiler :: Env -> AST a -> Code Q a

      data AST a where ...
      data Ident a = ...

      type Env = MapF Ident (Code Q)

      newtype Code m a = Code (m (TExp a))


   In this example, the ``MapF`` maps an ``Ident String`` directly to a ``Code Q String``.
   Using one of these map types currently requires creating your own newtype and constantly
   wrapping every quotation and unwrapping it when using a splice. Achievable, but
   it creates even more syntactic noise than normal metaprogramming.


3. ``Quote m => m (TExp a)`` is ugly to read and write. This is a weak reason but one everyone
   can surely agree with.


Proposed Change Specification
-----------------------------

A newtype is defined called ``Code``::

  newtype Code m a = Code (m (TExp a))

There are three constructs that the proposal affects.

Quoting an expression ``e :: T`` now produces an expression of typed ``Quote m => Code m T``.::

  -- foo :: Quote m => m (TExp Int)
  foo :: Quote m => Code m Int
  foo = [|| 5 ||]

Top-level splicing requires an expression of type ``Code Q T`` and produces a value of type ``T``.::

  bar :: Int
  bar = $$foo

The return type of lifting a value is changed from ``m (TExp a)`` to ``Code m a``.::

  class Lift a where
    lift :: Quote m => a -> m Exp
    liftTyped :: Quote m => a -> Code m a

The functions ``unsafeTExpCoerce`` and ``unTypeQ`` are modified to work directly
with ``Code``::

  unsafeTExpCoerce :: m Exp -> Code m a
  unTypeQ :: Code m a -> m Exp

A new method is added in order to perform monadic actions inside of ``Code``.::

  liftCode :: m (TExp a) -> Code m a
  liftCode = Code

It is also useful to implement a method to modifying the underlying monadic
representation. For example, in order to handle additional effects before running
a top-level splice::

  handleM :: (forall a . m a -> n a) -> Code m a -> Code n a
  handleM f (Code a) = Code (f a)

  -- Can be used to handle a state effect
  handleState :: Code (StateT Int Q) a -> Code Q a
  handleState = handleM (flip runState 0)

The ``Code`` data constructor is also exposed to users in case they want to
explicitly interact with the underlying monadic computation in another manner.


Effect and Interactions
-----------------------
The proposal solves the main problem because now it is easily possible to write
instances for the ``Code`` type because it is no longer a composition of two
type constructors.


Costs and Drawbacks
-------------------

The main drawback is that this will break all users of typed Template Haskell who
write type signatures.
However, I feel like I am the only user so the impact will be minimal.


Alternatives
------------

Instead of ``liftCode`` it might have been more intuitive to add convenient
functions such as ``addM`` and ``addMThen`` to the API::

  addM :: m () -> Code m a -> Code m a
  addMThen :: m a -> (a -> Code m b) -> Code m b

These functions can already be implemented with ``liftCode``::

  addM q c = liftCode (q >> fromCode c)
  addMThen q k = liftCode (q >>= fromCode . k)


So can be added as library functions.


Unresolved Questions
--------------------


Implementation Plan
-------------------
Implementation is straightforward.
