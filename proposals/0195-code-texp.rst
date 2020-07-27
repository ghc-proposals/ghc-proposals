Make Q (TExp a) into a newtype
==============


.. author:: Matthew Pickering
.. date-accepted:: 2020-05-26
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/16177
.. implemented:: 9.0
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/195>`_.
.. contents::

I propose to modify the Typed Template Haskell API to make the ``Code`` type
more abstract. In particular we introduce a new data type called ``Code`` which
is abstract such that ``Code m a`` represents an expression of type ``a`` produced
in a monadic context ``m``. The reader should note that this proposal builds on
top of the `overloaded quotations proposal <https://github.com/ghc-proposals/ghc-proposals/pull/246>`_ which was
accepted before this proposal.

The Untyped Template Haskell API is unmodified. This proposal is only about
Typed Template Haskell.


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

      instance Quote m => Lang (Code m) where
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


3. ``m (TExp a)`` is ugly to read and write, understanding ``Code m a`` is
   easier. This is a weak reason but one everyone
   can surely agree with.


Proposed Change Specification
-----------------------------

In this section, the proposed complete interface for ``Code`` and ``TExp`` is
given.

A newtype is defined called ``Code``::

  newtype Code m a = Code (m (TExp a))

There are three main constructs that the proposal affects.

Quoting an expression ``e :: T`` now produces an expression of typed ``Quote m => Code m T``::

  -- foo :: Quote m => m (TExp Int)
  foo :: Quote m => Code m Int
  foo = [|| 5 ||]

Top-level splicing requires an expression of type ``Code Q T`` and produces a value of type ``T``::

  bar :: Int
  bar = $$foo

Nested splicing requires an expression of type ``Code m T`` and the overall
type of the quotation is a union of the constraints on all the nested splices::

  baz :: Quote m => Code m Int
  baz = [|| 1 + $$(foo) ||]

The return type of the ``liftTyped`` method of the class ``Lift``
is changed from ``m (TExp a)`` to ``Code m a``.::

  class Lift a where
    lift :: Quote m => a -> m Exp
    liftTyped :: Quote m => a -> Code m a

The functions ``unsafeCodeCoerce`` and ``unTypeCode`` are introduced to work directly
with ``Code``::

  unsafeCodeCoerce :: m Exp -> Code m a
  unTypeCode :: Code m a -> m Exp

There are still the normal functions for interacting with ``TExp a``::

  unsafeTExpCoerce :: Quote m => m Exp -> m (TExp a)
  unsafeTExpCoerce = fmap unsafeExpToTExp
  TExp :: Exp -> TExp a
  unType :: TExp a -> Exp
  unType (TExp a) = a

A new function is added to ``Language.Haskell.TH.Syntax`` in order to perform monadic actions inside of ``Code``::

  liftCode :: m (TExp a) -> Code m a
  liftCode = Code

And also a function which allows access to the wrapped ``TExp`` value::

  examineCode :: Code m a -> m (TExp a)
  examineCode (Code m) = m

``Code`` is still exported though so users can pattern match on it themselves
rather than using these convenience functions.

It is also useful to implement a method to modifying the underlying monadic
representation. For example, in order to handle additional effects before running
a top-level splice::

  hoistCode :: (forall a . m a -> n a) -> Code m a -> Code n a
  hoistCode f (Code a) = Code (f a)

  -- As an example, hoistCode can be used to handle a state effect
  handleState :: Code (StateT Int Q) a -> Code Q a
  handleState = hoistCode (flip runState 0)

Two more useful combinators are ``bindCode`` and ``bindCode_`` which
are versions of ``>>=`` and ``>>`` and interact nicely with QualifiedDo::

  bindCode :: m a -> (a -> Code m b) -> Code m b
  bindCode q k = liftCode (q >>= examineCode . k)

  bindCode_ :: m a -> Code m b -> Code m b
  bindCode_ q c = liftCode (q >> examineCode c)

The ``Code`` data constructor is also exposed to users in case they want to
explicitly interact with the underlying monadic computation in another manner.


Effect and Interactions
-----------------------
The proposal solves the main problem because now it is easily possible to write
instances for the ``Code`` type because it is no longer a composition of two
type constructors.


Costs and Drawbacks
-------------------

The main drawback is that this will break all users of Typed Template Haskell who
write type signatures.
However, I feel like I am the only user so the impact will be minimal.


Alternatives
------------

Unresolved Questions
--------------------


Implementation Plan
-------------------
Implementation is straightforward.
