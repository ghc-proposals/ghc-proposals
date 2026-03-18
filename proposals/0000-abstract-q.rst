Abstract Q
==============

.. author:: Teo Camarasu
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/700>`_.
.. sectnum::
.. contents::

Template Haskell is GHC's metaprogramming facility.
It allows users to make use of Haskell programs at compile-time that can manipulate Haskell syntax trees.
They can be effectful and run ``IO`` actions or introspect into the compiler state in certain limited ways.
These effects are exposed to users through the ``Quote`` and ``Quasi`` typeclasses and the ``Q`` monad.

These typeclasses are both external interfaces used by users and internal interfaces of GHC.
This exposes implementation details to users, which makes it difficult or impossible to alter
the internal interface while keeping the external interface the same.

We propose splitting the existing single interface into two, with a clear separation between the internal and external interfaces.
The internal interface would be part of GHC and versioned with it. The external would be able to evolve independently and be implemented in terms of the internal interface.

This will be implemented by adding a new ``MetaHandlers`` record type of actions as this purely internal interface.
``Q`` is to be changed into an abstract newtype over ``MetaHandlers -> IO a``. ``Quasi`` and ``Quote`` are to be implemented in terms of it.

The existing interface of ``template-haskell`` exposes a top-level ``runQ :: Quasi m => Q a -> m a`` function.
This would no longer be possible to implement with the new definition of ``Q``. So, we also propose adding a corresponding method to ``Quasi``.

In order to hide the implementation details, we propose hiding the ``Q`` (value) constructor and adding a backwards compatible ``unQ`` pattern synonym.

As such, the change from this proposal is very small. We alter the interface of ``Language.Haskell.TH.Syntax``:

* remove ``Q`` constructor
* add new ``qRunQ :: Quasi m => Q a -> m a`` method to the ``Quasi`` typeclass.

The rest of this proposal is about the detailed explanation for why we want to do that and how the implementation details for these choices.


Motivation
----------

Let's motivate this change with an example.

Suppose I wanted to add new effect that could be used in Template Haskell splices:
``reifyCore :: Name -> Q Core``, which given the ``Name`` of a term would produce the Core representation of it.

I would currently implement this by adding this as a method to ``Quasi`` and adding an implementation in GHC.
This forces a breaking change of ``template-haskell``, since it re-exports ``Quasi``.

We cannot hide this method for the sake of compatibility, since end users depend on the ability to write custom instances of ``Quasi`` for their own monads
(eg, `th-orphans <https://hackage.haskell.org/package/th-orphans-0.13.16/docs/Language-Haskell-TH-Instances.html>`_ provides instances for certain monad transformers).

We would run into the same issue if we wanted to remove a method from ``Quasi``  or change a method's type.
The change in GHC would have to be reflected in the exposed interface of ``template-haskell``, forcing end-users to upgrade to a new major release of ``template-haskell`` if they want to use the new version of GHC.

By separating out the internal interface, we can avoid this tight coupling.
``reifyCore`` can be added as a field to ``MetaHandlers``. ``Quasi`` would then live purely in ``template-haskell``.
Old versions of ``template-haskell`` would still be compatible with the new version of GHC, as they can simply ignore the new field.
A new major version of ``template-haskell`` can add the corresponding method to ``Quasi`` and use the field from ``MetaHandlers`` to implement it.

The definition of ``Q`` is fixed by GHC. A single definition of it for each version of GHC. Since it is implemented in terms of ``Quasi``, ``Quasi`` is also fixed.
By breaking this tight coupling, we allow ``template-haskell``\'s interface to potentially be compatible with a greater range of GHC versions and to evolve independently of it.

Our new definition of ``Q`` is also easier to optimise for the compiler, since it uses a known ``Monad``. Though this is a minor benefit, since the runtime performance of splices is rarely an issue.

It also has potential to interface well with the ``bluefin``/``effectful`` family of effects libraries, which are implemented in terms of ``IO``, and could not express Template Haskell effects previously.

Proposed Change Specification
-----------------------------

No changes are being proposed to the language.

Proposed Library Change Specification
-------------------------------------

This proposal requires making changes to the interface of ``template-haskell``.
In this section we will purely focus on the changes to the interface.
We will return to the implementation details in `Implementation plan <#7implementation-plan>`_

The interface of ``Language.Haskell.TH.Syntax`` (and ``Language.Haskell.TH``) will change from::

 -- Note: these is defined in ghc-internal:GHC.Internal.TH.Syntax
 -- and only re-exported from template-haskell. These are known key definitions for GHC.

 newtype Q a = Q { unQ :: forall m. Quasi m => m a }

 class (MonadIO m, MonadFail m) => Quasi m where
  qNewName :: String -> m Name
  qRecover :: m a -> m a -> m a
  qReport  :: Bool -> String -> m ()
  qReify   :: Name -> m Info
  ... and so on

to::

 -- Note: Q is defined in ghc-internal:GHC.Internal.TH.Syntax
 -- and only re-exported from template-haskell.
 -- It is still known key.
 newtype Q a -- Q is abstract or opaque
 -- bundled pattern synonym for backwards compatibility
 pattern unQ :: Q a -> forall m. Quasi m => m a

 -- Note: Quasi is now defined in template-haskell. It is no longer known key.

 class (MonadIO m, MonadFail m) => Quasi m where
  qRunQ     :: Q a -> m a -- New method
  qNewName :: String -> m Name
  qRecover :: m a -> m a -> m a
  qReport  :: Bool -> String -> m ()
  qReify   :: Name -> m Info
  ... and so on
  {-# MINIMAL qRunQ qRecover #-}

``unQ`` and the ``Q`` constructor would no longer be exported from ``template-haskell``.
This is a breaking change.

A new ``qRunQ :: Quasi m => Q a -> m a`` method would be added to a ``Quasi``, so that the top-level ``runQ`` can still be implemented.
This is a breaking change.

If a user gives a definition of ``runQ`` then all other methods except for ``qRecover`` can be implemented by lifting the method from the ``Q`` instance.
Therefore we would also make all methods of ``Quasi`` except for ``qRunQ`` and ``qRecover`` optional.
This means that libraries that implement ``Quasi`` instances would likely not have to make any changes if a new method is added.

``qRecover`` cannot be implemented in terms of ``qRunQ`` as it includes a mention of the monad in negative position.

The rest of the changes are internal to GHC and ``ghc-internal``.

Effect and Interactions
-----------------------

* The `Pure Template Haskell proposal <https://github.com/ghc-proposals/ghc-proposals/pull/655>`_ aims to
  empower users to ban use of ``IO`` in Template Haskell splices.
  This proposal opens up a lightweight implementation path for something along these lines.
  One could implement a ``dropIO :: Q a -> Q a`` function that removes the ``runIO`` effect from the ``MetaHandlers`` record,
  replacing it with an error call. This function could only be implemented by accessing ``ghc-internal``.


Costs and Drawbacks
-------------------

The main cost of this proposal is that it entails a breaking change to the ``template-haskell`` interface.
The implementation should be relatively simple and if anything it should simplify things as an existential is being replaced with a common-or-garden record.


Backward Compatibility
----------------------

While this is a breaking change to ``template-haskell``, this interface breaks regularly anyway. For instance, GHC-10 will include a new method in the ``Quasi`` typeclass, which would have a similar level of breakage. Implementing this change will protect users from future frequent breakages.

Despite this, we've tried to maximise backwards compatibility by adding an ``unQ`` pattern synonym to the interface.

The following packages on Hackage will be impacted as they give custom instances of ``Quasi``:

* ``RepLib``
* ``aeson-schema``
* ``large-records``
* ``geniplate``
* ``th-test-utils``
* ``nyan-interpolation-core``
* ``th-traced``


Implementation Plan
-------------------
Teo Camarasu will implement this.

This section is purely here to clarify the internal changes required to implement the changes to the interface.
The exact details of GHC's internals are out of scope of the proposal.

We would make the following changes in ``GHC.Internal.TH.Monad``::

  -- we create a new type
  data MetaHandlers m =
    MetaHandlers
    { mReify :: Name -> m Info
    , mNewName :: String -> m Name
    , mRecover :: forall a. Q a -> Q a -> m a
    ... and so on
    }

  -- we change the definition of Q
  newtype Q a = Q { unQ :: forall m. (forall x. m x -> IO x) -> MetaHandlers m -> IO a }

  -- we move Quasi Language.Haskell.TH.Syntax
  -- so the code is deleted from here

We would make the following changes in ``Language.Haskell.TH.Syntax``::

 class (MonadIO m, MonadFail m) => Quasi m where
  qRunQ     :: Q a -> m a -- New method
  qRecover :: m a -> m a -> m a
  qNewName :: String -> m Name
  qNewName nm = qRunQ $ \handlers -> runInIO $ mNewName handlers nm -- we add default methods
  qReport  :: Bool -> String -> m ()
  qReport severity msg = qRunQ $ \runInIO handlers -> runInIO $ mReport handlers severity msg -- we add default methods
  qReify   :: Name -> m Info
  qReify nm = qRunQ $ \runInIO handlers -> runInIO $ mReify handlers nm -- we add default methods
  ... and so on
  {-# MINIMAL qRunQ qRecover #-}

  instance Quasi Q where
    qRunQ = id
    qRecover r k = Q $ \runInIO handlers -> runInIO $ mRecover handlers r k 
    -- all other methods are just the default

  runQ :: Quasi m => Q a -> m a
  runQ = qRunQ

Note that ``qRecover`` is a special case. It cannot use the default method like the other methods as we get a value of ``Q`` in a negative position.

The new type of ``Q`` might seem somewhat complex, but a good way to think about it is that it is a computation running in ``IO`` along with a all the ``Quasi`` methods that run in ``env -> IO _`` and also a value of ``env``. The type is abstract since we don't want to make any assumptions about the monads defined in ``lib:ghc`` in ``ghc-internal``.

We would alter the code for running splices in ``GHC.Tc.Gen.Splice`` and would construct a value of type ``MetaHandlers TcM`` using the existing implementations.
To do so we would defined something like this::

  -- this replaces: runQuasi :: Quasi m => m a -> TcM a
  runQinTcM :: Q a -> TcM a
  runQInTcM (Q m) = IOEnv $ \env -> 
    let
      runInIO :: forall x. TcM x -> IO x
      runInIO (IOEnv n) = n env
    in m runInIO metaHandlersTcM

  metaHandlersTcM :: MetaHandlers TcM
  metaHandlersTcM = MetaHandlers
    { mFail = \str -> fail str
    , mRecover = \r k -> tryTcDiscardingErrs (runQinTcM r) (runQinTcM k)
    ...
    }

You might have noticed above that ``mRecover`` has type ``Q a -> Q a -> m a`` rather than ``m a -> m a -> m a``. This is because we have access to ``Q a -> TcM a`` when defining ``MetaHandlers TcM``, but we do not have access to ``TcM a -> Q a`` when defining the ``qRecover`` instance for ``Quasi`` in ``template-haskell``, since we are not allowed to depend on ``lib:ghc`` there.

We also have to modify the definition of the external interpreter, which simply proxies messages to the compiler. 
