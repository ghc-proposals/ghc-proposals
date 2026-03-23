Abstract Q
==============

.. author:: Teo Camarasu
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/700>`_.
.. sectnum::
.. contents::

Template Haskell is GHC's metaprogramming facility.
It allows users to make use of Haskell programs at compile-time to manipulate Haskell syntax trees.
These programs are written in the ``Q`` monad, which is analogous to the familiar ``IO`` monad of everyday Haskell programs.

They can be effectful and, for instance, run ``IO`` actions (via ``liftIO :: IO a -> Q a``) or introspect into the compiler state in certain limited ways.
For instance, we can reify certain information about a name (``reify :: Name -> Q Info``), or store some state between different splice invocations in the same module ``putQ :: Typeable a => a -> Q ()``.

The list of primitive methods exposed from ``Q`` is defined by the ``Quasi m`` typeclass. This typeclass is exposed to users via ``template-haskell`` but is also used inside the compiler to define how computations in ``Q`` should be run.

Any change to the interface of ``Q`` / ``Quasi`` is a breaking change, which requires a new major release of ``template-haskell``,
and this change cannot be made forwards or backwards compatible.

The aim of this proposal is to allow modifying this interface in a forwards and backwards compatible way and to only require a minor release of ``template-haskell`` when such a change is made.

This will allow us to give greater stability guarantees for the very widely used ``template-haskell`` library, which has 13936 transitive reverse dependencies. Thus reducing the amount of upper bound updating required when a new version of GHC comes out is very valuable. Being able to change these interfaces more easily also opens the door to cleaning up historical issues in a non-breaking way.

Our strategy is to hide the definition of the ``Q`` monad. At present, it is defined in terms of the ``Quasi`` typeclass, which is part of the public interface. We replace this implementation. We continue to export ``Quasi`` in order to preserve backwards compatibility, but it will no longer be a load bearing part of the interface.

While this does lead to a breaking change to the interface, we try to keep the damage to a minimum.

In short, we propose to:

* no longer export ``Q`` constructor
* add new ``qRunQ :: Quasi m => Q a -> m a`` method to the ``Quasi`` typeclass.


Motivation
----------

The definition of Q and Quasi 
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``Q`` typeclass is defined as::

  newtype Q a = Q {unQ :: forall m. Quasi m => m a}

This tells us that a value of ``Q a`` consists of a ``Quasi`` dictionary for some type ``m`` and a value of ``m a``.
When constructing a value of ``Q a``, we do not know which concrete monad that computation will be executed in, but we do know that monad will be an instance of ``Quasi``.
Thus we can only rely on ``Quasi`` and derived operations when constructing our value.

This roundabout definition exists to solve a problem. Users of ``template-haskell`` do not wish to depend on the ``ghc`` library, but that is where the definitions of the methods of ``Quasi``, which are run when splices are executed, can be found. This definition allows us the invert this dependency. Rather than ``template-haskell`` depending on ``ghc``, ``ghc`` actually depends on the location where ``Quasi`` and ``Q`` are defined, ``ghc-internal``.

To allow running splices. GHC provides a ``Quasi`` instance for the typechecking monad, ``TcM``, and then uses ``unQ`` specialized to ``Q a -> Quasi TcM => TcMa``. 

Both ``Quasi`` and ``Q`` are defined in ``ghc-internal`` and then used by ``ghc`` and also re-exported by ``template-haskell``.


Adding a new method to splices
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Let's say we want to add ``reifyCore :: Name -> Q Core`` to the set of methods accessible from TemplateHaskell splices.
We would modify the definition of ``Quasi`` in ``ghc-internal`` by adding a new ``qReifyCore :: Name -> m Core`` method.
We would give a definition of that method for ``TcM`` in the compiler (and for the external interpreter).
Finally, we would create a utility function ``reifyCore nm = Q $ qReifyCore nm`` in ``template-haskell`` to lift this into ``Q``.

As we've added a new method to a typeclass exposed from ``template-haskell`` we need to release a new major version as per the `PVP <https://pvp.haskell.org/>`_.
Suppose that the previous version of GHC was ``GHC-1`` and that it ships with ``template-haskell-0.1.0``.
Then ``GHC-2`` will need to ship with ``template-haskell-0.2.0``.

Note that ``Quasi`` lives in ``ghc-internal``, so it must be versioned with GHC.
This means that ``template-haskell-0.1`` cannot be made compatible with ``GHC-2`` and 
``template-haskell-0.2`` cannot be made compatible with ``GHC-1``.

Any user of ``template-haskell`` is then forced to update their upper bound on ``template-haskell`` if they wish to upgrade to ``GHC-2``, and this must be done atomically.
They could not update ``template-haskell`` first and ``GHC`` second or vice versa.

Other instances of Quasi
^^^^^^^^^^^^^^^^^^^^^^^^
As we have exposed this ``Quasi`` typeclass, end users are free to give their own instances of it.
For instance, the `th-orphans <https://hackage.haskell.org/package/th-orphans-0.13.16/docs/Language-Haskell-TH-Instances.html>`_ provides instances for certain monad transformers.
Such instances are never used when running splices in the compiler, but they are useful in similar ways to how it is useful to place ``IO`` in monad transformer stacks and use ``MonadIO`` to lift ``IO`` operations into that.
In practice, ``Quasi`` often functions equivalently to ``MonadIO`` for ``Q``.

Concrete monad
^^^^^^^^^^^^^^

The use of an abstract ``Monad`` in the definition of ``Q`` means that the optimizer cannot inline the ``>>=``, which can be important for the performance of longer splices. Although for most users this is extremely unlikely to be an issue. Our design uses a concrete ``Monad`` to alleviate this.

Our design also allows better interfacing with the  ``bluefin``/``effectful`` family of effects libraries, which are implemented in terms of ``IO``, and could not express Template Haskell effects previously.

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
 -- backwards compatibility function to shim over the removed record selector
 unQ :: Q a -> forall m. Quasi m => m a

 -- Note: Quasi is now defined in template-haskell. It is no longer known key.

 class (MonadIO m, MonadFail m) => Quasi m where
  qRunQ     :: Q a -> m a -- New method
  qNewName :: String -> m Name
  qRecover :: m a -> m a -> m a
  qReport  :: Bool -> String -> m ()
  qReify   :: Name -> m Info
  ... and so on
  {-# MINIMAL qRunQ qRecover #-}

The ``Q`` constructor would no longer be exported from ``template-haskell``.
This is a breaking change.

A new ``qRunQ :: Quasi m => Q a -> m a`` method would be added to a ``Quasi``, so that the top-level ``runQ`` can still be implemented.
This is a breaking change.

Adding the ``qRunQ`` method is crucial to ensure that users can still give meaningful instances of ``Quasi`` even if it is no longer tightly coupled to the implementation of ``Q``.
``qRunQ`` is analgous to ``liftIO`` but we have chosen to avoid the term, since a ``runQ`` function with the correct type already existed in the ``template-haskell`` interface, and out of a worry that ``liftQ :: Quasi m => Q a -> m a`` could be confused with ``lift :: Lift a => a -> Q Exp``.

If a user gives a definition of ``runQ`` then all other methods except for ``qRecover`` can be implemented by lifting the method from the ``Q`` instance.
Therefore we would also make all methods of ``Quasi`` except for ``qRunQ`` and ``qRecover`` optional.
This means that libraries that implement ``Quasi`` instances would likely not have to make any changes if a new method is added.

``qRecover`` cannot be implemented in terms of ``qRunQ`` as it includes a mention of the monad in negative position.

As ``Quasi`` is no longer part of the implementation of ``Q``, it technically could be removed from the interface of ``template-haskell``, but we have chosen to retain it for backwards compatibility and because it is useful for user's to use analogously to ``MonadIO``.

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

Despite this, we've tried to maximise backwards compatibility by adding an ``unQ`` function to the interface.

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

Our implementation basically duplicates the ``Quasi`` typeclass as a record of handlers.
We choose to go with a record rather than a typeclass purely for the sake of simplicity.

As we always run ``Q`` computations in monads which are themselves reader monads on top of ``IO``, we can use ``IO`` as our concrete monad.

We would make the following changes in ``GHC.Internal.TH.Monad``::

  -- we create a new type
  data MetaHandlers =
    MetaHandlers
    { mReify :: Name -> IO Info
    , mNewName :: String -> IO Name
    , mRecover :: forall a. Q a -> Q a -> IO a
    ... and so on
    }

  -- we change the definition of Q
  newtype Q a = Q { unQ :: MetaHandlers -> IO a }

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
    qRecover r k = Q $ \handlers -> mRecover handlers r k 
    -- all other methods are just the default

  runQ :: Quasi m => Q a -> m a
  runQ = qRunQ

  -- backwards compatibility for removed unQ field selector
  unQ :: Quasi m => Q a -> m a
  unQ = runQ

Note that ``qRecover`` is a special case. It cannot use the default method like the other methods as we get a value of ``Q`` in a negative position. This is only relevant for user defined instances of ``Quasi``.

We would alter the code for running splices in ``GHC.Tc.Gen.Splice`` and would construct a value of type ``MetaHandlers`` using the existing implementations.
To do so we would defined something like this::

  -- this replaces: runQuasi :: Quasi m => m a -> TcM a
  runQinTcM :: Q a -> TcM a
  runQInTcM (Q m) = IOEnv $ \env -> 
    let
      runInIO :: forall x. TcM x -> IO x
      runInIO (IOEnv n) = n env
    in m runInIO metaHandlersTcM

  metaHandlersTcM :: (forall a. TcM a -> IO a) -> MetaHandlers
  metaHandlersTcM runInIO = MetaHandlers
    { mFail = \str -> runInIO $ fail str
    , mRecover = \r k -> runInIO $ tryTcDiscardingErrs (runQinTcM r) (runQinTcM k)
    ...
    }

You might have noticed above that ``mRecover`` has type ``Q a -> Q a -> IO a`` rather than ``IO a -> IO a -> IO a``. This is because we have access to ``Q a -> TcM a`` when defining ``MetaHandlers TcM``, but we do not have access to ``TcM a -> Q a`` when defining the ``qRecover`` instance for ``Quasi`` in ``template-haskell``, since we are not allowed to depend on ``lib:ghc`` there.

We also have to modify the definition of the external interpreter, which simply proxies messages to the compiler. 

How the implementation satisfies the motivation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This new definition of ``Q`` achieves the goals we have set in
`Motivation <#motivation>`_.

If I wish to add a new method ``reifyCore :: Name -> Q Core`` that can be run in TemplateHaskell splices, then I need only make a change to the compiler. I add a new method to ``MetaHandlers`` in ``ghc-internal``, say ``mReifyCore :: Name -> IO Core`` and give a definition in the compiler. I can then release the compiler without requiring any change to ``template-haskell``. So ``GHC-2`` can be released which is compatible with ``template-haskell-0.1`` (re-using the version numbers from earlier). Then I can independently release ``template-haskell-0.2`` with the new method. Since these definitions are entirely internal to the compiler, I can also backport my patch without worrying about breaking previous versions of ``template-haskell``, so we could make the next minor release in the previous line, ``GHC-1.1`` compatible with the new major version of ``template-haskell``, ``template-haskell-0.2``.

The same sorts of techniques can be used for removing methods from the interface of ``Q`` in ``template-haskell`` or for modifying methods (equivalent to adding and then removing methods).

Our definition still allows end-users to give their own instances of ``Quasi`` and in fact greatly reduces the boilerplate involved as they only need to define ``qRunQ`` and ``qRecover``, the other methods can be derived from these two. They can also stilll run ``Q`` in ``IO`` by using ``runQ``.

Our definition of ``Q`` is now given in terms of a concrete monad, which opens up opportunities for easier optimization. And our choice of a reader over ``IO`` allows us to nicely fit in to the ``bluefin`` / ``effectful`` ecosystem.


