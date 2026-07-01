Top-level IO-initialized bindings via ``%TopLevelIO``
======================================================

.. author:: Brandon Chinn
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/766>`_
.. sectnum::
.. contents::

Motivation
----------

Haskell programmers occasionally need a top-level, effectfully-initialized value, for example a global ``IORef`` counter or a process-wide cache. While global variables are well-known to be poor architecture decisions if used liberally, there are perfectly reasonable and principled uses for it in certain scenarios, especially when hidden behind a well-designed API. Even `GHC itself <#102top-level-vars-in-ghc>`_ uses top-level IO references!

Standard guidance has been to initialize values in proper IO (e.g. the ``main`` function) and passed through with a Reader monad stack or effects, implicit parameters, or manually plumbed through function arguments. But this is unergonomic and violates the principle of abstraction: the ``main`` function / monad environment / entire call stack now needs to know about all the values used everywhere in the application.

The current idiom for creating a top-level effectfully-initialized value is:

::

    foo :: IORef Int
    foo = unsafePerformIO (newIORef 0)
    {-# NOINLINE foo #-}

However, this idiom has a couple sharp edges:

1. ``NOINLINE`` alone might not be sufficient. Some people have expressed anecdotal concerns about cases where it doesn't prevent inlining. The lack of clarity around whether ``NOINLINE`` actually works undermines trust in using these idioms in production code.

2. As documented in ``unsafePerformIO``, a polymorphic return type like ``IORef [a]`` is not actually type safe and can cause a core dump.

GHC 9.4 introduced ``{-# OPAQUE #-}``, which is described in the GHC User Guide as:

     Instructs the compiler to ensure that every call of name remains a call of name, and not some name-mangled variant.

This might address concerns with sharp edge #1, but even if it does, it's incidental and not semantically equivalent to what we want here. It's not obvious that preventing name mangling prevents inlining a shared top-level reference.

This proposal adds a new ``%TopLevelIO`` modifier that makes a semantic guarantee that it will run at most once and not inlined at any use-sites. Declarations with this modifier will run in a new ``TopLevelIO`` monad that only exposes operations that are safe to run at the top level.

Additional context and discussion:

* `Top level mutable state (Haskell Wiki) <https://wiki.haskell.org/index.php?oldid=64612>`_
* `Section 10.1 Motivating case studies <#101motivating-case-studies>`_

Proposed Change Specification
------------------------------

A new extension ``-XEnableTopLevelIO`` is added, which enables the following syntaxes:

::

    -- IO is run exactly once, on first usage.
    %TopLevelIO
    foo :: IORef Int
    foo <- TopLevelIO.newIORef 0

    -- Equivalent to above
    %TopLevelIO
    foo :: IORef Int <- TopLevelIO.newIORef 0

The ``%TopLevelIO`` syntax is reused from the ``-XModifiers`` extension, although it's a hardcoded modifier and not a true usage of the extension, since ``TopLevelIO`` won't be a valid identifier in scope.

Only a declaration modified with ``%TopLevelIO`` will be able to use this new top-level ``<-`` syntax. This syntax is borrowed from do-notation to reuse the concept of the RHS being a monadic value and the LHS being the "unwrapped" value.

``TopLevelIO`` will be a module the user must import (preferably qualified) with the restricted set of functions allowed to run at the top-level. The RHS must be a value of type ``TopLevelIO ty``, where the ``TopLevelIO`` monad comes from the ``TopLevelIO`` module. See `Section 2.1 TopLevelIO module <#21toplevelio-module>`_.

This construct has the following runtime semantics:

* The action will run at most once, with its result shared by all users of the binding.
* The action will run when the value is first evaluated to WHNF.
* The action is guaranteed to not run if the value is never evaluated to WHNF.

TopLevelIO module
~~~~~~~~~~~~~~~~~

This API is directly inspired by `ACIO <https://wiki.haskell.org/index.php?oldid=64612#Proposal_2(c):_Use_a_new_monad>`_ as described in the Haskell Wiki. We rename it to align the name with the modifier, and also to be more understandable.

::

    -- Constructor not exported
    --
    -- Intentionally NOT a newtype over IO so that FFI can't do arbitrary C
    -- operations within 'TopLevelIO'
    newtype TopLevelIO a = TopLevelIO (forall s. ST s a)

    instance Functor TopLevelIO
    instance Applicative TopLevelIO
    instance Monad TopLevelIO

    toIO :: TopLevelIO a -> IO a

    runST :: (forall s. ST s a) -> TopLevelIO a

    unsafeRunIO :: IO a -> TopLevelIO a

    newIORef :: a -> TopLevelIO (IORef a)

    newMVar :: a -> TopLevelIO (MVar a)

    emptyMVar :: TopLevelIO (MVar a)

    newTVar :: a -> TopLevelIO (TVar a)

The primary laws ``TopLevelIO`` should uphold are:

* ``u >> v === v``

  * Explanation #1: No side effects are indirectly observable
  * Explanation #2: If the result is unneeded, side effects need not be performed

* ``(u >>= \a -> v >>= \b -> w) === (v >>= \b -> u >>= \a -> w)``

  * Explanation: Actions are reorderable with no observable differences

We provide ``unsafeRunIO`` because it's useful for C FFI actions that the user has manually verified to be safe. Regardless, it's always possible for someone to use ``unsafeCoerce`` to run arbitrary IO in ``TopLevelIO``, so it wouldn't be any safer without this function.

Monomorphism restriction
~~~~~~~~~~~~~~~~~~~~~~~~

A ``%TopLevelIO`` declaration containing any type variables will be rejected by the compiler. This prevents unsoundness issues.

::

    -- Compile time error!
    %TopLevelIO
    bad :: IORef [a]
    bad <- TopLevelIO.newIORef []

This only applies to free type variables, bound by an implicit ``forall``. Rank-N types are fine, same as with ``unsafePerformIO`` today:

::

    type AnyNum = forall a. Num a => a

    valid :: AnyNum
    valid = unsafePerformIO (pure 123)

    %TopLevelIO
    alsoValid :: AnyNum
    alsoValid <- pure 123

Examples
--------

Global atomic counter from the `atomic-primops <https://hackage.haskell.org/package/atomic-primops>`_ package:

::

    -- Provided by `atomic-primops`
    newTopLevelCounter :: Int -> TopLevelIO AtomicCounter
    newTopLevelCounter i = TopLevelIO.runST $ do
      arr <- Prim.newByteArray INT_SIZE_IN_BYTES
      Prim.writeByteArray arr 0 i
      let Prim.ByteArray arr' = arr
      pure (AtomicCounter arr')

    -- User code
    %TopLevelIO
    counter :: AtomicCounter
    counter <- newTopLevelCounter 0

Global random number generator, from the `random <https://hackage.haskell.org/package/random>`_ package:

::

    -- In `random`, not exported
    %TopLevelIO
    theStdGen :: IORef StdGen
    theStdGen <- do
      seed <- TopLevelIO.unsafeRunIO initSMGen
      TopLevelIO.newIORef (StdGen seed)


Effect and Interactions
------------------------

* **Template Haskell** — Modifiers syntax isn't currently implemented in Template Haskell, so ``-XEnableTopLevelIO`` will not be supported in either TH quotes or in the AST.

* **No interaction with evaluation order/strictness analysis** — Demand analysis on uses of ``foo`` is unaffected; only the binding ``foo`` itself is protected from CSE/floating, exactly as under plain ``OPAQUE``.

* **Scoped thread locals** (`GHC Proposal #751 <https://github.com/ghc-proposals/ghc-proposals/pull/751>`_) — #751 is related to, but orthogonal to, this proposal. One of its examples even has an instance of top level IO that could take advantage of this proposals.

  ::

    %TopLevelIO
    traceKey :: Key [String]
    traceKey <- TopLevelIO.newKey
    -- TopLevelIO.newKey would be implemented as unsafeRunIO newKey.
    -- It uses atomicModifyIORef, so it can't be implemented directly in TopLevelIO,
    -- but newKey as an API is safe in TopLevelIO because:
    --   1. If the key is never used, not bumping the counter is safe, and
    --   2. If newKey is reordered with any other IO action, it might store a different ID, but
    --      it's not observable in any meaningful way

Costs and Drawbacks
--------------------

* A new reserved modifier name (``TopLevelIO``) and a new check for the modifier in all the locations that ``OPAQUE`` is currently used

* Two ways to write "the same thing" now exist (hand-written ``unsafePerformIO``/``OPAQUE``, and ``%TopLevelIO``). This is mitigated by ``%TopLevelIO`` being strictly safer, shorter, and more semantically descriptive, so it is expected to dominate in new code over time.

Alternatives
------------

* **Do nothing; rely on documentation** — The status quo. Does not address the core problem that the correct idiom (``OPAQUE``, not ``NOINLINE``) is hard to discover and easy to get wrong.

* **Add a TOPLEVEL_IO pragma instead of using modifiers**

  * e.g. ``foo :: IORef Int; foo = unsafePerformIO $ newIORef 0; {-# TOPLEVEL_IO foo #-}``
  * ``unsafePerformIO`` would be required again, since pragmas shouldn't affect type-checking
  * Would still error on non-monomorphic type, in addition to everything ``OPAQUE`` does
  * ``unsafePerformIO`` would now be misleading, since the ``TOPLEVEL_IO`` pragma would make it safe

* Some other option documented in `Top-level mutable state <https://wiki.haskell.org/index.php?oldid=64612>`_

Unresolved Questions
--------------------

Implementation Plan
-------------------

Brandon Chinn can implement this.

Endorsements
------------

Appendix
--------

Motivating case studies
~~~~~~~~~~~~~~~~~~~~~~~

Observability
^^^^^^^^^^^^^

When instrumenting an application with metrics and observability, it's useful to be able to drop in instrumentation without rearchitecting the entire system to pass it through. This is even a guiding principle in `Prometheus's guide for writing client libraries <https://prometheus.io/docs/instrumenting/writing_clientlibs/>`_:

    These should be primarily used as file-static variables, that is, global variables defined in the same file as the code they’re instrumenting. The client library SHOULD enable this. The common use case is instrumenting a piece of code overall, not a piece of code in the context of one instance of an object. Users shouldn’t have to worry about plumbing their metrics throughout their code, the client library should do that for them (and if it doesn’t, users will write a wrapper around the library to make it "easier" - which rarely tends to go well).

    There MUST be a default CollectorRegistry, the standard metrics MUST by default implicitly register into it with no special work required by the user. 

The ideal API would be:

::

    %TopLevelIO
    requestsCounter :: Counter
    requestsCounter <- Counter.register "requests_total" "Number of requests handled"

    myHandler :: Request -> IO Response
    myHandler req = do
      Counter.inc requestsCounter
      ...

For more details, see `this issue <https://github.com/bitnomial/prometheus/issues/59>`_ in the ``prometheus`` library.

Top level vars in GHC
~~~~~~~~~~~~~~~~~~~~~

GHC itself is not an exception to top-level IO variables! Just in the compiler alone, as of GHC 9.14:

* `GHC.Data.FastString.stringTable <https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.14/compiler/GHC/Data/FastString.hs#L381-L406>`_
* `GHC.Linker.Loader.gccSearchDirCache <https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.14/compiler/GHC/Linker/Loader.hs#L1550-L1555>`_
* `GHC.SysTools.Terminal.stderrSupportsAnsiColors <https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.14/compiler/GHC/SysTools/Terminal.hs#L20-L24>`_
* `GHC.Utils.GlobalVars <https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.14/compiler/GHC/Utils/GlobalVars.hs>`_
* `GHc.Utils.Panic.signalHandlersRefCount <https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.14/compiler/GHC/Utils/Panic.hs#L227-L235>`_
* `GHC.StgToJS.Object headers <https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.14/compiler/GHC/StgToJS/Object.hs#L138-L147>`_
