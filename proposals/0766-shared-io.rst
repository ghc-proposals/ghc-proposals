Top-level shared references via ``%SharedIO``
=============================================

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

Haskell programmers occasionally need a top-level shared mutable reference, for example a global ``IORef`` counter or a process-wide cache. One current workaround is to use ``unsafePerformIO`` + ``NOINLINE``, which allows declaring a top-level CAF that looks pure, but is really hiding IO. In this proposal, we target a smaller subset of functionality: IO that's semantically guaranteed to only run once and cached for the lifetime of the program. This would benefit many programs, even `GHC itself <#102shared-references-in-ghc>`_.

The current workaround (shown below) has a couple sharp edges:

::

    foo :: IORef Int
    foo = unsafePerformIO (newIORef 0)
    {-# NOINLINE foo #-}

1. ``NOINLINE`` alone might not be sufficient. Some people have expressed anecdotal concerns about cases where it doesn't prevent inlining. The lack of clarity around whether ``NOINLINE`` actually works undermines trust in using these idioms in production code.

2. As documented in ``unsafePerformIO``, a polymorphic return type like ``IORef [a]`` is not actually type safe and can cause a core dump.

GHC 9.4 introduced ``{-# OPAQUE #-}``, which is described in the GHC User Guide as:

     Instructs the compiler to ensure that every call of name remains a call of name, and not some name-mangled variant.

This might address concerns with sharp edge #1, but even if it does, it's incidental and not semantically equivalent to what we want here. It's not obvious that preventing name mangling prevents inlining a shared top-level reference.

This proposal adds a new ``%SharedIO`` modifier that semantically guarantees that it will run at most once.

Additional context and discussion:

* `Top level mutable state (Haskell Wiki) <https://wiki.haskell.org/index.php?oldid=64612>`_
* `Section 10.1 Motivating case studies <#101motivating-case-studies>`_

Proposed Change Specification
------------------------------

A new extension ``-XSharedIO`` is added, which enables the following syntax:

::

    -- IO is run exactly once, on first usage.
    %SharedIO
    getFooRef :: IO (IORef Int)
    getFooRef = newIORef 0

The ``%SharedIO`` syntax is reused from the ``-XModifiers`` extension, although it's a hardcoded modifier and not a true usage of the extension, since ``SharedIO`` won't be a valid identifier in scope.

The type of the declaration *must* be IO or a newtype, using the same rules as C FFI. It may also be an unparametrized newtype or type alias:

::

    type MyRef = IO (IORef Int)
    newtype MyRef2 = MyRef2 (IO (IORef Int))

    %SharedIO
    getFooRef :: MyRef
    getFooRef = newIORef 0

    %SharedIO
    getFooRef2 :: MyRef2
    getFooRef2 = MyRef2 <$> newIORef 0

Using this value would be normal IO invocations:

::

    getAndInc :: IO ()
    getAndInc = do
      fooRef <- getFooRef
      n <- atomicModifyIORef' fooRef $ \x -> (x + 1, x + 1)
      print n

    main :: IO ()
    main = getAndInc >> getAndInc

However, it would have different behavior with/without this functionality:

* ``getFooRef`` does not use ``%SharedIO``: prints 1 twice
* ``getFooRef`` uses ``%SharedIO``: prints 1, then 2

Unlike the current ``unsafePerformIO``/``NOINLINE`` idiom, this does not bypass the type system at all, since using the shared reference requires you to be in IO. This means that some current uses of ``unsafePerformIO``/``NOINLINE`` will still be necessary, if the author wishes to use the value in a pure context (with all the footguns that it comes with).

Example implementation
~~~~~~~~~~~~~~~~~~~~~~

One way GHC could implement this is desugaring it to an implementation that works today, e.g.:

::

    -- Original
    %SharedIO
    getFoo :: IO (IORef Int)
    getFoo = newIORef 0

    -- Desugared
    data Box a = Box a
    getFoo_caf :: Box (IORef Int)
    getFoo_caf = (unsafePerformIO . fmap Box) (newIORef 0)
    {-# OPAQUE getFoo_caf #-}
    getFoo :: IO (IORef Int)
    getFoo = let !(Box a) = getFoo_caf in pure a

Alternatively, ``%SharedIO`` could be a new primitive that the RTS handles specially. The benefit of this approach is that the compiler wouldn't have to reverse engineer its own optimization pipeline to get the proper semantics.

Regardless of the implementation, GHC would take ownership over the behavior and users would have the compiler's guarantee that it would behave as expected. This proposal does not make any decisions about how compilers should implement the behavior (e.g. MicroHs might implement this differently than GHC).

Monomorphism restriction
~~~~~~~~~~~~~~~~~~~~~~~~

A ``%SharedIO`` declaration containing any type variables will be rejected by the compiler. This prevents unsoundness issues.

::

    -- Compile time error!
    %SharedIO
    bad :: IO (IORef [a])
    bad = newIORef []

This only applies to free type variables, bound by an implicit ``forall``. Rank-N types are fine, same as with ``unsafePerformIO`` today:

::

    type AnyNum = forall a. Num a => a

    valid :: IORef AnyNum
    valid = unsafePerformIO (newIORef 123)

    %SharedIO
    alsoValid :: IO (IORef AnyNum)
    alsoValid = newIORef 123

Examples
--------

Global atomic counter from the `atomic-primops <https://hackage.haskell.org/package/atomic-primops>`_ package:

::

    -- Already provided by `atomic-primops`
    newCounter :: Int -> IO AtomicCounter

    -- User code
    %SharedIO
    getCounter :: IO AtomicCounter
    getCounter = newCounter 0

Global random number generator, from the `random <https://hackage.haskell.org/package/random>`_ package:

::

    -- In `random`, not exported
    %SharedIO
    theStdGen :: IO (IORef StdGen)
    theStdGen = newIORef . StdGen =<< initSMGen


Effect and Interactions
------------------------

* **Template Haskell** — Modifiers syntax isn't currently implemented in Template Haskell, so ``%SharedIO`` will not be supported in either TH quotes or in the AST.

* **Scoped thread locals** (`GHC Proposal #751 <https://github.com/ghc-proposals/ghc-proposals/pull/751>`_) — #751 is related to, but orthogonal to, this proposal. One of its examples even has an instance of a shared top-level value that could take advantage of this proposal.

  ::

    %SharedIO
    getTraceKey :: IO (Key [String])
    getTraceKey = newKey

Costs and Drawbacks
--------------------

* A new reserved modifier name (``SharedIO``)

Alternatives
------------

* **Do nothing; rely on workarounds** — The status quo. There are ways to implement this in user-land, such as [Sylvain's workaround](https://gist.github.com/brandonchinn178/0a15b413f306e85f019b4fe9b2029f28), but without changing the language, there's no guarantee GHC (or another compiler) would respect the implementation.

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

    -- Provided by library
    newtype Counter = Counter (IO AtomicCounter)

    -- User of library
    %SharedIO
    requestsCounter :: Counter
    requestsCounter = Counter.register "requests_total" "Number of requests handled"

    myHandler :: Request -> IO Response
    myHandler req = do
      Counter.inc requestsCounter
      ...

For more details, see `this issue <https://github.com/bitnomial/prometheus/issues/59>`_ in the ``prometheus`` library.

Shared references in GHC
~~~~~~~~~~~~~~~~~~~~~~~~

GHC itself is not an exception to shared mutable references! Just in the compiler alone, as of GHC 9.14:

* `GHC.Linker.Loader.gccSearchDirCache <https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.14/compiler/GHC/Linker/Loader.hs#L1550-L1555>`_
* `GHC.SysTools.Terminal.stderrSupportsAnsiColors <https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.14/compiler/GHC/SysTools/Terminal.hs#L20-L24>`_
* `GHC.Utils.GlobalVars <https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.14/compiler/GHC/Utils/GlobalVars.hs>`_
* `GHc.Utils.Panic.signalHandlersRefCount <https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.14/compiler/GHC/Utils/Panic.hs#L227-L235>`_
