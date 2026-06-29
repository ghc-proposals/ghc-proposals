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

Haskell programmers occasionally need a genuinely top-level, mutable, effectfully-initialized value, for example a global ``IORef`` counter or a process-wide cache. While global variables are well-known to be poor architecture decisions if used liberally, there are perfectly reasonable and principled uses for it in certain scenarios, especially when hidden behind a well-designed API.

The current idiom / workaround is:

::

    foo :: IORef Int
    foo = unsafePerformIO (newIORef 0)
    {-# NOINLINE foo #-}

However, this idiom has a couple sharp edges:

1. ``NOINLINE`` alone is **not sufficient**. It prevents the simplifier from substituting ``foo``'s definition at use sites, but it does not prevent optimizations like CSE.

2. As documented in ``unsafePerformIO``, a polymorphic return type like ``IORef [a]`` is not actually type safe and can cause a core dump.

GHC 9.4 introduced ``{-# OPAQUE #-}``, which is described in the GHC User Guide as:

     Instructs the compiler to ensure that every call of name remains a call of name, and not some name-mangled variant.

This happens to fix sharp edge #1, but it's incidental and not semantically equivalent to what we want here. It's not obvious that preventing name mangling prevents inlining a shared top-level reference.

This proposal adds a small piece of surface syntax, ``%TopLevelIO``, that resolves all of these issues. In practice, it should be equivalent to ``OPAQUE`` + ``unsafePerformIO``, but it provides an explicit semantic tag for this use-case, should ``OPAQUE`` or ``unsafePerformIO`` change implementations.

For additional context and discussion, see the `Top level mutable state <https://wiki.haskell.org/index.php?oldid=64612>`_ page in the Haskell Wiki.

Proposed Change Specification
------------------------------

A new extension ``-XEnableTopLevelIO`` is added. When enabled, a new modifier ``%TopLevelIO`` is made available, reusing the existing type modifier syntax (as used for, e.g., multiplicity annotations). It may only appear in the type signature of a top-level value binding:

::

    foo :: %TopLevelIO (IORef Int)
    foo = newIORef 0

If a binding is annotated with the type ``%TopLevelIO ty``, its RHS must have the type ``IO ty``. But the binding will have the type ``ty`` when used in an expression. The modifier has the following runtime semantics:

* The IO action will run at most once, with its result shared by all users of the binding.

* The IO action is guaranteed to run at some point up to the value being evaluated to WHNF. There are no guarantees when it will run relative to other IO actions, unless the other IO actions are used in the RHS.

* The IO action is guaranteed to not run if the value is never evaluated to WHNF.

The modifier is a property of the binding, not the type. In the example below, ``bar``'s type is inferred to simply be ``IORef Int``, without ``%TopLevelIO``; indeed, the modifier is not needed because the IO is done within ``foo`` and ``bar`` is now using the result of the IO action:

::

    foo :: %TopLevelIO (IORef Int)
    foo = newIORef 0

    -- Inferred type: IORef Int
    bar = foo

Placement
~~~~~~~~~

``%TopLevelIO`` is only allowed as a modifier on the type of a top-level value binding. It is rejected anywhere else, including:

* Partially applied type

  * ``foo :: (%TopLevelIO IORef) Int``
  * ``foo :: %TopLevelIO IORef Int``

* Nested inside another type

  * ``foo :: Maybe (%TopLevelIO (IORef Int))``
  * ``foo :: Int -> %TopLevelIO (IORef Int)``

* In local bindings

  * ``foo = newIORef 0 :: %TopLevelIO (IORef Int)``
  * ``foo = let bar :: %TopLevelIO (IORef Int); bar = _ in bar``
  * ``foo = bar where bar :: %TopLevelIO (IORef Int); bar = _``

* In type synonyms

  * ``type Foo = %TopLevelIO (IORef Int)``

* etc.

Monomorphism restriction
~~~~~~~~~~~~~~~~~~~~~~~~

If the type modified by ``%TopLevelIO`` contains any type variables, it is rejected by the compiler. This prevents unsoundness issues.

::

    -- Compile time error!
    bad :: %TopLevelIO (IORef [a])
    bad = newIORef []

This only applies to free type variables, bound by an implicit ``forall``. Rank-N types are fine, same as with ``unsafePerformIO`` today:

::

    type AnyNum = forall a. Num a => a

    valid :: AnyNum
    valid = unsafePerformIO (pure 123)

    alsoValid :: %TopLevelIO AnyNum
    alsoValid = pure 123

Examples
--------

Global atomic counter, using the `atomic-primops <https://hackage.haskell.org/package/atomic-primops-0.8.8/docs/Data-Atomics-Counter.html>`_ package:

::

    counter :: %TopLevelIO AtomicCounter
    counter = newCounter 0

Process-wide cache, with extra IO actions:

::

    globalConfigCache :: %TopLevelIO (IORef (Map String String))
    globalConfigCache = do
      path <- getEnv "APP_CONFIG"
      contents <- readFile path
      newIORef (parseConfig contents)

Effect and Interactions
------------------------

* **Template Haskell** — Modifiers syntax isn't currently implemented in Template Haskell, so ``-XEnableTopLevelIO`` will not be supported in either TH quotes or in the AST.

* **GHCi** — ``:type foo`` should display the signature as written by the user, including the modifier, for clarity. ``:print foo`` and ordinary evaluation runs the IO action at most once, as usual.

* **No interaction with evaluation order/strictness analysis** — Demand analysis on uses of ``foo`` is unaffected; only the binding ``foo`` itself is protected from CSE/floating, exactly as under plain ``OPAQUE``.

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
---------------------

Implementation Plan
---------------------

Brandon Chinn can implement this.

Endorsements
-------------
