Decorate exceptions with backtrace information
==============================================

.. authors:: Ben Gamari; David Eichmann; Sven Tennie
.. date-accepted::
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/18159
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/330>`_.
.. contents::

Ease localization of runtime errors reported via the synchronous exception mechanism
by attaching backtraces to exceptions.


Motivation
----------
Exceptions are one of the primary mechanisms by which Haskell programs report
errors. However, in contrast to most languages, Haskell provides only few tools for
identifying the source of such errors. This leads to a poor debugging experience
and makes it difficult to monitor systems in production.

While over the past few years GHC has grown a variety of mechanisms for reporting
backtraces (e.g. ``HasCallStack``, ``GHC.Stack.CCS``, and DWARF debug
information), currently we do not have a means to attach such backtraces to
exceptions. The goal of this proposal is to fix this `long-standing
<https://www.youtube.com/watch?v=J0c4L-AURDQ>`_ problem.

We want to ensure that exceptions report provenance information *by
default* without requiring action on the part of the developer. To provide this provenance we leverage
the existing mechanisms for collecting backtraces listed above. Furthermore, we
want to ensure that this information is available for consumption in structured
form by the user program, to allow use by logging libraries (for instance, see
`kaptip-raven #1
<https://github.com/cachix/katip-raven/issues/1#issuecomment-625389463>`_),
automatic error reporting, code analysis tools, and the like.

Proposed Change Specification
-----------------------------

This proposal consists of two largely-independent parts:

1. a new root of the exception hierarchy, ``SomeExceptionWithBacktrace``,
   which replaces ``SomeException`` which currently 
   underlies GHC's exception mechanism. ``SomeExceptionWithBacktrace``
   includes backtrace information and wraps a ``SomeException``.
2. a common representation of backtraces from the above-described mechanisms
3. a facility for choosing which backtrace mechanism(s)
   should be used to collect exception provenance.

We will describe these two parts below.

Adding ``SomeExceptionWithBacktrace``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All exceptions in Haskell are currently represented by the
``Control.Exception.SomeException`` type. In order to extend this while preserving
backwards compatibility, we propose to keep ``SomeException`` as-is but add
``Control.Exception.SomeExceptionWithBacktrace`` "above" it as a wrapper in which to add
backtrace information. Hence all exceptions will be represented by a
``SomeException`` wrapped in a ``SomeExceptionWithBacktrace`` as follows: ::

    data SomeExceptionWithBacktrace
      = SomeExceptionWithBacktrace
        SomeException       -- ^ the exception
        [Backtrace]         -- ^ backtraces

We will leave the details of the representation of the ``Backtrace`` type for
later.

The ``Exception`` class and ``SomeException`` instance will be updated to reflect the new
root of the hierarchy: ::

    class (Typeable e, Show e) => Exception e where
        -- | Represent the exception as 'SomeExceptionWithBacktrace'.
        -- If @e@ isn't already of type 'SomeExceptionWithBacktrace' this usually implies some kind of wrapping.
        toException   :: e -> SomeExceptionWithBacktrace
        
        -- | Extract and cast the exception from its wrapped representation.
        -- If the exception cannot be casted to the expected type then the result is 'Nothing'.
        fromException :: SomeExceptionWithBacktrace -> Maybe e

        -- Default definitions:
        toException e = SomeExceptionWithBacktrace (SomeException e) []
        fromException (SomeExceptionWithBacktrace (SomeException e) _) = cast e

        -- | Render this exception value in a human-friendly manner.
        --
        -- Default implementation: @'show'@.
        --
        -- @since 4.8.0.0
        displayException :: e -> String
        displayException = show

    instance Exception SomeException where
      toException e = SomeExceptionWithBacktrace e []
      fromException (SomeExceptionWithBacktrace e _) = Just e

    instance Exception SomeExceptionWithBacktrace where
        toException se = se
        fromException = Just
        displayException (SomeExceptionWithBacktrace e _) = displayException e

A quick search through ``github.com`` and Hackage packages reveals that the vast
majority of ``Exception`` instances use the default definitions or follow the pattern
described in the `documentation
<https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#t:Exception>`_;
such instances are backwards compatible with this proposed change.

Representing and capturing backtraces
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC has four distinct mechanisms for capturing backtraces, each with
its own backtrace representation:

* ``HasCallStack`` is available in all programs and provides precise backtraces
  but requires modification of the source program.
* the cost-centre profiler (accessible from Haskell with
  ) can provide cost-center stacks, but requires
  the program to be built with the ``-prof`` flag.
* DWARF debug information  can be used
  to provide terse, but still often useful, backtraces with no runtime
  overhead in the non-failing case (although backtrace collection is quite
  slow). However, it is currently not available on some widely-used platforms like
  Windows and MacOS.
* Stack-traces based on Info Table Provenance Entry (IPE) information are provided by
  ``GHC.Stack.CloneStack``, are enabled with ``-finfo-table-map``. This is a
  mechanism with (almost) no runtime overhead, but it contains fewer stack frames
  because it uses return stack frame addresses to provide backtraces.

* ``HasCallStack``:
   * Pros: Can be used on all platforms; provides precise backtraces
   * Cons: Requires manual modification of the source program; runtime overhead
* Cost-centre profiler (via ``GHC.Stack.CCS.getCurrentCCS``)
   * Pros: Can be used on all platforms; fairly precise backtraces
   * Requires profiled executable; runtime overhead; may require manual ``SCC`` pragmas
* DWARF debug information in conjunction with GHC's `built-in stack unwinder
  <https://www.haskell.org/ghc/blog/20200405-dwarf-3.html>`_
   * Pros: No runtime overhead; can trace through foreign code
   * Cons: Highly platform-specific (currently only available on Linux); slow backtrace collection; imprecise backtraces; large binary size overhead
* Info-table provenance (IPE) information (via ``GHC.Stack.CloneStack``)
   * Pros: Can be used on all platforms; no runtime overhead
   * Cons: Large binary size overhead; no visibility into foreign code

All of these backtrace mechanisms have their uses, offering a range of
levels of detail, executable size, and runtime overhead. Given the complementary nature of these mechanisms, GHC
shouldn't dictate which of these mechanisms should be
used to report exception backtraces.  Consequently, our ``Backtrace`` type is
designed to capture all of them: ::

    -- | An exception backtrace.
    --
    -- @since 4.15
    data Backtrace
      = -- | a cost-centre profiler backtrace
        CostCenterBacktrace (Ptr CostCentreStack)
      | -- | a stack from 'GHC.Stack.HasCallStack'
        HasCallStackBacktrace GHC.Stack.CallStack
      | -- | a stack unwinding (e.g. DWARF) backtrace
        ExecutionBacktrace [GHC.ExecutionStack.Location]
      | -- | a backtrace from Info Table Provenance Entries
        IPEBacktrace [StackEntry]

With the machinery described above
GHC could, for instance, provide a variant of ``throwIO`` that
attaches a ``HasCallStack`` backtrace to the thrown exception: ::

    module GHC.IO where

    -- | Throw an exception with a 'Backtrace' gathered by the 'HasCallStackBacktraceMech' mechanism.
    -- If the exception already has backtraces, the new one is added.
    throwIOWithCallStack :: (HasCallStack, Exception e) => e -> IO a
    throwIOWithCallStack e = do
        maybeBt <- collectHasCallStackBacktrace
        let !e' = case maybeBt of
                    Just bt -> addBacktrace bt $ toException e
                    Nothing -> toException e
        IO (raiseIO# e')

We propose that ``GHC.Exception`` provides a family of these functions for
the ``HasCallStack``, cost-center stack, info table provenance (IPE) and execution
stack cases. ::

    throwIOWithCostCenterStack :: Exception e => e -> IO a

    throwIOWithCallStack :: (HasCallStack, Exception e) => e -> IO a

    throwIOWithExecutionStack :: Exception e => e -> IO a

    throwIOWithIPEStack :: Exception e => e -> IO a

We also propose to apply the same ideas to the pure ``throw`` function,
keeping some symmetry between ``GHC.Exception`` and ``GHC.IO``: ::

    module GHC.Exception where

    throwWithCallStack :: HasCallStack => forall (r :: RuntimeRep). forall (a :: TYPE r). forall e.
            Exception e => e -> a

    throwWithIPEStack :: forall (r :: RuntimeRep). forall (a :: TYPE r). forall e.
            Exception e => e -> a

    throwWithCostCenterStack :: forall (r :: RuntimeRep). forall (a :: TYPE r). forall e.
            Exception e => e -> a

    throwWithExecutionStack :: forall (r :: RuntimeRep). forall (a :: TYPE r). forall e.
            Exception e => e -> a

(The prototype implementation showed that these functions do not add much code.)

Pretty Printing Backtraces
~~~~~~~~~~~~~~~~~~~~~~~~~~

To be compliant with the convention that ``Show`` instances should output
valid Haskell code, the ``Show`` instance of ``Backtrace`` delegates to the
instances of the inner types.

However, since backtraces often need to be presented to the user, an
additional pretty printing function will be provided to output the ``Backtrace``
in a more readable form: ::

    module GHC.Exception

    -- | Pretty print a list of 'Backtrace's.
    -- This function should be used to output the backtraces to a terminal.
    -- The format is subject to change. The caller should not depend on it.
    showBacktrace :: Backtrace -> String
    
    showExceptionWithBacktrace :: SomeExceptionWithBacktrace -> String

Selecting the backtrace mechanism
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With the machinery described above, we can now address a common debugging scenario: locating for an exception
thrown by a third-party library. As most Haskell code is legacy code,  likely calls the old ``throw`` and
consequently would not produce a backtrace). For this we propose a pragmatic,
stateful approach to allow the user to select which mechanism(s) should be used
for backtrace collection in ``throw``, ``throwIO`` and similar functions: ::

    module GHC.Exception.Backtrace where

    -- | Which kind of backtrace to collect when an exception is thrown.
    data BacktraceMechanism
      = -- | collect a cost center stacktrace (only available when built with profiling)
        CostCenterBacktraceMech
      | -- | use execution stack unwinding with given limit
        ExecutionStackBacktraceMech
      | -- | collect backtraces from Info Table Provenance Entries
        IPEBacktraceMech
      | -- | use 'HasCallStack'
      HasCallStackBacktraceMech
      deriving (Eq, Show)

    currentBacktraceMechanisms :: IORef [BacktraceMechanism]
    currentBacktraceMechanisms = unsafePerformIO $ newIORef []
    {-# NOINLINE currentBacktraceMechanisms #-}

    -- | Set how 'Control.Exception.throwIO', et al. collect backtraces.
    setDefaultBacktraceMechanisms :: [BacktraceMechanism] -> IO ()
    setDefaultBacktraceMechanisms = writeIORef currentBacktraceMechanisms

    -- | Returns the currently selected 'BacktraceMechanism'.
    getDefaultBacktraceMechanisms :: IO [BacktraceMechanism]
    getDefaultBacktraceMechanisms = readIORef currentBacktraceMechanisms


A ``collectBacktrace`` primitive used by ``throw`` (and likewise ``throwIO``)
simply dispatches to the appropriate backtrace collection scheme as determined
by the currently selected ``BacktraceMechanism``s: ::

    module GHC.Exception.Backtrace where

    -- | Collect a list of 'Backtrace's via all current default 'BacktraceMechanism's.
    -- See 'setDefaultBacktraceMechanisms'
    collectBacktraces :: HasCallStack => IO [Backtrace]
    collectBacktraces = do
        mechs <- getDefaultBacktraceMechanisms
        catMaybes `fmap` mapM collectBacktraces' mechs
      where
        -- | Collect a 'Backtrace' via the given 'BacktraceMechanism'.
        collectBacktraces' :: HasCallStack => BacktraceMechanism -> IO (Maybe Backtrace)
        collectBacktraces' CostCenterBacktraceMech = collectCostCenterBacktrace
        collectBacktraces' ExecutionStackBacktraceMech = collectExecutionStackBacktrace
        collectBacktraces' IPEBacktraceMech = collectIPEBacktrace
        collectBacktraces' HasCallStackBacktraceMech = collectHasCallStackBacktrace


    module GHC.Exception where

    -- | Throw an exception. Exceptions may be thrown from purely
    -- functional code, but may only be caught within the 'IO' monad.
    -- 'Backtrace' backtraces are collected according to the configured
    -- 'BacktraceMechanism's.
    --
    -- WARNING: If you are in an `IO` context you may want to rather use 'throwIO' instead so that your pure code
    -- stays exception-free.
    throw :: HasCallStack => forall (r :: RuntimeRep). forall (a :: TYPE r). forall e.
            Exception e => e -> a
    throw e =
      runRW#
        ( \s0 ->
            let e'@(SomeExceptionWithBacktrace _ bts) = toException e
            in if null bts
                  then case unIO collectBacktraces s0 of
                    (# _, bts' #) ->
                      let e'' = foldr addBacktrace e' bts'
                      in raise# e''
                  else raise# e'
        )

Note that this proposed change to ``throw`` (and likewise ``throwIO``) includes
adding a ``HasCallStack`` constraint. The prototype implementation showed that this
likely does not imply a big performance decrease.

Examples
--------

User programs would typically call ``setGlobalBacktraceMechanisms`` during
start-up to select a backtrace mechanism appropriate to their usage: ::

    main :: IO ()
    main = do
        setGlobalBacktraceMechanisms [HasCallStackBacktrace, ExecutionStackBacktrace]

        -- do interesting things here...

Some other programming languages use environment variables to configure
backtrace reporting (e.g. the Rust runtime enables debugging with
``RUST_BACKTRACE=1``). It would be straightforward to provide a utility (either
in a third-party library or perhaps ``base`` itself) which would configure the
global backtrace mechanism from the environment: ::

    setBacktraceMechanismFromEnv :: IO ()
    setBacktraceMechanismFromEnv =
        getEnv "GHC_BACKTRACE" >>= setGlobalBacktraceMechanisms . parseBacktraceMechanisms

This could then be called during program initialization, providing the ease of
configuration found in other languages. As it could be added at any time,
``setBacktraceMechanismFromEnv`` is not part of the scope of this proposal.


Effect and Interactions
-----------------------

The described mechanism provides users with a convenient means of gaining greater
insight into the sources of exceptions. Currently the runtime system's ``+RTS
-xc`` flag provides an ad-hoc mechanism in the runtime system which relies on the
cost-center profiler. In principle the ``-xc`` mechanism is subsumed by the
mechanism proposed here.

In a later step all ``Exception``s that reach the end of ``main`` could be pretty
printed with their corresponding backtraces.

Costs and Drawbacks
-------------------

While the global backtrace mechanism is convenient, it suffers from the usual
drawbacks associated with global state: it does not compose well and may result
in surprising behavior when manipulated by more than one actor.

This being said, we consider this approach to be a compromise which reflects
the fact that stack traces are primarily a debugging tool and somewhat of a
cross-cutting concern. While a stateless approach would be preferred, we
believe that this compromise is a significant improvement over the status quo.

The Haskell community will have to adapt its code to the new exception structure.
As described in `Adding ``SomeExceptionWithBacktrace```_ the expected impact isn't
very high. This is further discussed in `Migration`_ .

Migration
---------

There was an intense discussion in the comments of the pull request of this
proposal about how to reach two goals
(<https://github.com/ghc-proposals/ghc-proposals/pull/330>):
- Keep the migration costs as low as possible (i.e. most usages should work)
  without any change.
- Get a type checking error when the changed exception structure breaks existing
  code (i.e. no silent changes in behaviour.)

The solution presented in this proposal has been agreed upon by all involved
parties.

``catch``and ``handle`` work with both, ``SomeExceptionWithBacktrace`` and
``SomeException``. This is the main reason for keeping ``SomeException``
as a layer in the exception hierarchy.

The usages of ``throw`` and ``throwIO`` don't have to be changed, too.
In fact, it showed that most submodules of GHC don't need any changes.
Only a few changes were needed to be made to GHC itself (e.g. in ``compiler/``).

Direct calls to ``toException`` and ``fromException``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As the types of ``toException`` and ``fromException`` changed, calls to them might
need to be adjusted to the new types. This is guided by type checking errors.

Usually, when these calls aren't bound to ``SomeException`` specific types, no
change should be needed.

In expressions where ``fromException``'s changed parameter type leads to type 
errors, one common solution is to convert the exception value first with
``toException``.

As an example let's consider the case of GHC's internal `GHC.TopHandler.real_handler` function; to ease compatibility, we may want to avoid changing the `SomeException` argument to `SomeExceptionWithBacktrace`. We can achieve this with a strategically-placed `fromException . toException`:
``GHC.TopHandler.real_handler`` to keep the needed migration effort small. ::

    real_handler :: (Int -> IO a) -> SomeException -> IO a
    real_handler exit se = do
      flushStdHandles -- before any error output
      -- The call to fromException needs to be preceded by a call to
      -- toException. (Original line from master commented out.)
      -- case fromException se of
      case (fromException . toException) se of
          Just StackOverflow -> do
              reportStackOverflow
              exit 2

          ... [Other cases]

Type synonym
~~~~~~~~~~~~

If no pattern matches are needed, compatibility with older compilers can be preserved by defining the `SomeExceptionWithBacktrace` type as a type synonym: ::

    module Control.Monad.Catch where

    #if __GLASGOW_HASKELL__ < 903
    type SomeExceptionWithBacktrace = SomeException
    #endif

    catchAll :: MonadCatch m => m a -> (SomeExceptionWithBacktrace -> m a) -> m a

    handleAll :: MonadCatch m => (SomeExceptionWithBacktrace -> m a) -> m a -> m a

This is only needed when ``SomeExceptionWithBacktrace`` should be used as type
in the program (e.g. to be able to access the backtraces).
As already discussed, in very most cases it is fine to stick with
``SomeException`` which is supported by old versions of GHC and those that
implement this proposal.

In general, it should always be considered if a "down cast" to ``SomeException``
using ``toException`` and ``fromException`` would not be sufficient to solve the
issue at hand. But, as ``SomeException`` does not carry backtraces, these would
be lost.

Type and pattern synonym
~~~~~~~~~~~~~~~~~~~~~~~~

If there are existing pattern matches on the structure of ``SomeException``, a
combination of type and a pattern synonym could be applied. ::

    #if __GLASGOW_HASKELL__ < 903
    type SomeExceptionWithBacktrace = SomeException

    {-# COMPLETE SomeExceptionWithBacktrace #-}
    pattern SomeExceptionWithBacktrace :: forall. SomeException -> () -> SomeException
    pattern SomeExceptionWithBacktrace e unit <- (\x -> ((), x) -> (unit, e))
      where
        SomeExceptionWithBacktrace (SomeException e) _ = SomeException e
    #endif

This should be considered to be a measure of last resort! Please refer to `Alternatives`_
about why this is not a general solution and might break existing code.

The preferred solution should always be to rewrite the code to not pattern
match on the internals of the root exception (``SomeExceptionWithBacktrace``
or ``SomeException``, respectively), but use ``fromException`` and
``toException`` instead.

This combination of type and pattern synonym was successfully applied in prior 
incarnations of the prototype's implementation. Though we advise to better not use
it, we don't want to leave it unmentioned.

Alternatives
------------

The original proposal suggested keeping ``SomeException`` as the root exception
type, changing the constructor to add a ``Maybe Backtrace`` field and a pattern
synonym for backwards compatibility: ::

    data SomeException where
      SomeExceptionWithLocation
        :: forall e. Exception e
        => Maybe Backtrace   -- ^ backtrace, if available
        -> e                 -- ^ the exception
        -> SomeException

    pattern SomeException e <- SomeExceptionWithLocation _ e
      where
        SomeException e = mkSomeExceptionWithLocation e

The problem with this is that the pattern match completeness checker does not
play well with pattern synonyms. Additionally, it may introduce a ``MonadFail``
constraint where one did not exits before. For example, the following would no
longer type check due to the lack of a ``MonadFail m`` constraint: ::

    f :: Monad m => SomeException -> m ()
    f someException = do
      SomeException e <- pure someException   -- Pattern synonym is assumed fallible
      ...

In addition to the runtime-configurable ``setGlobalBacktraceMechanisms``
mechanism described above, GHC could gain support for setting the backtrace
mechanism at compile-time via a compiler flag (this would essentially come down
to GHC emitting a call to ``setGlobalBacktraceMechanisms`` in its start-up
code).

Alternatively, the community might rather choose one of the backtrace
mechanisms discussed above and use this mechanism exclusively in exception
backtraces. However, we suspect that a single mechanism won't be sufficient:

* there have been `previous efforts <https://gitlab.haskell.org/ghc/ghc/issues/17040>`_
  to add ``HasCallStack`` constraints to all partial functions in ``base``. While we
  believe that this is a worthwhile complementary goal, we don't believe that
  ``HasCallStack`` alone can address the full scope of the problem due to its
  invasive nature.
* likewise, the cost center profiler can provide descriptive backtraces but is
  widely regarded as being impractical for use in production environments due
  to its performance overhead.
* native stack unwinding approaches offer stacktraces that are necessarily
  approximate (due to tail calls) and can be harder to interpret but have no
  runtime overhead in the non-failing case.

Yet another design would be to relegate handling and reporting of backtraces
completely to the runtime system. This would avoid the thorny design questions
surrounding adding ``SomeExceptionWithBacktrace`` but we would lose out on many of
the benefits of offering structured backtraces to the user.


Implementation Plan
-------------------

There is an active branch with an implementation of this proposal:
<https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6797>


Endorsements
-------------

* @domenkozar has indicated that the problem addressed by this proposal poses a
  significant challenge for his work in production and that the approach
  presented here would be an improvement over the status quo.
