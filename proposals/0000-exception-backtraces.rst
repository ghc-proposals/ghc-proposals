Decorate exceptions with backtrace information
==============================================

.. author:: Ben Gamari
.. date-accepted::
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/18159
.. implemented:: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3236
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/330>`_.
.. contents::

Ease localization of errors reported via the synchronous exception mechanism
by attaching backtraces to exceptions.


Motivation
----------
Exceptions are one of the primary mechanisms by which Haskell programs report
errors. However, in contrast to most languages, Haskell provides few tools for
identifying the source of such errors. This leads to a poor debugging experience
and makes it difficult to monitor systems in production.

While GHC has grown a variety of mechanisms for reporting backtraces over the
past few years (e.g. ``HasCallStack``, ``GHC.Stack.CCS``, and DWARF debug
information), currently we do not have a story for attaching such backtraces to
exceptions. This proposal endeavors to fix this `long-standing
<https://www.youtube.com/watch?v=J0c4L-AURDQ>`_ problem.

In short, we want to ensure that exceptions report provenance information *by
default* without requiring action on the part of the ``throw``-er by leveraging
the existing mechanisms for collecting backtraces listed above. Furthermore, we
want to ensure that this information is available for consumption in structured
form by the user program, to allow use by logging libraries (for instance, see
`kaptip-raven #1
<https://github.com/cachix/katip-raven/issues/1#issuecomment-625389463>`_ and
the like.

Proposed Change Specification
-----------------------------

This proposal consists of two largely independent pieces:

1. a new root of the exception hierarchy, ``SomeExceptionWithBacktrace``, which
   underlies GHC's exception mechanism. ``SomeExceptionWithBacktrace``
   accommodates backtrace information and wraps a ``SomeException`` (the current
   root of the exception hierarchy).
2. a facility for choosing how backtraces should be collected

We will consider these two pieces in turn.

Adding ``SomeExceptionWithBacktrace``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All exceptions in Haskell are currently represented by the
``Control.Exception.SomeException`` type. In order to extend this while keeping
backwards compatibility, we propose to keep ``SomeException`` as is but add
``Control.Exception.SomeExceptionWithBacktrace`` as a wrapper in which to attach
backtrace information. Hence all exceptions will be represented by a
``SomeException`` wrapped in a ``SomeExceptionWithBacktrace`` as follows: ::

    data SomeExceptionWithBacktrace
      = SomeExceptionWithBacktrace
          [Backtrace]       -- ^ backtraces
          SomeException     -- ^ the exception

We will leave the details of the representation of the ``Backtrace`` type for
later.

The ``Exception`` class and ``SomeException`` must be updated to reflect the new
root of the hierarchy: ::

    class (Typeable e, Show e) => Exception e where
      toException   :: e -> SomeExceptionWithBacktrace
      fromException :: SomeExceptionWithBacktrace -> Maybe e

      toException e = SomeExceptionWithBacktrace [] (SomeException e)
      fromException (SomeExceptionWithBacktrace _ (SomeException e)) = cast e

      displayException :: e -> String
      displayException = show

    instance Exception SomeExceptionWithBacktrace where
      toException = id
      fromException = Just

    instance Exception SomeException where
      toException = SomeExceptionWithBacktrace []
      fromException (SomeExceptionWithBacktrace _ e) = e

A quick search through github.com and cabal packages reveals that the vast
majority of ``Exception`` instances are fully default and/or follow the pattern
described in the `documentation
<https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#t:Exception>`_.
Such instances are backwards compatible with this proposed change.

Representing and capturing backtraces
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC has at least three distinct mechanisms for capturing backtraces, each with
their own backtrace representation:

* ``HasCallStack`` is available in all programs, but requires modification of
  the source program
* the cost-centre profiler (accessible from Haskell with
  ``GHC.Stack.CCS.getCurrentCCS``) can provide cost-center stacks
* DWARF debug information in conjunction with GHC's `built-in stack unwinder
  <https://www.haskell.org/ghc/blog/20200405-dwarf-3.html>`_ can be used
  to provide terse (but nevertheless useful) backtraces with no runtime
  overhead in the non-failing case (although backtrace collection is quite
  slow)

All of these backtrace options have their time and place, offering a range of
levels of detail, executable size, and runtime overhead. GHC, being a compiler,
shouldn't be in the business of dictating which of these mechanisms should be
used to report exception backtraces.  Consequently, our ``Backtrace`` type is
designed to capture them all: ::

    -- | An exception backtrace.
    data Backtrace
      = Backtrace
        (Maybe (Ptr GHC.Stack.CCS.CostCentreStack))
        -- ^ a cost center profiler backtrace
        (Maybe GHC.Stack.CallStack)
        -- ^ a stack from 'GHC.Stack.HasCallStack'
        (Maybe [GHC.ExecutionStack.Location])
        -- ^ a stack unwinding (e.g. DWARF) backtrace

    instance Show Backtrace

With such a type we can easily write a variant of ``throwIO`` that, for
instance, attaches a ``HasCallStack`` backtrace: ::

    -- | Throws an exception with a 'HasCallStack' backtrace.
    throwIOWithCallStack :: (Exception e, HasCallStack) => e -> IO a
    throwIOWithCallStack exc = throw ?callStack exc

We propose that ``GHC.Exception`` provide a family of these functions for
the ``HasCallStack``, cost-center stack, and execution stack cases.

Making backtraces ubiquitous
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

While these functions are useful building blocks, they do not
address the most common debugging scenario: searching for an exception
thrown by a third-party library (which likely calls the old ``throw`` and
consequently would not produce a backtrace). For this we propose a pragmatic,
stateful approach to allow the user to select which mechanism(s) should be used
for backtrace collection in ``throw`` and similar functions: ::

    module GHC.Exception.Backtrace where

    -- | Which kind of backtrace to collect when an exception is thrown.
    data BacktraceMechanism
      = NoBacktrace
      | CollectCostCenterBacktrace
      | HasCallStackBacktrace
      | ExecutionStackBacktrace

    -- | Set the global backtrace mechanism(s).
    -- (this state would be represented internally as a simple global IORef.
    setGlobalBacktraceMechanisms :: [BacktraceMechanism] -> IO ()

A ``collectBacktrace`` primitive used by ``throw``
simply dispatches to the appropriate backtrace collection scheme as determined
by the currently selected ``BacktraceMechanism``s: ::

    collectBacktrace :: HasCallStack => IO Backtrace
    collectBacktrace = ...

    throw :: (Exception e, HasCallStack) => e -> SomeException
      throw e = unsafePerformIO $ do
          bt <- collectBacktrace
          let SomeExceptionWithBacktrace bts e' = toException e
          return (raise# (SomeExceptionWithBacktrace (bt:bts) e'))

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
configuration found in other languages.


Effect and Interactions
-----------------------

This mechanism provides users with a convenient means of gaining greater
insight into the sources of exceptions. Currently the runtime system's ``+RTS
-xc`` flag provides an ad-hoc mechanism in the runtime system which relies on the
cost-center profiler. In principle the ``-xc`` mechanism is subsumed by the
mechanism proposed here.


Costs and Drawbacks
-------------------

While the global backtrace mechanism is convenient, it suffers from the usual
drawbacks associated with global state: it does not compose well and may result
in surprising behavior when manipulated by more than one actor.

This being said, we consider this approach to be a compromise which reflects
the fact that stack traces are primarily a debugging tool and somewhat of a
cross-cutting concern. While a stateless approach would be preferred, we
believe that this compromise is a significant improvement over the status quo.

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

* there have been `previous efforts <https://gitlab.haskell.org/ghc/ghc/issues/17040>`_ to add ``HasCallStack``
  constraints to all partial functions in ``base``. While we believe that this is
  a worthwhile complementary goal, we don't believe that ``HasCallStack`` alone
  can address the full scope of the problem due to its invasive nature.
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


Unresolved Questions
--------------------

Rather than collecting backtraces on ``throw`` functions, the backtrace can be collected on construction of the
``SomeExceptionWithBacktrace``. Currently nearly all exception values are constructed via the
``toException`` method of the ``Exception`` typeclass. However, the type of
this method lacks a ``HasCallStack`` constraint, meaning that ``HasCallStack``
backtraces will be largely useless. There are at least two ways of addressing
this issue:

* Add a ``HasCallStack`` constraint to ``toException``, incurring potentially
  unnecessary runtime cost and changing the type of a fairly widely used
  function (albeit in a backwards compatible way)

* Teach the ``throw`` functions to add a backtrace to the exception
  returned by ``toException`` if one is not present. That is what the current
  proposal suggests.

The resulting backtraces may be different depending on where they were
collected. Is there some advantage to collecting on construction rather than on
throw?


Implementation Plan
-------------------
@bgamari has a branch in progress which sketches an implementation.

Endorsements
-------------

* @domenkozar has indicated that the problem addressed by this proposal poses a
  significant challenge for his work in production and that the approach
  presented here would be an improvement over the status quo.
