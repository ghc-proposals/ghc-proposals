Decorate exceptions with backtrace information
==============================================

.. authors:: Ben Gamari; David Eichmann; Sven Tennie
.. date-accepted::
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/18159
.. implemented:: in-progress <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8869>
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

This proposal consists of three largely-independent parts:

1. augmenting the current root of the exception hierarchy,
   ``SomeException``, with additional metadata in the form of an
   ``ExceptionContext``.
2. the introduction of annotation types to capture backtraces from the
   above-described backtrace-collection mechanisms
3. a facility for choosing which backtrace mechanism(s)
   should be used to collect exception provenance.

We will describe these parts below.

Introducing ``ExceptionContext``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All exceptions in Haskell are currently represented by the
``Control.Exception.SomeException`` type which captures the exception along
with its ``Exception`` dictionary: ::

    data SomeException where
        SomeException :: forall a. (Exception a)
                      => a -> SomeException

Such a ``SomeException`` value carries all of the information needed to catch,
identify, and display an exception, but lacks any generic facility for encoding
relevant metadata about the exception.

As a first step towards attaching backtraces to exceptions, we propose to extend
``SomeException`` to carry an additional "context" value, consisting of a collection
of dynamically-typed annotations. The dynamic nature of these annotations
allows the mechanism to handle not just backtraces but any arbitrary
context-relevant value which may be inspected by a handler for analysis and logging.
To avoid breaking existing users of ``SomeException``, we introduce this
context as an implicit parameter constraint:  ::

    data SomeException where
        SomeException :: forall a. (Exception a, ?exceptionContext :: ExceptionContext)
                      => a -> SomeException

    data ExceptionContext = ExceptionContext [SomeExceptionAnnotation]

    -- | Contexts are merged by concatenation
    instance Semigroup ExceptionContext
    instance Monoid ExceptionContext

Following the example of the ``Exception`` class, we propose that exception
annotations implement a minimal typeclass which provides ``Typeable`` evidence
and the ability to render the annotation to a ``String``: ::

    data SomeExceptionAnnotation where
        SomeExceptionAnnotation :: forall a. (ExceptionAnnotation a)
                                => a -> SomeExceptionAnnotation

    class Typeable a => ExceptionAnnotation a where
        displayExceptionAnnotation :: a -> String
        
        default displayExceptionAnnotation :: (Show a) => a -> String
        displayExceptionAnnotation = show

To allow users to populate this new annotation field we propose that the
``Exception`` class be extended with a new ``toExceptionWithContext`` method: ::

    class (Typeable e, Show e) => Exception e where
        -- These are unchanged:
        toException            :: e -> SomeException
        fromException          :: SomeException -> Maybe e
        displayException       :: e -> String

        -- This is new:
        toExceptionWithContext :: e -> ExceptionContext -> SomeException
        toExceptionWithContext e ?exceptionContext = SomeException e

        -- toException is implemented in terms of toExceptionWithContext
        toException e = toExceptionWithContext e mempty

The ``Exception`` instance for ``SomeException`` would accumulate and display
contexts: ::

    instance Exception SomeException where
        -- This is unchanged:
        toException se = se
        fromException = Just

        -- toExceptionWithContext *adds* context to an existing SomeException:
        toExceptionWithContext se ctxt = addExceptionContext ctxt se

        -- displayException shows context after the exception itself:
        displayException (SomeException e) =
            displayException e ++ "\n" ++ displayExceptionContext ?exceptionContext

    displayExceptionContext :: ExceptionContext -> String
    displayExceptionContext = ...

Since the ``SomeException``'s ``displayException`` implementation is used to
by GHC's top-level exception handler to display uncaught exceptions, this
change carries the consequence that uncaught exceptions will have their context
automatically printed as part of the error message presented to the user.

To make context-carrying exceptions easier to work with, we propose to
introduce the following combinators: ::

    -- In Control.Exception:
    exceptionContext :: SomeException -> ExceptionContext
    exceptionContext (SomeException _) = ?exceptionContext

    -- | Add the given 'ExceptionContext' to an exception.
    addExceptionContext :: ExceptionContext -> SomeException -> SomeException

    -- | Add the given 'ExceptionContext' to any exception thrown by the given
    -- action.
    withExceptionContext :: ExceptionContext -> IO a -> IO a
    withExceptionContext ctxt action = 
        catch action $ \(e :: SomeException) ->
           throwIO (addExceptionContext ctxt e)

Representing and capturing backtraces
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC currently has four distinct mechanisms for capturing backtraces, each with
its own backtrace representation:

* ``HasCallStack``:
   * Pros: Can be used on all platforms; provides precise backtraces
   * Cons: Requires manual modification of the source program; runtime overhead
* Cost-centre profiler (via ``GHC.Stack.CCS.getCurrentCCS``):
   * Pros: Can be used on all platforms; fairly precise backtraces
   * Requires profiled executable (``-prof``); runtime overhead; may require
     manual ``SCC`` pragmas
* DWARF debug information in conjunction with GHC's `built-in stack unwinder <https://www.haskell.org/ghc/blog/20200405-dwarf-3.html>`_:
   * Pros: No runtime overhead; can trace through foreign code
   * Cons: Highly platform-specific (currently only available on Linux); slow
     backtrace collection; imprecise backtraces; large binary size overhead
     (built with ``-g3``)
* Info-table provenance (IPE) information (via ``GHC.Stack.CloneStack``):
   * Pros: Can be used on all platforms; no runtime overhead
   * Cons: Large binary size overhead; no visibility into foreign code; must be
     built with ``-finfo-table-map``

All of these backtrace mechanisms have their uses, offering a range of levels
of detail, executable size, and runtime overhead. Given the complementary
nature of these mechanisms, GHC should not dictate which of these mechanisms
should be used to report exception backtraces. Consequently, we use the
above-described context mechanism to allow backtraces from any of these
mechanisms to be captured attached to exceptions.

Specifically, we propose to introduce the following set of exception
annotations: ::

    -- | A backtrace from the cost-centre profiler.
    data CostCentreBacktrace = CostCentreBacktrace { ... }
    instance ExceptionAnnotation CostCentreBacktrace

    -- | A backtrace using the native (e.g. DWARF-based) stack unwinder
    data ExecutionStackBacktrace = ExecutionStackBacktrace { ... }
    instance ExceptionAnnotation ExecutionStackBacktrace

    -- | A backtrace from GHC's Haskell stack unwinder and info-table
    -- provenance map.
    data InfoProvBacktrace = InfoProvBacktrace { ... }
    instance ExceptionAnnotation InfoProvBacktrace

    -- | A backtrace from HasCallStack evidence.
    data HasCallStackBacktrace = HasCallStackBacktrace { ... }
    instance ExceptionAnnotation HasCallStackBacktrace

Handling of rethrowing
^^^^^^^^^^^^^^^^^^^^^^

One pattern frequently seen in Haskell programs is *rethrowing*. Typically this
takes the form of catching one type of exception and throwing in its place
another exception more specific to the application domain. For instance, ::

    data MyAppError = MissingConfigurationError | ...

    readFile "my-app.conf" `catch` $ \ (ioe :: IOError) ->
        if isDoesNotExistError ioe
          then throwIO MissingConfigurationError
          else throwIO ioe

This pattern can be problematic in the presence of exception context: the
exception thrown by the handler lacks any of the context attached to the
original ``IOError``, including any backtraces.

While in some select cases dropping context may be desireable (e.g. to avoid
exposing implementation details unnecessarily to the user), in general this
proposal seeks to make exception provenance information ubiquitous and
reliable. Consequently, we propose to that ``catch`` and ``handle`` be modified
to preserve exception context when an exception is thrown from a handler.


Selecting backtrace mechanisms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With the machinery described above, we can now address a common debugging
scenario: locating the origin of an exception thrown by a third-party library.
By far the most common means of throwing exceptions are ``throw``, ``throwIO``,
``error``, and ``undefined``, none of which have any notion of backtrace collection.
This raises the question of how the user should select which of the above
mechanism(s) these functions should use to collect their backtrace.

For this we propose a pragmatic, stateful approach to allow the user to enable
individual mechanism(s) should be used for backtrace collection in ``throw``,
``throwIO`` and similar functions: ::

    module GHC.Exception.Backtrace
        ( enableMechanism, BacktraceMechanism(..) ) where

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

    -- | Enable the given 'BacktraceMechanism' to be used when
    -- 'Control.Exception.throwIO', et al. collect backtraces.
    enableMechanism :: BacktraceMechanism -> IO ()
    enableMechanism = ...
        -- Internally this would be mutate program-global state

    disableMechanism :: BacktraceMechanism -> IO ()
    disableMechanism = ...

A ``collectBacktrace`` primitive used by ``throwWithContext``
simply dispatches to the currently-selected ``BacktraceMechanism``\ s: ::

    module GHC.Exception.Backtrace where

    -- | Collect an 'ExceptionContext' containing backtraces from all enabled
    -- 'BacktraceMechanism's.
    collectBacktraces :: HasCallStack => IO ExceptionContext
    collectBacktraces = do
        mechs <- readIORef currentBacktraceMechanisms
        mconcat `fmap` mapM collectBacktrace mechs

    -- | Collect a 'Backtrace' via the given 'BacktraceMechanism'.
    collectBacktrace :: HasCallStack => BacktraceMechanism -> IO ExceptionContext


    module GHC.Exception where

    -- | Throw an exception. Exceptions may be thrown from purely
    -- functional code, but may only be caught within the 'IO' monad.
    -- Backtraces are collected using the backtrace mechanisms selected by
    -- 'GHC.Exception.Backtrace.enabledBacktraceMechanisms'.
    throwWithContext :: forall e a. (HasCallStack, Exception e)
                     => e -> ExceptionContext -> a
    throwWithContext e ctxt = do
        -- (implementation simplified for clarity)
        backtraces <- collectBacktraces
        raise# (toExceptionWithContext (ctxt <> backtraces) e)

Note that in order to provide ``HasCallStack`` backtraces we propose that a
``HasCallStack`` constraint be added to ``throw``, ``throwIO``, and similar
functions. Our prototype implementation suggests that this likely does not
carry a significant performance impact.

Since some users may want to explicitly opt out of backtrace collection when
throwing certain exceptions (e.g. in codebases where exceptions are used for
non-exceptional flow control), we also propose to add non-backtrace-collecting
``throw`` variants: ::

    throwNoBacktrace   :: forall e a. (Exception e) => e -> a
    throwIONoBacktrace :: forall e a. (Exception e) => e -> a


Teach top-level handler to use ``displayException``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For historical reasons, GHC's top-level exception handler currently displays
unhandled exceptions using ``Show`` rather than the ``Exception`` class's
``displayException``.

Since only ``displayException`` will display exception
context, we propose that this behavior is changed: unhandled exceptions
should be displayed to the user using ``displayException``.
As the default implementation of ``displayException`` simply delegates to
``show``, we expect that the messages produced by most exceptions will be
unaffected by this change (except for the context added by ``SomeException``\'s
``displayException`` implementation).

Examples
--------

User programs would typically call ``setEnabledBacktraceMechanisms`` during
start-up to select a backtrace mechanism appropriate to their usage: ::

    main :: IO ()
    main = do
        setEnabledBacktraceMechanisms [HasCallStackBacktrace, ExecutionStackBacktrace]

        -- do interesting things here...

Some other programming languages use environment variables to configure
backtrace reporting (e.g. the Rust runtime enables debugging with
``RUST_BACKTRACE=1``). It would be straightforward to provide a utility (either
in a third-party library or perhaps ``base`` itself) which would configure the
global backtrace mechanism from the environment: ::

    setBacktraceMechanismFromEnv :: IO ()
    setBacktraceMechanismFromEnv =
        getEnv "GHC_BACKTRACE" >>= setEnabledBacktraceMechanisms . parseBacktraceMechanisms

This could be called during program initialization, providing the ease of
configuration found in other languages. As it could be added at any time,
``setBacktraceMechanismFromEnv`` is not part of the scope of this proposal.


Effects and Interactions
------------------------

The described mechanism provides users with a convenient means of gaining greater
insight into the sources of exceptions. Currently the ``+RTS -xc``
runtime system flag provides an ad-hoc mechanism for reporting exception
backtraces using the cost-center profiler. While the ``-xc`` mechanism is
largely subsumed by the mechanism proposed here, we do not propose to remove it
in the near future.

During discussions on a previous iteration of this proposal, various community
members mentioned that they were using dynamically-typed annotations on
exceptions in their own code-bases to great effect. One such library,
``annotated-exception``, served as the inspiration for the annotation notion
proposed above and could likely be largely superceded by
``ExceptionAnnotation``.

Costs and Drawbacks
-------------------

The introduction of exception context adds a bit of complexity to GHC's
exception machinery in exchange for a significant improvement in observability.

All-in-all, GHC's exception interface grows considerably under this proposal,
even if we don't provide every possible variant. Moreover, these changes will
need to be mirrored in downstream packages (e.g. ``exceptions``).

Moreover, the general nature of exception context slightly muddies the waters
when it comes to exception hierarchy design. Library authors now have two ways
of conveying failure information to the caller: they may introduce a new
exception type (as they can do today) or they can augment an existing exception
type via the context field. Correctly choosing from between these options may
be, in some cases, non-obvious and could require an element of design taste.

The introduction of the global state for backtrace mechanism selection is quite
ad-hoc. We consider this approach to be a compromise which makes robust
backtraces available by default with minimal additional code. Exception
backtraces are primarily a debugging tool and are a cross-cutting concern. The
global backtrace mechanism selection facility proposed here recognizes this but
it suffers from the usual drawbacks associated with global state: it does not
compose well and may result in surprising behavior when manipulated by more
than one actor.

Migration
---------

Unlike previous versions of this proposal, the change described above has
nearly no impact on existing user-code while allowing existing users to benefit
from backtraces. The only direct breakage will result in applications of the
``SomeException`` data constructor, where the user will be faced with a
compile-time error complaining that ``?exceptionContext`` is not in scope.
In our experience, this sort of code is rare and generally quite
straightforward to adapt; a survey of Hackage suggests that nearly all uses of
``SomeException`` are in pattern contexts.

One existing use-case which does not break but arguably results in non-ideal
behavior is that of exception re-throwing. For instance, consider the program:
::

    catch do_something $ \(e :: MyException) ->
        -- Do something
        throwIO e

Here the original annotations attached to ``e``  (which may include, e.g.,
backtraces) will be lost when the exception is re-thrown.

Alternatives
------------

Exception hierarchy design (alternative one)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An earlier version of this proposal changed the root of the
exception hierarchy to a new type which included a backtrace:
::

    data SomeExceptionWithBacktrace
      = SomeExceptionWithBacktrace
        SomeException       -- ^ the exception
        [Backtrace]         -- ^ backtraces

Unsurprisingly, this change had a non-negligible (although
acceptable) impact on existing user code. Moreover, the
change introduced confusion as users of the old
``SomeException`` type would silently not benefit from the
introduction of backtraces. Moreover, this proposal was
considerably less generic, focusing on static backtraces
instead of arbitrary user-defined annotations.

Exception hierarchy design (alternative two)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Yet an earlier version suggested keeping ``SomeException`` as the root exception
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
constraint where one previously did not exist. For example, the following would no
longer typecheck due to the lack of a ``MonadFail m`` constraint: ::

    f :: Monad m => SomeException -> m ()
    f someException = do
      SomeException e <- pure someException   -- Pattern synonym is assumed fallible
      ...

Backtrace mechanism selection
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In addition, there are several alternatives to the
``enableMechanism`` backtrace-mechanism selection facility.
For instance:

* GHC could gain support for setting the backtrace mechanism at compile-time
  via a compiler flag (this would essentially come down to GHC emitting a call
  to ``enabledBacktraceMechanisms`` in its start-up code).
* the backtrace mechanism could be set in a lexically-scoped manner, at the
  expense of implementation complexity and runtime cost
* alternatively, the community might rather choose one of the backtrace
  mechanisms discussed above and use this mechanism exclusively in exception
  backtraces.

While the last approach may be simpler, we suspect that a single mechanism will not be sufficient:

* There have been `previous efforts <https://gitlab.haskell.org/ghc/ghc/issues/17040>`_
  to add ``HasCallStack`` constraints to all partial functions in ``base``. While we
  believe that this is a worthwhile complementary goal, we don't believe that
  ``HasCallStack`` alone can be our sole backtrace source due to its
  invasive nature.
* The cost center profiler can provide descriptive backtraces but is
  widely regarded as being impractical for use in production environments due
  to its performance overhead.
* GHC's stack unwinder approaches offer stacktraces that are necessarily
  approximate (due to tail calls) and can be harder to interpret but have no
  runtime overhead in the non-failing case.
* Only DWARF backtraces can provide visibility through foreign calls, as
  provided by many polyglot deployment environments

Yet another design would be a complete relegation of handling and reporting of backtraces
completely to the runtime system. This would avoid the thorny library design questions
addressed by this proposal but would lose out on many of the benefits of
offering structured backtraces to the user, in addition to significantly
complicating implementation.

Handling of rethrowing
~~~~~~~~~~~~~~~~~~~~~~

The preservation of ``ExceptionContext`` in ``catch``, et al. is a design
choice whose value (namely, assurance context is not lost on rethrowing) may
not be worth the slight overhead it imposes.

In addition, there is the question of whether rethrown exceptions should gain a
backtrace for the ``catch`` callsite. We currently err on "no" here to avoid
undue overhead, but it may be worth revisiting this in the future.


Ubiquity of ``HasCallStack``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Today, ``HasCallStack`` is the most commonly available and therefore widely
used backtrace mechanism. However, it can introduce overhead by way of small
amounts of allocation in otherwise non-allocating code. The proposal above adds
a ``HasCallStack`` constraints to ``throw``.

Implementation Plan
-------------------

There is an active branch with an implementation of this proposal:
<https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8869>


Acknowledgments
---------------

* Sven Tennie (``@supersven``) has been the driving force through most of this proposal, having
  implemented an early version of this proposal and helped considerably in the
  proposal's language
* Vladislav Zavialov (``@int-index``) contributed significantly to the library design
  with his proposed use of implicit parameters to avoid changing the exception
  hierarchy.
* Matt Parsons (``@parsonsmatt``) also significantly improved the library design by
  pointing out the generalization to dynamically-typed annotations.

Endorsements
-------------

* @domenkozar has indicated that the problem addressed by this proposal poses a
  significant challenge for his work in production and that the approach
  presented here would be an improvement over the status quo.
