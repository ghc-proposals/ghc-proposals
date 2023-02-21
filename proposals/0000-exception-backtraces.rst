Decorate exceptions with backtrace information
==============================================

.. authors:: Ben Gamari; David Eichmann; Sven Tennie
.. date-accepted::
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/18159
.. implemented:: in-progress <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8869>
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/330>`_.
.. sectnum::
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
`katip-raven #1
<https://github.com/cachix/katip-raven/issues/1#issuecomment-625389463>`_),
automatic error reporting, code analysis tools, and the like.


Proposed Change Specification
-----------------------------

This proposal consists of four largely-independent parts:

1. augmenting the current root of the exception hierarchy,
   ``SomeException``, with additional metadata ("annotations") in the form of
   an ``ExceptionContext``;
2. the introduction of annotation types to capture backtraces from the
   backtrace-collection mechanisms mentioned above;
3. a facility for choosing which backtrace mechanism(s)
   should be used to collect exception provenance;
4. changing the top-level exception handler to use ``displayException`` rather
   than ``show``.

We will summarize these changes in the subsections below and give further
context in the section that follows.

Annotations
~~~~~~~~~~~

Export the following new definitions from ``Control.Exception.Annotation``:

* The class of exception annotations: ::

    class Typeable a => ExceptionAnnotation a where
      displayExceptionAnnotation :: a -> String

      default displayExceptionAnnotation :: Show a => a -> String
      displayExceptionAnnotation = show

* An existential wrapper for dynamically-typed exception annotations: ::

    data SomeExceptionAnnotation where
        SomeExceptionAnnotation ::
          forall a. (ExceptionAnnotation a) => a -> SomeExceptionAnnotation

Backtraces
~~~~~~~~~~

Export the following new definitions from ``Control.Exception.Backtrace``:

* An enumeration of the mechanisms by which GHC can collect backtraces: ::

    type BacktraceMechanism :: Type -> Type
    data BacktraceMechanism a where
      CostCentreBacktrace   :: BacktraceMechanism (Ptr CostCentreStack)
      HasCallStackBacktrace :: BacktraceMechanism GHC.Stack.CallStack
      ExecutionBacktrace    :: BacktraceMechanism [GHC.ExecutionStack.Location]
      IPEBacktrace          :: BacktraceMechanism [StackEntry]

* During program execution, each backtrace mechanism is either enabled or
  disabled. This is tracked in global mutable state that can be accessed using
  the following functions ::

    getEnabledBacktraceMechanisms :: IO EnabledBacktraceMechanisms
    setEnabledBacktraceMechanisms :: EnabledBacktraceMechanisms -> IO ()

    newtype EnabledBacktraceMechanisms =
      EnabledBacktraceMechanisms {
        backtraceMechanismEnabled :: forall a. BacktraceMechanism a -> Bool
      }

  Note that ``EnabledBacktraceMechanisms`` is isomorphic to ``(Bool, Bool, Bool, Bool)``,
  i.e. a flag per mechanism.

  By default, ``HasCallStackBacktrace`` is enabled and other mechanisms are disabled.

* A record of collected backtraces: ::

    newtype Backtraces =
      Backtraces {
        getBacktrace :: forall a. BacktraceMechanism a -> Maybe a
      }

  Note that ``Backtraces`` is isomorphic to a record containing:

  * ``Maybe (Ptr CostCentreStack)``
  * ``Maybe (GHC.Stack.CallStack)``
  * ``Maybe [GHC.ExecutionStack.Location]``
  * ``Maybe [StackEntry]``

* A function to render ``Backtraces`` to a user-readable string: ::

    displayBacktraces :: Backtraces -> String
    displayBacktraces = ...

* An instance of ``ExceptionAnnotation`` for ``Backtraces``: ::

    instance ExceptionAnnotation Backtraces where
      displayExceptionAnnotation = displayBacktraces

* A procedure to collect backtraces at a given point in the program: ::

    collectBacktraces :: HasCallStack => IO Backtraces

  This function collects backtraces for the currently enabled mechanisms.
  As a consequence, enabling or disabling a mechanism will affect its performance.

Representing Exception Context
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Export the following new definitions from ``Control.Exception.Context``:

* An abstract data type for exception contexts: ::

    data ExceptionContext

    instance Monoid ExceptionContext
    instance Semigroup ExceptionContext

  We do not export its constructors to allow for future changes.

* A constraint synonym for an implicitly passed exception context: ::

    type HasExceptionContext = (?exceptionContext :: ExceptionContext)

  The fact that ``HasExceptionContext`` is defined as an implicit parameter is
  an implementation detail and is not considered a part of the API.

* Functions to construct, extend, and deconstruct exception contexts: ::

    emptyExceptionContext :: ExceptionContext
    addExceptionAnnotation :: ExceptionAnnotation a => a -> ExceptionContext -> ExceptionContext
    getExceptionAnnotations :: ExceptionAnnotation a => ExceptionContext -> [a]
    getAllExceptionAnnotations :: ExceptionContext -> [SomeExceptionAnnotation]

  The order of annotations is preserved: ::

    getAllExceptionAnnotations $
        addExceptionAnnotation ann1 $
        addExceptionAnnotation ann2 $
        ...
        addExceptionAnnotation annk $
        emptyExceptionContext
      ≡
    [
      SomeExceptionAnnotation ann1,
      SomeExceptionAnnotation ann2,
      ...
      SomeExceptionAnnotation annk
    ]

  Advertise the following time complexity for operations on contexts (the actual
  implementation may be more efficient):

  * ``addExceptionAnnotation`` – O(1)
  * ``getExceptionAnnotations`` – O(n)
  * ``getAllExceptionAnnotations`` – O(n)

* A function to display the annotations of an ``ExceptionContext`` in
  human-readable form using ``displayExceptionAnnotation``: ::

    displayExceptionContext :: ExceptionContext -> String

Attaching Context to Exceptions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In ``Control.Exception``, modify existing definitions as follows:

* Store the exception context in ``SomeException``: ::

    - data SomeException = forall e.                      (Exception e) => SomeException e
    + data SomeException = forall e. (HasExceptionContext, Exception e) => SomeException e

* Modify the ``Exception`` instance of ``SomeException`` as follows: ::

    instance Exception SomeException where
        toException se = se
        fromException = Just
        displayException (SomeException e) =
            displayException e ++ displayExceptionContext ?exceptionContext

Export the following new definitions from ``Control.Exception``:

* A function to retrieve the ``ExceptionContext`` attached to an exception: ::

    someExceptionContext :: SomeException -> ExceptionContext
    someExceptionContext (SomeException _) = ?exceptionContext

* A function to add (in the ``Semigroup`` sense) context to ``SomeException``: ::

    addExceptionContext :: ExceptionContext -> SomeException -> SomeException

* A function that catches any exception thrown by an ``IO`` action, adds an
  annotation to it using ``addExceptionAnnotation``, and then rethrows it: ::

    annotateIO :: ExceptionAnnotation a => a -> IO r -> IO r

  It never calls ``collectBacktraces``, adding **only** the user-specified
  annotation.

Providing context to handlers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Export the following new definitions from ``Control.Exception`` which provide a
convenient way to gain access to ``ExceptionContext`` in exception handlers: ::

  data ExceptionWithContext a =
    ExceptionWithContext ExceptionContext a

  instance Show a => Show (ExceptionWithContext a)

  instance Exception a => Exception (ExceptionWithContext a) where
      toException (ExceptionWithContext ctxt e) = SomeException e
        where ?exceptionContext = ctxt
      fromException se = do
          e <- fromException se
          return (ExceptionWithContext (exceptionContext se) e)
      displayException = displayException . toException

Preserving context on rethrowing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In ``Control.Exception``, modify existing definitions as follows:

* Modify ``catch`` to preserve ``ExceptionContext`` when a handler rethrows an
  exception: ::

    catch :: Exception e => IO a -> (e -> IO a) -> IO a
    catch (IO io) handler = IO $ catch# io handler'
     where
       handler' e =
         case fromException e of
           Just e' -> unIO (annotationIO (someExceptionContext e) (handler e'))
           Nothing -> raiseIO# e

  Modify ``catchJust`` and ``handleJust`` accordingly (mutatis mutandis).

Capturing Backtraces on Exceptions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In ``Control.Exception``, modify existing definitions as follows:

* Add the following method and default definition to the ``Exception``
  typeclass: ::

    backtraceDesired :: forall a. Bool
    backtraceDesired = True

* Modify ``throwIO`` as follows: ::

    throwIO :: forall e a. Exception e => e -> IO a
    throwIO e = do
        ?exceptionContext <-
          if backtraceDesired @e
            then do
              bt <- collectBacktraces
              return (addExceptionAnnotation bt emptyExceptionContext)
            else return emptyExceptionContext
        let se :: SomeException
            se = SomeException e
        raiseIO# se

Export the following new definitions from ``Control.Exception``:

* The following ``newtype`` wrapper and instance which can be used by the user
  when throwing an exception to disable backtrace collection: ::

    newtype NoBacktrace e = NoBacktrace e

    instance Show e => Show (NoBacktrace e)

    instance Exception e => Exception (NoBacktrace e) where
      fromException = NoBacktrace . fromException
      toException (NoBacktrace e) = SomeException e
      backtraceDesired = False

``HasCallStack`` Backtraces for Thrown Exceptions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In ``Control.Exception`` add ``HasCallStack`` constraints to the exception
``throw`` functions to allow inclusion in backtrace context: ::

    throwIO :: forall e a. (HasCallStack, Exception e) => e -> IO a
    throw   :: forall e a. (HasCallStack, Exception e) => e -> a

Modifying the top-level handler
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For historical reasons, the the top-level exception handler which all programs
run under currently uses ``Show`` to display uncaught exceptions to the user.
Change this handler to instead use the ``displayException`` method of the
``Exception`` class.


Discussion
----------

The dynamically-typed open-world of exception types supported by Haskell is
achieved through use of ``Typeable`` and the existentially-quantified
``SomeException`` type (see [Marlow2006]_ for details). We
extend this type to allow exceptions to be extended in the "product" sense,
allowing users to decorate existing exception types with ad-hoc metadata
(represented by the ``ExceptionContext`` type).

The notion of ``ExceptionContext`` proposed here is taken from the generalized
exception annotation machinery found in the ``annotated-exception`` `library
<https://hackage.haskell.org/package/annotated-exception>`_, which demonstrated
the utility of being able to attach ad-hoc contextual data to exceptions.
By folding this notion into ``base``, we provide the community with a common
means of capturing backtraces as well as application-specific metadata.

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

The fact that backtrace collection with some of these mechanisms can be
rather expensive motivates two features of this proposal:

* the ``NoBacktrace`` wrapper, allowing users to disable backtrace collection
  at the ``throw``-site. This is sometimes necessary when exceptions are used
  for non-exceptional control flow.

* the ability to enable and disable individual exception mechanisms via
  ``setEnabledBacktraceMechanisms``.

Since most of these mechanisms require changes in build configuration from the
user to be useful, we proposal to only enable collection of ``HasCallStack``
backtraces by default.

.. [Marlow2006] Marlow, S. "An Extensible Dynamically-Typed Hierarchy of Exceptions."
   Haskell '06 (<https://simonmar.github.io/bib/papers/ext-exceptions.pdf>).

Handling of rethrowing
~~~~~~~~~~~~~~~~~~~~~~

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
to preserve exception context when an exception is thrown from a handler by
catching the rethrown exception and augmenting its context with that of the old
exception. This ensures reliability of backtraces at the expense of a
constant-time cost when exceptions are handled.

.. top-level-handler:

Teach top-level handler to use ``displayException``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Under the original 2006 design of GHC's extensible exception machinery, the
only means of displaying exceptions to the user was ``Exception``\ 's  ``Show``
superclass. However, this introduced an uneasy tension: While, on one hand,
``Show`` output is generally not appropriate to show to (often not
Haskell-inclined) end-users, in principle ``Show`` is intended to produce
Haskell syntax, invertible using ``Read``.

For this reason, the ``displayException`` method was introduced
[displayException-discussion]_ to ``Exception`` in 2014 to produce
human-readable output. However, at the time there was some disagreement
regarding whether it would be appropriate to change the top-level handler away
from using ``Show``, arguing that ``Show`` may be more appropriate for
developers, who are free to introduce their own handler using
``displayException`` if desired.

However, in this proposal we do not propose to change the ``Show`` instance of
``SomeException`` to include exception context as implicit parameter syntax is
not Haskell 2010.

Since only ``displayException`` will display exception
context, we propose that the the top-level handler behavior be changed as was
originally proposed in 2014: unhandled exceptions should be displayed to the
user using ``displayException``. As the default implementation of
``displayException`` simply delegates to ``show``, we expect that the messages
produced by most exceptions will be unaffected by this change (except for the
context added by ``SomeException``\'s ``displayException`` implementation).

.. [displayException-discussion] See
   the `libraries@haskell.org discussion
   <https://mail.haskell.org/pipermail/libraries/2014-November/024176.html>`_
   and GHC `#9822 <https://gitlab.haskell.org/ghc/ghc/-/issues/9822>`_.


Examples
--------

User programs would typically call ``setEnabledBacktraceMechanisms`` during
start-up to select a backtrace mechanism appropriate to their usage: ::

    main :: IO ()
    main = do
        setEnabledBacktraceMechanisms $ EnabledBacktraceMechanisms $ \case
          IPEBacktrace          -> True
          HasCallStackBacktrace -> True
          _                     -> False

        -- do interesting things here...

Some other programming language implementations use environment variables to configure
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
All-in-all, GHC's exception interface grows modestly under this proposal,
even if we don't provide every possible variant.

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


Alternatives
------------

Exception hierarchy design (alternative one)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An earlier version of this proposal changed the root of the exception hierarchy
to a new type which included a backtrace: ::

    data SomeExceptionWithBacktrace
      = SomeExceptionWithBacktrace
          :: SomeException       -- ^ the exception
          -> [Backtrace]         -- ^ backtraces
          -> SomeExceptionWithBacktrace

Unsurprisingly, this change had a non-negligible
impact on existing user code. Moreover, the
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
In addition, there are several alternatives to the proposed backtrace mechanism
selection facility. For instance:

* a simpler, non-GADT-based approach might be used
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
used backtrace mechanism. The proposal above adds ``HasCallStack`` constraints
to ``throw`` and ``throwIO``. However, it can introduce overhead by way of
small amounts of allocation in otherwise non-allocating code (although this can
generally be mitigated by freezing the callstack at the ``throw`` callsite).
One could also leave these functions as-is at the expense of giving up
``HasCallStack`` backtraces on exceptions.


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
