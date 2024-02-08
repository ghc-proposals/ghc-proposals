Set program exit code by main return type
=========================================

.. author:: Shea Levy
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/631>`_.
.. sectnum::
.. contents::

To reduce surprise and match common programming language behavior,
this proposal would allow programs to signal their exit code
based on the return value from ``main``.

Motivation
----------
Many languages, perhaps most notably ``C``, allow specifying a program's exit
code by returning an appropriate value from ``main``. ``Haskell98`` *does*
allow a ``main`` which yields values of any type, but surprisingly it does not
do anything with the result: any successful execution will result in a successful
program exit, even if ``main`` yields ``1`` or ``ExitFailure 1``.

Before this proposal, the following program would exit with code ``0`` even
when there are no results:

::

 main :: IO ExitCode
 main = do
   results <- doSomeWork
   case results of
     [] -> pure (ExitFailure 1)
     _ -> print results >> pure ExitSuccess

With this proposal in place, the program would exit with code ``1`` in that
case.

This proposal follows `Rust <https://doc.rust-lang.org/std/process/trait.Termination.html>`_
in using a typeclass for return types which can signal exit codes.

This is a minor quality of life improvement, but one that I've seen hit
newcomers now and then and matches reasonable expectations of program
behavior.

Proposed Change Specification
-----------------------------

If, for a given program, ``(main >>= exitWith . GHC.IO.Exit.toExitCode) :: IO ()`` type-checks,
then the resulting program will behave as if that had been written for ``main``
instead. Otherwise, the resulting program will behave as if ``main >> exitWith ExitSuccess``
had been written, but the compiler will emit a warning.

Proposed Library Change Specification
-------------------------------------

Add a new module to ``ghc-experimental``:

::

 module GHC.IO.Exit where

 import GHC.IO.Exception (ExitCode (..))
 import Data.Void (Void, absurd)

 class ExitStatus e where
   toExitCode :: e -> ExitCode

 instance ExitStatus ExitCode where
   toExitCode = id

 instance ExitStatus () where
   toExitCode = const ExitSuccess

 instance ExitStatus Void where
   toExitCode = absurd

Eventually, ``ExitStatus`` and its instances should move into ``base``,
probably in ``System.Exit``.

Examples
--------

::

 data ExitReason
   = AllsWell
   | NoResults

 instance ExitStatus ExitReason where
   toExitCode AllsWell = ExitSuccess
   toExitCode NoResults = ExitFailure 1

 main = do
   results <- doSomeWork
   case results of
     [] -> pure NoResults
     _ -> print results >> pure AllsWell

With this proposal in place, the program would exit with code ``1`` if
``doSomeWork`` doesn't yield results.

Effect and Interactions
-----------------------
This change will allow users to write ``main`` programs which yield an
``ExitCode`` and have that code respected.

No known interactions.

Costs and Drawbacks
-------------------
Development seems likely to be minimal, and maintenance basically
non-existent. This will have no *negative* impact on novice users
learning the language.


Backward Compatibility
----------------------
Technically, this will cause a change in behavior in extremely rare cases
(this is level ``1`` on the breakage scale): If a program currently has
``main :: IO ExitCode`` and some branch currently yields ``ExitFailure n``,
the program will currently exit with ``0`` on that branch. After this proposal,
it will exit with ``n``. But the latter is almost certainly the desired behavior for
writing code like that, and I'd be surprised if it exists in any real program.

Because this behavior change, if it ever actually matters, is likely in the direction
of *improving* the program behavior, it's not clear that there are any real costs
to this "breakage". Still, we could add a warning to the ``ExitStatus ExitCode``
instance and encourage users to use a custom type, or see the alternatives
for `an option <#no-exitcode-instance>`_ with no backwards incompatibility.

Alternatives
------------

Retain the status quo
^^^^^^^^^^^^^^^^^^^^^

While the current behavior is functional and any surprise is likely to be
caught early in testing, this change will make learning Haskell a bit
easier and arguably makes for ``main`` functions which better match
the Haskell ethos of well-typed structured interfaces.

If we did stick to the status quo, users could perhaps
have ``mainWithExitStatus :: ExitStatus e => IO e -> IO ()``
and always define ``main = mainWithExitStatus $ do { ... }``.

Do it just for ExitCode
^^^^^^^^^^^^^^^^^^^^^^^

Rather than a typeclass, we could just give ``ExitCode``
special treatment: If ``main`` is typed as ``IO ExitCode``, then the
program behaves as if ``main >>= exitWith`` had been written for
``main`` instead.

Using the typeclass allows using domain-specific types
to exit codes as in `the example <#Examples>`_, letting the user
give semantic meaning to the exit status in the typical Haskell
way.

No ExitCode instance
^^^^^^^^^^^^^^^^^^^^

To preserve full backwards compatibility and encourage custom domain-specific
types, we could avoid having a ``ExitStatus ExitCode`` instance, or have one
which is ``const ExitSuccess`` with a warning emitted if it's ever used. Having
an ``ExitCode`` instance reduces surprise and reduces overhead for simple
programs.

Add Int instance
^^^^^^^^^^^^^^^^

Some users may expect ``main :: IO Int`` to work, and we could add a
``ExitStatus Int`` instance to satisfy that. But this is more likely
to cause behavior changes in real programs, and perpetuates a practice of
semantically loose types.

::

 instance ExitStatus Int where
   toExitCode 0 = ExitSuccess
   toExitCode n = ExitFailure n

Require an ExitStatus instance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Instead of falling back to ``main >> exitWith ExitSuccess`` when there is no ``ExitStatus``
instance, we could have compilation simply fail in this case. This would be backwards
incompatible (in particular, breaking any ``main :: forall a. IO a``, which may be
used to indicate a ``main`` which does not return), but would ensure explicitness and
probably not impact very many programs.

This is omitted mainly because it can be done as a follow-up without centrally impacting
the value of this proposal, after the warning has been in place for some time.

Restrict main to IO ()
^^^^^^^^^^^^^^^^^^^^^^

If ``main`` *had* to be ``IO ()``, this would also reduce surprise,
or at least make it apparent at compile time. This is
backwards-incompatible, but would likely not break that many
programs and the fix would be straightforward.

This alternative would break more programs than the proposal,
and would miss out on the added benefit of program behavior
being specified by more normal Haskell control flow. Also,
this appears to have been the behavior in Haskell 1.4, and
presumably the Haskell 98 authors changed this for a reason.

Restrict main to IO Void
^^^^^^^^^^^^^^^^^^^^^^^^

This would force programmers to be explicit about exit codes,
and indicate that program exit is something different than
normal ``IO`` completion. It might have been a reasonable
choice when Haskell was new, but as it would break almost
every program out there today it's not worth the churn.

Implementation Plan
-------------------
I'd (@shlevy) be willing to implement this if accepted.
