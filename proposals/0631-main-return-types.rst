Set program exit code by main return type
==============

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
Add a new typeclass, ``System.Exit.Termination``, with a function
``report :: Termination a => a -> ExitCode``. If, for a given program,
``(main >>= exitWith . report) :: IO ()`` type-checks, then
the resulting program will behave as if that had been written for ``main``
instead.

Proposed Library Change Specification
-------------------------------------

Add the ``Termination`` typeclass to ``System.Exit``, with the
``report`` function/method, as described above.

Add a ``Termination`` instance for ``ExitCode``.

Examples
--------

::

 data ExitReason
   = AllsWell
   | NoResults

 instance Termination ExitReason where
   report AllsWell = ExitSuccess
   report NoResults = ExitFailure 1

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
to this "breakage". Still, we could add a warning to the ``Termination ExitCode``
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
have ``mainWithTermination :: Termination a => IO a -> IO ()``
and always define ``main = mainWithTermination $ do { ... }``.

Do it just for ExitCode
^^^^^^^^^^^^^^^^^^^^^^^

Rather than a typeclass, we could just have ``main :: IO ExitCode`` be
respected. Using the typeclass allows using domain-specific types
to exit codes as in `the example <#Examples>`_, letting the user
give semantic meaning to the exit status in the typical Haskell
way.

No ExitCode instance
^^^^^^^^^^^^^^^^^^^^^

To preserve full backwards compatibility and encourage custom domain-specific
types, we could avoid having a ``Termination ExitCode`` instance, or have one
which is ``const ExitSuccess`` with a warning emitted if it's ever used. Having
an ``ExitCode`` instance reduces surprise and reduces overhead for simple
programs.

Add Int instance
^^^^^^^^^^^^^^^^

Some users may expect ``main :: IO Int`` to work, and we could add a
``Termination Int`` instance to satisfy that. But this is much more likely
to cause behavior changes in real programs, and perpetuates a practice of
semantically loose types.

Unresolved Questions
--------------------
The name of ``Termination`` and ``report`` are copied from ``Rust``, but
perhaps we want a different paint color.


Implementation Plan
-------------------
I'd (@shlevy) be willing to implement this if accepted.
