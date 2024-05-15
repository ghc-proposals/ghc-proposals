Meaningful main return types
=========================================

.. author:: Shea Levy
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/631>`_.
.. sectnum::
.. contents::

To reduce surprise in light of common programming language behavior,
this proposal constrains the type of ``main`` to cases where the
program exit code logically corresponds to the value yielded
by ``main``.

Motivation
----------
Many languages, perhaps most notably ``C``, allow specifying a program's exit
code by returning an appropriate value from ``main``. ``Haskell98`` *does*
allow a ``main`` which yields values of any type, but surprisingly it does not
do anything with the result: any successful execution will result in a successful
program exit, even if ``main`` yields ``1`` or ``ExitFailure 1``.

This proposal changes GHC to warn when a misleading ``main`` type is used, and
an extension to enable ``main :: IO ExitCode`` to behave as one would expect.

This is a minor quality of life improvement, but one that I've seen hit
newcomers now and then and matches reasonable expectations of program
behavior.

Proposed Change Specification
------------------------------

When ``-XNoMeaningfulMainReturn`` (the default), GHC emits ``Wambiguous-main-return`` (on by default)
if ``main`` has a type that doesn't unify with ``IO ()`` or ``IO Void``.

When ``-XMeaningfulMainReturn``, GHC emits an error if ``main`` has a type that doesn't unify with
``IO ()``, ``IO Void``, or ``IO ExitCode``. If ``main``'s type unifies with ``IO ExitCode``, the
resulting program behaves as if its entrypoint were ``realMain = main >>= exitWith``.

``-XMeaningfulMainReturn`` will become the default in the next GHC language edition.

Examples
--------

::
   {-# LANGUAGE NoMeaningfulMainReturn #-}

   main :: IO ()
   main = pure ()

This is accepted as it is now.

::
   {-# LANGUAGE NoMeaningfulMainReturn #-}

   main = main

This unifies with ``IO ()``, so it is accepted without warning.

::
   {-# LANGUAGE NoMeaningfulMainReturn #-}

   main :: IO Int
   main = pure 1

Warning emitted, program exit code is ``0``, not ``1``

::
   {-# LANGUAGE MeaningfulMainReturn #-}

   main :: IO Int
   main = pure 1

Compile error, ``Int`` is potentially ambiguous.

::
   {-# LANGUAGE MeaningfulMainReturn #-}

   main :: IO Void
   main = pure undefined

Successful compilation, program exit code is ``0``

::
   {-# LANGUAGE MeaningfulMainReturn #-}

   main :: IO ExitCode
   main = pure $ ExitFailure 1

Successful compilation, program exit code is ``1``

Effect and Interactions
-----------------------
No known interactions.

Costs and Drawbacks
-------------------
Development seems likely to be minimal, and maintenance basically
non-existent. This will have no *negative* impact on novice users
learning the language.


Backward Compatibility
----------------------
This will be backwards-incompatible in the next language edition in the
(likely quite rare) case of an unusual ``main`` type (resulting in a new
compilation failure) or in the (almost certainly non-existent) case of
``main :: IO ExitCode`` (resulting in the exit code actually matching the
yielded value). In that latter case, this is almost certainly the desired
behavior anyway.

Alternatives
------------

Retain the status quo
^^^^^^^^^^^^^^^^^^^^^

While the current behavior is functional and any surprise is likely to be
caught early in testing, this change will make learning Haskell a bit
easier and arguably makes for ``main`` functions which better match
the Haskell ethos of well-typed structured interfaces.

Have a typeclass
^^^^^^^^^^^^^^^^

Rather than just enabling ``()``, ``Void``, and ``ExitCode``, we could
instead define a typeclass which enables custom data types to meaningfully
signal exit codes (like Rust's `Termination trait <https://doc.rust-lang.org/std/process/trait.Termination.html>`_).

If desired, this can be added later.

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
