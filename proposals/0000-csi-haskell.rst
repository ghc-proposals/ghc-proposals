Haskell Program Coverage Traces
==============

.. author:: Matthías Páll Gissurarson and Leonhard Applis
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/664>`_.
.. sectnum::
.. contents::


Haskell Program Coverage has been around for ~17 years now, and is a scalable,
easy to use, and low overhead way to generate coverage information for Haskell
programs. However, coverage only is of limited value, especially when it comes
to locating bugs in programs.

We propose to extend the current HPC functionality with *evaluation traces*,
allowing users to see what the program was doing right before the error
occurred.


Motivation
----------

However: sometimes coverage is not quite enough to track down a bug. Often, the
*history* of the execution is required to understand the program behavior. This
is especially true when doing fault-localization: when a program encounters an
error, it can be useful to see what was happening immediately prior.

An interesting case is when the bug is caused by a data producer, but can only
be determined by the data consumer. Currently, errors only report the current
callstack, but if the erroneous data has already been evaluated, it will not
be on the callstack.

One way to tackle this is to add an *evaluation trace*. This allows the error
message to report on evaluation that occurred right before the error.

For some common errors, an evaluation trace is more useful for locating
faulty expression rather than a stack trace, such as
division-by-zero when the zero is from a variable,
calling ``head`` on an empty list,
providing an out-of-bounds index for ``(!)`` on a list,
and non-exhaustive pattern matches in general.


Previous tracing attempts have focused on whole-program tracing, using
an external tool to transform the entire program to instrument and keep
trace the program from start to finish.

A big drawback is that whole program this incurs a heavy cost both in tooling
and especially memory use: for long running programs the trace grows linearly
with time.

However, recent research indicates that we don't have to trace the entire
program to be useful: since data involved in an error is often evaluated
right before the error occurs, the locations involved in generating erroneous
data will be recently evaluated.

By restricting the traces to *recently evaluated locations*, we can produce
more useful runtime error messages, while incurring minimal overhead. By
allowing users to choose the trace length, we can adapt to various contexts.

These traces can be further processed and visualized to help both newcomers
and more experienced programmers to better understand the order of
evaluation.


Examples
--------

Consider the following module, saved as ``Divs.hs``::

    module Main where
    divs :: Int -> [Int]
    divs n = go 2
    where go i | i == n = []
            go i = if d i
                then i:(go (i+1))
                else go (i+1)
            d i = n `mod` i == 0

    smallestDiv n = head (divs n)

    main :: IO ()
    main = print (smallestDiv 13)

The bug here is that it ``divs`` should include the number itself in the list,
i.e. the base case of go should be ``go i | i == n = [i]``.


When compiled with ``-prof`` and ``-prof-auto``, this prints the following error
message::

  divs: Prelude.head: empty list
  CallStack (from HasCallStack):
    error, called at libraries/base/GHC/List.hs:1643:3
      in base:GHC.List
    errorEmptyList, called at
      libraries/base/GHC/List.hs:82:11 in base:GHC.List
    badHead, called at libraries/base/GHC/List.hs:78:28
      in base:GHC.List
    head, called at Div.hs:10:17 in main:Main
  CallStack (from -prof):
    Main.smallestDiv (Divs.hs:10:17-29)
    Main.main (Divs.hs:13:15-28)
    Main.main (Divs.hs:13:8-29)
    Main.CAF (<entire-module>)

It correctly points out that the error was caused by ``head`` encountering
an empty list. However, the bug is not caused by the use of ``head``, but
rather an off-by-one error causing it to generate an empty list instead

With the proposed change, when the ``-fhpc`` and the new
``-hpc-trace`` flag is present an *evaluation trace* of
*recently evaluated locations* is printed as well::

  divs: Prelude.head: empty list
  CallStack (from HasCallStack):
    error, called at
      libraries/base/GHC/List.hs:1749:3 in base:GHC.List
    errorEmptyList, called at
      libraries/base/GHC/List.hs:89:11 in base:GHC.List
    badHead, called at
      libraries/base/GHC/List.hs:83:28 in base:GHC.List
    head, called at Divs.hs:10:17 in main:Main
  CallStack (from -prof):
    Main.smallestDiv (Divs.hs:10:17-29)
    Main.main (Divs.hs:13:15-28)
    Main.main (Divs.hs:13:8-29)
    Main.CAF (<entire-module>)
  Recently evaluated locations:
    Divs.hs:4:25-4:26  ... = []
    Divs.hs:4:16-4:21  |...,i == n,...=... (was matched)
    repeats (11 times in window):
      Divs.hs:4:9-7:28   Main:divs>go
      Divs.hs:7:21-7:28  ... = go (i+1)
      Divs.hs:5:19-5:21  ...else d i
      Divs.hs:8:9-8:28   Main:divs>d
      Divs.hs:5:16-7:28  ... = if d i...
      Divs.hs:4:16-4:21  |...,i == n,...=... (not matched)
    Divs.hs:4:9-7:28   Main:divs>go
    Divs.hs:3:1-8:28   Main:divs
   Previous expressions
     Divs.hs:10:1-10:29  Main:smallestDiv
     Divs.hs:13:1-13:29  Main:main

Showing where the empty list in question originates.


Proposed Change Specification
-----------------------------

We propose introducing the ``-fhpc-trace`` and the optional ``-fhpc-trace-length``
flags, and extending the HPC instrumentation to collect a trace of recently
evaluated locations when these flags are present.

The traces track both when an expression starts being evaluated
and when it stops being evaluated, as well as tracking the current
*evaluation depth*, i.e. how many expressions we have started evaluating
but not finished yet.

These two in combination allows us to produce richer traces and error messages,
such as the "Previous expressions" in the message above, which helps us
understand what is currently being evaluated and what is still unresolved,
effectively embedding some notion of the callstack into the trace itself.



Proposed Library Change Specification
-------------------------------------


An alternative is to change the existing `error` function to include
the evaluation trace whenever the flag is present.



Effect and Interactions
-----------------------

Tools that parse runtime error messages directly will be impacted when the
flag is present.


Costs and Drawbacks
-------------------

Most of this proposal has already been implemented and evaluated. Maintenance
is similarly minimal, and should not be more than the current burden of
maintaining HPC in general.

Novices will have to be made aware of the new flag, which will be covered in
the users guide. Since the goal is to improve error messages, hopefully it will
Make the language easier to learn.

With the flag enabled, binary size, wall time and memory use of programs
will be impacted. However, since we assume it will only be used during testing
and development, the impact on end-users should be minimal.


Backward Compatibility
----------------------

Existing programs should not change. The `.tix` files produced by HPC will
include the additional tracing information, however, this will be accompanied
by changes to the HPC parser to accommodate this extra information.

Tools that consume this file directly will be impacted, those who use the
HPC library will not.

The error message for runtime errors will change when the flag is present.
However, since it is gated behind a flag, the impact will be minimal


Alternatives
------------

There are a few design decisions that are up to debate.

1. Change nothing and keep the current callstack only reporting.
2. Keep track of the entire trace and not just the suffix.
   This would introduce additional runtime overhead and
   potentially massive memory use as well as a higher maintenance burden.
3. Keep track of the trace, but do not extend the default error message
   even if the flag is present. The trace can then be recovered by specialist
   tools using the ``hpc`` library.
4. Adding a `errorWithEvaluationTrace` function to `base` that
   displays the evaluation trace as well when the flag is enabled.
   Functions like ``head`` and ``(!)`` and other non-total functions in base would
   be changed to use this new function.
5. Instead of extending the current ``.tix`` files, we would introduce a
   new file to track the trace. This would limit impact on tools that
   read ``.tix`` files directly, however, since the two are closely linked
   (and the locations refer to the same indexes in the ``.mix`` files), this
   would incur additional maintenance costs.
6. Targeting the eventlog instead of the ``.tix`` format.
   GHC already has eventlog capabilities that log things such as scheduling
   events, garbage collection statistics, profiling information and
   *user defined tracing events*. However, this runs into the problem
   described in the motivation where we specifically don't want to keep
   a track of everything that happened, but rather only a suffix of
   recent events, leading to increased overhead.




Unresolved Questions
--------------------

1. Comparison to the ``:trace`` command.
   GHCi already has a ``:trace`` command that can also provide an evaluation
   trace. However, this requires GHCi itself, and is harder to use during
   testing.
2. Choosing a good default trace size is still unresolved.
   `Research https://mpg.is/papers/gissurarson2023csi.pdf` indicates
   that a default of 500 is sufficient for most cases in the ``nofib-buggy``
   dataset, though this is very dependent on the structure of the
   program itself. The ``:trace`` command uses a default of ``50`` for.
2. Interaction with parallel Haskell is poorly understood.
   We could possibly add thread ids and timestamps to the trace,
   though this would incur additional overhead.


Implementation Plan
-------------------

I will implement the proposal, based on the work already done for the
`CSI: Haskell paper https://mpg.is/papers/gissurarson2023csi.pdf`.

The current implementation can be found at
https://github.com/Tritlo/ghc/commit/62fa1edbe81d8942ce922d586d50c3f1f79ffca4,
though it will need to be updated for the latest version of GHC.

