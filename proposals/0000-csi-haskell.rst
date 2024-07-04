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


Example Divs
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

Example PrimeTest (Nofib-Buggy)
-------------------------------
The program `PrimeTest` checks the *Mersenne Prime 2^608-1* to be a prime number (and should return True). 

Amongst the 4 `.lhs`-files, the bug is in `Prime.lhs` and consist of a faulty function from L72 to L77:: 

    findKQ :: Integer -> (Integer, Integer)
    findKQ n = f (0, (n-1))
         -- BUG: The following line contains a bug
    	where f (k,q) = if r == 0 then f (k, d) else (k, q)
         -- CORRECT -- then f (k+1, d) else (k, q)
    		where (d, r) = q `divMod` 2

The issue consists of a non-exhaustive pattern match happens at a different location, `singleTestX` which uses `multiTest` which in turn uses the faulty `findKQ`. Not counting internal functions, the reported location and function are *two jumps* away from the actual fault.  

The evaluation trace (length 50) shows the following::

    Main: Prime.lhs:92:12-82: Non-exhaustive patterns in t : ts
    Recently evaluated locations:
      ./Prime.lhs:73:17-73:17  0
      ./Prime.lhs:75:48-75:48  k
      ./Prime.lhs:75:60-75:60  k
      ./Prime.lhs:92:47-92:47  k
      ./Prime.lhs:92:34-92:48  (fromInteger k)
      ./Prime.lhs:92:29-92:82  take (fromInteger k) (ite...
      ./Prime.lhs:91:6-91:6    t
     Previous expressions:
      ./Prime.lhs:75:59-75:64  ... = (k, q)
      ./Prime.lhs:75:33-75:38  ...else r == 0
      ./Prime.lhs:75:20-77:56  Prime:findKQ>f
      ./Prime.lhs:75:45-75:52  ... = f (k, d)
      ./Prime.lhs:75:33-75:38  ...then r == 0
      ./IntLib.lhs:23:3-23:24  IntLib:readInteger
      Main.lhs:76:12-76:36     Main:doLine>n
      Main.lhs:74:3-78:30      Main:doLine
      Main.lhs:69:26-69:68     ... = doLine l (\state -> doInp...
      Main.lhs:68:3-69:68      Main:doInput
      Main.lhs:62:3-62:29      Main:process
      Main.lhs:56:3-56:68      Main:main
     There were 147 evaluations in total but only 92 were recorded.
     Re-run again with a bigger trace length for better coverage.

While the non-exhaustive-patternmatch reports a different location, the latest evaluations are at the exact position of the introduced fault. 
We expect this to be true for most non-exhaustive patterns (that do not overlap with their stack-trace). 

Example Sorting (Nofib Buggy) & Other Divide-By-Zero
----------------------------------------------------

The `Sorting` example from Nofib Buggy has a trivial fault that divides by zero in L144-147:: 

    div2 :: Int -> Int
    -- BUG:The following line contains a bug
    div2 k = k `div` (2-2)
    -- CORRECT -- div2 k = k `div` 2


The resulting output (length 25) shows:: 

    Main: divide by zero
    Recently evaluated locations:
      ./Sort.hs:146:25-146:25  2
      ./Sort.hs:146:23-146:23  2
      ./Sort.hs:146:22-146:26  (2-2)
      ./Sort.hs:146:14-146:26  k `div` (2-2)
     Previous expressions:
      ./Sort.hs:146:5-146:26  Sort:heapSort>div2
      Main.hs:14:36-14:43     ... =
      Main.hs:13:5-22:57      Main:mangle>sort
    
     There were 668 evaluations in total but only 50 were recorded.
     Re-run again with a bigger trace length for better coverage.


Admittedly, the fault is a bit trivial. 
But: There are *real* divide-by-zeros out there and the evaluation trace provides a clear point to look at and do an easy fix. 

Example Parafins (Nofib Buggy)
----------------------------------

The parrafins example from Nofib-Buggy calculates `paraffins` based on an input list. I do not fully understand the domain, but paraffins express a concept in chemistry based on how atoms/molecules can connect, which follows a clear mathematical model. 

The fault is in the `Main.hs` from L47 to L51:: 

    three_partitions :: Int -> [(Int,Int,Int)]
    three_partitions m =
    -- BUG: The following line contains a bug:
      [ (i,j,k) | i <- [0..(div m 3)], j <- [i..(div (m-i) 2)], k <- [i - (i+j)]]
    -- CORRECT --   [ (i,j,k) | i <- [0..(div m 3)], j <- [i..(div (m-i) 2)], k <- [m - (i+j)]]


The issue can be summarized as using a wrong variable, resulting in (wrong) triples. Especially, it can *count out* , depending on the input, resulting in an error such as `Main: Ix{Int}.index: Index (-1) out of range ((0,3))`.  

When using the extended evaluation trace, a 50 entries log results in:: 

    Main: Ix{Int}.index: Index (-1) out of range ((0,3))
    Recently evaluated locations:
      Main.hs:69:56-69:56  k
      Main.hs:69:47-69:54  radicals
      Main.hs:69:47-69:56  ... = radicals!k
     Previous expressions:
      Main.hs:69:21-69:26  ...else (j==k)
      Main.hs:54:21-54:44  ... = (r:rs) : (remainders rs)
      Main.hs:68:59-68:68  ... = radicals!j
      Main.hs:68:33-68:38  ...else (i==j)
      Main.hs:53:1-54:44   Main:remainders
    
    There were 579 evaluations in total but only 50 were recorded.
    Re-run again with a bigger trace length for better coverage.

Which does *not* cover the fault. 

A higher trace length, namely 500, contains the faulty position:: 

    Main: Ix{Int}.index: Index (-1) out of range ((0,3))
    
    Recently evaluated locations:
      Main.hs:69:56-69:56  k
      Main.hs:69:47-69:54  radicals
      Main.hs:69:47-69:56  ... = radicals!k
     Previous expressions:
      Main.hs:69:21-69:26  ...else (j==k)
      Main.hs:54:21-54:44  ... = (r:rs) : (remainders rs)
      Main.hs:68:59-68:68  ... = radicals!j
      Main.hs:68:33-68:38  ...else (i==j)
      Main.hs:53:1-54:44   Main:remainders
      Main.hs:54:21-54:44  ... = (r:rs) : (remainders rs)
      repeats (2 times in window):
        Main.hs:53:1-54:44   Main:remainders
        Main.hs:53:17-53:18  ... = []
      Main.hs:53:1-54:44   Main:remainders
      repeats (2 times in window):
        Main.hs:69:33-69:40  ... = (rj:rjs)
        Main.hs:69:21-69:26  ...then (j==k)
        Main.hs:54:21-54:44  ... = (r:rs) : (remainders rs)
        Main.hs:68:45-68:52  ... = (ri:ris)
        Main.hs:68:33-68:38  ...then (i==j)
        Main.hs:53:1-54:44   Main:remainders
        Main.hs:54:21-54:44  ... = (r:rs) : (remainders rs)
        Main.hs:53:1-54:44   Main:remainders
        Main.hs:48:1-50:77   Main:three_partitions
        Main.hs:64:1-69:58   Main:rads_of_size_n
        repeats (2 times in window):
          Main.hs:53:17-53:18  ... = []
          Main.hs:53:1-54:44   Main:remainders
      Main.hs:69:33-69:40  ... = (rj:rjs)
      Main.hs:69:21-69:26  ...then (j==k)
      Main.hs:54:21-54:44  ... = (r:rs) : (remainders rs)
      Main.hs:68:45-68:52  ... = (ri:ris)
      Main.hs:68:33-68:38  ...then (i==j)
      Main.hs:53:1-54:44   Main:remainders
      Main.hs:54:21-54:44  ... = (r:rs) : (remainders rs)
      Main.hs:53:1-54:44   Main:remainders
      Main.hs:48:1-50:77   Main:three_partitions
      Main.hs:64:1-69:58   Main:rads_of_size_n
    
     There were 579 evaluations in total but only 500 were recorded.
     Re-run again with a bigger trace length for better coverage.

This is not optimal, but a step towards the right direction; 
The original error message (`Main: Ix{Int}.index: Index (-1) out of range ((0,3))`) has little to no information to act on, while the trace is able to re-construct the program flow. 

The high length is necessary as the program spawns a lot of chunks, and the current evaluation-trace only reports on *matched* evaluations that have their start and end within the observed window. 
The location of the fault in `Previous expressions: ` is also due to the evaluation of `three_partitions` being fully evaluated and (despite the faulty output) being correctly executed. The issue is not with the call, but with the value.  

Thus we argue, that this type of issue can only be covered in a evaluation trace. There is no issue with the call-stack, there is only an issue with faulty data. 

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
   `Research <https://mpg.is/papers/gissurarson2023csi.pdf>`_ indicates
   that a default of 500 is sufficient for most cases in the ``nofib-buggy``
   dataset, though this is very dependent on the structure of the
   program itself. The ``:trace`` command uses a default of ``50`` for.
3. Interaction with parallel Haskell is poorly understood.
   We could possibly add thread ids and timestamps to the trace,
   though this would incur additional overhead.


Implementation Plan
-------------------

I will implement the proposal, based on the work already done for the
`CSI: Haskell paper <https://mpg.is/papers/gissurarson2023csi.pdf>`_.

The current implementation can be found at
`this commit <https://github.com/Tritlo/ghc/commit/62fa1edbe81d8942ce922d586d50c3f1f79ffca4>`_,
though it will need to be updated for the latest version of GHC.

