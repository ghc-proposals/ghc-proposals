.. proposal-number::

.. trac-ticket::

.. implemented::

.. highlight:: haskell

This proposal is `discussed at this pull requst <https://github.com/ghc-proposals/ghc-proposals/pull/53>`_.

.. contents::

Relaxed default module header
============================

I propose to change the default module header to ``module Main where``.
    
Motivation
------------

Currently, the report states

::

    An abbreviated form of module, consisting only of the module body,
    is permitted. If this is used, the header is assumed to be ``module
    Main(main) where``.

I propose to change that to

::

    An abbreviated form of module, consisting only of the module body,
    is permitted. If this is used, the header is assumed to be ``module
    Main where``.

The rationale is that a main-less main module is still useful, e.g.
when you are working a lot in GHCi, and offload a few extensions to a
separate file. Currently, tools like hdevtools will complain about a
missing main function when editing such a file.

It would also work better with GHC’s ``-main-is`` flag, and avoid problems
like the one described in https://ghc.haskell.org/trac/ghc/ticket/13704


I don’t see any downsides. When compiling to a binary, implementations
are still able to detect that a ``Main`` module is not imported by any
other module and only the main function is used, and optimize as if
only main were exported.


Costs and Drawbacks
-------------------

If we decide to make this change as a always-on divergence from the standard, then the implementation is easy. Otherwise there is a modest cost of implementing a flag.

Iavor points out: One potential difference between the two is that the current behavior allows the ``Main`` module to import ``main`` from another module, while the new behavior would fail in that case.

For example, a file that has only a single line::

    import SomeOtherModule(main)

This still seems like a fairly unusual corner case (with an obvious work around), so I don't think it matters much.

Otherwise I don’t see any downsides. When compiling to a binary, implementations
are still able to detect that a Main module is not imported by any
other module and only the main function is used, and optimize as if
only main were exported.

Unresolved questions
--------------------
Does this change require a flag, a language pragma, or can it just be done.

Implementation Plan
-------------------

I can do this.
