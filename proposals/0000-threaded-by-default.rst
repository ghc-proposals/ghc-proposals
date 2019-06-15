Compile with threaded RTS by default
==============

.. proposal-number:: <blank>
.. trac-ticket:: 16126
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/240>`_.
.. sectnum::
.. contents::

Today, in order to use the threaded variant of the runtime, one have to pass the ``-threaded`` flag. The proposal suggests making ``-threaded`` the default: in order to get the threaded RTS one would not have to do anything, while to fallback to the current, single-threaded mode, one would use the new ``-single-threaded`` flag.


Motivation
------------

Parallel hardware is here. The overwhelming majority of GHC users probably has access to multiple cores. Parts of GHC's ``base`` library, as well as FFI implementation, utilize those resources. In this setting, **defaulting** to the single-threaded RTS seems to be a legacy, which is not only over-pessimistic about the program environment, but also leads to subtle crushes and deadlocks when those GHC features are in use. E.g. 

* using ``GHC.Conc.registerDelay`` aborts compilation without ``-threaded``,
* calling a foreign function that is “safe” and blocks makes the nonthreaded RTS blocking altogether.

The proposal suggests avoiding these subtle problems by defaulting on the threaded RTS, and provide a way to fallback for users in need of single-threaded RTS mode for certain reasons (determinism, debugging, limited hardware, etc.). The latter can be done by the means of the new ``-single-threaded`` flag.


Proposed Change Specification
-----------------------------

The GHC compiler should start in ``-threaded`` mode by default. To fallback to the non-threaded mode, one would have to pass ``-single-threaded`` flag to the compiler. Otherwise, if a user pass the ``-threaded`` flag, they should get a warning that this flag has no effect anymore.


Effect and Interactions
-----------------------

The users of GHC will get their programs compiled with the threaded RTS by default. This follows principle of the least surprise: many programs implicitly depending on tools such as `forkIO` can deadlock or crush with no good explanation with the single-threaded RTS. As surprising as it may sound, the threaded RTS comes closer to the principle of the least surprise than the single-threaded RTS.

One of the main concerns with regards to the threaded RTS is parallel GC, which, anecdotally, almost certainly brings performance losses when not tuned carefully (cf., e.g. `#9221 <https://gitlab.haskell.org/ghc/ghc/issues/9221>`_). It is important to understand that parallel GC **is not implied** by ``-threaded``. The threaded mode still starts the program with one capability by default (unless ``-N`` is explicitly provided by the user), and GC runs sequential.

Those who passed ``-threaded`` before will get a new warning, which might be painful (e.g. in the ``-Werror`` setting).


Costs and Drawbacks
-------------------

The main concern of this change is sudden changes to performance of compiled programs. In some cases it will improve, but in certain cases it may degrade, i.e. programs with no concurrent IO or programs running on single-threaded architectures might observe performance degradation upon updating to a newer GHC, and we should make sure to advertise the option to opt-out (via ``-single-threaded``) in the release notes. Changes in performance shouldn't be significant on a typical architecture given that parallel GC is not implied by ``-threaded``.

Implementation has a very low cost and mostly concern with figuring out necessary adaptations in the GHC test suite.


Alternatives
------------

One alternative is status quo: default to the non-threaded RTS. This is a plausible option but feels outdated as of now.

Another alternative suggested by Cris Done:

    GHC could determine at the Core/STG phase whether in the call graph from main, directly or transitively, there was a reference to ``fork#`` and other primops related to threading, and if neither ``-threaded`` nor ``-single-threaded`` was specified, it could warn "Your program may use multi-threaded code, please specify a mode by either: ``-threaded`` or ``-single-threaded``".

This, in fact, does not exclude the option to switch the deafult.

There is also a minor concern about the fallback flag name. Possible options that have been suggested so far are ``-single-threaded`` and ``-non-threaded``.


Unresolved Questions
--------------------
None.


Implementation Plan
-------------------

The implementation is started in `!538 <https://gitlab.haskell.org/ghc/ghc/merge_requests/538>`_.
