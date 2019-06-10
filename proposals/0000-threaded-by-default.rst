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
Parallel hardware is here. The overwhelming majority of GHC users probably has an access to multiple cores. In this setting, defaulting to single-threaded RTS seems to be a legacy, and the one we should cleanup at some point anyway. The proposal suggest exactly this. We should default on the common case, which is parallel hardware today, and provide a way to fallback for users of single-core architectures. The latter can be done by the means of the new ``-single-threaded`` flag.


Proposed Change Specification
-----------------------------

The GHC compiler should start in ``-threaded`` mode by default. To fallback to the non-threaded mode, one would have to pass ``-single-threaded`` flag to the compiler. Otherwise, if a user pass the ``-threaded`` flag, they should get a warning that this flag has no effect anymore.


Effect and Interactions
-----------------------
The users of GHC will get their programs compiled with the threaded RTS by default. This is felt to be the reasonable default these days. 

Those who passed ``-threaded`` before will get a new warning, which might be painful (e.g. in the ``-Werror`` setting).


Costs and Drawbacks
-------------------
The main concern of this change is sudden changes to performance of compiled programs. In some cases it will improve, but in certain casses it may degrade. In the lack of objective data at hand, we can only speculate that: 1) typical architecture of a GHC user has multiple cores; 2) switch to ``-threaded`` will *most likely* improve performance in such a setting. In contrast, the users of single-threaded acrchitectures might observe certain degradations upon updating to a newer GHC, and we should make sure to advertise the option to opt-out (via ``-single-threaded``) in the release notes.

Implementation has a very low cost and mostly concern with figuring out neccessary adaptations in the GHC test suite.


Alternatives
------------
The alternative is status quo: deafult to the non-threaded RTS. This is a plausible option but feels outdated as of now.

There is also a minor concern about the fallback flag name. Possible options taht have been suggested so far are ``-single-threaded`` and ``-non-threaded``.


Unresolved Questions
--------------------
None


Implementation Plan
-------------------
The implementation is started in `!538 <https://gitlab.haskell.org/ghc/ghc/merge_requests/538>`_. The main challenge is to get test suite pass because of certain warnings arising after the switch to the new default.

