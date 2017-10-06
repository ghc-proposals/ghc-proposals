.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: `#14324 <https://ghc.haskell.org/trac/ghc/ticket/14324>`_

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_. **After creating the pull request, edit this file again, update the number in the link, and delete this bold sentence.**

.. contents::

Deprecating STM invariant mechanism
===================================

The invariant-checking mechanism provided by GHC's ``stm`` library is buggy and
appears to be largely unused. Let's remove it.


Motivation
------------

GHC's STM subsystem has long had the ability to run user-specified invariant
checks when committing transactions, embodied by the ``always`` and
``alwaysSucceeds`` functions of ``Control.Monad.STM``.

However, if Hackage is any indication this feature has seen very little
use of the past ten years. In fact, it has very likely been quite broken
(see `#14310 <https://ghc.haskell.org/trac/ghc/ticket/14310>`_) for this entire
duration yet no one noticed. Moreover, the
mechanism is likely mediocre at achieving its goal of catching broken
invariants due to odd interleavings of transactions as its
implementation is extremely sychronization-heavy. Additionally, the
mechanism currently fails to handle some corner-cases correctly
(`#7930 <https://ghc.haskell.org/trac/ghc/ticket/7930>`_).

In short, the feature is complex, buggy, and not pulling its weight.


Proposed Change Specification
-----------------------------
For GHC 8.4.1 we mark ``Control.Monad.STM.always`` and
``Control.Monad.STM.alwaysSucceeds`` with ``DEPRECATED`` pragmas. These
interfaces will be removed three major releases later.


Effect and Interactions
-----------------------
This will allow us to remove the invariants implementation from the runtime
system.

Current users of the interface have a few options to adapt their code to
manually call invariant their checks where necessary.


Costs and Drawbacks
-------------------
The removal of the interfaces will break any existing users.

Alternatives
------------
Not removing the interface.


Unresolved questions
--------------------
How many users does this interface have? Is it crucial to their programs' functions?


Implementation Plan
-------------------
I, Ben Gamari, will implement this.
