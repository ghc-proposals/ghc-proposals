.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_. **After creating the pull request, edit this file again, update the number in the link, and delete this bold sentence.**

.. contents::

Proposal NoImplicitExport
==============

Much of the community has settled around the idea that implicit exports are a misfeature. Disabling implicit exports is a cheap method of amending this situation. This feature pragma is intended to be used in ``default-extensions`` and applied project wide.


Motivation
------------

Explicit exports allow GHC and other external tooling to identify dead code, ease optimization, and reduce linking surface. As well implicit exports tend to enable pollution of the global name-space causing authors to create more byzantine naming schemes. By removing implicit exports we open up a wide surface of improvement at the cost of programmer book keeping.


Proposed Change Specification
-----------------------------
This proposal introduces a new language pragma ``NoImplicitExport``.

When enabled, open module exports are reinterpreted as empty.

The following statement:

::
 module Foo where

becomes:

::
 modules Foo () where

This pragma is treated as a syntactic extension and only effects modules within the given project.

Files that do not explicitly export produce a compiler warning:

::
 Warning: module "Foo" does not produce any exports.

 module Foo where
        ^^^


Effect and Interactions
-----------------------
Limiting the effect of this pragma to the given project should cause little concern for side effect, other than programmer surprise. This can be mitigated by a compiler warning.


Costs and Drawbacks
-------------------
This pragma presents very little cost for maintenance. It will shift the burden of export maintenance to the user.


Alternatives
------------
External tooling like ``hlint`` could cover this case, but linting is often opt in and produces more complexity with CI.


Unresolved questions
--------------------
n/a


Implementation Plan
-------------------
n/a
