Multiple Home Packages (multi-package GHCi)
==============

.. author:: John Ericson (@Ericson2314)
.. co-author:: Ken Micklas (@kmicklas)
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

Allow GHC to work with multiple packages at once.
Most importantly to the end user, this allows proper developing of multiple packages in GHCi.
There are other workflows which can benefit from such functionality, such as incremental compilation and Haskell IDE Engine.

Most broadly, this is step towards the principle that division of compilation "workers"
(e.g. GHC processes, GHC "sessions" within each process)
shouldn't be tied to division of work
(e.g. defintions, modules, or packages, etc to be compiled, pipeline stages done so far).
The structure and originization of code is too important to compromise for how implementations today consume it.

Motivation
----------

Haskell prides itself on extreme composibility.
Programs should and indeed are written in terms of the fine-grained composition of numerous abstractions.
Yet various limitations of Haskell tooling limit the potential of ecosystem in this regard.
On one hand, it is much easier to work on a single library than multiple at once.
On the other, the better the abstraction is the "farther" (module, component, package, etc) it ought to live from its use-site.

Multi-package GHCi destroys the contradiction by allowing as many libraries (the biggest unit of code) to be loaded in GHCi (the fastest debug cycle) as one likes.
Indeed it is already popular today.
Stack emulates it to the best of its ability today.
TODO link or something brief.
It is also a requested feature for Cabal `<https://github.com/haskell/cabal/issues/3659>`_.

The problem is it's *only* emulatable today.
A GHCi session is tied to a GHC session, which only supports one "home package", i.e. package which can be recompiled dynamically.
TODO more.

Proposed Change Specification
-----------------------------

TODO Reference `#10827`_.

Specify the change in precise, comprehensive yet concise language. Avoid words
like "should" or "could". Strive for a complete definition. Your specification
may include,

* grammar and semantics of any new syntactic constructs
* the types and semantics of any new library interfaces
* how the proposed change interacts with existing language or compiler
  features, in case that is otherwise ambiguous

Note, however, that this section need not describe details of the
implementation of the feature or examples. The proposal is merely supposed to
give a conceptual specification of the new feature and its behavior.

Examples
--------
This section illustrates the specification through the use of examples of the
language change proposed. It is best to exemplify each point made in the
specification, though perhaps one example can cover several points. Contrived
examples are OK here. If the Motivation section describes something that is
hard to do without this proposal, this is a good place to show how easy that
thing is to do with the proposal.

Effect and Interactions
-----------------------

In the GHC wiki page `Multi Session GHC API`_ it was originally proposed that multiple GHC sessions be able to exist in one process.
This was a large component of a number of changes to make the GHC API more flexible, and better able to support Haskell IDE Engine.
This is still a good change, but multiple packages within one session largely supplants it.
While the former is great for developing two indepent packages, it doesn't work well when one depends on the other:
The upstream one still must be completely built to be loaded, normally, by the session for the downstream one.
With multiple packages per session, one must merely parse and analyze both packages' modules alike.

Detail how the proposed change addresses the original problem raised in the
motivation.

Discuss possibly contentious interactions with existing language or compiler
features.


Costs and Drawbacks
-------------------
Give an estimate on development and maintenance costs. List how this effects
learnability of the language for novice users. Define and list any remaining
drawbacks that cannot be resolved.


Alternatives
------------
List existing alternatives to your proposed change as they currently exist and
discuss why they are insufficient.

Approximations:

- Stack
- Obelisk

TODO: why they are insufficient.


Unresolved Questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and
specification. Be upfront and trust that the community will help. Please do
not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to
the steering committee.


Implementation Plan
-------------------

The generalizing of HscEnv has begun in `!935`_.
TODO future steps.

.. _Multi Session GHC API: https://gitlab.haskell.org/ghc/ghc/wikis/Multi-Session-GHC-API
.. _#10827: https://gitlab.haskell.org/ghc/ghc/issues/10827
.. _!935: https://gitlab.haskell.org/ghc/ghc/merge_requests/935
