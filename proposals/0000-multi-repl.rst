Notes on reStructuredText - delete this section before submitting
==================================================================

The proposals are submitted in reStructuredText format.  To get inline code, enclose text in double backticks, ``like this``.  To get block code, use a double colon and indent by at least one space

::

 like this
 and

 this too

To get hyperlinks, use backticks, angle brackets, and an underscore `like this <http://www.haskell.org/>`_.


Multiple Home Packages (multi-package GHCi)
==============

.. author:: John Ericson
.. co-author:: Ken Micklas
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
It also has other potential development benefits, e.g. with incremental compilation and haskell-ide-engine.

Most broadly, this is step towards the principle that division of "laborers"
(e.g. GHC processes, GHC "sessions" within each process)
shouldn't be tied to division of work
(e.g. defintions, modules, or packages, etc to be compiled, pipeline stages done so far).
The structure and originization of code is too important to compromise for how implementations today consume it.

Motivation
----------

Haskell prides itself on extreme composibility.
Programs should and indeed are written in terms of the fine-grained composition of numerous abstractions.
Yet the tools push us away from this back towareds everyone else.
On one hand, it is much easier to work on a single library than multiple at once.
On the other, the better the abstraction is the "farther" (module, component, package, etc) it ought to live from its use-site.

Multi-package GHCi destroys the contradiction by allowing as many libraries (the biggest unit of code) to be loaded in GHCi (the fastest debug cycle) as one likes.
Indeed it is already popular today.
Stack emulates it to the best of its ability today.
TODO link or something brief.
It is also a requested feature for Cabal `<https://github.com/haskell/cabal/issues/3659>`_.

The problem is it's *ownly* emulatable today.
A GHCi session is tied to a GHC session, which only supports one "home package", i.e. package on the operating table.
TODO more.

Proposed Change Specification
-----------------------------

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


Unresolved Questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and
specification. Be upfront and trust that the community will help. Please do
not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to
the steering committee.


Implementation Plan
-------------------

The generalizing of HscEnv has begun in `!935`_
TODO future steps.

.. _!935: https://gitlab.haskell.org/ghc/ghc/merge_requests/935
