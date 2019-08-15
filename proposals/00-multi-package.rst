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
(e.g. defintions, modules, or packages, etc. to be compiled, pipeline stages done so far).
The structure and originization of code is too important to compromise for how implementations today consume it.

Motivation
----------

Haskell prides itself on extreme composibility.
Programs should and indeed are written in terms of the fine-grained composition of numerous abstractions.
Yet various limitations of Haskell tooling limit the potential of the ecosystem in this regard.
On one hand, it is much easier to work on a single library than multiple at once.
On the other, the better the abstraction is the "farther" (module, component, package, etc) it ought to live from its use-site.

Multi-package GHCi would overcome this contradiction by allowing as many libraries (the biggest unit of code) to be loaded in GHCi (the fastest debug cycle) as one likes.
Indeed it is already popular today: Stack emulates it to the best of its ability.
Obelisk's ``ob run`` does a limited version of this also.
[See :ref:`Alternatives` for more on what Stack and Obelisk exactly do.]
It is also a requested feature for Cabal `<https://github.com/haskell/cabal/issues/3659>`_.

The problem is it's *only* unreliably emulatable today.
`#10827`_ was opened in GHC a while back explaining the problem that (originally just) Stack faced.
A GHCi session is tied to a GHC session, which only supports one "home package", i.e. package which can be recompiled dynamically.
Smashing together multiple packages as one package fails because there is one ``DynFlags`` shared across all the underlying packages.
As `#10827`_ points out, you hack with per-module ``DynFlags`` some settings specific to a certain module, but this doesn't work for settings affecting how modules *relate*.
Name uniqueness, import resolution, module visibility enforcement, etc. can't just be done by faking the module flags, but need slightly richer data structures in GHC.
Concretely, `#10827`_ points out two data structures are global causing problems, ``HomePackageTable`` and ``DynFlags``.
It tries to imagine generalizing the first but can't decide exactly how. then proposes the module hack half-heartedly.
Everyone agrees packages have different sets of ``DynFlags`` in principle, and ``HomePackageTable`` certainly purports to be a package-specific data structure too.
We propose going straight to making both per-package.

Proposed Change Specification
-----------------------------

1. Change ``HscEnv``.
   The core change boils down to replacing an ``HscEnv``-wide ``HomePackageTable`` and ``DynFlags`` with a pair of each per ``UnitId``.
   [This core change and its fallout is what `!935`_ does.]

2. Extend command line to allow setting the package-specific ``DynFlags``.
   TODO: massive bikeshed.

3. Update Cabal to take advantage of the new CLI.
   [Cabal's own syntax is easy to generalize; ``cabal new-repl foo bar`` in fact already parses.]

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

Proposal `#243`_ would isolate regular, TH quote, and TH splice code with their own binding namespace and imports.
This is mostly easily implemented by treating each stage as a separate module in conjunction with this proposal.

Costs and Drawbacks
-------------------

``HscEnv`` becomes more unwieldy, as for virtually every task not all the information within it is relevant.
I think a good solution is to make a per-package "view" which is close to its old definition.
GHC would store everything needed, but each package-specific task would work off the view instead.
In general any feature that involves changing lots of existing code is liable to create tech debt.
But with enough refactors and the benefit of hindsight, nothing should be permanent.
The requested feature here shouldn't be "inherently architecturally ugly".

Performance of changing the data structures, with or without the "view" mentioned above, remains unevaluated.
There is nothing to do but try it.

Alternatives
------------

Approximations today:

- Stack's ``stack repl``

- Obelisk's ``ob repl``

  - https://github.com/obsidiansystems/obelisk/pull/489 extends `ob run` to do more than the 3 default packages.

As mentioned in the notification, neither of these are correct, and this becomes more apparent with the number of packages one loads at once.
If multple packages are controlled by the author this can be worked around, by syncronizing the default-extensions, GHC flags, etc.
However, some of the best use-cases of multi-package repl are working on your own project while simultaneously changing upstream packages you do not control.
In this case, flag mismatch is likely to cause issues.

Unresolved Questions
--------------------

The exact command line syntax needs to be decided.

Implementation Plan
-------------------

The generalization of ``HscEnv`` has begun in `!935`_.
Hopefully that can land before the exact CLI is agreed upon.

.. _Multi Session GHC API: https://gitlab.haskell.org/ghc/ghc/wikis/Multi-Session-GHC-API
.. _#10827: https://gitlab.haskell.org/ghc/ghc/issues/10827
.. _!935: https://gitlab.haskell.org/ghc/ghc/merge_requests/935
.. _#243: https://github.com/ghc-proposals/ghc-proposals/pull/243
