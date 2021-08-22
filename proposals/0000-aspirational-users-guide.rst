Coordinate proposals with an "Apsirational User's Guide"
==============

.. author:: John Ericson
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

This is a meta-proposal to change the proposal format and process.
We currently propose changes to an *implicit* state, the current design which is a roll-up of GHC and all unimplemented, accepted proposals.
Instead, we should propose changes to an *explicit* design document, which is a copy of the GHC User's Guide known as the "Aspirational User's Guide".
This will catch subtle design interactions earlier at proposal time rather than implementation design, and encourage holistic rather than piecemeal planning.

Motivation
----------

As stated in the summary, we currently propose changes against...nothing in particular.
The proposals are meant to clearly specify a change, which they do, but what is not specified clearly is *what* is being change.
"The design", yes, but where is it? In our heads.

Keeping track of this design is a lot of hard mental labor, even for our best experts and those making the decisions.

An example of this is the situation we recently got into with a few pattern- and scoping-related proposals.
Joachim's [email] to the GHC Steering Committee list about this issue is very instructive.
There are 3 accepted but unimplemented proposals and 3 open proposals all touching on related portions of the design, and the result is very hard to follow.
In this situation, the solution adopted was that Richard would go make a mega proposal combining all of them.
That's fine, but also a lot of work.
Wouldn't it be nice if we could still proposal incremental changes but also track their ramifications without "applying patches" mentally?

[email]: https://mail.haskell.org/pipermail/ghc-steering-committee/2021-August/002571.html

Proposed Change Specification
-----------------------------

The GHC users guide is be copied into this repo, where it will be known as the "Aspirational User's Guide" --- the user's guide for the GHC we wish we had.
Proposals will be co-written with the changes to the users guide they wish to effect, and the "proposed changed specification" and "effects and interactions" sections will instead be glosses that point to the changed sections.
The can thus be shorter because the Aspirational User's Guide alone is authoritative.

In order to have a good starting point for proposals with this new process, we also need to apply this process retroactively.
The steering committee in conjunction with proposal authors will codify the changes specified in accepted but unimplemented proposals (already-implemented ones should already be in the initial copy) in the Aspirational User's Guide.
Thus, the Aspirational User's Guide will become up-to-date with the committee's decisions to this point.

When proposals are implemented, those changes should be synchronized with real GHC User's Guide as part of the implementation.
Additionally, other changes to the real User's Guide not corresponding to implemented proposals should be synced back to the Aspiration version in this repo.
In general, the difference between them should be kept to a minimum.

``git subtree`` will be used to sync the aspirational users guide so the history is intact for e.g. ``git blame`` purposes in both repos.

Examples
--------

Implemented proposals demonstrate the changes to the users guide that should now be made up front.

Effect and Interactions
-----------------------

#. The proposal process will be harder, because some of the work that was previously deferred to the implementation must be done up front.
   It is a matter of opinion whether this front-loading is good, bad, or neutral, so I put it in this section.

#. Proposals that remain unimplemented too long will impose a burden in more merge conflicts with the two User's Guides synchronization.
   We might need to create a process to unaccept such proposals if this becomes to burdensome.

#. The current User's Guide might not go into enough depth to adequately describe the relevant portion of the design prior to some proposal.
   In that case, the author of the proposal could first improve the User's Guide to better describe the status quo so the subsequent diff is meaningful.
   This improves the quality of the documentation of GHC regardless of whether the proposal is accepted.

Costs and Drawbacks
-------------------

#. While proposals texts can still be written in Markdown, authors will need to know reStructuredText to modify the Aspirational User's Guide.
   This somewhat undermines the past decisions to allow Markdown to be used.

#. Managing the merge conflicts creates more work for the committee at large.
   But hopefully in most cases those merge conflicts are just materializing the design interactions we need to adjudicate anyways.

#. Mentioning version numbers etc. in commits can wreck havoc with GHC and proposals' issue trackers -- something we saw when Hadrian was merged in GHC as a subtree.
   This can be avoided by using git-filter-branch to modify commit messages.
   It should be possible to make that process idempotent so that the extracted subtree commits are in fact properly shared in both repos and over-subsequent splits/merges.

Alternatives
------------

#. The design document proposals could be a greenfield document rather than a fork of the users guide.
   But this just strikes me as more work: now we have a distinct Users Guide and design document that cannot be synchronized semi-automatically via Git.
   It also means we lose out on the side benefit of a regular users not interested in the proposal process getting better-maintained documentation.

Unresolved Questions
--------------------

None at this time.

Implementation Plan
-------------------

I will happily assist the steering committee with modifying the proposal templates, figuring out how exactly to sync the User's Guide via git, and codifying existing proposals.

Endorsements
-------------

None at this time.
