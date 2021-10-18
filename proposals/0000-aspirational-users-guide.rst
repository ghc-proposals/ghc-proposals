Codify proposals in an "Apsirational User's Guide"
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

This is a meta-proposal to try out an extension to the proposal process on an experimental basis.

We currently propose changes to an *implicit* state, the current design that is a roll-up of GHC and all unimplemented, accepted proposals.
Instead, we should make that state *explicit* by codifying excepted proposals in a design document, which is to be copy of the GHC User's Guide known as the "Aspirational User's Guide".
This will catch subtle design interactions earlier than implementation time, and make it easier to evaluate new proposals against an accurate baseline anyone can read.

Motivation
----------

Understanding proposals in context
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As stated in the summary, we currently propose changes against...nothing in particular.
The proposals are meant to clearly specify a change, which they do, but *what* is being changed is not specified so clearly.
"The design", yes, but where is it?
In our heads.

Keeping track of this design is a lot of hard mental labor, even for our best experts including those on the steering committee.

A recent example of this is the situation we recently got into with a few pattern- and scoping-related proposals.
Joachim's `email`_ to the GHC Steering Committee list about this issue is very instructive.
There are 3 accepted but unimplemented proposals and 3 open proposals all touching on related portions of the design, and the result is very hard to follow.

In this situation, the solution adopted was that Richard would go make a mega-proposal combining all of them.
That's fine, but also a solution we perhaps could only arrive at post-hoc.
Are we, ever time we find that some proposals need tighter integration, going to have to re-propose them?
Wouldn't it be nice if we could still proposal incremental changes, but also track their ramifications explicitly?
If we could catch subtle interactions earlier we could do that.

.. _`email`: https://mail.haskell.org/pipermail/ghc-steering-committee/2021-August/002571.html

Taking baby steps by making this an experiment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An earlier draft of this proposal went directly to changing the proposal process, but based on `Richard's response`_ to a comment of mine alluding to that prior plan, I decided to scale the proposal back.
Different processes do indeed burden different classes of participants in different ways (think author vs committee burden trade-offs).
And we shouldn't make any drastic changes without a trial run period, or we risk a big "blow back" if something goes wrong.

The current form of this proposal thus instead has us maintain the "Aspirational User's Guide" alongside the existing proposal process, with no mandatory synchronization points between them.
If the experiment goes well, I hope people will be inspired to keep it up to date and we can more formally include the Aspirational User's Guide in the proposal process.
If the experiment becomes a burden no one wants to deal with, this rightly goes back to the drawing board...or the dumpster.

.. _`Richard's response`: https://github.com/ghc-proposals/ghc-proposals/pull/283#issuecomment-924218834

Background info
---------------

Prior art
~~~~~~~~~

I am no lawyer, but my understanding is that the new task I am proposing is similar to the codification of law.
(The `English Wikipedia article`_ has some information, I am sure a lawyer could point us towards other descriptions.)
Very crudely, as I understand it, bills/acts/edicts/whatever are "informal patches" that might also contain "side effects" (one-off acts of governments).
Humans separate the two parts and apply the first to another document.
The process is somewhat asynchronous, at least historically, so conflicts/mistakes/ambiguities are expected.
While I don't think GHC proposals typically have the "side effects" part to sieve out, the rest of the process closely mirrors what is proposed below in spirit.

.. _`English Wikipedia article`: https://en.wikipedia.org/wiki/Codification_(law)

Prehistory of our proposal process
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Our proposal process is inspired by the `Rust proposal process`_.
Rust's RFC process was `proposed <https://mail.mozilla.org/pipermail/rust-dev/2014-March/008973.html>`_ as a formalization of the existing pattern of tagging some emails to the mailing list "RFC:".
The trail after that is a but fuzzier, but I think it's safe say it eventually goes back to the original Internet Engineering Task Force (IETF) Request for Comments (and, yes, its ARPANET antecedents).

This and its descendants all have a *lack* of any codification process in common.
But, unlike the Rust and GHC proposals which govern a single implementation, The IETF ones describe a bunch of protocols shared between many different implementations, and certainly no single implementation implementing all of them.
As such, it's a *good* thing the RFCs are relatively standalone, listing the just the priors RFC they build upon or supersede, because for the purpose of protocol specification *modularity is more important than cohesiveness*.

Of course, we don't want e.g. every language extension to only make sense in the full context of every other, so there is no initial thread for beginners to pull.
But that doesn't negate that fact that we are planning for *one* GHC, and it is very important that we understand the interactions of our design decisions up front, and without having to understand all the implementation details.

So zooming back out, I think Rust uncritically grandfathered in a lack of codification, but that was an understandable output because their RFC process evolved rather organically.
We, in turn, were looking to emulate Rust's success with it, so it made sense we also would basically take the process as-is.
But Rust is a more centralized project than Haskell, especially back then with Mozilla being the main facilitator, so while I think a lack of a central design document is bad for both compilers, it makes sense the issues could be more felt here.

.. _`Rust proposal process`: https://www.haskell.org/ghc/blog/20160709-rethinking-proposals.html

Proposed Change Specification
-----------------------------

A copy GHC users guide is to be split out of the GHC repo into its own repo.
That copy will be known as the "Aspirational User's Guide" --- the user's guide for the GHC we *wish* we had.

Accepted proposals should be "codified" by documenting them as if they had already been implemented in the Aspirational User's Guide.
The GHC steering committee would ultimately be responsible for this, in order to not burden proposal authors during the course of the experiment, but proposal authors are encoraged to help out.

In order to have a good starting point for proposals with this new process, we also need to apply this process retroactively.
The steering committee should codify the changes specified in accepted but unimplemented proposals (already-implemented ones should already be in the initial split-out repo) in the Aspirational User's Guide.
This is the first "prelude" phase of the experiment; if getting the Aspirational User's Guide up to date with the accepted but unimplemented proposals proves too arduous, we'll have to rethink this experiment.

When proposals are implemented, their codification in the Aspirational User's Guide should be synchronized with real GHC User's Guide as part of the implementation.
Beyond saving work, this helps ensure that the two user's guides don't drift apart without bound.
Additionally, other changes to the real User's Guide (not corresponding to implemented proposals) should be synced back to the Aspiration version in this repo for the same reason.

In general, the difference between them should be kept to a minimum --- were the difference between them to become too vast, that would serve as a good "canary in the coal mine" that we have too many accepted but unimplemented proposals.

``git subtree`` is suggested to be used to split and sync the two users guides so the history is intact for e.g. ``git blame`` purposes in both repos.

Examples
--------

If this proposal gets near approval, I would be happy to demo the codification process with some accepted but implemented proposals.

Effect and Interactions
-----------------------

#. Proposals that remain unimplemented too long will impose a burden in more merge conflicts with the two User's Guides synchronization.
   We might need to create a process to unaccept such proposals if this becomes to burdensome.

#. The current User's Guide might not go into enough depth to adequately describe the relevant portion of the design prior to some proposal.
   In that case, the author of the proposal could first improve the User's Guide to better describe the status quo so the subsequent diff is meaningful.
   This improves the quality of the documentation of GHC regardless of whether the proposal is accepted.

Costs and Drawbacks
-------------------

#. During the experimental phase, there will be more burden on the steering committee codifying proposals.
   (If we wish to stick with the experiment, we can shift that burden around, but it might not go away.
   That said, I have some optimism that forking the Aspirational User's Guide *before* one writes the proposal could make writing the proposed change specification easier, as one doesn't need to remember as much and can just "follow the text")

#. Managing any merge conflicts with ``git subtree`` might be too arcane.
   But hopefully in most cases those merge conflicts are just materializing the design interactions we need to adjudicate anyways, so it's better we catch them than not, even if the process is a bit annoying.

#. Mentioning issue numbers etc. in commits can wreck havoc if there are multiple issue/PR counters -- something we saw when Hadrian was merged in GHC with ``git subtree``.
   This can be avoided by using git-filter-branch to modify commit messages.
   It should be possible to make that process idempotent so that the extracted subtree commits are in fact properly shared in both repos and over-subsequent splits/merges.

Alternatives
------------

#. The design document proposals could be a greenfield document rather than a fork of the users guide.
   But this just strikes me as more work: now we have a distinct Users Guide and design document that cannot be synchronized semi-automatically via Git.
   It also means we lose out on the side benefit of a regular users not interested in the proposal process getting better-maintained documentation.

#. Skip pure-side experiment and start requiring something of proposals / proposal authors too.
   In order of increasing severity:

   #. Proposal is not truly approved until codified (first approval is tentative).

   #. Initial draft of codification must be done by proposal author.

   #. Codification and proposal must be submitted together.
      While the most draconian, this does have the advantage that we could slim down or remove the "detailed design" section to avoid duplicate work.

   For the 3rd option, we would probably want to permanently merge the Apsirational User's Guide into the proposals repo.

Unresolved Questions
--------------------

None at this time.

Implementation Plan
-------------------

I will happily assist the steering committee with modifying the proposal templates, figuring out how exactly to sync the User's Guide via git, and codifying existing proposals.

Endorsements
-------------

None at this time.
