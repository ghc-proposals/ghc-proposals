.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

Accept PRs on GitHub
====================

This proposes to accept pull requests (PRs) against GHC on GitHub (GH). There are a few
possible mechanisms for doing so, as delineated below.

This proposal comes directly out of a `conversation <https://mail.haskell.org/pipermail/ghc-devs/2016-September/012828.html>`_
on the mailing lists.

Motivation
----------

The overhead involved in getting started with the current Phabricator
based process for submitting patches for review and merging them is
non-trivial. One needs to,

* setup an account on Phabricator,
* setup an account on Trac,
* add a new public ssh key in Phabricator,
* Install PHP then checkout the Git repository for arcanist to get the
  CLI tool for code reviews going,
* Install a new certificate in Arcanist, using ``arc install-certificate``,
* submit one or several code reviews on Phabricator using ``arc`` commands,
* update the Trac ticket.

Many potential would-be contributors don't have unwieldy patches
including large refactorings to the type inference engine or spiffy
optimizations in the RTS to propose. Many of them also don't
contribute every day, so they have to relearn the process every time.
Whereas there is a process that many such contributors (as evidenced
by the constant clamour) *do* use nearly every day, and which is
particularly simple:

.. code-block: shell

   $ git checkout -b feature-branch
   $ git commit -a -m "Commit messasge."
   $ git push
   $ hub pull-request

This process uses GitHub as the review tool and for tracking the
status of feature branches (in progress, proposed for review, merged).

By allowing pull requests, the GHC project gets to tap into the
largest social network of programmers in the world, giving it greater
visibility and allowing contributors to use a familiar workflow that
has very little overhead (no extra account to create, no additional
tool to install, no new interface to learn, no unfamiliar commands to
type).

The objective of this proposal is to make contributing to GHC easier
through the use of familiar and lightweight tools and platforms, with
a particular emphasis on encouraging first-time and occasional
contributors. It is expected that this will have a knock-on on the
pace of development of the project: more first-time contributors and
more frequent occasional contributors means more regular contributors
down the line.

It is often said that any process should instead optimize for regular
contributors. But the proposed changes below don't imply any changes
to the current workflow for existing contributors and existing
reviewers that don't want to use GitHub, so the two are not mutually
exclusive.

The point is, **how patches should be reviewed is a matter of
agreement between the contributor and the reviewer(s)**. Reviewers'
role is simply to vet the patch, something that is done systematically
in the case of contributors without commit bits. Through which vector
a patch is accepted isn't significant, provided the reviewers are
comfortable with it. GHC's release manager or some other role should
ensure that all potential reviewers are identified, and move the patch
to Phabricator if need be.

Proposed Change
---------------

1. Start welcoming PRs on GH.
2. Lock the ``master`` branch of the ``ghc`` repository on Github to avoid
   force pushes.
3. Setup automatic 2-way sync between the GitHub mirror of GHC and the
   repository in Phabricator.
4. Document on the "Contributing" page on the Trac wiki that GitHub
   pull requests are permitted. Contributors may be requested to
   migrate their pull request to Phabricator at the discretion of the
   release manager or at the request of any reviewer. Only small
   patches are likely to be accepted via GitHub.
5. Create a tool (call it say ``ghc-hub``) that GHC HQ (only) will use. This tool
   allows migration of a GH PR to a Diff on Phab. GHC HQ will migrate "large" patches
   (for an evolving definition of "large") to Phab and keep "small" patches on GH.
   Generally, "small" patches will be accepted as-is; back-and-forth on a patch would
   take place on Phab.

Michael Sloan has volunteered to write ``ghc-hub`` or the automatic migration tool mentioned
in option (1).

6. Accept small, imperfect PRs from new contributors.

We want GHC to be as close to perfect as possible, and we thus impose a high
bar on new contributors for their patches. Yet this may discourage some. If a
patch is sufficiently small and the recommended changes sufficiently
nit-picky, then GHC HQ/other committers should make the changes directly
instead of requiring back-and-forth with the contributor, especially on a
first patch. The PR could be merged to a branch, fixed up, and then merged
to master.

If the contributor is on track to becoming a regular contributor,
then reviewers will educate the contributor about our standards and how the
patches can be improved (if necessary).

Naturally, all of the above (large vs. small, nit-picky vs. substantive)
comes down to judgment calls. We do not expect others' judgments always to
line up with ours.

Drawbacks
---------

There are now two separate platforms through which reviews are
performed. This raises two concerns:

* Extra work for GHC HQ in dealing with patches. To mitigate this, we could
  consider dropping support for submitting patches via Trac.

* it is potentially more work to maintain two separate platforms and
  make sure the same validation applies to both.

* More tooling (``ghc-hub``) that needs to be maintained.

* Potential confusion for users submitting a PR on GH and then being re-routed
  to Phab.

* it becomes slightly harder to monitor the list of all proposed and
  merged patches.
  
* GH PRs get ticket numbers that look suspiciously like Trac ticket numbers.
  There is the possibility for confusion here.

It should be noted that having two review tools under active use is
not entirely `comparable
<https://mail.haskell.org/pipermail/ghc-devs/2015-September/009834.html>`_
to using multiple version control systems. Review tools don't maintain
the "ground truth" about what code goes in nor are they mutually
exclusive. It just adds a new vector for merging in commits and
assessing their quality. Of which there are already several: landing
Phabricator Diffs as Git commits, pushing Git commits on the
command-line directly to ``master``, pulling the occasional patches
via mailing lists or from the Trac issue tracker.

Alternatives
------------

The status quo is:

* Don't do this: continue preventing PRs on GH. This requires contributors to
  learn about Phab (or Trac) before submitting a patch.

Beyond sticking to the status quo, alternatives consist in either
moving *all* reviews to GitHub or conversely to permit *proposing*
a change via GitHub but conduct the review on Phabricator exclusively.
The former is far too disruptive to the existing development workflow
of regular contributors, for little gain. The way the latter works is
that a bot would automatically create a Phabricator Diff whenever
a contributor submits a pull request. The contributor then sees a link
to the new Diff, submitted as a comment in the pull request by the bot.

This latter approach is very close to the status quo: the only
difference is that GitHub users are provided a lead-in, and users can
submit a patch by pushing a Git branch rather than using Arcanist. But
they can't easily modify their patch after submitting a pull-request -
by that point if the review happens on Phabricator then it's a much
simpler workflow to use the Phabricator tools and process from
submission to merge.

Unresolved Questions
--------------------

* How many contributions will we get via GH?
* Perhaps it is in fact possible to generate new revisions in
  Phabricator automatically anytime a user updates the branch
  corresponding to a PR? This would make the alternative, i.e.
  accepting GitHub PR's but performing *all* reviews on Phabricator
  a lot more viable.
