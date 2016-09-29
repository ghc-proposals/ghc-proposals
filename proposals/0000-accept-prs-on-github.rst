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

Lack of this feature to our workflow has been cited many times as a barrier to
contributors. So let's do it!

Proposed Change
---------------

* Start welcoming PRs on GH.

From there, we have two choices:

1. Automatically migrate all GH PRs to become Differentials in Phab. Users submitting
   a PR on GH will be politely informed that their PR has been migrated to Phab, along
   with instructions on how they can follow the PR.

2. Create a tool (I will call it ``ghc-hub``) that GHC HQ (only) will use. This tool
   allows migration of a GH PR to a Diff on Phab. GHC HQ will migrate "large" patches
   (for an evolving definition of "large") to Phab and keep "small" patches on GH.
   Generally, "small" patches will be accepted as-is; back-and-forth on a patch would
   take place on Phab.

Michael Sloan has volunteered to write ``ghc-hub`` or the automatic migration tool mentioned
in option (1).

Regardless of which option above:

* Accept small, imperfect PRs from new contributors.

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

* Extra work for GHC HQ in dealing with patches. To mitigate this, we could
  consider dropping support for submitting patches via Trac.

* More tooling (``ghc-hub``) that needs to be maintained.

* Potential confusion for users submitting a PR on GH and then being re-routed
  to Phab.

Alternatives
------------

Don't do this: continue preventing PRs on GH. This requires contributors to
learn about Phab (or Trac) before submitting a patch.


Unresolved Questions
--------------------

* How many contributions will we get via GH?
