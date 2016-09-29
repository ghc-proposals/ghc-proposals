.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

Add an "auto-commit" feature to Phab
====================================

Allow contributors with commit rights to push a diff to Phab that will be merged
to master once it validates. A failed validate or merge will abort the commit
and email the contributor.

Motivation
----------

Suppose I (a regular contributor with commit rights)
have a patch ready to commit. It is either minor in scope or has already
undergone a review process and has received approval from the reviewers.

I have two choices:

1. Merge to master and push. There may have been some intervening commits
   since I last validated and this might break validation.

2. Push to Phab and wait for validation. Even if it validates, I may task
   switch before validation is complete, and then I can't get around to
   merging until tomorrow. Then, I have these same choices again.

By having Phab do the merge and push for me, I can have my cake (that is,
validate) and eat it too (get my patch into master).
   
Proposed Change
---------------

Add a mechanism to Phab that allows a merge to master and upstream push
the moment a patch validates. If anything goes wrong with the merge or validation,
this process is aborted.

Merging would be done at the branch level and would not squash commits. That
is, it would take the state of a git branch as an input and then merge that
with master (rebasing if necessary) upon successful validation.

The precise mechanism should be filled in by someone with more knowledge of
what would be easiest to implement. Do you know of such a mechanism? Please
submit a PR against this PR!

Note that this is entirely an opt-in feature and not meant to circumvent the
usual review process. This new feature should be used only as an alternative
to a merge-and-push.

Drawbacks
---------

* It's possible that the merge succeeds but that validation still breaks.
  Perhaps it would be better to abort the auto-commit if any intervening
  commit touches the same files.

* This does not help those without commit rights. However, if a committer
  spots a patch that s/he wants to accept, then perhaps this feature could
  be used to get Phab to do a final validate and then push.

Alternatives
------------

Don't do this, keeping us with choosing between (1) and (2), above. Not having
this auto-commit feature tangibly decreases my rate of contributions to GHC.

Unresolved Questions
--------------------

* Is this feasible to implement?

* What mechanism is best?
