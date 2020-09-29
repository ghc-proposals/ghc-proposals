How to Accept a Proposal
========================

This document describes the technical process for accepting a proposal,
assuming the proposal has gone through the `proposal process <https://github.com/ghc-proposals/ghc-proposals/#committee-process>`_.

Acceptance checklist
--------------------

Before accepting, the shepherd should check for the following
(extracted from the `review criteria <https://github.com/ghc-proposals/ghc-proposals/#review-criteria>`_):

1. Is the proposal self-standing? It should not refer explicitly to the GitHub
   trail, for instance.

2. Is the proposal precise? The specification section should fully describe
   the proposal payload. In particular:

   a. If there is new syntax, is it described in BNF notation?

   b. If applicable, are there typing rules? If there are not, could
      typing rules be reasonably extracted from the proposal text?

   c. Are any changes to runtime behavior precisely specified?

   d. Is there a reasonable description of how the new feature might
      be documented in Haddock? Linted by HLint? Treated by other tools?
      (These will not apply to every proposal.)

3. Is the proposal sufficiently illustrated with examples? Examples help
   us understand the details that will be needed during implementation.

Acceptance steps
----------------

A committee member accepts a proposal by following this sequence of
steps:

1. Add a new commit on top of the PR branch that:

   a. Changes the filename of the proposal to correspond to the PR number.

   b. Updates any metadata fields that may have changed in the template on ``master`` since
      the PR branch split off.

   c. Fills in these metadata fields as appropriate, including changing "is discussed"
      to "was discussed".

2. Merge the PR branch into master, and push.

3. Update the PR description to start
   with the text "The proposal has been accepted; the following discussion is mostly of historic interest."
   where the word "proposal" links to the final rendered version, as found on https://github.com/ghc-proposals/ghc-proposals/tree/master/proposals

4. If the PR title has "(under review)", remove it.
   
5. Set the PR to have the "Accepted" label.

6. Comment on the PR that the proposal was accepted.

7. Close the PR if GitHub has not detected the merge.

8. Announce on the committee mailing list.
