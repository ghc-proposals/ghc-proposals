Proposal Submission
===================

While the full process is described in the `proposal
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0000-proposal-process.rst>`_ describing the proposal
process, here is a short summary,

1. Edit the `template
   <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0000-template.rst>`_
   to reflect your proposal. Rename the template file according to your
   desired proposal, but leave the numerical prefix as ``0000``.
   This can be done either online through GitHub's in-place
   editing feature (the pencil icon visible when viewing a file on GitHub)
   or by forking this repository and cloning the fork.
   See GitHub's `documentation
   <https://help.github.com/articles/fork-a-repo/>`_ if you are unfamiliar with
   this aspect of GitHub's workflow.

3. Write the proposal. At very least this should describe the following,

   a. *Motivation*: What is the problem that you are trying to solve? Be specific:
      a concrete example or two can do wonders. Be sure to point out the
      particular ways in which the status quo falls short.
   b. *Proposed Change*: What change are you proposing? This is the
      specification of your change and should be precise and comprehensive. This
      might include,

      * grammar and semantics of any new syntactic constructs
      * the types and semantics of any new library interfaces
      * how the proposed change addresses the original problem
        (perhaps returning to the concrete examples introduced in the
        *Motivation* section).
      * how the proposed change might interact with existing language or
        compiler features

      This generally needn't discuss the implementation of the change.
   c. *Drawbacks*: There's no such thing as a free lunch; what is the cost of
      your proposal? This includes the cost of implementing the proposal,
      mainintaining it indefinitely, teaching it to new users, and considering
      its interaction with future proposals.
   d. *Alternatives*: What alternatives to the proposed change exist? These can
      range from minor variants to completely . This doesn't need to go into
      great detail, just give the reader a sketch of the design space.
   e. *Unresolved questions*: What issues area still outstanding in the design?
      This needn't discuss outstanding *implementation* questions, we are
      presently only concerned with the conceptual design of your idea.

   Note that proposals are written in `ReStructuredText
   <http://www.sphinx-doc.org/en/stable/rest.html>`_ rather than Markdown for
   its expressiveness and ease of integration into other GHC infrastructure.
   See the `GHC Users Guide
   <http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/editing-guide.html>`_
   for a brief introduction to ReStructuredText.

4. When you feel your proposal document is complete, push your branch to your
   fork (e.g. ``git push origin type-indexed-typeable``), and open a Pull
   Request requesting that your branch be merged into the ``master`` branch of
   the ``ghc-proposals/ghc-proposals`` repository. If you unfamiliar with
   GitHub pull requests then see the `relevant documentation
   <https://help.github.com/articles/creating-a-pull-request/#creating-the-pull-request>`_.

   Be sure to include a link to the rendered view of your proposal in the pull
   request description. Your proposal will automatically be announced on the
   ``ghc-devs`` mailing list when this pull request is opened.

5. Discussion will proceed on the pull request; it is very likely that multiple
   iterations will be necessary before the proposal stabilizes.

6. When discussion has died down notify the `GHC Commitee
   <steering-committee.rst>`_ with a comment mentioning ``@bgamari``. The
   committee will review the proposal, as well as the feedback collected on the
   pull request and general community sentiment, and decide whether the proposal
   will be accepted.

7. When your proposal is accepted your pull request will be merged. At this
   point you or someone else may choose to implement your proposal.
