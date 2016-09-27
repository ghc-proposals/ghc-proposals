GHC Proposals
=============

.. note::
    Currently the process that goes along with this repository is itself in the
    proposal stage. See the first proposal,
    `Pull Request #1 <https://github.com/ghc-proposals/ghc-proposals/pull/1>`_, for details. 


This repository contains specifications for proposed changes to the
`Glasgow Haskell Compiler <https://www.haskell.org/ghc>`_.

What is a proposal?
-------------------

A GHC Proposal is a document describing a proposed change to the compiler, the
GHC/Haskell language, or the libraries in the ``GHC.*`` module namespace. These
include,

* A syntactic change to GHC/Haskell (e.g. the various ``ShortImports``
  `proposals <https://ghc.haskell.org/trac/ghc/ticket/10478>`_, ``do``
  `expressions <https://ghc.haskell.org/trac/ghc/ticket/10843>`_ without ``$``)

* A major change to the user-visible behaviour of the compiler (e.g. the recent
  `change <https://ghc.haskell.org/trac/ghc/ticket/11762>`_ in super-class
  solving, and ``-Wall`` `behavior <https://ghc.haskell.org/trac/ghc/ticket/11370>`_)

* The addition of major features to the compiler (e.g. ``-XTypeInType``, GHCi
  `commands <https://ghc.haskell.org/trac/ghc/ticket/10874>`_,
  `type-indexed <https://ghc.haskell.org/trac/ghc/wiki/Typeable>`_
  ``Typeable`` representations)

How do I comment on a proposal?
-------------------------------

Discussion on a proposal occurs on its associated pull request. Click on the
Pull Requests tab in GitHub to see a listing of the proposals currently under
discussion. To see the text of the proposal click on the "Files Changed" tab on
corresponding Pull Request page.

Github offers two ways of viewing diffs: the "source diff" view, which shows a
plain text diff, and the "rich diff" view, which provides a more readable
rendered view of the markup. The view can be selected using the buttons on the
top-right corner of the "Files Changed" tab.

.. figure:: rich-diff.png
    :alt: The view selector buttons.
    :align: right

    Use the view selector buttons on the top right corner of the "Files
    Changed" tab to change between "source diff" and "rich diff" views.

Feedback on a proposal can be offered on open pull requests using both Github's
in-line and pull request commenting features.

.. figure:: inline-comment.png
    :alt: The ``+`` button appears while hovering over line in the source diff view.
    :align: right

    Hover over a line in the source diff view of a pull request and
    click on the ``+`` to leave an inline comment

How do I submit a proposal?
---------------------------

While the full process is described in the `proposal
<https://github.com/ghc-proposals/ghc-proposals/pull/1>`_ describing the proposal
process, here is a short summary,

1. Fork this repository and create a branch in your fork for your new proposal.

2. Starting with the `template
   <https://github.com/ghc-proposals/ghc-proposals/blob/master/0000-template.rst>`_,
   describe your proposed change in a file in the ``proposals/`` directory. Use
   a filename of the form ``proposals/0000-short-proposal-title.rst``; leave the
   proposal number as ``0000``, it will be chosen when the proposal is
   accepted.

3. Write the proposal. At very least this should describe the following,

   a. *Motivation*: What is the problem that you are trying to solve? Be specific:
      a concrete example or two can do wonders. Be sure to point out out the
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

4. When you feel your proposal document is complete, open a Pull Request
   requesting that your branch be merged into the ``master`` branch of this
   repository. Be sure to include a link to the rendered view of your proposal
   in the pull request description. Your proposal will automatically be
   announced on the ``ghc-devs`` mailing list when this pull request is opened.

5. Discussion will proceed on the pull request; it is very likely that multiple
   iterations will be necessary before the proposal stabilizes.

6. When discussion has died down notify the (yet to be formed) GHC Commitee via
   email. The committee will will review the proposal, the feedback collected on
   the pull request, and general community sentiment and decide whether the
   proposal will be accepted.

7. When your proposal is accepted your pull request will be merged. At this
   point you or someone else may choose to implement your proposal.
   
