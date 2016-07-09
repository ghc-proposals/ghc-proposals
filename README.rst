GHC Haskell proposals
=====================

.. note::
    Currently the process that goes along with this repository is itself in the
    proposal stage. See the first proposal,
    `Pull Request #1 <https://github.com/bgamari/ghc-proposals/pull/1>`_, for details. 


This repository contains specifications for proposed changes to the
`Glasgow Haskell Compiler <http://ghc.haskell.org/>`_.

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
Pull Requests tab in Github to see a listing of the proposals currently under
discussion. To see the text of the proposal click on the "Files Changed" tab on
corresponding Pull Request page.

Github offers two ways of viewing diffs: the "source diff" view, which shows a
plain text diff, and the "rich diff" view, which provides a more readable
rendered view of the markup. The view can be selected using the buttons on the
top-right corner of the "Files Changed" tab.

.. image:: rich-diff.png
     :alt: Use the view selector buttons on the top right corner of the "Files
           Changed" tab to change between "source diff" and "rich diff" views.

Feedback on a proposal can be offered on open pull requests using both Github's
in-line and pull request commenting features.

.. image:: inline-comment.png
    :alt: Hover over a line in the Raw Source Diff view of a Pull Request and
          click on the ``+`` to leave an inline comment

How do I submit a proposal?
---------------------------

While the full process is described in the `proposal
<https://github.com/bgamari/ghc-proposals/pull/1>`_ describing the proposal
process, here is a short summary,

1. Clone this repository and create a branch in your clone for your new proposal.

2. Starting with the `template <https://github.com/bgamari/ghc-proposals/blob/master/0000-template.rst>`_,
   describe your proposed change in a file in the ``proposals/`` directory.

3. When you feel your proposal document is complete, open a ``Pull Request``
   requesting that your branch be merged into the ``master`` branch of this
   repository. Be sure to include a link to the rendered view of your proposal
   in the pull request description. Your proposal will automatically be
   announced on the ``ghc-devs`` mailing list when this pull request is opened.
4. Discussion will proceed on the pull request; it is very likely that multiple

   iterations will be necessary before the proposal stabilizes.

5. When discussion has died down email the (yet to be formed) GHC Commitee , who
   will review the proposal, the feedback collected on the pull request, and
   general community sentiment and decide whether the proposal will be accepted.

6. When your proposal is accepted your pull request will be merged. At this
   point you or someone else may choose to implement your proposal.
   
