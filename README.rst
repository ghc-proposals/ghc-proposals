GHC Proposals
=============

This repository contains specifications for proposed changes to the
`Glasgow Haskell Compiler <https://www.haskell.org/ghc>`_. 
The purpose of the GHC `proposal process <./README.md#Proposals>`_, and of the GHC
Steering Committee, is to broaden the discussion of the evolution of GHC.

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

Where can I find proposals currently under discussion?
------------------------------------------------------

Proposal discussion occurs on GitHub pull requests. See the
`Pull Requests list </ghc-proposals/ghc-proposals/pulls>`_ list for the list of
active discussions. To see the text of the proposal click on the *Files Changed*
tab on corresponding Pull Request page.

How do I comment on a proposal?
-------------------------------

To comment on a proposal you need to be viewing the proposal's diff in "source
diff" view. To switch to this view use the buttons on the top-right corner of
the *Files Changed* tab.

.. figure:: rich-diff.png
    :alt: The view selector buttons.
    :align: right

    Use the view selector buttons on the top right corner of the "Files
    Changed" tab to change between "source diff" and "rich diff" views.

Feedback on a open pull requests can be offered using both GitHub's in-line and
pull request commenting features. Inline comments can be added by hovering over
a line of the diff.

.. figure:: inline-comment.png
    :alt: The ``+`` button appears while hovering over line in the source diff view.
    :align: right

    Hover over a line in the source diff view of a pull request and
    click on the ``+`` to leave an inline comment

For the maintenance of general sanity, try to avoid leaving "me too" comments.
If you would like to register your approval or disapproval of a particular
comment or proposal, feel free to use GitHub's "Reactions"
`feature <https://help.github.com/articles/about-discussions-in-issues-and-pull-requests>`_.

How do I submit a proposal?
---------------------------

See the `Proposal Submission <proposal-submission.rst>`_ page.

How are proposals decided upon?
-------------------------------

Proposals are ultimately evaluated by the `GHC Steering Committee
<steering-committee.rst>`_ based upon a number of criteria and in light of
community feedback.

Who do I contact if I have questions about the process?
-------------------------------------------------------

Feel free to contact any of the members of the `GHC Steering Committee
<steering-committee.rst>`_ with questions. Email and IRC (``#ghc`` on
``irc.freenode.net``) are both good ways of accomplishing this.
