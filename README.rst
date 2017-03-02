GHC Proposals
=============

This repository contains specifications for proposed changes to the
`Glasgow Haskell Compiler <https://www.haskell.org/ghc>`_.
The purpose of the GHC proposal process and of
the GHC Steering Committee, is to broaden the discussion of the evolution of
GHC.

What is the timeline of a propsoal?
-----------------------------------

1. The author drafts a proposal.

   `What is a proposal? <#what-is-a-proposal>`_ • `What should a proposal look like? <#what-should-a-proposal-look-like>`_

2. The author submits the proposal to the wider Haskell community for discussion, as a pull request against this repository.

   `How to submit a proposal <#how-to-submit-a-proposal>`_

3. The wider community discusses the proposal in the commit section of the pull
   request, while the author refines the proposal. This phase lasts as long as necessary.

   `Discussion goals <#discussion-goals>`_ •
   `How to comment on a proposal <#how-to-comment-on-a-proposal>`_ •
   `List of proposals under discussion <https://github.com/ghc-proposals/ghc-proposals/pulls?q=is%3Aopen+is%3Apr+no%3Alabel>`_

4. Eventually *the proposal author* brings the proposal before the committee for review.

   `How to bring a proposal before the committee <#how-to-bring-a-proposal-before-the-committee>`_ •
   `Who is the committee? <#who-is-the-committee>`_

5. One committee member steps up as a shephard, and generates consensus within the commitee within four or five weeks.

   `Committee process <#committee-process>`_ •
   `Review criteria <#review-criteria>`_ •
   `List of proposals under review <https://github.com/ghc-proposals/ghc-proposals/pulls?q=is%3Aopen+is%3Apr+label%3A%22Pending+committee+review%22>`_

6. Eventually, the committee rejects a proposal, or passes it back to the author for review, or accepts it.

   `List of accepted proposals <https://github.com/ghc-proposals/ghc-proposals/pulls?q=label%3AAccepted>`_

Once a proposal is accepted, it still has to be implemented.  The author may do that, or someone else.
Acceptance of the proposal implies that the implementation will be accepted into GHC provided
it is well-engineered, well-documented, and does not complicate the code-base too much. 

Do not hesitate to `contact <#questions>`_ us if you have questions.

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

What should a proposal look like?
--------------------------------

Each proposal document must follow the following outline. A template is provided in `proposal template <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0000-template.rst>`_.

a. *Motivation*: Give a strong reason for why the community needs this change. Describe the use case as clearly as possible and give at least one concrete example. Explain how the status quo is insufficient or not ideal.

b. *Proposed Change Specification*: Specify the change in precise and comprehensive yet concise language. Your specification may include,

   - grammar and semantics of any new syntactic constructs
   - the types and semantics of any new library interfaces
   - how the proposed change addresses the original problem (perhaps referring back to the example)

c. *Effect and Interactions*. Detail how the proposed change addresses the original problem raised in the motivation. Detail how the proposed change interacts with existing language or compiler features. Think about what surprising or problematic interactions may occur.

d. *Costs and Drawbacks*. What are the drawbacks and costs to the community should this change be implemented? For example, does this make Haskell harder to learn for novice users?  Does it make Haskell code harder to read or reason about? Will the implementation be complex or invasive?

e. *Alternatives*: List alternatives to your proposed change and discuss why they are insufficient.

f. *Unresolved questions*: Explicitly list any remaining issues that remain in the conceptual design and specification. Be upfront and trust that the community will help. Please do not list *implementation* issues.

g. *Implementation Plan* (Optional): If accepted who will implement the change? It's quite okay to say "unknown" if you don't feel willing or able to carry out the implementation yourself.


Note that proposals are written in `ReStructuredText
<http://www.sphinx-doc.org/en/stable/rest.html>`_ rather than Markdown for its
expressiveness and ease of integration into other GHC infrastructure. See the
`GHC Users Guide
<http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/editing-guide.html>`_
for a brief introduction to ReStructuredText.

How to submit a proposal
-------------------------

If you are unfamiliar with GitHub, see the `detailed instructions <detailed-instructions.rst>`_.

1. Fork the ``ghc-proposals`` repository
2. Setup your fork,

   a. Create a branch for your proposal
   b. Start your proposal by copying ``proposals/0000-template.rst`` to a new file in the ``proposals/`` directory.

3. Write the proposal document, commit, and push to your ``ghc-proposals`` fork
4. Submit your proposal for discussion by opening a pull request for your branch against the ``master`` branch of ``ghc-proposals/ghc-proposals``.

The pull request summary should include a brief description of your
proposal, along with a link to the rendered view of proposal document
in your branch. For instance,

.. code-block::

    This is a proposal augmenting our existing `Typeable` mechanism with a
    variant, `Type.Reflection`, which provides a more strongly typed variant as
    originally described in [A Reflection on
    Types](http://research.microsoft.com/en-us/um/people/simonpj/papers/haskell-dynamic/index.htm)
    (Peyton Jones, _et al._ 2016).

    [Rendered](https://github.com/bgamari/ghc-proposals/blob/typeable/proposals/0000-type-indexed-typeable.rst)


Discussion goals
----------------

Members of the Haskell community are warmly invited to offer feedback on
proposals. Feedback ensures that a variety of perspectives are heard, that
alternative designs are considered, and that all of the pros and cons of a
design are uncovered. We particularly encourage the following types of feedback,

- Completeness: Is the proposal missing a case?
- Soundness: Is the specification sound or does it include mistakes?
- Alternatives: Are all reasonable alternatives listed and discussed. Are the pros and cons argued convincingly?
- Costs: Are the costs for implementation believable? How much would this hinder learning the language?
- Other questions: Ask critical questions that need to be resolved.
- Motivation: Is the motivation reasonable?


How to comment on a proposal
-----------------------------

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

How to bring a proposal before the committee
---------------------------------------------

When the discussion has ebbed down and the author thinks the proposal is ready, he

1. reviews the discussion thread and ensure that the proposal text accounts for
   all salient points.
2. adds a comment to the a pull request, briefly summarizing the major points raised
   during the discussion period and stating your belief that the proposal is
   ready for review. In this comment, tag the committee secretary (currently
   ``@nomeata``).

`The secretary <#who-is-the-committee>`_, will then label the pull request with
``Pending committee review`` and notify the committee. (If this does not happen
within a day or two, please ping the secretary or the committee.)

Who is the committee
--------------------

The current members of the GHC steering committe, who you can reach
by email at ghc-steering-committee@haskell.org, are:

======================  ==================================================  =========
Christopher Allen       `@bitemyapp <https://github.com/bitemyapp>`_
Manuel M T Chakravarty  `@mchakravarty <https://github.com/mchakravarty>`_
Simon Peyton-Jones      `@simonpj <https://github.com/simonpj>`_            co-chair
Simon Marlow            `@simonmar <https://github.com/simonmar>`_          co-chair
Atze Dijkstra           `@atzedijkstra <https://github.com/atzedijkstra>`_
Richard Eisenberg       `@goldfirere <https://github.com/goldfirere>`_
Iavor Diatchki          `@yav <https://github.com/yav>`_
Joachim Breitner        `@nomeata <https://github.com/nomeata>`_            secretary
Ben Gamari              `@bgamari <https://github.com/bgamari>`_
======================  ==================================================  =========

Members have terms of 3, 4, and 5 years.

Committee process
-----------------

-  Once the committee has been notified that a proposal is ready for
   decision, the committee is responsible for making the deadline known
   and notifying the wider community for comment.

-  The committee organically determines which member who will oversee
   discussion of the proposal. If organic doesn’t work, our chair(s)
   assign the proposal to a member. Membership on the committee means
   that we will volunteer to handle proposals as appropriate. The
   committee member running this discussion process is the Shepherd of
   the proposal.

-  The shepherd and the committee are *not* responsible for reading
   GitHub (or other) commentary. The proposal will be considered on its
   own. If the author wishes the committee to consider any commentary,
   that commentary should be incorporated into the proposal.
   Incorporation here means the information is summarized with
   attribution, *not* copied and pasted.

-  Once a decision is requested, the shepherd has four weeks (in holiday
   times or near the ICFP deadline, 5) to generate consensus. If
   consensus is elusive, then we vote, with the Simons retaining veto
   power. Naturally, if consensus occurs in less time, then a decision
   can be rendered early.

-  **If we say no:** the shepherd updates the proposal (not just the
   commentary) with the reasons for rejection. The pull request will be closed
   with label ``Rejected``.

   The proposer is welcome
   to revise and try again, but the document should retain this original
   rejection information.

   In the case that the proposed change has already been implemented in
   GHC, it will be reverted.

-  **If we say maybe:** If during the Debate, the need for substantial changes
   does arise, we reject the proposal in its current state and it can go back to
   Development for revision.

-  **If we say yes:** A Trac ticket is created, referring back to the
   proposal and commentary. (The shepherd is responsible for making sure
   this happens.) At this point, the proposal process is technically
   complete. It is outside the purview of the committee to implement,
   oversee implementation, attract implementors, etc.


Review criteria
---------------

Below are some criteria that the committee and the supporting GHC
community will genearlly use to evaluate a proposal. Note that this list
is merely set of a guidelines; it is the committee's job to weigh these
and any other relevant considerations appropriately.

-  *Utility and user demand*. What exactly is the problem that the
   feature solves? Is it an important problem, felt by many users, or is
   it very specialised? The whole point of a new feature is to be useful
   to people, so a good proposal will explain why this is so, and
   ideally offer evidence of some form.

-  *Elegant and principled*. Haskell is a beautiful and principled
   langauge. It is tempting to pile feature upon feature (and GHC
   Haskell has quite a bit of that), but we should constantly and
   consciously strive for simplicity and elegance.

   This is not always easy. Sometimes an important problem has lots of
   solutions, none of which have that "aha" feeling of "this is the Right
   Way to solve this"; in that case we might delay rather than forge ahead
   regardless.

-  *Fit with the language.* If we just throw things into GHC
   willy-nilly, it will become a large ball of incoherent and
   inconsistent mud. We strive to add features that are consistent with
   the rest of the language.

-  *Specification cost.* Does the benefit of the feature justify the
   extra complexity in the language specification? Does the new feature
   interact awkwardly with existing features, or does it enhance them?
   How easy is it for users to understand the new feature?

-  *Implementation cost.* How hard is it to implement?

-  *Maintainability.* Writing code is cheap; maintaining it is
   expensive. GHC is a very large piece of software, with a lifetime
   stretching over decades. It is tempting to think that if you propose
   a feature *and* offer a patch that implements it, then the
   implementation cost to GHC is zero and the patch should be accepted.

   But in fact every new feature imposes a tax on future implementors, (a)
   to keep it working, and (b) to understand and manage its interactions
   with other new features. In the common case the original implementor of
   a feature moves on to other things after a few years, and this
   maintenance burden falls on others.


Questions?
----------

Feel free to contact any of the members of the `GHC Steering Committee
<#who-is-the-committee>`_ with questions. `Email <https://wiki.haskell.org/Mailing_lists>`_
and IRC (``#ghc`` on ``irc.freenode.net``) are both good ways of accomplishing this.
