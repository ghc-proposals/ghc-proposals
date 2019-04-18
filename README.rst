GHC Proposals
=============

This repository contains specifications for proposed changes to the
`Glasgow Haskell Compiler <https://www.haskell.org/ghc>`_.
The purpose of the GHC proposal process and of
the GHC Steering Committee, is to broaden the discussion of the evolution of
GHC.

What is the timeline of a proposal?
-----------------------------------

1. The author drafts a proposal.

   `What is a proposal? <#what-is-a-proposal>`_ • `What should a proposal look like? <#what-should-a-proposal-look-like>`_

2. The author submits the proposal to the wider Haskell community for discussion, as a pull request against this repository.

   `How to submit a proposal <#how-to-start-a-new-proposal>`_

3. The wider community discusses the proposal in the commit section of the pull
   request, while the author refines the proposal. This phase lasts as long as necessary.

   `Discussion goals <#discussion-goals>`_ •
   `How to comment on a proposal <#how-to-comment-on-a-proposal>`_ •
   `≡ List of proposals under discussion <https://github.com/ghc-proposals/ghc-proposals/pulls?q=is%3Aopen+is%3Apr+no%3Alabel>`_

4. Eventually *the proposal author* brings the proposal before the committee for review.

   `How to bring a proposal before the committee <#how-to-bring-a-proposal-before-the-committee>`_ •
   `Who is the committee? <#who-is-the-committee>`_

5. One committee member steps up as a shepherd, and generates consensus within the committee within four or five weeks.

   `Committee process <#committee-process>`_ •
   `Review criteria <#review-criteria>`_ •
   `≡ List of proposals under review <https://github.com/ghc-proposals/ghc-proposals/pulls?q=is%3Aopen+is%3Apr+label%3A%22Pending+committee+review%22>`_

6. Eventually, the committee rejects a proposal, or passes it back to the
   author for review, or accepts it.

   Acceptance of the proposal implies that the implementation will be accepted
   into GHC provided it is well-engineered, well-documented, and does not
   complicate the code-base too much.

   `≡ List of accepted proposals <https://github.com/ghc-proposals/ghc-proposals/pulls?q=label%3AAccepted>`_

7. If a proposal sees no activity for along time, they are marked as “dormant”.

   `What is a dormant proposal <#what-is-a-dormant-proposal>`_ •
   `≡ List of dormant proposals <https://github.com/ghc-proposals/ghc-proposals/pulls?q=is%3Aopen+is%3Apr+label%3A%22dormant%22>`_


8. Once a proposal is accepted, it still has to be implemented.  The author
   may do that, or someone else. We mark the proposal as “implemented” once it
   hits GHC’s ``master`` branch (and we are happy to be nudged to do so by
   email or GitHub issue).


   `≡ List of implemented proposals <https://github.com/ghc-proposals/ghc-proposals/pulls?q=is%3Apr+label%3A%22Implemented%22>`_

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

Changes to the GHC API or the plugin API are not automatically within the scope
of the committee, and can be contributed following the usual GHC workflow.
Should the GHC maintainers deem a change significant or controversial enough to
warrant that, they may, at their discretion, involve the committee and ask the
contributor to write a formal proposal.


What should a proposal look like?
---------------------------------

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

How to start a new proposal
---------------------------

To start a proposal, create a pull request that adds your proposal as ``proposals/0000-proposal-name.rst``. Use ``proposals/0000-template.rst`` as a template.

If you are unfamiliar with git and github, you can use the GitHub web interface to perform these steps:

1. `This link`__ loads the proposal template.
2. Change the filename and edit the proposal.
3. Press “Commit new file”

__ https://github.com/ghc-proposals/ghc-proposals/new/master?filename=proposals/new-proposal.rst;message=Start%20new%20proposal;value=Notes%20on%20reStructuredText%20-%20delete%20this%20section%20before%20submitting%0A%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%0A%0AThe%20proposals%20are%20submitted%20in%20reStructuredText%20format.%20%20To%20get%20inline%20code%2C%20enclose%20text%20in%20double%20backticks%2C%20%60%60like%20this%60%60.%20%20To%20get%20block%20code%2C%20use%20a%20double%20colon%20and%20indent%20by%20at%20least%20one%20space%0A%0A%3A%3A%0A%0A%20like%20this%0A%20and%0A%0A%20this%20too%0A%0ATo%20get%20hyperlinks%2C%20use%20backticks%2C%20angle%20brackets%2C%20and%20an%20underscore%20%60like%20this%20%3Chttp%3A//www.haskell.org/%3E%60_.%0A%0A%0AProposal%20title%0A%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%0A%0A..%20proposal-number%3A%3A%20Leave%20blank.%20This%20will%20be%20filled%20in%20when%20the%20proposal%20is%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20accepted.%0A..%20trac-ticket%3A%3A%20Leave%20blank.%20This%20will%20eventually%20be%20filled%20with%20the%20Trac%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20ticket%20number%20which%20will%20track%20the%20progress%20of%20the%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20implementation%20of%20the%20feature.%0A..%20implemented%3A%3A%20Leave%20blank.%20This%20will%20be%20filled%20in%20with%20the%20first%20GHC%20version%20which%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20implements%20the%20described%20feature.%0A..%20highlight%3A%3A%20haskell%0A..%20header%3A%3A%20This%20proposal%20is%20%60discussed%20at%20this%20pull%20request%20%3Chttps%3A//github.com/ghc-proposals/ghc-proposals/pull/0%3E%60_.%0A%20%20%20%20%20%20%20%20%20%20%20%20%2A%2AAfter%20creating%20the%20pull%20request%2C%20edit%20this%20file%20again%2C%20update%20the%0A%20%20%20%20%20%20%20%20%20%20%20%20number%20in%20the%20link%2C%20and%20delete%20this%20bold%20sentence.%2A%2A%0A..%20sectnum%3A%3A%0A..%20contents%3A%3A%0A%0AHere%20you%20should%20write%20a%20short%20abstract%20motivating%20and%20briefly%20summarizing%20the%20proposed%20change.%0A%0A%0AMotivation%0A------------%0AGive%20a%20strong%20reason%20for%20why%20the%20community%20needs%20this%20change.%20Describe%20the%20use%20case%20as%20clearly%20as%20possible%20and%20give%20an%20example.%20Explain%20how%20the%20status%20quo%20is%20insufficient%20or%20not%20ideal.%0A%0A%0AProposed%20Change%20Specification%0A-----------------------------%0ASpecify%20the%20change%20in%20precise%2C%20comprehensive%20yet%20concise%20language.%20Avoid%20words%20like%20should%20or%20could.%20Strive%20for%20a%20complete%20definition.%20Your%20specification%20may%20include%2C%0A%0A%2A%20grammar%20and%20semantics%20of%20any%20new%20syntactic%20constructs%0A%2A%20the%20types%20and%20semantics%20of%20any%20new%20library%20interfaces%0A%2A%20how%20the%20proposed%20change%20interacts%20with%20existing%20language%20or%20compiler%20features%2C%20in%20case%20that%20is%20otherwise%20ambiguous%0A%0ANote%2C%20however%2C%20that%20this%20section%20need%20not%20describe%20details%20of%20the%20implementation%20of%20the%20feature.%20The%20proposal%20is%20merely%20supposed%20to%20give%20a%20conceptual%20specification%20of%20the%20new%20feature%20and%20its%20behavior.%0A%0A%0AEffect%20and%20Interactions%0A-----------------------%0ADetail%20how%20the%20proposed%20change%20addresses%20the%20original%20problem%20raised%20in%20the%20motivation.%0A%0ADiscuss%20possibly%20contentious%20interactions%20with%20existing%20language%20or%20compiler%20features.%20%0A%0A%0ACosts%20and%20Drawbacks%0A-------------------%0AGive%20an%20estimate%20on%20development%20and%20maintenance%20costs.%20List%20how%20this%20effects%20learnability%20of%20the%20language%20for%20novice%20users.%20Define%20and%20list%20any%20remaining%20drawbacks%20that%20cannot%20be%20resolved.%0A%0A%0AAlternatives%0A------------%0AList%20existing%20alternatives%20to%20your%20proposed%20change%20as%20they%20currently%20exist%20and%20discuss%20why%20they%20are%20insufficient.%0A%0A%0AUnresolved%20questions%0A--------------------%0AExplicitly%20list%20any%20remaining%20issues%20that%20remain%20in%20the%20conceptual%20design%20and%20specification.%20Be%20upfront%20and%20trust%20that%20the%20community%20will%20help.%20Please%20do%20not%20list%20%2Aimplementation%2A%20issues.%0A%0AHopefully%20this%20section%20will%20be%20empty%20by%20the%20time%20the%20proposal%20is%20brought%20to%20the%20steering%20committee.%0A%0A%0AImplementation%20Plan%0A-------------------%0A%28Optional%29%20If%20accepted%20who%20will%20implement%20the%20change%3F%20Which%20other%20ressources%20and%20prerequisites%20are%20required%20for%20implementation%3F%0A

.. link generated with
   python -c "import urllib;print 'https://github.com/ghc-proposals/ghc-proposals/new/master?filename=proposals/new-proposal.rst;message=%s;value=%s' % (urllib.quote('Start new proposal'), urllib.quote(file('proposals/0000-template.rst').read()))"

The pull request summary should include a brief description of your
proposal, along with a link to the rendered view of proposal document
in your branch. For instance,

.. code-block:: md

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

`The secretary <#who-is-the-committee>`_ will then label the pull request with
``Pending committee review`` and start the `committee process <#committee-process>`_.
(If this does not happen within a day or two, please ping the secretary or the
committee.)

What is a dormant proposal
--------------------------

In order to keep better track of actively discussed proposals, proposals that
see no activity for an extended period of time (a month or two) might be marked
as “``dormant``”. At any time the proposer, or someone else can revive the
proposal by picking up the discussion (and possibly asking `the secretary
<#who-is-the-committee>`_ to remove the ``dormant`` tag).

You can see the `list of dormant proposals <https://github.com/ghc-proposals/ghc-proposals/pulls?q=is%3Aopen+is%3Apr+label%3A%22dormant%22>`_.

Who is the committee
--------------------

The current members of the GHC steering committee, who you can reach
by email at ghc-steering-committee@haskell.org, are:

======================  ====================================================  =========
Christopher Allen       `@bitemyapp <https://github.com/bitemyapp>`_
Vitaly Bragilevsky      `@bravit <https://github.com/bravit>`_
Joachim Breitner        `@nomeata <https://github.com/nomeata>`_              secretary
Manuel M T Chakravarty  `@mchakravarty <https://github.com/mchakravarty>`_
Iavor Diatchki          `@yav <https://github.com/yav>`_
Richard Eisenberg       `@goldfirere <https://github.com/goldfirere>`_
Ben Gamari              `@bgamari <https://github.com/bgamari>`_
Simon Marlow            `@simonmar <https://github.com/simonmar>`_            co-chair
Simon Peyton-Jones      `@simonpj <https://github.com/simonpj>`_              co-chair
Eric Seidel             `@gridaphobe <https://github.com/gridaphobe>`_
======================  ====================================================  =========

Members have terms of 3, 4, and 5 years.

The committee members have committed to adhere to the `Haskell committee guidelines for respectful communication <GRC.rst>`_.

We would also like to thank our former members

======================  ====================================================
Ryan Newton             `@rrnewton <https://github.com/rrnewton>`_
Roman Leshchinskiy      `@rleshchinskiy <https://github.com/rleshchinskiy>`_
======================  ====================================================


Committee process
-----------------

The committee process starts once the committee has been notified that a
proposal is ready for decision, and takes place on the
`ghc-steering-committee <https://mail.haskell.org/cgi-bin/mailman/listinfo/>`_
mailing list. All interested parties are invited to follow the discussion.

-  The secretary nominates a member of the committee, the *shepherd*, to oversee
   the discussion.

-  Based on the proposal text (but not the GitHub commentary), the shepherd
   decides whether the proposal ought to be accepted or rejected or returned for
   revision.
   
-  If the shephard thinks the proposal ought to be rejected, they post their
   justifications on the GitHub thread, and invites the authors to respond with
   a rebuttal and/or refine the proposal. This continues until the either
    
   * the shepherd changes their mind and supports the proposal now,
   * the authors withdraw their proposal,
   * the authors indicate that they will revise the proposal to address the shepherds
     point. The shepherd will label the pull request as
     `Needs Revision <https://github.com/ghc-proposals/ghc-proposals/pulls?q=label%3A"Needs+revision">`_.
   * the authors and the shepherd fully understand each other’s differing
     positions, even if they disagree on the conclusion.

-  Now the shepherd proposes acceptance or rejectance, by email to the mailing
   list. Discussion among the committee ensues on the mailing list, and 
   silence is understood as agreement with the shepherds recommendation.

-  Ideally, the committee reaches consensus, as determined by the secretary or
   the shepherd. If consensus is elusive, then we vote, with the Simons
   retaining veto power.

-  The decision is announced, by the shepherd to the mailing.

-  The secretary tags the pull request accordingly, and either merges or closes it.
   In particular
  
   *  **If we say no:** 
      The pull request will be closed and labeled
      `Rejected <https://github.com/ghc-proposals/ghc-proposals/pulls?q=label%3Arejected>`_.

      If the proposer wants to revise and try again, the new proposal should
      explicitly address the rejection comments.

      In the case that the proposed change has already been implemented in
      GHC, it will be reverted.

   *  **If we say yes:**
       The pull request will be merged, numbered and labeled
      `Accepted <https://github.com/ghc-proposals/ghc-proposals/pulls?q=label%3AAccepted>`_.
      
      At this point, the proposal process is technically
      complete. It is outside the purview of the committee to implement,
      oversee implementation, attract implementors, etc.

      The proposal authors or other implementors are encouraged to update the
      proposal with the implementation status (i.e. trac ticket number and the
      first version of GHC implementing it.)


Review criteria
---------------

Below are some criteria that the committee and the supporting GHC
community will generally use to evaluate a proposal. Note that this list
is merely set of a guidelines; it is the committee's job to weigh these
and any other relevant considerations appropriately.

-  *Utility and user demand*. What exactly is the problem that the
   feature solves? Is it an important problem, felt by many users, or is
   it very specialised? The whole point of a new feature is to be useful
   to people, so a good proposal will explain why this is so, and
   ideally offer evidence of some form.

-  *Elegant and principled*. Haskell is a beautiful and principled
   language. It is tempting to pile feature upon feature (and GHC
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

How to build the proposals?
---------------------------

The proposals can be rendered by running::

   nix-shell shell.nix --run "make html"

this will then create a directory ``_build`` which will contain an ``index.html``
file and the other rendered proposals. This is useful when developing a proposal
to ensure that your file is syntax correct.


Questions?
----------

Feel free to contact any of the members of the `GHC Steering Committee
<#who-is-the-committee>`_ with questions. `Email <https://wiki.haskell.org/Mailing_lists>`_
and IRC (``#ghc`` on ``irc.freenode.net``) are both good ways of accomplishing this.
