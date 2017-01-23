The GHC Steering Committee
==========================

This document defines the structure and process of the GHC Steering
Committee.

Process
-------

Debate steps
~~~~~~~~~~~~

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

-  If we say no: the shepherd updates the proposal (not just the
   commentary) with the reasons for rejection. The proposer is welcome
   to revise and try again, but the document should retain this original
   rejection information.
   
   In the case that the the proposed change has already been implemented in
   GHC, it will be reverted.

-  If during the Debate, the need for substantial changes does arise, we
   reject the proposal in its current state and it can go back to
   Development for revision.

-  If we say yes: A Trac ticket is created, referring back to the
   proposal and commentary. (The shepherd is responsible for making sure
   this happens.) At this point, the proposal process is technically
   complete. It is outside the purview of the committee to implement,
   oversee implementation, attract implementors, etc. 

.. comment::

   Do we solicit a vote 7 days before deadline like Chakravarty suggested? Or is the commentary enough since any vote would be advisory anyway? I (Chris) worry that voting could be contentious and if it's advisory, would prefer to focus on the weight of the arguments brought forth.

   > Manuel, is your proposal that the end of Debate stage culminates with a community vote that the committee regards as advisory?  (i.e. not binding in any way, just informative)


Criteria for successful proposals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Membership
----------

The current members are:

- Christopher Allen
- Manuel M T Chakravarty
- Simon Peyton-Jones
- Simon Marlow
- Atze Dijkstra
- Richard Eisenberg
- Iavor Diatchki
- Joachim Breitner
- Ben Gamari

Simon Peyton-Jones and Simon Marlow co-chair the committee.

.. comment::

    TODO: Every two years henceforth, the committee agrees on a chair.
    Keep? Chakravarty was fine with perpetual Simonarchy.

- Members have terms of 3, 4, and 5 years.

.. comment::

    Term limits and nomination process clarifying
