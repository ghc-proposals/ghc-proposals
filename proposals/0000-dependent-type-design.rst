Design GHC to Support Dependent Types
=====================================

.. author:: Richard Eisenberg
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

As detailed in the Motivation section below, there are a handful of GHC proposals
currently up for debate that, in part, hinge on whether or not we eventually want
dependent types in GHC. While each proposal has its own merits and idiosyncracies --
and this proposal does not directly decide the fate of any other -- it seems helpful
to put the question "Do we want to allow for ergonomic dependent types in GHC?" front
and center. This proposal does just that, in the hopes that the ensuing debate
(and any decision) can influence the design of other proposals.

To be clear: the "specification" component of this proposal is about our design
processes. This proposal has no direct effect on GHC itself.

While much of this proposal strives to maintain neutrality on the main question,
the specification section advocates for making a commitment to continue to design
GHC in a way that will support ergonomic dependent types. Thus, support for this
proposal is support for such a design; disagreement with this proposal advocates
for prioritizing other design goals over allowing further growth toward
ergonomic dependent types.

To be clear where I stand: I strongly advocate for acceptance, based on the
usefulness of dependent types (with links to supporting evidence below).

.. _`#126`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0126-type-applications-in-patterns.rst
.. _`#291`: https://github.com/ghc-proposals/ghc-proposals/pull/291/files
.. _`Type Applications in Patterns`: https://richarde.dev/papers/2018/pat-tyvars/pat-tyvars.pdf
.. _`#270`: https://github.com/ghc-proposals/ghc-proposals/pull/270
.. _`#281`: https://github.com/ghc-proposals/ghc-proposals/pull/281
.. _`#242`: https://github.com/ghc-proposals/ghc-proposals/pull/242
.. _`linear types`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst
.. _`#236`: https://github.com/ghc-proposals/ghc-proposals/pull/236
.. _`#106`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0106-type-data.rst
.. _`#102`: https://github.com/ghc-proposals/ghc-proposals/pull/102
.. _`#81`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0081-forall-arrow.rst
.. _`my thesis`: https://richarde.dev/papers/2016/thesis/eisenberg-thesis.pdf#chapter.3
.. _`singletons`: https://hackage.haskell.org/package/singletons

Motivation for why we need this proposal
----------------------------------------
There have been quite a number of proposals considered that have added more
features inspired by dependently typed languages. In crafting and debating
these proposals, we have tried to meet two design criteria:

1. The proposal should be self-standing and coherent with the rest of the design
   of Haskell. That is, the proposal should complement and improve
   the language GHC accepts on its own, even in the absence of further proposals
   introducing new aspects of dependent types.

2. The proposal should be forward-compatible with a hypothetical design of
   dependent types in GHC. That is, the new features proposed would continue
   to be at home when surrounded by other dependent-type features.

We currently have several proposals on the table (enumerated below) that have
aspects that seem unable to meet both of these criteria. Instead of simply rejecting
such proposals or hobbling them to avoid contravening either criterion, this current
proposal aims to prioritize among these criteria, so that we can make decisions
accordingly.

Existing proposals in contention
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* `#126`_ / `#291`_: Proposal `#126`_ was accepted some time ago; it introduces
  type applications in patterns, like ``f (Just @Int x) = x + 1`` or
  ``g (Dynamic @a rep n) = Dynamic @[a] (withTypeable rep typeRep) [n,n,n]``,
  where the latter's type argument might be useful with ``-XOverloadedLists``
  enabled. However, `#126`_ refers to a Haskell Symposium paper, `Type Applications
  in Patterns`_, which includes a rule around scoping. It says that a
  pattern e.g. ``K @a`` should bring ``a`` into scope if it is not already in
  scope; otherwise, treat the ``a`` as an occurrence (so that the type is matched
  against an in-scope ``a``).

  Proposal `#291`_ is an amendment to `#126`_, saying instead that ``K @a`` should
  *always* bring ``a`` into scope, shadowing any existing binding for ``a``.

  The original rule is motivated by its similarity to how pattern signatures work
  today; these bring variables into scope only when the variable is not already
  in scope. The amended rule is motivated by its similarity to how other variables
  in patterns are scoped: when we say ``f (Just x) = ...``, ``x`` is brought into
  scope as a fresh variable regardless of any ``x`` already in scope.

  Whether we adopt `#291`_ or not, the scoping rule will be similar to one nearby
  case and dissimilar to one nearby case. The question is, thus: when we look
  at ``K @a x :: ty``, do we consider the ``a`` to be more similar to ``x`` or
  to ``ty``? Put another way, is the ``@`` marker something that says "a type
  comes next" or something that says "an visible instantiation of an invisible
  argument coems next"?

  Rejecting `#291`_ amounts to prioritizing criterion 1; accepting it amounts
  to prioritizing criterion 2.

* `#270`_: (What follows is an opinionated, yet faithful, reinterpretation of
  the proposal.) This proposal introduces two new warnings, ``-Wpuns`` and
  ``-Wpun-bindings``. The ``-Wpuns`` warning triggers whenever the user writes
  an identifier that has bindings in scope in both the term-level and
  type-level namespace. The ``-Wpun-bindings`` warning triggers whenver the
  user writes a construct that introduces a new identifier into one namespace
  when that identifier already exists in the other.

  The rest of the proposal introduces new mechanisms in order to allow users
  to avoid triggering the warnings, including a standard way to write e.g.
  ``List a`` instead of the type ``[a]`` (which would conflict with a one-element
  list). There are also a few other sympathetic features included, such as
  making ``~`` non-built-in syntax and deprecating the way ``'`` is used to
  select the data-constructor namespace in a type.

  The goal of `#270`_ is to encourage users not to pun, as puns are
  problematic when the delineation between types and terms is less clear.
  However, in a language that keeps terms and types well apart, the motivation
  to avoid punning is smaller: it is simply to avoid newcomer confusion.
  While a worthwhile goal, it is not universally agreed that punning causes
  confusion, and it is not clear that the extra mechanisms introduced by the
  proposal are worth satisfying the goal.

  If we were committed to exploring adding dependent types further, the
  motivation behind this proposal would be stronger.

  Criterion 1 may suggest to reject `#270`_, while criterion 2 suggests
  (strongly) to accept it.

* `#281`_: This proposal introduces the visible ``forall`` in the types
  of terms. For example, consider
  ``Data.Typeable.typeRep :: Typeable a => proxy a -> TypeRep``. Any
  use of this function will have to specify the type ``a`` for which we
  want a representation. Currently, this is done via a (polymorphic) proxy.
  Instead, it would be cleaner to be able to say
  ``typeRep :: forall a -> Typeable a => TypeRep``, where the ``forall a ->``
  syntax means that all call sites must supply the choice of type, as in
  ``typeRep Int``.

  A central challenge in `#281`_ is that neither the parser nor
  the renamer will know that ``typeRep`` expects a type. Its argument
  will therefore be treated as a term up until the type-checker looks
  at it. Coping with this fact is the primary driver of the considerable
  complexity of the current proposal, describing how the argument is parsed
  (what if it contains a ``forall`` or ``->``?) and renamed (what if it
  contains ``[a]`` or an operator such that the term-level operator of that
  spelling has a different fixity than the type-level operator of that spelling?).

  Various solutions have been proposed, including requiring that all type
  arguments be prefixed with ``@``, as in ``typeRep @Int``. However, requiring
  the ``@`` would be very awkward in a dependently typed language, when types
  and terms are considered on even footing: why would some arguments get ``@``
  and others not? The only answer would be an awkward retelling of the days
  when Haskell did not have dependent types. If we were never getting depenent
  types, though, the ``@`` prefix may work nicely.

  On the other hand, `#281`_ could be simplified considerably if it did not
  need to deal with the possibility of type/term ambiguity: that is, if there
  were no puns. For example, we could declare that the use of any punned
  identifier in a type argument is an error. (This could easily be checked in
  the type-checker.) Doing so would greatly simplify the proposal. However,
  we would now need much of the machinery of `#270`_ (not yet accepted) in order
  not to lose expressiveness. If we knew we were marching toward dependent types,
  we could consider accepting `#270`_ and thus simplifying `#281`_.

  It is relevant to note that `#270`_ was originally meant as a precursor to
  `#281`_. However, the motivation of `#270`_ on its own seemed insufficient,
  so `#281`_ was written. Now, however, in coping with a world without `#270`_,
  `#281`_ is deemed too complex. Considering this current proposal (the one
  you are reading) may help disentangle this dependency.

  Criterion 1 favors putting in the ``@``\-sign, while criterion 2 forbids it.

Other proposals in this space
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There have been many other proposals that interact with dependent types. Reviewing
some of these may help put this all in context.

* `#81`_: This accepted, implemented proposal describes the concrete ``forall ... ->``
  syntax currently used to denote visible dependent quantification (visible ``forall``)
  in types of types (that is, kinds). In the deliberation for this proposal, the
  committee expressed doubts about whether the new syntax fit into a larger picture.
  `#102`_ (described below) is that larger picture.

* `#102`_: This tabled proposal lays out bits of the concrete syntax for dependent types.
  Discussion around the proposal was generally positive, but inconclusive. The proposal
  was merely to reserve syntax, not to actually add dependent types. It was decided
  to table the proposal until the features are ready, but also understood that we wouldn't
  steal syntax invalidating `#102`_. That is, we implicitly refined criterion 2 to
  include the syntax described in `#102`_, without directly committing to including
  dependent types.

* `#106`_: This accepted, unimplemented proposal describes a way to define a datatype
  such that its constructors enter the type-level namespace, not the term-level namespace.
  Some debate around the syntax worked hard to satisfy criteria 1 and 2, which was
  acheived successfully. However, we may have settled upon different syntax without
  having criterion 2 in mind.

* `#236`_: This meta-proposal is another attempt to fill out details of criterion 2.
  It has served as a useful place to imagine what dependent types in Haskell would
  look like and to coordinate other proposals in fitting together.

* `#242`_: This proposes unsaturated type family applications. A key challenge in accepting
  unsaturated type families is in type inference: If we know ``a b ~ Maybe Int``, can
  we conclude ``a ~ Maybe`` and ``b ~ Int``? Only if ``a`` is not a type family -- that
  is, only if ``a`` is *matchable* (a combination of generative and injective).
  (Section 4.2.4 of `my thesis`_ provides an introduction and should be understandable independent
  of the rest of the thesis.) Matchability is properly the property of a function
  arrow: we say that ``Maybe :: Type -> Type`` has a matchable arrow (because we
  can match on ``Maybe Int`` in a type family to extract out the ``Int``) while
  ``Id :: Type -> Type`` has an unmatchable arrow.

  A key question is how we distinguish matchable arrows from unmatchable ones. Currently,
  all arrows in types of types are matchable; all arrows in types of terms are unmatchbale.
  Today, without dependent types, matchability only matters in the types of types because
  matchability really is needed only to inform type inference. (We don't yet perform
  *term* inference.) Conversely, `linear types`_ matter only in the types of terms;
  we don't yet have compile-time linearity. So, we might imagine using the same
  syntax for both linear types as for matchability. In practice, without dependent types,
  there would be no conflict. Yet if we are exploring dependent types, such a
  syntax would be terribly forward-incompatible.

  As it turns out, there is enough syntactic space for these two features to avoid
  each other (and thus satisfy both criteria 1 and 2), but this choice had to be
  made intentionally.

The history of these proposals suggest that we indeed have been worried about criterion
2 for some time, without ever being very explicit about it. This current proposal
is about making this choice more explicit -- and committing to continue to honor
criterion 2 going forward.

Motivation for dependent types
------------------------------
Dependent types would allow Haskellers to encode more invariants in their
types, allow more flexible (often heterogeneous) data structures, and allow
for the possibility of more code optimizations. Given the availability of
the `singletons`_ library, which simulates dependent types and has 91 reverse
dependencies, many of these
examples are possible in Haskell today. However, dependent types are far
from easy to use today, and the overarching goal of the proposals that would
be affected by this current one is to make them easier to work with.

* Chapter 3 of `my thesis`_ is all about motivating dependent types in Haskell.
* `Why Dependent Types Matter <http://www.cs.nott.ac.uk/~psztxa/publ/ydtm.pdf>`_
* `The Power of Pi <https://cs.ru.nl/~wouters/Publications/ThePowerOfPi.pdf>`_
* `Constrained Type Families <https://richarde.dev/papers/2017/partiality/partiality.pdf>`_ and `Partial Type Constructors <https://richarde.dev/papers/2020/partialdata/partialdata.pdf>`_ would fit better in a language with dependent types; the latter explicitly desugars into a dependently typed language.
* `Stitch <https://richarde.dev/papers/2018/stitch/stitch.pdf>`_ uses techniques from dependent types to implement a lambda-calculus interpreter that is well-typed by construction.
* `Dependent Types in Haskell <https://www.youtube.com/watch?v=J8iitZtNeJk>`_, a talk by
  Stephanie Weirich on how to encode well-formed regular expressions with dependent types.
* `A Reflection on Types <https://richarde.dev/papers/2016/dynamic/dynamic.pdf>`_, on dynamic typing in Haskell, relying on dependent-type machinery. Expansions of this idea
  will require even more power in the type system.
* Though I do not have an easily-separable example, the use of dependent types
  allow us to drop tags in certain scenarios: if the type invariants indicate
  that only one disjunct of a union type is possible, then we can skip the runtime
  check for that type.
* The `singletons paper
  <https://richarde.dev/papers/2012/singletons/paper.pdf>`_ contains an
  example of well-typed database access using dependent types; it would be
  possible to skip certain dynamic type checks if we could rely on the
  dependent types instead.

Any reader is invited to add more links to this list via a pull request.

Proposed Change Specification
-----------------------------
The GHC committee would consider compatibility with ergonomic dependent types
when evaluating new proposals. A proposal that would worsen compatibility
with ergonomic dependent types would be considered to be fork-like, even if
it would not be fork-like today.

Put another way: this proposal elevates criterion 2 to an important criterion
in evaluating other proposals.

Examples
--------
If this current proposal is accepted, I would expect the committee to accept
`#291`_ (with significant revisions to the text, but not the spirit) and
`#270`_ (perhaps with significant revisions to the details). `#281`_ could
then be drastically simplified and designed to work only in the subset of
the language that contains no puns; my hope is then that `#281`_, too, would
be accepted.

Effect and Interactions
-----------------------
* By accepting this proposal, the committee reaffirms Haskell's status as
  an evolving, forward-thinking language, excited to adopt new ideas.

* This proposal does *not* invalidate any current syntax, nor does it mean
  that GHC will not consider non-dependent-type proposals. This proposal is
  all about informing judgment calls, mostly around concrete syntax, in other
  proposals.

* This proposal does *not* eliminate criterion 1. It simply makes explicit that
  we care (deeply) about criterion 2. At all times, we would continue to try
  to meet both criteria.
  
* A rejection of this proposal would likely lead to some "brain drain": I am
  aware of a number of active contributors to our community who are excited
  about the possibility of dependent types. Rejecting this proposal may signal
  to them that Haskell is not interested in what they have to offer; they may
  join other language communities.

* This proposal does *not* say anything about *backward* compatibility. Specifically,
  it does not propose that we sacrifice backward compatibility in the service
  of forward compatibility. It is every expectation that proposals building
  dependently typed features would maintain backward compatibility.
  
Costs and Drawbacks
-------------------
* Accepting this proposal would mean that, sometimes, we may accept a proposal
  that upholds criterion 2 more than it does 1. That is, we may accept a proposal
  that has an awkward fit with the language of today, in service of a better fit
  with the dependent types.

* Some members of our community have expressed a desire to see types remain types
  and terms remain terms. This viewpoint has made good sense for Haskell.
  However, in my opinion, it is antithetical to ergonomic dependent types.
  Accepting this proposal would likely displease such members of our community.

Alternatives
------------
Unlike most proposals, I do not see "no action" as a viable option. Instead,
given proposals that are currently under debate, we must make a decision on
this point, so that we can treat these proposals cohesively. Perhaps individual
proposals have a design that satisfies both criteria; if they do, we should
pursue that design. However, it is not clear that every proposal has such
a happy design point, and so a decision here can help inform committee debate
on such proposals.

Unresolved Questions
--------------------
None at this time.

Endorsements
-------------
It may be helpful to have a list of community endorsers of this proposal,
as I imagine the community voice will be important in our consideration.
Feel free to submit a PR against this branch adding your name as an endorser.
