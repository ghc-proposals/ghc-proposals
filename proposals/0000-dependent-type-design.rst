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
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/378>`_.
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

To flesh out the meaning of "ergonomic dependent types", Simon PJ and I wrote
a `design sketch <https://gitlab.haskell.org/ghc/ghc/-/wikis/dependent-haskell>`_
that gives the basic idea. Examples of this in action are in a `repo <https://gitlab.haskell.org/rae/dependent>`_
I maintain.

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
  argument comes next"?

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

  A separate question is one of defaults: when we write ``Type -> Type``, should
  that arrow be matchable or unmatchable? The proposal describes the choice here
  as a tension between backward compatibility and forward compatibility. (To be
  fair, though, there isn't a true backward-compatibility problem, as the matter
  of defaults arises only when a new extension is enabled. No existing programs
  will break.) See point (2) under the `Unresolved Questions <https://github.com/kcsongor/ghc-proposals/blob/unsaturated-type-families/proposals/0000-unsaturated-type-families.rst#7unresolved-questions>`_ section of `#242`_.

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
* These blog posts show off effective uses of dependent types in Haskell
  (such as we can use them today):

  - https://www.poberezkin.com/posts/2020-06-29-modeling-state-machine-dependent-types-haskell-1.html
  - https://www.poberezkin.com/posts/2020-09-04-dependent-types-to-code-are-what-static-types-to-data.html
  - https://blog.jle.im/entry/introduction-to-singletons-1.html
  - https://blog.jle.im/entry/introduction-to-singletons-2.html
  - https://blog.jle.im/entry/introduction-to-singletons-3.html
  - https://blog.jle.im/entry/introduction-to-singletons-4.html

Any reader is invited to add more links to this list via a pull request.

Proposed Change Specification
-----------------------------

When evaluating new proposals,
the GHC committee would consider compatibility with `the proposed design
sketch of dependent types on the GHC wiki <https://gitlab.haskell.org/ghc/ghc/-/wikis/dependent-haskell>`_. Generally speaking, new proposals should be forward-compatible
with the design sketch; that is, the new features proposed would continue to
be at home when surrounded by other dependent-type features.

Of course, the committee remains free to revise the design sketch or to accept
proposals that encroach upon it (i.e. contradicting this guidance), but such choices
should be made explicitly.

See also the committee's `Review Criteria <https://github.com/ghc-proposals/ghc-proposals/#review-criteria>`_: put another way, this proposal says that we consider
the design sketch alongside other features of today's Haskell when assessing
a new proposal's fit with the language.

Note that compatibility with dependent types is far from the only criterion
the committee would use to evaluate a proposal. Other review criteria, such
as learnability, clarity of error messages, performance, etc., remain just
as ever.

Design of Dependent Types
-------------------------
The GHC wiki has a page with a `design for dependent types in Haskell <https://gitlab.haskell.org/ghc/ghc/-/wikis/dependent-haskell>`_. This design describes a way we could incrementally add
features to Haskell in a way that could grow to encompass full dependent types.

That linked wiki page serves as an understanding of what we might achieve if
this proposal were accepted. Of course, each individual piece will have to be
separately proposed later, as this proposal does not include any specific new features
for GHC.

The repo at `<https://gitlab.haskell.org/rae/dependent>`_ includes (in the ``dh``
directory) some examples of what dependent Haskell might look like. If there is
demand, I can expand this.

In addition, because it is easy to make mistakes in this area, this proposal
includes the following subsection, describing what is *not* a part of dependent
types.

Non-design of dependent types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* False: **Dependent Haskell and/or this proposal is trying to ban definitions like `data T = T`.**

  There is no effort as far as I'm aware to eliminate code containing
  definitions like ``data T = T``. This is an example of *punning*, where
  identifiers of the same spelling are used at the term level and at the type
  level. The design of DH I've been thinking about, and every concrete
  description I've seen, continues to allow ``data T = T``, into perpetuity.

  Instead, the leading design for DH introduces warnings ``-Wpuns`` and
  ``-Wpun-bindings`` that warn at either occurrences or binding sites
  (respectively) of punned identifiers. This is (in my view) the main payload
  of `#270`_. (The rest of `#270`_ is just about giving users a way to silence the
  warnings.) No one has to enable these warnings. All DH features work with
  punned identifiers, perhaps at the expense of requiring a little more
  disambiguation. `#270`_ has the details.

  It is true that we believe that idiomatic DH will tend to avoid punning, but
  it will be up to the community to see how it will all play out. Maybe the
  disambiguation means are easy enough (at a minimum, prefixes like ``D.`` or
  ``T.``) that punning remains commonplace.

* Overstated: **Dependent Haskell is complicated.**

  @simonpj's `comment
  <https://github.com/ghc-proposals/ghc-proposals/pull/281#issuecomment-733715402>`_
  is the source of this one. According to my understanding, the complication
  he refers to is twofold: (1) the need to think about two namespaces, and (2)
  the need for the T2T translation.

  1. In corner cases, we do need to worry about the two namespaces -- but only
     when the user binds an identifier in both. Proposal `#281`_ thus
     irons out which namespace takes precedence. However, if a name is not
     punned, then the user may remain blissfully unaware of the distinction.
     Thus, when I say DH is not complicated in this way, I mean that idiomatic
     DH -- where the user disambiguates between the namespaces instead of
     using punning -- is not.

     Even a user who does use punning is OK: names bound to the left of a ``::``
     are term-level names; those bound to the right of one are type-level
     names. Occurrences to the left of a ``::`` look in the term-level namespace
     first; those to the right of one look in the type-level namespace first.
     Of course, there are subtleties here, as spelled out in the proposal, but
     that summary is morally all there is to it.

  2. The T2T translation of `#281`_ is needed only until we merge terms and types. Note
     that this merger is *independent* of the namespace issue: we can imagine
     identical ASTs for terms and for types, but with different
     name-resolution characteristics. There are relatively few barriers to
     merging terms and types: essentially, we have to sort out the fact that
     ``'`` means something different in the two ASTs (it selects the term-level
     namespace in types, while it denotes a TH name quote in terms) and we
     will have to be able to parse type-like things such as ``forall`` and ``->``
     in terms. Happily, ``->`` is *already* illegal in terms, so this probably
     boils down to making ``forall`` a keyword.

     There may be a stretch of time that we retain the complexity of T2T, but
     my hope is that this time will be limited. One of the reasons I wrote
     `#378`_ is to motivate us to deal with that temporary complexity.

  So I claim things are not as bad as they appear here.

* Likely False: **It would work just fine to have dependent types but keep
  terms as terms and types as types.**

  It is possible to have a dependently typed language that keeps terms and
  types separate. For example `Twelf <http://twelf.org/wiki/Main_Page>`_ is such
  a language. I agree that this is possible. But I claim such a language is
  complicated in precisely the way that @simonpj is worried about for DH, and
  thus a design to avoid.

  Twelf works by having a notion of type *indices*, distinct from type
  parameters. (I am not a Twelf expert; please correct me if I go wrong here.)
  Indices are terms. Thus, if we say (adapting to Haskell syntactic
  conventions) ``x :: T (a b c)``, that ``a b c`` is a *term*, not a type. This is
  because Twelf types are indexed by terms. We thus have a clear separation
  between types and terms: the thing right after a ``::`` is a type, and all of
  its arguments are terms. Yet, we have dependent types.

  However, Twelf is missing a feature crucial in Haskell: polymorphism. That
  is, Haskellers like to talk about ``Maybe Int``, where the argument to a type
  ``Maybe`` is another type ``Int``. This is impossible in Twelf.

  To mix type arguments and term arguments, we can imagine (at least) two strategies:

  1. Disambiguate according to a type's kind. That is, if we see ``T (a b c) (d e f)``,
     we can look at ``T``\'s kind to determine whether each of ``a b c`` and
     ``d e f`` are types or terms. This is challenging for several reasons.
     Firstly, it would be impossible to parse using a parser generator, if
     types and terms have separate parsers. Let's assume we get around that
     hurdle by combining syntaxes. Then, it would be very hard to do name
     resolution. It means we would need the kind of ``T`` before we can do name
     resolution on ``a b c`` or ``d e f``. Maybe it seems that this is not
     unreasonable for a type constructor like ``T``. But what about ``t (a b c) (d e f)``,
     where ``t`` is a type variable, perhaps subject to kind
     inference? We are now sliding down a slippery slope. Either we say we
     can't abstract over types that take terms as argument (and hobble our
     type system) or have strict requirements on kind annotations, etc., to
     make sure we know ``t``\'s kind before ever even doing name resolution on
     its arguments. I don't envy someone trying to implement this.

  2. Disambiguate with syntactic markers. That is, we require users to write
     ``T (a b c) (data d e f)`` where the ``data`` keyword indicates that a term
     comes next. This would mean that *every* use of ``T`` would need the ``data``
     keyword right there, which would quickly become annoying to users. It's
     especially annoying when there is no semantic difference between a type
     argument and a term argument: both would be erased during compilation.
     The ``data`` keyword would just be there to select a different
     sub-language, but with no semantic distinction.

  Either design *also* requires a considerable amount of duplication. We would
  need type families in order to do computation on types, alongside functions
  to do computation on terms. (We already have this, and it's already painful,
  in my opinion.) Consider also the desire for propositional equality (i.e.
  ``Data.Type.Equality.:~:``). Is it parameterized by types or terms? We'd need
  both variants, in practice. Would we need basic datatypes that work over
  both terms and types? Quite possibly.

  So, my claim here is that, while possible, this design is unappealing. If
  the costs of going to a unified language were very high, then maybe it would
  be worth it. But I claim that the costs are small: we introduce a way to
  disambiguate puns (as well as a way to control the built-in puns around
  lists tuples), and we merge the syntaxes. Disambiguating puns is relatively
  low-cost: it is an opt-in feature (see my first refutation above -- no one
  is proposing to ban puns), and the designs for disambiguation hook nicely
  into the module system (another disambiguation mechanism). Unifying the
  syntaxes is also relatively low-cost: it means making ``forall`` (and perhaps
  ``foreach``) unconditionally a keyword, and it means changing the meaning of
  ``'`` in types. These costs are non-zero. But I think they are worth paying in
  order to avoid having a distinction among sub-languages without a
  difference.

* False: **Dependent Haskell destroys the phase distinction and/or type erasure.**

  Other dependently typed languages (notably, Agda and Idris 1) have a murky
  notion of what information is kept around at runtime, and what is erased
  during compilation. For example, I can write this in Agda::

    quickLength : ∀ {a : Set} {n : ℕ} → Vec a n → ℕ
    quickLength {n = n} _ = n

  This function returns the length of a vector simply by looking at the index
  it is parameterized by. By contrast, we cannot write this function in
  Haskell, because the ``n`` stored as the length of the vector is a
  compile-time quantity, not available at runtime. To get the length of a
  length-indexed vector in Haskell, we must traverse the entire vector, just
  as we do for lists.

  In the design for Dependent Haskell, this phase distinction (the fact that
  some data is compile-time and some data is run-time) remains, unlike in
  Agda. Every argument to a function, both implicit and explicit, must somehow
  be marked as *relevant* or *irrelevant*.

  Continuing our example, we could write ::

    quickLength :: forall (a :: Type). foreach (n :: Nat). Vec a n -> Nat
    quickLength @_ @n _ = n

    slowLength :: forall (a :: Type) (n :: Nat). Vec a n -> Nat
    slowLength Nil = Zero
    slowLength (_ :> v) = Succ (slowLength v)

  Note that ``quickLength`` uses ``foreach (n :: Nat)``. The ``foreach`` quantifier
  (also known as ``pi`` or ``∏``) tells us that its argument is relevant and must
  be passed at runtime. Accordingly, the caller of ``quickLength`` must somehow
  already know (at run-time!) the length of the vector before calling. If we
  were to write the implementation of ``quickLength`` with the type of
  ``slowLength``, we would get an error, saying that we cannot return an input
  that is known only at compile-time.

  A few other notes on this example:

  * The kind annotations (``:: Type`` and ``:: Nat``) are unnecessary and could be inferred.

  * Leaving off any quantification would yield ``slowLength``\'s type. That
    is, we assume irrelevant quantification in types.

  * The ``forall a.`` is necessary in ``quickLength`` is necessary because of
    the forall-or-nothing rule.

  * We could reverse the order of implicit arguments in both examples.

  If a function is missing a type signature, it is actually easy to infer
  relevance: just look at the usages of a variable. If every usage is as an
  irrelevant argument, then the variable can be quantified irrelevantly.
  Otherwise, it must be relevant. Relevance inference could be done over a
  mutually recursive group much like role inference works today, by finding a
  fixpoint. Also, note that role inference just works -- it has needed
  essentially no maintenance since being written with the original
  implementation of roles. I would expect similar of relevance inference.

* False: **Dependent Haskell will require functions to terminate.**

  This has not come up much recently, but it's a misconception I've heard. I
  won't refute it longhand here. But it's not true. No one is proposing a
  termination checker. Dependent types without a termination checker is not
  suitable for use as a proof assistant, but it makes for a wonderfully
  type-safe language.

Effect and Interactions
-----------------------
* By accepting this proposal, the committee reaffirms Haskell's status as
  an evolving, forward-thinking language, excited to adopt new ideas.

* If this current proposal is accepted, I would expect the committee to accept
  `#291`_ (with significant revisions to the text, but not the spirit) and
  `#270`_ (perhaps with significant revisions to the details). `#281`_ could
  then be drastically simplified and designed to work only in the subset of
  the language that contains no puns; my hope is then that `#281`_, too, would
  be accepted.

* @simonpj has asked for a "list of the things [we] might have to give up".
  Here is an attempt at this list:

  - One namespace for types and another for terms. As `#270`_ points out, we can
    keep this distinction for those that want it, but it seems quite painful
    to mix this feature with dependent types.
    
  - The use of ``'`` to use the term-level namespace in types. Instead, ``'``
    would unambiguously be used to denote a Template Haskell ``Name``.
    
  - The use of ``forall`` (and perhaps ``foreach``) as term-level variable names

  
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
  dependently typed features would maintain backward compatibility. Where that
  is impossible, a gentle migration strategy would be paramount.

* This proposal does *not* address approachability or the new-Haskeller experience.
  Keeping Haskell learnable (or, indeed, making it more learnable) should be
  a key criterion when evaluating proposals. This proposal does not attempt to
  change our stance toward learnability.

  In my opinion, we as a committee have paid too little attention to learnability,
  and I explicitly implicate myself as a contributor to this problem. Yet there
  appears to be no reason, a priori, that dependent types should make a language
  more or less learnable. As proposals arise for adding components of dependent
  types, we should strive to do better at considering what the proposal means
  for learnability.

  In particular, `#270`_ suggests introducing new syntax for list types and tuple
  types. (The old syntax would remain, but someone enabling ``-Wpuns`` would get
  lots of warnings.) How would this new syntax affect learners who are using
  materials (e.g. books, blog posts, etc.) that were written with the traditional
  syntax? This is a good question, and would be an interesting point of debate
  on `#270`_.

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

Summary of Discussion
---------------------
The `GitHub PR <https://github.com/ghc-proposals/ghc-proposals/pull/378>`_ has a
great deal of discussion. Here are a few takeaways:

* Many industrial Haskellers came out of the woodwork to support this proposal.

* There are lots of areas of uncertainty around what the design is. The `wiki page <https://gitlab.haskell.org/ghc/ghc/-/wikis/dependent-haskell>`_ hopefully answers many of these.

* There are a number of misconceptions out there, hopefully addressed by the "non-design",
  above.

* There is worry that dependent types will somehow, non-specifically make Haskell worse.
  I `responded <https://github.com/ghc-proposals/ghc-proposals/pull/378#issuecomment-788536398>`_
  in the thread.

* A concern was raised about the word "quantifier" in the way it is used in my thesis,
  where it describes things like ``forall a.`` or ``foreach (b :: Nat) ->`` or ``Show a =>``.
  I am agnostic on the choice of vocabulary here. In any case, this proposal does not
  fix that vocabulary item.

* There remain a few individuals who appear to remain deeply unconvinced. However, these seem to
  be a small minority. The reasons they are not convinced appear to be around lack of
  understanding of the proposal/design and general worry about unintended consequences.
  I have tried to address both of these, but I do not believe my efforts have been fully
  successful.

* There is some concern that we should spend our collective energy elsewhere, away
  from dependent types.

* There was an observation that irrelevant class constraints are useful; current
  designs do not allow any syntax for irrelevant class constraints. We should indeed revisit
  this if/when `#102 <https://github.com/ghc-proposals/ghc-proposals/pull/102>`_ gets
  reopened; I agree that this is a small problem with current designs.

Unresolved Questions
--------------------
None at this time.

Endorsements
-------------
It may be helpful to have a list of community endorsers of this proposal,
as I imagine the community voice will be important in our consideration.
Feel free to submit a PR against this branch adding your name as an endorser.
