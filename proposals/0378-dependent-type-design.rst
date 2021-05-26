Design for Dependent Types
==========================

.. sectnum::
.. author:: Richard Eisenberg
.. date-accepted:: 2021-05-26
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/378>`_.
.. contents::

As detailed in the Motivation section below, there are a handful of GHC proposals
currently up for debate that, in part, hinge on whether or not we eventually want
dependent types in GHC. While each proposal has its own merits and idiosyncracies --
and this proposal does not directly decide the fate of any other -- it seems helpful
to put the question "Do we want to have dependent types in GHC?" front
and center. This proposal does just that, by putting forward a design sketch
for what dependent types might look like in GHC. While I expect this design
sketch to get fleshed out over the course of other proposals (which would
modify this one), I'm hoping that an acceptance (or rejection) of this proposal
can inform the design directions on other proposals.

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

Motivation for why we need this proposal now
--------------------------------------------
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
aspects that seem unable to meet both of these criteria. Instead simply rejecting
such proposals or hobbling them to avoid contravening either criterion, it
would be good to have some high-level direction on how we expect (or not) to
integrate dependent types in GHC.

This current proposal aims give some detail to what dependent types may look like.
Acceptance does not commit us to an unalterable course of inclusion of exactly
the features described here. But it would suggest that other proposals in
this space should be designed in such a way that they are compatible with the
details presented here.

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
the GHC committee would consider compatibility with the design sketch
below. Generally speaking, new proposals should be forward-compatible
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

Design Sketch for Dependent Types
---------------------------------

The term "dependently typed programming language" covers a huge range of
designs, and there is a danger that we'll each have something different in
mind. So this wiki page outlines one particular part of the design space, the
one that Richard and Stephanie have in mind. It's not the only possible design
-- and in any case it's not a fixed design, more sub-space of the huge design
space -- but perhaps it can serve as a concrete baseline to help bring clarity
to our discussion.

Given the Haskell's community lack of experience with dependent types, there
are also a number of misconceptions that have arisen around the design of
dependent types. A section below describes several common misconceptions and
better ways of understanding certain design points.

The repo at `<https://gitlab.haskell.org/rae/dependent>`_ includes (in the ``dh``
directory) some examples of what dependent Haskell might look like. If there is
demand, I can expand this.

Here, then, are the design principles for Dependent Haskell, originally drafted
by Simon PJ and then co-edited collaboratively.

Type inference
^^^^^^^^^^^^^^

Dependent Haskell embodies type inference, just like Haskell.  Indeed, every Haskell
program is a DH program: no extra type annotations are required.

This stands in contrast to some dependently-typed languages (e.g. Agda, Idris)
that require every binder to be explicitly type-annotated.

Of course, just as in GHC/Haskell today, to reach the more sophisticated
corners of the type system the programmer must supply some type annotations
(for example, define higher-rank types, guide impredicative type inference,
check GADT pattern-matches), but the goal is to have simple, predictable rules
to say when such annotations are necessary.

Erasure
^^^^^^^

In DH, *the programmer knows, for sure, which bits of the program will be
retained at runtime, and which will be erased*. We shall call this the
**Predictable Erasure Principle (PEP)**. Some dependently typed languages
(Idris1, but notably not Idris2) leave this choice to a compiler analysis, but
in DH we make it fully explicit in the types.

We will see under "Quantifiers" below exactly *how* this is made explicit to the programmer,
but as erasure is such a key property, there should be absolutely no ambiguity about it.
Haskell has very strong erasure properties, and so does DH.

Just as in Haskell today, some programmers may prefer to omit the annotations
that guide erasure, and GHC will infer how much it can erase (choosing to
erase as much as possible). The one exception to this is in datatypes, where
erasure must always be made explicit (otherwise, GHC has no way to know what
should be erased, unlike in functions).

Lexical scoping, term-syntax and type-syntax, and renaming
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Status quo
""""""""""

Haskell adheres to the following principle:

* **Lexical Scoping Principle (LSP)**. For every *occurrence* of an
  identifier, it is possible to uniquely identify its *binding site*, without
  involving the type system.

This allows a compiler to proceed in two phases:

* *Rename* the program, by deciding, for every occurrence, what its corresponding binder is.
  
* *Typecheck* the program.

This two-stage approach is not just an implementation matter: it makes the
language easier to describe to Haskell's users, by separating the concerns of
*scoping* and *typing*.

A Haskell program contains both types and terms:

* **Types** appear
  
  * in type or class declarations,
  * after ``::`` in a type or kind signature, and
  * after the "``@``" sign in visible type application.

  We say that the bits of the program in these places as written in **type-syntax**.

* **Terms** appear in value declarations, such as ``f x = x+1``. We describe
  them as written in **term-syntax**.

(GHC aficionados know type-syntax as ``HsType`` and term-syntax as ``HsExpr``.)

Term-syntax and type-syntax have different name-spaces, which allows "punning". We can write ::

  data Age = Age Int

  birthday :: Age -> Age         -- Type
  birthday (Age n) = Age (n+1)   -- Term

We have the type constructor ``Age`` in the type namespace, and an eponymous
data constructor ``Age`` in the term namespace. When renaming a type, we look
up in the type namespace, while when renaming a term we look up in the term
namespace. ("Renaming" means resolving, for each occurrence of an identifier,
what is the binding site to which that occurrence refers.)

Changes to support dependent types
""""""""""""""""""""""""""""""""""

In DH, *we support the same Lexical Scoping Principle, including Haskell's
dual namespace*, slightly generalized:

1. In type-syntax, DH will continue to use the type namespace.
2. In term-syntax, DH will continue to use the term namespace.
3. When a lookup in the primary namespace fails, DH will look in the other namespace.

Point (3) is a natural extension of today's ``DataKinds`` approach. With
``DataKinds``, when renaming a type, if ``T`` is not in scope in the type
namespace we look in the term namespace (for a data constructor ``T``). (We
also provide an escape mechanism, the tick-mark: in a type, ``'T`` refers
unconditionally to the term namespace, and we might consider extending that
escape to lower-case variables in DH.)

Due to this extra lookup, the implicit quantification in type signatures (e.g.
``f :: a -> a``, where ``a`` is implicitly quantified, making the type read
``f :: forall a. a -> a``) would happen only for variables that are in scope
in neither namespace. For backward compatibility, this change to implicit
quantification would likely be guarded by an extension flag.

DH programmers may find it convenient to avoid punning, so that they no longer
need to consider the context of an identifier occurrence to be able to
interpret its meaning. (That is, to understand an occurrence ``Age`` in the
example above, we need to look around to see what context we are in.) We
expect DH to support these programmers' desire to avoid punning by providing
optional warnings, while still also supporting easy interaction with other
code that uses puns. Proposal `#270`_ describes a way that might happen; the
additional support of `local modules
<https://github.com/ghc-proposals/ghc-proposals/pull/283>`_ would allow for
even easier use of punned identifiers in pun-avoiding code.

Syntactic unification
"""""""""""""""""""""

Going further, we aim to support the following principle:

**Syntactic Unification Principle (SUP).** In the absence of punning, there is
no difference between type-syntax and term-syntax.

This is a *long term* goal: see The Glorious Future, below. It is *not* true
of Dependent Haskell as described here: type-syntax is, for now, a proper
subset of term-syntax. We describe this further in Dependent application and
the Static Subset. However, from a *scoping* point of view, it is already
true: absent punning, you do not need to reason about term-syntax vs
type-syntax when resolving scopes.

The Syntactic Unification Principle means that a DH programmer who avoids
punning can (in the end) simply forget about the distinction between
type-syntax and term-syntax, and the context-sensitivity these notions
require. This is meant to be a simplification available to those programmers.
As we design DH, this principle informs design decisions, so that it may be
true once DH is fully realized.

Switching syntaxes
""""""""""""""""""

Given that some programmers will continue to use punning, it may be necessary
to explicitly tell GHC to switch syntaxes. As originally described in `#281`_,
we propose using the keyword ``type`` to tell GHC to switch to interpreting
type-syntax, not term-syntax. This changes both parsing and name resolution.
For example, we might say ``sizeof (type Bool)`` to allow disambiguation
between a ``Bool`` in the term-level namespace and one in the type-level
namespace. We can similarly imagine a ``data`` herald to switch to the
term-level namespace.

There are some details to be worked out here (e.g. the precise BNF), but a
disambiguation syntax may be necessary, and this section suggests a way to
accommodate one.

Quantifiers
^^^^^^^^^^^

There are three "attributes" to a quantifier::

  Attribute    |  What it means
  -----------------------------------------------
  Dependence   |  The argument appears later in the type
  Visibility   |  Argument is explicit at both definition and call site
  Erasure      |  Completely erased at runtime.  Aka "relevance"

As the `Hasochism
<http://homepages.inf.ed.ac.uk/slindley/papers/hasochism.pdf>`_ paper points
out, in ML, and largely in Haskell, these three attributes are treated
differently in types and terms, thus::

  Attribute   |    Types       |   Terms        |
  ------------------------------------------------------------
  Quantifier  | forall a. ty   |   t1 -> t2     |
              |                |                |
  Dependence  | Dependent      |  Non-dependent | Compiler reasons about equality of types,
              |                |                |   but never of terms
  Visibility  | Invisible      |  Visible       | Programmer never supplies type arguments,
              |                |                |   always supplies value arguments
  Erasure     | Erased         | Retained       | Types completely erased at runtime;
              | aka Irrelevant | aka Relevant   |    terms never erased

NB: visible type application in GHC Haskell adds a refinement to this
setup, by allowing the programmer to give a visible type argument ``(e @ty)``
to a term ``(e :: forall a.blah)``.  But the basic setup is as above.

**A key aspect of a dependently typed language is that these three
can be chosen independently**.
To cut to the chase, we have (interchanging rows and columns) ::

                    ------------  Attribute ------------------
  Quantifier        Dependence     Visibility     Erasure
  ------------------------------------------------------------
  forall a. ty      Dependent      Invisible      Erased
  forall a -> ty    Dependent      Visible        Erased
  foreach a. ty     Dependent      Invisible      Retained
  foreach a -> ty   Dependent      Visible        Retained
  Eq a => ty        Non-dependent  Invisible      Retained
  t1 -> t2          Non-dependent  Visible        Retained

You can see that

* The ``forall`` vs ``foreach`` part governs erasure: ``forall``\ s are erased,
  while ``foreach``\ s are retained. ``foreach`` is the default quantifier that
  appears in Coq, Agda, and Idris; it is also known as ``∏`` in the
  literature.

* The "``.``" vs "``->``" part governs visibility: ``.`` says "invisible",
  while ``->`` says "visible"

* The presence of ``forall``\/\ ``foreach`` (vs having neither) governs
  dependence: These dependent quantifiers introduce a variable that can be
  used later in the type. Other abstractions (e.g. ``->``) do not.

* There appear to be two missing rows. Non-dependent, erased arguments cannot
  be used at compile-time or at runtime, and are thus useless and omitted.

* GHC already supports ``forall k -> ty``, in *kinds*, meaning that the programmer must apply
  a type ``(T :: forall k -> ty)`` to an explicit kind argument
  (`#81`_).  For example::
  
    data T k (a::k) = ...
  
  Here an application of ``T`` must look like ``T Type Int``, where ``T`` is
  explicitly applied to the kind ``Type``. We can tell that from its kind: ``T
  :: forall k -> k -> Type``.

* `Proposal 281 <https://github.com/ghc-proposals/ghc-proposals/pull/281>`_
  extends the ``forall ->`` quantifier to *types* as well as *kinds*. For
  example, we could then write ::
  
    f :: forall a -> a -> Int
    f a (x::a) = 4     -- The pattern signature on (x::a) is optional

  This is natural extension of what happens at the type level, where you can write ::
  
    type T :: forall k -> k -> Type
    data T k (a::k) = MkT    -- The kind signature on (a::k) is optional

  This is a natural way to "fill out" GHC's current design, but it does not
  introduce anything fundamentally new; for example the intermediate language
  does not change.

* In contrast, the two ``foreach`` quantifiers are fundamentally new. They
  allow us to have an argument (visible or invisible) that:
  
   * Can appear in the rest of the type. E.g. ``f :: foreach (a::Bool) -> T a -> Int``.
     
   * Is reasoned about at compile time. E.g. ``f True x`` is type-incorrect if
     ``x :: T False``.
     
   * Is passed at runtime (just like ``(Eq a => blah)``).

* The ``foreach ->`` quantifier allows us to eliminate the vast mess of singleton types,
  about which the Hasochism paper is eloquent. (That is, ``foreach ->`` quantifies over an
  argument usable both at compile-time *and* and runtime, the hallmark of dependent types.)
  For example, today we are sometimes forced
  to write ::
  
    data Nat = Z | S Nat
    data Natty (n::Nat) where
      Zy :: Natty Z
      Sy :: Natty n -> Natty (S n)
    zeroVec :: forall (n::Nat). Natty n -> Vec n
    zeroVec n = ...

  Here, ``Natty`` is a singleton type, mirroring ``Nat``.  But it's
  terribly painful to construct these singleton values at call sites.  With
  ``foreach`` we can say what we want directly::

    zeroVec :: foreach (n::Nat) -> Vec n
    zeroVec n = ...

  and a call might look like ``zeroVec 7``.

* The ``foreach .`` quantifier does the same thing for invisible
  arguments (not written by the programmer).  In Haskell today we have
  to encode that even further ::

    class NATTY (n::Nat) where
      natty :: Natty n

  Now we can write ::

    foo :: forall (n::Nat). NATTY n => blah

  Now, at a call site for ``foo`` the compiler will figure out the evidence
  for ``NATTY n``, and will construct a value that is passed, at runtime, to
  ``foo``.

  Again, the encoding is heavy (read Hasochism); with ``foreach`` we can write ::

    foo :: foreach (n::Nat). blah
    foo = ...n...

  and at call sites the compiler will work out a suitable ``Nat`` to pass to ``foo``.

* New research suggests that the way we denote relevance should line up with
  the way we denote linearity. See this `POPL 2021 paper
  <https://arxiv.org/abs/2011.04070>`_. We may thus want to change the syntax
  so that the distinction between ``foreach`` and ``forall`` is syntactically
  similar to the way we specify the multiplicity of a function. However, it is
  also possible to line up relevance and multiplicity in the internal language
  without exposing it in Haskell.

* Programmers will have to think about what information to preserve at
  runtime. We can imagine implementing warnings when a programmer retains
  unnecessary information.

* Proposal `#102`_ sets out this syntax, as well.

The ``foreach`` quantifier is the defining feature that makes Dependent
Haskell a dependently-typed language. We now look at how ``foreach``\ -functions
are applied (eliminated) and defined (introduced).

Dependent pattern-match
^^^^^^^^^^^^^^^^^^^^^^^

When we pattern-match on a value that also appears in a type (that is,
something bound by a ``foreach``), the type-checker can use the
matched-against pattern to refine the type. For example, consider an
implementation of ``vReplicate``::

  vReplicate :: foreach (n :: Nat) -> a -> Vec n a
  vReplicate Zero     _ = Nil
  vReplicate (Succ n) x = x :> replicate n x

The right-hand side must have a type ``Vec n a`` -- but ``n`` is the first
pattern to be matched against. Thus, when we write ``vReplicate Zero _``, the
right-hand side can have type ``Vec Zero a``. This is the essence of
informative pattern-matches (also called dependent pattern-match).

In order to support Haskell's current type inference of the result of matches,
dependent pattern-matches will happen only when the type of the result is
already known, via a type signature. (That is, we use dependent
pattern-matching only when in *checking* mode of bidirectional type-checking,
never in *inference* mode.) In the ``vReplicate`` example above, we do indeed
know the result type: ``Vec n a``. We can thus perform an informative
pattern-match, as required to accept the definition.

Dependent application and the Static Subset
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Suppose we have a function ``f :: foreach (a::ty) -> blah`` or ``f :: forall
(a::ty) -> blah``. Then at a call site the programmer must supply an explicit
argument, so the call will look like ::
  
  f <arg>

**Question 1**: is ``arg`` written in term syntax or in type syntax? Our
answer: in term syntax.

Recall that term-syntax vs type-syntax affects both which syntactic forms are
allowed, and what namespace is used during renaming. But during parsing and
renaming we do not know the type of ``f``, and DH maintains Haskell's
separation of renaming and typechecking. So we can only use term syntax for
``arg``, and the term namespace for resolving identifier occurrences in
``arg``.

A consequence of writing ``arg`` in term-syntax is that we need to be able to
write e.g. ``Int -> Int`` in term-syntax. This implies a modest expansion of
what can be parsed and renamed as a term. The type-checker will know to treat
``Int -> Int`` as a type. It is here, however, that a punned ``Int``
identifier would be annoying.

An alternative would be to require the programmer to add a syntactic marker
for dependent arguments of a function, in which case they could be written in
type-syntax. However, the syntactic marker would be redundant once we
otherwise uphold the *Syntactic Unification Principle*.

**Question 2**: can ``arg`` be *any* expression whatsoever? Lambdas? List
comprehensions? Applicative-do? Local function bindings?

Ultimately we hope that the answer will be "yes", but DH is carefully crafted
so that we do not need a "big bang" to get there. Rather, we can move
incrementally, one step at a time. Here's how:

* ``arg`` is *parsed* as a term (an ``HsExpr`` in GHC-speak)
  
* ``arg`` is *renamed* as a term
  
* But during *typechecking* the compiler treats an application chain ``f arg1
  arg2 ... argn`` specially. If it knows that ``f :: forall a -> blah``, then
  it checks that ``arg1`` is a term written only in a specified sub-language
  of terms -- initially a sub-language that maps directly to the language of
  (current) types.

We call this "specified sub-language of terms" the **Static Subset** of terms.
In GHC-speak, a ``HsExpr`` in the Static Subset can readily be converted to a
``HsType``.

For example, suppose ``f :: foreach (a :: [Bool]) -> blah``. An initial
version of DH might allow constructors and applications in the static subset,
but not list comprehensions, lambdas, or case expressions::

  f [True]            -- Allowed
  f [True,False]      -- Allowed
  f (True : [])       -- Allowed

  f [not x | x <- xs]   -- Not allowed: list comprehension
  f (case ... of ...)   -- Not allowed: case
  f ((\y -> y) [True])  -- Not allowed: lambda
   
  f xs                -- Allowed: xs equals only itself
  f (reverse xs)      -- Allowed: reverse equals only itself and xs equals only itself

These dependent applications might give rise to a need for compile-time
reasoning over Haskell's very rich expression language. The Static Subset
notion polices this boundary, initially allowing only simple expressions into
type inference. Over time we expect to widen Static Subset of terms, to allow
more syntactic forms.

Dependent application also requires us to extend term-syntax to include all
types. For example, if ``g :: forall a -> Int -> T a`` we want to allow ::

  g (Int -> Int)           -- Instantiates `a` with `Int -> Int`
  g (forall b. b->b)       -- Instantiates `a` with `forall b. b->b`

Although these type-like forms (function arrow, forall, foreach) are now valid
term-syntax, accepted anywhere in term-syntax by the parser and renamer, they
are rejected by the typechecker in actual terms, just as lambda and case are
rejected in actual types. Thus::

  f x = Int -> Int       -- Accepted by parser and renamer, rejected by typechecker
  g y = forall a. a->a   -- Ditto

The technology for treating application chains specially is worked out in details in
`A quick look at impredicativity <https://www.microsoft.com/en-us/research/publication/a-quick-look-at-impredicativity/>`_.
It is *already* used to govern Visible Type Application (which also requires knowledge of whether the
function part of the application has a forall-type). This aspect is well understood.

The examples above include applications to variables. These variables will be
treated exactly as skolems at compile-time, *even if they are ``let``-bound
with known right-hand sides*. For example, suppose we now have ``f2 :: foreach
(bs :: [Bool]) -> T bs -> blah``. Then::

  g :: [Bool] -> blah
  g bs t = f2 bs (undefined :: T bs)    -- this is allowed, but the second argument must have type `T bs`

  h = let bs = [True]
          t :: T [True]
          t = ...
      in
      f2 bs t    -- surprisingly rejected, as bs is equal only to itself

In the ``h`` example, we might expect ``f2 bs t`` to be accepted, but it will
not be, as variables used in types are equal only to themselves. That is, GHC
will forget the relationship between ``bs`` and ``[True]``.

Similarly, if we see ``f :: forall xs. T (reverse xs) -> blah``, can the
``(reverse xs)`` ever reduce (e.g. when ``f`` is instantiated at a call site)?
Our answer for now is no: variables used in types are equal only to
themselves. (After all, ``reverse`` might be defined in a separately compiled
module, and might be defined with arbitrary Haskell terms.)

This approach keeps things simple for now; we might imagine retaining the
knowledge that ``bs = [True]`` when, say, the right-hand side of a ``let`` is
in the Static Subset, but we leave that achievement for later.

Dependent definition
^^^^^^^^^^^^^^^^^^^^

Principle: We will never *infer* a type with ``foreach .``, ``foreach ->``, or
``forall ->``. We will continue to infer types with ``forall .``, via
``let``\ -generalization, just as we do today.

Just as with all the other first-class polymorphism work, users can write a type signature
to define functions with these quantifiers. Examples::

  vReplicate :: foreach (n :: Nat) -> a -> Vec n a
  vReplicate Zero     _ = Nil
  vReplicate (Succ n) x = x :> vReplicate n x

  vReplicateImplicit :: foreach (n :: Nat). a -> Vec n a
  vReplicateImplicit x = case n of   -- n is in scope from -XScopedTypeVariables
    Zero   -> Nil
    Succ _ -> x :> vReplicateImplicit x

  -- alternative approach, from https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0155-type-lambda.rst
  vReplicateImplicit :: foreach (n :: Nat). a -> Vec n a
  vReplicateImplicit @Zero     _ = Nil
  vReplicateImplicit @(Succ _) x = x :> vReplicateImplicit x
    -- NB: This is a dependent pattern-match, where the type-checker knows, in each equation, that n is either
    -- Zero or a Succ

  the :: forall (a :: Type) -> a -> a
  the b x = (x :: b)    -- 'a' is not in scope here, as we're forced to bind 'b'.
  -- example usage: the Int 3

All variables introduced in term-syntax are in the term namespace. In
particular, this applies to the ``b`` in the ``the`` example. Its use in a
type relies on the lookup failing in the type namespace and succeeding in the
term namespace.

Phase distinction
^^^^^^^^^^^^^^^^^

Erased arguments cannot be used at runtime. More specifically, they cannot be
pattern-matched against, returned from a function, or otherwise used, except
as an argument to a function expecting an erased argument. Examples::

  ex1 :: forall (n :: Nat) -> Nat
  ex1 n = n    -- no: cannot return an erased argument

  ex2 :: foreach (n :: Nat) -> Nat
  ex2 n = n    -- OK, though arguments to 'ex2' will need to be in the Static Subset

  ex3 :: forall (n :: Nat) -> Bool
  ex3 Zero     = True
  ex3 (Succ _) = False
    -- no: cannot pattern-match on an erased argument

  ex4 :: forall (a :: Type) -> a
  ex4 a = the a undefined   -- OK: can pass an erased argument to 'the', expecting an erased argument

  ex5 :: foreach (a :: Type) -> a
  ex5 a = the a undefined   -- OK: even though a is retained, can still pass to a function expecting an erased argument
    -- ex5 would compile to a function that ignores its argument completely
    -- this argument, of type 'Type', would be a runtime representation of a type, something like TypeRep

  data T where
    MkT :: forall (a :: Int) -> foreach (b :: Int) -> X a b -> T

  ex6 :: T -> Int
  ex6 (MkT a b x) = a   -- no: a is erased

  ex7 :: T -> Int
  ex7 (MkT a b x) = b   -- OK: b is retained

  ex8 (MkT a b x) = x   -- no: x's type has existentially bound variables and returning it would cause skolem-escape
    -- this last one is not about phase distinction, but it seems worth mentioning

An open question: Can we do this? ::

  f :: foreach (a :: Type) -> a -> a
  f a x = case a of
    Bool -> not x
    _    -> x

The theory says "yes"; the choice of ``a`` is available for pattern-matching.
But can we implement this in practice? I think we can, by use type
representations. Yet, we may choose to defer such behavior until later; we can
always make ``Type`` opaque and unavailable for pattern-matching.

Full expressiveness
^^^^^^^^^^^^^^^^^^^

One worry that some have about dependent types is that other dependently typed
languages sometimes require all functions to be proved to terminate. (For
example, Agda will not accept a transliteration of ::

  step :: Natural -> Natural
  step n
    | even n    = n `div` 2
    | otherwise = 3 * n + 1

  collatz :: Natural -> Natural
  collatz 0 = 0
  collatz 1 = 0
  collatz n = 1 + collatz (step n)

without a proof that ``collatz`` terminates. Do let me know if you have such a
`proof <https://en.wikipedia.org/wiki/Collatz_conjecture>`_.) Backward
compatibility (and the usefulness of not-known-to-terminate functions, such as
interpreters) compels us to avoid adding this requirement to Haskell. Perhaps
someday we will add a termination checker has an aid to programmers, but it
will not be required for functions to terminate. Due to the way dependent
types in Haskell are designed (e.g., as explained in this `ICFP'17
paper <https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf>`_),
it is not necessary to have a termination proof to support dependent types.

Typed intermediate language
^^^^^^^^^^^^^^^^^^^^^^^^^^^

GHC has from the beginning supported a *typed* intermediate language. The type
safety of this intermediate language is what allows us to say that Haskell
itself is type-safe (no one has attempted a proof of type safety for Haskell
itself), and the checks on this internal language allow us to catch many
errors that otherwise would have crept into GHC's optimizer.

Dependent Haskell continues to support a typed intermediate language, but one
supporting dependent types natively. Designing such a language is hard and has
been the subject of some research. We believe that the most recent paper
(listed first below) is an adequate candidate for implementation in GHC.

* `*A graded dependent type system with a usage-aware
  semantics* <https://richarde.dev/papers/2021/grad/grad-extended.pdf>`_. Pritam
  Choudhury, Harley Eades III, Richard A. Eisenberg, and Stephanie Weirich.
  POPL'21. This paper combines linearity with dependent types.
* `*A role for dependent types in
  Haskell* <https://richarde.dev/papers/2019/dep-roles/dep-roles-extended.pdf>`_.
  Stephanie Weirich, Pritam Choudhury, Antoine Voizard, and Richard A.
  Eisenberg. ICFP'19. This paper combines roles with dependent types.
* `*A specification for dependently-typed
  Haskell* <https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf>`_;
  `appendix <https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec-appendix.pdf>`_.
  Stephanie Weirich, Antoine Voizard, Pedro Henrique Azevedo de Amorim, and
  Richard A. Eisenberg. ICFP'17. This paper introduces homogeneous equality as
  a simplification over previous approaches.
* `*Dependent types in Haskell: Theory and practice*
  <https://richarde.dev/papers/2016/thesis/eisenberg-thesis.pdf>`_. Richard A.
  Eisenberg. PhD thesis, 2016. This work describes both a surface language and
  intermediate language for Dependent Haskell.
* `*Type inference, Haskell, and dependent types*
  <https://adam.gundry.co.uk/pub/thesis/thesis-2013-12-03.pdf>`_. Adam Gundry.
  PhD thesis, 2013. This work describes an intermediate language and the
  Static Subset included in this design document.

The Glorious Future
^^^^^^^^^^^^^^^^^^^

One glorious day, perhaps all terms will be understood by the static type
checker. To put it another way, any term whatsoever will be acceptable as an
argument to ``f :: foreach a -> blah``; and any term whatsoever would be
acceptable in a type or kind signature. (NB: Richard and Stephanie definitely
want this. Simon is not yet convinced that the pain will be worth the gain.)

If that Glorious Day comes, the Static vs Non-static distinction will vanish,
and why it would be unseemly to force some syntactic marker in the code to
indicate dependent arguments.

Instead DH simply imposes restrictions on the terms that can be seen by the
static type checker, and ensures that they lie within its ability to reason.

Note: full-spectrum dependently typed languages treat ``t1 -> t2`` as a mere
abbreviation of ``foreach (_ :: t1) -> t2``. But until the Glorious Day, DH
will treat these two very differently:

* If ``f1 :: t1 -> t2``, then in a call ``(f1 arg)``, there are no
  restrictions on ``arg`` (except of course that it has type ``t1``).
  
* If ``f2 :: foreach (_ :: t2) -> t2``, then in a call ``(f2 arg)`` arg must
  lie in the Static Subset of terms.
  
Even once we reach the Glorious Day, nothing forces us to unify ``t1 -> t2``
with ``foreach (_ :: t1) -> t2``, and we may decide not to.

Learnability of Haskell
^^^^^^^^^^^^^^^^^^^^^^^

A cross-cutting concern in the design of depdendent types in Haskell is whether
they will make learning Haskell more difficult for newcomers to the language, or
less usable for long-time Haskellers who prefer to avoid dependent types.

We thus set forth this principle:

**The Opt-In Principle (OIP):** Users who do not opt into dependent types will
not be affected by them.

By "opt into", we mean that users would have to enable ``-XDependentTypes`` or
import a module that exposes functions with depenently-typed interfaces. These
modules would not be standard modules that are routinely imported today, such
as ``Data.List`` or ``Prelude``.

Like all principles, we cannot promise that there will not be exceptions, but
any exceptions made would be made wary of the OIP. An example from the history
of adding fancy types to GHC is around the type of ``($)``, which became levity-polymorphic
for GHC 8.0, despite the fact that ``($)`` is exported in ``Prelude``. The solution
is to have ``-fprint-explicit-runtime-reps``, off by default, that allows users
to see the full type of ``($)``. Without ``-fprint-explicit-runtime-reps``, users see
a simplified type ``(a -> b) -> a -> b``, as they might expect. Extrapolating to
dependent types, if a function in ``Prelude`` were to get a dependent type, we
would design the new type to be backward compatible and to be hidden behind a flag
similar to ``-fprint-explicit-runtime-reps``. It is our hope that efforts toward
better IDE support for Haskell will make such designs easier to contemplate, where
a user might, say, click on a confusing aspect of an error message to get more detail.

Another important aspect of the OIP is in error messages. Suppose my second day of
Haskell includes ::

  x :: Just Int
  x = Nothing

I would then see a suggestion ``A data constructor of that name is in scope;
did you mean DataKinds?``. This suggestion is unhelpful on the second day of
Haskelling: much better would be for GHC to suggest that the user meant ``Maybe``
instead of ``Just``. There is an inherent tension between keeping users in
a language subset that is more understandable and discoverability. One possibility
would be for an error message to label how "advanced" a language extension is
as it is being suggested, or to link to a page with more information and guidance.
If our second-day-of-Haskell user were told
``Did you mean to enable advanced extension DataKinds?`` perhaps they would
seek other fixes to their program before enabling ``-XDataKinds``.

Another possibility
is for users to somehow indicate a universe of extensions that are in scope; error
messages would not suggest extensions outside of that universe. This universe could
be quite small by default, and then have a way of being easily enlarged; it is all
easier to imagine in the context of an IDE than as a command-line interface.

The goal here is not to suggest a concrete approach to upholding the OIP, only
that solutions exist. As we develop out dependent types and implement it, we can
revisit these solutions as necessary.

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
* If this current proposal is accepted, I would expect the committee to accept
  `#291`_ (with significant revisions to the text, but not the spirit) and
  `#270`_ (perhaps with significant revisions to the details). `#281`_ could
  then be drastically simplified and designed to work only in the subset of
  the language that contains no puns; my hope is then that `#281`_, too, would
  be accepted.

* Simon PJ has asked for a "list of the things [we] might have to give up".
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

* There is worry that dependent types will somehow, non-specifically make Haskell worse.
  I `responded <https://github.com/ghc-proposals/ghc-proposals/pull/378#issuecomment-788536398>`_
  in the thread.

* A concern was raised about the word "quantifier" in the way it is used in my thesis,
  where it describes things like ``forall a.`` or ``foreach (b :: Nat) ->`` or ``Show a =>``.
  I am agnostic on the choice of vocabulary here. In any case, this proposal does not
  fix that vocabulary item.

* One particular worry is that people who do not wish to use dependent types will
  somehow be forced to, either by following unhelpful suggestions in error messages
  or through the adoption of dependent types in necessary libraries. The OIP was added
  to address the concern about error messages -- I think this is an easy mistake to
  avoid. The problem of adoption in libraries
  is not really one that can be addressed within GHC. While there is a possibility
  that some systemically important libraries will adopt dependent types before they are
  sufficiently settled, it is my hope (and belief) that the community will work to avoid this
  becoming a problem.

* There is some concern that we should spend our collective energy elsewhere, away
  from dependent types.

* There was an observation that irrelevant class constraints are useful; current
  designs do not allow any syntax for irrelevant class constraints. We should indeed revisit
  this if/when `#102 <https://github.com/ghc-proposals/ghc-proposals/pull/102>`_ gets
  reopened; I agree that this is a small problem with current designs.

* Previous versions of this proposal did not include the design sketch, which is
  now incorporated.

Unresolved Questions
--------------------
None at this time.

Endorsements
-------------
It may be helpful to have a list of community endorsers of this proposal,
as I imagine the community voice will be important in our consideration.
Feel free to submit a PR against this branch adding your name as an endorser.
