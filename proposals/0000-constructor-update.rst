Constructor Update Syntax
=========================

.. author:: John Ericson
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

Introduce a new constructor update syntax which makes the variant (and thus type) explicit.
This avoids a source of ambiguities and implementation complexity with working with ambiguous field names as allowed in ``DuplicateRecordField``.

Motivation
----------

The ``DuplicateRecordFields`` extension is currently ridiculously cumbersome to implement.
`<https://github.com/ghc-proposals/ghc-proposals/pull/160>`_ would attack one source of this: top-level record disambiguation.
In `<https://github.com/ghc-proposals/ghc-proposals/pull/160#issuecomment-413457758>`_, SPJ makes not the implementation complexity, and proposes that ``DuplicateRecordFields`` imply that proposal's (anti-)extension.

The other main source of the complexity of ``DuplicateRecordFields`` is record update.
Behold, the manual has an `entire section <https://downloads.haskell.org/~ghc/8.4.3/docs/html/users_guide/glasgow_exts.html#record-updates>`_ describing how it works.
Note that the rules involve inspecting adjacent syntax nodes, or otherwise getting the type checker to follow the right "hypothesis".
This is a very awkward implementation-driven way of defining a feature.

Much cleaner would just be an alternative syntax that forces the disambiguation up front.
This proposal is just that.
For what its worth, the syntax is borrowed from Rust, where it solves the same problem.
But it also has some visual similarity with the list constructor syntax ``[start, ..end``, so hopefully won't appear "foreign".

Proposed Change Specification
-----------------------------

The haskell 2010 report has the syntax
::
  aexp → aexp⟨qcon⟩ { fbind1 , … , fbindn }  (labeled update, n ≥ 1)
with semantics
::
  e { bs } = case e of
    C1 v1 … vk1 -> C1 (pick1C1 bs v1) … (pickk 1C1 bs v k1)
         ...
    Cj v1 … vkj -> Cj (pick1Cj bs v1) … (pickk jCj bs v kj)
    _ -> error "Update error"
We introduce alternative syntax
::
  aexp → qcon { fbind1 , … , fbindn, .. exp }  (labeled update with constructor, n ≥ 1)
with semantics
::
  C { bs, ..e } = case e of
    C v1 … vk1 -> C (pick1C1 bs v1) … (pickk 1C1 bs v k1)
    _ -> error "Update error"
The new syntax is enabled with ``ConstructorUpdate``.
The old syntax is disabled with ``NoLegacyUpdate``.

Initially, ``DuplicateRecordFields`` will imply ``ConstructorUpdate``, and warn on legacy update.
Later, it will also imply ``NoLegacyUpdate``.
Then the complicated disambiguating code can be removed.

Examples
--------
This section illustrates the specification through the use of examples of the
language change proposed. It is best to exemplify each point made in the
specification, though perhaps one example can cover several points. Contrived
examples are OK here. If the Motivation section describes something that is
hard to do without this proposal, this is a good place to show how easy that
thing is to do with the proposal.

Effect and Interactions
-----------------------

The constructor identifies the type through name resolution alone.
This allows

There many complaints with Haskell's records overall.
``<https://prime.haskell.org/wiki/ExistingRecords>`` has some (albeit old) complaints.
The general takeaway might be the haskell records are simultaneous too rigid and two flexible:
they offer certain ad-hoc flexibilties but no well-founded polymorphism composition.
Something completely different, e.g. lens (``HasField``) or row-type based (??), would be a proper solution.

This proposal continues the trend of ratcheting down the legacy record system so as to clear space for something better.
The drawback below, of lacking a "variant-polymorphic" update, can be mitigated by using labels and lenses for a truly (type-) polymorphic update.
An exact product (variant), or true polymorphism, seem to me to be better points in the design space.
If, in the future, this syntax is the only allowed one, we could repurpose the original syntax or something overlapping it to desugar to lenses and labels, or whatever the more expressive idiom *du jour* is.

As a final note, the precedence rules for legacy record update can be surprising:
::
  foo bar { .. } baz { .. }
This certainly looks like 4 arguments to me!
The new syntax at least matches an existing similar oddity in the pattern syntax:
::
  foo A { a = a } A { a = b } = 1
where ``A { a = a }`` is a single pattern not requiring parenthesis.

Of course, we could propose mandating parantheses with either syntax, but this one is still easier to disambiguate (for the computer or the human!) in that the braces and constructor together distinguish the terminal.
Reading left to right, the first character immediately distinguishes the constructor, and only in that scenario versus the very general case of an ``aexp`` are the braces allowed.

If we don't add the parenthesis, the precedence tricks seem more justifiable to me with this.
The normal treatment of whitespace as function application can be viewed as an
"implicit infix operator".
It is already an accepted proposal that
::
  foo do { … } do { … } do { … }
be accepted under similar precedence-based reasoning.
The constructor isn't as iron-clad a disambiguator as ``do``, ``case``, or some other head of a layout syntax rule,
but at least offers some syntactic hint as described above, so the "implicit infix operator" can be decently parsed from both sides.

Costs and Drawbacks
-------------------

The most important change to note is with the new syntax, it is no longer possible to update multiple different variants of the same type.
But most Haskellers already shun using record syntax in type with multiple variants.
In that case, the semantics are identical.

Alternatives
------------

 - Keep the current situation with its difficult implementation.

 - Disallow record update entirely with ``DuplicateRecordFields``

The first I argue is a silly waste of preacious compiler development resources.
The second is fine with me, but might be deemed too draconian.
It seems record update is a lessor offender than top-level accessors, in that it is just conflating different type's field namespaces rather than doing that and additionally dumping the mess the in the top level namespace.
As such, it deserves less "punishment" than being banned entirely.

Unresolved Questions
--------------------

 - The exact deprecation cycle.
   In what releases do warnings and errors happen?

 - Should parentheses be required anywhere?
   Should that be left to a separate proposal?

Explicitly list any remaining issues that remain in the conceptual design and specification. Be upfront and trust that the community will help. Please do not list *implementation* issues.

Implementation Plan
-------------------

This should be a good beginner ticket for anyone, including me, to get familiar with GHC.

Endorsements
-------------
(Optional) This section provides an opportunty for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.
