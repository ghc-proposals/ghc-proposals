Disambiguate Record Update
==========================

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

Introduce a new update syntax which makes the type constructor, and thus fields explicit.
This avoids the remaining source of ambiguities and implementation complexity with working with ambiguous field names as allowed in ``DuplicateRecordFields``.
It also avoids the difficult type errors possible with the various overloaded-labels-related strategies.

Motivation
----------

Namespacing, not overloading
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``DuplicateRecordFields`` extension was more cumbersome to implement than expected.
`Proposal 160`_  attacks one source of this: top-level record disambiguation.
In that thread, @simonpj and others noted how not having any record selector functions should make it easier to reuse the same record name.
Indeed, not having any selectors at all avoids dealing with the overloading entirely.

The problem is there is an even larger headache with ``DuplicateRecordFields`` that this leaves unaddressed, namely record update.
Haskell's record update, like record selectors, do not explicitly spell out the record type constructor (or any data constructors), and so only from the record fields, or worse, type inference, can meaning and desugaring of the update be computed.
Behold, `GHC users guide on -XDuplicateRecordFields record updates`_ has an entire section describing how it works, and indeed *both* nasty alternatives are in use.
Note that the rules involve inspecting adjacent syntax nodes, or otherwise getting the type checker to follow the right "hypothesis".
As described in the `GHC users guide on -XDisambiguateRecordFields`_, even that earlier extension, while less aggressive, is still an ad-hoc constraint solver.
With both extensions, code can suddenly break if unrelated imported record data types gain new fields creating a fresh ambiguity.
This is a very awkward way of defining a feature that doesn't follow the principles of the either type inferring (collect constraints until one can proceed guess-free) or renaming (trivial, lexical, and local) algorithms.

Again, like `Proposal 160`_, we can step to the side and avoid the overloading problem altogether.
By providing a different record update syntax, we can force the author of the code to disambiguate so GHC---and readers of the code---can easily understand the code written.
This proposal is just that.
For what its worth, the syntax is borrowed from Rust, where it solves the same problem.
But it also has some visual similarity with the list constructor syntax ``[start, stride ..end]``, so hopefully won't appear "foreign".

Hygiene and avoiding extra polymorphism
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The other approach to this problem is `Proposal 280`_, building upon the overloaded labels extension that we have already.
In essence, whereas this approach favors the renamer, that approach favors the type checker:
the proposal deprecates the ad-hoc disambiguation algorithm by *actually* making it collect constraints, letting the typechecker grind through them with everything else.

The first thing to note is there is no reason we can't have both.
But, there are a few reasons to prefer this alone.
Firstly, keep in mind that today's records are still nominal types.
Each record is a distinct type no more related to one other record type than any other.
Fields may be private / unexported, and there is no expectation identically-named fields in different records have anything in common.
Secondly, recall the recall the importance of alpha equivalence---names shouldn't matter.
Of course, our syntax is alpha-equivalence.
Moreover, our macros are also hygienic, a somewhat nebulous term until it is defined as alpha-equivalence for *transformations* of syntax.
Ought not our field names respect alpha-equivalence too?

Using Overloaded labels for records degrades the nominal nature of types and has dubious hygiene.
The fields of a record all become unavoidable part of the interface with overloaded labels.
There are no private type class instances, and so also no way to hide fields.
Type errors appear very different based on whether names coincide (though at least the guess-free nature of the type error ought to ensure *whether* one gets an error doesn't matter).
Downstream users are also encouraged by inferred principle types to be polymorphic in a pseudo row types way, without regard to whether nominal types sharing the fields have anything in common.

As a final aside, yes, if we actually had structural record types (e.g. row types) alone, these criticism would no longer apply.
The field identifiers really would be the only "nominal intent" in the record type, and so the polymorphism and lack of privacy would be warranted.
But even in that case, rather than using ``Symbol``, I would want an open union (like ``Type``) with labels declared in specific modules an subject to name resolution (like uninhabited ``data Foo`` type constructors for labels).
That way, two modules independently choosing the same field name wouldn't interact, again in spirit of alpha equivalence broadly construed.
I know this the choice of label kind is not constrained by other proposals (you can use the classes without the overloaded labels syntax), and that we aren't about to adopt a row types ruling this out either.
I bring this up to better exemplify that spirit of alpha equivalence outside of the specific semantics for records as they exist today which isn't very popular.

Recovering syntax for future records
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we get structure record types, ``f { a = b }`` would be nice to interpret as ``(f) ({ a = b })``, i.e. ``f`` applied to the "anonymous" record ``{ a = b }``.
This proposal frees up that syntax for that purpose (when the on-by-default extension for the legacy syntax is disabled).

Proposed Change Specification
-----------------------------

The haskell 2010 report specifies record update syntax

::

  aexp → aexp_⟨qcon⟩ { fbind1 , … , fbindn }  (labeled update, n ≥ 1)

with some side conditions on the well-formedness of the field names, and semantics:

  ::

    e { bs } = case e of
      C_1 v_1 … v_k_1 -> C_1 (pick_1^C_1 bs v_1) … (pick_k_1^C_1 bs v_k_1)
        …
      C_j v_1 … v_k_j -> C_j (pick_1^C_j bs v_1) … (pick_k_j^C_j bs v_k_j)
      _ -> error "Update error"

  where {``C_1``, …, ``C_j``} is the set of constructors containing all labels in ``bs``, and ``k_i`` is the arity of ``C_i``.

We introduce alternative syntax:

::

  aexp → qtycon { fbind1 , … , fbindn .. exp }  (labeled update with type constructor, n ≥ 1)

with the same side conditions and desugaring, but using the type constructor to unambiguously determine the constructors:

  ::

    SomeTyCon { bs .. e } = ... -- same as before

  where {``C_1``, …, ``C_j``} is the subset of constructors of ``SomeTyCon`` containing all labels in ``bs``, and ``k_i`` is the arity of ``C_i``.

To control these, there will be two new extensions ``TyconRecordUpdate`` and ``BareRecordUpdate``.
The new syntax is available only when ``TyconRecordUpdate`` is enabled, which is not enabled by default.
The old syntax is available only when ``BareRecordUpdate`` is enabled, which is enabled by default for backwards compatibility.

Examples
--------

Old way::

  r { a = b }

New way::

  MyRecord { a = b ..r }

Effect and Interactions
-----------------------

Tech debt
~~~~~~~~~

As mentioned in the motivation, with ``-XNoFieldSelectors`` and ``-XBareRecordUpdate``, all occurrences of field identifiers in syntax can be resolved as part of renaming.
We should definitely take advantage of that, and consider a future where these (or these and some overloaded labels thing) are the only options so that nasty implementation bits that prop up the status quo can be removed altogether.

Structural records
~~~~~~~~~~~~~~~~~~

This proposal continues the trend of ratcheting down the legacy record system so as to clear space for something better.
Even if one disagrees with the criticism of overloaded labels and ``Symbol`` in the motivation, they may still appreciate this proposal is part of a deprecation cycle for today's records.
If, in the future, this syntax is the only allowed one, we could repurpose the original syntax, or something conflicting with it, it to desugar to future records.
Unlike stealing the syntax immediately, this avoids the need for the new desugaring to stretch and strain itself do everything the old one can (e.g. some polymorphic record updates that only are hard for nominal record types) during a transition period.

Precedence
~~~~~~~~~~~

The precedence rules for legacy record update can be surprising:
::
  foo (quix bar) { .. } (quix baz) { .. }
This certainly looked like 4 arguments to me the first time I saw it, but is actually 2!
The new syntax at least matches an existing similar oddity in the pattern syntax:
::
  foo A { a = a } A { a = b } = 1
where ``A { a = .. }`` is a single pattern not requiring parenthesis.

Of course, we could propose mandating parantheses with either syntax, but this one is still easier to disambiguate (for the computer or the human!) in that the braces and constructor together distinguish the terminal.
Reading left to right, the first character immediately distinguishes the constructor, and only in that scenario versus the very general case of an ``aexp`` are the braces allowed.

If we don't add the parenthesis, the precedence tricks seem more justifiable to me with this.
The normal treatment of whitespace as function application can be viewed as an
"implicit infix operator".
With ``-XBlockArguments``
::
  foo do { … } do { … } do { … }
is deemed valid and parsed under similar precedence-based reasoning.
The constructor isn't as iron-clad a disambiguator as ``do``, ``case``, or some other head of a layout syntax rule,
but at least offers some syntactic hint as described above, so the "implicit infix operator" can be decently parsed from both sides.

Costs and Drawbacks
-------------------

The new syntax is more verbose than the old record update syntax.

Alternatives
------------

- Keep the current situation with its difficult implementation for ``DuplicateRecordFields``.
  I argue this makes duplicate record is a waste of precious compiler development resources to keep that complexity.
  It is possible that other proposals of the overloaded labels variety can also obviate it in conjunction with ``-XNoFieldSelectors``.

- Disallow record update entirely with ``DuplicateRecordFields``
  The second is fine with me, but rather draconian.
  The two workarounds---either using record wildcards or having to write all the untouched fields---are not appealing to me.

- Use data constructors rather than type constructors in the new update syntax.
  I like the way this *looks*, in that it matches record construction syntax, but would need a different semantics, and as-such is less of a clear replacement.

Unresolved Questions
--------------------

When, if ever, should ``-XDuplicateRecordFields`` imply and require ``-XNoFieldSelectors`` and ``-XBareRecordUpdate`` so the nasty bits of the implementation can be removed?

Implementation Plan
-------------------

I thought this should be a good beginner ticket, but that would only be the case perhaps only once `Proposal 160`_, which has been harder to implement than expected, is done.
I'll do it myself or offer to assist in any event.

Endorsements
-------------

(Optional) This section provides an opportunty for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.

.. _`GHC users guide on -XDisambiguateRecordFields`: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#record-field-disambiguation
.. _`GHC users guide on -XDuplicateRecordFields record updates`: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#record-updates

.. _`Proposal 160`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0160-no-toplevel-field-selectors.rst

.. _`Proposal 280`: https://github.com/ghc-proposals/ghc-proposals/pull/282
