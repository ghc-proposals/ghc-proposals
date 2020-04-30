(No)FallibleDo
==============

.. author:: Cale Gibbard and John Ericson, on behalf of Obsidian Systems
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

Motivation
----------

It's hard to know when to fail
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are situations in which pattern completeness checking and ``do``\ -syntax can presently interact in frustrating ways.
On the one hand, there are cases involving GADTs where one knows the index type of a GADT being pattern matched, and said GADT has only a single constructor with that index, making the pattern match theoretically complete, but GHC presently fails to discover this.
For example:

::

  data MyGADT :: * -> * where
    A :: Int -> MyGADT Int
    B :: MyGADT Bool

  example :: (Monad m) => m (MyGADT Int) -> m Int
  example x = do
    A n <- x
    return n

This presently fails because the desugaring of the ``do``\ -expression involves a use of ``fail``, which induces a ``MonadFail`` constraint, even though the generated ``fail`` is theoretically dead code.
Let's break down exactly how this happens:

#. The Renamer uses a conservative heuristic to decide whether pattern is fallible, includes ``fail`` syntax if so.
   Patterns involving GADT constructors like ``A`` and pattern synonyms, are always deemed failing, however.

#. The Typechecker works on the undesugared ``do`` notation, and the presence of absence of the ``fail`` syntax effects whether a ``MonadFail`` constraint is wanted.

#. The Desugarer to Core ingests ``do`` notation directly (no intermediate desugaring to regular surface Haskell), during which pattern match checker may remove the ``fail`` if the pattern is in fact.

Before ``MonadFail``, this was a fine state of affairs:

- Extra ``fail`` is harmless, because it won't influence type checking, and still gets cleaned up.
  (I suspect there wasn't even a heuristic and it was included in all cases, why not!)

- Type errors always involve what the user actually wrote rather than some intermediate representation.

However, after ``MonadFail``, we are paying the costs of uneeded wanted ``MonadFail`` constraints that cannot be easily removed like the ``fail`` itself after the fact.
The problem is a bit of a Gordian knot:

- Pattern match exhaustiveness checking can only be done after type checking.
  (The heuristic during renaming is fundamentally unfixable.)

- The type of a ``do``\ -expression straightforwardly depends on how it is desugared

- Whether to use ``fail``, optimally, depends on pattern match exhaustiveness checking

- Type checking must occur after we've decided what syntax to type check (!), unless we are sure the choice has no non-local effects.
  (Need to decide whether to ``fail`` before type checking.)

Now, we can put on our Tarski hats and perhaps come up with some ingenious staging and/or least-fixed-point plan, where we cleverly find the minimal ``fail`` insertions to keep everything working optimally.
For sake of argument, let's even assume this wouldn't have abysmal performance.
Do we really want ``do`` notation to be this complex?
It is far from clear whether we want the unfolding of syntax sugar to depend on anything that is going on at the type level in the first place, as that makes it far more challenging to teach and understand.
Monads are already an infamous steep part of the learning curve without having to drag in the entire architecture of the compiler frontend to explain ``do``\ -notation.

Avoid ``fail`` altogether
~~~~~~~~~~~~~~~~~~~~~~~~~

Looking at the problem as described above, our options are limited.
``fail`` worked well before because its presence or absence did not influence type checking.
At the same time, we do not want to go back to putting ``fail`` back inside ``Monad``.
The simplest thing to do is just cut the Gordian knot, and provide a way to not use ``fail`` in the desugaring.

This may sound drastic, even taking into account we'll propose something opt-in rather than a breaking change in the next section.
But, we'd like the case that given the way Haskell is actually written, it isn't.
Often, one has no interest in having ``MonadFail`` constraints appear at all.
Errors made with ``fail`` are always strings, and so completely unstructured.
Whether one prefers ``Either`` and ``EitherT`` or synchronous exceptions, the norm is to use types to structure failure modes, just as we use types to structure everything else in Haskell.
Furthermore, even if one does want to use unstructured textual errors, ``fail`` uses ``String`` rather than something with better performance characteristics like ``Text``.
For these reasons, one might want to avoid going down a rabbit hole of subtler issues of completeness checking when the real immediate problem at hand is ``fail`` ever being used in the translation at all.

Proposed Change Specification
-----------------------------

We propose a module-level means of switching off the use of ``fail`` in ``do``\ -syntax altogether via an extension flag.
Specifically, there is a extension flag ``FallibleDo``, which is enabled by default, that controls the desugaring of fallible patterns in ``do``\ -notation binds (i.e. ``pat <- stmt`` syntax).

With ``FallibleDo``, the usual translation of the ``do``\ -syntax involving ``fail`` is used.
With ``NoFallibleDo`` the use of ``fail`` is replaced with the usual throwing of a `PatternMatchFail <https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#t:PatternMatchFail>`_.

When the ``-Wincomplete-uni-patterns`` warning flag is enabled alongside ``NoFallibleDo``, we will warn about the incomplete pattern match.

Monad comprehensions are effected by this extension, but list comprehension are not---fallible patterns always turn into ``[]`` / ``mzero`` there.

Examples
--------

If we take the example from the Motivation section:

::

  example x = do
    A n <- x
    return n

With FallibleDo turned on (the default), this presently translates to:

::

  example x = x >>= \v -> case v of
    A n -> return n
    _ -> fail "..."

which has an inferred type which is constrained by ``MonadFail m``

with NoFallibleDo, this would become:

::

  example x = x >>= \v -> case v of
    A n -> return n
    _ -> throw (PatternMatchFail "...")

whose inferred type is only constrained by ``Monad m``.

Except for the exact error message in the ``PatternMatchFail``, this is just like the desugaring everyone learned::

  example x = x >>= \(A n) -> return n

Effect and Interactions
-----------------------

This effectively sidesteps the issues where completeness checking is imperfect in the translations of ``do``\ -syntax by simply not making use of ``fail`` in the first place, which avoids the spurious ``MonadFail`` constraints.

The use of a ``PatternMatchFail`` might seem surprising.
Who actually likes infallible pattern matching?
Why not just ban fallible pattern outright so as to not pick and choose between bad static semantics (the pattern match heuristic) and bad dynamic semantics (some oft-maligned synchronous exception)?

The first reason is consistency with the rest of the language.
Nowhere else are complete patterns always required, and the user can always get this behavior with ``-Werror=incomplete-uni-patterns``.
(And soon ``-Wall -Werror``, too, once `Proposal 71`_ is implemented.)
Is there truly a need to forge a different path here?

The second reason is balancing the competing interests of programming in the small and programming in the large.
For programming in the large, we do expect the vast majority users to strive for no implicit partiality using ``-Werror=...``, banning certain unfortunate ``Prelude`` functions, and what-not.
But for programming in the small, one is often trying to throw together a happy path to get some job done in the next few minutes, never mind what failure modes exist provided we don't hit them right now.
``fail`` sugar is useful because it allows you to defer thinking about those failure cases, and if your code does blow up, it says where.
``PatternMatchFail`` sugar provides these same two benefits.
Conversely, we don't think the difference between pure monadic failures, throwing synchronous exceptions in pure code, and throwing synchronous exceptions in impure code---the sorts of options  one would carefully chose between when programming in the large---matter terribly much when programming in the small.

Perhaps in the future we would want to switch the defaults so all pattern matches must be total by default, and incomplete patterns with match failure exceptions are an opt-in debugging escape hatch akin to ``-fdeferred-type-errors``.
(@Ericson2314 at least would love this.)
But this applies to all pattern matching not just binds in ``do``\ -notation, and as such is out of scope of this proposal.

Costs and Drawbacks
-------------------

Toggling this option on or off can definitely have an impact on the meaning of code.
Disabling ``FallibleDo`` can turn working code into code which dies with an exception at runtime, if whatever caught the ``fail`` doesn't catch the ``PatternMatchFail``.
But with ``-Werror=incomplete-uni-patterns``, the user is at least made aware all locations this could possibly happen.
They can then rewrite the false positives with explicit error handling to not spuriously trigger the warning.

We may want to come up with some new syntax that indicates a finer-grained per-binding or per-do-block intent.
Many such designs would obviate a module-wide extension like this.
See the alternatives section for details.

We probably ought to get just rid of the heuristic earlier in the compilation pipeline that conservatively decides whether a pattern match is infallible.
By getting rid of the main problem it causes, we disincentive doing that work.
However, if we get rid of ``FallibleDo`` unconditionally, which this proposal points the way to, we will no longer have any need for that heuristic.

To the cost in particular, one of the reasons we picked this route is that the implementation cost seemed by-far the most minimal while also solving the problems our client was running into, and thus far the work has borne that out:
After merging some cleanup MRs which are good in any event, it's a quite small change.

Alternatives
------------

Some potential fixes that spring to mind are rather costly, and also don't completely fix the problem:

- Type checking post desugaring of ``do`` notation would conceptually simplify things, but result in confusing errors without major engineering effort.
  And this still doesn't break the "Gordian knot" cycles mentioned in the motivation.

- One might want to use ``{-# COMPLETE #-}`` annotations in today's heuristic pattern match checker.
  It is already accepted that ``{-# COMPLETE #-}`` is needed to help GHC figure things out across module boundaries, so is requiring it within modules that bad?
  However, not only are ``COMPLETE`` pragmas not currently available at rename time, they also couldn't be in full generality:
  ``{-# COMPLETE Pats :: TyCon #-}`` that indicate the completion of a pattern just at certain type arguments are impossible to adjudicate.
  And, this still leaves out GADTs.
  On could imagine a hypothetical: ``{-# COMPLETE Pats :: TyCon iargs #-}``, but this too embroilers the type checker and thus can't be used by the heuristic.

`Proposal 216`_ proposes that we specify how ``do`` notation (and possibly other syntactic sugar) is desugared per-use, rather than per module.
At first glance, this seems like something which ought to subsume the solutions mentioned here, but we caution it is less than a shoe-in than it first appears.
One might want to use that to provide a ``fail`` expression with a custom type error (or "type warning", if that is proposed).
However, due to the "Gordian knot", one would get spurious errors/warnings due to the conservatism of the heuristic before the desugar has a change to remove them.
One would need to rig up a special warning pass that looked at the generated core, at which point `Proposal 216`_ isn't such a subsuming solution anymore.
This is however a good fit for a desugaring using ``MonadZero`` instead of ``MonadFail``, imitating list comprehensions.

A final option is a per-binding syntax, within ``do`` blocks.
People usually care where failures might occur in a do block, not just that they do.
There is also a great opportunity to together with this solve the problem of indicating which "binds" should be turned into ``Applicative`` expressions with ``ApplicativeDo``.
This is the most promising alternative, but also the most work.
Also consider these two opposing opinions:

 - What really is so bad about explicit error handling anyways?
   I would argue it's not writing down the error patterns but other things, and opened `Proposal 327` to indicate what an alternate sugar might look like.
   This proposal keeps pattern exhaustiveness simple, and so in conjunction with ``-XNoFallibleDo`` can get back some of the lost concision without sacrificing the benefits.

 - Some of us prefer ideom brackets to do notation for ``Applicative`` anyways.
   A TH-style, more explicit "ideom brackets" with explicit splices within the quotes/brackets also works for things beyond ``Applicative``, such as the "overloaded lambda and application" ideas that have been stewing for a while as a replacement for arrow syntax.

Unresolved Questions
--------------------

If anyone wants to discuss other potential names for the extension, I'm not entirely sold on the name.
But note that ``(No)MonadFailDesugaring`` is already a thing, which can make many options a bit awkward.
Somewhat in line with ``RecursiveDo``, we ended up going with the name ``FallibleDo`` for the default behaviour of the ``do``\ -syntax which uses ``fail`` (this becomes an addition to the list of default-on extensions), and so ``NoFallibleDo`` turns the use of ``fail`` off.

Implementation Plan
-------------------

Obsidian Systems will implement the change.
We have a work in progress PR where the implementation is already essentially complete, modulo support in Cabal and possibly other tools.
See https://gitlab.haskell.org/ghc/ghc/merge_requests/2333

Endorsements
-------------

Obsidian Systems did this work on behalf of MIRI.

.. _`Proposal 71`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0071-Wall-uni-patterns.rst

.. _`Proposal 216`: https://github.com/ghc-proposals/ghc-proposals/pull/216

.. _`Proposal 327`: https://github.com/ghc-proposals/ghc-proposals/pull/327
