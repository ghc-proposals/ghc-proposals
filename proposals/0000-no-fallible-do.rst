(No)FallibleDo
==============

.. author:: Cale Gibbard, on behalf of Obsidian Systems
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

There are situations in which pattern completeness checking and ``do``-syntax can presently interact in frustrating ways. On the one hand, there are cases involving GADTs where one knows the index type of a GADT being pattern matched, and said GADT has only a single constructor with that index, making the pattern match theoretically complete, but GHC presently fails to discover this. For example:

::

  data MyGADT :: * -> * where
    A :: Int -> MyGADT Int
    B :: MyGADT Bool
  
  example :: (Monad m) => m (MyGADT Int) -> m Int
  example x = do
    A n <- x
    return n

This presently fails because the desugaring of the ``do``-expression involves a use of ``fail``, which induces a ``MonadFail`` constraint, even though the generated ``fail`` is theoretically dead code.

There is a bit of a Gordian knot in this, in that the type of a ``do``-expression straightforwardly depends on how it is desugared, but if we improve the completeness checker and make it operate in time to handle this, the syntactic meaning of a ``do``-expression may then depend on its type as well. In non-contrived scenarios, I don't expect that apparent dependency cycle to be a problem, but it still complicates things from the perspective of determining where in the compiler this should all take place.

It's also unclear in general whether we want the unfolding of syntax sugar to depend on anything that is going on at the type level in the first place, as that makes it far more challenging to explain and understand.

On the other hand, there are additionally cases where completeness checking of pattern synonyms becomes quite challenging as well, and ``{-# COMPLETE #-}`` pragmas may presently be required to convince GHC that certain pattern matches are complete. Even if those are specified, at present, the information about which ``COMPLETE`` pragmas exist is not available at the time of desugaring ``do``-syntax. Both making GHC take ``COMPLETE`` pragmas into consideration when deciding how to desugar `do`-expressions, and making further improvements to completeness checking in the face of pattern synonyms seem like good ideas, but there's certainly more work to be done in sorting out how this will actually be achieved in the context of GHC.

Avoid ``fail`` altogether
~~~~~~~~~~~~~~~~~~~~~~~~~

These problems are frustrating in context, because often one has no interest in having ``MonadFail`` constraints appear at all.
Errors made with `fail` are always strings, and so completely unstructured. Whether one prefers `Either` and `EitherT` or synchronous exceptions, the norm is to use types to structure failure modes, just as we use types to structure everything else in Haskell.
Furthermore, even if one does want to use unstructured textual errors, `fail` uses `String` rather than something with better performance characteristics like `Text`.
For these reasons, one might want to avoid going down a rabbit hole of subtler issues of completeness checking when the real concern is for ``fail`` not to appear in the translation at all.

So to cut through the knots a bit and provide a way to avoid the more complicated and challenging issues about completeness checking while their solutions are worked out, we decided to try something simple and provide a way to turn off the use of ``fail`` altogether.

Proposed Change Specification
-----------------------------

We propose a module-level means of switching off the use of ``fail`` in ``do``-syntax altogether via an extension flag. Specifically, there is a default extension flag ``FallibleDo`` which indicates the usual translation of the ``do``-syntax involving ``fail``, and ``NoFallibleDo`` then replaces the use of ``fail`` with throwing a `PatternMatchFail <https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#t:PatternMatchFail>`_. Moreover, when the ``-Wincomplete-uni-patterns`` warning flag is enabled alongside NoFallibleDo, we will warn about the incomplete pattern match.

Potentially failing pattern matches in the ``pat <- stmt`` syntax then result in a generated application of ``throw`` that provides the source location of the pattern match failure with a message about the reason for the exception.

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

Except for the exact error message in the `PatternMatchFail`, this is just like the desugaring everyone learned::

  example x = x >>= \(A n) -> return n

Effect and Interactions
-----------------------

This effectively sidesteps the issues where completeness checking is imperfect in the translations of ``do``-syntax by simply not making use of ``fail`` in the first place, which avoids the spurious ``MonadFail`` constraints.

Costs and Drawbacks
-------------------

Toggling this option on or off can definitely have an impact on the meaning of code. ``NoFallibleDo`` can turn working code into code which dies with an exception at runtime. However, when it does so, it at least results in a warning.

As mentioned above, we probably want to also provide something at the expression level, and it's unlikely that this design for control over the desugaring of ``do``-syntax will want to stay in exactly this form once that happens. I don't anticipate the migration in those cases to be particularly challenging though.

A possible disadvantage is that this perhaps somewhat disincentivises work on those deeper issues that were raised, however, I see this extension as somewhat of a stop-gap measure.

If the completeness checker gets really good (and finds its way to being used at the time of ``do``-syntax desugaring despite the awkwardness inherent in that), then perhaps ``NoFallibleDo`` will eventually lose its reason to exist and can be deprecated and removed. Similarly, if we come up with better syntax for controlling the unfolding of ``do``-syntax at the term level which is coordinated with a module-level version of the same, it might obviate this extension as well.

One of the reasons we picked this route is that the implementation cost seemed minimal while also solving the problems our client was running into, and the work thus far has borne that out, it's a fairly small change overall.

Alternatives
------------

Aside from eventually fixing the issues with completeness checking and its interaction with `do`-syntax that prompted this, one might also wish for a way to specify at the term-level rather than the module-level which of the proliferating translations of ``do`` we wanted to use. That seems like an entirely reasonable thing as well, but first a concrete syntax for it would have to be invented. The options seem somewhat ugly and I hadn't yet the stomach to paint that bikeshed myself. In any case, once we did have that, we'd probably also want a means of specifying the default choice of translation at a module level regardless.

Unresolved Questions
--------------------

If anyone wants to discuss other potential names for the extension, I'm not entirely sold on the name. But note that (No)MonadFailDesugaring is already a thing, which can make many options a bit awkward. Somewhat in line with ``RecursiveDo`` we ended up going with the name ``FallibleDo`` for the default behaviour of the ``do``-syntax which uses ``fail`` (this becomes an addition to the list of default-on extensions), and so ``NoFallibleDo`` turns the use of ``fail`` off.

Implementation Plan
-------------------

Obsidian Systems will implement the change. We have a work in progress PR where the implementation is already essentially complete, modulo support in Cabal and possibly other tools. See https://gitlab.haskell.org/ghc/ghc/merge_requests/2333

Endorsements
-------------

Obsidian Systems did this work on behalf of MIRI.
