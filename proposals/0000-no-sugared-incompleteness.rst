``NoSugaredIncompleteness``
==============

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

Provide an extension to disallow syntax that would desugar to run-time errors without a ``-fdefer-*`` flag.

Motivation
----------

Going back to Milner's "Well-Typed expressions do not go wrong", typed functional programmers, and Haskellers in particular, usually prefer their programs to be free of run-time failures stemming from programmer error.
For those learning Haskell, learning this mindset of being fastidious and aiming for code that is correct-by-construction about even banal errors is sometimes as important and novel as learning the language itself.
We should help those learners by trying to guide them in that direction every step of the code they write.

Unfortunately, the compiler and language do not always help with this.
We have old syntax that can not only introduce run-time errors, but does so implicitly.
Incomplete pattern matches, incomplete record updates, etc., are all seldom-asked-for "conveniences" that just get in the way of a programmer trying to understand where their program can "go wrong".

I think most of us would agree with the above paragraph.
But many of us also would say it's a solved problem, because we have ``-Werror=...`` to catch all these things.
Why write yet another proposal, for yet another language extension, when this is solved problem?

Well, I just don't think ``-Werror=...`` is the right mechanism to rely on.
This is, of course, a matter of opinion, but to me warnings are for "extra" analyses, not "essential" ones like type checking which enforce the language proper.
Deciding whether e.g. pattern matching is exhaustive feels to me like one of those essential analyses.
I am asking that we switch our vantage point and instead of considering programs with implicit partiality well-formed programs that are iffy, consider them ill-formed programs that we can permissively accept anyways.

So, what's the practical upshot of this lawyering?
And what mechanism might we use rather than ``-Werror=...``?
There is another mechanism for controlling run-time errors which I think is a perfect model for how these programs should be dealt with: ``-fdefered-type-errors``.
As I mentioned above, nobody would propose type checking is some extraneous analysis a Haskell implementation need not do by default.
The "defer" in the name makes clear we are taking what is normally a compile-time error and deferring it to run-time behind additional branching.
Whether for quick slap-dash programming, or arguing with proponents of untyped languages, ``-fdefered-type-errors`` allows GHC to both express its opinion and yet also not straight-jacket the user into any one policy.

My idea is to do the same thing for exhaustiveness checking: it should be mandatory, but as a debugging aid there are ``-fdefered-...`` flags to defer compile time errors into a run-time errors.

Proposed Change Specification
-----------------------------

Let there be a new extension ``SugaredIncompleteness``, which allows:

- Incomplete pattern matching, except in ``do``\ -notation where it is always allowed, but including lambdas parameters and other "uni-patterns" where only one pattern is allowed.

- Incomplete record construction, field selection, or update.
  We cannot get rid of accessor functions defined in other modules, but we can at least limit record syntax in the current module, and also related syntax like overloaded labels and record dot update.

- Missing items (without defaults) in instances

It is on by default.
With ``NoSugaredIncompleteness`` those things are disallowed, regardless of warnings.

Let there be a new flag ``-fdefer-incompleteness-errors``, which defers the errors from modules with ``NoSugaredIncompleteness``.
Those will be warned under warning categories:

- ``deferred-incomplete-patterns``
- ``deferred-incomplete-uni-patterns``
- ``deferred-incomplete-record-updates``
- ``deferred-incomplete-record-selection``
- ``deferred-incomplete-record-construction``
- ``deferred-missing-methods``

Those in turn can be ignored with ``-Wno-deferred-*``, or turned (back) into errors with ``-Werror=deferred-*``, like any other warning category.
Those warnings are on by default, so plain ``-Werror`` will suffice to make them all errors.

Examples
--------

- Nothing: quiet

- ``-Wall``: warnings

- ``NoSugaredIncompleteness``: errors

- ``-Wincomplete-*``: warnings

- ``-XNoSugaredIncompleteness -fdefer-incompleteness-errors``: warnings

- ``-XNoSugaredIncompleteness -fdefer-incompleteness-errors -Werror``: errors

- ``-XNoSugaredIncompleteness -fdefer-incompleteness-errors -Werror -Wno-deferred-*``: quiet

- ``-XNoSugaredIncompleteness -fdefer-incompleteness-errors -Wno-deferred-* -Wincomplete-*``: warnings

Effect and Interactions
-----------------------

- ``NoMethodError``, ``RecUpdError``, ``RecConError``, ``RecSelError``, and ``PatternMatchFail`` should all be thought of as debugging aids like ``TypeError``.

- `Proposal 319`_ proposes a ``-XNoFallibleDo`` to disable fail sugar in ``do``\ -notation, having incomplete patterns in bind statements throw ``PatternMatchFail`` just like other incomplete patterns.
  The idea isn't that ``PatternMatchFail`` is actually good, but just to be consistent with the rest of the language and avoid using a exhaustiveness heuristic weaker than the regular exhaustiveness checker.
  The combination of ``-XNoFallibleDo`` and ``-XNoSugaredIncompleteness`` would make those incomplete patterns errors like all the others under ``-XNoSugaredIncompleteness``.

Costs and Drawbacks
-------------------

- In the short term, we have far too many knobs to control the same thing.
  But, my hope is that in future versions of the language ``SugaredIncompleteness`` can be deprecated and then removed, reducing the number of knobs back to something sane.

- Somebody is going to think this has something to do with Kurt Gödel unless we choose a different name.

Alternatives
------------

- Tweaks to the exact flags:

  - Use the original warning categories instead of ``deferred-*`` variants for the deferred errors.
    (``missing-fields`` would be used instead of a new ``incomplete-record-construction`` with identical meaning.)

  - Use one ``deferred-incompleteness`` warning category.

  - Use more ``-fdefer-*`` flags, so we have one per warning category.

- Have no defer mechanism at all, forcing the user to write a manual error message themselves like in ML or Rust.
  I am not really that opposed, but I think this would just make the proposal more controversial to little benefit.

- Deprecate ``SugaredIncompleteness`` immediately.

Unresolved Questions
--------------------

Any other source of implicit partiality I forgot?
I compiled this list by looking at the `instances`_ for the ``Exception`` class in ``base``.

Implementation Plan
-------------------

This should be very easy to implement since all the analyses exist in warnings already.

Endorsements
-------------

.. _`Proposal 319`: https://github.com/ghc-proposals/ghc-proposals/pull/319
.. _`instances`: https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Exception-Base.html#t:Exception
