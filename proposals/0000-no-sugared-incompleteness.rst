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

Provide an extension to disallow syntax that would desugar to run-time errors to better reflect our values.

Motivation
----------

The culture around Haskell has become one striving for programming correctness, and avoiding run-time errors in particular.
For those learning Haskell, learning this mindset is sometimes as important and novel as learning the language itself.
We should help those learners by trying to guide in that direction every step of the code they right.

Haskell's culture wasn't always that way, however, and we do however have old syntax that can not only introduces run-time errors, but does so implicitly.
Incomplete pattern matches, incomplete record updates, etc., are all un-asked-for "conveniences" that just get in the way of a programmer trying to understand where their program can go wrong.

I think most of us would agree with the above paragraph.
But many of us also would say it's a solved problem, because we have ``-Werror`` to catch all these things.
Why write yet another proposal for yet another extension when this is solved problem?

Well, I just don't think ``-Werror`` is good enough.
Partial code is to me just worse than the other things we warn for, valuable as I think those other warnings are.
Also, whereas I think of most warnings as stemming from extra analysis to find flaws in otherwise good programs, partial sugar seams like the opposite case where the compiler does extra work adding the run-time error throwing to make broken program run.

There is another mechanism for controlling run-time errors which I think is a perfect model for how these things should work: ``-fdefered-type-errors``.
Nobody would propose type checking is some extraneous analysis GHC need not do by default.
The "defer" in the name makes clear we are getting what is normally a compile-time check and deferring it to run-time.
Whether for quick slap-dash programming, or arguing with proponents of untyped languages, ``-fdefered-type-errors`` allows GHC to both express its opinion and yet also not straight-jacket-ing the user into any one policy.

My idea is to do the same thing for incompleteness checking ``

Proposed Change Specification
-----------------------------

Let there be a new extension ``SugaredIncompleteness``, which allows:

- Incomplete pattern matching, except in ``do``\ -notation where it is always allowed, but including lambdas parameters and other "uni-patterns" where only one pattern is allowed.

- Incomplete record construction, field selection, or update.
  We cannot get rid of accessor functions defined in other modules, but we can at least limit record syntax in the current module, and also related syntax like overloaded labels and record dot update.

- Missing items (without defaults) in instances

- TODO other things?

It is on by default.
With ``NoSugaredIncompleteness`` those things are disabled, regardless of warnings.

Let there be a new flag ``-fdefer-incompleteness-errors``, which defers the errors from modules with ``NoSugaredIncompleteness``.
Those will be warned under warning categories:

- ``deferred-incomplete-patterns``
- ``deferred-incomplete-uni-patterns``
- ``deferred-incomplete-record-updates``
- ``deferred-incomplete-record-selection``
- ``deferred-incomplete-record-construction``
- ``deferred-missing-methods``

Those in turn can be ignored with ``-Wno-deferred-*``, or turned (back) into errors with ``-Werror=deferred-*``, like any other error category.
Those warnings are on by default, so plain ``-Werror`` will suffice to make them all errors.

Examples
--------

- Nothing: quiet

- ``-Wall``: warnings

- ``NoSugaredIncompleteness``: errors

- ``-Wincomplete-*``: warnings

- ``-XNoSugaredIncompleteness -fdefer-incompleteness-errors``: warnings

- ``-XNoSugaredIncompleteness -fdefer-incompleteness-errors -Werror``: errors

- ``-XNoSugaredIncompleteness -fdefer-incompleteness-errors -Werror -Wno-defer-*``: quiet

- ``-XNoSugaredIncompleteness -fdefer-incompleteness-errors -Wno-defer-* -Wincomplete-*``: warnings

Effect and Interactions
-----------------------

- ``NoMethodError``, ``RecUpdError``, ``RecConError``, ``RecSelError``, and ``PatternMatchFail`` should all be thought of as debugging aids like ``TypeError``.

- `Proposal 319`_ proposes a ``-XNoFallibleDo`` to disable fail sugar in ``do``\ -notation, having incomplete patterns in bind statements throw ``PatternMatchFail`` just like other incomplete patterns.
  The idea isn't that ``PatternMatchFail`` is actually good, but just to be consistent with the rest of the language and avoid using a exhaustiveness heuristic weaker than the regular exhaustiveness checker.
  The combination of ``-XNoFallibleDo`` and ``-XNoFallibleDo`` would make those incomplete patterns errors like all the others.

Costs and Drawbacks
-------------------

- In the short term, we have far to many knobs to control the same thing.
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

None at this time.

Implementation Plan
-------------------

This should be very easy to implement since all the analyses exist in warnings already.

Endorsements
-------------

.. _`Proposal 319`: https://github.com/ghc-proposals/ghc-proposals/pull/319
