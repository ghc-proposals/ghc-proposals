``NoIncomplete``
==============

.. author:: John Ericson
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/351>`_.
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
There is another mechanism for controlling run-time errors which I think is a perfect model for how these programs should be dealt with: ``-fdefer-type-errors``.
As I mentioned above, nobody would propose type checking is some extraneous analysis a Haskell implementation need not do by default.
The "defer" in the name makes clear we are taking what is normally a compile-time error and deferring it to run-time behind additional branching.
Whether for quick slap-dash programming, or arguing with proponents of untyped languages, ``-fdefer-type-errors`` allows GHC to both express its opinion and yet also not straight-jacket the user into any one policy.

My idea is to do the same thing for exhaustiveness checking: it should be mandatory, but as a debugging aid there are ``-fdefer-...`` flags to defer compile time errors into a run-time errors.

Proposed Change Specification
-----------------------------

#. Let there be a new language extension ``Incomplete``.
   This extension is on by default, as that corresponds to the status quo.

   We first describe the behavior of ``NoIncomplete``.
   This restricts the programs accepted compared to the status quo.
   More important than the specific specification is the guiding principle behind that specification.
   It is this:

     Prohibit any program that can fail for reasons not explicitly specified by the programmer, except non-termination and exhaustion of resources.

   For GHC, that means to prohibit any program for which GHC would need to emit a synchronous exception throw not specified by the user.

   To accomplish this, we will mainly rely on existing analyses.
   Any program that would emit warnings from the following warning categories is prohibited:

   - ``incomplete-patterns``
   - ``incomplete-uni-patterns``
   - ``incomplete-record-updates``
   - ``missing-fields``
   - ``missing-methods``

   Additionally, with ``-XFieldSelectors`` (also on by default), any program that would emit warnings with ``-Wpartial-fields`` is also prohibited.
   Finally, ``HasField`` is only emitted for fields and types when that field is present in all variants for the type.

   With ``Incomplete`` enabled, the guiding principle is relaxed, and GHC works as it does today.

     While this is enough to specify ``NoIncomplete`` and ``Incomplete`` for GHC, language extensions are supposed to be proposed in a more implementation agnostic manner, so that they are eligible for inclusion in future Haskell reports.
     The field and method restrictions are fairly clear cut and easy to specify from first principles, but the pattern match completeness checking GHC does today is not.

   For a report, the guiding principle behind ``NoIncomplete`` only requires that the Haskell implementation's pattern match completeness checking by sound, not sophisticated.
   Even banning all pattern matching would abide by the principle.
   We do want some programs to be guaranteed to be valid Haskell, of course.
   So if and when ``NoIncomplete`` were submitted to be the official default, a simple and conservative completness checking algorithm would be specified in the report.

     To be clear, punting on that spec need not block implementing for ``NoIncomplete`` for GHC, as GHC can always accept more programs than the spec.

#. Let there be a new flag ``-fdefer-incompleteness-errors``, which defers these new compile-time errors from modules with ``NoIncomplete`` to be run-time errors.
   It has no effect on ``Incomplete`` modules.

   The deferred errors still exist at compile time, but as warnings.
   Warnings will be categorized under these new warning categories:

   - ``deferred-incomplete-patterns``
   - ``deferred-incomplete-uni-patterns``
   - ``deferred-incomplete-record-updates``
   - ``deferred-incomplete-record-selection``
   - ``deferred-incomplete-record-construction``
   - ``deferred-missing-methods``

   These warning categories in turn can be ignored with ``-Wno-deferred-*``, or turned (back) into errors with ``-Werror=deferred-*``, like any other warning category.
   They are enabled by default, so plain ``-Werror`` will suffice to make them all errors.

Examples
--------

- Nothing: quiet

- ``-Wall``: warnings

- ``NoIncomplete``: errors

- ``-Wincomplete-*``: warnings

- ``-XIncomplete -Wincomplete-* -Werror=deferred-*``: warnings, new warning categories don't matter with ``-XNoIncomplete``

- ``-XIncomplete -Wno-incomplete-* -Wdeferred-*``: quite, new warning categories don't matter with ``-XNoIncomplete``

- ``-XNoIncomplete``: warnings

- ``-XNoIncomplete -Werror``: errors

- ``-XNoIncomplete -fdefer-incompleteness-errors``: warnings

- ``-XNoIncomplete -fdefer-incompleteness-errors -Werror``: errors

- ``-XNoIncomplete -fdefer-incompleteness-errors -Werror -Wno-deferred-*``: quiet

- ``-XNoIncomplete -fdefer-incompleteness-errors -Wno-deferred-* -Wincomplete-*``: quite, legacy warning categories don't matter with ``-XNoIncomplete``.

Effect and Interactions
-----------------------

- With ``NoIncomplete``, The exception types ``NoMethodError``, ``RecUpdError``, ``RecConError``, ``RecSelError``, and ``PatternMatchFail`` should all be thought of as debugging aids like ``TypeError``.

- Note that ``NoIncomplete`` as specified for GHC allows incomplete patterns in ``do``\ notation.
  This is, most imminently, a consequence of none of the ``-Werror`` analyses mentioned catching it, and thus may seem like an oversight.
  But, it is actually intentional, as the failure case is a call to ``fail``, rather than the direct emission of an exception throw that is the red line we chose when translate ``NoIncomplete``\ 's guiding principle into a GHC-specific maxim.

  Still, one could reasonably argue that this still violates the guiding principle, demonstrating that the GHC-specific maxim is too narrow a reading.
  The intended solution is to use ``-XNoIncomplete`` in conjunction with ``-XNoFallibleDo`` from `Proposal 319`_.

  Perhaps surprisingly, `Proposal 319`_ has ``-XNoFallibleDo`` disable fail sugar in ``do``\ -notation by having incomplete patterns in bind statements throw ``PatternMatchFail`` just like other incomplete patterns today.
  The idea isn't that ``PatternMatchFail`` is actually good, but rather just to be consistent with the rest of the language.
  The combination of ``-XNoFallibleDo`` and ``-XNoIncomplete`` would make those incomplete patterns errors like all the others under ``-XNoIncomplete``.
  This keeps the "knobs" orthogonal, and also keeps the language consistent whether ``Incomplete`` is disabled or not.

Costs and Drawbacks
-------------------

- While Haskell 2010 works well with a simple completeness checker that wouldn't be too hard to specify, extensions like ``GADTs`` that could be standardized in the future immediately ramp up the desire for a far more sophisticated pattern match completeness checker.
  Standardizing ``Incomplete`` could make it harder standardize ``GADTs`` and those other extensions later.

  See the future work section for how this is mitigated.

- This proposal effectively promotes some GHC warnings to a language extension.
  While GHC strives for comparability, ensuring programs with ``-Werror=...``, once accepted, continue to be accepted in future releases was lower down on the prioritization of aspects of compatibility GHC sought to ensure.
  This makes the completeness checker improvements changes more risky, as they now entail somewhat stronger comparability guarantees.

  See the future work section for how this is mitigated.

- In the short term, we have far too many knobs to control the same thing with each new ``-fdefer-*`` flag corresponding to an existing warning.
  But, my hope is that in future versions of the language ``Incomplete`` can be deprecated and then removed, reducing the number of knobs back to something sane.

- Somebody is going to think this has something to do with Kurt Gödel unless we choose a different name.

Possible Future Work
--------------------

The drawbacks are serious enough that I feel compelled to sketch some future plans that would mitigate them.

Missing vs intentionally omitted patterns
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The chief drawback with pattern matching today as it relates to completeness checking is that the Haskell implementation cannot tell whether a user skipped a pattern because they think it's impossible or because they forgot.
If we had something vaguely like Agda's "absurd patterns", however, the user could use those to use those cover the impossible the cases, making clear that anything else the user really did forget.

  The following examples use a completely strawman syntax.
  As this isn't part of the proposal proper I don't wish to take up more space in this proposal working out a proper spec.

Using absurd patterns for types like plain ``Void`` may seem low value::

  \case {}

Is hardly worse than::

  \case ABSURD

Laziness also helps, my making one have to force nested patterns anyways.
But therein we also get a new shorthand::

  \case Identity (v :: Void) -> case v of {}

can become::

  \case Identity !ABSURD


Finally, the real benefit is with absurdities stemming from type equalities.
For example, given:

::

  data G :: Type -> Type where
    GBool :: G Bool
    GInt :: G Int

instead of:

::

  f :: G Bool -> ...
  f GBool -> ...

do something like:

::

  f :: G Bool -> ...
  f GBool -> ...
  f (ABSURD @ GInt)

Firstly, this should help make code more self-documenting and allow for better error/warning messages.
But more relevant to the problem at hand, this allows simpler Haskell implementations that aren't so sophisticated that they can derive proofs of pattern impossibility very well on their own, but can verify user-written arguments.
This allows extension conjunctions like ``NoIncomplete`` and ``GADTs`` to be specified in ways that are less onerous on the Haskell implementation.

Desugaring advanced completeness checks with Dependent Haskell
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

While offering "absurd patterns" is the lowest hanging fruit to improve the situation, we may soon to have an opportunity to better specify the advanced analyses that GHC does so absurd patterns are not the only defense against different Haskell implementations differing in behavior.
With Dependent Haskell, we should have an opportunity for pattern matching to introduce equality constraints between *terms* just as it already does with equality constraints on types.
This would allow reformalizing e.g. the recent analysis allowing incomplete-seeming scrutinizing of a value within an alternative of a previous scrutinizing of that value.
Now, the proof obligation on the impossibility of the other branches can be solved by the constraint solver not an ad-hoc extra compilation pass.
If we find some way to introduce *in*\ equality constraints, we might accumulate proofs of pattern refutations and desugar *most or all* pattern match exhaustiveness checking into problems for the constraint solver.
This is all quite advanced, but hopefully demonstrates show how we could have more powerful and yet easier pattern match completeness checking today.
We certainly don't have to do this, but I hope to show it might at least be possible to have our cake and at it too, codifying ``NoIncomplete`` and advanced type system features, not relying on user-written proofs with absurd pattern, and also not dumping ad-hoc static analyses in a Haskell Report.

Alternatives
------------

- Tweaks to the exact flags:

  - Use the original warning categories instead of ``deferred-*`` variants for the deferred errors.
    (``missing-fields`` would be used instead of a new ``incomplete-record-construction`` with identical meaning.)

  - Use one ``deferred-incompleteness`` warning category.

  - Use more ``-fdefer-*`` flags, so we have one per warning category.

- Have no defer mechanism at all, forcing the user to write a manual error message themselves like in ML or Rust.
  I am not really that opposed, but I think this would just make the proposal more controversial to little benefit.

- Deprecate ``Incomplete`` immediately.

Unresolved Questions
--------------------

Any other source of implicit partiality I forgot?
I compiled this list by looking at the `instances`_ for the ``Exception`` class in ``base``.

Implementation Plan
-------------------

This should be very easy to implement since all the analyses exist in warnings already.

Endorsements
-------------

.. _`Proposal 126`: https://github.com/ghc-proposals/ghc-proposals/pull/126
.. _`Proposal 319`: https://github.com/ghc-proposals/ghc-proposals/pull/319
.. _`instances`: https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Exception-Base.html#t:Exception
