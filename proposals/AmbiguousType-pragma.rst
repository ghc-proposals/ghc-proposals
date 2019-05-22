

Ambiguous Type per-signature pragma
===================================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

.. Text for pull request [move there when PR created]:

Per-signature pragma ``{-# AMBIGUOUS #-}`` to allow that signature's type to be ambiguous; instead of the module-wide ``-XAllowAmbiguousTypes``, which dangerously lifts ambiguity checking on all signatures.

Also tweak the error reporting to avoid recklessly suggesting users turn on ``-XAllowAmbiguousTypes``; and provide a flag ``-Wallowed-ambiguous-types`` that shows the ambiguity message as a warning.

.. Here you should write a short abstract motivating and briefly summarizing the proposed change.

Provide a per-signature ``{-# AMBIGUOUS #-}`` pragma, to precision-control which signatures are allowed/intended to be ambiguous. This to be instead of the module-wide ``LANGUAGE AllowAmbiguousTypes`` setting. Because ambiguous signatures (at definition sites) for functions/methods are more likely to be an error than be intended -- even for code written to combine with ``-XTypeApplications`` at usage sites. Currently the ``-XAllowAmbiguousTypes`` lifts the ambiguity check indiscriminately for all signatures in a module.

The likelihood of error is made worse by GHC's current error reporting that suggests a blanket ``To defer the ambiguity check to use sites, enable AllowAmbiguousTypes``. Naive (and not-so-naive) users tend to grab for this suggestion which a) will only move the ambiguity to the usage site where it is more difficult to diagnose; and b) is liable to allow through other ambiguous signatures that were not intended. So this proposal also tightens error reporting and wording.

This proposal is a residue from discussions around 'Top-level signatures' #148. Thank you to @goldfirere and @int-index.

Motivation
------------


Ambiguous types/signatures at definition sites combine powerfully with ``TypeApplications`` at usage sites for fancy type trickery. This is an 'advanced' feature that should be opt-in. Currently ``LANGUAGE AllowAmbiguousTypes`` is a module-wide setting that abandons checking for ambiguity, whether or not the user intends to use some function/method with ``TypeApplications``.

Consider this code (a classic of its time)
::

    class Collects c e  where
      insert :: e -> c -> c
      empty :: c

GHC correctly rejects this::

    * Could not deduce (Collects c e0)
      from the context: Collects c e
        bound by the type signature for:
                   empty :: forall c e. Collects c e => c
      The type variable `e0' is ambiguous
    * In the ambiguity check for `empty'

So far so good. There are several possible approaches to avoiding this ambiguity

* Use a (Associated) Type Family to derive ``e`` from ``c``, so ``e`` can be removed as a type parameter (or constrained by an ``~`` to the TF.)
* Use a Functional Dependency.
* Use method ``singleton :: e -> c`` instead of ``empty`` with its failure to ground ``e``.

Any of those are better than what GHC suggests next::

    To defer the ambiguity check to use sites, enable AllowAmbiguousTypes

Novice users will switch on that option and get class ``Collects`` to compile. Great, thank you GHC. Except they are now in for a world of pain and confusion. At usage sites, they'll get a whole series of ``The type variable `e0' is ambiguous`` and ``Could not deduce (Collects [e1] e0)`` messages. If they persevere they might reach::

    instance Collects [e] e  where
      insert = (:)
      empty = []

    *> insert 'c' (empty :: [Char])

    * Ambiguous type variable `e0' arising from a use of `empty'
      prevents the constraint `(Collects [Char] e0)' from being solved.
      Probable fix: use a type annotation to specify what `e0' should be.

Grr. There *is* a type annotation on that ``empty``; and the ``insert 'c' ...`` is telling the type of ``e0``. Where/how on earth to specify ``e0`` otherwise? The answer (which they'll get on StackOverflow) is there's only one way: use a ``TypeApplications``. Why didn't GHC say that long ago? Why did GHC lead down the rabbit-hole of ``AllowAmbiguousTypes``? The User Guide is no help: for ``AllowAmbiguousTypes``, there's a series of examples where types at the usage site can be resolved by an annotation or a literal of specific type. There's brief mention of ``TypeApplications``. The patient helpers at StackOverflow are adept at winding users back to the definition site, and advising they probably didn't ever need to go down the rabbit-hole.

Oh, and thanks to ``AllowAmbiguousTypes`` being a module-wide setting, there's probably several other definitions in the module that didn't get ambiguity-checked. So at usage sites there's piles of these ambiguities to be unwound.

In an ideal world, that error message should probably say something long-winded to the effect: do you know what you're doing with ``TypeApplications``/is that what you intend at usage sites? Then you forgot to switch on ``AllowAmbiguousTypes`` in the definition module. (Oh, and watch out that any/all of your definitions might be ambiguous, I won't be checking them.) It should go on to say: if you don't understand what I'm talking about, probably there's a better way to avoid that ambiguity.

Then this proposal firstly aims to avoid users blundering blindly into ambiguous signatures, by improving error messages and warnings; secondly avoids the dangers of module-wide abandoning ambiguity checking.

Proposed Change Specification
-----------------------------

1. There is to be a pragma ``{-# AMBIGUOUS #-}``, to appear immediately after the ``::`` of a function or method definition's signature (so before the type). Not applicable for term type annotations beginning ``::``, nor for pattern signatures. Examples
::

    f :: {-# AMBIGUOUS #-} C a => Int

    class Sized a  where
      sizeOf :: {-# AMBIGUOUS #-} Integer

2. Signatures marked ``AMBIGUOUS`` are to be validated as if ``-XAllowAmbiguousTypes`` is set, for that signature only. (If that is already set module-wide, the pragma has no further effect.)

3. This does not change the validation for ambiguous types/type variables at usage sites.

4. The error reporting from the ambiguity check that currently suggests ``To defer the ambiguity check to use sites, enable AllowAmbiguousTypes`` must make clear this is likely to entail using ``TypeApplications`` at usage sites, and that there are several possible approaches to avoid ambiguous type variables.

 Precise wording to be arrived at in discussion of this PR. (Prefer not mentioning ``AllowAmbiguousTypes`` at all.) Starting bikeshed::
 
     The ambiguity might be resolvable through TypeApplications at use sites. Then mark this signature as AMBIGUOUS
 
5. There is to be a flag ``-Wallowed-ambiguous-types`` controlling whether a warning is raised for ambiguous types -- allowed either from the ``AMBIGUOUS`` pragma or ``-XAllowAmbiguousTypes``.




Effect and Interactions
-----------------------
By lifting the ambiguity check only for signatures deliberately flagged, this ensures ambiguity checking does apply for the bulk of the signatures in the program *at the definition site*. Then ambiguity is less likely to manifest at *usage* sites, where it is more difficult to diagnose -- particularly if that is in a separate module.

The proposed behaviour affects only validation and error/warning messages, not type checking rules or type inference.

Existing code using ``AllowAmbiguousTypes`` is not affected. That is, ambiguities are not checked. The migration path is:

* Switch on ``-Wallowed-ambiguous-types``; compile the module to examine signatures that are currently ambiguous.

* If their ambiguity is expected and understood; mark as ``{-# AMBIGUOUS #-}``. Otherwise diagnose and correct.

* Remove the ``LANGUAGE AllowAmbiguousTypes`` setting and recompile.


Costs and Drawbacks
-------------------

The proposal is for superficial tweaks to error reporting/warnings. There is no deep impact on type checking or inference.

For code intending to make heavy use of ``TypeApplications`` at usage sites, there may be many ambiguous signatures, needing many pragmas at definition sites that might be onerous to code. Against that, the per-signature pragma means that other definitions in the module do get properly checked against ambiguity.

GHC's suggestion ``To defer the ambiguity check to use sites, enable AllowAmbiguousTypes`` is currently costing a great deal of perplexity and frustration for novice and not-so-novice users. Evidence: StackOverflow questions anon. Switching on the option in the definition module is not likely to help anything compile, unless the user is consciously intending to use ``TypeApplications`` at the usage site/module. That may not be the best approach for the coding requirements (see Motivation section), but GHC's message does not suggest other options. Novice and not-so-novice users are likely to attach too much weight to that suggestion.


Alternatives
------------
Do nothing. That is, continue with the module-wide ``AllowAmbiguousTypes`` setting.

    These definitions do not compromise type safety or class coherence. If you don't use ``-XTypeApplications``, then they're just useless definitions. [@goldfirere commenting in #148]
    
I would disagree with that "useless". I see the confusion they cause as harmful. Especially because that follows from the error message's misleading ``enable AllowAmbiguousTypes``.

Unresolved questions
--------------------

* Precise wording to be discussed for the rejection message that currently suggests enabling ``AllowAmbiguousTypes``.

* Re pragmas that change semantics (such as the ``{-# OVERLAPPABLE #-}`` series), there has been comment they're difficult for source tooling utilities to observe. As well as the ``AMBIGUOUS`` pragma per signature, should there be a module-wide ``LANGUAGE`` setting? ``-XAllowAmbiguousTypesPragma``.

* For modules containing more ambiguous types than not, so with ``AllowAmbiguousTypes`` switched on, should there be a per-signature pragma ``{-# NOAMBIGUOUS #-}`` that *does* apply the ambiguity check?


Implementation Plan
-------------------

I am not accredited to interfere in GHC's type checking. Hopefully this is a narrowly targetted mod that merely suppresses the rejection message, if the pragma is present in the AST for the signature.

