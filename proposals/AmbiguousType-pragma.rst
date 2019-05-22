Notes on reStructuredText - delete this section before submitting
==================================================================

The proposals are submitted in reStructuredText format.  To get inline code, enclose text in double backticks, ``like this``.  To get block code, use a double colon and indent by at least one space

::

 like this
 and

 this too

To get hyperlinks, use backticks, angle brackets, and an underscore `like this <http://www.haskell.org/>`_.


Proposal title
==============

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

Text for pull request [move there when PR created].

Per-signature pragma ``{-# AMBIGUOUS #-}`` to allow that signature's type to be ambiguous; replacing the module-wide ``-XAllowAmbiguousTypes``, which dangerously lifts ambiguity checking on all signatures.

Also tweak the error reporting to avoid recklessly suggesting users turn on ``-XAllowAmbiguousTypes``; and provide a flag ``-Wallowed-ambiguous-types`` that shows the ambiguity message as a warning.

To do: Here you should write a short abstract motivating and briefly summarizing the proposed change.

This proposal was the residue from discussions around 'Top-level signatures' #148. Thank you to @goldfirere and @int-index.

Motivation
------------
Give a strong reason for why the community needs this change. Describe the use case as clearly as possible and give an example. Explain how the status quo is insufficient or not ideal.


Proposed Change Specification
-----------------------------

1. There is to be a pragma ``{-# AMBIGUOUS #-}``, to appear immediately after the ``::`` of a function or method definition's signature (so before the type). Not applicable for term type annotations beginning ``::``, nor for pattern signatures. Example
::

 class Sized a  where
   sizeOf :: {-# AMBIGUOUS #-} Integer

2. Types marked ``AMBIGUOUS`` are to be validated as if ``-XAllowAmbiguousTypes`` is set, for that signature only. (If that is already set module-wide, the pragma has no further effect.)

3. This does not change the validation for ambiguous types/type variables at usage sites.

4. The error reporting from the ambiguity check that currently suggests ``To defer the ambiguity check to use sites, enable AllowAmbiguousTypes`` must make clear this is likely to entail using ``TypeApplications`` at usage sites, and that there are several possible approaches to avoid ambiguous type variables.

 Precise wording to be arrived at in discussion of this PR. (Prefer not mentioning ``AllowAmbiguousTypes`` at all.)
 
 5. There is to be a flag ``-Wallowed-ambiguous-types`` controlling whether a warning is raised for ambiguous types -- allowed either from the ``AMBIGUOUS`` pragma or ``-XAllowAmbiguousTypes``.




Effect and Interactions
-----------------------
By lifting the ambiguity check only for signatures deliberately flagged, this ensures ambiguity checking does apply for the bulk of the signatures in the program *at the definition site*. Then ambiguity is less likely to manifest at *usage* sites, where it is more difficult to diagnose -- particularly if that is in a separate module.

The proposed behaviour affects only validate and error/warning messages, not type checking rules or type inference.

Existing code using ``AllowAmbiguousTypes`` is not affected. That is, ambiguities are not checked. The migration path is:

* Switch on ``-Wallowed-ambiguous-types``; compile the module to examine signatures that are currently ambiguous.

* If their ambiguity is expected and understood; mark as ``{-# AMBIGUOUS #-}``. Otherwise diagnose and correct.

* Remove the ``LANGUAGE AllowAmbiguousTypes`` setting and recompile.


Costs and Drawbacks
-------------------
Give an estimate on development and maintenance costs. List how this effects learnability of the language for novice users. Define and list any remaining drawbacks that cannot be resolved.


Alternatives
------------
Do nothing. That is, continue with the module-wide ``AllowAmbiguousTypes`` setting.

    These definitions do not compromise type safety or class coherence. If you don't use ``-XTypeApplications``, then they're just useless definitions. [@goldfirere commenting in #148]
    
I would disagree with that "useless". I see the confusion they cause as harmful. Especially because that follows from the error message's misleading ``enable AllowAmbiguousTypes``.

Unresolved questions
--------------------

* Precise wording to be discussed for the rejection message that currently suggests enabling ``AllowAmbiguousTypes``.

* Re pragmas that change semantics (such as the ``{-# OVERLAPPABLE #-}`` series), there has been comment they're difficult for source tooling utilities to observe. As well as the ``AMBIGUOUS`` pragma per signature, should there be a module-wide ``LANGUAGE`` setting? ``-XAllowAmbiguousTypesPragma``.

* For modules containing more ambiguous types than not, so with ``AllowAmbiguousTypes`` switched on, should there be a per-signature pragma ``{-# NOAMBIGUOUS#-}`` that *does* apply the ambiguity check?


Implementation Plan
-------------------

I am not accredited to interfere in GHC's type checking. Hopefully this is a narrowly targetted mod that merely suppresses the rejection message, if the pragma is present in the AST for the signature.

