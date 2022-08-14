Clean up and simplify the treatment of implicit binding
==============

..
author:: John Ericson
..
date-accepted:: Leave blank.
This will be filled in when the proposal is accepted.
..
ticket-url:: Leave blank.
This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
..
implemented:: Leave blank.
This will be filled in with the first GHC version which
                 implements the described feature.
..
highlight:: haskell
..
header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/523>`_.
..
sectnum::
..
contents::

I believe #448 landed with some ill-advised decisions, and I wish to correct them.


Motivation
----------

Summary of changes to other documents in this repo, and their motivation

Principles
~~~~~~~~~~

A positive (to me) thing I appreciated near the end of the #448 review process was that @gridaphobe agreed with my assessment that some of the principles were insufficiently distinct.
We were thus able to reach an agreement with @goldfirere to axe the "local lexical scoping principle", and fold it in to the "lexical scoping principle".

This was a great start, but upon rereading the principles (in order to connect the amended proposals to them), I realized that the "lexical scoping principle" an "explicit binding principle" still felt too close, and awkwardly dividing the labor between them.

I thus flipped their order ("explicit binding principle" first) and gave them the following roles (pithily summarized):

#.
**Explicit Binding Principle**: Every implicit binding form needs an explicit equivalent.
#.
**lexical scoping principle**: Must be possible to turn off implicit binding forms so only the explicit ones remain.


I think this is much simpler and easy to understand.

Proposal #425
~~~~~~~~~~~~~

I just updated to refer to principles.
Most of this is about very nuanced arity, but the "easy parts" are very much part of the 448 story and thus relate to the same principle.

Proposal #448
~~~~~~~~~~~~~

A single unified ``-XImplicitBinds``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Originally this is how #285 was.
Then someone convinced me pattern synonym binding and implicit foralls are quite different.
More recently, we realized some of the #285 examples were not covered by *either* extension.
Too bad! Because those examples are of things people wanting the other restricted behavior would also want.

One solution was to make a *third* `-XNo` relating to binds, to pick up the missing things.
Believe me, I was tempted! But, I know everyone is getting weary of type variable extensions :).

I think the better solution --- which I went with --- is just to recombine things.
Yes, implicit foralls and implicit pattern synonym binds are indeed *not* the same, but the *motivations* for why to disable them are.
The same people that dislike one of them dislike all of them, and vice versa.
Likewise, the same motivations around education and syntactic consistency that apply to one of them apply to all of them.

``-Werror=pattern-signature-binds`` considered inadequate
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Finally, note that `-XNoPatternSignatureBinds`, one of the former constituents of `-XNoImplicitBinds`, had been downgraded to a warning.
This might seem fine (use `-Werror=...`!) but it really isn't, because it fails each step of the motivation:

Education
^^^^^^^^^

The point of disabling features with `-XNo` is so the student can be *completely unaware* they exist.
But warnings must always be phrased in terms "that thing you did you might not have wanted to".
That means making the student aware of the thing after all --- "that thing you did" is something the student was never taught and therefore should never be goaded by a warning into learning about after all.

Concretely, for educational purposes we want to get rid of implicit bindings, and get rid of the *concept* of implicit binding.
We want the student to be *unable* to write them, and we want the compiler to *not* tell them with other configuration options feature exists, the same way a Haskell 98 users should not be told about "type families" or weather.

Single namespace syntactic uniformity
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#270 has a very nice story about making single name-space code not fork-like by accepting fewer programs.
``-Werror=pattern-signature-binds`` either breaks the "non fork-like" condition, or breaks the "single namespace" condition.
Either is not acceptable.

This relates to the education case in that both are about being able to hide what "might have been" under other config settings.

Proposal #523
~~~~~~~~~~~~~

This unmerged proposal is referenced in #448 (in PR form, with no implication that it is eventually accepted).
The reason for this I think is worth elaborating on.

I think the reason we got into the confusing situations we have so far is because pattern signature binds are not obviously "syntactic sugar", in that there is no simple non-type-directed desugaring of what they do.
I am steadfast that any such "weird" feature is "sugar in waiting" --- we simply need to create the much simpler primitives until it is sugar, but others are more "wait and see" and "by the book", and therefore don't want to ascribe to something the negative connotations of syntactic sugar until it is manifestly clear that it in fact is syntactic sugar.

#523 fixes this, by hinting at (it is not fully specified yet) the ``let type var = _ in`` syntax that can be used instead of pattern synonym binds.
The desugaring is simple, not type directed, and only rename-directed in that we need to know what variables are as-of-yet not explicitly bound.

I don't call pattern signature binding "sugar" in the revised text, but I do call it "implicit', because any syntax that could be either a use or a binding based on the context (of in-scope variables) I define as "implicit".
For those not comfortable with this yet, I suggest we hurry up and accept #523 so that it is also unambiguously "implicit" and "sugar" by having the explicit ``let type var = _ in`` syntax it can be desugared to.
