Clean up and simplify the treatment of implicit binding
==============

.. author:: John Ericson
.. date-accepted:: 
.. ticket-url:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/523>`_.
.. sectnum::
.. contents::

I believe `#448`_ landed with some ill-advised decisions, and I wish to correct them.


Motivation
----------

Summary of changes to other documents in this repo, and their motivation

Principles
~~~~~~~~~~

Philosophical reason for change
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A positive (to me) thing I appreciated near the end of the `#448`_ review process was that @gridaphobe agreed with my assessment that some of the principles were insufficiently distinct.
We were thus able to reach an agreement with @goldfirere to axe the "local lexical scoping principle", and fold it in to the "lexical scoping principle".

This was a great start, but upon rereading the principles (in order to connect the amended proposals to them), I realized that the "lexical scoping principle" an "explicit binding principle" still felt too close, and awkwardly dividing the labor between them.

I thus flipped their order ("explicit binding principle" first) and gave them the following roles (pithily summarized):

#. **Explicit Binding Principle**: Every implicit binding form needs an explicit equivalent.
#. **Lexical scoping principle**: Must be possible to turn off implicit binding forms so only the explicit ones remain.

I think this is much simpler and easy to understand.

Practical reason for change
^^^^^^^^^^^^^^^^^^^^^^^^^^^

A main change to Proposal `#448`_, as described below, is combining mechanisms to disable implicit-binding.
The idea of having multiple mechanism dates back to `#285`_ (also by me).
Originally I *did* propose in `#285`_ a single language extension knob, but then it was requested that that knob be split into two, and so I changed it.

I am undoing that here, something which was explicitly requested by the committee, and so I want to be sure I get the reasoning right (and am not being merely contrarian).

I think the root cause for the request of the spliting into multiple knobs was a sense that the different forms of implicit binding we had are *not* alike.
I think the old version of the two principles I changed somewhat obliquely supported that view, in its distinction-making.
I do think the division of labor between the old two principles was subtle and confusing, and the two old knobs overkill, but I do want to respect the fact that the principles and design were in sync.

The two new reworked principles, by contrast, have a chief goal in emphasizing the *commonalities* between all implicit binds.
They don't all (yet!) have an explicit alternative, but they should, and tentative proposals exist (see below) fill in those gaps.
(I.e. we are in the process of satisfying the **Explicit Binding Principle**, and I suspect we will complete it.)
This leaves the **Lexical scoping principle**, and then we have a *single* knob to bring us into compliance with a *single* feature.
The principles and design are once again back in sync!

Proposal `#425`_
~~~~~~~~~~~~~~~~

I just updated to refer to principles.
Most of this proposal ended up being about clarifying very nuanced issues with type families and data families, as that was where the complexity lies.
But the "easy parts" for just explicitness and syntactic uniformity are very much part of the 448 story and thus relate to the same principles.

Proposal `#448`_
~~~~~~~~~~~~~~~~

A single unified ``-XImplicitBinds``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Originally this is how `#285`_ was.
Then someone convinced me pattern signature binding and implicit foralls are quite different.
More recently, we realized some of the `#285`_ examples were not covered by *either* extension.
Too bad! Because those examples are of things people wanting the other restricted behavior would also want.

One solution was to make a *third* `-XNo` relating to binds, to pick up the missing things.
Believe me, I was tempted! But, I know everyone is getting weary of type variable extensions :).

I think the better solution --- which I went with --- is just to recombine things.
Yes, implicit foralls and implicit pattern signature binds are indeed *not* the same, but the *motivations* for why to disable them are.
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

`#270`_ has a very nice story about making single name-space code not fork-like by accepting fewer programs.
``-Werror=pattern-signature-binds`` either breaks the "non fork-like" condition, or breaks the "single namespace" condition.

For example, suppose a single namespace user wrote::

  import Foo (B) -- from `type B = Int`

  a = Int

  f (x :: a) = ....
  f (x :: B) = ....

``x :: B`` definitely doesn't bind ``B``, but what about ``x :: a``?

#. If ``a`` is a use, then if this code is copy pasted to another file without ``-Wpuns -Wpun-bindings`` it will *change meaning**::

     f (x :: Int) = ... -- no puns, after inlining
     f = let type a = _ in \(x :: a) -> ... -- yes puns

   This makes the pun change fork-like, and disqualifies it from being a warning.
   That is exactly the sort of outcome many people are worried about "no pun idem" code leading to, and which the authors of `#270`_ have worked strenuously to avoid.

#. If ``a`` is a bind, the meaning with and without puns will be the same::

     f = let type a = _ in \(x :: a) -> ... -- no puns
     f = let type a = _ in \(x :: a) -> ... -- yes puns

   but the "illusion" of a single namespace is shattered, because ``a`` and ``B`` are *not* treated the same way.

   Furthermore, as a practical matter, how *should* ``a`` be used here?
   ``-Werror=pattern-signature-binds`` will catch and error on our pattern signature bind, but it won't allow us to refer to ``a``.
   Not being able to use without rebinding it leaves an awkward tripping point that must be worked around with more variable indirection.

With ``-XNoImplicitBinds`` instead, we go with option 2 to ``-Wpuns`` and ``-Wpun-binding`` stay genuine warnings that do not change behavior.
``-XImplicitBinds`` however *can* change behavior, and it results in the same behavior (unification variable) regardless of ``-Wpuns`` and ``-Wpun-binding``.
Documentation can make clear that, yes, the punning warnings are insufficient on their own to create the single-namespace "illusions", but that are intended to work with ``-XNoImplicitBinds`` in which case they do succeed in their intended purpose.

This relates to the education case in that both are about being able to hide what "might have been" under other config settings.

Proposal `#523`_
~~~~~~~~~~~~~~~~

This unmerged proposal is referenced in `#448`_ (in PR form, with no implication that it is eventually accepted).
The reason for this I think is worth elaborating on.

I think the reason we got into the confusing situations we have so far is because pattern signature binds are not obviously "syntactic sugar", in that there is no simple non-type-directed desugaring of what they do.
I am steadfast that any such "weird" feature is "sugar in waiting" --- we simply need to create the much simpler primitives until it is sugar, but others are more "wait and see" and "by the book", and therefore don't want to ascribe to something the negative connotations of syntactic sugar until it is manifestly clear that it in fact is syntactic sugar.

`#523`_ fixes this, by hinting at (it is not fully specified yet) the ``let type var = _ in`` syntax that can be used instead of pattern signature binds.
The desugaring is simple, not type directed, and only rename-directed in that we need to know what variables are as-of-yet not explicitly bound.

I don't call pattern signature binding "sugar" in the revised text, but I do call it "implicit', because any syntax that could be either a use or a binding based on the context (of in-scope variables) I define as "implicit".
For those not comfortable with this yet, I suggest we hurry up and accept `#523`_ so that it is also unambiguously "implicit" and "sugar" by having the explicit ``let type var = _ in`` syntax it can be desugared to.

.. _`#270`: https://github.com/ghc-proposals/ghc-proposals/pull/270
.. _`#285`: ./0285-no-implicit-binds.rst
.. _`#425`: ./0425-decl-invis-binders.rst
.. _`#448`: ./0448-type-variable-scoping.rst
.. _`#523`: https://github.com/ghc-proposals/ghc-proposals/pull/523
