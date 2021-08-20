Coordinate proposals with an "Apsirational User's Guide"
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

This is a meta-proposal to change the proposal format and process.
We currently propose changes to an *implicit* state, the current design which is a roll-up of GHC and all unimplemented, accepted proposals.
Instead, we should propose changes to an *explicit* design document, which is a copy of the GHC User's Guide known as the "Aspirational User's Guide".
This will catch subtle design interactions earlier at proposal time rather than implementation design, and encourage holistic rather than piecemeal planning.

Motivation
----------

As stated in the summary, we currently propose changes against...nothing in particular.
The proposals are meant to clearly specify a change, which they do, but what is not specified clearly is *what* is being change.
"The design", yes, but where is it? In our heads.

Keeping track of this design is a lot of hard mental labor, even for our best experts and those making the decisions.

An example of this is the situation we recently got into with a few pattern- and scoping-related proposals.
Joachim's [email] to the GHC Steering Committee list about this issue is very instructive.
There are 3 accepted but unimplemented proposals and 3 open proposals all touching on related portions of the design, and the result is very hard to follow.
In this situation, the solution adopted was that Richard would go make a mega proposal combining all of them.
That's fine, but also a lot of work.
Wouldn't it be nice if we could still proposal incremental changes but also track their ramifications without "applying patches" mentally?

[email]: https://mail.haskell.org/pipermail/ghc-steering-committee/2021-August/002571.html

Proposed Change Specification
-----------------------------
Specify the change in precise, comprehensive yet concise language. Avoid words
like "should" or "could". Strive for a complete definition. Your specification
may include,

* BNF grammar and semantics of any new syntactic constructs
  (Use the `Haskell 2010 Report <https://www.haskell.org/onlinereport/haskell2010/>`_ or GHC's ``alex``\- or ``happy``\-formatted files
  for the `lexer <https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Parser/Lexer.x>`_ or `parser <https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Parser.y>`_
  for a good starting point.)
* the types and semantics of any new library interfaces
* how the proposed change interacts with existing language or compiler
  features, in case that is otherwise ambiguous

Strive for *precision*. The ideal specification is described as a
modification of the `Haskell 2010 report
<https://www.haskell.org/definition/haskell2010.pdf>`_. Where that is
not possible (e.g. because the specification relates to a feature that
is not in the Haskell 2010 report), try to adhere its style and level
of detail. Think about corner cases. Write down general rules and
invariants.

Note, however, that this section should focus on a precise
*specification*; it need not (and should not) devote space to
*implementation* details -- there is a separate section for that.

The specification can, and almost always should, be illustrated with
*examples* that illustrate corner cases. But it is not sufficient to
give a couple of examples and regard that as the specification! The
examples should illustrate and elucidate a clearly-articulated
specification that covers the general case.

Examples
--------
This section illustrates the specification through the use of examples of the
language change proposed. It is best to exemplify each point made in the
specification, though perhaps one example can cover several points. Contrived
examples are OK here. If the Motivation section describes something that is
hard to do without this proposal, this is a good place to show how easy that
thing is to do with the proposal.

Effect and Interactions
-----------------------
Your proposed change addresses the issues raised in the motivation. Explain how.

Also, discuss possibly contentious interactions with existing language or compiler
features. Complete this section with potential interactions raised
during the PR discussion.


Costs and Drawbacks
-------------------
Give an estimate on development and maintenance costs. List how this effects
learnability of the language for novice users. Define and list any remaining
drawbacks that cannot be resolved.


Alternatives
------------
List alternative designs to your proposed change. Both existing
workarounds, or alternative choices for the changes. Explain
the reasons for choosing the proposed change over these alternative:
*e.g.* they can be cheaper but insufficient, or better but too
expensive. Or something else.

The PR discussion often raises other potential designs, and they should be
added to this section. Similarly, if the proposed change
specification changes significantly, the old one should be listed in
this section.

Unresolved Questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and
specification. Be upfront and trust that the community will help. Please do
not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to
the steering committee.


Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other resources
and prerequisites are required for implementation?

Endorsements
-------------
(Optional) This section provides an opportunty for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.
