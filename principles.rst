.. sectnum::
.. highlight:: haskell

Principles for GHC
==================

This document lays out high-level principles for the evolution of GHC
and the language it compiles. In much the way that we like to write
*properties* of our functions -- instead of just writing down individual
test cases -- this document strives to write *principles* that our proposals
ought to maintain (or build towards).

Articulating these principles helps to guide future proposals: proposals
that maintain the principles in this document are more likely to be accepted,
while proposals that work against these principles are more likely to be rejected.
Note that neither direction is (at all) a guarantee: there will be excpetions
in both directions. Yet by writing down the principles, we can have an informed
discussion of the tradeoffs of accepted or rejecting an individual proposal.

How to update these principles
------------------------------

When making a proposal, following the `usual guidelines <https://github.com/ghc-proposals/ghc-proposals/#how-to-start-a-new-proposal>`_,
feel free to include a diff against this listing of principles. We can then
discuss the validity of the new principle(s) alongside the concrete proposal for
a change to GHC.

We urge proposers to resist the temptation to propose principles without an
accompanying concrete change to GHC. Debating principles in the abstract does
not seem a productive use of time, though it is possible to imagine exceptions.
For example, the justification for the rejection of a proposal may be distilled into
a new principle here. Accordingly, this document is not (and presumably never will be)
comprehensive: it contains the articulated prinicples guiding GHC's development
but lacks the unarticulated principles, which will be added over time.

All principles in this document are linked back to the PR that introduce them,
so that readers can learn the context in which they were written.

Accepted principles
-------------------

.. _`#378`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst

Syntax
~~~~~~

.. _SUP:

**Syntactic Unification Principle (SUP).** (From `#378`_.) In the absence of punning, there is
no difference between type-syntax and term-syntax.

The SUP_ is a *long term* goal; today, there are many, many violations of this principle. (For example,
``case`` can never appear in types, and ``Int -> Bool`` is meaningless in a term.) However, this principle
should direct future proposals not to make the situation worse, and proposals that bring us closer to
the SUP_ are to be valued.

Name resolution
~~~~~~~~~~~~~~~

.. _LSP:

**Lexical Scoping Principle (LSP)**. (From `#378`_.) For every *occurrence* of an
identifier, it is possible to uniquely identify its *binding site*, without
involving the type system.

The LSP_ is true today, though Template Haskell splices may need to be run before
completing name resolution (and running those splices requires type-checking them).

Semantics
~~~~~~~~~

.. _PEP:

**Predictable Erasure Principle (PEP)**. (From `#378`_.) The programmer knows, for sure, which bits of the program will be
retained at runtime, and which will be erased.

The PEP_ is true today: types are erased, while terms are retained.

User experience
~~~~~~~~~~~~~~~

.. _OIP:

**The Opt-In Principle (OIP):** (From `#378`_, slightly generalized.) Users who do not opt into an advanced feature will
not be affected by it.

This principle is violated in various ways today: it is easy for GHC to generate error messages that refer to
advanced features even when writing simple code. In addition, the existence of advanced features likely slow
down GHC even when those features are not active. Yet this principle is important to keep in mind going forward,
as we hope not to make the current situation worse.