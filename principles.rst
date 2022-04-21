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

We use a notational convention here where each principle is given an abbreviation,
but we use its full spelling wherever possible. The full spelling is useful for readers,
given the multitude of principles in play. Including the abbreviation is useful in case
the abbreviation is used in other fora (e.g. email, GitHub, etc.) to allow the curious
to find (e.g. with Ctrl+F) the abbreviation within this page.

Accepted principles
-------------------

.. _`#281`: proposals/0281-visible-forall.rst
.. _`#378`: proposals/0378-dependent-type-design.rst
.. _`#473`: proposals/0473-existentials.rst

Syntax
~~~~~~

Syntactic Unification Principle (SUP)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _`Syntactic Unification Principle`:

**Principle:** In the absence of punning, there is
no difference between type-syntax and term-syntax.

The `Syntactic Unification Principle`_ is a *long term* goal; today, there are many, many violations of this principle. (For example,
``case`` can never appear in types, and ``Int -> Bool`` is meaningless in a term.) However, this principle
should direct future proposals not to make the situation worse, and proposals that bring us closer to
the `Syntactic Unification Principle`_ are to be valued.

*Motivation:* The `Syntactic Unification Principle`_ keeps us forward-compatible with a possible future where the
distinction between term-syntax and type-syntax is removed.

From `#378`_.

Implicit/Explicit Principle (IEP)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _`Implicit/Explicit Principle`:

**Principle:** Each Haskell construct that allows the user to omit part of their program
should allow for both per-occurrence specification of the omitted fragment and a declaration
form that requires all occurrences to specify the omitted fragment.

This principle is a general design principle, and it must be interpreted in the context of
individual features; it is possible some features do not allow for the "require all occurrences"
part.

Example: ``forall``. Originally, all type instantiation was omitted, inferred by the compiler.
As GHC's type system became sufficiently complex, a need arose for visible type application, allowing
programmers to specify the omitted type on a per-occurrence bases. However, that, too, was not
enough, and now `#281`_ allows programmers to require that the instantiating type be supplied
at every occurrence. This principle intends to encourage the design of future features so that
we do not have to retrofit explicit overrides.

*Motivation:* Sometimes, the programmer really does know better than GHC, and the programmer
should be allowed to have their say.

From `#473`_.

Name resolution and scoping
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Lexical Scoping Principle (LSP)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _`Lexical Scoping Principle`:

**Principle**:

For every *occurrence* of an
identifier, it is possible to uniquely identify its *binding site*, without
involving the type system.

The `Lexical Scoping Principle`_ is true today, with two complications:

1. Template Haskell splices may need to be run before completing name resolution (and running those splices requires type-checking them).

2. The `deprecated mechanism <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/duplicate_record_fields.html#selector-functions>`_ for disambiguating duplicate record fields violates the `Lexical Scoping Principle`_ by requiring the type system.

*Motivation:* This principle means that we can understand the binding
structure of a program without relying on type inference, important both for the
implementation of GHC and the sanity of programmers.

From `#378`_.

Semantics
~~~~~~~~~

Predictable Erasure Principle (PEP)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _`Predictable Erasure Principle`:

**Principle**: The programmer knows, for sure, which bits of the program will be
retained at runtime, and which will be erased.

The `Predictable Erasure Principle`_ is true today: types are erased, while terms are retained.

From `#378`_.

User experience
~~~~~~~~~~~~~~~

Opt-In Principle (OIP)
^^^^^^^^^^^^^^^^^^^^^^

.. _`Opt-In Principle`:

**Principle**: Users who do not opt into an advanced feature will
not be affected by it.

This principle is violated in various ways today: it is easy for GHC to generate error messages that refer to
advanced features even when writing simple code. In addition, the existence of advanced features likely slow
down GHC even when those features are not active. Yet this principle is important to keep in mind going forward,
as we hope not to make the current situation worse.

From `#378`_, slightly generalized.