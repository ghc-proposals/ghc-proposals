BlockArguments Extension
========================

.. author:: Takano Akio
.. date-accepted:: 2017-12-05
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/10843
.. implemented:: 8.6.1
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/90>`_.
.. contents::

This document proposes the addition of an extension called ``BlockArguments``. This is a syntactic extension that allows a ``do`` block, a lambda, and a few other syntactic constructs to be written directly as a function argument, without parentheses or a ``$``.

Motivation
----------

In the Haskell grammar, there is a certain class of syntactic constructs that can be an argument to an operator, but not to a function. In Haskell 2010, This class includes lambda, ``do``, ``case``, ``if``, and ``let`` expressions. GHC Haskell adds a few more constructs to this category, such as ``mdo``, ``\case`` and ``proc`` blocks.

However, threre seems to be no compelling reason that those types of expressions should not be allowed in function argument positions. Having them in function argument positions allows one to remove some awkward parentheses from the code, and more frequently, to reduce the use of the ``$`` operator. At the same time, the change arguably simplifies the grammar by removing an artificial restriction from it. The proposed ``BlockArguments`` extension does precisely this.

With this extension, instead of writing:

::

 atomically $ do
   v <- readTVar tv
   writeTVar tv $! v + 1

one can write:

::

 atomically do
   v <- readTVar tv
   writeTVar tv $! v + 1

Also, instead of:

::

  1 + length (do
    x <- xs
    f x a)

one can write:

::

  1 + length do
    x <- xs
    f x a

Proposed Change Specification
-----------------------------

The Haskell report defines the lexp nonterminal thus (* indicates a rule of interest):

::

  lexp  →  \ apat1 … apatn -> exp            (lambda abstraction, n ≥ 1)  *
        |  let decls in exp                  (let expression)             *
        |  if exp [;] then exp [;] else exp  (conditional)                *
        |  case exp of { alts }              (case expression)            *
        |  do { stmts }                      (do expression)              *
        |  fexp

  fexp  →  [fexp] aexp                       (function application)

  aexp  →  qvar                              (variable)
        |  gcon                              (general constructor)
        |  literal
        |  ( exp )                           (parenthesized expression)
        |  qcon { fbind1 … fbindn }          (labeled construction)
        |  aexp { fbind1 … fbindn }          (labelled update)
        |  …

When enabled, the extension will make the following change to the grammar.

::

  lexp  →  fexp

  fexp  →  [fexp] aexp                       (function application)

  aexp  →  qvar                              (variable)
        |  gcon                              (general constructor)
        |  literal
        |  ( exp )                           (parenthesized expression)
        |  qcon { fbind1 … fbindn }          (labeled construction)
        |  aexp { fbind1 … fbindn }          (labelled update)
        -- Here are the moved rules
        |  \ apat1 … apatn -> exp            (lambda abstraction, n ≥ 1)  *
        |  let decls in exp                  (let expression)             *
        |  if exp [;] then exp [;] else exp  (conditional)                *
        |  case exp of { alts }              (case expression)            *
        |  do { stmts }                      (do expression)              *
        |  …

Now the lexp nonterminal is redundant and can be dropped from the grammar.

Note that this change relies on the existing meta-rule to resolve ambiguities:

    The grammar is ambiguous regarding the extent of lambda abstractions, let expressions, and conditionals. The ambiguity is resolved by the meta-rule that each of these constructs extends as far to the right as possible.

For example, ``f \a -> a b`` will be parsed as ``f (\a -> a b)``, not as ``f (\a -> a) b``.

Effect and Interactions
-----------------------

The proposed extension does not change the meaning of any valid program. It just accepts more programs.

The following constructs that are added by other GHC extensions will also be allowed in function argument positions when ``BlockArguments`` is enabled:

* ``\case``

* Multi-way ``if``

* ``mdo``

* ``proc``

The following pragmas are currently handled by the same parsing rule as ``do``, etc.. They will *not* be affected by the proposed extension.

* ``{-# SCC #-}``

* ``{-# CORE #-}``

Allowing expressions lead by one of these pragmas in argument positions would create cases that violate the principle that pragmas must be able to be safely ignored. For example, ``f {-# SCC "A" #-} a b`` would parse differently from ``f a b``, which is bad.

The proposed extension has some not-so-obvious consequences. First, it allows one to pass multiple ``do``-blocks to a single function:

::

  f do{ x } do{ y }

or equivalently:

::

  f
    do x
    do y

Second, it allows a ``do``-block etc. to act as a function that is applied to an argument. For example,

::

  do f &&& g
  x

will be equivalent to:

::

  (f &&& g) x

If, one day, this extension becomes part of the Haskell standard, we may consider dropping the hack in the type checker where it allows impredicative instantiations of the ``$`` operator.

Costs and Drawbacks
-------------------

I have a `preliminary implementation <https://github.com/takano-akio/ghc/commits/argumentdo>`_ of this proposal, and I expect the remaining cost of implementation to be low (< 5 hours).

Unless a special care is taken, an implementation will add a large number of shift-reduce conflicts to the parser, due to the reliance on the meta-rule mentioned above. The only problem I can see with this is some maintenance overhead (someone making a parser change and seeing an unexpected shift/reduce conflict may have harder time tracking it down). I don't know how big a problem this is.

Since this is a syntactic extension, there is an inherent cost in terms of learnability and a potential cultural fragmentation within the community. I believe these costs to be minimal, because I expect that the meaning of practical code written using the proposed extension will be quite clear, even for those who do not know about the extension.

In previous discussions `on Haskell-cafe <https://mail.haskell.org/pipermail/haskell-cafe/2015-September/121217.html>`_ and `on Reddit <https://www.reddit.com/r/haskell/comments/447bnw/does_argument_do_have_a_future/>`_, the proposal was met with a mixed response. In particular, it has been pointed out that some people find those expressions without ``$`` harder to read.

Alternatives
------------

* Do nothing.

* In argument positions, only allow ``do`` expressions, not any other expressions like lambdas. An argument for this alternative is that ``do`` expressions is clearly marked at their end (either with a curly brace or layout), whereas other expressions have less visible endings and can be visually confusing. A problem with this alternative is that it seems hard to justify the special-casing of ```do```. Users may end up having to remember one more arbitrary rule.

* Allow blocks in the RHS of the function application, but not in the LHS. This has the advantage of catching more errors in the parser (rather than in the typechecker), because such expression is most likely a mistake. A downside of this alternative is the need of a special case in the grammar. Below is one example grammar that implements the alternative:

::

  lexp  →  fexp
        | block                              (standalone block)

  fexp  →  [fexp] aexp                       (function application)
        |  fexp block                        (block application)

  aexp  →  qvar                              (variable)
        |  gcon                              (general constructor)
        |  literal
        |  ( exp )                           (parenthesized expression)
        |  qcon { fbind1 … fbindn }          (labeled construction)
        |  aexp { fbind1 … fbindn }          (labelled update)
        |  …

  block →  \ apat1 … apatn -> exp            (lambda abstraction, n ≥ 1)  *
        |  let decls in exp                  (let expression)             *
        |  if exp [;] then exp [;] else exp  (conditional)                *
        |  case exp of { alts }              (case expression)            *
        |  do { stmts }                      (do expression)              *

Unresolved questions
--------------------

Implementation Plan
-------------------

If accepted, I (@takano-akio) will implement this change.
