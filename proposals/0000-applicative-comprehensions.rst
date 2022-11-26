Applicative Comprehensions
==========================

.. author:: Matthew Farkas-Dyck
.. date-accepted:: 
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/10976
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/526>`_.
.. sectnum::
.. contents::

Note: This proposal includes content from the `original proposal <https://gitlab.haskell.org/ghc/ghc/-/wikis/applicative-comprehensions>`_ on the GHC wiki, written by Tobias Dammers.

The `ApplicativeComprehensions` extension would make GHC try to desugar comprehensions to code needing only an `Applicative` (rather than a `Monad`) constraint, if possible. It generalizes `MonadComprehensions`.

A comprehension in which no variable bound in a generator statement is used in any other generator expression can be desugared thus. For example:

::

  [b | a₁ <- x₁, a₂ <- x₂, …]

can be desugared as

::

  (\ a₁ a₂ … -> b) <$> x₁ <*> x₂ <*> …

but

::

  [c | a <- x, b <- f a]

needs a `Monad` constraint.

Motivation
----------

As explained in the *Motivation* section for `ApplicativeDo <https://gitlab.haskell.org/ghc/ghc/-/wikis/applicative-do>`_, some kinds of `Applicative` code can be difficult to read. For example (given there):

::

  (\ x y z -> x*y + y*z + z*y) <$> expr1 <*> expr2 <*> expr3

However, as an applicative comprehension:

::

  [x*y + y*z + z*x | x <- expr1, y <- expr2, z <- expr3]

At least in this author's opinion, it is easier to follow, having fewer operators and clearer structure, but nonetheless looks much like the desugared code, particularly compared to the equivalent `ApplicativeDo` expression.

Furthermore, `ApplicativeComprehensions` has some upside over `ApplicativeDo`: the latter as implemented in GHC 8 (and as far as this proposal's author knows, also 9) is implemented by matching the last statement against some special cases involving the names `pure` and `return`, which can lead to some befuddlesome corner cases. For example, the following code is accepted by GHC 8 with `LANGUAGE ApplicativeDo`:

::

  incrA :: Applicative f => f Int -> f Int
  incrA nA = do
    n <- nA
    return (n + 1)

However, the η-expanded version of the same code is not accepted:

::

  incrA :: Applicative f => f Int -> f Int
  incrA nA = do
    n <- nA
    (\ x -> return x) (n + 1)

This infelicity is inexpressible with `ApplicativeComprehensions`, for an application of `pure` to the return value is implicit in the comprehension. `incrA` as a comprehension:

::

  incrA nA = [n + 1 | n <- nA]

Proposed Change Specification
-----------------------------

We introduce a new language extension `ApplicativeComprehensions`. It implies `MonadComprehensions`.

When `ApplicativeComprehensions` enabled, the compiler will try to desugar comprehensions as `Applicative` expressions. If it can not so desugar a particular comprehension, it would get a `Monad` constraint as usual.

In particular, when enabled, the compiler would desugar all comprehensions applicatively, but if a generator expression uses a term bound in a generator statement, that would be desugared as a monadic bind, as usual, and so need the `Monad` constraint.

Modifying, deleting, or usurping `ApplicativeDo` is not in scope of this proposal.

The typing rule is the same as type-checking the desugared code.

The desugaring rules are as for `ApplicativeDo`, except the last statement (the return value of the comprehension) has an implicit `pure`, so checking that is not needed.

Examples
--------

In the author's experience, one case where `ApplicativeComprehensions` would be very useful is command-line argument parsing, in particular with the "optparse-applicative" package. Thus one could write as follows (also with `NamedFieldPuns`):

::
Other than the extension flag, it touches 3 modules in the compiler: one each in the typechecker, the renamer, and `HsToCore`. Given that, the author estimates the upkeep costs should be fairly low.
  data Options = Options { foo :: Foo, bar :: Bar }

  optionsP :: Parser Options
  optionsP = [Options { foo, bar } | foo <- fooP, bar <- barP]

Effect and Interactions
-----------------------

The `ApplicativeComprehensions` extension would enable the user to write comprehensions for which mere `Applicative` constraints would be inferred, and avoids the problem of recognizing a `return` application.

Costs
-------------------

This extension has been implemented and tested (in a feature branch).

Other than the extension flag, it touches 3 modules in the compiler: one each in the typechecker, the renamer, and `HsToCore`. Given that, the author estimates he upkeep costs should be fairly low.

Drawbacks
-------------------

Some criticisms of `ApplicativeDo` also apply here. For example:

- It may not be obvious to some users why a particular comprehension gets a `Monad` constraint.
- The optimal desugaring algorithm is O(n³).
- Desugaring seems unpredictable to some users.

Alternatives
------------

The primary alternative is the status quo: either `Applicative` methods or `ApplicativeDo`.

Compared to `Applicative` methods, `ApplicativeComprehensions` may be preferable as it avoids the need to count arguments to figure out which binder corresponds to which expression.

Compared to `ApplicativeDo`, `ApplicativeComprehensions` may be preferable as it avoids the possibility of writing code which is not accepted for non-obvious reasons (particularly η-expansion, as explained earlier; it may also be a problem with unusual applicational operators, as `$` is now a special case).

Unresolved Questions
--------------------

We could desugar `ApplicativeComprehensions` which contain guards as expressions with an `Alternative` constraint. For example:

::

  [y | a₁ <- x₁, a₂ <- x₂, p]

as

::

  (\ a₁ a₂ () -> y) <$> x₁ <*> x₂ <*> guard p

but this would be restricted as before: the guard expression can not refer to any other variables bound in the comprehension. This seems less useful than `ApplicativeComprehensions` in general, but might be worth including.

Other Notes
--------------------

An `Applicative` comprehension can always be rewritten as an `ApplicativeDo` expression. In particular:

::

  [y | a₁ <- x₁, …]

can be rewritten as

::

  do a₁ <- x₁; …; pure y

Implementation Plan
-------------------

N/A, already implemented

Endorsements
-------------
