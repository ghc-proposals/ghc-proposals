Case bind
==============

.. author:: John Ericson @Ericson2314
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

Provide a more concise why to handle failure patterns in do notation.

Motivation
----------

Let's say I need to manually handle some failures in do notation by pattern matching.
"Manually", because the ``fail`` desugar doesn't do what I want,
and "by pattern matching" because I am not catching an exception or anything like that.
There are roughly to ways to write this.

The first way is like this::

  do
    res0 <- action0
    case res0 of
      Good0... -> do
        res1 <- action1
        case res1 of
          Good1... ->
            ...
          Bad1_0... -> ...
          Bad1_1... -> ...
      Bad0_0... -> ...
      Bad0_1... -> ...

Notice how the indentation grows at every failure, and also the earlier good and bad patterns are quite far apart.
The latter problem can sometime be fixed if the bad pattern doesn't have too many wildcards can can come first.
The former problem however is unsolvable in this style.

The second way is like this::

  do
    res0 <- action0
    TupleVars0 <- case res0 of
      Good0... -> pure TupleVars0
      Bad0_0... -> ...
      Bad0_1... -> ...
    res1 <- action1
    TupleVars1 <- case res1 of
      Good1... -> pure TupleVars1
      Bad1_1... -> ...
      Bad1_1... -> ...
    ...

This solves the rightward drift and distant alternatives problems, but at the cost of making the user manually construct destruct tuples of all the variables they bound in the "good case".
This is tedious, and makes the code harder to modify.

Clearly, both styles have drawbacks.

Proposed Change Specification
-----------------------------

We introduce a new sugar in ``do``\ -notation, using the Haskell Report 2010's non-terminals::

  stmt → case pat <- exp of { alts }

where ``of`` is, as usual, a layout herald.

The existing ``do``\ -notation desugar is augmented to handled this as follows::

  do { case p <- e of { alts }; stmts } =
    e >>= \case { p -> do { stmts }; alts }

Examples
--------

We can rewrite the motivation's example as::

  do
    case Good0... <- action0 of
      Bad0_0... -> ...
      Bad0_1... -> ...
    case Good1... <- action1 of
      Bad1_0... -> ...
      Bad1_1... -> ...
    ...

This also gets rid of the ``res0`` and ``res1``.
I did not complain about those in the motivation as ``action0 >>= \case`` also gets rid of them does that today.

Effect and Interactions
-----------------------

- Exhaustiveness checking is very easy.
  Unlike the current ``fail`` desugar, errors handling doesn't rely on any intentional incomplete patterns.

- Recursive ``do``\ -notation makes the second option with the tuple harder still to use, as one cannot reuse the same variable names.

Costs and Drawbacks
-------------------

This is more syntactic sugar, which is rightfully deemed an indulgence.
I find no joy in proposing sugar.
But, when weighed together with `Proposal 319`_, we'd be deprecating one sugar for another, which seems more morally neutral.

Alternatives
------------

- Syntax variations such as ``pat case <- expr of``

- Status quo

- Some way of marking patterns as intending to be fallible / infallible for sake of the problems brought up in `Proposal 319`_.
  That does however leave the problems in this proposal unaddressed.

Unresolved Questions
--------------------

None at this time.


Implementation Plan
-------------------

I suppose I could do it.

Endorsements
-------------

.. _`Proposal 319`: https://github.com/ghc-proposals/ghc-proposals/pull/319
