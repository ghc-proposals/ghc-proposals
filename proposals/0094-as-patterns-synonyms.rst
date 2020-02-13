As patterns in pattern synonyms
==============

.. author:: Simon Peyton Jones
.. date-accepted:: 2018-03-18
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: 8.6
.. highlight:: haskell
.. header :: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/94>`_ and implemented in commit `411a97e2 <https://github.com/ghc/ghc/commit/411a97e2c0083529b4259d0cad8f453bae110dee>`_.
.. contents::

As-patterns (and n+k patterns) are currently disallowed in unidirectional pattern synonyms.  There is no good reason for this.
I propose that we lift the restriction.

Motivation
------------
Why lift the restriction on as-patterns?

* If the restriction is lifted, then *all* patterns become OK in unidirecitonal pattern synonyms.
  Having no exceptions makes our users' lives easier: it is a real prize.  In fact the user manual already claims (falsely) that there are no such restrictions.

* A slab of code can simply be deleted from the compiler.

* The semantics of matching does not change at all.

What's not to like?

I regard n+k patterns, which are deprecated anyway, as a side issue, but they should be treated
uniformly.

For bidirectional pattern synonyms, there are already many restrictions on what patterns you can write, and rightly so because a bidirectional pattern synonym must be used both to pattern match and to construct values.  I do not propose any change in the rules for bidirectional pattern synonyms.

Proposed Change Specification
-----------------------------
There is only one change:

* Allow as-patterns and n+k patterns in unidirectional pattern synonyms.

For example, this definition would become legal.

::

 patttern MP x y <- x@(Just y)

::

Currently this is rejected.  Why?  Because of worries about what this might mean (see #9793) :

::

 f (MP (Just z) v) = e

::

With a "macro-expansion" model of pattern synonyms, that might be equivalent to

::

  f (Just z)@(Just v) = e

::

which is a jolly funny pattern.  But the semantics of pattern synonyms are NOT simply macro-expansion: see `the paper (Section 5) <https://www.microsoft.com/en-us/research/publication/pattern-synonyms/>`_.
Rather, their semantics is given thus:

* To match a pattern ``(P p1 .. pn)``, where ``P`` is a pattern synonym defined by ``P x1 ... xn <- p``,
  match the value aginst ``p`` (binding x1..xn); and then match the ``xi`` against ``pi``.

This description works perfectly for as-patterns. For example to match a value against ``(MP (Just z) v)``,
first match the value against ``x@(Just y)``, binding x and y; and then match ``x`` against ``Just z`` and ``y`` against ``v``.




Effect and Interactions
-----------------------
None that I can see.  It just lifts a restriction.

Note that, just as it is possible to write a view pattern that never matches, so it is
possible to write a pattern synonym that never matches using an as-pattern.  For example

::

   patttern MP x y <- x@(Just y)

   f (MP Nothing v) = ...

::

According to the rules, we first match the argument ``v`` against the RHS of the pattern synonym ``x@(Just y)``. Maybe that fails; if so the match fails. Maybe it succeeds, binding ``x`` to ``Just v2`` and ``y`` to ``v2``. Now match the value of ``x`` (namely ``Just v2``) against ``Nothing``. That fails, so the overall match fails. So the rules say that this pattern will never match.

There is nothing wrong with this; it is possible now, and it remains possible.  (GADT patterns can also be guaranteed to fail.)

Costs and Drawbacks
-------------------
Implementation is a matter of deleting code.

Alternatives
------------
One could imagine extending the syntax of patterns, to include ``pat1@pat2``, with matching semantics thus:

* To match a pattern ``p1@p2`` aagainst a value ``v``, match ``p1`` against ``v`` (binding some variables ``x1..xn``), the match ``p2`` against ``v`` (binding some variables ``y1..ym``).  If both matches succeed, the overall match succeeds, binding ``x1..xn,y1..ym``.

That would make a lot of sense: ``p1@p2`` would be an and-pattern, dual to the proposed or-patternns.  I'm not actually proposing that change here; it would be a very sensible follow-on.  But it the committee prefers, it could even be accepted right away.

Indeed, via a pattern synonym you can get an and-pattern

::

     pattern And x y <- x@y

::

Now, according to the rules, ``And p1 p2`` will match only if both ``p1`` and ``p2`` match.


Unresolved questions
--------------------
None that I can see

Implementation Plan
-------------------
I can implement it.
