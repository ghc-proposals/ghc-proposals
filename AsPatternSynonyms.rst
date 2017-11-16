.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/94>`_.

.. contents::

As patterns in pattern synonyms
==============

As-patterns and n+k patterns are currently disallowed in unidirectional pattern synonyms.  There is no good reason for this.
I propose that we lift the restriction.

Motivation
------------
Why lift the restriction on as-patterns?  (I regard n+k patterns, which are deprectaed anyway, as a side issue, but they should be treated
uniformly.)

* If the restriction is lifted, then *all* patterns become OK in unidirecitonal pattern synonyms. 
  Having no exceptions is a prize.  In fact the user manual already claims (falsely) that there are no such restrictions.

* A slab of code can simply be deleted from the compiler.

* THe semantics of matching does not change at all.

What's not to like?

Proposed Change Specification
-----------------------------
There is only one change:

* Allow as-patterns and n+k patterns in unidirectional pattern synonyms.

For example, this definition would become legal.

::

 patttern MP x y <- x@(Just y)

::

Currently this is rejected.  Why?  Because of worries about what this might mean (see Trac #9793) :

::

 f (MP (Just z) v) = e
 
::

With a "macro-espansion" model of pattern synonyms, that might be equivalent to

::

  f (Just z)@(Just v) = e
  
::

which is a jolly funny pattern.  But the semantics of pattern synonyms are NOT simply macro-expansion: see [the paper](https://www.microsoft.com/en-us/research/publication/pattern-synonyms/) Section 5.
Rather, their semantics is given thus:

* To match a pattern `(P p1 .. pn)`, where `P` is a pattern synonym defined by `P x1 ... xn <- p`, 
  match the value aginst `p` (binding x1..xn); and then match the `xi` against `pi`.
  
This description works perfectly for as-patterns. For example to match a value against `(MP (Just z) v)`,
first match the value against `x@(Just y)`, binding x and y; and then match `x` against `Just z` and `y` against `v`.

Side note: one could imagine extending the syntax of patterns, to include `pat1@pat2`, with matching semantics thus:

* To match a pattern `p1@p2` aagainst a value `v`, match `p1` against `v` (binding some variables `x1..xn`), the match `p2` against `v` (binding some variables `y1..ym`).  If both matches succeed, the overall match succeeds, binding `x1..xn,y1..ym`.

But I'm not actually proposing that change here.  End of side note.




Effect and Interactions
-----------------------
None that I can see.  It just lifts a restriction. 


Costs and Drawbacks
-------------------
Implementation is a matter of deleting code.

Alternatives
------------
Status quo.

Unresolved questions
--------------------
None that I can see

Implementation Plan
-------------------
I can implement it.
