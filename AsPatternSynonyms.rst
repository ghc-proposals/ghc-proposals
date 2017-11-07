.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_. **After creating the pull request, edit this file again, update the number in the link, and delete this bold sentence.**

.. contents::

Notes on reStructuredText - delete this section before submitting
==================================================================

The proposals are submitted in reStructuredText format.  To get inline code, enclose text in double backticks, ``like this``.  To get block code, use a double colon and indent by at least one space

::

 like this
 and

 this too

To get hyperlinks, use backticks, angle brackets, and an underscore `like this <http://www.haskell.org/>`_.   


As patterns in pattern synonyms
==============

As-patterns and n+k patterns are currently disallowed in unidirectional in pattern synonyms.  There is no good reason for this.
I propose that we lift the restriction.

Motivation
------------
Why lift the restriction on as-patterns?  (I regard n+k patterns, which are deprectaed anyway, as a side issue, but they should be treated
uniformly.

* If the restriction is lifted, then *all* patterns become OK in unidirecitonal pattern synonyms. 
  Having no exceptions is a prize.  In fact the user manual claims (falsely) that there are no such restrictions.

* A slab of code can simply be deleted from the compiler.

* THe semantics of matching does not change at all.

What's not to like?

Proposed Change Specification
-----------------------------
Allow as-patterns and n+k pattenns in unidirectional pattern synonyms.  For exmaple

::

 patttern MP x y = x@(Just y)

::

Currently this is rejected.  Why?  Because (see Trac #9793) of worries about what this might mean:

::

 f (MP (Just z) v) = e
 
::

With a "macro-espansion" model of pattern synonyms, that might be equivalent to

::

  f (Just z)@(Just v) = e
  
::

which is a jolly funny pattern.  But the semantics of pattern synonyms are NOT simply macro-expansion: see the paper Section 5.
Rather, their semantics is given thus:

* To match a pattern `(P p1 .. pn)`, where `P` is a pattern synonym defined by `P x1 ... xn <- p`, 
  match the value aginst `p` (binding x1..xn); and then match the xi against pi.
  
This description works perfectly for as-patterns. For example to match a value against `(MP (Just z) v)`,
first match the value against `x@Just y)`, binding x and y; and then match `x` against `Just z` and `y` against `v`.

In effect "as-patterns" are interpreted as "and-patterns".



Effect and Interactions
-----------------------
Detail how the proposed change addresses the original problem raised in the motivation.

Discuss possibly contentious interactions with existing language or compiler features. 


Costs and Drawbacks
-------------------
Give an estimate on development and maintenance costs. List how this effects learnability of the language for novice users. Define and list any remaining drawbacks that cannot be resolved.


Alternatives
------------
List existing alternatives to your proposed change as they currently exist and discuss why they are insufficient.


Unresolved questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and specification. Be upfront and trust that the community will help. Please do not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to the steering committee.


Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other ressources and prerequisites are required for implementation?
