Proposal title
==============

.. proposal-number::
.. trac-ticket::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

The assumption that operator precedence is a total order, with only 10 level is too strong.

By removing the assumption, library user will get less binding that suprise them,
and library writer need not worry about selecting the best fixity, as it could be added without breaking backward compatibility.


Motivation
------------
This proposal is motivated by 'https://www.reddit.com/r/haskell/comments/27ifst/how_do_you_choose_the_fixity_of_operators_when/'.

In the reddit post, the top comment is a redditor arguing for a partial order.

The second top comment is another redditor suggesting to use :info on operators that is related, and choose the precedence accordingly.

In effect, what the second top comment is suggesting, is that we declare operator precedence by solving a partial ordering constraint.

So why not declare the constraint directly instead of going through extra step?

Proposed Change Specification
-----------------------------
Instead of representing precedence as [0, 10), 
we will represent them as a node on the operator-precedence-graph, along with associativity attribute.

It is a directd graph with edge a -> b meaning a bind tighter then b. Note that we do not assume transitivity.

We will add a language extension WeakPrecedence.

With the language extension, infix operator will not be assigned a default fixity.

Instead, one can specify five constraint of a operator, related to other operators(that are import).
a == b: they are at the same precedence. put them in the same node.
a < b: draw an edge from a -> b.
a > b: same as b < a.
infixl a: a is left associative.
infixr a: a is right associative.

one of a or b need to be defined at the current module (no orphan), and both a b need to be present at the definition,
as haskell bind operator precedence to enitty, not name.

Ambiguity operator precedence graph is allowed. Only when we parse a concrete syntax, we need to make sure that it is unambiguous.

If it is indeed ambiguous, an error is required, possibly with what different parse tree can arise.

Effect and Interactions
-----------------------
It will effect with the existence level-based operators.

We can be fully backward compatible, by regarding level [0, 10) as predefined nodes on the precedence graph,
in which 0 -> 1, 0 -> 2, 0 -> 3, 1 -> 2, 1 -> 3, 2 -> 3, ...

Costs and Drawbacks
-------------------
There need to be a little modification on shaunting yard algorithm to make it work.

It does not effect the learnability for novice users. If any it should increase it, as there is less precedence to remember and trip up.

(Of course, since Preclude already use the default level-based operators, it will only effect novice learning using non-preclude material, which is rare).


Alternatives
------------
There is a lot of work on custom operator, including 'Reasonably Programmable Syntax' and 'Parsing Mixfix Operators'.

We can also step one place further, and impose that they form a partial order (assuming transitivity).

However, I think that we should move in small step, each time making as little assumption as possible, 
so we do not risk implementing a big feature as in 'Reasonably ...' and getting fixated on the design choice it make.
It also take considerably more time to implement a large feature.

List existing alternatives to your proposed change as they currently exist and discuss why they are insufficient.


Unresolved questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and specification. Be upfront and trust that the community will help. Please do not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to the steering committee.


Implementation Plan
-------------------
