Decidable Instances
===================

.. proposal-number:: 
.. trac-ticket::
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/114>`_.
.. sectnum::
.. contents::


Almost every real-world use case of ``MultiParamTypeClasses`` with ``FunctionalDependencies`` today requires the use of
``UndecidableInstances``, as the structural-recursion check used by the Haskell Report for single parameter
type classes is too naïve when copied over to the multi-parameter scenario. This proposal offers a middle-ground.

Motivation
------------

Consider

:: 

 class Monad m => MonadState s m | m -> s 


In this scenario, instances like

::

 instance MonadState s m => MonadState s (ReaderT e m)

require ``UndecidableInstances`` today, but are in theory perfectly decidable. The type ``s`` isn't being refined
by structural recursion, but the type ``ReaderT e m`` that determines it is. Today to compile this instance in the
``mtl`` we have to turn on the ``UndecidableInstances`` check, and risk accidentally writing instances that really can
send execution into a tail-spin, and then reason case by case through each instance to see whether or not the instances
in question are truly undecidable.

Proposed Change Specification
-----------------------------

The proposal is to add an extension ``DecidableInstances`` that serves as a middle ground between the existing rules
for structural recursion in instance handling and the free-forall offered by ``UndecidableInstances``.

Under ``DecidableInstances`` the decidability check is relaxed so that instead of requiring structural recursion on
every parameter of a multi-parameter type class individually, each parameter merely requires that it is functionally
dependent upon a set of properly structurally-recursive parameters.

To introduce some vocabulary, take ``<=`` to be the relation "is a subset of" then the set of all super-sets of a
particular set ``p`` is a `principal ideal <https://en.wikipedia.org/wiki/Ideal_(order_theory)#Basic_definitions>`_
``↑p = { x | p <= x }`` and the set of all subsets of a particular set ``q`` is the
`principal filter <https://en.wikipedia.org/wiki/Filter_(mathematics)#Filter_on_a_set>`_ ``↓q = { x | x <= q }``.

It will be convenient to group the parameters in a functional dependency. Since each functional dependency logically
implies all of the weaker functional dependencies obtained by taking a super-set of the inputs and/or sub-set of the
outputs, let's denote each functional dependency ``a b c .. -> d e f ..`` as ``↑p -> ↓q`` for the sets
``p = {a,b,c..}`` and ``q = {d,e,f..}``. Note: ``p`` and ``q`` are sets of parameters, and while ``↑p`` and ``↓q`` are
logically sets of sets, they are representable simply by storing their principal element ``p`` or ``q`` respective.

Given an instance, we can compute the set of class parameters for which the instance performs a simple structural
recursion. Let's call this set the set ``P`` of known-good structurally-recursive parameters.

Given a set ``P`` of known-good structurally-recursive parameters, and a functional dependency ``↑p -> ↓q``, if ``p``
is a subset of ``P``, then all parameters in the principal ``q`` should be added to the set of known-good
structurally-recursive parameters.

Moreover, each functional dependency can only successfuly "fire" once, after which it can be removed from consideration,
as adding to the set of knowns is idempotent.

Taking these facts together yields a straight-forward algorithm:

For each instance:

1. If ``UndecidableInstances`` is turned on accept the instance as usual.
2. Compute the set of structurally recursive parameters ``P``.
3. If the set of structurally recursive parameters contains all of the parameters to the class then accept the instance as usual.
4. If ``DecidableInstances`` is turned off reject the instance as usual.
5. Start with a list of functional dependencies for the class.
6. Try to fire each functional dependency to expand the set of known recursive parameters, removing it from the working list if it succeeds.
7. If no functional dependencies fired, reject the instance.
8. If ``P`` contains all parameters, accept the instance.
9. Go back to 6.

Steps 1-4 ensure that the existing behavior is unmodified.

Termination is ensured by induction as each iteration makes progress on removing from the list of functional
dependencies that serves as fuel or ends the loop.

Effect and Interactions
-----------------------
With ``DecidableInstances`` in place, libraries like the ``mtl`` would not have to risk non-termination in order to
compute the state or environment type for a given monad transformer stack.

Costs and Drawbacks
-------------------
The cost is that if you turn on the extension there is an actual check performed by GHC, where there was nothing before.
Using ``UndecidableInstances`` is free from GHC's perspective but places a cognitive burden on developers.

Development-wise, most of the pieces should already be present in the compiler, as we need to compute whether
or not a given parameter for a class is structurally recursive already. The additions are a new language
extension, a check for the extension and the loop implied by steps 5-9. The main operational cost is an extra
pair of nested loops for instances we would previously have used `UndecidableInstances` to allow. Mind you,
in the mind-boggling scenario where the performance burden is too high, users are of course free to stick with
the existing approach, and just appeal to ``UndecidableInstances``.

In addition, the criteria for describing when an instance is structurally recursive become a bit more
complicated to state.

Alternatives
------------
Alternatives for implementation include directly computing the transitive closure once and for-all in the class,
then simply checking if the set of goals is entailed by the set of known recursive-arguments.

Logically, each functional dependency ``↑p -> ↓q``, implies all of the weaker functional-dependencies you'd get
where each member of ``↑p`` entails each member of ``↓q``. Naïvely representing the expanded set of functional
dependencies directly and computing a reflexive transitive closure to get a partial ordering on subsets of variables, 
would take something like ``O(4^n)`` space, just to represent the relation, so this is cost-prohibitive.

However, a more practical middle ground could modify this approach to compute the transitive closure on the subsets of
our parameters by exploiting the same principal ideal/principal filter relationship for each functional dependency we use
in the algorithm presented as the specification. The composition of the two principal relations: ``↑p -> ↓q`` and ``↑r -> ↓s``
yields a extra rule ``↑p -> ↓s`` whenever ``r <= q``. Given this encoding, performing an initial computation of the transitive
closure could be considerably more expensive, but the check for entailment now no longer has to iterate to a fixed point for
each instance, it simply has to loop once over a larger set of rules and check that all parameters to the class are included.

The downside of this is that we'd have to store the transitive closure somewhere, or recompute it, and if we recompute it, we
lose asymptotically relative to the presented algorithm.

As an alternative to adding a new extension, one option would be to simply extend the semantics of ``FunctionalDependencies`` to
just work this way going forward. This seems like a slightly riskier plan in that if there turned out to be a major bug in the
specification, this could blow up an entire compiler release, but would reduce the number of moving parts in the
proposal, and ensure that it gets tested quite thoroughly.

A more drastic alternative is to stop using functional dependencies. However, this hasn't proven popular with users where
libraries like the ``mtl`` are concerned as the type family solutions are more verbose to use.

Unresolved questions
--------------------
I'm unaware of any.

Implementation Plan
-------------------
Up for grabs.
