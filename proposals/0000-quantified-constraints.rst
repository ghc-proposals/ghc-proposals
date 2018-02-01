.. proposal-number::

.. trac-ticket::

.. implemented::

.. highlight:: haskell

..
   This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_. **After creating the pull request, edit this file again, update the number in the link, and delete this bold sentence.**

.. contents::

Quantified Constraints
======================

This proposal introduces quantified constraints, which have been proposed years ago
to raise the expressive power of type classes to essentially first-order logic.
These quantified class constraints enable instance declarations that are currently
very difficult or even impossible to express in Haskell.


Motivation
------------
Introducing quantified constraints offers two main benefits:

- Firstly, they enable terminating resolution where this was not possible before.
Consider for instance the following instance declaration for the general rose datatype::

 data Rose f x = Rose x (f (Rose f x))
    
 instance (Eq a, forall b. Eq b => Eq (f b)) => Eq (Rose f a) where
   (Rose x1 rs1) == (Rose x2 rs2) = x1 == x2 && rs1 == rs2

This extension allows to write constraints of the form ``forall b. Eq b => Eq (f b)``,
which is needed to solve the ``Eq (f (Rose f x))`` constraint arising from the
second usage of the ``(==)`` method.

- Secondly, quantified constraints allow for more concise and precise specifications.
As an example, consider the MTL type class for monad transformers::

 class Trans t where
   lift :: Monad m => m a -> (t m) a

The developer knows that a monad transformer takes a monad ``m`` into a new monad ``t m``.
But this is property is not formally specified in the above declaration.
This omission becomes an issue when defining monad transformer composition::

 newtype (t1 * t2) m a = C { runC :: t1 (t2 m) a }

 instance (Trans t1, Trans t2) => Trans (t1 * t2) where
   lift = C . lift . lift

The goal here is to ``lift`` from monad ``m`` to ``t2 m`` and
then ``lift`` this again into ``t1 (t2 m)``.
However, this second ``lift`` can only be accepted when ``(t2 m)`` is a monad
and there is no way of establishing that this fact universally holds.

Quantified constraints enable this property to be made explicit in the ``Trans``
class declaration::

 class (forall m. Monad m => Monad (t m)) => Trans t where
   lift :: Monad m => m a -> (t m) a

More motivating examples can be found in this paper :
`<https://dl.acm.org/citation.cfm?id=3122967>`_


Proposed Change Specification
-----------------------------
We propose to add a new GHC extension called ``{-# QuantifiedConstraints #-}``.
Currently, GHC allows only simple class constraints in class and instance contexts.
When this extension is enabled, constraints can contain type quantifiers and
implications in arbitrarily nested positions.

As an example, consider the declaration mentioned above, containing a quantified constraint::

 instance (Eq a, forall b. Eq b => Eq (f b)) => Eq (Rose f a) where
   (Rose x1 rs1) == (Rose x2 rs2) = x1 == x2 && rs1 == rs2


Costs and Drawbacks
-------------------
There are currently no known drawbacks to this feature.


Alternatives
------------
Several alternatives have already been considered.

GHC currently supports a form a cycle-aware resolution,
which enables writing the rose example mentioned above, without quantified constraints.
Unfortunately, this approach is not generally applicable since the
resolution process can diverge without cycling,
rendering the cycle-aware resolution useless in these scenarios.

Secondly, alternative encodings exist, such as the one presented in this paper:
`<https://dl.acm.org/citation.cfm?id=871906>`_
Unfortunately, they all render the code significantly longer, more complex
and none of these alternative encodings are generally applicable.


Unresolved questions
--------------------


Implementation Plan
-------------------
`<https://phabricator.haskell.org/D4353>`_


Additional Links
----------------
- `<https://ghc.haskell.org/trac/ghc/wiki/QuantifiedConstraints>`_
- `<https://ghc.haskell.org/trac/ghc/ticket/2893>`_
