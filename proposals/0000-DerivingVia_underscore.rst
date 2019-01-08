`DerivingVia` underscore
==============

.. proposal-number:: 
.. trac-ticket:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/192>`_.
.. sectnum::
.. contents::

This is an idea I haven't fleshed out, but I have a feeling there is a sensible way to use underscores `_` to avoid repeating yourself in `via` types.


Motivation
------------

Is there a way to write

::

 newtype X a = X a

 instance Show (X a) where
  show :: X a -> String
  show _ = "<<..>>"

 data Boolean = F | T
  deriving
   Show
  via
   (Wrap _)

so that its meaning is `via (Wrap Boolean)`? Similarly (`that's where the idea started <http://www.haskell.org/>`_)

::

 newtype Suave :: Type -> Type where
  Suave :: StateT Int STM (Input a) -> Suave a

  deriving
    (Functor, Applicative)
  via
    (_ `Compose` _) -- (StateT Int STM `Compose` Input)

Proposed Change Specification
-----------------------------

Specify the change in precise, comprehensive yet concise language. Avoid words like should or could. Strive for a complete definition. Your specification may include,

* grammar and semantics of any new syntactic constructs
* the types and semantics of any new library interfaces
* how the proposed change interacts with existing language or compiler features, in case that is otherwise ambiguous

Note, however, that this section need not describe details of the implementation of the feature. The proposal is merely supposed to give a conceptual specification of the new feature and its behavior.


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
