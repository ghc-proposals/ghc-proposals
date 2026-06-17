``infixr ::`` 
==============

.. proposal-number:: 
.. trac-ticket:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/197>`_.
.. sectnum::
.. contents::

I apologize for the proposal name ``infixr`` colons but that's what we get. Code like

::

 'a' :: Char :: Type

fails with a parse error on ``::``. I suppose I am proposing making `::` act right-associative, despite not being an operator.

Why? Well, I would like to show that ``Type -> Type`` and ``Int`` are both ``Type``, and I think it would look snappier if you could write

::

  Maybe :: Type -> Type
  Maybe :: Type -> Type :: Type
  Maybe :: Type -> Type :: Type :: Type

and not


::

  Maybe :: Type -> Type
  Maybe :: (Type -> Type :: Type)
  Maybe :: (Type -> Type :: (Type :: Type))

I suppose that's all. Similarly

::

  '() :: () :: Type :: Type

   () :: () :: Type :: Type


Open to all comments and criticisms.

Motivation
------------
Give a strong reason for why the community needs this change. Describe the use case as clearly as possible and give an example. Explain how the status quo is insufficient or not ideal.


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
