.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/80>`_.

.. contents::

Type-level type applications
============================

Allow the use of type applications at the type level. For example,
we could write::

  'Just @Nat

instead of::

  'Just :: Nat -> Maybe Nat


Motivation
------------
There are two major motivations:

1. To allow users to get the power and convenience of explicit type
   applications at the type level as well as the term level.

2. To allow ``-fprint-explicit-kinds`` and the ``Show`` instance for
   ``TypeRep`` to produce more readable output. Currently,
   ``show (typeRep @('Just 3))`` produces ``"'Just Nat 3"``, making
   no distinction between levels. With the proposed change, we could
   quite legitimately produce ``"'Just @Nat 3"``, which seems much clearer.


Proposed Change Specification
-----------------------------
Allow visible type application in types as well as terms. In precisely
the same way that it currently is used to reduce ``forall``s in terms,
it will reduce ``forall``s in types.


Effect and Interactions
-----------------------
I am not aware of any substantial interactions.


Costs and Drawbacks
-------------------
I have no estimate of development or maintenance costs. As a user, I was
quite surprised to find that this didn't work already, so I don't think
the learning cost will be high.


Alternatives
------------
I am not aware of any existing alternatives.


Unresolved questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and specification. Be upfront and trust that the community will help. Please do not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to the steering committee.



Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other ressources and prerequisites are required for implementation?
