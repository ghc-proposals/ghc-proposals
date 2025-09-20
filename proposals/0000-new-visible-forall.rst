New syntax for Visible ForAll
=============================

.. author:: Viktor WW
.. date-accepted::
.. ticket-url:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/729>`_.
.. sectnum::
.. contents::


This proposal introduces a syntax for visible forall type variables.

Motivation
----------

The Visible ForAll awesome feature was added to GHC as an extension with ... a temporary syntax.

However, that syntax looks unappealing, and it feels unappealing; mixing it with other features makes it even less appealing.
::

  -- "naked" syntax for just 1 type variable
  id :: forall a -> a -> a
  
  -- mixing syntax with Classes
  show' :: forall a -> Show a => a -> String

  -- syntax with several tyvars
  toTuple :: forall a -> forall b. a -> b -> (a, b) 

And it is better to change the syntax as soon as possible, before the old syntax becomes Legacy.

This Proposal suggests adding a new syntax for visible type variables and deprecating the use of the old one.
::

  -- current syntax
  idt :: forall a -> a -> a
  idt (type a) x = x
  
  show' :: forall a -> Show a => a -> String
  
  toTuple :: forall a -> forall b. a -> b -> (a, b) 

  -- new syntax
  idt :: forall a. (type a) -> a -> a
  idt (type a) x = x
  
  id' :: (type a) -> a -> a
  
  show' :: forall a. Show a => (type a) -> a -> String
  
  read' :: Read a => (type a) -> String -> a
  
  toTuple :: forall a b. (type a) -> a -> b -> (a, b)


Proposed Change Specification
-----------------------------

We add a temporary language expression ``NewVisibleForaAll`` which forbids the use of old syntax and allows the use of a new syntax with ``RequiredTypeArguments`` extension.

We allow writing ``type a`` in a type signature, which says that this term is a type.


Examples
--------

Sure, we could omit the ``forall`` keyword sometimes :
::

  idt :: (type a) -> a -> a
  idt (type a) x = x


We could also infer the visibility from the term: 
::

  -- foo :: (type a) -> a -> ...
  foo (type a) x = ...

  -- bar :: (type a) -> a -> ...
  bar (a :: type b) x = ...
 
  
Effect and Interactions
-----------------------

None is known, except the ``RequiredTypeArguments`` extension.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs to be less than average difficulty.


Backward Compatibility
----------------------

This proposal is fully backward compatible.


Alternatives
------------

The main alternative is "status quo" - to remain as it is.


Unresolved Questions
--------------------

None at this time.


Implementation Plan
-------------------

It is unclear.

Endorsements
-------------

Acknowledgments
---------------
