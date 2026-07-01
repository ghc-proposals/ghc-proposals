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

The Visible ForAll awesome feature was added to GHC as an extension with ... an initial pre-proposal syntax.

However, that syntax looks unappealing, and it feels unappealing; mixing it with other features makes it even less appealing.
::

  -- "naked" syntax for just 1 type variable
  id :: forall a -> a -> a
  
  -- mixing syntax with Classes
  show' :: forall a -> Show a => a -> String

  -- syntax with several tyvars
  toTuple :: forall a -> forall b. a -> b -> (a, b) 

Even more, the current syntax suggest to have 2 different (but specialized) universal quantifiers to represent just one "∀" quantifer.

And the current syntax forcefully mixes a context with the argument's types:
::

  --      ↓------- separate context from arg type
  baz :: forall a b. (Read a, Show b) => a -> b -> forall c -> Read c => c -> forall d -> Show d => d -> ((a, c), (b, d))
  --                     mix context with arg type ---↖-------------------------↗ 


Since the ``RequiredTypeArguments`` extension is an Experimental extension and the syntax is not widespread yet, it is not too late to make some changes to the syntax. 
It is better to make this syntax more optimal **as soon as possible**, before it gets "locked in" and gives backwards-maintainability headaches.

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

  baz :: forall a b c d. (Read a, Read c, Show b, Show d) => a -> b -> (type c) -> c -> (type d) -> d -> ((a, c), (b, d))

This Proposal also reduces the number of universal quantifers from 2 (``forall a.`` and ``forall a ->``) to just one ``forall a.``.

We could even make more interesting type arguments in the future:
::

  bar :: forall a b c. a -> b -> c -> (type (a, b, c) ) -> (a, b, c)
  -- usage:  bar 1 2 3 (type (Int, Integer, Float))
  -- compare bar 1 2 3 ::    (Int, Integer, Float)


Proposed Change Specification
-----------------------------

We add a temporary language expression ``NewVisibleForaAll`` which forbids the use of old syntax and allows the use of a new syntax with ``RequiredTypeArguments`` extension.

We allow writing ``type a`` in a type signature, which says that this term is a type.


Examples
--------

Sure, we could write the signature with implicit ``forall`` :
::

  idt :: (type a) -> a -> a
  idt (type a) x = x


We could also infer the visibility from the term: 
::

  -- foo :: (type a) -> a -> ...
  foo (type a) x = ...

Currently, the ``type`` keyword is required, however, the Roadmap of DT suggests making this keyword optional. 
If such a situation arises, we could still infer the function signature if we modify an argument slightly:
::

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
