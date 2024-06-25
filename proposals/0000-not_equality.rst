=================
Not Type Equality
=================

.. author:: Viktor WW
.. date-accepted::
.. ticket-url:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/659>`_.
.. sectnum::
.. contents::

This proposal introduces Not-Equality into GHC

.. _`Type-inequality operator`: https://chrisdone.com/posts/type-inequality-operator/


Motivation
----------

Not Type Equality is flexible tool for Equality constraints. It could help to avoid in many cases Overlapping instances.

It is already possible to emulate such behavior.

Yes, it is more verbose then this Proposal changes and yes, error messages are less clearer then errors from type equality errors.

Chris Done in his blog wrote an article `Type-inequality operator`_ . In brief it says next:

Type-inequality operator
~~~~~~~~~~~~~~~~~~~~~~~~

Here’s a fun trick. If you want to define a function that accepts everything but one type, you can make a type family like this.
::

  type family a /~ b where
    a /~ a = False
    _ /~ _ = True

We can use it like this
::

  foo :: (Integral i, i /~ Word ~ True) => i -> ()
  foo = undefined

The argument can be any type ``i`` that doesn’t unify with ``Word`` . So this type-checks
::

  bar = foo (undefined ::  Int)

But this doesn’t
::

  bad = foo (undefined :: Word)
  -- Error: Couldn't match type ‘'False’ with ‘'True’.


Alternative Type-inequality operator
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Another example (credit to @s-and-witch) with ``AllowAmbiguousTypes``
::

  type a /~ b = (a == b) ~ False

  typesNotEqual :: (a /~ b) => ()
  typesNotEqual = ()


The argument can be any types that don’t match. So this type-checks
::

  bar = typesNotEqual @Bool @Int

But this doesn’t
::

  bad = typesNotEqual @Bool @Bool
  -- Error: Couldn't match type ‘'False’ with ‘'True’.


Proposed Change Specification
-----------------------------

Constraints in GHC can only be:

- Implicit parameter constraints

- Class constraints

- Equality constraints

This proposal extends Equality constraints

Introduce a new extension ``-XNotEquality`` .


We propose additional constraint not-"operators" ``(/~)`` , ``(/~~)`` , ``(/~#)``  which are opposite of ``(~)`` , ``(~~)`` , ``(~#)``.
:: 

  -- Pseudo-haskell rule
  a /~ b   ===  not. a ~ b
  a /~~ b  ===  not. a ~~ b
  a /~# b  ===  not. a ~# b

Partly Theta overlapping checking
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"Guess-free" means that the constraint solver doesn't make guesses about what instance should be picked in the given context. 

This abstract rule manifests itself in a concrete way: during constraint solving, GHC looks at instance head only.
::

     --                        instance head
     --                      |----------------|
  instance (C a b, D f g) => H Int b c d a Bool ...
     --    |-----------|
     --    theta/instance context


"Guess-free" must become more complicated:

1. All instances, which contains not-equality in Theta we mark internally as "consists inequality" instance.

2. If instance head fits the guess and it is marked as "consists inequality", then we additionally check inequality part of Theta.

Examples
--------

Reduce overlapping
~~~~~~~~~~~~~~~~~~

We could easy reduce overlapping in many cases
::

  -- we wish to have
  instance C Int  b where ..  -- (A)
  instance C a Bool where ..  -- (B) Error: Overlapped

  -- With Not-Equality, NEW!
  -- OR
  instance forall b. b /~ Bool => C Int  b where ..  -- (A)
  instance                        C a Bool where ..  -- (B) including C Int Bool

  -- OR
  instance                        C Int  b where ..  -- (A) including C Int Bool
  instance forall a. a /~ Int  => C a Bool where ..  -- (B)

  -- OR
  instance forall b. b /~ Bool => C Int  b where ..  -- (A)
  instance forall a. a /~ Int  => C a Bool where ..  -- (B)
  instance C Int Bool where ..                       -- (C) isolated C Int Bool



Effect and Interactions
-----------------------

UnicodeSyntax
~~~~~~~~~~~~~

``≁`` (≁, Not Tilde, U+2241) is added to ``UnicodeSyntax`` as synonym for ``/~`` operator.

Quantified Constraints
~~~~~~~~~~~~~~~~~~~~~~

Let we have ``class MyShow`` :
::

  class MyShow a where
    myshow :: a -> String

  instance MyShow String where
    myshow = id

   -- Error: Self-Overlapping
   instance MyShow a => MyShow [a] where
      myshow xs = concatMap myshow xs

We could write condition more accurate, using ``QuantifiedConstraints`` :
::
  
  -- QuantifiedConstraints
  instance (MyShow a, forall b. a /~ [b]) => MyShow [a] where
    myshow xs = concatMap myshow xs

Is GHC is smart enough to check this?


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of ``NotEquality`` extension has minimum difficulty.


Backward Compatibility
----------------------

This proposal is backward compatible, except type operator's names ``(/~)`` , ``(/~~)`` , ``(/~#)``.


Alternatives
------------

Main alternatives are ``Type Families`` (including Associated type families) extension and deprecated ``OverlappingInstances`` extension.


Prior art
---------

In Rust language similar feature "Negative Equality" is highly requested ``<T: !U>`` , but is not yet approved. 


Unresolved Questions
--------------------

None yet.


Implementation Plan
-------------------

It is unclear.
