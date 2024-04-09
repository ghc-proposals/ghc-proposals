===============
Not Constraints
===============

.. author:: Viktor WW
.. date-accepted::
.. ticket-url:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/648>`_.
.. sectnum::
.. contents::

This proposal introduces Not-Constraints into GHC

.. _`#631`: https://github.com/ghc-proposals/ghc-proposals/pull/631


Motivation
----------

``Type Families`` (including Associated Types) extension helps to write a lot of non-overlapping instances (and derivings).

But it is over-verbose way, often over-complicated, and sometimes it is not flexible enough to write some instances.

Deprecated ``OverlappingInstances`` and ``IncoherentInstances`` extensions have more flexible tools, but they has own weakness.
::

  ---	Class constraints
  instance (Ord a) => Ord (Maybe a)  where ...
	
  -- we also wish to have
  instance (not. Ord a) => Ord (Maybe a) where

This Proposal is a way to write non-overlapping instances (and derivings) as simple as possible with huge flexibility.


Proposed Change Specification
-----------------------------

Constraints in GHC can only be:

- Implicit parameter constraints

- Class constraints

- Equality constraints

Syntax
~~~~~~

Introduce a new extension ``-XNotConstraints`` .

Syntax of ``not`` pseudo-quantifier has 1 simple form.
::

  not. 

It says that this Constraint is Not-Constraint.


Implicit parameter Not-Constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This Proposal ignore to add Implicit parameter Not-Constraints.

Even there is possible to have Implicit parameter Not-Constraints, there are no benefits to have them.

If any useful benefits would be found, then Not-Constraints could be added later in future.


Class Not-Constraints
~~~~~~~~~~~~~~~~~~~~~

In Haskell there is no extra syntax for ``not`` functions, like "!" in some languages. Even more, "!" is highly used by bang-patterns.

So we propose to add specific pseudo-quantifier to constraints.

Most promised candidate is ``not.`` pseudo-quantifier. Second one is ``fornot.`` pseudo-quantifier.

Since it is used in constraints, it is enough to be pseudo-keyword.

::

	instance (Ord a) => Ord (Maybe a) where
	
	-- NEW! Non-Overlapping instance
	instance (not. Ord a) => Ord (Maybe a) where


Not-Constraints works as filters only, they do not require to use any of Class-depended function. Just, like in example:

::

  idNum :: forall a. Num a => a -> a
  idNum x = x

  -- NEW!
  idNotNum :: forall a. not. Num a => a -> a
  idNotNum x = x


Equality Not-Constraints
~~~~~~~~~~~~~~~~~~~~~~~~

Equality Constraints require casting to another type, but Equality Not-Constraints **do not cast anything**, they works as filters only!

Not-function and depended from it another functions are so highly used in other languages and Haskell, so we propose additional constraint not-"operators" ``(/~)`` , ``(/~~)`` , ``(/~#)`` .
:: 

  -- Not a mandatory, but it is expected
  a /~ b   ===  not. a ~ b
  a /~~ b  ===  not. a ~~ b
  a /~# b  ===  not. a ~# b

  -- Not a mandatory, but with least surprise we expect
  not. a /~ b   ===  a ~ b
  not. a /~~ b  ===  a ~~ b
  not. a /~# b  ===  a ~# b

With Equality Not-Constraints we could easily create Non-Overlapping instances:
::

  -- we wish to have
  instance C Int  b where ..  -- (A)
  instance C a Bool where ..  -- (B) Error: Overlapped

  -- With Equality Not-Constraints, NEW!
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


Late Instances and Derivings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Not-Constraints are so powerful, that could fix Instances and Derivings too early.

As example we take `#631`_ Proposal, which propose to add exit code by main return type. And one of possible mechanisms is to have ``ExitStatus`` class

::

  class ExitStatus e where
    toExitCode :: e -> ExitCode

  instance ExitStatus ExitCode where
    toExitCode = id

  instance ExitStatus () where
    toExitCode = const ExitSuccess

  instance ExitStatus Void where
    toExitCode = absurd

It is Ok, if signature of ``main`` function is either ``IO ()`` or ``IO Void`` or ``IO ExitCode`` . But what to do, if it has a signature ``main :: IO a`` ?

With this proposal it is easy:
::

  instance (not. ExitStatus a) => ExitStatus a where
    toExitCode = const ExitSuccess

But if we have this instance in library/export module, we are blocked to add a new instance into scope 
::

  import GHC.IO.Exit
  
  -- Error: Overlap
  instance ExitStatus Int where ... 

So, we wish to have Late Instances and Derivings.

We propose to use keyword ``then`` (or some new one pseudo-keyword like ``late`` ) before a instance-keyword or deriving-keyword
::

  then instance (not. ExitStatus a) => ExitStatus a where
    toExitCode = const ExitSuccess

It requires to solve this instance as late as possible: after solving all "early" ones. 

In future this feature could be extended to ``then then instance`` if it is needed, which is solved after all then-instances.

Recursive Not-Constraints
~~~~~~~~~~~~~~~~~~~~~~~~~

Not-Constraints are so powerful, that could create recursive dependences.

Let we have:
::

  class MyShow a where
    myshow :: a -> String

  instance MyShow String where
    myshow = id

  -- we wish to have
  instance MyShow a => MyShow [a] where -- Error: Overlapped

With Not-Constraints we could write same instances, but they do not overlap and means different:
::

	-- self-recursive: allow [a], including [String]  
	instance (MyShow a, not. MyShow [a]) => MyShow [a] where
       myshow xs = concatMap myshow xs

	-- self-recursive: allow [[a]], including [[String]] 
	instance (MyShow a, not. MyShow [a]) => MyShow [a] where
       myshow xs = concatMap myshow xs

	-- self-recursive: allow [[[a]]] 
	instance (MyShow a, not. MyShow [a]) => MyShow [a] where
       myshow xs = concatMap myshow xs

	-- self-recursive: allow [[[[a]]]] 
	instance (MyShow a, not. MyShow [a]) => MyShow [a] where
       myshow xs = concatMap myshow xs

	-- self-recursive: allow [[[[[a]]]]]
	instance (MyShow a, not. MyShow [a]) => MyShow [a] where
       myshow xs = concatMap myshow xs

Is GHC is smart enough to understand this? If No, could we help to compiler with mark ``instance rec ...`` or similar?
::

	-- self-recursive: allow [a], including [String] 
    -- and [[a]], [[[a]]], [[[[a]]]], [[[[[a]]]]]
	-- rec 5
	instance rec 5 (MyShow a, not. MyShow [a]) => MyShow [a] where
       myshow xs = concatMap myshow xs

This is unresolved question.


Examples
--------

Set Functors
~~~~~~~~~~~~

Alternative of ``Functor Set``:
::

  class Functorb f b  where
    fmapb :: (a -> b) -> f a -> f b 
    
    --instance {-# OVERLAPPABLE #-} Functor f =>  Functorb f b  where 
    instance (Functor f) =>  Functorb f b  where 
        fmapb = fmap
    
    instance (Ord b, not. Functor Set) => Functorb Set b  where
        fmapb = mapSet

Reduce overlapping
~~~~~~~~~~~~~~~~~~

We could easy reduce overlapping in many cases
::

  -- was
  instance {-# OVERLAPPABLE #-} context1 => C Int b     where ...  -- (A)
  instance {-# OVERLAPPABLE #-} context2 => C a   Bool  where ...  -- (B)
  instance {-# OVERLAPPABLE #-} context3 => C a   [b]   where ...  -- (C)
  instance {-# OVERLAPPING  #-} context4 => C Int [Int] where ...  -- (D)

  -- NEW!
  instance forall b. b /~ Bool   => C Int b     where ...  -- (A)
  instance                          C a   Bool  where ...  -- (B)
  instance forall a b. b /~ Int  => C a   [b]   where ...  -- (C)
  instance                          C Int [Int] where ...  -- (D)


Unlocking Multi-Defaults
~~~~~~~~~~~~~~~~~~~~~~~~

This proposal does not require to have non-overlapping class multi-defaults with ``DefaultSignatures`` extension, but this Proposal allow to write such defaults easy **if it would permitted**
::

  -- unlock non-overlapping default
  class C1 a where
    foo :: a -> a -> a
  
    default foo :: (not. C3 a,     C2 a, C1 a) => a -> a -> a

    default foo :: (C3 a,     not. C2 a, C1 a) => a -> a -> a

    default foo :: (C3 a,          C2 a, C1 a) => a -> a -> a


Effect and Interactions
-----------------------

UnicodeSyntax
~~~~~~~~~~~~~

``∄`` (∄, There Does Not Exist, U+2204) is added to ``UnicodeSyntax`` as synonym for ``not`` pseudo-keyword.

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

We expect the implementation and maintenance costs of ``NotConstraints`` extension has medium difficulty.


Backward Compatibility
----------------------

This proposal is backward compatible.


Alternatives
------------

Main alternatives are ``Type Families`` (including Associated type families) extension, and deprecated ``OverlappingInstances`` and ``IncoherentInstances`` extensions


Prior art
---------

In Rust language similar feature "Negative Trait Bounds" is highly requested ``<T: !SomeTrait>`` , but is not yet approved. 

A magical Trait ``Sized`` supports special ``<T: ?Sized>`` (which is similar to ``<T: Sized or !Sized>`` on pseudo-Rust)

Also Rust recently adds feature "Negative Impls" ``impl !SomeAutoTrait for SomeType`` which forbids to deriving and custom implementations. Negative impls are used to declare that ``&T: !DerefMut`` and ``&mut T: !Clone`` , as required to fix the soundness of ``Pin``.


Unresolved Questions
--------------------

It is unclear about Self Recursive Instances and some conditions with QuantifiedConstraints.


Implementation Plan
-------------------

It is unclear.

