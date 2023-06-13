Hybrid class-instances and instance-classes
====================

.. author:: Viktor WW
.. date-accepted::
.. ticket-url:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/593>`_.
.. sectnum::
.. contents::

This proposal helps refactoring classes and instances such that axioms / rules and emergent properties are separated, but they are easy combined


Motivation
----------

Usually we wish to have axioms / rules and emergent properties to be separated, but they are easy combined. 
Or we wish to change exist classes such that its properties have same behavior.

In these cases hybrid class-instances and instance-classes are useful.


Proposed Change Specification
-----------------------------

Syntax
~~~~~~

Syntax is easy ::

    -- class-instance declaration
    class instance (C1 a, C2 a, ... Cn a) => CI a where
        ...

    -- instance-class declaration
    instance class (C1 a, C2 a, ... Cn a) => IC a where
        ...
        
        
    -- instance of class-instance
    instance CI SomeType
    
    -- instance of instance-class
    instance IC SomeType



Class-Instance Semantics
~~~~~~~~~~~~~~~~~~~~~~~~

If we write ``class instance (C1 a, C2 a, ... Cn a) => CI a`` then:

1) class-instance ``CI`` is a class for ``CI``, so it could have new methods if any is needed
2) class-instance ``CI`` is a "default instance" for ``C1``, ``C2``, .. ``Cn``
3) default definitions inside ``CI`` from ``C1``, ``C2``, .. ``Cn`` methods of are stronger, 
   then in default definitions inside ``C1``, ``C2``, .., ``Cn`` ,
   but they are weaker then definitions on instances and deriving of ``C1``, ``C2``, .. ``Cn``
4) default definitions inside ``CI`` from ``C1``, ``C2``, .. ``Cn`` methods must cover ``MINIMAL`` 
   of ``C1``, ``C2``, .. ``Cn`` or minimal methods must be re-borrow into ``MINIMAL`` of ``CI``
5) instances of ``CI`` could (or must) implement methods of ``C1``, ``C2``, .. ``Cn`` classes
6) if instances of ``C1``, ``C2``, .. ``Cn`` does not yet exist after implementation of instances of ``CI``, 
   they are created automatically by empty-body instances
7) if ``CI`` have no instances, all "default instance" for ``C2``, ``C2``, .. ``Cn`` has no effect at all


Instance-Class Semantics
~~~~~~~~~~~~~~~~~~~~~~~~

If we write ``instance class (C1 a, C2 a, ... Cn a) => IC a`` then:

1) instance-class ``IC`` is a like an instance, so it must have no new methods
2) Rules (2) ... (7) of instance-class ``IC`` has same properties as class-instance ``CI``
3) instance-class ``IC`` are constraint synonym and we suggest to use it in this role


Examples 
--------

Refactoring Ord
~~~~~~~~~~~~~~

Let we wish refactor class `Ord`, but fully backward compatible. We could write next ::

    class  Ords a  where
        compare              :: a -> a -> Ordering
        (<), (<=), (>), (>=) :: a -> a -> Bool
        max, min             :: a -> a -> a

        x <= y = y >= x
        x >= y = not (x < y)
        x > y  = not (x <= y)
        x < y  = not (y <= x)
        max x y = if x <= y then y else x
        min x y = if x <= y then x else y
		
        {-# MINIMAL compare, ((<=) | (>=) | (<) | (>)) #-}
	

    instance class (Eq a, Ords a) => Ord a  where

        compare x y = if x == y then EQ
                      else if x <= y then LT
                      else GT

        x <= y = case compare x y of { GT -> False; _ -> True }
        x >= y = y <= x
        x > y = not (x <= y)
        x < y = not (y <= x)

        {-# MINIMAL compare | (<=) #-}	


Refactoring Num
~~~~~~~~~~~~~~

Let we wish refactor class `Num`, but fully backward compatible. We could write next ::

    class Numerals a  where
        fromInteger         :: Integer -> a
        abs                 :: a -> a

    class Numerals a => Sum a  where
        (+)                 :: a -> a -> a

    class Sum a => Subtract a  where
        {-# MINIMAL negate | (-) #-}
        (-)                 :: a -> a -> a
        negate              :: a -> a

        x - y               = x + negate y
        negate x            = 0 - x

    instance class (Sum a, Subtract a) => Counting a

    class Numerals a => Product a  where
        (*)                 :: a -> a -> a

    instance class (Sum a, Product a) => Arithmetic a

    class Numerals a => Sign a  where
        signum              :: a -> a
        
    class instance (Arithmetic a, Sign a) => Num a


Refactoring Group
~~~~~~~~~~~~~~

Let we wish refactor class Algebraic structure hierarchy, but fully backward compatible 

We need `Class Scoped Functions #590 <https://github.com/ghc-proposals/ghc-proposals/pull/590>`_ for full backward compatibility 

We could write next::

    class Magma a where
        (<>) :: a -> a -> a

    class Idempotent a where
        idempotent :: a

    class Inverse a where
        inverse :: a -> a

    class Absorb a where
        absorb :: a

    class instance Idempotent a => EmptyCat a
        -- RFC #590
        function cempty
        cempty = idempotent

        idempotent = cempty

    cempty :: EmptyCat a => a
    cempty = idempotent

    class instance Magma a => Associativity a
        (<>) :: a -> a -> a
        a <> b = sconcat (a :| [ b ])

        sconcat :: NonEmptyCat a -> a
        sconcat (a :| as) = go a as where
            go b (c:cs) = b <> go c cs
            go b []     = b

        {-# MINIMAL (<>) | sconcat #-}

    class instance Associativity a => Commutativity a

    instance class Associativity a => Semigroup a

    instance class Inverse a => Quasigroup a

    instance class EmptyCat a => UnitalMagma a

    instance class EmptyCat a, Semigroup a => MonoidCat a

    class instance MonoidCat a => Monoid a

        -- RFC #590
        function mempty
        mempty :: Monoid a => a
        mempty = cempty

        cempty = mempty

        -- RFC #590
        function mappend
        mappend :: Monoid a => a -> a -> a
        mappend = (<>)

        (<>) = mappend

        mconcat :: Monoid a => [a] -> a
        mconcat = foldr (<>) empty

    mempty :: Monoid a => a
    mempty = cempty

    mappend :: Monoid a => a -> a -> a
    mappend = (<>)

    instance class EmptyCat a, Quasigroup a => LoopCat a

    instance class Inverse a, EmptyCat a, Associativity a => Group a

    instance class Absorb a, Semigroup a => AbsorbSemigroup a

    instance class Absorb a, MonoidCat a => AbsorbMonoid a

    instance class Commutativity a, MonoidCat a => CommutativeMonoid a

    instance class Commutativity a, Group a => AbelianGroup a


Effect and Interactions
-----------------------

Any Effect and Interactions are unknown.

Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs for this feature to be minimal.

Backward Compatibility
----------------------

This proposal is fully backward compatible.

Alternatives
------------

A partial alternative is `Class Backend - separate interface from representation #461 <https://github.com/ghc-proposals/ghc-proposals/pull/461>`_

Implementation Plan
-------------------

It is unclear.
