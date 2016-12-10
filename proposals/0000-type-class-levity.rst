.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

Proposal title
==============

Use levity polymorphism for type classes.

Motivation
----------

Currently ``(+#) :: Int# -> Int# -> Int#``, ``(+##) :: Double# -> Double# -> Double#`` cannot be written with ``(+) @Int#``, ``(+) @Double#``. This redundancy can be avoided by making ``Num`` levity polymorphic and defining instances for those types.

Proposed Change
---------------

More detail in the trac ticket `#12708 <https://ghc.haskell.org/trac/ghc/ticket/12708/>`_.

Instead of

.. code-block:: haskell

  class Num (a :: Type) where
    (+) :: a -> a -> a
    
this proposal makes it levity polymorphic

.. code-block:: haskell

  class Num (a :: TYPE rep) where
    (+) :: a -> a -> a
  
  instance Num Int# where
    (+) :: Int# -> Int# -> Int#
    (+) = (+#)
    
  instance Num Double# where
    (+) :: Double# -> Double# -> Double#
    (+) = (+##)

This also works for other type classes (thoughts on kind of `Functor`)

.. code-block:: haskell

  class Show        (a :: TYPE rep)
  class Read        (a :: TYPE rep)
  class Eq          (a :: TYPE rep)
  class Eq a => Ord (a :: TYPE rep)
  class Semigroup   (a :: TYPE rep)
  class Monoid      (a :: TYPE rep)
  class Functor     (f :: TYPE rep1 -> TYPE rep2)
  class Pointed     (f :: TYPE rep1 -> TYPE rep2)
  class Applicative (f :: Type      -> TYPE rep)
  class Alternative (f :: Type      -> TYPE rep)
  class Foldable    (f :: TYPE rep1 -> TYPE rep)
  class Eq1         (f :: TYPE rep1 -> TYPE rep2)
  ...

and the interesting ``Distributive :: (TYPE rep -> TYPE rep) -> Constraint`` and ``Representable`` which has a ``Distributive`` superclass but may have a more general kind, I will look into this. This allows weird instances like

.. code-block:: haskell

  data PAIR :: TYPE IntRep -> Type where
    (:#) :: int -> int -> PAIR int
    
  instance Functor PAIR where
    fmap :: (a -> b) -> (PAIR a -> PAIR b)
    fmap f (a :# b) = f a :# f b
    
  instance Pointed PAIR where
    point :: a -> PAIR a
    point i = i :# i
    
  instance Foldable PAIR where
    fold :: Monoid m => PAIR m -> m
    fold (a :# b) = a <> b

giving us ``fold (10# :# 100#) :: Int#``.

Once the `unlifted data types <https://ghc.haskell.org/trac/ghc/wiki/UnliftedDataTypes/>`_ proposal is implemented we will have a better motivating examples.

Drawbacks
---------

Since `TypeApplications` order of type arguments matters. What was once ``(+) @Int`` would now become ``(+) @IntRep @Int``, ``(+) @_ @Int``.

Alternatives
------------


Unresolved Questions
--------------------

* What should the kinds be.
* How many type classes are susceptible to this treatment, does it somehow affect the hiearchy (see ``Distributive`` and ``Representable``).
