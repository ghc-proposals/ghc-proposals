.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/141>`_.

.. contents::

InfixAs Extension
==========================

This is a proposal for a language extension that provides some benefits to readability and conformity in the declaration of infix operators.

Consider the following typeclass, and associated infix operator:

.. code-block:: haskell

  class Semigroup a where
    app :: a -> a -> a

  infixr 5 <>

  (<>) :: Semigroup a => a -> a -> a
  (<>) = app

With -XInfixAs enabled, one must write the following instead:

.. code-block:: haskell
  
  class Semigroup a where
    app :: a -> a -> a

  infixr 5 app as <>

For code generation, this will generate a top-level binding, so that error messages arising
from the use of ``<>`` do not confusingly reference ``app`` (i.e., ``<>`` would not just be a function 'synonym' ala type synonyms).
The top-level binding would look like this:

.. code-block:: haskell
  
  infixr 5 <>

  (<>) :: Semigroup a => a -> a -> a
  (<>) = app

Another example:

.. code-block:: haskell

  append :: [a] -> [a] -> [a]
  append xs [] = xs
  append [] ys = ys
  append (x:xs) ys = x : append xs ys

  infixr 5 append as ++

  oneToThree :: [Int]
  oneToThree = [1,2,3]

  fourToSix :: [Int]
  fourToSix = [4,5,6]

  oneToSix :: [Int]
  oneToSix = oneToThree ++ fourToSix

Motivation
------------

The point of this syntax is to ensure that there is no other way to define an operator, which guarantees by construction that all operators will have a corresponding function name, thus avoiding the situation in Haskell where it is sometimes unclear what the canonical pronunciation of an operator is. It also forces you to write a fixitiy declaration for every operator. Enabling this language extension would make operator definitions a compile-time error.

Proposed Change Specification
-----------------------------

Effect and Interactions
-----------------------

I don't currently know of any.

Costs and Drawbacks
-------------------

The only cost I can see as of right now is the work to implement this.
Admittedly I do not know how, but I would be willing to do the work if given guidance.

Alternatives
------------

Unresolved questions
--------------------

Implementation Plan
-------------------

Currently Unknown.
