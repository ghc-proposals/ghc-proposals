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

Extensions to Infix Syntax
==========================

This is a proposal to extend Haskell's ``infix`` syntax to include a feature of PureScript's.

Consider the following typeclass:

.. code-block:: haskell

  class Semigroup a where
    app :: a -> a -> a

With this extension to the syntax, one could write the following:

.. code-block:: haskell
  
  infixr 5 app as <>

  foo :: String
  foo = "Hello " <> "World!"

This will actually generate a top-level binding, so that error messages arising
from the use of ``<>`` do not confusingly reference ``app``. The top-level binding
would look like this:

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

This proposal makes it easier to define infix synonyms for binary operators/functions,
a relatively common pattern employed by library authors.

Proposed Change Specification
-----------------------------

Effect and Interactions
-----------------------

I don't know of any interactions other than making it syntactically simpler to define infix functions.

Costs and Drawbacks
-------------------

The only cost I can see as of right now is the work to implement this.
Admittedly I do not know how.

Alternatives
------------

Unresolved questions
--------------------

Implementation Plan
-------------------

Currently Unknown.
