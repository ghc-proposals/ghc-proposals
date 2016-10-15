.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

Constraint to Bool type family
==============================

The proposal is to include a new wired-in type family,::
   
   type family Fulfilled (c :: Constraint) :: Bool where

If GHC can solve the constraint ``c``, then ``Fulfilled c ~ True`` else
``Fulfilled c ~ False``.

Motivation
----------

Currently GHC doesn't easily allow to choose a type-class instance based on a
context. Suppose for example that you want to provide a new ``ShowHTML`` class
that pretty print a data type into a nice HTML document,::

   class ShowHTML a where
      showHTML :: a -> HTML

Not everyone is using your class yet so you would like fall back cases for data
types with ``Show`` instances and also for data types without a ``Show``
instance,::

   class Show a => ShowHTML a where
      showHTML a = toHtml (show a)

   -- how to declare a default ShowHTML instance for all the data types that
   -- have neither a ShowHTML nor a Show instance?
   -- class ShowHTML a where
   --    showHTML _ = toHtml "Cannot show this data type"


This proposal allows to simply fix these kinds of problems.

Let's present a simpler example first, the one exposed here
`AdvancedOverlap <https://wiki.haskell.org/GHC/AdvancedOverlap>`_.
We would like to write the following code, but we can't because both instances
match the same data types (remember that contexts are not taken into account
during instance selection),::

   class Print a where
       print :: a -> IO ()

   instance Show a => Print a where
       print x = putStrLn (show x)

   instance           Print a where
       print x = putStrLn "No show method"

The solution consists in adding a parameter to the type class that indicates
whether to use one instance or the other,::

   class Print u a where
       print' :: a -> IO ()

   instance Show a => Print True a where
       print' x = putStrLn (show x)

   instance           Print False a where
       print' x = putStrLn "No show method"

The wiki page presents several methods that force the user to declare
boilerplate class instances or type family instances that serve as evidences
that a data type has a ``Show`` instance. But we don't need that anymore, we can
use the ``Fulfilled`` type family instead,::


   print :: forall u a.
               ( u ~ Fulfilled (Show a)
               , Print u a
               ) => a -> IO ()
   print = print' @u

And it works,::

   main :: IO ()
   main = do
      print (5 :: Int)        -- "5"
      print (5.0 :: Double)   -- "5.0"
      print id                -- "No show method"


Now this was a simple example with only two cases, but we can handle the more
complex ``ShowHTML`` example as easily,::

   {-# LANGUAGE ConstraintKinds #-}
   {-# LANGUAGE FlexibleContexts #-}
   {-# LANGUAGE DataKinds #-}
   {-# LANGUAGE TypeFamilies #-}
   {-# LANGUAGE MultiParamTypeClasses #-}
   {-# LANGUAGE FlexibleInstances #-}
   {-# LANGUAGE AllowAmbiguousTypes #-}
   {-# LANGUAGE ScopedTypeVariables #-}
   {-# LANGUAGE TypeApplications #-}
   {-# LANGUAGE TypeOperators #-}
   {-# LANGUAGE UndecidableInstances #-}
   {-# LANGUAGE PolyKinds #-}
   
   import GHC.Exts
   import GHC.TypeLits
   
   ------------------------
   -- Some helpers
   ------------------------
   
   data Assoc (c :: Constraint) (u :: Symbol)
   
   -- | Select the first fulfilled constraint in the list.
   -- Fail otherwise.
   type family SelectConstraint (fs :: [*]) :: Symbol where
      SelectConstraint fs = TSel fs fs
   
   type family TSel (fs :: [*]) (as :: [*]) :: Symbol where
      TSel '[] as               = TypeError (Text "Cannot find any fulfilled constraint in"
                                             :<>: ShowType (ExtractC as))
      TSel (Assoc c a ': cs) as = If (Fulfilled c) a (TSel cs as)
   
   type family ExtractC (fs :: [*]) where
      ExtractC '[]               = '[]
      ExtractC (Assoc c u ': cs) = c ': ExtractC cs
   
   type family If c t e where
      If 'True  t e = t
      If 'False t e = e
   
   --------------------------------
   -- the instance selection logic
   --------------------------------
   
   data HTML = HTML String deriving (Show)
   
   class ShowHTML a where
      showHTML :: a -> HTML
   
   class ToHTML u a where
      toHTML' :: a -> HTML
   
   instance ShowHTML a => ToHTML "showHTML" a where
      toHTML' = showHTML
   
   instance Show a     => ToHTML "oldShow" a where
      toHTML' a = HTML ("<old>" ++ show a ++ "</old>")
   
   instance               ToHTML "none" a where
      toHTML' _ = HTML "<none>Cannot show this data type</none>"
   
   -- THE INTERESTING FUNCTION
   toHTML :: forall a u.
      ( ToHTML u a
      , u ~ SelectConstraint
               '[ Assoc (ShowHTML a) "showHTML"
                , Assoc (Show a)     "oldShow"
                , Assoc ()           "none"  -- try commenting this line
                ]
      ) => a -> HTML
   toHTML = toHTML' @u
   
   --------------------------------------
   -- Some data and type class instances
   --------------------------------------
   
   instance ShowHTML Int where
      showHTML a = HTML "<new>Int</new>"
   
   data Dummy = Dummy
   
   data Nice = Nice
   instance ShowHTML Nice where
      showHTML _ = HTML "<new>Nice!</new>"
   
   --------------------------------------
   -- Some tests
   --------------------------------------
   
   main :: IO ()
   main = do
      print (toHTML (5 :: Int))     -- Int has both ShowHTML and Show instances
      print (toHTML (5 :: Double))  -- Double has only Show instance
      print (toHTML Nice)           -- Nice has only ShowHTML
      print (toHTML Dummy)          -- Dummy has none of ShowHTML and Show
      print (toHTML' @"oldShow" (5 :: Int)) -- We can also select the method explicitly

   -- Printed results:
   -- HTML "<new>Int</new>"
   -- HTML "<old>5.0</old>"
   -- HTML "<new>Nice!</new>"
   -- HTML "<none>Cannot show this data type</none>"
   -- HTML "<old>5</old>"

To understand this code, you need to see that ``SelectConstraint`` tries to
``Fulfil`` each constraint in the list in sequence and returns the ``Symbol``
associated to the first fulfilled constraint (i.e., "showHTML", "oldShow" or
"none") or fails with a type-error. Then this symbol is used to select the
``ToHTML`` instance.



Proposed Change
---------------

Add a wired-in type family in GHC.Exts for instance,::
   type family Fulfilled (c :: Constraint) :: Bool where

If GHC can solve the constraint ``c``, then a coercion ``Fulfilled c ~ True`` is
provided, otherwise a coercion ``Fulfilled c ~ False`` is provided. ``c``
mustn't be used to provide evidences for other constraints. Solving ``c``
mustn't trigger any warning or error.

I already have a working proof-of-concept implementation:
https://github.com/hsyl20/ghc/compare/constraint-to-bool Examples above have
been sucessfully tested with it. The interesting function is
``tcFulfilConstraint``. Comments and code reviews are welcome.

Drawbacks
---------

I can't think of any drawback.

Alternatives
------------

None.

Unresolved Questions
--------------------

None.
