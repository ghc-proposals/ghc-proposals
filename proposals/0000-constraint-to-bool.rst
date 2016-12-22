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

The proposal is to include a new wired-in type family::
   
   type family Fulfilled (c :: Constraint) :: Bool where

If GHC can solve the constraint ``c``, then ``Fulfilled c ~ True`` else
``Fulfilled c ~ False``.

Motivation
----------

Currently GHC doesn't easily allow to choose a type-class instance based on a
context. Suppose for example that you want to provide a new ``ShowHTML`` class
that pretty print a data type into a nice HTML document::

   class ShowHTML a where
      showHTML :: a -> HTML

Not everyone is using your class yet so you would like fall back cases for data
types with ``Show`` instances and also for data types without a ``Show``
instance::

   class Show a => ShowHTML a where
      showHTML a = toHtml (show a)

   -- how to declare a default ShowHTML instance for all the data types that
   -- have neither a ShowHTML nor a Show instance?
   -- class ShowHTML a where
   --    showHTML _ = toHtml "Cannot show this data type"


This proposal allows to simply fix these kinds of problems.

Example 1
~~~~~~~~~

Let's present a simpler example first, the one exposed on
`AdvancedOverlap <https://wiki.haskell.org/GHC/AdvancedOverlap>`_.
We would like to write the following code, but we can't because both instances
match the same data types (remember that contexts are not taken into account
during instance selection)::

   class Print a where
       print :: a -> IO ()

   instance Show a => Print a where
       print x = putStrLn (show x)

   instance           Print a where
       print x = putStrLn "No show method"

The solution consists in adding a parameter to the type class that indicates
whether to use one instance or the other::

   class Print u a where
       print' :: a -> IO ()

   instance Show a => Print True a where
       print' x = putStrLn (show x)

   instance           Print False a where
       print' x = putStrLn "No show method"

The wiki page presents several methods that force the user to declare
boilerplate class instances or type family instances that serve as evidences
that a data type has a ``Show`` instance. But we don't need that anymore, we can
use the ``Fulfilled`` type family instead::


   print :: forall u a.
               ( u ~ Fulfilled (Show a)
               , Print u a
               ) => a -> IO ()
   print = print' @u

And it works::

   main :: IO ()
   main = do
      print (5 :: Int)        -- "5"
      print (5.0 :: Double)   -- "5.0"
      print id                -- "No show method"

Example 2
~~~~~~~~~

Now this was a simple example with only two cases, but we can handle the more
complex ``ShowHTML`` example as easily::

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

Example 3
~~~~~~~~~

And now this is the real world example that led to this proposal.

Current ``Foreign.Storable`` class is defined as::

   class Storable a where
      sizeOf :: a -> Int
      peek   :: Ptr a -> IO a
      ...

We can automatically generate instances from a ``Generic`` instance (cf
`c-storable-deriving package
<https://github.com/maurer/c-storable-deriving/blob/master/Foreign/CStorable/TypeClass.hs>`_)
with default signatures::

   class Storable a where
      sizeOf :: a -> Int
      peek   :: Ptr a -> IO a

      default sizeOf :: (Generic a, GStorable (Rep a)) => a -> Int
      sizeOf = genericSizeOf

      default peek :: (Generic a, GStorable (Rep a)) => Ptr a -> IO a
      peek = genericPeek
      ...

Issue 1) I want to replace ``sizeOf`` with a type-level literal (to compute
offsets at compile time, among other things)::

   class Storable a where
      type SizeOf a :: Nat
      peek   :: Ptr a -> IO a

AFAIK, we don't have the equivalent of default signatures for associated type
families, so I can't define::

   class Storable a where
      type SizeOf a :: Nat
      default type (Generic a, GStorable (Rep a)) => SizeOf a = GenericSizeOf a

Issue 2) I want to support several ways to store a data type, namely ``Struct``
and ``PackedStruct``, with ``Struct`` being the default. A solution would be to
allow several default signatures, but it isn't supported either (see `#7395
<https://ghc.haskell.org/trac/ghc/ticket/7395>`_)::

   class Storable a where
      peek :: Ptr a -> IO a

      default peek :: ( Generic a, GStorable (Rep a)
                      , Layout a ~ Struct) => Ptr a -> IO a
      peek = genericPeekStruct

      default peek :: ( Generic a, GPackedStorable (Rep a)
                      , Layout a ~ PackedStruct) => Ptr a -> IO a
      peek = genericPeekPackedStruct

      default peek :: (Generic a, GStorable (Rep a)) => Ptr a -> IO a
      peek = genericPeekStruct

It seems to me that default signatures already are a kind of instance
selection based on the context. But it is hard to compose with them (e.g.,
add default signatures to a class without modifying it, overload them, mix
them with default methods, etc.). So with the proposal, I wouldn't use
them but instead I could write a ``peek' :: Ptr a -> IO a`` method that
would test in sequence:

1) If (Fulfilled (Storable a)), then use Storable's peek method
2) If (Fulfilled (Generic a, HasStorageMethod a)), then use ``genericPeek @(StorageMethod a)``
3) If (Fulfilled (Generic a)), then use ``genericPeek @Struct``
4) TypeError

where::

   data Struct
   data PackedStruct

   class HasStorageMethod a where
      type StorageMethod a :: *

   class GStorable m r where
      genericPeek :: Ptr (r a) -> IO (r a)

   instance GStorable Struct U1 where ...
   instance GStorable Struct (a :*: b) where ...

   instance GStorable PackedStruct U1 where ...
   instance GStorable PackedStruct (a :*: b) where ...


And I can do the same thing for a ``SizeOf'`` type family::

   type SizeOf' a = SelectConstraint
         '[ Assoc (Storable a) (SizeOf a)
          , Assoc (Generic a, HasStorageMethod a)
               (GenericSizeOf (StorageMethod a) a)
          , Assoc (Generic a) (GenericSizeOf 'Struct a)
          , TypeError ...
          ]

Then I want to do the same kind of things to generate ``Binary`` instances from
``Storable`` or ``Generic`` instances.

Proposed Change
---------------

Add a wired-in type family in GHC.Exts for instance::

   type family Fulfilled (c :: Constraint) :: Bool where

GHC will try to simplify the constraint ``c`` in the current type-checking
context. I.e., ``c`` is not included in the current type-checking context.

Suppose we want to reduce ``Fulfilled c`` in a module ``M``.  When GHC tries to
simplify the constraint ``c``, it can return 3 different results:

1) Fulfilled: ``c`` is simplified into an empty constraint

2) Insoluble (e.g., found ``Int ~ Bool``)

3) Unsure: ``c`` is simplified into a new constraint ``c'``. This case happens
   with constraints involving open things (e.g., open type families, type classes)
   for which some evidences may be added in another context (e.g., open type family
   instances, orphan type class instances).

Case 1:
``c`` is fulfilled and we coerce ``Fulfilled c ~ True``. Every module importing
``M`` also imports the evidences that make ``c`` fulfilled.

Case 2:
``c`` is insoluble and we coerce ``Fulfilled c ~ False``.  Every module
importing ``M`` also imports the evidences that make ``c`` insoluble.

Case 3:
``c`` may be fulfilled in another type-checking context, but not in this one, so
we coerce ``Fulfilled c ~ False``. Every module importing ``M`` may provide new
evidences that make ``c`` fulfilled leading to unsoundness. Hence we export
``c'`` in a module ``unwantedConstraints`` set (exported in the module
interface). Now for a module ``N`` importing ``M``, we need to try to fulfil
each unwanted constraint ``c`` of ``M`` in the context of ``N``:

Case 3.1:
An unwanted constraint ``c`` has become fulfilled: we trigger an error. E.g.,
"Imported module M has already assumed the following constraint would be
insoluble while it isn't: c.  Use -XIncoherentUnwantedConstraints to allow the
import of M."

Case 3.2:
``c`` is now proved to be insoluble. We don't add it to the
``unwantedConstraints`` set of ``N``.

Case 3.3:
``c`` is still neither fulfilled nor insoluble. We add ``c'`` to the
``unwantedConstraints`` set of ``N``.

Unsoundness example
~~~~~~~~~~~~~~~~~~~

Here is an unsound example (provided by @ezyang)::

   module A where

   class C a

   f :: Fulfilled (C Bool) ~ True => a -> b
   f x = x


   module B where

   import A
   instance C Bool

   g :: a -> b
   g = f

An orphan ``instance C Bool`` is defined in ``module B``. If we don't check for
unwanted constraints, we will face a coherence issue because ``Fulfilled (C
Bool)`` was assumed to reduce to ``False`` in ``module A`` but it reduces to
``True`` in ``module B``.

In the current proof-of-concept implementation, GHC reports::

   B.hs:1:1: error:
       Imported module A has already assumed that the following constraint
       cannot be fulfilled, but now it can: C Bool

Recursive examples
~~~~~~~~~~~~~~~~~~

Recursive example (provided by @clintonmead)::

   forall t. t ~ Fulfilled (t ~ False)

``t ~ False`` doesn't simplify so we are in case 3 and we coerce ``Fulfilled (t ~ False) ~
False``. However, GHC seems to check the constraint again in the current
implementation because it reports: "Couldn't match type ‘'False’ with ‘'True’".

Variant::

   forall t. t ~ Fulfilled (t ~ True)

Similarily in this case we coerce ``Fulfilled (t ~ True) ~ False``. However this
time, the program compiles and we can show that ``t ~ False``.

Notwithstanding the difference between the two examples (one compiles and the
other one doesn't), what happens is that the reduction of a ``Fulfilled c``
makes the type-checking context change. In this new context, ``Fulfilled c`` may
have been reduced differently! To check this, once we have reduced ``Fulfilled
c``, we need to check that ``Fulfilled c`` still reduces in the same way.

This is not implemented yet.

Interaction with FlexibleInstances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Example provided by @int-index::

   {-# LANGUAGE FlexibleInstances #-}
   {-# LANGUAGE TypeFamilies #-}
   {-# LANGUAGE DataKinds #-}

   import GHC.Exts

   class C a where
      c :: a -> a

   instance Fulfilled (C a) ~ False => C a where
      c = id

This instance is invalid because it makes the type-checker goes into an infinite
loop. We should detect that the argument to ``Fulfilled`` "is no smaller than C
a" and force the use of ``UndecidableInstances`` to allow this.

This is not implemented yet.

Proof-of-concept implementation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

I already have a working proof-of-concept implementation:
https://github.com/hsyl20/ghc/compare/constraint-to-bool
Comments and code reviews are welcome.

Drawbacks
---------

To avoid unsoundness, each module exports a list of unwanted constraints. This
has a cost: interface files are larger; unwanted constraints are checked and
propagated by the importing modules.

The motivation cases mostly use ``Fulfilled`` to detect type-class instances
used as type annotations/tags. It would be useful to have "semi-closed"
type-classes that do not allow orphan instances, and hence would not propagate
in the "unwanted constraints" set.


Alternatives
------------


Unresolved Questions
--------------------

