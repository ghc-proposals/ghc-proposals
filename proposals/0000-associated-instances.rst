Associated Instances
====================

.. author:: Ashok Kimmel
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/757>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

This proposal allows for a ``instance`` declarations within a 
parent ``class`` declaration, so that classes can be split up while 
still preserving building with multiple versions (e.g. the ``base`` 3 version policy).


Motivation
----------
Haskell had, and still has quite a few problems with old classes, particularly ``Monoid``, ``Num``, and ``Monad``.
While ``Monoid`` was refactored to be a superclass of ``Semigroup``
, the other two classes, while imperfectly designed,
can't be changed without breaking almost all haskell code. 

This proposal is meant for classes to allow for associated instances, 
so that instances of the parent class automatically get instances of the associated class.
Another thing this helps with is long tedious class hierarchies, 
like the ``Profunctor,Choice,Strong,Traversing,Closed,Mapping``. 
While it's good for them to be seperate classes, class synonyms, like ``MappingAll``, could make bolierplate much easier.

**Code generation:**

Associated instances should not be used for code generation of simple classes, like ``Functor``.
The tool for that is already available: ``deriving via``, e.g. 
``deriving Functor f via WrappedTraversable f``.

If the boilerplate of writing ``data Wrapped___`` is too much,
just write ``data Wrapped1 cls f a = Wrapped1 (f a)``, 
and ``instance Traversable f => Functor (Wrapped1 Traversable f) where ...``.

``GHC`` code generation mechanisms are complicated enough: 
``default methods``, ``deriving via``, ``Template Haskell``, ``deriving Generic`` and ``deriving Data``. 
When coupled with the helper functions ``syb``, ``generics-sop``, ``Generically``, etc., form more than enough tools for code generation.

Proposed Change Specification
-----------------------------
Allow for ``instance`` declarations within a class declaration.
Allow for ``hiding instance`` and ``instance`` declarations within a class instance.
A new ``-wspurious-hiding-instance`` warning is added for when a ``hiding instance`` declaration doesn't hide an instance.
A new ``-XAssociatedInstances`` extension is added to enable all these features.

Syntax
~~~~~~

The proposed change introduces a new language extension ``-XAssociatedInstances`` that allows 
``associated instance`` declarations to be nested directly inside  the ``parent class``.
This enables classes to automatically providing instances for the ``associated class``.

**BNF Grammar:**
This proposal adds another production to cls_decl
::
 
  <cls_decl>       ::= ... | <inst_decl>

The former is used to disable ``implicit associated instance`` generation, 
and the latter is used to disable warning generated when there are no associated instances.
This proposal also adds a new production to inst_decl for ``Explicit Associated Instances``
:: 
 
  <inst_decl> ::= ... | <inst_decl> | "hiding" <inst_decl>

**Core Concept:**

The ``instance`` declarations nested inside a class definition are bundled with the class 
and are generated whenever an instance of that class is created. 
Generally speaking, to the creator of the instance declarations,
there is no difference if an associated instance was used. 

:: 
 
 class Small a where 
   smallMethod :: a -> String
 
 -- Note: Small is not a superclass of Big
 -- As such, Small a => c does not mean Big a => c

 -- In practice I expect that the associated instance will usually make the parent class a superclass

 -- The "Parent class"
 class Big where 
  -- lots of methods
  -- The "Associated class"
  instance Small a 


::
 
 instance Big MyType where 
  -- lots of methods
  smallMethod x = show x

This is an ``Implicit Associated Instance``. It is equivalent to:
:: 
 
 class Small a where 
   smallMethod :: a -> String
 class Big where 
  -- lots of methods

::
 
 instance Small MyType where
   smallMethod x = show x
 instance Big MyType where 
   -- lots of methods 

**Hiding instances:**

If you want to declare the ``Seperate Instances``,
use the ``hiding instance`` keywords within the class to disable ``Implicit Associated Instances``.
:: 
 
 instance Small String where 
  smallMethod1 x = x
 instance Big String where
  hiding instance Small String  
 -- lots of methods

When hiding an instance that doesn't exist, a ``-wspurious-hiding-instance`` warning is raised.
This warning will only be added to ``-Wall``, as a removed instance is unlikely to be readded, and 
library authors would could not remove an deprecated instance without causing all useages of ``hiding instance``
to be a warning. 


**Default implementations:**

You retain any features of an instance declaration or class declaration 
by using ``AssociatedInstances``, and can thus provide a default
implementation of associated instances, the same way you would in a class declaration.
``Minimal`` pragmas include the associated instances, and if an instance is hidden,
the class coverage checker should assume that all its methods are implemented.

::
 
 class Small a where 
    smallMethod1 :: a -> String
    smallMethod2 :: a -> Int
 class Small a => Big a where
  bigMethod1 :: a -> a
  instance Small a where 
    bigMethod1 :: a -> a 
    default smallMethod1 :: Show a => a -> String 
    smallMethod1 x = show $ bigMethod1 x
  {-# MINIMAL smallMethod2, bigMethod1 {-, the rest of the methods -} #-}

::
 
 instance Big String where 
  bigMethod1 = id 
  smallMetod2 = length x
  -- lots of other methods 

This satisfies the coverage check, and is equivalent to:

::
 
 instance Small String where 
  smallMetod2 = length x
  smallMethod1 x = show $ id x

 instance Big String where 
  hiding instance Small String
  bigMethod1 = id 
  -- lots of other methods 

Since ``smallMethod2`` is defined in a ``seperate instance``,
the class coverage checker assumes that it's implemented.

**Explicit Associated Instances:**
If the same method is used twice, as in this scenario 
::

  -- We will work with peano numbers and integers and doubles in this module
  -- As such, we want a SumX class that works for all three
  class SumX f x where 
    sumX :: f x -> x 

  class SumAny f where 
    sumAny :: Num a => f a -> a
    instance SumX f Integer where 
      sumX = sumIntegral 
    instance SumX f Peano where 
      sumX = sumIntegral
    instance SumX f Float where 
      sumX = sumIntegral

:: 
 
 instance SumX [] where 
  sumIntegral = sum 
  instance SumX [] Peano where 
    sumX x = foldr (+) 0 x 

This feature exists for disambiguating names. While it might not be common, 
it is better than such a thing simply being impossible.

**Nested Associated Instances** 

For nested ``Associated Instances`` we follow the principle of them being opauqe. 
With the example heirarchy below, 
:: 
 class Small a where 
    smallMethod1 :: a -> String
 class Small a => Medium a where
  mediumMethod1 :: a -> String
  instance Small a where 
 class Medium a => Big a where
  bigMethod1 :: a -> a
  instance Medium a

The following is perfectly legal, and is treated as you'd expect,
defining a ``Small``, ``Medium``, and ``Big`` instance for ``String``.
:: 
 instance Big String where 
  bigMethod1 = id 
  mediumMethod1 = id
  smallMethod1 = id

**Constraints in Associated Instances:**
Constraints in associated instances are legal, and work as you'd expect.
::

  class Show a where
    show :: a -> String
  class Show1 f where 
    show1 :: (a -> String) f a -> String
    instance Show a => Show (f a) where 
      show = show1 show 

This allows for anyone making a ``Show1`` instance to get a ``Show`` instance for free. 
This class isn't actually a good idea, as something like ``Const`` doesn't 
need a ``Show a`` instance to get ``Show (Const Int a)``, 
but it shows how constraints work in associated instances. 

**Deprecating Associated Instances:**
If we want to refactor a class, eventually we will want to deprecate the associated instance.
This works in the same way as any other instance. 
::
 
 class Big a where 
  instance {-# DEPRECATED "Use a seperate Small instance" #-} Small a 

This will trigger on all ``Associated Instances``, explicit or implicit.
The only way to supress it is with a ``Seperate Instance`` declaration. 
Importing something like ``Big (smallMethod)`` will also trigger the warning, 
but importing ``Small (smallMethod)`` will not.

Examples
--------
**Functor hierarchy:** 

::

    {-# LANGUAGE AssociatedInstances #-}
    module Data.Functor.Hierarchy where
    
    class Invariant f where 
      invmap :: (a -> b) -> (b -> a) -> f a -> f b
    
    class Invariant f => Functor f where 
      fmap :: (a -> b) -> f a -> f b
      instance Invariant f where 
        invmap f _ x = fmap f x 
    
    class Functor f => Apply f where 
      (<*>) :: f (a -> b) -> f a -> f b
      (<*>) = liftA2 id 
      liftA2 :: (a -> b -> c) -> f a -> f b -> f c
      liftA2 f x y = fmap f x <*> y
    
    class Apply f => Applicative f where 
      pure :: a -> f a
      instance Apply f 
    
    class Apply f => Bind f where 
      (>>=) :: f a -> (a -> f b) -> f b
    
    class (Bind f, Applicative f) => Monad f where 
      instance Bind f

::

    {-# LANGUAGE AssociatedInstances #-}
    module Data.Functor.Compose where 
    import Data.Functor.Hierarchy
    
    newtype Compose f g a = Compose (f (g a))
    
    -- More general class constraints than the automatic version 
    instance (Invariant f, Invariant g) => Invariant (Compose f g) where 
      invmap f g (Compose x) = Compose $ invmap (invmap f g) (invmap g f) x
    -- Normal functor instance.

    instance (Functor f, Functor g) => Functor (Compose f g) where 
      hiding instance Invariant (Compose f g)
      fmap f (Compose x) = Compose $ fmap (fmap f) x
    
    -- Note: Imagine this module was created before the Apply class was introduced.
    -- However, since the creator of Data.Functor.Hierarchy put
    -- instance Apply f
    -- We get the instance 
    -- instance (Applicative f, Applicative g) => Apply (Compose f g)
    -- this doesn't have the more general class constraints
    -- but it's better than the code failing.
    instance (Applicative f, Applicative g) => Applicative (Compose f g) where 
      pure x = Compose $ pure (pure x)
      liftA2 f (Compose a) (Compose b) = Compose $ (liftA2 . liftA2) f a b
    

1. We provided an explicit ``Invariant`` instance (not using automatic generation).
2. We used ``hiding instance`` so ``Functor`` doesn't define the ``Invariant`` instance.
3. For ``Applicative``, the compiler automatically generates the ``Apply`` instance.
4. Our old code that was written before ``Apply`` was a superclass of ``Applicative`` continues to work.

**Refactoring Num without breaking code**

We currently cannot change the ``Num`` class,
because it would break all existing code that defines ``Num`` instances. 
With ``Associated Instances`` we can make it slightly better:

::
 
 class Ring a where
   (+) :: a -> a -> a
   (*) :: a -> a -> a
   -- ... other ring operations
 
 class Ring a => Num a where
   abs :: a -> a
   negate :: a -> a
   -- ... other numeric operations
   instance Ring a
   -- No need for where as we don't have a custom implementation.

This also works.

**Seperating local, ask, and reader from MonadReader**
One of the reasons why Alexis King's automatic deriving of ``mtl`` class is not implemented is 
because of the ``local`` method in ``MonadReader``.
::
  class MonadAsk where 
    ask :: m r
    ask = reader id
    reader :: (r -> a) -> m a
    reader f = f <$> ask
  class MonadAsk => MonadReader r m where 
   instance MonadAsk r m
   local :: (r -> r) -> m a -> m a

Effect and Interactions
-----------------------

**How This Solves the Motivation:**

This proposal directly addresses the issue raised in the Motivation section by allowing class 
hierarchies to evolve while respecting the 3-version policy:

The three version policy states that Lib(N), Lib(N+1), and Lib(N+2) 
must all be compatible, and that it should be possible to support all three versions 
without warnings and without CPP. Here is a sketch for how to do that.
1. Version Lib(N+1), we add a new superclass to the class, and add an associated instance for it.
To support version Lib(N-1), Lib(N),and Lib(N+1) simultaneously, just do nothing.
2. Version Lib(N+3), we deprecate the associated instance.
   To maintain compatibility for 3 versions, users can use ``hiding instance`` to hide the associated instance, and provide their own instance.
3. When people have appropriatly migrated, remove the associated instance.
4. Once the associated instance is removed long enough, users can remove the ``hiding instance`` declaration.

- **Before this proposal**: 
Splitting a method into its own class would break code, 
All existing instances had to be updated.
Before the instance was added,
code couldn't prepare for the new method location, CPP was necessary.

- **After this proposal**: 

Adding a new superclass to a class only requires adding a new associated 
instance. Existing code continues to work because the associated instance provides a default implementation.
After a few versions, the associated instance can be deprecated and eventually removed.

**Interactions with Existing Language Features:**

1. **Constraint solving:**
Associated instances are solved in the renamer, they don't affect the constraint solver.

2. **Instance Overlap:**
Associated instances follow the same overlap rules as normal instances. If multiple 
associated instances could match, the compiler gives an error between a manual and an automatic instance. 
The compiler should also suggest using ``hiding instance`` to resolve the duplication.
An {-# OVERLAPs/Overlapping/Incoherent/Overlappable #-} pragma on a ``Parent class`` also applies to all of its 
``Implicit Associated Instances``. 
``Explicit Associated Instances`` require have their own pragmas.

3. **Imports of associated class methods:**
Something like this is legal:
::
 
 import Data.Functor.Hierarchy (Monad((>>=)))

This allows for users to ``import Data.Functor.Hierarchy (Monad((>>=)))`` even though it's been moved to the 
``Bind`` class. If the ``Bind`` associated instance was deprecated, then users would get a warning on that import. 

4. **Exports of associated class methods:**
If an associated class is used only to provide default implementations,
then the module author may not want to export the associated class method.
e.g. 
:: 

 module Data.Functor.Hierarchy (Functor(fmap),Invariant(invmap),{- , everything else -}) where 
 -- NOTE: import Data.Functor.Hierarchy Functor(invmap) is no longer legal
 -- but import Data.Functor.Hierarchy (Invariant(invmap)) is legal
5. **Deriving declarations**
Deriving from a ``Parent class`` will also try to derive the associated instances.
However, this is legal: ``data MyType = MyType Int deriving (Big hiding instance (Small MyType,Small2 MyType))``.

Costs and Drawbacks
-------------------
This is a large change to the language, and has little precedent. 
However, the implementation is relatively straightforward, 
and the benefits are significant.
It is also easy to understand what it does. 
Since most of the costs are in the renamer, the implementation should be maintainable.

However, this is a large change, so the maintance burden is significant.

Backward Compatibility
----------------------
0 breaking changes are expected, ``instance`` is already a keyword
, and the new syntax is guarded by an extension.

Alternatives
------------
1. Do nothing
2. Proposal 597 is another option, but it requires the typechecker to help the renamer, 
doesn't work with fancy type level machinery. e.g.
::
 data Some c where Some :: c a => a -> Some c
 type SomeNum = Some Num
This doesn't work if we turn Num into a type synonym as you can't have an unreduced type synonym.
This proposal also doesn't work specify how it works with equality and implicit parameter constraints. 

3. The ``Class Aliases`` proposal which is similar
, but introduces more syntax, and focuses on making classes into synonyms, 
while this proposal allows them to be classes, just with associated instances.
I think this proposal focuses most on the renamer, which simplifies a lot of questions. 

4. After writing this I found `default superclass instances <https://gitlab.haskell.org/ghc/ghc/-/wikis/default-superclass-instances>`_,
which is nearly identical to this proposal (even the syntax), despite being written independently.
However, it proposes using lots of Associated Instances (e.g. for code generation), while this proposal focuses on 
getting rid of them and switching to normal instances (e.g. depreciation).

5. Class polymorphism in ``Associated Instance Declarations``
:: 

  class (a, b) => C2 '(a,b) where 
    instance a
    instance b

This allows for definitions like this (which is very similar to ``#597``):
::

  instance C2 '(Eq X, Ord X) where 
    compare a b = ...
    a == b = compare a b == EQ
    
But this significantly complicates the implementation, so I think not.


Unresolved Questions
--------------------
N/A right now 

Implementation Plan
-------------------
I cannot implement this plan. I do not have the time to, and I get sad each time I try to work on ghc.

Endorsements
-------------

(Optional) This section provides an opportunity for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.

Acknowledgments
---------------

(Optional) This section provides an opportunity to say thanks
to third parties for their contributions to the proposal.
