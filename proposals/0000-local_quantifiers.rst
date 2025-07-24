====================
Local Quantifiers
====================

.. author:: Viktor WW
.. date-accepted::
.. ticket-url:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/710>`_.
.. sectnum::
.. contents::

.. _`#448`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0448-type-variable-scoping.rst


This proposal introduces local quantifier into GHC, which grab local type variables

Motivation
----------

With GHC's powerful type-level programming features, we need powerful abilities to explicitly bring local type variables into scope. 

To write some trivial functions ``ScopedTypeVariables`` extension is needed, which adds implicit rules how to read type signatures!

And this is a bit unhandy to use implicit only rules in language which has a huge system of types.

This Proposal suggest to add the simple **ForThis Quantifier**, **ForThat Quantifier**, **ForSame Quantifier** and **ForUsed Quantifier**, **ForInner Quantifier**, **ForNested Quantifier** which allows to write explicitly type signatures, which depends from internal or external type variables.

Explicitness is preferential in Haskell over implicitness. And this Proposal propose how to write local quantifiers explicitly!

Just like ``ExplicitForall`` extension allow explicitly say exactly what this specific type variable is ``forall`` quantified, this Proposal allow to switch on ``ExplicitScoped`` extension explicitly say exactly what this specific type variable is local quantified!
 
Main alternative is "Modern Scoped Type Variables" `#448`_ which was added into ``ScopedTypeVariables`` extension.

``ScopedTypeVariables`` is *de facto* **Implicit Forunique** : Implicit rules to add a local scope (or universal) quantifier to type variables if they are not explicitly quantified.

Also Alternative is ``PartialTypeSignatures`` extension, with opposite philosophy: compiler infer type not for holes.


Rule (aka math-like proof)
~~~~~~~~~~~~~~~~~~~~~~~~~~

De facto Local Quantifiers are a special case of Existential Quantifier, which is known during compile time.

**Math-like Proof:**
::

  -- pseudo-haskell
  
  f1 :: ∀ a. [a] -> [a]
  f1 (x:xs) = xs ++ [ x :: ∃₌₁ b. b ]

  f :: ∀ a. [a] -> [a]
  f xs = ys ++ ys
     where
       ys :: ∃₌₁ b. [b]
       ys = reverse xs


Main benefit is that all local quantifiers are utilized by Haskell-renamer, so nothing is required to change in Core-language.

Local Quantifiers are just explanation to GHC which external type variable they means.


Proposed Change Specification
-----------------------------

Local Quantifiers "grab" type variables external to this signature
::

  f :: forall a b. [a] -> [b] -> [(a, b)]
  f @aa @bb xs ys  = zip (xs :: forthis aa. [aa]) yys
     where
       yys :: forused _ b. [b]
       yys = reverse ys


By using ``for{local} a`` we ask do not create a new type variable ``forall a``, but use already existed external type variable ``a``.

1. ForThis ``forthis`` quantifier pick type variable **by name** lifted from argument **type-term**, not from **type**.

2. ForThat ``forthat`` quantifier pick type variable **by name** from ``class`` and ``instance`` head type variable.

3. ForSame ``forsame`` quantifier pick type variable **by name** from explicit signature declaration.

4. ForUsed ``forused`` quantifier pick type variable **by position** from ``forall`` in signature declarations in one-to-one corresponded order.

5. ForInner ``forinner`` quantifier pick type variable **by position** from inner ``forall`` from Existential types and GADS-like types in one-to-one corresponded order.

6. ForNested ``fornested`` quantifier pick type variable **by position** from signature high-ranked ``forall`` argument in one-to-one corresponded order.

Since ``forthis`` , ``forthat`` , ``forsame`` are quantifier by picking by name, they must use same **name** for type variable as external ones.

Since ``forused`` , ``forinner`` , ``fornested`` are quantifier by picking by position, they must cold use **name** for type variable different from external ones.

Local quantifier's type variable "grabs" type variables only from nearest parent signature.

Local quantifier's type variable could "grabs" type variables only from nearest *grand*-parent signature - then we use coma ``,`` for this (except ``forthat`` quantifier).

Local quantifier's type variable could "grabs" type variables only from nearest *grand-..-grand*-parent signature - then we use additional comas ``,`` for this.

Local quantifier's which pick type variable *by position*:

1. Could use pure wildcard ``_`` for unused variable. This means use ``for{local} _ _ a.`` instead of ``for{local} unused1 unused2 a.``

2. Could omit all unused variables "righter" then last used one. This means use ``for{local} a.`` instead of ``for{local} a _ _ _.``

3. Could omit all variables if all of them are unused from some signature. This means use ``for{local} ,, a.`` instead of ``for{local} _ _ _, _ _, a.``

4. ``forused`` must use semicolon ``;`` for shifted ``forall`` to omit *term* argument. This means write ``forused _ ; b.`` if signature is ``f :: forall a. a -> forall b. b -> ...``

5. ``forinner`` and ``fornested`` must use semicolon ``;`` for shifted ``forall`` to omit *type* argument. This means write ``for{local} _ ; b.`` if signature is ``f :: forall a. a -> forall b. T b -> ...``

Since ForSame Quantifier uses explicit signature declaration only and ignore shifted ``forall`` , we mark ``forsame`` as **DEPRECATED**. But this quantifier is useful for future of ``ScopedTypeVariables`` extension


Extension
~~~~~~~~~~~~

Introduce a new extension ``-XLocalQuantifiers`` .

With ``-XLocalQuantifiers`` words ``forthis``, ``forthat``, ``forsame``, ``forused``, ``forinner``, ``fornested`` becomes keywords in types.

Syntax
~~~~~~

Syntax for local quantifiers has a simple form.

::

  forthat   (tyvar_i )+
  
  forthis   (, |tyvar_i )* tyvar
  
  forsame   (, |tyvar_i )* tyvar     -- DEPRECATED

  forused   (; |, |tyvar_i )* tyvar

  forinner  (; |, |tyvar_i )* tyvar
  
  fornested (; |, |tyvar_i )* tyvar



Examples
--------

Almost every example from  "Modern Scoped Type Variables" `#448`_ could be used with local quantifiers

ForThis Quantifier
~~~~~~~~~~~~~~~~~~

Examples uses ForThis Quantifier
::

  -- Example 1
  data T = forall a. MkT [a] (a -> Int)
			
  f :: T -> [Int]
  f (MkT @a xs f) = let mf :: forthis a. [a] -> [Int]
                        mf = map f
                    in mf xs

  -- Example 2
  foo :: forall b. Maybe b -> ()
  foo @a (_ :: forthis a. Maybe a) = ()

  -- Example 3
  bar :: forall b. Maybe b -> ()
  bar (Just @a (_ :: forthis a. a)) = ()

  -- Example 4
  baz :: forall b. b ~ () -> ()
  baz @b () = ()
    where
      () :: forthis b. b = ()
	  
  -- Example 5
  data T a where
    MkT1 :: forall a.              T a
    MkT2 :: forall a.              T (a,a)
    MkT3 :: forall a b.            T a
    MkT4 :: forall a b. b ~ Int => T a
    MkT5 :: forall a b c. b ~ c => T a

  foo :: T (Int, Int) -> ()
  foo (MkT1 @(Int,Int))  = ()
  foo (MkT2 @x)          = (() :: forthis x. x ~ Int => ())
  foo (MkT3 @_ @x)       = (() :: forthis x. x ~ x => ())
  foo (MkT4 @_ @x)       = (() :: forthis x. x ~ Int => ())

  -- Example 6
  f :: Maybe Int -> Int
  f (Nothing @a) = (4 :: forthis a. a)
  f (Just @a _)  = (5 :: forthis a. a)
  
  -- Example 6
  g :: forall a. a -> a
  g @a x = (x :: forthis a. a)

  -- Example 7  
  f8 @a (x :: forthis a. a) = x    -- accepted

  f2 @a True  x (y :: forthis a. a) = x
  f2 @_ False x y                   = y   -- accepted

  f3 @a True  x (y :: forthis a. a) = x
  f3    False x y                   = y   -- rejected: too confusing to have different type variable bindings

  f4 :: Bool -> a -> a -> a
  f4 @a True  x (y :: forthis a. a) = x
  f4    False x y                   = y   -- accepted: the type signature allows us to do this

  f5 :: Bool -> forall a. a -> a -> a
  f5 True @a x (y :: forthis a. a) = x
  f5 False   x y                   = y    -- accepted
  
  -- Example 8
  id :: forall a. a -> a
  id @t x = x :: forthis t. t


ForThat Quantifier
~~~~~~~~~~~~~~~~~~

Examples uses ForThat Quantifier
::

  -- Example 1
  class C a where
    foo :: forthat a. forall b. b -> a -> (a, [b])

  -- Example 2
  class Trans t where
    lift :: forthat t. forall m. Monad m => m a -> (t m) a
	
  -- Example 3
  class C a where
    op :: forthat a. [a] -> a
  
    op xs = let ys:: forthat a. [a]
                ys = reverse xs
            in
            head ys
			
  -- Example 4
  instance C b => C [b] where
    op xs = reverse (head (xs :: forthat b. [[b]]))

  -- Example 5	
  class D a where
    m :: forthat a. a -> a

  instance Num a => D [a] where
    m :: forthat a. [a] -> [a]
    m x = map (*2) x
	
  -- Example 6
  class Collects e ce | ce -> e where
    empty  :: forthat e. ce
    insert :: forthat e ce. e -> ce -> ce
    member :: forthat e ce. e -> ce -> Bool


Example uses both ForThat and ForThis Quantifiers:
::

  type C :: forall i. (i -> i -> i) -> Constraint
  class C @i a where
    p :: forthat a. forthis i. P a i

ForInner Quantifier
~~~~~~~~~~~~~~~~~~~~~~~~

Examples uses ForInner Quantifier
::

  -- Example 1
  type Foo = forall b. [b] -> [b]

  f3 :: Foo
  f3 (x:xs) = xs ++ [ x :: forinner b. b ]
  
  -- Example 2
  data T = forall a. MkT [a]

  k :: T -> T
  k (MkT [t :: forinner a. a]) =
      MkT t3
    where
      (t3 :: forinner a. [a]) = [t,t,t]
	  
  -- Example 3
  data T = forall a. MkT [a] (a -> Int)

  f :: T -> [Int]
  f (MkT (xs :: forinner a. [a]) f) = let mf :: forinner a. [a] -> [Int]
                              mf = map f
                          in mf xs
  
  -- Example 4
  bar :: forall b. Maybe b -> ()
  bar (Just @a (_ :: forinner b. b)) = ()

  -- Example 5
  f :: Maybe Int -> Int
  f Nothing   = (4 :: forinner a. a)
  f (Just _)  = (5 :: forinner a. a)

ForNested Quantifier
~~~~~~~~~~~~~~~~~~~~~~~~

Nested local variables are not part of Modern Local Scope Variables, but was a part of previous Old Local Scope Variables.

Examples uses ForInner Quantifier.
::

  -- Example 1
  type family F a

  higherRankF :: (forall a. F a -> F a) -> ...

  usage = higherRankF (\ (x :: fornested a. F a) -> ...)
  
  
ForUsed Quantifier
~~~~~~~~~~~~~~~~~~~~~~~~

Examples uses ForUsed Quantifier
::

  -- Example 1
  f1 :: forall a. [a] -> [a]
  f1 (x:xs) = xs ++ [ x :: forused a. a ]   -- OK

  f = runST ( (op >>= \(x :: forused s. STRef s Int) -> g x) :: forall s. ST s Bool )

  g (x:: forused a. a) = x
  
  -- Example 2
  f1 :: forall a. [a] -> [a]
  f1 (x:xs) = xs ++ [ x :: forused b. b ]

  -- Example 3
  f :: [a] -> [b] -> [(a, b)]      -- no explicit forall: we could use forused with a, b
  f xs ys = zip (xs :: forused a. [a]) yys 
     where
       yys :: forused _ b. [b]
       yys = reverse ys

  -- Example 4
  f :: forall a b c. [a] -> [b] -> c -> ....
  f xs ys z = .....
    where
      zzs :: forused _ _ c. [c]
      zzs = [z, z, z] 
      yys :: forused _ b. [b]
      yys = reverse ys
      x2 :: forall a. a -> ....
      x2 t = ...
        where
          x3 :: forused , a. a
          x3 = head xs
          xt :: forused a2, a1. (a2, a1)
          xt = (t, x3)
   
  -- Example 5
  f2 :: forall a. a -> forall b. [b] -> [b]
  f2 _ (x:xs) = xs ++ [ x :: forused _ ; b. b ]


ForSame Quantifier
~~~~~~~~~~~~~~~~~~~~~~~~

Examples uses ForSame Quantifier
::

  -- Example 1
  f1 :: forall a. [a] -> [a]
  f1 (x:xs) = xs ++ [ x :: forsame  a. a ]
  
  -- Example 2
  f2 :: forall a. [a] -> [a]
  f2 (x:xs) = xs ++ [ x :: forsame a. a ]

  -- Example 3
  f :: [a] -> [b] -> [(a, b)]  
  f xs ys = zip (xs :: forsame a. [a]) yys 
     where
       yys :: forsame b. [b]
       yys = reverse ys

  -- Example 4
  f :: forall a b c. [a] -> [b] -> c -> ....
  f xs ys z = .....
    where
      zzs :: forsame c. [c]
      zzs = [z, z, z] 
      yys :: forsame b. [b]
      yys = reverse ys
      x2 :: forall d. d -> ....
      x2 t = ...
        where
          x3 :: forsame , a. a
          x3 = head xs
          xt :: forsame d, a. (d, a)
          xt = (t, x3)
  
 
Effect and Interactions
-----------------------

ScopedTypeVariables
~~~~~~~~~~~~~~~~~~~~~~~~

``ScopedTypeVariables`` extension ignores local quantified variables.

But we could reuse part of searching algorithms from ``ScopedTypeVariables`` algorithms.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of ``ForuniqueQuantifier`` is medium difficulty.


Alternatives
------------

Main alternative is "Modern Scoped Type Variables" `#448`_ (``ScopedTypeVariables`` extension), but also ``TypeAbstractions`` and ``PartialTypeSignatures``.


Backward Compatibility
----------------------

This proposal is fully backward compatible.


Unresolved Questions
--------------------

It is unclear which local quantifier should be used in next example
::

    data Proxy a = P

    g2 :: Proxy (Nothing @(a, a)) -> ()
    g2 (P @(Nothing :: for??? t. Maybe (t, t))) = ()


We could reuse ``forthat`` for this case or to add a new special quantifier ``fornew``.

Or maybe ``forused t.`` is enough.


Future possibilities
--------------------

ScopedTypeVariables
~~~~~~~~~~~~~~~~~~~~~~~~

In future we could rewrite ``ScopedTypeVariables`` extension in terms of local-scoped quantifiers.

Type in terms
~~~~~~~~~~~~~~~~~~

Right now Haskell doesn't support types in terms. If it is allowed, we could add ``forterm`` local quantifier.
::

  t = Int
  foo (x :: forterm t. t) = 0

Implementation Plan
-------------------

It is unclear.
