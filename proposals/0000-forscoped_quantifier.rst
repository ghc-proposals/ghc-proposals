====================
ForScoped Quantifier
====================

.. author:: Viktor WW
.. date-accepted::
.. ticket-url:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/709>`_.
.. sectnum::
.. contents::

.. _`#448`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0448-type-variable-scoping.rst


This proposal introduces ``forscoped`` local quantifier into GHC, which grab local type variables only

Motivation
----------

With GHC's powerful type-level programming features, we need powerful abilities to explicitly bring local type variables into scope. 

To write some signatures ``ScopedTypeVariables`` extension is needed, but it is still has ambiguity for each unquantified type variable - is it local scoped or forall type variable.

This proposal Introduce ``forscope`` quantifier, which explicitly require local scope only or error otherwise.


Proposed Change Specification
-----------------------------

ForScoped Quantifier "grab" local scope type variables only
::

  f :: forall a b. [a] -> [b] -> [(a, b)]
  f xs ys  = zip (xs :: forscoped a. [a]) yys
     where
       yys :: forscoped b. [bb]
       yys = reverse ys


``ScopedTypeVariables`` extension for all non-quantifiered type variables try to find local scope. If local scope is not found,``forall`` quantifier is used.

But for ``forscoped`` type variable ``ScopedTypeVariables`` extension must check for local scope only. If local scope is not found error must occur.

This make this quantifier more specific and less ambiguous which allows to catch more errors early.


Extension
~~~~~~~~~~~~

We introduce a new extension ``-XForScopedQuantifier`` which also implies ``ScopedTypeVariables`` extension.

With ``-XForScopedQuantifier``, ``forscoped`` becomes keyword in types.

Syntax
~~~~~~

Syntax for local quantifiers has a simple form.

::

  forscoped   (tyvar_i )+.

ForScoped quantifier is utilized by Haskell-renamer, so no changes require for Core-Language.


Examples
--------

Almost every example from  "Modern Scoped Type Variables" `#448`_ could be used with local quantifiers

Examples uses ForScoped Quantifier
::

  -- Example 1
  data T = forall a. MkT [a] (a -> Int)
			
  f :: T -> [Int]
  f (MkT @a xs f) = let mf :: forscoped a. [a] -> [Int]
                        mf = map f
                    in mf xs

  -- Example 2
  foo :: forall b. Maybe b -> ()
  foo @a (_ :: forscoped a. Maybe a) = ()

  -- Example 3
  bar :: forall b. Maybe b -> ()
  bar (Just @a (_ :: forscoped a. a)) = ()

  -- Example 4
  baz :: forall b. b ~ () -> ()
  baz @b () = ()
    where
      () :: forscoped b. b = ()

  -- Example 5
  f :: Maybe Int -> Int
  f (Nothing @a) = (4 :: forscoped a. a)
  f (Just @a _)  = (5 :: forscoped a. a)

  -- Example 6
  id :: forall a. a -> a
  id @t x = x :: forscoped t. t

  -- Example 7
  f1 :: forall a. [a] -> [a]
  f1 (x:xs) = xs ++ [ x :: forscoped  a. a ]
  
  -- Example 8
  f2 :: forall a. [a] -> [a]
  f2 (x:xs) = xs ++ [ x :: forscoped a. a ]

  -- Example 9
  f :: [a] -> [b] -> [(a, b)]  
  f xs ys = zip (xs :: forscoped a. [a]) yys 
     where
       yys :: forscoped b. [b]
       yys = reverse ys

  -- Example 10
  f :: forall a b c. [a] -> [b] -> c -> ....
  f xs ys z = .....
     where
       zzs :: forscoped c. [c]
       zzs = [z, z, z] 
       yys :: forscoped b. [b]
       yys = reverse ys
	   x2 :: forall d. d -> ....
	   x2 t = ...
	      where
		    x3 :: forscoped a. a
			x3 = head xs
			xt :: forscoped d a. (d, a)
			xt = (t, x3)
  
 
Effect and Interactions
-----------------------

None.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs is minimum difficulty.


Alternatives
------------

Alternative is to use more ambiguous absent of quantifier to type variable.


Backward Compatibility
----------------------

This proposal is fully backward compatible.


Unresolved Questions
--------------------

None is known.


Implementation Plan
-------------------

It is unclear.
