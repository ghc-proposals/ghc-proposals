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

To write some signatures for ``ScopedTypeVariables`` extension is needed (or ``TypeAbstractions`` in some cases). But it still has ambiguity for each unquantified type variable - it could be either a local scoped type variable or a universal quantified type variable.

This proposal introduces ``forscoped`` quantifier, which explicitly requires local scope only or throws an error otherwise.


Proposed Change Specification
-----------------------------

ForScoped Quantifier "grabs" (use) local scope type variables only
::

  f :: forall a b. [a] -> [b] -> [(a, b)]
  f xs ys  = zip (xs :: forscoped a. [a]) yys
     where
       yys :: forscoped b. [b]
       yys = reverse ys


``ScopedTypeVariables`` extension for all non-quantifiered type variables try to find local scope. If local scope is not found, universally ``forall`` quantifier is used.

But for ``forscoped`` type variable ``ScopedTypeVariables`` extension must check for local scope only. If the local scope is not found, the error must occur.

Using this quantifier with type variables makes signatures more specific and less ambiguous, which allows for catching more errors early.


Extension
~~~~~~~~~~~~

We introduce a new extension ``-XForScopedQuantifier`` which also implies ``ScopedTypeVariables`` and ``TypeAbstractions`` extensions.

The ``forscoped`` becomes keyword in types with ``-XForScopedQuantifier`` extesion.

Syntax
~~~~~~

The syntax for local quantifiers has a simple form.

::

  type ::= ......
       | 'forscoped' { tyvar } tyvar '.'

ForScoped quantifier is utilized by Haskell-renamer, so no changes are required for Core-Language.


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

UnicodeSyntax
~~~~~~~~~~~~~

We wish to preserve ``∃`` (There Exists, U+2203) symbol for universal existential quantifier, so it is proposed to add 2 symbols ``∃?`` (``∃`` + ``?``) to represent ``forscoped`` quantifier.
::

  -- Example 1
  foo :: ∀ b. Maybe b -> ()
  foo @a (_ :: ∃? a. Maybe a) = ()

  -- Example 2
  f1 :: ∀ a. [a] -> [a]
  f1 (x:xs) = xs ++ [ x :: ∃? a. a ]

  -- Example 3
  f2 :: ∀ a. [a] -> [a]
  f2 xs = ys ++ ys
     where
       ys :: ∃? a. [a]
       ys = reverse xs

Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs has minimum difficulty.


Alternatives
------------

An alternative is to use a more ambiguous absence of a quantifier to type's variable..

Alternative keywords
~~~~~~~~~~~~~~~~~~~~

We could choose differen keywords instead of proposed latin ``forscoped`` and unicode ``∃?`` keywords.

For example to have ``forlocal`` or/and ``∃ℒ`` ((There Exists, U+2203, Mathematical Operators) ++ (Script Capital L, U+2112, Letterlike Symbols)).

Backward Compatibility
----------------------

This proposal is fully backward compatible.


Unresolved Questions
--------------------

None is known.


Implementation Plan
-------------------

It is unclear.
