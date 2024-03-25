================
Extended Forsome
================

.. author:: Viktor WW
.. date-accepted::
.. ticket-url:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/646>`_.
.. sectnum::
.. contents::


This proposal extends same-ranked existentials ``forsome`` in GHC

Motivation
----------

ForSome Quantifier as SameRanked Existential Quantifier could be added with limited abilities with Proposal ``Explicit ForSome``. 

This proposal extends abilities for ``forsome`` to be used fully.

Rule
~~~~

- **SameRanked Existential** rule: Any N-Ranked *type* (or *type_variable* ) is ALSO N-Ranked ``forsome`` *type_variable* 
::

  --SameRanked Existentials:
  id1 :: forall a. a -> a
  id1 x = x

  -- same as 
  id1 :: forall a. forsome b <- a. a -> b
  
  
  f1 :: forall a. [a] -> [a]
  f1 (x:xs) = xs ++ [ x :: forsome a. a ]


  i42 :: Int
  i42 = 42

  -- same as
  i42 :: forsome a <- Int. a
  i42 = 42


*if* ∀a: f a *then* ∀a: Əb, b ∈ a : f b


Proposed Change Specification
-----------------------------

SameRanked Existential Quantifier ``forsome`` could play 2 roles.

Roles
~~~~~

1. Local scope quantifier 
::

  f :: forall a. [a] -> [a]
  f xs = ys ++ ys
     where
       ys :: forsome a. [a]
       ys = reverse xs

The support of this (1) role is discussed in Proposal "Explicit ForSome"!

2. Local type synonym quantifier
::

  i42 :: Int
  i42 = 42

  i42 :: forsome a <- Int. a  -- NEW!
  i42 = 42

It is similar to expression `:: let ... in` at type level. ForSome quantifier do not create a new type variable ( ``forall a.`` ), but add local type synonym in signature.

It is not a powerful feature, but it fully cover all ``forsome`` roles  and possibilities


Extension
~~~~~~~~~

This Proposal extends ``ExplicitForsome`` extension.


Syntax
~~~~~~

We extend syntax for ``forsome`` quantifier from simple form to full form. And we add 2 additional sugaring forms. Such, that simple one form become a sugared version of full form. 

All valid old code with ``forsome`` quantifier remains valid.

1. The **Full form** is 
::

  forsome a1 <- tb1, a2 <- tb2, a3 <- tb3.
  
  -- a bit similar to
  let a1 = tb1 in let a2 = tn2 in let a3 = tb3 in ...

Where `` <- ...`` is a binding part of quantifier.

2. Sugared **Same-name form**. If for some ``N`` we have same names ``aN == tbN`` then we could omit to write this specific binding of type variable. 
::

  forsome a1 a2 a3. 

  -- desugars into
  forsome a1 <- a1, a2 <- a2, a3 <- a3.


  -- partial same-name
  forsome a1 <- tb1, a2 a3.

  -- desugars into
  forsome a1 <- tb1, a2 <- a2, a3 <- a3.


Main difference between bindings from ``forsome`` and ``exists`` (aka ``foralive`` ) quantifiers is that ``exists`` binds type variable only, but ``forsome`` could binds even types.

Grammar
~~~~~~~

1. The grammar is modified as follows (baseline: GHC's parser)::

        -- NEW!
        forsome_telescope → 'forsome' tv_unbnd_fs '.'  -- NEW!
                          -- | 'forsome' tv_bndrs '.'  -- REMOVE!
                          | {- empty -}

        -- NEW!  
        tv_unbnd_fs → tv_bndr tv_unbnd_fs
                    | tv_bndr '<-' ctype tv_bind_nxt_fs
                    | {- empty -}
					
        tv_bind_nxt_fs → ',' tv_unbnd_fs
                       | {- empty -}

Examples
--------

Sometimes is handy to replace some long type with ``forsome`` type variable  (with "ExtendedForsome")
::

  data IIRState = 
    forsome uf <- {-# UNPACK #-} Float. 
    MkIIRState
    { x0 :: uf
    , x1 :: uf
    , x2 :: uf
    , y0 :: uf
    , y1 :: uf
    , y2 :: uf
    }
    deriving (Show)	

Or simplify some long type signature
::

  applyBasicAuth :: forsome b <- ByteString, r <- Request. b -> b -> r -> r
  
Or make the opposite
::

  id1 :: forall a. forsome b <- a. a -> b
  
  id2 :: forall a. forsome b <- a. b -> b
  id2 @b x = x


Effect and Interactions
-----------------------

None at this time.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs is minor and has minimum difficulty.


Backward Compatibility
----------------------

This proposal is fully backward compatible.


Alternatives
------------

Main alternative is type synonyms.


Unresolved Questions
--------------------

None at this time.


Implementation Plan
-------------------

It is unclear.

