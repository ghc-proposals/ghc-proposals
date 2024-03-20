Explicit ForSome
==========================

.. author:: Viktor WW
.. date-accepted::
.. ticket-url:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/643>`_.
.. sectnum::
.. contents::

This proposal introduces same-ranked existentials into GHC


.. _`#448`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0448-type-variable-scoping.rst
.. _`#448rd`: https://ghc-proposals.readthedocs.io/en/latest/proposals/0448-type-variable-scoping.html



Motivation
----------

Richly typed programming invariably uses its share of existential types, and this proposal makes it vastly easier to work with existentials.

Alternative is "Modern Scoped Type Variables" `#448`_ (rendered `#448rd`_ ) and it was added into ``ScopedTypeVariables`` extension.

It is *de facto* **Implicit Forsome**.

This Proposal suggest to add the simple **SameRanked Existential** as **Explicit Forsome**, which follows one pf Haskell principle: the Explicit Variable Principle.

- **SameRankedRanked Existential** rule: Any N-Ranked *type* (or *type_variable* ) is ALSO N-Ranked ``forsome`` *type_variable* 
::

  --SameRanked Existentials:
  id1 :: forall a. a -> a
  id1 x = x

  -- same as (1-variable sugaring version)
  id1 :: forall a. forsome b <- a. a -> b

  -- same as (full version)
  id1 :: forall a. forsome b | b <- a. a -> b
  
  
  --(same-name sugaring version)
  f1 :: forall a. [a] -> [a]
  f1 (x:xs) = xs ++ [ x :: forsome a. a ]


  i42 :: Int
  i42 = 42

  -- same as (1-variable sugaring version)
  i42 :: forsome a <- Int. a
  i42 = 42


if ∀a: f a then ∀a: ∃b, b ∈ a : f b

But since SameRankedRanked Existential Quantifier ``forsome`` cannot use same symbol "∃", we use a different one: "Ə"

if ∀a: f a then ∀a: Əb, b ∈ a : f b


Proposed Change Specification
-----------------------------

Roles
~~~~~

2. Local scope quantifier 
::

  f :: forall a. [a] -> [a]
  f xs = ys ++ ys
     where
       ys :: forsome a. [a]    -- NEW!
       ys = reverse xs

2. Local type synonym quantifier 
::

  i42 :: Int
  i42 = 42

  i42 :: forsome a <- Int. a  -- NEW!
  i42 = 42


Extension
~~~~~~~~~

Introduce a new extension -XExplicitForsome.

1. Introduce a new extension ``-XExplicitForsome``.

#. With ``-XExplicitForsome``, ``forsome`` is a keyword in both types and terms.

#. With ``-XExplicitForsome``, introduce a new type for forsome existentials.

Even ``ScopedTypeVariables`` extension is an alternative to ``ExplicitForsome`` extension, they both could coesist in same file.

Syntax
~~~~~~

Syntax of ``exists`` quantifier has 3 forms.

1. The **Full form** is 
::

  forsome a1 a2 a3 | a1 <- tb1, a2 <- tb2, a3 <- tb3.

Where ``| ... <- ...`` is a binding part.

2. Sugared **Same-name form**. If for some ``N`` we have same names ``aN == tbN`` then we could omit to write this specific binding of type variable. 
::

  forsome a1 a2 a3. 

  -- desugars into
  forsome a1 a2 a3 | a1 <- a1, a2 <- a2, a3 <- a3.


  -- partial same-name
  forsome a1 a2 a3 | a1 <- tb1.

  -- desugars into
  forsome a1 a2 a3 | a1 <- tb1, a2 <- a2, a3 <- a3.

3. Sugared **One-variable form**. If ``forsome`` has just one variable we could write binding shortly. 
::

  forsome a1 <- tb1.

  -- desugars into
  forsome a1 | a1 <- tb1.


Main difference between bindings from ``forsome`` and ``exists`` quantifiers is that ``exists`` binds type variable only, but ``forsome`` could binds even types.

Grammar
~~~~~~~

1. The grammar is modified as follows (baseline: GHC's parser)::

        ctype → quantifiers_telescope ctype   -- NEW!
              -- forall_telescope ctype       -- REMOVE!
              | context '=>' ctype
              | ...

        -- + exists
		quantifiers_telescope → exists_telescope forsome_telescope forall_telescope forsome_telescope -- NEW!
		
		-- - exists
		quantifiers_telescope → forall_telescope forsome_telescope -- NEW!

        -- just for comparison
        forall_telescope → 'forall' tv_bndrs '.'
                         | 'forall' tv_bndrs '->'
                         | {- empty -}

        -- NEW!
        forsome_telescope → 'forsome' tv_bind_1fs '.'
                          | 'forsome' tv_bndrs_ex '.'
                          | {- empty -}

        tv_bndrs_fs → tv_bndr tv_bndrs tv_bind_fs
                    | {- empty -}

        tv_bind_fs → '|' tv_bind tv_bind_fs_next
                   | {- empty -}

        tv_bind_fs_next → ',' tv_bind_1fs tv_bind_fs_next
                        | {- empty -}
  
        tv_bind_1fs → tv_bndr '<-' ctype



Examples
--------

Local scope quantifier
~~~~~~~~~~~~~~~~~~~~~~

Almost every example from  "Modern Scoped Type Variables" `#448`_ (rendered `#448rd`_ ) could be used with ``forsome``
::

  f1 :: forall a. [a] -> [a]
  f1 (x:xs) = xs ++ [ x :: forsome a. a ]   -- OK

  f = runST ( (op >>= \(x :: forsome s. STRef s Int) -> g x) :: forall s. ST s Bool )

  g (x:: forsome a. a) = x


  data T = forall a. MkT [a] (a -> Int)

  f :: T -> [Int]
  f (MkT (xs :: forsome a. [a]) f) = 
                        let mf :: forsome a. [a] -> [Int]
                            mf = map f
                        in mf xs


  class C a where
    op :: [a] -> a

    op xs = let ys:: forsome a. [a]
                ys = reverse xs
            in
            head ys
		  
  instance C b => C [b] where
    op xs = reverse (head (xs :: forsome b. [[b]]))

Local type synonym quantifier
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sometimes is handy to replace some long type with ``forsome`` type variable  
::

  data IIRState = 
    forsome uf | uf <- {-# UNPACK #-} Float. 
    MkIIRState
    { x0 :: uf
    , x1 :: uf
    , x2 :: uf
    , y0 :: uf
    , y1 :: uf
    , y2 :: uf
    }
    deriving (Show)	


Effect and Interactions
-----------------------

UnicodeSyntax
~~~~~~~~~~~~~

``Ə`` (Latin Capital Letter Schwa, U+018F) is added to ``UnicodeSyntax`` as synonym for ``forsome`` keyword.

Why Ə (Latin Capital Letter Schwa, U+018F)? Historically Schwa Letter is rarely used. Small letter "ə" is used as the schwa sound in International Phonetic Alphabet (IPA).

1. Reason of using: In nowadays Symbol Ə has rare and limited use, so it is free to use here

2. Reason of representation: Symbol Ə a bit similar to ∃ Symbol which reflects meaning of existential

3. Reason of supporting: Ə(U+018F) was added in 1.1 (June 1993) Unicode Version, same version were were added ∀(For All, U+2200) and ∃(There Exists, U+2203)

5. Reason of clearness: Symbol Ə is clear and easy distinguishable from numbers and Latin letters (and from many non-Latin too)


Visible ForAll and UnErased ForAll
~~~~~~~~~~~~~~

1. There is no limitations for forsome quantifier for catch retained type variables or visible type variables.

2. Even there no requirement to forbid to use retained forsome quantifier (aka ``for_one a.`` ) it makes no sense to have it.

3. Even there no requirement to forbid to use visible forsome quantifier (in arrow forsome ``forsome a ->`` ) it makes no sense to have it.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of ``DownRankedExistential`` is minor and has minimum difficulty.


Backward Compatibility
----------------------

This proposal is fully backward compatible.


Alternatives
------------

Main alternative is "Modern Scoped Type Variables" `#448`_ (``ScopedTypeVariables`` extension)


Unresolved Questions
--------------------

None at this time.


Implementation Plan
-------------------

It is unclear.

