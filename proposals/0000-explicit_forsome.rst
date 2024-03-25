================
Explicit ForSome
================

.. author:: Viktor WW
.. date-accepted::
.. ticket-url:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/643>`_.
.. sectnum::
.. contents::

.. _`#448`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0448-type-variable-scoping.rst
.. _`#448rd`: https://ghc-proposals.readthedocs.io/en/latest/proposals/0448-type-variable-scoping.html

This proposal introduces same-ranked existentials into GHC

Motivation
----------

To write some trivial functions ``ScopedTypeVariables`` extension is needed, which add implicit rules how to read type signatures!

And this is a bit unhandy to use in such language which have a huge system of types.

This Proposal suggest to add the simple **SameRanked Existential** as **Explicit Forsome** Quantifier, which follows one of Haskell principles: the Explicit Variable Principle.

Alternative is "Modern Scoped Type Variables" `#448`_ (rendered `#448rd`_ ) which was added into ``ScopedTypeVariables`` extension.

It is *de facto* **Implicit Forsome**.

Rule
~~~~

- **SameRanked Existential** rule: Any N-Ranked *type* (or *type_variable* ) is ALSO N-Ranked ``forsome`` *type_variable* 
::

  --SameRanked Existentials:
  f1 :: forall a. [a] -> [a]
  f1 (x:xs) = xs ++ [ x :: forsome a. a ]

  f :: forall a. [a] -> [a]
  f xs = ys ++ ys
     where
       ys :: forsome a. [a]
       ys = reverse xs


if ∀a: f a then ∀a: ∃b, b ∈ a : f b

But since SameRanked Existential Quantifier ``forsome`` cannot use same symbol "∃" (which could be used by NotSameRanked Existential Quantifier ``exists`` ), we use a different one: "Ə" (Latin Capital Letter Schwa)

if ∀a: f a then ∀a: Əb, b ∈ a : f b


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
       ys :: forsome a. [a]    -- NEW!
       ys = reverse xs

By using ``forsome a`` we ask do not create a new type variable ``forall a``, but use already existed scoped version of type variable ``a``.

2. Local type synonym quantifier (with "ExtendedForsome")
::

  i42 :: Int
  i42 = 42

  i42 :: forsome a <- Int. a 
  i42 = 42


The support of this (2) role is discussed in Proposal "ExtendedForsome"! This proposal fully ignore this role for ``forsome`` quantifier.


Extension
~~~~~~~~~

Introduce a new extension -XExplicitForsome.

With ``-XExplicitForsome``, ``forsome`` is a keyword in both types and terms.

Even ``ScopedTypeVariables`` extension is an alternative to ``ExplicitForsome`` extension, they both could coexist together in same file.


Syntax
~~~~~~

Syntax of ``forsome`` quantifier has 1 simple form.

::

  forsome a1 a2 a3. 

It says that type variables ``a1, a2, a3`` are locally scoped ones, not a new ones. 


Grammar
~~~~~~~

1. The grammar is modified as follows (baseline: GHC's parser)::

        ctype → quantifiers_telescope ctype   -- NEW!
              -- forall_telescope ctype       -- REMOVE!
              | context '=>' ctype
              | ...

        -- + exists/foralive
        quantifiers_telescope → exists_telescope forsome_telescope forall_telescope forsome_telescope -- NEW!
		
        -- - exists/foralive
        quantifiers_telescope → forall_telescope forsome_telescope -- NEW!

        -- just for comparison
        forall_telescope → 'forall' tv_bndrs '.'
                         | 'forall' tv_bndrs '->'
                         | {- empty -}

        -- NEW!
        forsome_telescope → 'forsome' tv_bndrs '.'
                          | {- empty -}


Examples
--------

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


Effect and Interactions
-----------------------

UnicodeSyntax
~~~~~~~~~~~~~

The ``Ə`` (Ə, Latin Capital Letter Schwa, U+018F) is added to ``UnicodeSyntax`` as synonym for ``forsome`` keyword.

Why Ə (Latin Capital Letter Schwa, U+018F)?  

1. Reason of using: Historically Schwa Letter is rarely used. ( *Small* letter "ə" is used as the schwa sound in International Phonetic Alphabet (IPA).) In nowadays Symbol Ə has rare and limited use, so it is free to use here

2. Reason of representation: Symbol Ə a bit similar to ∃ Symbol which reflects meaning of existential

3. Reason of supporting: Ə(U+018F) was added in 1.1 (June 1993) Unicode Version, same version were were added ∀(For All, U+2200) and ∃(There Exists, U+2203)

5. Reason of clearness: Symbol Ə is clear and easy distinguishable from numbers and Latin letters (and from many non-Latin too)


Visible ForAll and UnErased ForAll
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. There is no limitations for forsome quantifier for catch retained type variables or visible type variables.

2. Even there no requirement to forbid to use retained forsome quantifier (aka ``for_one a.`` ) it makes no sense to have it.

3. Even there no requirement to forbid to use visible forsome quantifier (in arrow forsome ``forsome a ->`` ) it makes no sense to have it.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of ``ExplicitForsome`` is minor and has minimum difficulty.


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
