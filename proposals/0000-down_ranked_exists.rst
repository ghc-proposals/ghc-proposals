========================
Extractable Existentials
========================

.. author:: Viktor WW
.. date-accepted::
.. ticket-url:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/642>`_.
.. sectnum::
.. contents::

This proposal introduces down-ranked existentials into GHC

.. _`#473`: https://github.com/ghc-proposals/ghc-proposals/pull/473
.. _`#81`:  https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0081-forall-arrow.rst
.. _`#281`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst
.. _`#281rd`: https://ghc-proposals.readthedocs.io/en/latest/proposals/0281-visible-forall.html
.. _`#378`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst
.. _`#378rd`: https://ghc-proposals.readthedocs.io/en/latest/proposals/0378-dependent-type-design.html


Motivation
----------

Richly typed programming invariably uses its share of existential types, and this proposal makes it vastly easier to work with existentials. 

Currently, every existential must be encoded using its own datatype, which is laborious. Furthermore, packing and unpacking these datatypes must be done by hand, which is cluttersome.

Unboxible Existentials
~~~~~~~~~~~~~~~~~~~~~~

We already could create existential types with ``ExistentialQuantification`` extension::

  data Box = forall a. MkBox a
  
Unfortunately we cannot unbox it inner type. ::

  fromBox :: Box -> ???
  fromBox (MkBox x)  = x

Our goal is to change type system to be able to extract inner type of ``Box``. ::

  fromBox :: Box -> exists a. a
  fromBox (MkBox x)  = x

And we can do this with Extractable Existentials.

Proposed Change Specification
-----------------------------

Extension
~~~~~~~~~

Introduce a new extension ``-XExtractableExistential``.

With ``-XExtractableExistential``, ``exists`` is a keyword in both types and terms or at least pseudo-keyword.

``-XExtractableExistential`` implies ``ExistentialQuantification`` and ``RankNTypes`` extensions.

Syntax
~~~~~~

Syntax of ``exists`` quantifier has 3 forms: full, .

1. Full form
::

  exists a1 a2 a3, type1 type2 type3. some_inner_type

2. Repetitive Inferred form
::

  exists a1 a2 a3, a1 a2 a3. some_inner_type

3. Inferred-simple form
::

  exists a1 a2 a3. some_inner_type

Grammar
~~~~~~~

The grammar is modified as follows (baseline: GHC's parser)::

  ctype → quantifiers_telescope ctype   -- NEW!
        -- forall_telescope ctype       -- REMOVE!
        | context '=>' ctype
        | ...

  quantifiers_telescope → forall_telescope exists_telescope -- NEW!

  -- just for comparison
  forall_telescope → 'forall' tv_bndrs '.'
                   | 'forall' tv_bndrs '->'
                   | {- empty -}

  -- NEW!
  exists_telescope → 'exists' tv_bndrs '.'
                   | 'exists' tv_bndrs ',' ctypes '.'
                   | {- empty -}


Core Language
~~~~~~~~~~~~~

We add one type to Types:
::

  Type → n
       | Type1 → Type2
       | Type1 Type2
       | ∀ n. Type
       | ∃ n, Type1. Type2  -- NEW!
       | ...

We add Dissolve expression typing rule:
::

  Γ; ├`tm e : ∃α^κ, β^κ. τ
  ----------------------------
  Γ; ∆ ├`tm e : τ[ α^κ → β^κ ]

  --Tm DissolveExType

And we several lifting rules to Existential type:
::

  Γ ├`tm e : τ
  Γ ├`tm τ : k
  -------------------------
  Γ; ∆ ├`tm e : ∃α^κ, τ. α

  --Tm LiftTyEx

  Γ ├`tm e : τ1 τ2
  Γ ├`tm τ1 : k
  -----------------------------
  Γ; ∆ ├`tm e : ∃α^κ, τ1. α τ2

  --Tm LiftExTyApp1

  Γ ├`tm e : τ1 τ2
  Γ ├`tm τ2 : k
  -----------------------------
  Γ; ∆ ├`tm e : ∃α^κ, τ2. τ1 α

  --Tm LiftExTyApp2

  Γ ├`tm e : τ1 → τ2
  Γ ├`tm τ1 : k
  -------------------------------
  Γ; ∆ ├`tm e : ∃α^κ, τ1. α → τ2

  --Tm LiftExTyFun1

  Γ ├`tm e : τ1 → τ2
  Γ ├`tm τ2 : k
  -------------------------------
  Γ; ∆ ├`tm e : ∃α^κ, τ2. τ1 → α

  --Tm LiftExTyFun2

And we add rules for coerce and Constraints binders absolutely same as for ``∀ n. Type`` type.

Examples
--------

Filter Nat-Vectors
~~~~~~~~~~~~~~~~~~

We could use boxing/unboxing existential types for Vectors ::

  data Nat = Zero | Succ Nat

  type Vec :: Nat -> Type -> Type
  data Vec n a where
    VNil :: Vec Zero a
    (:>) :: a -> Vec n a -> Vec (Succ n) a
  infixr 5 :>

  data VecE a = forall n. MkVecE { unVecE :: Vec n a }

  vec2E :: forall a n. Vec n a -> VecE a
  vec2E = MkVecE

  vecEFrom :: forall a. VecE a -> exists m. Vec m a
  vecEFrom (MkVecE x) = x

  fromList :: forall a. [a] -> VecE a
  fromList []     = MkVecE VNil                
  fromList (x:xs) = MkVecE $ x :> unVecE $ fromList xs

  filter :: forall a n. (a -> Bool) -> Vec n a -> VecE a
  filter p VNil = MkVecE VNil
  filter p (x :> xs)
    | p x       = MkVecE $ x :> $ unVecE $ filter p xs
    | otherwise = filter p xs


Constraints-existentials
~~~~~~~~~~~~~~~~~~~~~~~~

Constraints-existentials data ::

  data DocE = forall a. Show a => MkDoc a

  fromDocE :: DocE -> exists a. Show a => a
  fromDocE (MkDoc x) = x


Effect and Interactions
-----------------------

UnicodeSyntax
~~~~~~~~~~~~~

``∃`` is added to ``UnicodeSyntax`` as synonym for ``exists`` keyword.


Visible ForAll
~~~~~~~~~~~~~~

Visible ForAll was added by `#81`_ and `#281`_ (rendered `#281rd`_ ).

1. It is useless to catch visible type variable (in arrow forall ``forall a ->`` ) by existential quantifier, even there is no requirement to forbid this, since type variable is already reachable in all (N-m)-Ranked levels.

2. It makes no sense to have visible existential quantifier (in arrow exists ``exists a ->`` ), even there is no requirement to forbid it.


UnErased ForAll
~~~~~~~~~~~~~~~

UnErased ForAll is accepted and could be added by `#378`_ (rendered `#378rd`_ ).

It is called Retained ForEach ``foreach a.`` and ``foreach a ->``

1. There is no limitations for existential quantifier for catch retained type variables.

2. It makes unclear if it has sense to have retained existential quantifier (aka ``forany a.`` ). But we expect, that ``exists`` preserve "erasing"/"unerasing" property of type variable.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of ``ExtractableExistential`` has medium difficulty.


Backward Compatibility
----------------------

This proposal is backward compatible.


Alternatives
------------

Main alternative is "First-class existential types" `#473`_ 

Alternative names
~~~~~~~~~~~~~~~~~

Alternative name of `exists` quantifier is ``forsome`` , ``forunique`` , ``forany`` , ``foralive`` , ``forone`` , ...


Unresolved Questions
--------------------

None at the moment.


Implementation Plan
-------------------

It is unclear.


Endorsements
------------

This proposal is highly inspired by `#473`_ author Richard Eisenberg.
