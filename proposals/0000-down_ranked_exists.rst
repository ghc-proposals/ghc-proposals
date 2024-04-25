=======================
DownRanked Existentials
=======================

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

Alternative Proposal is "First-class existential types" `#473`_ .

It suggests to add *de facto* **UpRanked Existential**

- **UpRanked Existential** rule: Any N-Ranked ``forall`` *type_variable* is INSTEAD (N+1)-Ranked ``exists`` *type_variable* 
::

  --UpRanked Existentials:
  cmpg :: forall a. a -> a ->(forall b. b -> b -> Bool) -> Bool
  cmpg x y f = f x y

  -- same as
  cmpg :: forall a. a -> a ->((exists b. b -> b) -> Bool) -> Bool

  -- same as
  cmpg :: (exists a. a -> a ->((exists b. b -> b) -> Bool)) -> Bool

Unfortunately, working with High-Ranked Types is complicated in Haskell and it's Core.

This Proposal suggest to add the opposite: **DownRanked Existential**  

- **DownRanked Existential** rule: Any N-Ranked ``forall`` (or ``exists``) *type_variable* is ALSO (N-1)-Ranked ``exists`` *type_variable* 
::

  --DownRanked Existentials:
  cmpg2 :: forall a. a -> a ->(forall b. b -> b -> (b, Bool)) -> (a, Bool)
  cmpg2 x y f = f x y

  -- same as
  cmpg2 :: forall a. exists b. a -> a ->(forall b. b -> b -> (b, Bool)) -> (b, Bool)


The Main rule: *if* ∀a: f a *then* ∃b: ∀a, a ≡ b : f b

Second rule: *if* ∃a: f a *then* ∃b: ∃a, a ≡ b : f b

This is the core idea of this Dependent existential type from Higher-Ranked (Exactly N+1 Ranked) ``forall`` / ``exists`` type variables with `a ≡ b` equality condition.

*Note: using same keyword "exists" for both UpRanked and DownRanked Existential Quantifiers is incompatible and inconsistent idea*


Proposed Change Specification
-----------------------------

Roles
~~~~~

1. Direct-catch a Data-quantifier 
::

  -- hidden conventional existential GHC type
  data Box = forall a. MkBox a

  -- open existential type
  data exists a. Ex = forall a. MkEx { unEx :: a }  -- NEW!

2. Extractor from Data-quantifier 
::

  fromBox :: Box -> ???
  fromBox (MkBox x) = x       -- Error!

  fromEx :: exists a. Ex -> a
  fromEx (MkEx x) = x         -- OK! NEW!

  fromEx2 :: exists a. Ex -> a
  fromEx2 = unEx              -- OK! NEW!


3. Indirect Data-existential 
::

  toBox :: forall a. a -> Box
  toBox = MkBox

  toEx :: forall a. a -> exists a. Ex
  toEx = MkEx

4. Absorption different types into one inner type
::

  fromEither :: forall a b. Either a b -> exists c. Ex
  fromEither (Left  x) = MkEx x
  fromEither (Right y) = MkEx y

5. Direct Non-data catch and extract 
::

  upd :: forall a. exists b. a ->(forall b. b -> b) -> b

6. Existential Boundaries are the same as a escaper type variable boundaries 
::

  data Doc = forall a. Show a => MkDoc a
  
  data exists a. Show a => DocE = forall a. Show a => MkDocE a  -- NEW!


Extension
~~~~~~~~~

Introduce a new extension -XDownRankedExistential.

1. Introduce a new extension ``-XDownRankedExistential``.

#. With ``-XDownRankedExistential``, ``exists`` is a keyword in both types and terms or at least pseudo-keyword.

#. With ``-XDownRankedExistential``, introduce a new type for existentials.


Syntax
~~~~~~

Syntax of ``exists`` quantifier has 1 simple form.
::

  exists a1 a2 a3. 

It says that type variables a1, a2, a3 are from N+1 Ranked ``forall`` / ``exists`` , not a new ones.


Grammar
~~~~~~~

1. The grammar is modified as follows (baseline: GHC's parser)::

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
                         | {- empty -}


2. The grammar is modified for ``data`` declaration too.


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

  data exists n. VecE a = forall n. MkVecE { unVecE :: Vec n a }

  vec2E :: forall a n. Vec n a -> exists n. VecE a
  vec2E = MkVecE

  vecEFrom :: forall a. exists m. VecE a -> Vec m a
  vecEFrom (MkVecE x) = x

  fromList :: forall a. [a] -> exists n. VecE a
  fromList []     = MkVecE VNil                
  fromList (x:xs) = MkVecE $ x :> unVecE $ fromList xs

  filter :: forall a n. (a -> Bool) -> Vec n a -> exists m. VecE a
  filter p VNil = MkVecE VNil
  filter p (x :> xs)
    | p x       = MkVecE $ x :> $ unVecE $ filter p xs
    | otherwise = filter p xs  


Phantom-existentials
~~~~~~~~~~~~~~~~~~~~

Phantom-existentials data ::

  -- Phantom-existential Type
  data exists a. UnitE = MkUnit

Partly Phantom-existential ::

  -- Partly Phantom-existential Type
  data exists a. MaybyE = forall a. JustE a | NothingE

Hidden-existentials
~~~~~~~~~~~~~~~~~~~

Hidden-existentials are existentials, which we could not catch directly ::

  -- hidden conventional existential GHC type
  data Box = forall a. MkBox a
  
  -- Partly Phantom-existential / Partly Hidden-existentials
  data exists a. ExLeftEither = forall a. MkExLeft a | forall b. MkExRight b

  -- Partly Phantom-existential Type / Partly Hidden-existentials
  data exists a. ListE = forall a. Con a (exists b. ListE) | Nil


Poly-existentials
~~~~~~~~~~~~~~~~~

Poly-existentials data ::

  -- Sum-Type existential
  data exists a b. ExEither = forall a. MkExLeft a | forall b. MkExRight b

  -- Head, next-to-Head existential
  -- we catch `b` twice and not from `forall`, but from `exists`
  data exists a b. L2 = forall a. exists b. Con a (exists b c. L2) | Nil

  -- Head-next-next existential
  data exists a b c. L3 = forall a. exists b c. Con a (exists b c d. L3) | Nil  

Poly-existentials could have an ambiguity existential-errors :: 

  -- ERROR! Which `a` we catch? From MkExBAD1 or MkExBAD2 ?
  data exists a. ExBAD = forall a. MkExBAD1 a | forall a. ExBAD2 a

Non-data existential
~~~~~~~~~~~~~~~~~~~~~~

Non-data existential is a bit tricky ::

  mk :: Bool -> exists a. (forall a. (a, a -> Int))
  -- or more specific with Equality Constrains
  -- mk :: Bool -> exists a. a ~ Int | Bool => (forall a. a ~ Int | Bool => (a, a -> Int))
  mk True  = (5, id)
  mk False = (False, \ b -> if b then 1 else 0)

  example = (let x = mk True in snd x) (fst (mk True)) -- error
  
  example = let x = mk True in (snd x) (fst x)         -- Ok


Effect and Interactions
-----------------------

UnicodeSyntax
~~~~~~~~~~~~~

``∃`` is added to ``UnicodeSyntax`` as synonym for ``exists`` keyword.


Visible ForAll
~~~~~~~~~~~~~~

Visible ForAll was added by `#81`_ and `#281`_ (rendered `#281rd`_ ).

1. Even there no requirement to forbid to use existential quantifier for catch visible type variable (in arrow forall ``forall a ->`` ), since type variable is already reachable in all (N-m)-Ranked levels it is useless to catch it by existential quantifier.

2. Even there no requirement to forbid to use visible existential quantifier (in arrow exists ``exists a ->`` ) it makes no sense to have it.


UnErased ForAll
~~~~~~~~~~~~~~~

UnErased ForAll is accepted and could be added by `#378`_ (rendered `#378rd`_ ).

It is called Retained ForEach ``foreach a.`` and ``foreach a ->``

1. There is no limitations for existential quantifier for catch retained type variables.

2. Even there no requirement to forbid to use retained existential quantifier (aka ``forany a.`` ) it makes no sense to have it.


GADTs
~~~~~

GADTs require 
- to catch existential type variable on same Rank as quantifier! 

- "sub-type" must consist same amount of existential variables!

- "sub-type" each of existential variables catch no more then one quantifier !

Example ::

  data Foo b where
    MkFoo :: forall a. a -> (a -> Bool)   -> exists a. Foo Bool -- Ok
  
    --MkBar :: forall b. b -> (b -> Bool) -> exists b. Foo Bool -- Error! "Foo Bool" is already "exists a."
    MkBar :: forall b. b -> (b -> Bool)   -> exists a. Foo Bool -- Ok
  
    MkYaz :: forall c. c                  -> exists c. Foo Char -- Ok! "Foo Char" is not "Foo Bool" nor "Foo Int"
  
    --MkBaz :: Bool         -> Foo Bool -- Error! "Foo Bool" is already "exists a."
    MkBaz :: Bool -> exists a. Foo Bool -- Ok!
  
    MkYan :: Int            -> Foo Int  -- Ok! "Foo Int" is neither "Foo Bool" nor "Foo Char"


Type Families
~~~~~~~~~~~~~

Type Families require same catching rules for existential as GADTs.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of ``DownRankedExistential`` has medium difficulty.

**Drawbacks**: using same keyword ``exists`` for both UpRanked and DownRanked quantifiers is **incompatible** and **inconsistent**.


Modifying `#473`_ Proposal if both Proposal are Accepted
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Proposal `#473`_ requires to use same quantifier ``exists`` and we suggest to modify it, if both #473 and this Proposals are Accepted.

This proposal suggest to change ``exists`` keyword for `#473`_ (if it will be approved) into ``forany`` (or other).

And change "∃" Unicode symbol into "∋" (or other)!

This proposal also suggest to rename proposed in `#473`_ (if it will be approved) ``ExistentialTypes`` extension into ``UpRankedExistential`` or ``ForanyQuantification`` (or other).


Temporary solutions and Testing
+++++++++++++++++++++++++++++++

But as **temporary** solutions and *testing* this proposal DownRanked Existentials could use ``foralive`` keyword for ForAlive quantifier and "∋" Unicode symbol. 


Backward Compatibility
----------------------

This proposal is backward compatible.


Alternatives
------------

Main alternative is "First-class existential types" `#473`_ 


Unresolved Questions
--------------------

Equality constraints
~~~~~~~~~~~~~~~~~~~~

Existential types could use equality constraints ::

  --vec2E :: forall a n. Vec n a -> exists m. VecE a
  vec2E :: forall a n. Vec n a -> exists m. m ~ n => VecE a
  vec2E = MkVecE

But some existential types also require in many cases "polymorphic types" equality constraints ::

  data exists a. Ex = forall a. MkEx a

  fromEither :: forall a b. Either a b -> exists c. Ex
  fromEither (Left  x) = MkEx x
  fromEither (Right y) = MkEx y
  
  fromEither :: forall a b. Either a b -> exists c. c ~ ??? => Ex -- How to write it ?
  
What us to do if we wish to add a "probabilistic" type? "Polymorphic types" consists none, one or more ``|`` (or alternatively ``\/`` ) ::

  fromEither :: forall a b. Either a b -> exists c. c ~ a |  b => Ex
  
  fromEitherInt :: forall a. Either a Int -> exists c. c ~ Int | a => Ex
  fromEitherInt = fromEither

Polymorphic types follow next 2 rules for type equality:

- Union rule: ``a | a ~ a``

- Commutativity rule: ``a | b ~ b | a``

- Transitivity rule: ``c ~ a | b, a ~ c, b ~ c``

But not every equality constraints we could write. And not all of them we could check ::

  --fromList :: forall a. [a] -> exists n. n ~ Nat => VecE a
  fromList :: forall a. [a] -> 
              exists n. n ~ Zero | ???? => VecE a   -- How to write it ?
  fromList []     = vec2E VNil                
  fromList (x:xs) = vec2E $ x :> vecEFrom $ fromList xs

  --filter :: forall a n. (a -> Bool) -> Vec n a -> exists m. VecE a
  filter :: forall a n. (a -> Bool) -> 
            Vec n a -> 
            exists m. Succ m ~ n | Succ n => VecE a   -- How to check it ?
  filter p VNil = vec2E VNil
  filter p (x :> xs)
    | p x       = vec2E $ x :> $ vecEFrom $ filter p xs
    | otherwise = filter p xs  


Implementation Plan
-------------------

It is unclear.


Endorsements
------------

This proposal is highly inspired by `#473`_ author Richard Eisenberg.
