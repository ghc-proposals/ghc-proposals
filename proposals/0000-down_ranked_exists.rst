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

Currently, every existential must be encoded using its own datatype, which is laborious. Furthermore, packing and unpacking these datatypes must be done by hand, which is cluttersome.

UpRanked Existentials
~~~~~~~~~~~~~~~~~~~~~

Alternative Proposal is "First-class existential types" `#473`_ .

#473 suggests to add *de facto* **UpRanked Existential**

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

DownRanked Existentials
~~~~~~~~~~~~~~~~~~~~~~~

This Proposal suggest to add the opposite (of #473) : **DownRanked Existential**

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

Unboxible Existentials
~~~~~~~~~~~~~~~~~~~~~~

We already could create existential types with ``ExistentialQuantification`` extension::

  data Box = forall a. MkBox a
  
Let we have value of ``Either a b`` type and we wish to unbox it. ::

  fromEither :: forall a b. Either a b -> ???
  fromEither (Left x)  = x
  fromEither (Right y) = y

We have several ways to do this, but we choose to "absorb" types with existential ``Box`` type ::

  fromEither :: forall a b. Either a b -> Box
  fromEither (Left x)  = MkBox x
  fromEither (Right y) = MkBox y
  
Our goal is to change ``Box`` type to be unboxible, but preserve "absorption rule". ::

  data exists a. Box a = forall a. MkBox a

  fromEither :: forall a b. Either a b -> exists c. Box c
  fromEither (Left x)  = MkBox x
  fromEither (Right y) = MkBox y

  fromBox :: exists a. Box a -> a
  fromBox (MkBox x) = x

And we can do this with DownRanked Existentials.


Proposed Change Specification
-----------------------------

Main and second rules give us unique proprieties of DownRanked existentials.

Roles
~~~~~

1. Boxing type variable into a Data declaration 
::

  -- hidden conventional existential GHC type
  data Box = forall a. MkBox a

  -- open existential type
  data exists a. Ex a = forall a. MkEx { unEx :: a }  -- NEW!

Main and second rules say that in open existential type we capture on N-Rank **same** *type variable* which escaped from (N+1 Ranked) ``forall`` (or ``exists`` )

So, left ``exists a`` is a capture (aka "visible") of a right ``forall a`` in definition of ``exists a. Ex a`` data.

2. Extractor / unboxing / escaping from Data-constructor 
::

  fromBox :: Box -> ???
  fromBox (MkBox x) = x       -- Error!

  fromEx :: exists a. Ex a -> a
  fromEx (MkEx x) = x         -- OK! NEW!

  fromEx2 :: exists a. Ex a -> a
  fromEx2 = unEx              -- OK! NEW!

Main and second rules guarantee us that unboxing give us same type as boxing or it is ⊥ bottom type (for phantom or partly phantom existentials)

Any type ``@t`` ∈ ``forall a. a`` (including ``@t1`` , ``@t2`` , ``@t3`` ... ) and we expect, that also escaped ``exists b. b`` ∈ ``forall a. a`` .

But exists only one type ``@tm`` ∈ ``exists b. b`` .

3. Direct capture type variable by Data-constructor
::

  toBox :: forall a. a -> Box
  toBox = MkBox

  toEx :: forall a. a -> exists a. Ex a
  toEx = MkEx

4. Absorption (indirect capture) different types into one inner type
::

  fromEither :: forall a b. Either a b -> exists c. Ex c
  fromEither (Left  x) = MkEx x
  fromEither (Right y) = MkEx y

Absorption happens when we do not care what we absorb follow ``RankNTypes`` rules.

5. Existential Boundaries are the same as a escaper type variable boundaries 
::

  data Doc = forall a. Show a => MkDoc a
  
  data exists a. Show a => DocE a = forall a. Show a => MkDocE a  -- NEW!


6. Direct Non-data capture of type variable and extracting (maybe as future possibility)
::

  upd :: forall a. exists b. a -> (forall b. b -> b) -> b


Extension
~~~~~~~~~

Introduce a new extension ``-XDownRankedExistential``.

With ``-XDownRankedExistential``, ``exists`` is a keyword in both types and terms or at least pseudo-keyword.

``-XDownRankedExistential`` implies ``ExistentialQuantification`` and ``RankNTypes`` extensions.

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

  data exists n. VecE n a = forall n. MkVecE { unVecE :: Vec n a }

  vec2E :: forall a n. Vec n a -> exists n. VecE n a
  vec2E = MkVecE

  vecEFrom :: forall a. exists m. VecE m a -> Vec m a
  vecEFrom (MkVecE x) = x

  fromList :: forall a. [a] -> exists n. VecE n a
  fromList []     = MkVecE VNil                
  fromList (x:xs) = MkVecE $ x :> unVecE $ fromList xs

  filter :: forall a n. (a -> Bool) -> Vec n a -> exists m. VecE m a
  filter p VNil = MkVecE VNil
  filter p (x :> xs)
    | p x       = MkVecE $ x :> $ unVecE $ filter p xs
    | otherwise = filter p xs


Phantom-existentials
~~~~~~~~~~~~~~~~~~~~

Phantom-existentials data ::

  -- Phantom-existential Type
  data exists a. UnitE a = MkUnit

Partly Phantom-existential ::

  -- Partly Phantom-existential Type
  data exists a. MaybyE a = forall a. JustE a | NothingE

Even we could create phantom existentials, the use of them is unclear.

Hidden-existentials
~~~~~~~~~~~~~~~~~~~

Hidden-existentials are existentials, which we could not catch directly ::

  -- hidden conventional existential GHC type
  data Box = forall a. MkBox a
  
  -- Partly Phantom-existential / Partly Hidden-existentials
  data exists a. ExLeftEither a = forall a. MkExLeft a | forall b. MkExRight b

  -- Partly Phantom-existential Type / Partly Hidden-existentials
  data exists a. ListE = forall a. exists b. Con a (ListE b) | Nil


Poly-existentials
~~~~~~~~~~~~~~~~~

Poly-existentials data ::

  -- Sum-Type existential
  data exists a b. ExEither a b = forall a. MkExLeft a | forall b. MkExRight b

  -- Head, next-to-Head existential
  -- we catch `b` twice and not from `forall`, but from `exists`
  data exists a b. L2 a b = forall a. exists b c. Con a (L2 b c) | Nil

  -- Head-next-next existential
  data exists a b c. L3 a b c = forall a. exists b c d. Con a (L3 b c d) | Nil  

Poly-existentials could have an ambiguity existential-errors :: 

  -- ERROR! Which `a` we catch? From MkExBAD1 or MkExBAD2 ?
  data exists a. ExBAD a = forall a. MkExBAD1 a | forall a. ExBAD2 a

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

Non-data existentials is an optional for implementation or we could remain it as future possibility.


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


GADTs
~~~~~

GADTs require "sub-type" each of existential variables catch no more then one quantifier !

Example ::

  data Foo b e where
    MkFoo :: forall a. a -> (a -> Bool)   -> exists a. Foo Bool a -- Ok
  
    --MkBar :: forall b. b -> (b -> Bool) -> exists b. Foo Bool b -- Error! "Foo Bool a" is already "exists a."
    --MkBar :: forall a. a -> (a -> Bool) -> exists a. Foo Bool a -- Error! same type variable name as in MkFoo
    MkBar :: forall b. b -> (b -> Bool)   -> exists a. Foo Bool a -- Ok
    
    MkBaz :: Int                          -> exists a. Foo Bool a -- Ok

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

Alternative names
~~~~~~~~~~~~~~~~~

Alternative name of `exists` quantifier is ``forsome`` , ``forunique`` , ``forany`` , ``foralive`` ...


Unresolved Questions
--------------------

Equality constraints
~~~~~~~~~~~~~~~~~~~~

Existential types could use equality constraints ::

  --vec2E :: forall a n. Vec n a -> exists m. VecE m a
  vec2E :: forall a n. Vec n a -> exists m. m ~ n => VecE m a
  vec2E = MkVecE

But some existential types also require in many cases "polymorphic types" equality constraints ::

  data exists a. Ex a = forall a. MkEx a

  fromEither :: forall a b. Either a b -> exists c. Ex c
  fromEither (Left  x) = MkEx x
  fromEither (Right y) = MkEx y
  
  fromEither :: forall a b. Either a b -> exists c. c ~ ??? => Ex c -- How to write it ?
  
What us to do if we wish to add a "probabilistic" type? "Polymorphic types" consists none, one or more ``|`` (or alternatively ``\/`` ) ::

  fromEither :: forall a b. Either a b -> exists c. c <~ a |  b => Ex c
  
  fromEitherInt :: forall a. Either a Int -> exists c. c <~ Int | a => Ex c
  fromEitherInt = fromEither

Polymorphic types follow next 2 rules for type equality:

- Union rule: ``a | a ~ a``

- Commutativity rule: ``a | b ~ b | a``

- Transitivity rule: ``c ~ a | b, a <~ c, b <~ c``

But not every equality constraints we could write. And not all of them we could check ::

  --fromList :: forall a. [a] -> exists n. n ~ Nat => VecE n a
  fromList :: forall a. [a] -> 
              exists n. n <~ Zero | ???? => VecE n a    -- How to write it ?
  fromList []     = vec2E VNil                
  fromList (x:xs) = vec2E $ x :> vecEFrom $ fromList xs

  --filter :: forall a n. (a -> Bool) -> Vec n a -> exists m. VecE m a
  filter :: forall a n. (a -> Bool) -> 
            Vec n a -> 
            exists m. Succ m <~ n | Succ n => VecE m a  -- How to check it ?
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
