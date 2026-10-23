===================
Output Existentials
===================

.. author:: Viktor WW
.. date-accepted::
.. ticket-url:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/773>`_.
.. sectnum::
.. contents::

This proposal introduces output existentials into GHC

.. _`#473`: https://github.com/ghc-proposals/ghc-proposals/pull/473

Motivation
----------

Richly typed programming invariably uses its share of existential types, 
and this proposal makes it vastly easier to work with existentials. 

Currently, every existential must be encoded using its own datatype, which is laborious. 
Furthermore, packing and unpacking these datatypes must be done by hand, which is cluttersome.

We already could create existential types with ``ExistentialQuantification`` extension::

    data Box = forall a. MkBox a
  
Unfortunately we cannot unbox it inner type. ::

    fromBox :: Box -> ???
    fromBox (MkBox x)  = x

One of the great architectural achievements of Haskell language 
is using "input types" for ``forall`` quantifier.
::

    idS :: String -> String
	idS x = x

    idA :: forall a. a -> a
	idA @a x = x

This proposal propose similar feature: using construction ``therefore in`` as "output types" for ``exists`` quantifier.

Exists 3 kinds of Output Existentials, and this proposal covers all 3 cases:

- **Rigid Existentials**: special type which is known how to monotyped during a compile time
  and it is known during a compile time

  ::
  
    anytyped :: exists a. a
    anytyped = therefore @String in "Some small text"
	
  Rigid Existentials could be simplified to Ordinary types during a compile time.
  
- **Extractable Existentials**: special type which is could be checked during a compile time and 
  is known how to monotyped during a compile time but it is known during a runtime

  ::
  
    data E =  forall a. MkE [a]

    unE :: E -> exists a. [a]
    unE (MkE @e xs) = therefore @e in xs
	
  Extractable Existentials allow to get rid of permanent wrap-unwrap if it was existentially wrapped once.

- **Branched Existentials**: special type which is could be checked during a compile time but 
  is known how to monotyped during a runtime and it is known during a runtime

  ::
  
    data T a where
      MkInt  :: T Int
      MkBool :: T Bool

    serialise :: T a -> String
    serialise MkInt  = "int"
    serialise MkBool = "bool"

    deserialise :: String -> exists a. T a
    deserialise "int"  = therefore @Int  in MkInt
    deserialise "bool" = therefore @Bool in MkBool
	
  Branched Existentials allow to get rid of most of dynamic types.


Proposed Change Specification
-----------------------------

Extension
~~~~~~~~~

Introduce a new extension ``OutputExistentials``.

With ``OutputExistentials``, ``exists`` is a keyword in both types and terms.

With ``OutputExistentials``, ``therefore`` is a keyword in terms.

``OutputExistentials`` implies ``ExistentialQuantification``, ``TypeAbstractions`` and ``RankNTypes`` extensions.

Syntax
~~~~~~

Syntax of ``exists`` quantifier is similar to ``forall`` quantifier.
::

  exists a1 a2 ... an.

Syntax of ``therefore`` expression is similar to ``let`` expression.
::

  therefore @t1 @t2 ... @tn in expr 
  
This expression is a magical one which lifts a type from terms to types, 
which is the output type, which must be marked with ``exists`` quantifier.


Grammar
~~~~~~~

The grammar for quantifiers is modified as follows for GHC's parser

.. code:: abnf

  ctype ::= context '=>' ctype
        | quantifiers_telescope ctype   ;-- UPD!
        | ……

  quantifiers_telescope ::= forall_telescope exists_telescope ;-- NEW!

  ;-- just for comparison
  forall_telescope ::= 'forall' tv_bndrs '.'
                   | 'forall' tv_bndrs '->'
                   | ;-- empty

  ;-- NEW!
  exists_telescope ::= 'exists' tv_bndrs '.'
                   | ;-- empty


The grammar for ``therefore`` expression is added as follows for GHC's parser

.. code:: abnf

  aexp ::= qvar                   
        | 'therefore' '@' type1  {'@' typek} 'in' exp   ;-- NEW!
        | ……


Hierarchal Substitution
~~~~~~~~~~~~~~~~~~~~~~~

Right now we have next Hierarchal Substitution::

    (Int :: k) ⊆ ∀(a :: k).a

We extend Hierarchal Substitution to next::

    (Int :: k) ⊆ ∃(a :: k).a ⊆ ∀(b :: k).b

Update Type of Case
~~~~~~~~~~~~~~~~~~~

Right now we have ``case`` expression, which allow to have any ``n`` branches,
each of them has type ``tk1 tk2 ... tkn``.
We expect that every type in column ``tki ~ tmi`` is equal.
And it is an error if exist at least one pair such that ``tpi /~ tmi``

We extend this type.
If exist at least one type pair in column such that ``(tpi :: kpi) /~ (tmi :: kmi)``, 
but every kind  in column ``kpi ~ kmi`` is equal, the we change 
ordinary ``i``-column type to existential variable.


Core Language
~~~~~~~~~~~~~

We add one type to Types:

.. code:: abnf

    Type ::= n
       | Type1 → Type2
       | Type1 Type2
       | ∀ n. Type
       | ∃ n. Type     ;-- NEW!
       | ……

Existential type has same rules as Forall type.


Examples
--------

Filter Nat-Vectors
~~~~~~~~~~~~~~~~~~

We could use output existential types for Vectors ::

  data Nat = Zero | Succ Nat

  type Vec :: Nat -> Type -> Type
  data Vec n a where
    VNil :: Vec Zero a
    (:>) :: a -> Vec n a -> Vec (Succ n) a
  infixr 5 :>

  toVec :: forall a. [a] -> exists (n::Nat). Vec n a
  toVec []     = therefore @Zero     in VNil
  toVec (x:xs) = therefore @(Succ m) in x :> tvxs
     where 
       tvxs :: exists m. Vec m a
       tvxs = toVec xs

  filterVec :: forall a (n::Nat). (a -> Bool) -> Vec n a -> exists (m::Nat). Vec m a
  filterVec p VNil =   therefore @Zero     in VNil
  filterVec p (x :> xs) = if
        | p x       -> therefore @(Succ r) in x :> fpxs
        | otherwise -> therefore @r        in fpxs
     where 
       fpxs :: exists r. Vec r a
       fpxs = filterVec p xs


Constraints-existentials
~~~~~~~~~~~~~~~~~~~~~~~~

Constraints-existentials data ::

  data DocE = forall a. Show a => MkDoc a

  fromDocE :: DocE -> exists a. Show a => a
  fromDocE (MkDoc @a x) = therefore @a in x


Effect and Interactions
-----------------------

UnicodeSyntax
~~~~~~~~~~~~~

``∃`` (There Exists, U+2203) is added to ``UnicodeSyntax`` as synonym for ``exists`` keyword.

``∴`` (Therefore, U+2234) is added to ``UnicodeSyntax`` as synonym for ``therefore`` keyword.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of ``OutputExistentials`` has medium difficulty.


Backward Compatibility
----------------------

This proposal is backward compatible.


Alternatives
------------

Main alternative is "First-class existential types" `#473`_ 

Alternative names
~~~~~~~~~~~~~~~~~

Alternative names of ``exists`` quantifier are ``forany`` , ``forsome`` , ``fornew`` , ...

Alternative names of ``therefore`` expression are ``then`` , ``suchthat`` , ...


Unresolved Questions
--------------------

None at the moment.


Implementation Plan
-------------------

It is unclear. The author cannot implement this proposal.


Acknowledgments
---------------

This proposal is highly inspired by "First-class existential types" `#473`_ author Richard Eisenberg.

Endorsements
------------
