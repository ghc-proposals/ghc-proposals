====================
ForAlone Quantifier
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


This proposal introduces ``foralone`` unique quantifier into GHC, which grabs local type and term variables.

Motivation
----------

With GHC's powerful type-level programming features, we need powerful abilities 
to explicitly bring local type variables into the scope. 

To write some trivial functions ``ScopedTypeVariables`` extension is needed (or ``TypeAbstractions``), 
which adds implicit rules for how to read type signatures!

And this is a bit unhandy to use implicit-only rules in a language that has a huge system of types.

Right now there are three kinds of scoped:

* The scope local to each type signature
::

  -- 'a' is introduced and used only within this type signature
  res :: a -> a
  --res :: forall a. a -> a
  res x = x

* The lexical scope around a type signature (modified by ``TypeAbstractions``)
::

  {-# LANGUAGE TypeAbstractions #-}
  const :: forall b. b -> c -> b
  const @a x = res where
    -- uses 'a' from the lexical scope '@a'
    res :: a
  --res :: foralone %term a. a
    res = x

* The magical / borrowed scope introduced by ``ScopedTypeVariables``
::

  {-# LANGUAGE ScopedTypeVariables #-}
  const :: forall a. a -> b -> a
  const x = res where
    -- uses 'a' from parent signature 'const :: forall a.'
    res :: a
  --res :: foralone %type a. a
    res = x

Notice how the signature ``res :: a`` in (2) and (3) 
does not itself say where ``a`` comes from.

This is confusing because traditionally Haskell has made it optional 
to write "forall" in a signature, so it is unclear if ``res :: a`` means ``res :: forall a. a`` 
or the other thing (and it's not even possible to express what 
that the other meaning is currently!).

This proposal says such uses of a should explicitly say they use '``a``' 
from somewhere else (exactly which-where) in the program.

This Proposal suggests to add the **ForAlone Quantifier** with 3 local clarifications: 
by term, by type and by argument.
ForAlone quantifier allows to write explicitly type signatures, 
which depends from internal or external type variables.

Explicitness is preferential in Haskell over implicitness. 
And this Proposal propose how to write local quantifiers explicitly! 
It does not aim to allow writing more programs, just to allow being more explicit 
about where type variables come from. 
Non-quantified type variable means that this variable is somehow-quantified.

Just like ``ExplicitForall`` extension allow explicitly say exactly what 
this specific type variable is ``forall`` quantified, 
this Proposal allow to switch on ``ForAloneQuantifier`` extension explicitly say 
exactly what this specific type variable is local and uniquely quantified!

An additional advantage is that adding such quantifiers makes signatures 
that have one-to-one correspondences with pure mathematical descriptions in Predicate Logic!
 
Main alternative is "Modern Scoped Type Variables" `#448`_ which was added into 
``ScopedTypeVariables`` extension and ``TypeAbstractions`` extension.

``ScopedTypeVariables`` and partly ``TypeAbstractions`` are *de facto* **Implicit Local Quantifiers** : 
Implicit rules to add a local scope (or universal) quantifier to type variables 
if they are not explicitly quantified.

Also Alternative is ``PartialTypeSignatures`` extension, 
with opposite philosophy: compiler infer type not for holes.


Rule (aka math-like proof)
~~~~~~~~~~~~~~~~~~~~~~~~~~

De facto local ForAlone Quantifier is a special case of Existential Quantifier 
(Existential Unique Quantifier), which is known during compile time.

Some people have doubt that this Proposal use correct theoretical term names.

See more details in "Unresolved Questions" section.

Author of this proposal use "Duck typing logic":

- if it looks like "quantifier" then it is a "quantifier"

- if it looks like "existential quantifier" then it is an "existential quantifier"

- if it looks like "unique quantifier" then it is an "unique quantifier"

- if it looks like "local quantifier" (which binds local type variables) then it is a "local quantifier"

**Math-like Proof:**

All local scoped and parametric non-quantified type variables in Haskell 
are **uniquely** quantified (if not ``forall``-quantified) type variables.

::

  -- pseudo-haskell
  
  f1 :: ∀ a. [a] -> [a]
  f1 (x:xs) = xs ++ [ x :: ∃! b. b ]

  f :: ∀ a. [a] -> [a]
  f xs = ys ++ ys
     where
       ys :: ∃! b. [b]
       ys = reverse xs

If we use mathematical induction we could show that all "similar" cases could use unique quantifier.

Main benefit is that local ForAlone quantifiers are utilized by Haskell-renamer,
so nothing is required to change in Core-language.

Local ForAlone Quantifiers are just explanation to GHC which external type variable they means: 
they indicates the binding site of the type variable (e.g. whether it was bound by a type abstraction, 
a scoped type variable bound in a type signature, or somewhere else).


Proposed Change Specification
-----------------------------

Local ForAlone Quantifier "grab"(use) already existed type variables external to this signature
::

  f :: forall a b. [a] -> [b] -> [(a, b)]
  f @aa @bb xs ys  = zip (xs :: foralone %term aa. [aa]) yys
     where
       yys :: foralone %type b. [b]
       yys = reverse ys


By using ``foralone %<local> a`` quantifier we ask do not create a new type variable ``forall a``, 
but use already existed external type variable ``a``.

1. Modifier ``%term`` says to pick type variable **by name** 
   lifted from nearest explicit **type-term** argument 
   (full or partial either ``@tyterm`` or ``(type a)`` or ``a`` in place which 
   is responsible from ``forall ->`` quantifier), not from **type**.

2. Modifier ``%type`` says to pick ``forall`` type variable **by name** 
   from explicit only signature declaration from nearest sibling ones, 
   then from parent one ans so on, except siblings of top declaration signatures.

3. Modifier ``%arg`` says to pick type variable **by name** 
   from ``class``, ``instance``, ``data``, ``type`` and ``newtype`` head type variable.
   
   In ``data``, ``type`` and ``newtype`` signatures is also allowed to write "old way" - 
   with ``forall`` quantifier without warning. Order of type variables has meaning.

   In ``class`` and ``instance`` signatures it is the only allowed quantifier - 
   Order of type variables hasn't any meaning. 
   But for future Backward Compatibility it is better to write first with head declaration order.


Since ``foralone %how a %how b %how c %how d.`` is a quantifier by picking by name, 
they must use same **name** for type variable as external ones.

Extension
~~~~~~~~~~~~

Introduce a new extension ``ForAloneQuantifier`` .

With ``ForAloneQuantifier`` word ``foralone`` becomes a keyword in types.

Syntax
~~~~~~

Syntax for foralone quantifier has a simple form.

.. code:: abnf

  quantifiers ::= { quantifier }

  quantifier  ::=
    | 'forall'   { tyvar }           tyvar          ( '.' | '->' )
    | 'foralone' { modifier tyvar }  modifier tyvar   '.'

where ``%argm``, ``%term`` and ``%type`` are modifiers.


With ``-XModifiers``, introduce modifier syntax on forall type variables if we don't want to mix quantifiers

.. code:: abnf

  quantifier ::= ......
       | 'forall' { modifiers tyvar } modifiers tyvar ( '.' | '->' )


where we could use 2 modifiers: ``%alone`` + local one ``%arg``, ``%term`` or ``%type`` modifiers.


Every unique quantifier is utilized by the Haskell renamer, so no changes are required for the Core Language.

Examples
--------

Local Term clarification
~~~~~~~~~~~~~~~~~~~~~~~~

Examples uses ForAone Quantifier with Term clarification
::

  -- Example 1
  data T = forall a. MkT [a] (a -> Int)
			
  f :: T -> [Int]
  f (MkT @a xs f) = let mf :: foralone %term a. [a] -> [Int]
                        mf = map f
                    in mf xs

  -- Example 2
  foo :: forall b. Maybe b -> ()
  foo @a (_ :: foralone %term a. Maybe a) = ()

  -- Example 3
  bar :: forall b. Maybe b -> ()
  bar (Just @a (_ :: foralone %term a. a)) = ()

  -- Example 4
  baz :: forall c. c ~ () -> ()
  baz @b () = ()
    where
      () :: foralone %term b. b = ()
	  
  -- Example 5
  data T a where
    MkT1 :: forall a.              T a
    MkT2 :: forall a.              T (a,a)
    MkT3 :: forall a b.            T a
    MkT4 :: forall a b. b ~ Int => T a
    MkT5 :: forall a b c. b ~ c => T a

  foo :: T (Int, Int) -> ()
  foo (MkT1 @(Int,Int))  = ()
  foo (MkT2 @x)          = (() :: foralone %term x. x ~ Int => ())
  foo (MkT3 @_ @x)       = (() :: foralone %term x. x ~ x => ())
  foo (MkT4 @_ @x)       = (() :: foralone %term x. x ~ Int => ())

  -- Example 6
  f :: Maybe Int -> Int
  f (Nothing @a) = (4 :: foralone %term a. a)
  f (Just @a _)  = (5 :: foralone %term a. a)
  
  -- Example 6
  g :: forall a. a -> a
  g @a x = (x :: foralone %term a. a)

  -- Example 7  
  
  -- accepted
  f8 @a (x :: foralone %term a. a) = x 

  -- accepted
  f2 @a True  x (y :: foralone %term a. a) = x
  f2 @_ False x y                   = y

  -- rejected: too confusing to have different type variable bindings
  f3 @a True  x (y :: foralone %term a. a) = x
  f3    False x y                   = y

  -- accepted: the type signature allows us to do this
  f4 :: Bool -> a -> a -> a
  f4 @a True  x (y :: foralone %term a. a) = x
  f4    False x y                   = y

  -- accepted
  f5 :: Bool -> forall a. a -> a -> a
  f5 True @a x (y :: foralone %term a. a) = x
  f5 False   x y                   = y
  
  -- Example 8
  id :: forall a. a -> a
  id @t x = x :: foralone %term t. t

Local Type clarification
~~~~~~~~~~~~~~~~~~~~~~~~

Examples uses ForAlone Quantifier with Type clarification
::

  -- Example 1
  f1 :: forall a. [a] -> [a]
  f1 (x:xs) = xs ++ [ x :: foralone %type  a. a ]
  
  -- Example 2
  f2 :: forall a. [a] -> [a]
  f2 (x:xs) = xs ++ [ x :: foralone %type a. a ]

  -- Example 3
  f :: [a] -> [b] -> [(a, b)]  
  f xs ys = zip (xs :: foralone %type a. [a]) yys 
     where
       yys :: foralone %type b. [b]
       yys = reverse ys

  -- Example 4
  f :: forall a b c. [a] -> [b] -> c -> ....
  f xs ys z = .....
    where
      zzs :: foralone %type c. [c]
      zzs = [z, z, z] 
      yys :: foralone %type b. [b]
      yys = reverse ys
      x2 :: forall d. d -> ....
      x2 t = ...
        where
          x3 :: for1 %type a. a
          x3 = head xs
          xt :: foralone %type a %type d. (d, a)
          xt = (t, x3)

Local Arg clarification
~~~~~~~~~~~~~~~~~~~~~~~~

Examples uses ForAlone Quantifier with Arg clarification
::

  -- Example 1
  class C a where
    foo :: foralone %arg a. forall b. b -> a -> (a, [b])

  -- Example 2
  class Trans t where
    lift :: foralone %arg t. forall m. Monad m => m a -> (t m) a
	
  -- Example 3
  class C a where
    op :: foralone %arg a. [a] -> a
  
    op xs = let ys:: for1 %arg a. [a]
                ys = reverse xs
            in
            head ys
			
  -- Example 4
  instance C b => C [b] where
    op xs = reverse (head (xs :: foralone %arg b. [[b]]))

  -- Example 5	
  class D a where
    m :: foralone %arg a. a -> a

  instance Num a => D [a] where
    m :: foralone %arg a. [a] -> [a]
    m x = map (*2) x
	
  -- Example 6
  class Collects e ce | ce -> e where
    empty  :: foralone %arg ce. ce
    insert :: foralone %arg e ce. e -> ce -> ce
    member :: foralone %arg e ce. e -> ce -> Bool


Example uses both Arg and Term Clarifications:
::

  type C :: forall i. (i -> i -> i) -> Constraint
  class C @i a where
    p :: foralone %arg a %term i. P a i
  
New alternative way to write data declarations:
::

  -- Example 1
  data T a where
    MkT1 :: foralone %arg a.                        T a
    MkT2 :: foralone %arg a.                        T (a,a)
    MkT3 :: foralone %arg a. forall b.              T a
    MkT4 :: foralone %arg a. forall b. b ~ Int =>   T a
    -- with Modifiers extension
    MkT5 :: forall c %alone %arg a b. b ~ c =>   T a


Effect and Interactions
-----------------------

This proposals affect a lot of extensions, but mostly with "natural" way.

UnicodeSyntax
~~~~~~~~~~~~~~

We wish to preserve ``∃`` (There Exists, U+2203) symbol for universal existential quantifier, 
so it is proposed to have 2 symbols ``∃!`` to represent unique quantifier ``foralone``.

Maybe also UnicodeSyntax affects modifiers:

1. ``%＠`` could represent ``%term`` clarificator (Percent Sign, U+0025) + (Fullwidth Commercial At, U+FF20) ( NOT (Commercial At, U+0040) ).

2. ``%≡`` could represent ``%type`` clarificator (Percent Sign, U+0025) + (Identical To, U+2261).

3. ``%≝`` could represent ``%arg``  clarificator (Percent Sign, U+0025) + (Equal to By Definition, U+2254).


Examples
::

  id :: ∀ a. a -> a
  id @t x = x :: ∃! %＠ t. t

  f1 :: ∀ a b. [a] -> [b] -> [(a, b)]
  f1 @aa @bb xs ys  = zip (xs :: ∃! %＠ aa. [aa]) yys
     where
       yys :: ∃! %≡ b. [b]
       yys = reverse ys

  class D a where
    m :: ∃! %≝ a. a -> a

  instance Num a => D [a] where
    m :: ∃! %≝ a. [a] -> [a]
    m x = map (*2) x

Modifiers
~~~~~~~~~

We allow to write ``%alone`` + ``%arg``, ``%term`` or ``%type`` (or ``%each``) as modifiers 
for ``forall`` type variable declarations near (before) type variable.

ScopedTypeVariables
~~~~~~~~~~~~~~~~~~~

``ScopedTypeVariables`` extension ignores ``foralone``-quantified variables.

But we could reuse part of searching algorithms from ``ScopedTypeVariables`` algorithms.

ScopedTypeAbstractions
~~~~~~~~~~~~~~~~~~~~~~

``TypeAbstractions`` extension ignores ``foralone``-quantified variables.

But it has build lexical scoping searching rules for unquantified type variables, 
which it is better to segregate later into new ``JustTypeAbstractions`` and ``ScopeForTypeAbstractions`` extension.

.. code:: none

    TypeAbstraction = JustTypeAbstractions + ScopeForTypeAbstractions

Visible ForAll and ForEach
~~~~~~~~~~~~~~~~~~~~~~~~~~

Since ``foralone`` quantifier just use already existing type variables, 
there is no need to be used as visible or as unerased quantifiers.

NoImplicitForAll
~~~~~~~~~~~~~~~~

This Proposal do not contradicts ``NoImplicitForAll`` extension.

CurriedQuantifiers
~~~~~~~~~~~~~~~~~~

This proposal is better to use with curried / nested quantifiers (foralls) features.


Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs of ``ForAloneQuantifier`` has medium difficulty.


Alternatives
------------

Main alternative is "Modern Scoped Type Variables" `#448`_ (``ScopedTypeVariables`` extension), 
but also ``TypeAbstractions`` and ``PartialTypeSignatures``.

Alternative keywords
~~~~~~~~~~~~~~~~~~~~

We could choose different keywords instead of proposed latin and unicode keywords.

Alternative to latin name ``foralone`` could be chosen ``forone``, ``for1``, ``forunique``, ``forsingle``, ``foronly``, ...

Alternative to unicode name ``∃!`` could be for example ``∃1``.


Backward Compatibility
----------------------

This proposal is fully backward compatible.


Unresolved Questions
--------------------

Most unresolved question are theoretical: how to to call right local quantifiers.

Is it Quantifier? Is it Existential? Is it Unique?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some people think, that it it wrong naming: maybe it is more carefully 
to call foralone "quantifier" as either "pseudo-quantifier" or "quasi-quantifier".

Also some people think, that is wrong naming: 
either it call "existential quantifier" or "unique quantifier".

1) `Adam Gundry <https://github.com/adamgundry>`__ said:
     
    I don't think you are using the term "quantifier" as it is normally 
    understood in logic or type theory, and it is difficult 
    to unpack what you mean.

    I think the question of the relationship of this proposal to 
    (unique) existential quantification is a red herring. 
    The core idea of the proposal seems to be that a "local quantifier" 
    is an annotation on a type signature that does not itself bind a type variable 
    or quantify over types, but rather indicates the binding site of the type variable 
    (e.g. whether it was bound by a type abstraction, a scoped type variable 
    bound in a type signature, or somewhere else). 
    This does not increase expressivity, but allows the programmer to be more explicit.

2) `Jaro <https://github.com/noughtmare>`__ said:

    Here's a proof in Agda that ``∃! b. Bool → b`` is false:

    ::
	
      open import Relation.Nullary.Negation
      open import Data.Product
      open import Data.Bool
      open import Relation.Binary.PropositionalEquality
      open import Data.Nat

      postulate Bool≠ℕ : ¬ (Bool ≡ ℕ)

      foo : Bool → Bool
      foo x = x

      bar : Bool → ℕ
      bar false = zero
      bar true = suc zero

      qux : ¬ (∃! _≡_ λ b → Bool → b)
      qux (A , this , that≡this) with that≡this foo | that≡this bar
      ... | refl | Bool≡ℕ = Bool≠ℕ Bool≡ℕ
	  

    I needed to postulate that Bool is not ℕ because I think that is a bit hard to prove.
	 
    ...
    The "truth" of a type in a functional language is, by 
    the Curry-Howard correspondence, whether it is inhabited or not.
	 
    ...
    The most confusing part seems to be the lambda. 
    You should really read it more like ``∃![ b ] (Bool → b)``. 
    It's just that the lambda is the only way to bind new variables 
    and ``∃!`` needs to bind the ``b`` variable in this case.
	 
    (The ``_≡_`` argument is just for saying up to which kind 
    of equality we want it to be unique. 
    In this case it is propositional equality which is the built-in equality in Agda. 
    We could instead use isomorphisms on types as our notion of equality, 
    which would also allow us to prove that ``Bool`` is not ``ℕ`` more easily.)
	 
    If I wanted to say that there is a unique function 
    then I'd write that something like this:
	 
    ::
	
      ¬ (∃! {A = Bool → ?} _≡_ λ f → ⊤)

3) `Tom Ellis <https://github.com/tomjaguarpaw>`__ said:

    I think that this is correct: ``∃!`` doesn't correspond to 
    uniqueness quantification in a Curry Howard interpretation. 
    In fact I'm not sure that ``∃! b. Bool -> b`` is a type at all. 
    Rather it seems to be a property of a type ``t``, 
    specifically ``∃! b. t`` is satisfied by types ``t'`` for which 
    there exists a type ``s`` such that ``t[b -> s] = t'``.

ForWhich
~~~~~~~~~

It is unclear which quantifier should be used in next example
::

    data Proxy a = P

    g2 :: forall a. Proxy (Nothing @(a, a)) -> ()
    g2 (P @(Nothing :: for??? t. Maybe (t, t))) = ()


Is it any? Or it is more correct to describe it as "as-pattern" in signatures at ``KindSignatures`` for that?

::

    g2 (P @(Nothing :: Maybe ( t@Type , t))) = ()

This proposal do not cover this example.


Implementation Plan
-------------------

Unclear. The author cannot implement this proposal.


Acknowledgments
---------------


Endorsements
------------
