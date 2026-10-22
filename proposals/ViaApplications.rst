ViaApplications
==============

.. proposal-number:: 
.. trac-ticket:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/218>`_.
.. sectnum::
.. contents::

Allow ``@(<ty> via <viaty>)`` which instantiates ``@<viaty>`` (using ``<viaty>`` instances) and then coerces as if we had written ``@<ty>``


Motivation
------------

Allow writing 

::

 foldMap @f @(Bool via Any) @a

(handwave) where we instantiate its second type parameter to ``Any`` (``foldMap @f @Any @a``) but coerced to the type of ``foldMap @f @Bool @a``, somehow ignoring that ``Monoid Bool`` does not hold

::

 coerce $ foldMap @f @Any @a :: forall f a. Foldable f => (a -> Bool) -> (f a -> Bool)

This would give us the following 

::

 sum :: forall a. Num a => [a] -> a
 sum = fold @[] @(a via Sum a)

 product :: forall a. Num a => [a] -> a
 product = fold @[] @(a via Product a)

 minimum :: foldl

 any :: (a -> Bool) -> ([a] -> Bool)
 any = foldMap @[] @(Bool via Any)

 all :: (a -> Bool) -> ([a] -> Bool)
 all = foldMap @[] @(Bool via All)

And allows an alternative default ``foldl`` definition which makes use of composing types and their (``Monoid``) functionality


like `Data.Foldable.foldl <https://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Foldable.html#foldl/>`_:

::

 foldl :: (b -> a -> b) -> (b -> t a -> b)
 foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z

 -- vs

 foldl :: (b -> a -> b) -> (b -> t a -> b)
 foldl f z as = foldMap @(b->b via Dual (Endo b))
   (flip f)
   as
   z

::

 >> mappend @(Int via Ap [] (Product Int)) [1,2,3] [1,11,111]
 [1,11,111,2,22,222,3,33,333]

`Data.Semigroup.Option <https://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Semigroup.html#line-521/>`_:

::

 instance Semigroup a => Semigroup (Option a) where
  (<>) = coerce ((<>) :: Maybe a -> Maybe a -> Maybe a)

 -- vs

 instance Semigroup a => Semigroup (Option a) where
  (<>) = (<>) @(Option a via Maybe a)

`Numeric.Natural.Natural <https://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Enum.html#line-968/>`_:

::

 instance Enum Natural where
  enumFromThen x y
    | x <= y    = coerce (enumFromThen :: Integer -> Integer -> [Integer]) x y
    | otherwise = enumFromThenTo x y (wordToNaturalBase 0##)

  enumFrom       = coerce (enumFrom     :: Integer -> [Integer])
  enumFromTo     = coerce (enumFromTo   :: Integer -> Integer -> [Integer])
  enumFromThenTo = coerce (enumFromThenTo :: Integer -> Integer -> Integer -> [Integer])

 -- vs

 instance Enum Natural where
  enumFromThen x y
    | x <= y    = enumFromThen @(Integer via Natural) x y
    | otherwise = enumFromThenTo x y (wordToNaturalBase 0##)

  enumFrom       = enumFrom @(Integer via Natural)
  enumFromTo     = enumFromTo     @(Integer via Natural)
  enumFromThenTo = enumFromThenTo @(Integer via Natural)

`Data.Bitraversable.bifoldMapDefault <https://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Bitraversable.html#bimapDefault/>`_:

::

 bifoldMapDefault :: forall t m a b. (Bitraversable t, Monoid m) => (a -> m) -> (b -> m) -> t a b -> m
 bifoldMapDefault = coerce
   (bitraverse :: (a -> Const m ())
               -> (b -> Const m ()) -> t a b -> Const m (t () ()))

 -- vs

 bifoldMapDefault :: forall t m a b . (Bitraversable t, Monoid m) => (a -> m) -> (b -> m) -> t a b -> m
 bifoldMapDefault = bitraverse @(m via Const m)


Proposed Change Specification
-----------------------------
Specify the change in precise, comprehensive yet concise language. Avoid words like should or could. Strive for a complete definition. Your specification may include,

* allows ``@(<ty> via <viaTy>)`` which means coercing the term ``@<viaTy>`` to the type if it had been ``@<ty>``


Effect and Interactions
-----------------------
Detail how the proposed change addresses the original problem raised in the motivation.

Discuss possibly contentious interactions with existing language or compiler features. 


Costs and Drawbacks
-------------------
Give an estimate on development and maintenance costs. List how this effects learnability of the language for novice users. Define and list any remaining drawbacks that cannot be resolved.


Alternatives
------------
The alternative is writing a cumbersome ``coerce`` with explicit annotations (which is what libraries do)

::

 bifoldMapDefault :: forall t m a b. (Bitraversable t, Monoid m) => (a -> m) -> (b -> m) -> t a b -> m
 bifoldMapDefault = coerce
   (bitraverse :: (a -> Const m ())
               -> (b -> Const m ()) -> t a b -> Const m (t () ()))


Unresolved questions
--------------------

Unfortunately this doesn't work, 

::

 bimapDefault :: forall t a b c d. Bitraversable t => (a -> b) -> (c -> d) -> t a c -> t b d
 bimapDefault = coerce
   (bitraverse :: (a -> Identity b)
               -> (c -> Identity d) -> t a c -> Identity (t b d))

 -- vs

 bimapDefault :: forall t a b c d. Bitraversable t => (a -> b) -> (c -> d) -> t a c -> t b d
 bimapDefault = bitraverse @(? via Identity)


Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other ressources and prerequisites are required for implementation?
