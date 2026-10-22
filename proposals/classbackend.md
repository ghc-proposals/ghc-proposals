---
author: Iceland_jack
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/461).

# Type Class Backend

This proposes a language extension `-XClassBackend` which separates
the interface of a type class (frontend) from the concrete
representation (backend). The frontend itself is virtual and is
immediately translated into the backend.

This allows deriving classes that could previously not be derived
because of their representation (notably `Traversable`) and enables us
to evolve and generalise type classes without breaking backwards
compatibility.

Any type class may have at most one backend specified. When defining
or deriving a class `Front` an instance of its backend `Back` is
automatially produced through the same mechanism used by
`DerivingVia`. The methods of `Front` are defined in terms of methods
of `Back`. The aim is to use `Front` without needing to be aware of
`Back`.

```haskell
class Front a where
  front :: a -> a
  front = .. back ..

  deriving Back a
  via W a
```

## Motivation
The need for type class backends comes up in different ways: for
purposes of presentation, representation, evolution and
generalisation.

### Presentation Argument
It is common to have more than one way to present a type class.

Some type classes have different but equivalent interfaces that may be
useful under different circumstances. One may want to define
`Applicative` as a "lax monoidal functor" without adding redundant
methods. Instead a new type class frontend (`Monoidal`) is defined
with `Applicative` as a backend.

```haskell
class Functor f => Monoidal f where
  unit :: f ()
  (**) :: f a -> f b -> f (a, b)

  deriving Applicative f
  via WrappedMonoidal f
```

The definition of `WrappedMonoidal` shows how `Monoidal` is translated into `Applicative`:

```haskell
type    WrappedMonoidal :: (Type -> Type) -> Type -> Type
newtype WrappedMonoidal f a = WrapMonoidal (f a)
  deriving newtype (Functor, Monoidal)

instance Monoidal f => Applicative (WrappedMonoidal f) where
  pure :: a -> WrappedMonoidal f a
  pure a = a <$ unit

  liftA2 :: (a -> b -> c) -> (WrappedMonoidal f a -> WrappedMonoidal f b -> WrappedMonoidal f c)
  liftA2 (·) as bs = uncurry (·) <$> mult as bs
```

There are different options for [bundling type
parameters](https://www.cas.mcmaster.ca/~carette/publications/gpce19main-p33.pdf),
whether to use an associated type family or a multi-parameter type
class (MPTC)? They have strengths and weaknesses; Associated type
families are derived implicitly and allow default definitions. MPTCs
can be shorter to write and allow explicit reference of their argument
when deriving. 

It allows an easy implementation for [removing methods from type
classes](https://github.com/haskell/core-libraries-committee/issues/3). We
can define an single-method `Eq` backend and drop the definition of
`(/=)` in translation without affecting use code.
### Representation Argument

Many classes are incompatible with `coerce`-based deriving. The most
infamous example of which being `Traversable`. For an in-depth explanation see
[`QuantifiedConstraints` and the trouble with
`Traversable`](https://ryanglscott.github.io/2018/06/22/quantifiedconstraints-and-the-trouble-with-traversable/).

The reason (representation) is very hard to tackle without major
breaking chages to `Functor` or `Traversable`. The problem is that to
derive `Traversable T1` via `T2` we must `coerce` under an `f` (`f (T2
b)` to `f (T1 b)`) but we do not know that `f` supports that (it may
have a nominal role).

```haskell
class .. => Traversable t where
  traverse :: Applicative f => (a -> f b) -> (t a -> f (t b))
```

All we have to do to allow deriving with this proposal: is to replace
the problematic `f (t b)` with [`Yoneda f (t
b)`](https://hackage.haskell.org/package/kan-extensions-5.2.3/docs/Data-Functor-Yoneda.html)
in the backend. This effectively pulls `t b` out of `f` and has a
representation amenable to deriving, because `Yoneda` is
representational in both arguments: we no longer coerce under `f ..`.

```haskell
class .. => BackTraversable t where
  backTraverse :: Applicative f => (a -> f b) -> (t a -> (t b -> x) -> f x)
```

This affects multiple type classes, I reckon most if not all can be
derived by translating them into a backend with a better
representation:

+ Any `Traversable` variant (generic `GTraversable`, higher-kinded
  `FTraversable`, `TraversableWithIndex`, `Bitraversable`, non-empty
  `Traversable1`, non-empty `Bitraversable1`, indexed `ITraversable`,
  monomorphic `MonoTraversable`, `TreeLike` .. and many more)
+ `ArrowApply`
+ `Data`
+ `MonadLogic`
+ `Uniform`, `UniformRange` ([issue](https://github.com/haskell/random/pull/71))
+ [`Serial`](https://hackage.haskell.org/package/bytes-0.17.1/docs/Data-Bytes-Serial.html#t:Serial)
+ `Distributive`
+ It blocks `Representable` from evolving ([issue](https://github.com/ekmett/adjunctions/issues/62), [issue](https://github.com/ekmett/adjunctions/issues/60))
+ [`Logistic`](https://www.reddit.com/r/haskell/comments/qugwed/logistic_is_to_setters_as_distributive_is_to/),
  a new class, this in particular can be derived using contravariant `Coyoneda` and covariant `Yoneda`.
+ Classes with optic methods, most classes in the [*lens*](https://hackage.haskell.org/package/lens) library.

Ultimately library writers should not have to choose between deriving
and the interface they want to present. Underivable classes are popping up
more frequently with the advent of optics and fancier types.

### Generalisation and Evolution Argument

Some classes have a natural backend that unifies their behaviour for
more types. 

+ A [categorical
  `Functor`](https://www.reddit.com/r/haskell/comments/eoo16m/base_category_polymorphic_functor_and_functorof/)
  generalizes many type classes (`Functor`, `Contravariant`,
  `Bifunctor`, `Profunctor`, `Filterable`, `Functor1`, `IxFunctor`,
  `HFunctor`, `ExpFunctor`, `ProfunctorFunctor`, `FunctorWithIndex`,
  [`MFunctor`](https://hackage.haskell.org/package/mmorph-1.2.0/docs/Control-Monad-Morph.html)
  and many more) and can serve as their backend. There are two type
  families associated with the categorical functor which makes it a
  candidate for a "presentational backend", unbundling the type
  families into parameters: `FunctorOf (->) (->) f`.
+ `Generic` and `Generic1` provide generic representations for types
  and unary type constructors but that's it. If we wanted to provide
  generic datatype definitions in terms of arbitrary kinds
  ([`kind-generics`](https://hackage.haskell.org/package/kind-generics))
  we could make them both frontends for `GenericK`.
+ `Monoid`, `Applicative` and `Monad` (and "pre-`Arrow`") can be
  viewed as [monoids](https://arxiv.org/pdf/1406.4823.pdf) in the same
  way but in different monoidal categories. Ultimately this proposal
  gives an option to make this explicit.
+ `Foldable` and `Foldable1` could be unified as converting to various 
  free algebras (the free `Monoid` and free `Semigroup` respectively).

## Proposed Change Specification

When the `-XClassBackend` extension is enabled it allows adding a
deriving clause to a type class.

```haskell
class F a where
  ..
  deriving B a
  via W a
```

An `F` instance behaves as if the user had implemented such an
instance and then derived `B` via the via type:

```haskell
instance F A where ..

deriving
  via W A
  instance B A
```

## Costs and Drawbacks

## Alternatives

For presentation you can sometimes add more methods to the type
class. For bundling and unbundling classes there is no real
alternative, except for defining some kind of class synonym but this
cannot be used to deriving or implementing instances of
`Representable`.

```haskell
class    (Representable f, Rep f ~ rep) => RepresentableOf rep f
instance (Representable f, Rep f ~ rep) => RepresentableOf rep f
```

For representation there is little recourse but to write the instances
by hand. Ultimately you have to choose between deriving and usability,
you can give `Functor` a superclass that makes it work for type
constructors with a representational role only or downgrade
`Applicative` in `Traversable` to only handle such functors. Other
solutions would require replacing every single method in `Traversable`
with `Yoneda` variants.

For generalization there is no alternative.

## Unresolved Questions

## Implementation Plan

## Endorsements
