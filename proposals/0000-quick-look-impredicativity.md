---
author: Alejandro Serrano Mena
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/274).

# Quick Look Impredicativity

`ImpredicativeTypes` is one of those extensions which are not usually needed, but is unavoidable once you require it. Alas, `ImpredicativeTypes` has been in a half-broken state for quite some time, and not officially supported. This proposal describes a new approach to impredicative type checking which is (1) powerful enough for the most common use cases, and (2) predictable, so errors can be readily explained.

This proposal is based on this [paper draft](https://www.microsoft.com/en-us/research/publication/a-quick-look-at-impredicativity/), which in turn borrows many ideas from [*Guarded impredicative polymorphism*](https://www.microsoft.com/en-us/research/publication/guarded-impredicative-polymorphism/) (published in PLDI'18).

## Motivation

As most languages based on the Hindley-Damas-Milner typing discipline do, Haskell 2010 distinguishes between *monomorphic types* such as `Int -> Int`, which contain no `forall`s, and *type schemes* or *polymorphic types* like `forall a. a -> a`. Each expression in a program must, in principle, be given a *monomorphic* type -- if a variable has a polymorphic type it is *instantiated* beforehand to a *variable*, whose value is found by type inference. For example, when we write `id True`, the `a` in the type of `id` is instantiated to the type `Bool`, which means that that specific occurrence of `id` has type `Bool -> Bool`.

On the other side of the spectrum we find System F, which forms the basis of GHC Core, in which instantiation is not restricted to monomorphic types. Using the syntax from `TypeApplications`, we can write `id @(forall a. a -> [a])` to obtain a function of type `(forall a. a -> [a]) -> (forall a. a -> [a])`. We say that such instantiation with a polymorphic type is an *impredicative* instantiation. System F is able to instantiatiate impredicatively simply because every instantiation there is *explicit*, as one can witness when looking at GHC Core.

Historically, the `ImpredicativeTypes` extension in GHC has allowed programmers to use impredicative instantiation. However, there was no clear specification of how it works or any guarantee that code that compiles now remains well-typed in the future. As a result, the official stance is that `ImpredicativeTypes` is not supported. The aim of this proposal is to remedy that, by giving a clear specification of impredicative polymorphism in the context of the modern GHC compiler.

One may question *why* impredicative instantiation should be supported. One of such reasons is that there are already packages in the wild which require this extension, and this proposal gives them a guarantee about their future support. Each of these packages provide an example of impredicative types.

Take for example lenses as defined by the `lens` library:

```haskell
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a
```

Creating a list of such lenses, `[Lens' a Int]`, requires impredicatively instantiating the constructors `(:)` and `[]`. Without no good support for impredicativity, programmers had to resort to wrapping the values in a `newtype`. For example, the `lens` library defines:

> ```haskell
> type ALens s t a b = LensLike (Pretext (->) a b) s t a b 
> ```
>
> This type can also be used when you need to store a `Lens` in a container, since it is rank-1.

This proposal makes most of these `newtype` wrappers unnecessary.

Furthermore, if we want to replace an expression such as `view l` with `view $ l`, we need to instantiate the types of `($) :: forall a b. (a -> b) -> a -> b` impredicatively (this case is so common that GHC contains a special case for them).

## Proposed Change Specification

This proposal is structured in two parts. A precise specification can be found in this [paper draft](https://www.microsoft.com/en-us/research/publication/a-quick-look-at-impredicativity/), which would most surely lead to an actual paper submitted to a research conference. Below you can find a more approachable description, which could ultimately lead to a section in the GHC Users Guide.

Any programmer who does not enable `ImpredicativeTypes` is unaware of the contents of this proposal: impredicative instantiation is *not* allowed (rank-n types do not contradict that statement, since there is no instantiation going on during their type checking). In particular, type checking an application `e_0 e_1 ... e_n` in done in the following steps:

1. *Infer* the type of `e_0`, which we shall call `sigma_0`,
2. Expose the first *n* argument types by instantiating `sigma_0` into a type of the form `sigma_1 -> ... -> sigma_n -> sigma_result`,
3. *Check* each argument expression *e_i* against the corresponding type `sigma_i`,
4. If we are in checking mode, run the subtype check (`tcSubType`) of `sigma_result` against the type being pushed.

For example, suppose we are inferring the type of `id True`:

1. We infer the type of `id` by merely looking up in the environment, `forall a. a -> a`,
2. We expose one argument, and for that we instantiate the type above with a new type variable, leading to `alpha -> alpha`,
3. Now we check the argument `True` against `alpha`:
   a. We infer the type of `True`, which is `Bool`,
   b. Steps (2) and (3) are not needed here, since there are no arguments,
   c. We perform the subtype check `alpha <= Bool`, leading to the unification `alpha := Bool`;
4. The result of the inference is `alpha`; after zonking the result is `Bool`.

When `ImpredicativeTypes` is on we introduce an additional step between (2) and (3), which we call *quick look*. Quick look traverses the arguments of the application, and tries to infer as much impredicative instantiations as possible. The results are applied before step (3), which means that arguments are type checked with the correct impredicative instantiations. The "quick look" pass only traverses *simple expressions*, which we define to be (possibly nested) applications of variables in the environment.

Another way to look at this proposal is that each application is type checked *twice*: first the "quick look" pass tries to infer impredicative instantiation, which is then fed to the second, real pass. For simple expressions it is crystal clear how impredicative instantiation is threaded. On the contrary, it never looks at abstractions, pattern matching, `let`s, or any other expression.

One important feature of Haskell's type system that "quick look" uses is the invariance of type constructors. In short, the subsumption rules ensure that if, for example, `Maybe t` is a more polymorphic than `Maybe s`, it must be the case that `t` equals `s`. This property holds for every type constructor except for function types, which require a slightly more complex handling. The proposed inference algorithm never tries to perform any complicated analysis on other types: impredicativity must be the *only obvious* solution to make the program type check (or as we say in the paper, "impredicativity is never guessed"). Note that since [proposal 287, *Simplify Subsumption*](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0287-simplify-subsumption.rst) has been accepted, function types are also considered invariant.

## Examples

Several examples can be found in the [paper draft](https://www.microsoft.com/en-us/research/publication/a-quick-look-at-impredicativity/). Let us review the main example, `(\x -> x) : ids`, where `ids :: [forall a. a -> a]` from the eyes of the "approachable" description.

1. We infer the type of the head of the application, `(:) :: forall a. a -> [a] -> [a]`.
2. We expose two arguments, which leads to the type `alpha -> [alpha] -> [alpha]` where `alpha` is a fresh unification variable.
3. Now we do *quick look* by checking the simple expressions against those types:
  a. `\x -> x` is not simple, so nothing is done.
  b. `ids :: [forall a. a -> a]` is checked against `[alpha]`. Since `alpha` is under a type constructor different than arrow, `alpha` *must* be `forall a. a -> a` for the expression to type check.
4. The result of quick look is thus `alpha := forall a. a -> a`. This means that the second, real type checking phase must check:
  a. `\x -> x` against `forall a. a -> a`,
  b. `ids` against `[forall a. a -> a]`.

Compare this example to `(\x -> x) : []`. In this case quick look does not return any impredicativity information (since `[]` does not contain any). Thus the inferred type for that expression is `[beta -> beta]`, without any impredicative polymorphism.

## Effect and Interactions

As discussed several times throughout this proposal, its goal is to give a clear and simple specification of impredicative instantiation within GHC. Whereas "simple" is a subjective matter, the availability of a set of typing rules defines a clear specification.

### `TypeApplications`

We deem the interaction with `TypeApplications` as a very important one; the goal is for our specification to benefit from user-written types as much as possible. We want `(:) @(forall a. a -> a) (\x -> x) []` to work, without the need of an additional type application in `[]`. The draft paper details the interaction between those features in depth.

### The typing rule for `($)`

Currently, GHC contains a [hard-coded typing rule for `($)`](https://gitlab.haskell.org/ghc/ghc/blob/795986aaf33e2ffc233836b86a92a77366c91db2/compiler/typecheck/TcExpr.hs#L368-397) which ensures that `f $ e` works even when `($)` would need to be impredicatively instantiated. The million dollar question is: can we drop this special case from the compiler?

In the releases of GHC prior to the introduction of levity polymorphism, the answer would have been **no**. The reason was that `($)` was the only function able to operate with both lifted and unlifted kinds. But nowadays, this fact is reflected in its type using a levity-polymorphic type:

```haskell
($) ::
  forall (r :: GHC.Types.RuntimeRep) a (b :: TYPE r).
  (a -> b) -> a -> b
```

So the initial answer would be **yes**. This should be taken with a grain of salt, though, since the specification in the draft paper does not consider polymorphic type representations. However, for the usual case of `b` having kind `*`, the rule may be dropped. This also means that other combinators such as `(&)` or `(.)` no longer are second-class with respect to impredicativity.

Note however that currently GHC is very lenient about required extensions when using `($)`. For example, this code compiles today *without extensions*:

```haskell
> import Control.Monad.ST
> runST $ return 0
0
```

Even though the [current documentation about `RankNTypes`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RankNTypes) does not make it clear whether the extension is required if you merely want to consume types of this form, it is clear that `($)` has to be impredicatively instantiated for the expression to type check. **Removing the special rule is thus backwards-incompatible, as users would be asked to enable `ImpredicativeTypes`.**

## Costs and Drawbacks

As discussed below, there is already a branch of GHC in which these changes have been implemented. Furthermore, we expect maintenance costs to be low, since "quick look" is a separate phase from the rest of type checking; there is no intrincate relation between the two phases in the code.

`ImpredicativeTypes` has always been an advanced feature of the language. However, once you arrive at it, programmers often ask themselves: "what is stopping the compiler from accepting this?". This proposal gives an answer to that question, with a succint explanation: "impredicative is never guessed, it must be obvious from the simple parts of each application".

## Alternatives

The main alternative is to keep the *status quo*: `ImpredicativeTypes` is there, but never officially supported. However, that may bring surprising behavior, which may change unexpectedly with a new GHC release.

Another possibility is to drop impredicative type inference altogether, and expect the programmers to use `TypeApplications` anytime it is required. The problem is that impredicative instantiation is quite contagious, so one might need to write things like:

```haskell
(++) @(forall a. a -> a) ids ([] @(forall a. a -> a))
```

### The dollar rule

As discussed above, currently GHC is very lenient in requiring the right extensions for impredicative instantiations of `($)`. One alternative (the one which is currently implemented in the corresponding merge request) is to keep the special rule, only to continue with that lenient behavior. This would coincide with the non-written rule among Haskellers that `f $ x` is exactly as writing `f x` but with different fixity.

## Unresolved Questions

Right now the "quick look" phase does not produce any errors on its own, but not inferring some types on that phase may lead to errors down the pipeline. Can we improve the current error messages about impredicative instantiation to suggest hints, such as eta-expanding a function or adding a type application?

## Implementation Plan

@simonpj is working on an implementation in [merge request !3220](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3220).
