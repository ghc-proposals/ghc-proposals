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

## Motivation

As most languages based on the Hindley-Damas-Milner typing discipline do, Haskell 2010 distinguishes between *monomorphic types* such as `Int -> Int`, which contain no `forall`s, and *type schemes* or *polymorphic types* like `forall a. a -> a`. Each expression in a program must, in principle, be given a *monomorphic* type -- if a variable has a polymorphic type it is *instantiated* beforehand to a *variable*, whose value is found by type inference. For example, when we write `id True`, the `a` in the type of `id` is instantiated to the type `Bool`, which means that that specific occurrence of `id` has type `Bool -> Bool`.

On the other side of the spectrum we find System F, which forms the basis of GHC Core, in which instantiation is not restricted to monomorphic types. Using the syntax from `TypeApplications`, we can write `id @(forall a. a -> a)` to obtain a function of type `(forall a. a -> [a]) -> (forall a. a -> [a])`. We say that such instantiation with a polymorphic type is an *impredicative* instantiation. System F is able to instantiatiate impredicatively simply because every instantiation there is *explicit*, as one can witness when looking at GHC Core.

Historically, the `ImpredicativeTypes` extension in GHC has allowed programmers to use impredicative instantiation. However, there was no clear specification of how it works or any guarantee that code that compiles now remains well-typed in the future. As a result, the official stance is that `ImpredicativeTypes` is not supported. The aim of this proposal is to remedy that, by giving a clear specification of impredicative polymorphism in the context of the modern GHC compiler.

One may question *why* impredicative instantiation should be supported. One of such reasons is that there are already packages in the wild which require this extension, and this proposal gives them a guarantee about their future support. Each of these packages provide an example of impredicative types.

Take for example lenses as defined by the `lens` library:

```haskell
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a
```

Creating a list of such lenses, `[Lens' a Int]`, requires impredicatively instantiating the constructors `(:)` and `[]`. Also if we want to replace an expression such as `view l` with `view $ l`, we need to instantiate the types of `($) :: forall a b. (a -> b) -> a -> b` impredicatively (this case is so common that GHC contains a special case for them).

## Proposed Change Specification

This proposal is structured in two parts. A precise specification can be found in this [paper draft](https://www.dropbox.com/s/hxjp28ym3lptmxw/quick-look-steps.pdf?dl=0), which would most surely lead to an actual paper submitted to a research conference. Below you can find a more approachable description, which could ultimately lead to a section in the GHC Users Guide.

Any programmer which does not enable `ImpredicativeTypes` is unaware of the contents of this proposal (with a small caveat for users of `RankNTypes`, see below): impredicative instantiation is *not* allowed (rank-n types do not contradict that statement, since there is no instantiation going on during their type checking). In particular, type checking an application `e_0 e_1 ... e_n` in done in the following steps:

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

When `ImpredicativeTypes` is on we introduce an additional step between (2) and (3), which we call *quick look*. Quick look traverses the arguments of the application, and tries to infer as much impredicative instantiations as possible. The results are applied before step (3), which means that arguments are type checked with the correct impredicative instantiations. Another way to look at this proposal is that each application is type checked *twice*: first the "simple" pass tries to infer impredicative instantiation, which is then fed to the second, real pass.

The name "quick look"  was chosen because this additional step only traverses the "simple" cases of type checking, namely (possibly nested) applications of variables in the environment. In those cases it is crystal clear how impredicative instantiation is threaded. On the contrary, it never looks at abstractions, pattern matching, `let`s, or any other expression.

In addition, "quick look" only unifies a type variable when it is found under a type constructor different than arrows, because invariance ensures that equality is the only way to make the expression type check. It never tries to perform any complicated analysis on other types: impredicativity must be the *only obvious* solution to make the program type check (or as we say in the paper, "impredicativity is never guessed").

## Examples

Several examples can be found in the [paper draft](https://www.dropbox.com/s/hxjp28ym3lptmxw/quick-look-steps.pdf?dl=0). Let us review the main example, `(\x -> x) : ids`, where `ids :: [forall a. a -> a]` from the eyes of the "approachable" description.

1. We infer the type of the head of the application, `(:) :: forall a. a -> [a] -> [a]`.
2. We expose two arguments, which leads to the type `alpha -> [alpha] -> [alpha]` where `alpha` is a fresh unification variable.
3. Now we do *quick look* by checking the "simple" expressions against those types:
  a. `\x -> x` is not "simple", so nothing is done.
  b. `ids :: [forall a. a -> a]` is checked against `[alpha]`. Since `alpha` is under a type constructor different than arrow, `alpha` *must* be `forall a. a -> a` for the expression to type check.
4. The result of quick look is thus `alpha := forall a. a -> a`. This means that the second, real type checking phase must check:
  a. `\x -> x` against `forall a. a -> a`,
  b. `ids` against `[forall a. a -> a]`.

Compare this example to `(\x -> x) : []`. In this case quick look does not return any impredicativity information (since `[]` does not contain any). Thus the inferred type for that expression is `[beta -> beta]`, without any polymorphism.

## Effect and Interactions

As discussed several times throughout this proposal, its goal is to give a clear and simple specification of impredicative instantiation within GHC. Whereas "simple" is a subjective matter, the availability of a set of typing rules defines a clear specification.

We deem the interaction with `TypeApplications` as a very important one; the goal is for our specification to benefit from user-written types as much as possible. We want `(:) @(forall a. a -> a) (\x -> x) []` to work, without the need of an additional type application in `[]`. The draft paper details the interaction between those features in depth,

The main disadvantage of making "quick look impredicativity" official is that it collides with *contravariance of function types*. That is, it no longer holds that:

`(Int -> Int) -> R is a subtype of (forall a. a -> a) -> R`

The problem is of technical nature, but without this restriction we would not be able to infer impredicative instantiations of functions like `($)`.

Note however that there is a simple workaround for this problem: eta-expansion. Take for example the functions `choose :: forall a. (a -> Bool) -> (a -> Bool) -> Bool`, `f :: (Bool -> Bool) -> Bool` and `g :: (forall a. a -> a) -> Bool`. In theory we can accept the expressions `choose f g` and `choose g f` if we instantiate `a` with `Int -> Int`, due to contravariance. In a non-contravariant world we need to eta-expand `g` to make it work: `choose f (\x -> g x)`.

Note anyway that the support for contravariance in GHC is really fragile. Here is an excerpt of a GHCi session which shows that the one of the expressions discussed above is accepted and the other is not:

```text
Prelude> :set -XRankNTypes -XImpredicativeTypes
Prelude> choose :: (a -> Bool) -> (a -> Bool) -> Bool ; choose = undefined
Prelude> f :: (Bool -> Bool) -> Bool ; f = undefined
Prelude> g :: (forall a. a -> a) -> Bool ; g = undefined
Prelude> :t choose f g

<interactive>:1:10: error:
    • Couldn't match type ‘Bool -> Bool’ with ‘forall a. a -> a’
      Expected type: (Bool -> Bool) -> Bool
        Actual type: (forall a. a -> a) -> Bool
    • In the second argument of ‘choose’, namely ‘g’
      In the expression: choose f g
Prelude> :t choose g f
choose g f :: Bool
```

## Costs and Drawbacks

As discussed below, there is already a branch of GHC in which these changes have been implemented. Furthermore, we expect maintenance costs to be low, since "quick look" is a separate phase from the rest of type checking; there is no intrincate relation between the two phases in the code.

`ImpredicativeTypes` has always been an advanced feature of the language. However, once you arrive at it, programmers often ask themselves: "what is stopping the compiler from accepting this?". This proposal gives an answer to that question, with a succint explanation: "impredicative is never guessed, it must be obvious from the simple parts of each application".

The main drawback is clearly the lack of contravariance. It is arguable whether contravariance of function types is something a beginner (or even intermediate) Haskeller expects, since up to that point all types are compared by equality. However, it should be possible during error reporting to scan whether a type error talks about two types which could be related if contravariance was taking into account, and report a message on the lines of:

```text
Hint: try to eta-expand the second argument.
```

## Alternatives

The main alternative is to keep the *status quo*: `ImpredicativeTypes` is there, but never officially supported. However, that may bring surprising behavior, which may change unexpectedly with a new GHC release.

Another possibility is to drop impredicative type inference altogether, and expect the programmers to use `TypeApplications` anytime it is required. The problem is that impredicative instantiation is quite contagious, so one might need to write things like:

```haskell
(++) @(forall a. a -> a) ids ([] @(forall a. a -> a))
```

## Unresolved Questions

* Is it possible to regain contravariance without over-complicating the specification?
* What should be the error messages in those cases in which "quick look" was unsuccessful and did not infer enough information for impredicative instantiation?

## Implementation Plan

The proposal has been implemented in a branch, and is under code review in [this merge request](https://gitlab.haskell.org/ghc/ghc/merge_requests/1659).
