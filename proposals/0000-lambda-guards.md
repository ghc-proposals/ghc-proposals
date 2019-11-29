---
author: Jakob Brünker
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/302>).

# Guards in Lambda Expressions

This proposal introduces a new extension `-XLambdaGuards`, which allows guards
to appear in lambda expressions.

## Motivation

Let's say we have the following definition of `take'`:

```Haskell
take' :: Int -> [a] -> [a]
take' _ []                   = []
take' n (x : xs) | n > 0     = x : take (n - 1) xs
                 | otherwise = []
```

If we want to write this definition using `foldr` instead of explicit
recursion, and if we want to keep the guards without binding the required
anonymous function to a name using `let` or `where`, we need to use some
syntactic hackery if we wish to stick to Haskell 2010:

```Haskell
take' :: Int -> [a] -> [a]
take' = flip $ flip foldr (const []) $
  \x more n -> case () of _ | n > 0     -> x : more (n - 1)
                            | otherwise -> []
```

GHC already provides extensions to make this a lot more palatable, firstly by
allowing the second `$` to be omitted thanks to `-XBlockArguments`, and
secondly through `-XMultiWayIf`:

```Haskell
take' :: Int -> [a] -> [a]
take' = flip $ flip foldr (const [])
  \x more n -> if | n > 0     -> x : more (n - 1)
                  | otherwise -> []
```

The next logical step is to eliminate the remaining boilerplate code, and
allowing lambdas to have guards, as in


```Haskell
take' :: Int -> [a] -> [a]
take' = flip $ flip foldr (const [])
  \x more n | n > 0     -> x : more (n - 1)
            | otherwise -> []
```

Not only is this the shortest, and arguably most easily readable, of the
`take'` variants using `foldr`, it is also more reminiscent of the original
version of `take'`.

This illustrates a more general advantage: With this change, lambda expression
syntax would more closely mirror the syntax of function declarations, making
the language more consistent.

[A stackoverflow thread from
2010](https://stackoverflow.com/questions/3416475/haskell-guards-on-lambda-functions)
asks about the existence of the proposed syntax, suggesting that it is actually
counter-intuitive to *not* allow it. In the answers, one can find various
workarounds. The fact that people have bothered finding several different
workarounds suggests that it is something that users want to use, but currently
cannot. Notably some of these workarounds rely on `-XLambdaCase`, which means
they couldn't effectively be used for the `take'` example, since `-XLambdaCase`
only works with lambdas that take a single argument.

Both `-XMultiWayIf` and `-XLambdaCase` provide precedents of similar extensions
being introduced into GHC.

## Proposed Change Specification

A new extension `-XLambdaGuards` is introduced that enables the proposed
syntax.

The proposed syntax will be desugared in the same way as the `-XMultiWayIf`
workaround, and has the same semantics, i.e.

```Haskell
\x y z | guard0 -> term0
       | guard1 -> term1
       | guard2 -> term2
```

is semantically equivalent to

```Haskell
\x y z -> if | guard0 -> term0
             | guard1 -> term1
             | guard2 -> term2
```

with `-XMultiWayIf`.

The syntax of lambdas is changed from (conceptually; this is not an exact
reflection of the GHC source code)

```
"\" pattern {pattern} "->" exp
```

to

```
"\" pattern {pattern} ( "->" exp | gdpat {gdpat} )
```

where `gdpat` is
```
"|" guard {"," guard} "->" exp
```

## Examples

The Motivation section mentions one example:

```Haskell
take' :: Int -> [a] -> [a]
take' = flip $ flip foldr (const [])
  \x more n | n > 0     -> x : more (n - 1)
            | otherwise -> []
```

Note that this is a multi-argument lambda expression.

The proposed syntax makes the lambda expression the [above-mentioned
stackoverflow](https://stackoverflow.com/questions/3416475/haskell-guards-on-lambda-functions)
question asks for possible:

```Haskell
\k
    | k < 0     -> "negative"
    | k == 0    -> "zero"
    | otherwise -> "positive"
```

This lambda expression can be used instead of any of the workarounds mentioned
in the thread and touched upon in the Motivation section.

Another example that's mentioned in the thread is this:

```Haskell
{-# ScopedTypeVariables #-}
o <- hGetContents e `catch` (\case (e :: IOException) | isEOFError e -> return "")
```

This could also be simplified with the proposed syntax:

```Haskell
{-# ScopedTypeVariables #-}
o <- hGetContents e `catch` (\(e :: IOException) | isEOFError e -> return "")
```

Pattern guards could also be used, as in

```Haskell
\maybeFirstName maybeLastName
  | Just firstName <- maybeFirstName
  , Just lastName <- maybeLastName
  , all isAlpha firstName
  , all isAlpha lastName             -> Right $ FullName firstName lastName
  | otherwise                        -> Left "Invalid name"
```

While the above could have been done conveniently with the existing
`-XLambdaCase` if it were a single-parameter lambda, this cannot be done so
easily with a multi-parameter lambda, as is the case here.

## Effect and Interactions

Enabling the extension enables users to use the suggested syntax. There should
be no interactions with anything else.

Since the proposed syntax currently results in a parse error, the change is
fully backwards compatible.

## Costs and Drawbacks

It is one additional syntactic construct to maintain, however the maintenance
cost should be fairly low due to the similarity to already existing constructs.

While this also means one additional construct to learn for beginners, the
syntax is consistent with similar constructs in the existing language, and, as
mentioned in the Motivation section, might thus actually make the language *more*
intuitive.

## Alternatives

The alternative is not to introduce this extension, thus forcing people to
use one of the noisier workarounds instead.

The name of the extension could be something else, though `-XLambdaGuards` is
consistent with the already existing `-XLambdaCase`.

## Unresolved Questions

None

## Implementation Plan

I (Jakob Brünker) will implement this proposal.
