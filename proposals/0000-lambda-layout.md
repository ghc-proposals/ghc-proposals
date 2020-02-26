---
author: Jakob Brünker
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/302).

# Multi-way lambda expressions (`\ of`)

This proposal introduces a new extension `-XMultiWayLambda`, which introduces a
lambda-like expression capable of handling multiple clauses as well as guards.

## Motivation

In Haskell 2010, there are two syntaxes to define a function: via the function
definition syntax, or via lambda expressions. The most obvious difference is
that the former assigns a name to the function, whereas the latter can be used
for anonymous functions. However, the differences go significantly beyond that:

 - Lambda expressions can only have a single clause, function declarations
   can have an arbitrary non-zero number of equations
 - Lambda expressions cannot have guards
 - Lambda expressions must have at least one parameter

There have been multiple attempts in the past to bring the capabilities of
lambda expressions closer to those of function declarations:

 1. The extension `-XLambdaCase` introduces a `\case` construct which allows
    lambda expression to have multiple clauses, however, only one pattern can
    be matched on. Like a regular case-expression, this can also have guards.
    [During its
    implementation](https://gitlab.haskell.org/ghc/ghc/issues/4359#note_44819)
    as well as [after it](https://github.com/ghc-proposals/ghc-proposals/pull/18),
    there were attempts to make it possible to match on multiple patterns. No
    solution was found, in part because this would make it different from
    regular case-expressions.
    - If there were an expression that had pattern matching syntax more similar
      to lambda expressions but which could also have guards and multiple clauses,
      it could be used instead of `-XLambdaCase` and would be able to match on
      multiple patterns.
 2. The extension `-XMultiWayIf` essentially introduces standalone guards,
    simplifying the use of guards that aren't at the outermost level of a
    function declaration or case-expression. Among other things, this made it
    easier to use guards inside of lambda expressions.
    - If there were an expression similar to lambda expressions that could have
      guards and wasn't required to have at
      least one parameter, it could be used instead of `-XMultiWayIf`. This
      includes all uses of `-XMultiWayIf`, not just those inside of lambdas
      (see Example section).
 3. During the implementation of `-XLambdaCase`,
    [some suggested](https://gitlab.haskell.org/ghc/ghc/issues/4359#note_51110)
    allowing lambda expressions to have multiple clauses. This was not
    implemented: The most obvious approach of turning `\` into a layout herald
    had the disadvantage of making some common idioms invalid.
    - This can be circumvented by introducing a new expression that isn't required
      to be backwards compatible with existing idioms.

This proposal, then, aims to overcome the shortcomings of lambda expressions
and allows a new expression to have the same capabilities as function declarations,
obviating the need for `-XMultiWayIf`, `-XLambdaCase`, and potentially even
most of function declaration syntax, if a user wishes to use the new expression
instead (see Example section). This new expression is introduced with the sequence `\ of`. It
behaves similarly to lambda expressions, except with layout, guards and multiple clauses; for example,
here is a definition of filter in terms of fix:

```haskell
filter = fix \of filter' _ []                 -> []
                 filter' p (x:xs) | p x       -> x : rest
                                  | otherwise ->     rest
                   where rest = filter' p xs
```

The `\of` can be read as "*lambda* comprised *of* the following cases".

By combining the functionality of several features into one feature in
a way that's consistent with the rest of the language, it enables users who
wish to use this functionality to work with a simpler and more consistent
language.

## Proposed Change Specification

A new extension `-XMultiWayLambda` is implemented. Under this extension, a new
expression is enabled, introduced by the token sequence `\ of`. The whitespace between  `\` and `of`
is optional and may contain an arbitrary sequence of whitespace characters.
`\ of` behaves in a way largely similar to `\`, but it is a layout herald, can have multiple
clauses, and may contain guards (see BNF for details).

Zero clauses are not permitted, as the expression would be ambiguous.
(See `Alternatives` section for details.)
This means that with `-XEmptyCase`,
`-XLambdaCase` still has one (albeit rarely used) construct that cannot be
replaced by a `\ of`-expression, though
`\case {}` can (even today) be replaced by `\x -> case x of {}`. This shortcoming
could potentially
be addressed in a future proposal, for example by adding absurd patterns, to
provide a more general solution. Note that the `\case {}` construct only works for matching
on a single pattern.

Like the existing behavior for alternatives in case- and
`\case`-expressions, and equations in function declaration syntax, it is
possible to use `where` clauses within each clause of a `\ of`-expression.

Explicit layout using braces can be used instead of the implicit layout.

As with function declaration equations, all clauses must have the same number of patterns.

Once the [*Binding type variables in lambda-expressions*](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0155-type-lambda.rst) proposal is being implemented,
with `-XTypeAbstractions`, `\ of`-expressions will also be able to bind type
variables.

### BNF of changed syntax

*ofexp* → `\` `of` `{` *ofalts* `}`  
*ofalts* → *ofalt₁* `;` … `;` *ofaltₙ*  
*ofalt* → [ *apat₁* … *apatₙ* ] `->` *exp* [`where` *decls*]  
 | [ *apat₁* … *apatₙ* ] *gdpat* [`where` *decls*]
 
Aside from the explicit layout using `{`, `}`, and `;`, implicit layout as described in the Haskell
report can also be used.

## Examples

Using multi-way lambda expressions with guards allows shortening some definitions:

```Haskell
{-# LANGUAGE MultiWayIf, BlockArguments #-}
take' :: Int -> [a] -> [a]
take' = flip $ flip foldr (const [])
  \x more n -> if | n > 0 -> x : more (n - 1)
                  | otherwise -> []

-- becomes

take' :: Int -> [a] -> [a]
take' = flip $ flip foldr (const [])
  \of x more n | n > 0 -> x : more (n - 1)
               | otherwise -> []
```

Multi-way lambdas can always replace `-XMultiWayIf`:

```Haskell
foo = bar baz if | g1 -> a
                 | g2 -> b

-- with -XBlockArguments becomes

foo = bar baz \of | g1 -> a
                  | g2 -> b
```

`\case` can be replaced by a `\ of`-expression:

```Haskell
\case Bar baz -> Just baz
      Quux -> Nothing

-- becomes

\of (Bar baz) -> Just baz
    Quux -> Nothing
```

Lambda expressions are more powerful since they can match on multiple patterns:

```Haskell
-- \case can't be used here!
-- At least not as easily as in the previous example
\foo bar baz -> case (foo, bar, baz) of
  (Just 4, 3, False) -> 42
  _ -> 0

-- becomes

\of
  (Just 4) 3 False -> 42
  _ _ _ -> 0
```

`\ of`-expressions can be used instead of regular function declaration syntax,
potentially resulting in more concise definitions:

```Haskell
extremelyLengthyFunctionIdentifier (Just a) False = Just 42
extremelyLengthyFunctionIdentifier (Just a) True  = Just (a / 2)
extremelyLengthyFunctionIdentifier _        _     = Nothing

-- becomes

extremelyLengthyFunctionIdentifier = \of
  (Just a) False -> Just 42
  (Just a) True  -> Just (a / 2)
  _        _     -> Nothing
```

This also makes it possible to have `where` bindings that scope over multiple
equations

```Haskell
-- have to repeat the definition of `magicNumber` or place it outside the definition of
-- foo
foo (Just x) | x < 0 = ...
             | let y = blah + 1 = ...
  where blah = x + magicNumber
        magicNumber = 5
foo Nothing = magicNumber
  where magicNumber = 5

-- becomes

-- note that the first `where` clause belongs to the first `\ of`-expression
-- clause, rather than the function declaration, because it is indented further
foo = \of
  (Just x) | x < 0 -> ...
           | let y = blah + 1 -> ...
    where blah = x + magicNumber
  Nothing -> magicNumber
  where
    magicNumber = 5
```

To illustrate with some real-world examples, this section shows
how some snippets found on hackage would look if they used this new syntax:
  
megaparsec-tests-8.0.0/tests/Text/Megaparsec/Char/LexerSpec.hs
```Haskell
forAll mkFold $ \(l0,l1,l2) -> do
  let {- various bindings -}
  if | end0 && col1 <= col0 -> prs p s `shouldFailWith`
       errFancy (getIndent l1 + g 1) (ii GT col0 col1)
     | end1 && col2 <= col0 -> prs p s `shouldFailWith`
       errFancy (getIndent l2 + g 2) (ii GT col0 col2)
     | otherwise -> prs p s `shouldParse` (sbla, sblb, sblc)

-- with -XMultiWayLambda

forAll mkFold $ \(l0,l1,l2) -> do
  let {- various bindings -}
  \of | end0 && col1 <= col0 -> prs p s `shouldFailWith`
        errFancy (getIndent l1 + g 1) (ii GT col0 col1)
      | end1 && col2 <= col0 -> prs p s `shouldFailWith`
        errFancy (getIndent l2 + g 2) (ii GT col0 col2)
      | otherwise -> prs p s `shouldParse` (sbla, sblb, sblc)
```

caramia-0.7.2.2/src/Graphics/Caramia/Texture.hs:
```Haskell
return $ if
    | result == GL_CLAMP_TO_EDGE -> Clamp
    | result == GL_REPEAT -> Repeat
    | otherwise -> error "getWrapping: unexpected wrapping mode."

-- with -XMultiWayLambda and -XBlockArguments

return \of
    | result == GL_CLAMP_TO_EDGE -> Clamp
    | result == GL_REPEAT -> Repeat
    | otherwise -> error "getWrapping: unexpected wrapping mode."
```

red-black-record-2.1.0.3/lib/Data/RBR/Internal.hs
```Haskell
_prefixNS = \case
    Left  l -> S l
    Right x -> case x of Here fv -> Z @_ @v @start fv
_breakNS = \case
    Z x -> Right (Here x)
    S x -> Left x

-- with -XMultiWayLambda
_prefixNS = \of
    (Left  l) -> S l
    (Right x) -> case x of Here fv -> Z @_ @v @start fv
_breakNS = \of
    (Z x) -> Right (Here x)
    (S x) -> Left x
```

recursors-0.1.0.0/Control/Final.hs
```Haskell
map (\case PlainTV n    -> n
           KindedTV n _ -> n) binders
           
-- With -XMultiWayLambda

map (\of (PlainTV n)    -> n
         (KindedTV n _) -> n) binders
```

roc-id-0.1.0.0/library/ROC/ID/Gender.hs
```Haskell
printGender :: Language -> Gender -> Text
printGender = \case
  English -> printGenderEnglish
  Chinese -> printGenderChinese

printGenderEnglish :: Gender -> Text
printGenderEnglish = \case
  Male   -> "Male"
  Female -> "Female"

printGenderChinese :: Gender -> Text
printGenderChinese = \case
  Male   -> "男性"
  Female -> "女性"

-- With -XMultiWayLambda - this makes use of the capability to have multiple parameters

printGender :: Language -> Gender -> Text
printGender = \of
  English Male   -> "Male"
  English Female -> "Female"
  Chinese Male   -> "男性"
  Chinese Female -> "女性"
```

## Effect and Interactions

Enabling the extension enables users to use the suggested syntax. This obviates
the need for `-XMultiWayIf` and `-XLambdaCase`.

As `of` is already a keyword, no currently allowed syntax is stolen by this extension,
and the behavior of no currently legal program would be changed with the extension
enabled.

## Costs and Drawbacks

It is one additional syntactic construct to maintain, however the maintenance
cost should be fairly low due to the similarity to already existing constructs.

While this also means one additional construct to learn for beginners, the
syntax is consistent with similar constructs in the existing language, and as
such users might be surprised that a construct with these capabilities
doesn't yet exist.

## Alternatives

 - Zero clauses could be permitted. In this case, however, a way would have to be found
   to indicate how many arguments a given `\ of`-expression matches on, as otherwise, it would
   be ambiguous.
   The number of arguments a `\ of`-expression pattern matches on becomes obvious from the
   clauses, e.g. `\ of a b -> ...` clearly matches on two arguments. Without clauses, this remains
   unclear. This means it would also be unclear whether the patterns are non-exhaustive:
   Consider the expression `\of {} :: Bool -> Void -> a`. If the expression is supposed to match on
   both arguments, the patterns are exhaustive. If it is only supposed to match on the first argument
   and evaluate to a funtion of type `Void -> a`, it is not exhaustive.
   With `\case {}` this problem doesn't arise, since it always matches on exactly one argument,
   and similarly for `case x of {}`, which only matches on `x`.
   A syntax to resolve this has been proposed in the discussion: `(\of)` for matching on no arguments,
   `(\of _)` for one, `(\of _ _)` for two, and so on.
   
 - Regular lambda expressions could be extended to use layout and guards, however,
   this necessitates some potentially controversial decision on when exactly to
   herald layout, since always doing so would disallow existing idioms; these would not
   be legal when the extension is enabled:
   ```haskell
   do
     f a >>= \b ->
     g b >>= \c ->
     h c
     
   foo = \x -> do
     a x
     b
   ```
   Two alternatives would be to only herald layout
     - if a newline immediately follows the `\` or
     - if, given that token `t` is the token after `\`, the line below the one with `t` has
       the same indentation as or greater than `t`

   Both of these would avoid the problem, but both rules are dissimilar from how layout heralding
   is handled in other Haskell constructs.

## Implementation Plan

I (Jakob Brünker) will implement this proposal.
