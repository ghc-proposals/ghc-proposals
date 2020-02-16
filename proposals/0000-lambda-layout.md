---
author: Jakob Brünker
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/302).

# Layout and guards in lambda expressions

This proposal introduces a new extension `-XLambdaLayout`, which allows lambda
expressions to have implicit layout as well as guards.

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
    - If lambda expressions could have multiple clauses as well as guards, they
      could be used instead of `-XLambdaCase`, and they can already match on
      multiple patterns.
 2. The extension `-XMultiWayIf` essentially introduces standalone guards,
    simplifying the use of guards that aren't at the outermost level of a
    function declaration or case-expression. Among other things, this made it
    easier to use guards inside of lambda expressions.
    - If lambda expressions could have guards and weren't required to have at
      least one parameter, they could be used instead of `-XMultiWayIf`. This
      includes all uses of `-XMultiWayIf`, not just those inside of lambdas
      (see Example section).
 3. During the implementation of `-XLambdaCase`,
    [some suggested](https://gitlab.haskell.org/ghc/ghc/issues/4359#note_51110)
    allowing lambda expressions to have multiple clauses. This was not
    implemented: The most obvious approach of turning `\` into a layout herald
    had the disadvantage of making some common idioms invalid.
    - This can be circumvented by having `\` only be a layout herald if a
      newline character immediately follows it.

This proposal, then, aims to overcome the shortcomings of lambda expressions
and allow them to have the same capabilities as function declarations,
obviating the need for `-XMultiWayIf`, `-XLambdaCase`, and potentially even
most of function declaration syntax, if a user wishes to use lambda expressions
instead (see Example section).

By combining the functionality of several features into one existing feature in
a way that's consistent with the rest of the language, they enable users who
wish to use this functionality to work with a simpler and more consistent
language.

## Proposed Change Specification

A new extension `-XLambdaLayout` is introduced. Under this extension, lambda
expressions can have guards. Unlike the current behavior with `-XMultiWayIf`,
these guards do not introduce an implicit layout, since there is an alternative
way of introducing one (see next paragraph). Lambda expressions are also
allowed to have zero parameters.

Furthermore, if after the `\`, a newline occurs before both the first pattern and
guard, the `\` serves as a layout herald. (This means other whitespace or comments
could appear between the `\` and the newline character. For simplicity, this is not
reflected in the BNF.) Multiple clauses of the form `clause` (see BFN of changed
syntax) may then be used in the following lines.

Zero clauses are not permitted. This means that with `-XEmptyCase`,
`-XLambdaCase` still has one (albeit rarely used) construct that cannot be
replaced by a lambda expression without making it slightly longer, though
where that is acceptable,
`\case {}` can (even today) be replaced by `\x -> case x of {}`. This shortcoming
could potentially
be addressed in a future proposal, for example by adding absurd patterns, to
provide a more general solution. Note that the `\case {}` construct only works
for functions of one argument.

Like the existing behavior for alternatives in case- and
`\case`-expressions, and equations in function declaration syntax, it is
possible to use `where` clauses within each clause of a multi-clause lambda
expression.

Explicit layout using braces can be used instead of the implicit layouts.

### BNF of changed syntax

```
clause = { pattern } ( "->" exp | guardAlt { guardAlt } ) [ "where" { whereClause } ]
guardAlt = "|" guard { "," guard } "->" exp
lambda = "\\" clause
         -- multiple clauses have to follow the layout rules with respect to indentation
         -- i.e. each new clause has to start at the same level of indentation as the first one
         -- All clauses must have the same number of patterns
       | "\\" "\n" clause { "\n" clause }
         -- explicit layout is also possible
       | "\\" "{" clause { ";" clause } "}"
```

## Examples

It is now possible to use guards in lambdas, which allows shortening some
definitions:

```Haskell
{-# LANGUAGE MultiWayIf, BlockArguments #-}
take' :: Int -> [a] -> [a]
take' = flip $ flip foldr (const [])
  \x more n -> if | n > 0 -> x : more (n - 1)
                  | otherwise -> []

-- becomes

take' :: Int -> [a] -> [a]
take' = flip $ flip foldr (const [])
  \x more n | n > 0 -> x : more (n - 1)
            | otherwise -> []
```

Lambdas can always replace `-XMultiWayIf`:

```Haskell
foo = bar baz if | g1 -> a
                 | g2 -> b

-- with -XBlockArguments becomes

foo = bar baz \ | g1 -> a
                | g2 -> b
```

`\case` can be replaced by a lambda expression:

```Haskell
\case
  Bar baz -> Just baz
  Quux -> Nothing

-- becomes

\
(Bar baz) -> Just baz
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

\
(Just 4) 3 False -> 42
_ _ _ -> 0
```

Lambda expressions can be used instead of regular function declaration syntax,
potentially resulting in more concise definitions:

```Haskell
extremelyLengthyFunctionIdentifier (Just a) False = Just 42
extremelyLengthyFunctionIdentifier (Just a) True  = Just (a / 2)
extremelyLengthyFunctionIdentifier _        _     = Nothing

-- becomes

extremelyLengthyFunctionIdentifier = \
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

-- note that the first `where` clause belongs to the first lambda expression
-- clause, rather than the function declaration, because it is indented further
foo = \
  (Just x) | x < 0 -> ...
           | let y = blah + 1 -> ...
    where blah = x + magicNumber
  Nothing -> magicNumber
  where
    magicNumber = 5
```

Common idioms still work if no newline follows the `\`:

```Haskell
foo >>= \a ->
bar >>= \b ->
pure $ a + b

baz = \x -> do
  a x
  b
```

These wouldn't work if `\` always introduced an implicit layout.

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

-- with -XLambdaLayout

forAll mkFold $ \(l0,l1,l2) -> do
  let {- various bindings -}
  \ | end0 && col1 <= col0 -> prs p s `shouldFailWith`
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

-- with -XLambdaLayout and -XBlockArguments

return \
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

-- with -XLambdaLayout
_prefixNS = \
    (Left  l) -> S l
    (Right x) -> case x of Here fv -> Z @_ @v @start fv
_breakNS = \
    (Z x) -> Right (Here x)
    (S x) -> Left x
```

recursors-0.1.0.0/Control/Final.hs
```Haskell
map (\case PlainTV n    -> n
           KindedTV n _ -> n) binders
           
-- With -XLambdaLayout - note that a newline has to be introduced

map (\
  (PlainTV n)    -> n
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

-- With -XLambdaLayout - this makes use of the capability to have multiple parameters

printGender :: Language -> Gender -> Text
printGender = \
  English Male   -> "Male"
  English Female -> "Female"
  Chinese Male   -> "男性"
  Chinese Female -> "女性"
```

## Effect and Interactions

Enabling the extension enables users to use the suggested syntax. This obviates
the need for `-XMultiWayIf` and `-XLambdaCase`.

Since these changes are guarded behind an extension, no code will break. Even
if the extension were enabled on all hackage packages, only three of them use a
newline immediately after a `\` in a lambda expression, and all instances of
this that would not work with the extension could be easily fixed by indenting
a line one level deeper.

## Costs and Drawbacks

It is one additional syntactic construct to maintain, however the maintenance
cost should be fairly low due to the similarity to already existing constructs.

While this also means one additional construct to learn for beginners, the
syntax is consistent with similar constructs in the existing language, and as
such users might be surprised that parts of it *don't* work at the moment.

Using newlines as indicator for whether or not there should be implicit layout
is not part of any other construct of Haskell, and may thus be surprising,
initially.

## Alternatives

 - This proposal is about several related but fairly independent changes. It
   would be possible to only implement a subset of them, though this would
   sacrifice parity of lambda expressions with function declarations.

 - For the same reason, it would be possible to introduce more than one extension
   for these changes.

 - The name of the extension could be something else, for example
   `-XExtendedLambdas`.
   
 - Guards in lambdas (or in general) could introduce layout, similar to how it works
   in `MultiWayIf` today. This could avoid some unintuitive behavior when multiple
   single-clause lambdas with guards are immediately chained, but that comes at the
   expense of additional complexity. In cases where that issue does arise, a
   layout-introducing lambda expression (i.e. one with a newline) can always be used
   instead.
   
 - An additional keyword (e.g. `mcase`) and the corresponding analogue for -XLambdaCase
   (e.g. `\mcase`) could be introduced to allow multiple patterns in case and lambda
   case expressions. However, this would increase the complexity of the syntax instead
   of allowing the syntax to become more consistent. Multiple patterns for regular case
   expressions also are not as much of an improvement, since tuples can already be used
   effectively for them.

## Implementation Plan

I (Jakob Brünker) will implement this proposal.
