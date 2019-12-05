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
   can have an arbitrary number of equations (apart from zero)
 - Lambda expressions cannot have guards
 - Lambda expressions must have at least one parameter

There have been multiple attempts in the past to bring the capabilities of
lambda expressions closer to the of function declarations:

 1. The extension `-XLambdaCase` introduces a `\case` construct which allows
    lambda expression to have multiple clauses, however, only one pattern can
    be matched on. Like a regular case-expression, this can also have guards.
    [During its
    implementation](https://gitlab.haskell.org/ghc/ghc/issues/4359#note_44819)
    and [after it](https://github.com/ghc-proposals/ghc-proposals/pull/302),
    there were attempts to make it possible to match on multiple patterns. No
    solution was found, in part because this would make it different from
    regular case-expressions.
    - If lambda expressions could have multiple clauses and guards, they
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
expressions can have guards. Like the current behavior with `-XMultiWayIf`,
these guards introduce an implicit layout. Lambdas expressions are also
allowed to have zero parameters.

Furthermore, if after the `\`, a newline occurs before the first pattern and
guard the `\` serves as a layout herald. (This means other whitespace or comments
could appear between the `\` and the newline character. For simplicity, this is not
reflected in the BNF.) Multiple clauses of the form `clause` (see BFN of changed
syntax) may then be used in the following lines.

Zero clauses are not permitted. This means that with `-XEmptyCase`,
`-XLambdaCase` still has one (albeit rarely used) construct that cannot be
replaced by a lambda expression without making it slightly longer, though
`\case {}` *can* be replaced by `\x -> case x of  {}`. This could potentially
be addressed in a future proposal, for example by adding absurd patterns, to
provide a more general solution. Note that the `\case {}` construct only works
for function of one argument.

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
-- have to repeat the definition of bar or place it outside the definition of
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

## Unresolved Questions

 - The reason `-XMultiWayIf` is layout-aware is due to examples like this:

   ```Haskell
   x = if | False -> if | False -> 1
                        | False -> 2
          | True -> 3
   ```

   This, intuitively, should evaluate to `3`, but without implicit layout, it
   fails. The same reasoning applies to the `\ | pat -> expr` syntax. However,
   for lambda expressions that introduce an implicit layout due to a newline,
   this is unnecessary, just like it is unnecessary for the guards in
   case-expressions, since `case ... of` is already a layout-herald.

   Should guards, for consistency, introduce implicit layout in those lambda
   expressions as well? If so, should all other places where guards occur (case,
   `\case`, function declaration) be adjusted in the same way, again for
   consistency, as long as the extension is enabled?

 - Is there an elegant alternative design that doesn't rely on a newline
   playing a significant role, while still allowing the snippets in the last
   example to work?

   @Ericson2314 suggests a more general approach of enabling inner layout
   heralds to dominate outer ones, as in

   ```Haskell
   do x do
    y z
   ```

   Would an approach along these lines solve the above-mentioned problems, and
   would it be feasible to implement in ghc?

## Implementation Plan

I (Jakob Brünker) will implement this proposal.
