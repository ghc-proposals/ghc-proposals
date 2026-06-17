---
author: Alejandro Serrano Mena
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/333).

# Defer parse errors

The syntax of `do` blocks in Haskell requires a final expression in order to even *parse*, lists cannot start with a comma, these are all examples of errors which GHC recognizes as *parse* errors nowadays. This means that half-baked `do` blocks, or a misplaced comment before the first element of a list, stop the pipeline, and no feedback can be gathered about name resolution or typing. This scenario arises quite often during interactive development. Our proposal is to treat those unfinished elements as having holes filling the missing places.

## Motivation

The [syntax of `do` blocks](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-470003.14) in Haskell *requires* a final expression in order to be syntactically correct. However, during development, it is quite common to have "half-baked" `do` blocks in which that final expression is missing.

```haskell
do putStr "what is your name?"
   name <- getLine
   -- this do block is syntactically wrong
```
```
The last statement in a 'do' block must be an expression
  name <- getLine
```

Since the error happens in the *parsing* phase, this means that it is impossible to get any feedback on following phases (name resolution, typing) until that error is solved. This problem manifests even more when using interactive development tools, as pointed out by [Neil Mitchell](https://neilmitchell.blogspot.com/2020/05/ghc-unproposals.html).

Another instance of this problem is that lists literals do not accept an *initial* comma. However, suppose we are developing code with uses a list, following a common code style within the Haskell community.

```haskell
thing [ a
      , b
      , c
      ]
```

Then we turn the first element into a comment, as follows.

```haskell
thing [ -- a
```

We have the same problem as with `do` blocks: this missing item makes the code syntactically wrong, leaving out any possibility of further analysis by the compiler.

A third instance is having one type signature without the corresponding definition. This also happens when writing code, as many people write the type signature and then the implementation.

```haskell
f :: Int -> Int
```
```
The type signature for ‘f’ lacks an accompanying binding
```

## Proposed Change Specification

Instead of flagging a parse error, the compiler allows such "unfinished" sequences of items to go over the parsing phase. Conceptually, they are treated as having a (typed) hole wherever the item is missing, but the error reports the location of the entire block instead. This is enough to allow the compiler to continue until the typing phase, which is great for interactive development. These holes are can be taken until runtime if `-fdefer-type-errors` is also enabled.

### `do` blocks

Considering the syntax [the corresponding section](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-470003.14) of the Haskell 2010 Report, we make the following change:

```diff
  lexp  → do { stmts }
- stmts → stmt1 … stmtn exp [;]
+ stmts → stmt1 … stmtn [exp] [;]
  stmt  → ...
```

The translation section is updated with the rule:

```diff
+ do { stmt } = do { stmt ; _end }  (where '_end' is a fresh hole)
```

As described below in the [implementation plan](#implementation-plan), GHC already allows unfinished `do` in its syntax.

### List literals

Considering the syntax for [list literals](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-340003.7) of the Haskell 2010 Report, we make the following change:

```diff
  aexp → [ exp1 , … , expk ]
+      | [ , exp1, … , expk ]
```

The translation section is updated with the rule:

```diff
+ [ , e1, …, ek ] = [ _elt , e1, …, ek ]
```

### Missing implementations

For those cases in which there is a type signature, but not a binding, we also want to replace it by a single implementation with a hole. This enables the compiler to continue. However, we do *not* want to introduce yet another message, since the already-existing one already points the problem correctly.

## Examples

The first example in the [motivation](#motivation) section would produce the following error message instead of the current one:

```
• Found unfinished do block
  (the last statement must be an expression)
    with inferred type :: IO b
    at <interactive>:(2,1)-(4,7)
  Where: ‘b’ is a rigid type variable bound by
            the inferred type of it :: IO b
            at <interactive>:(10,1)-(12,7)
• In an equation for ...
• Relevant bindings include
    name :: String (bound at <interactive>:3:4)
```

## Effect and Interactions

The effect of this proposal is that the programmer may get more useful feedback than the one given now by GHC in this kind of scenarios. Furthermore, tools for interactive development may switch on this flag by default, as they do now with other such as `-fdefer-type-errors`.

## Costs and Drawbacks

The maintenance cost seems quite low, given that the change is quite local.

One drawback is that tools for interactive development sometime hard-code the shape of the error messages. If we change the message, some of these tools may break.

## Alternatives

The main alternative is to keep the *status quo*. Some people (including myself) have learnt that whenever you start a `do` block you must immediately write `undefined` or a hole afterwards, in order not to break the interactive development cycle. However, it feels weird that this problem occurs given that GHC can detect the problem with a lot of precision.

## Unresolved Questions

### Other unfinished productions

Another usual suspect for error which are signalled as parse errors but occur often during interactive development is unfinished bind blocks.

```haskell
f = g 3
  where g =
```

Would it be possible to turn this into a hole too? My fear is that the syntax of Haskell, with its complicated layout rules, may require too much lookahead for this to work. Other than that, it would be great that we could obtain.

```
• Found missing implementation of 'g'
    with inferred type :: Num a => a -> b
```

## Implementation Plan

It turns out that GHC already checks that `do` blocks end with an expression in a separate phase! We can read in the [`Parser.y`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Parser.y#L3265) file (although `ParseUtils` no longer exists).

```haskell
-- The last Stmt should be an expression, but that's hard to enforce
-- here, because we need too much lookahead if we see do { e ; }
-- So we use BodyStmts throughout, and switch the last one over
-- in ParseUtils.checkDo instead
```

This suggests that the implementation should be quite straightforward.

The syntax of [list expressions](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Parser.y#L3024) is a bit more convoluted, since we have many different kinds (literals, comprehensions, sequences). But conceptually a new production rule:

```haskell
lexps :: { forall b. DisambECP b => PV [Located b] }
        : ...
        | ',' lexps
```

should be enough to make this work.