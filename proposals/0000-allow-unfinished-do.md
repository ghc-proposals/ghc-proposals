---
author: Alejandro
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/0>).
**After creating the pull request, edit this file again, update the number in
the link, and delete this bold sentence.**

# Unfinished `do` blocks

The syntax of `do` blocks in Haskell requires a final expression in order to even *parse*. This means that half-baked `do` blocks stops the pipeline, and no feedback can be gathered about name resolution or typing. This scenario arises quite often during interactive development. This proposal introduces a new flag in the compiler, `-fallow-unfinished-do`, which essentially treats those unfinished `do` blocks as having a hole at the end.

## Motivation

The [syntax of `do` blocks](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-470003.14) in Haskell *requires* a final expression in order to be syntactically correct. However, during development, it is quite common to have "half-baked" `do` blocks in which that final expression is missing.

```haskell
do putStr "what is your name?"
   name <- getLine
   -- this do block is syntactically wrong
```
```
<interactive>:3:4: error:
    The last statement in a 'do' block must be an expression
      name <- getLine
```

Since the error happens in the *parsing* phase, this means that it is impossible to get any feedback on following phases (name resolution, typing) until that error is solved. This problem manifests even more when using interactive development tools, as pointed out by [Neil Mitchell](https://neilmitchell.blogspot.com/2020/05/ghc-unproposals.html).

## Proposed Change Specification

There is a new flag in the compiler, `-fallow-unfinished-do`, which allows such "unfinished" `do`s to go over the parsing phase. Conceptually, they are treated as having a final typed hole, but the error reports the location of the entire `do` block instead. This is enough to block the compiler to go over the typing phase, but good enough for interactive development.

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

At the end of the parsing phase, if `-fallow-unfinished-do` is *not* enabled, a parsing error is raised, as usual, but with a hint to enable the extension.

```
The last statement in a 'do' block must be an expression
Use -fallow-unfinished-do to allow this
```

If `-fallow-unfinished-do` is enabled, the pipeline continues. If the compiler gets to the typing phase, it reports back the inferred type of the required final expression, and any useful context (as it would do with a hole).

As described below in the [implementation plan](#implementation-plan), GHC already allows unfinished `do` in its syntax.

## Examples

The example in the [motivation](#motivation) section would produce the following error message when `-fallow-unfinished-do` is enabled:

```
• Found unfinished do block
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

The effect of this proposal is that, when the flag is enabled, the programmer may get more useful feedback than the one given now by GHC in this kind of scenarios. Furthermore, tools for interactive development may switch on this flag by default, as they do now with other such as `-fdefer-type-errors`.

## Costs and Drawbacks

The maintenance cost seems quite low, given that the change is quite local.

One drawback is that tools for interactive development sometime hard-code the shape of the error messages. If we change the message given by a missing final expression when this flag is off (to hint about enabling it), these tools may break.

## Alternatives

The main alternative is to keep the *status quo*. Some people (including myself) have learnt that whenever you start a `do` block you must immediately write `undefined` or a hole afterwards, in order not to break the interactive development cycle. However, it feels weird that this problem occurs given that GHC can detect the problem with a lot of precision.

## Unresolved Questions

1. Should we treat completely empty `do` block in some special way? My feeling is **no**.
2. Should we have a flag to control this behavior? It feels that nothing wrong may happen if we allow the compiler pipeline to continue until the typing phase. Of course, we would be deviating from the Report.
3. Should this behavior be controlled by a language extension instead? I think **no**, because this does not make more programs to be accepted (we still get an error, just a bit later than before).
4. Should we have a similar behavior for other unfinished productions? For example, conditional expressions.

## Implementation Plan

It turns out that GHC already checks that `do` blocks end with an expression in a separate phase! We can read in the [`Parser.y`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Parser.y#L3265) file (although `ParseUtils` no longer exists).

```haskell
-- The last Stmt should be an expression, but that's hard to enforce
-- here, because we need too much lookahead if we see do { e ; }
-- So we use BodyStmts throughout, and switch the last one over
-- in ParseUtils.checkDo instead
```

This suggests that the implementation should be quite straightforward.

