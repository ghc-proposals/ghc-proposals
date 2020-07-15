---
author: Chris Smith <cdsmith@gmail.com>
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/346).

# Bracket-Sensitive Layout Rule

This proposal makes the following valid Haskell code:

```
{-# LANGUAGE BracketSensitiveLayout #-}

foo = (
  1,
  2
)
```

## Motivation

In most other programming languages, it is commonplace to align closing
parentheses, brackets, or braces with the statement that they belong to.
In Haskell, due to the layout rule, this is not allowed most of the time.
Partly vecause of this, Haskell has developed some unique formatting
conventions, such as leading commas:

```
foo =
  ( first
  , second
  , third
  )
```

Admittedly, some Haskell programmers have grown fond of these new styles.
This proposal doesn't prevent them from being used by choice.  However,
it's not optimal to force programmers into unusual style choices when it's
easy to fix the layout rule to not have that effect.

This rule has also caused complexity in Haskell tooling.  See
[here](https://www.tweag.io/blog/2019-10-11-ormolu-first-release/):

> That’s why we make an exception in our rendering rules—we move the
> closing parenthesis one indentation level to the right on the rare
> occasions it’s necessary.

So let's fix the layout rule, and let people
choose.

## Proposed Change Specification

A new language extension is added, called `BracketSensitiveLayout`.  When
enabled, the following rule is added to the beginning of the layout algorithm in
[section 10.3 of the Haskell 2010 Report](https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17800010.3):

* L (\<n\> : t : ts) (m : ms) = L (t : ts) (m : ms), **if** m = n, and t is one of: ), ], or }.

## Examples

With the `BracketSensitiveLayout` extension enabled, the following are all valid,
while they were previously syntax errors:

```
foo = (
  1,
  2
)
```

```
main = do
  mapM_ print [
    1,
    2,
    3
  ]
```

```
foo = a ++ b
  where
    a = [
      1,
      2,
      3
    ]
    b = [
      4,
      5,
      6
    ]
```

The following also becomes legal, though I'm not sure it should be:

```
names = [
  "Abe",
  "Beth",
  "Charles"
] ++ [
  "Donna",
  "Emily",
  "Frank"
]
```

I'd suggest this would be considered bad style, at least.  But not
all poor formatting is excluded by the parser.

Note that the following is still **not** legal, because the indentation
of the closing bracket is less than the current layout context, instead
of equal to it:

```
foo = a ++ b
  where
    a = [
      1,
      2
  ]
```

## Effect and Interactions

I don't know what else there is to say.

I am fairly sure that enabling this extension would not make
any change to the meaning of legal Haskell programs.  It would only
make additional Haskell programs legal.

## Costs and Drawbacks

There are two drawbacks worth considering:

1. It makes the layout rule more complex.  This is something Haskellers
   should definitely be concerned about.  However, I believe that these
   close-bracket tokens are already obviously not the starts of new
   statements, and the meanings of the new programs accepted by this rule
   are abundantly clear, which substantially mitigates the cost.
2. Having more optional syntax means that GHC no longer enforces as much
   consistency in style.  The intent here isn't to fork the layout rule
   indefinitely.  This change should only be accepted if the committee
   sees a real chance that it will be adopted in a future Haskell Report.

The implementation and maintenance cost for GHC should not be significant.

## Alternatives

The main alternative today, mentioned above, is to adopt new formatting
conventions different from other languages, as Haskell programmers currently
do.  This is obviously possible, but they make the transition from other
languages to Haskell more difficult.

Another alternative would be to adopt this proposal, but even further expand
the list of tokens that do *not* start new statements in layout.  Instead of
just close brackets, one could for instance add `=`, `,`, `::`, `in`, and
any infix operator.  I have not proposed to do so because it is less obvious
which tokens are continuations of the previous statement, and it then becomes
much harder to explain parsing rules.  Indeed, *which* tokens may occur at
the beginning of a layout statement may depend on other language extensions
in effect.  In the past, `deriving` was such a token, but now we have
`-XStandaloneDeriving` and it much be removed from the list.  To avoid these
unhappy effects, I propose to limit ourselves to close-brackets, which are
both the most important part of the problem, and the least problematic as
a solution.

## Unresolved Questions

None yet.

## Implementation Plan

I (cdsmith) will attempt to implement this if the proposal is accepted.  I
am not an experienced GHC developer, but I do not expect the implementation
to be difficult.

## Endorsements
