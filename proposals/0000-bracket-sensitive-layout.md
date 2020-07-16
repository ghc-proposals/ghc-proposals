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

In most other programming languages, it is very commonplace to
align closing parentheses, brackets, or braces with the statement
that they belong to.  These examples are all simplified from
tutorials, documentation, etc., where the style is overwhelmingly
used.

```
// C++
int i[3][3] = {
    { 1, 0, 0 },
    { 0, 1, 0 },
    { 0, 0, 1 }
};
```

```
// Java
Arrays.sort(myArray, new Comparator<>() {
    @Override
    public int compare(Integer a, Integer b) {
        return a - b;
    }
});
```

```
// JavaScript
const obj = {
    strings: [
        "one",
        "two",
        "three",
        "four"
    ],
    innerObj: {
        x: 40,
        y: 300
    },
    f: function() {
        return 42;
    }
};
```

```
# Python
mydict = {
    "key1": 1,
    "key2": 2,
    "key3": 3,
}

mylist = [
    (1, 'hello'),
    (2, 'world'),
]
```

In Haskell, due to the layout rule, this is not allowed most of the time.
Haskell has, therefore, developed some unique formatting conventions,
such as:

```
indentedCloseBracket = (
  first,
  second,
  third,
  )

twoLevelIndent =
  (
    first,
    second,
    third
  )

leadingCommas =
  ( first
  , second
  , third
  )
```

Some Haskell programmers have grown fond of these new styles and prefer
them on their own merits.  However, it's not optimal to force programmers
into unusual style choices when it's easy to tweak the layout rule to not
require them.

This layout rule has also caused complexity in multiple pieces of Haskell
tooling.  For example, comments [here](https://www.tweag.io/blog/2019-10-11-ormolu-first-release/):

> That’s why we make an exception in our rendering rules—we move the
> closing parenthesis one indentation level to the right on the rare
> occasions it’s necessary.

and the workaround [here](https://github.com/google/codeworld/blob/master/web/js/codeworld-mode.js#L593).

So let's fix the layout rule, and let people choose.

## Proposed Change Specification

A new language extension is added, called `BracketSensitiveLayout`.  When
enabled, the following rule is added to the beginning of the layout algorithm in
[section 10.3 of the Haskell 2010 Report](https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17800010.3):

* L (\<n\> : t : ts) (m : ms) = L (t : ts) (m : ms), **if** m = n, and t is a closing bracket.

Without language extensions, the phrase "is a closing bracket" means one of `}`, `)`, and `]`.  More
generally, using the token classifications in
[proposal 229](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0229-whitespace-bang-patterns.rst#proposed-change-specification),
a closing bracket is defined as a token that is "closing", but not "opening".  This extends
closing brackets to include:

* `#)` when `UnboxedTuples` are enabled.
* `|]` and `||]` when `TemplateHaskellQuotes` is enabled.
* `|)` when `Arrows` is enabled.
* `⟧` and `⦈` when `UnicodeSyntax` is enabled along with any of `TemplateHaskellQuotes` or `Arrows`, respectively.

Because proposal 229 already introduces the requirement that new tokens are correctly classified
as opening or closing, this will automatically extend the new layout rule to additional kinds of
closing brackets if they are added in the future.

## Examples

With the `BracketSensitiveLayout` extension enabled, the following are all valid,
while they were previously syntax errors:

```
example1 = (
  1,
  2
)
```

```
example2 = do
  mapM_ print [
    1,
    2,
    3
  ]
```

```
example3 = a ++ b
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

The following two examples also becomes legal, though perhaps poorly formatted:

```
example4 = [
  "Abe",
  "Beth",
  "Charles"
] ++ [
  "Donna",
  "Emily",
  "Frank"
]
```

```
example5 = [ (
  1,
  2,
), (
  3,
  4
)]
```

I'd suggest these would still be considered bad style, at least.  But it is
already true that not all bad style is excluded by the parser.  It is still
unambiguous what is meant by these expressions, and being able to parse them
actually makes it easier for formatting tools based on GHC's parser to
suggest better alignment.

Note that the following are **not** legal:

```
nonExample6 = (
  1,
  2,
3 )  -- First character isn't a close-bracket, so ; is inserted.
```

```
nonExample7 = ( (
  1,
  2
),
(  -- First character isn't a close-bracket, so ; is inserted.
  3,
  4
) )
```

```
nonExample8 = a ++ b
  where
    a = [
      1,
      2
  ]  -- Indent less than the layout context still inserts a }.
```

## Effect and Interactions

I am fairly certain that enabling this extension would not make
any change to the meaning of legal Haskell programs.  It would only
make additional Haskell programs legal.

The only effect this has, then, is on error messages.  I would suggest
that implementing this change has the opportunity to improve error
messages, because it detects a specific programmer mistake, which is
quite common in my experience with beginners, presenting the
opportunity to give a specific helpful message.  Specifically, this
incorrect code:

```
foo = (
  1, 2
)
```

currently results in:

```
error:
    parse error (possibly incorrect indentation or mismatched brackets)
```

which is correct, but fairly vague.  With this change, there would be
a chance to add a more specific error message for this mistake.

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
