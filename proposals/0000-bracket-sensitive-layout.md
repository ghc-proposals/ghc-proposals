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

```
foo = (
  1,
  2
)

test.hs:4:1: error:
    parse error (possibly incorrect indentation or mismatched brackets)
  |
4 | )
  | ^
```

There are two consequences of this:

**Divergence in formatting:** Haskell has, therefore, developed some unique
formatting conventions, such as:

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

These aren't necessarily bad on their own.  Many Haskell programmers are fond
of these new styles, and prefer them on their own merits.  However, it's not
optimal to force programmers into unusual style choices just because of the
limitations of layout.  It's easy to tweak the layout rule to not require them,
letting programmers make their own choices.

**Tool complexity:** This layout rule has also caused complexity in multiple
pieces of Haskell tooling, where applying natural rules consistently leads to
syntax errors, and tools introduce ad hoc rules to work around it.

I have not done a comprehensive search for tools that have had to react to this.
But from memory, I know this is identified as an issue in the article
[here](https://www.tweag.io/blog/2019-10-11-ormolu-first-release/):

> That’s why we make an exception in our rendering rules—we move the
> closing parenthesis one indentation level to the right on the rare
> occasions it’s necessary.

and is also the cause of the workaround
[here](https://github.com/google/codeworld/blob/43a3b947bfa57c7fc1b49c67090c3f569de80b8c/web/js/codeworld-mode.js#L593):

    minIndent = isBracket(parent) ? parent.column : parent.column + 1;

Because of all this, I propose to fix the layout rule, so that formatting is
not dictated by limitations of tools.

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

There are some (closely related) drawbacks worth considering:

1. It makes the layout rule a little bit more complex.  This is something
   Haskellers should be concerned about, lest it require more cognitive
   load for programmers to read and understand the structure of code.
   However, I believe that these closing bracket tokens are already
   obviously not the starts of new statements, and the meanings of the new
   programs accepted by this rule are abundantly clear, which substantially
   mitigates the cost.  (This is, however, a good reason not to adopt
   the alternative mentioned below.)
2. Having more optional syntax means that GHC no longer enforces as much
   consistency in style.  However, GHC already accepts plenty of terrible
   formatting.  Realistically, no one can rely on GHC to maintain consistent
   style in Haskell code.  There are other tools, such as linters and
   formatters, to do that job.  GHC's role is to accept programs when their
   meaning is clear.
3. There is a cost to having more language options, and two different
   syntax rules.  This is an inherent cost in making any change.  The intent
   here isn't to fork the layout rule in the long run.  This change should
   only be accepted if the committee sees a real chance that it would be
   adopted universally in a future Haskell Report.

The implementation and maintenance cost for GHC should not be significant.

## Alternatives

We consider three alternatives.

### Alternative 1: Do Nothing

The main alternative today is to adopt new formatting conventions different
from other languages, as Haskell programmers currently do.  This is obviously
possible, but the reasons to avoid it are mentioned in the motivation section
already.

### Alternative 2: Be More Liberal

Another alternative would be to adopt this proposal, but further extend the list
of tokens that are treated like closing brackets in the main proposal, and do
not start new statements.

Plenty of other tokens can never appear at the start of any statement in any
layout context in a valid Haskell source file.  Among others, these include:
`=`, `,`, `::`, `in`, or any infix operator.

I believe this is a bad idea for two reasons.
  
* First, even if these tokens cannot start a statement, it is less obvious that
  this is so, sometimes requiring complex reasoning about the grammar.  This
  adds cognitive load when reading code, and potentially surprises Haskell
  programmers who expect the parse tree to be clear and obvious.
* Second, and even more importantly, which tokens may occur at the beginning of
  a layout statement may depend on other language extensions, and change over
  time.  For example, in the past, `deriving` was such a token.  Now we have
  `-XStandaloneDeriving`, which adds new syntax where `deriving` can start a
  top-level declaration.

  In such situations, should tokens be removed from the list and possibly break
  more existing code even when a new syntax extension is disabled?  Should the
  resolution of layout depend on language extensions that affect higher-level
  syntax?  Neither option is acceptable.

By limiting this new behavior to closing brackets, which should obviously not
start a new layout statement, one avoids both of these unhappy situations.  At
the same time, one solves the entire motivating problem, which was about the
placement of closing brackets.

### Alternative #3: Be Less Liberal

A third alternative is to rewrite the layout rule in a different way.  The
existing layout rule is actually very unintuitive.  For example, it allows
syntax like this:

```
data MyRecord = MyRecord { a :: Int }

weirdButValid = 42
  where record = MyRecord {
a = 5
                 }
```

The root of the problem here is that the layout algorithm consolidates too
many states in an attempt to keep track of only one integer per layout
context, and shoehorns two meanings into that same value.  If one instead
separates the context type from the column, one gets this alternative
layout algorithm:

Assuming:
```
data Ctx = BracketCtx Token | LayoutCtx Int

indent :: [Ctx] -> Int
indent ctxs = head ([i | LayoutCtx i <- ctxs ] ++ [0])

close :: Char -> Maybe Char
close '{' = Just '}'
close '[' = Just ']'
close '(' = Just ')'
close _   = Nothing

isOpenBracket :: Char -> Bool
isOpenBracket c = close c /= Nothing

isCloseBracket :: Char -> Bool
isCloseBracket c = (`elem` ['}', ']', ')'])
```

Then:

```
L ({n} : ts) ctxs
  | n > indent ctxs       = '{' : L ts (LayoutCtx n : ctxs)
  | otherwise             = '{' : '}' : L (<n> : ts) ctxs

L (<n> : ts) (LayoutCtx m : ctxs)
  | m == n                = ';' : L ts (LayoutCtx m : ctxs)
  | n < m                 = '}' : L (<n> : ts) ctxs
L (<n> : ts) ctxs
  | n < indent ctxs       = parseError              -- (*)
L (<n> : ts) ctxs         = L ts ctxs

L (t : ts) ctxs
  | isOpenBracket t       = t : L ts (BracketCtx t : ctxs)
L (t : ts) (BracketCtx b : ctxs)
  | t == close b          = t : L ts ctxs
  | isCloseBracket b      = parseError
L (t : ts) (LayoutCtx _ : ctxs)
  | isParseError t        = '}' : L (t : ts) ctxs

L (t : ts) ctxs           = t : L ts ctxs
L [] []                   = []
L [] (LayoutCtx _ : ctxs) = '}' : L [] ctxs
L _ _                     = parseError
```

The advantage of this approach over the main proposal is that instead
of adding an unusual exception for one specific style, this imposes
minimal restrictions to keep layout meaningful when brackets are
missing.  (The equation marked `(*)` imposes restrictions that are not
necessary for parsing, but help with readability.  It could be
omitted.)

There are two big disadvantages to going this way:

1. As written, this is **not** backward compatible with the Haskell
   Report today.  I believe this could be fixed by omitting the
   equation marked `(*)`, but at the cost of accepting code where it
   is hard to parse layout indents.  However, I have not proven this.

   If this were implemented as above, some work would be needed to analyze
   large bodies of existing Haskell code to look for places where the
   behavior differs.  I suspect the answer would be that almost nothing
   breaks, but there's no way to verify that for proprietary code, for
   instance.

2. There's more metadata required than the main proposal.  GHC needs to
   track not just categories of tokens (which is required by other accepted
   proposals anyway), but which brackets match which others.

I am interested in more feedback on the wisdom of adopting this approach
versus the main proposal.

## Unresolved Questions

None yet.

## Implementation Plan

I (cdsmith) will attempt to implement this if the proposal is accepted.  I
am not an experienced GHC developer, but I do not expect the implementation
to be difficult.

## Endorsements
