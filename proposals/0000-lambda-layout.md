---
author: Jakob Brünker
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/302).

# Multi-way lambda expressions

This proposal introduces several alternatives that provide functionality to
case split on the arguments of multi-parameter lambda expressions.

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
    - This could be circumvented by introducing a new expression that isn't required
      to be backwards compatible with existing idioms.

In the discussion of this proposal, several designs to address these
limitations came up, each with some merits and some shortcomings. Thus, this
proposal presents all of the most promising of these designs, and leaves it up
to the steering committee to decide which one - if any - shall be adopted.

Doing so would make the existing `-XLambdaCase` extension obsolete and would
allow users to entirely forego conventional function declaration syntax and
use this new construct instead, should they wish to do so.

## Proposed Change Specification

### Alternative (1): Add a new keyword

Example:

```haskell
filter = \mcase _ []                 -> []
             p (x:xs) | p x       -> x : filter p xs
                      | otherwise ->     filter p xs
```

NB: This section assumes that `\mcase` is chosen as the keyword. Other keywords have
been suggested, such as `\of` and `\cases`.

A new extension `-XMultiWayLambda` is implemented. Under this extension, a new
expression is enabled, introduced by the token sequence <tt>\\&nbsp;mcase</tt>. The whitespace between  `\` and `mcase`
is optional and may contain an arbitrary sequence of whitespace characters.
`\mcase` behaves in a way largely similar to `\`, but it is a layout herald, can have multiple
clauses, and may contain guards (see BNF for details).

Zero clauses are not permitted, as the expression would be ambiguous.
(See `Alternatives` section for details.)
This means that with `-XEmptyCase`,
`-XLambdaCase` still has one (albeit rarely used) construct that cannot be
replaced by a `\mcase`-expression, though
`\case {}` can (even today) be replaced by `\x -> case x of {}`. This shortcoming
could potentially
be addressed in a future proposal, for example by adding absurd patterns, to
provide a more general solution. Note that the `\case {}` construct only works for matching
on a single pattern.

Like the existing behavior for alternatives in `case`- and
`\case`-expressions, and equations in function declaration syntax, it is
possible to use `where` clauses within each clause of a `\mcase`-expression.

As with function declaration equations, all clauses must have the same number of patterns.

Given an `\mcase`-expression `mcexp` with one or more scrutinees and function `f` declared with function
declaration syntax
and with the same alternatives and same guards for each alternative as `mcexp`, the semantics of the
expression `mcexp` are the same as those of the expression `f`. If `mcexp` has no scrutinees, the
semantics are the same as those of an expression `p` declared with a pattern binding with the same
guards as `mcexp`.

This alternative matches function declaration syntax very closely, making
refactoring easier. However, it is another syntactic construct that has to be
maintained. Since it covers (almost) all of the use-cases of `\case`,
adding a deprecation plan for `\case` could be considered.

#### BNF Changes for (1)

**Bold** indicates changes to the existing BNF.

<table>
    <tr>
        <td><i>lexp</i></td><td>&rarr;</td><td>&hellip;</td>
    </tr>
    <tr>
        <td></td><td><b>|</b></td><td><b><tt>\mcase</tt> <tt>{</tt> <i>nalts</i> <tt>}</tt></b></td><td><b>(<tt>\mcase</tt> expression)</b></td>
    </tr>
    <tr>
        <td><b><i>nalts</i></b></td><td><b>&rarr;</b></td><td><b><i>nalt</i><sub>1</sub> <tt>;</tt> &hellip; <tt>;</tt> <i>nalt</i><sub>n</sub></b></td><td><b>(n &ge; 1)</b></td>
    </tr>
    <tr>
        <td><b><i>nalt</i></b></td><td><b>&rarr;</b></td><td><b>[ <i>apat</i><sub>1</sub> &hellip; <i>apat</i><sub>n</sub> ] <tt>-></tt> <i>exp</i> [ <tt> where </tt> <i>decls</i> ]</b></td><td><b>(n &ge; 0)</b></td>
    </tr>
    <tr>
        <td></td><td><b>|</b></td><td><b>[ <i>apat</i><sub>1</sub> &hellip; <i>apat</i><sub>n</sub> ] <i>gdpat</i> [ <tt> where </tt> <i>decls</i> ]</b></td><td><b>(n &ge; 0)</b></td>
    </tr>
    <tr>
        <td></td><td><b>|</b></td><td></td><td><b>(empty alternative)</b></td>
    </tr>
<table>

Aside from the explicit layout using `{`, `}`, and `;`, implicit layout as described in the Haskell
report can also be used.

In expressions that have zero scrutinees and multiple guards, there is an ambiguity as to whether
the expression has multiple alternatives with one guard each or one alternative with multiple guards
(or any combination thereof). However, the semantics for these are equivalent, so this ambiguity can be
resolved in an arbitrary way.

### Alternative (2): Comma-separated `\case`

Example:

```haskell
filter = \case _, []               -> []
               p, x:xs | p x       -> x : filter p xs
                       | otherwise ->     filter p xs
```

This alternative does not introduce a new construct. It instead consists of a straightforward extension to an existing one:
Allow separating multiple patterns in `\case` by commas. This makes it the
least disruptive of the presented alternatives.

A clause would only match if all of its patterns match their respective
scrutinee.

Additionally, an analogous extension could be introduced for `case of`:

```haskell
case numerator, denominator of
  _          , 0 -> Nothing
  Whole n    , d -> Whole (n / d)
  Complex a b, d -> Complex (a / d) (b / d)
```

This can be used instead of using tuples to achieve something similar:

```haskell
case (numerator, denominator) of
  (_          , 0) -> Nothing
  (Whole n    , d) -> Whole (n / d)
  (Complex a b, d) -> Complex (a / d) (b / d)
```

With the advantage that users don't have to be worried or learn about whether
using tuples in such cases incurs a performance penalty, and it would mean that the
`\case` syntax stays consistent with `case of` syntax.

The lack of parentheses makes this slightly more concise than the other
alternatives, especially in cases with only a single pattern.

One potential concern is that this breaks the pattern of symmetry between
expressions and patterns that match them. For example, if a function is defined
as `f (Just a) (Right b) = a + b`, it can be called as `f (Just a) (Right b)`,
but when using `\case` (i.e. `f = \case Just a, Right b -> a + b`), the patterns are
separated by commas, whereas the expression calling `f` still uses parentheses.

#### BNF Changes for (2)

**Bold** indicates changes to the existing BNF.

<table>
    <tr>
        <td><i>alt</i></td><td>&rarr;</td><td><b>[ <i>pat</i><sub>1</sub><tt>,</tt> &hellip;, <i>apat</i><sub>n</sub> ]</b> <tt>-></tt> <i>exp</i> [ <tt> where </tt> <i>decls</i> ]</td><td><b>(n &ge; 1)</b></td>
    </tr>
    <tr>
        <td></td><td>|</td><td><b>[ <i>pat</i><sub>1</sub><tt>,</tt> &hellip;<tt>,</tt> <i>apat</i><sub>n</sub> ]</b> <t>gdpat</t> [ <tt> where </tt> <i>decls</i> ]</td><td>(n &ge; 1)</td>
    </tr>
<table>

### Alternative (3): One lambda per clause, `case of`

Example:

```haskell
filter = case of \_ []                 -> []
                 \p (x:xs) | p x       -> x : filter p xs
                           | otherwise ->     filter p xs
```

The functionality of `-XLambdaCase` is extended, according to the following
schema (for a more formal treatment, see BNF below):

```haskell
case [ scrutinee ] of
  [ Pattern_0a ] \ Pattern_1a ... Pattern_na -> Expression_a
  [ Pattern_0b ] \ Pattern_1b ... Pattern_nb -> Expression_b
  ...
```

Semantically, this would be equivalent to

```haskell
\var_1 ... var_n -> case ([ scrutinee, ] var_1, ..., var_n) of
  ([ Pattern_0a, ] Pattern_1a, ..., Pattern_na) -> Expression_a
  ([ Pattern_0b, ] Pattern_1b, ..., Pattern_nb) -> Expression_b
```

The `case` expression is now able to define anonymous functions. The scrutinee
may be omitted, in which case the corresponding pattern in each clause must also be
omitted. Furthermore, in each clause, between the usual pattern (if it is present) and
the arrow, a `\` and a number of patterns may be written. The
number of patterns must be consistent across all clauses, and the types of
corresponding patterns must match (e.g., the first pattern after the backslash
must have the same type for all clauses). As usual, `case` clauses can
contain guards as well.

The number of patterns after each `\` determine the arity of the function that
a `case` expression produces. The *n*th pattern after the `\` is matched
against the *n*th argument given to the function.

Note that the patterns after the `\` must be enclosed by parentheses if they
consist of more than one token, just like patterns in a lambda expression, but
unlike the pattern that can come before the `\`.

If there is no scrutinee, it is not immediately clear what the meaning of an
expression without clauses, i.e. the expression `case of {}`, should be, since
the number of arguments to the anonymous function is not specified. Users might
expect this to compile if the `-XEmptyCase` extension is enabled. However, due
to the inherent ambiguity, this proposal does not allow a `case` expression
that lacks both a scrutinee and clauses. Other approaches are possible, see
Alternatives section.

Like the existing behavior for alternatives in `case`
expressions, and equations in function declaration syntax, it is
possible to use `where` clauses within each clause of the extended `case`
expression. Furthermore, each clause can have guards, which appear after all
patterns (see BNF).

This alternative has some desirable properties, in that it extends an existing
syntactic construct rather than introducing a new one and is syntactically
similar to lambda expressions. On the other hand, it does not relate to
existing syntax as directly as others (e.g., it produces an anonymous function
but doesn't start with `\`, as opposed to lambda expressions and `\case`), and
its functionality overlaps with that of `\case`. As with alternative (2), a
deprecation plan for `\case` could thus be considered.

Since the introductory example only demonstrates the case without scrutinee,
here is a different example:

```haskell
sendEmail address = case validate address of
  Just emailAddress \subject content (Just attachment) -> sendWithAttachment emailAddress subject content attachment
  Just emailAddress \subject content Nothing           -> sendWithoutAttachment emailAddress subject content
  Nothing           \_       _       _                 -> error "invalid address"
```

#### BNF Changes for (3)

**Bold** indicates changes to the existing BNF.

<table>
    <tr>
        <td><i>lexp</i></td><td>&rarr;</td><td>&hellip;</td>
    </tr>
    <tr>
        <td></td><td>|</td><td><tt>case</tt> <i>exp</i> <tt>of</tt> <tt>{</tt> <i>alts</i> <tt>}</tt></td><td>(case expression)</td>
    </tr>
    <tr>
        <td></td><td><b>|</b></td><td><b><tt>case</tt> <tt>of</tt> <tt>{</tt> <i>nalts</i> <tt>}</tt></b></td><td><b>(case expression without scrutinee)</b></td>
    </tr>
    <tr>
        <td></td><td>|</td><td><tt>case</tt> <i>exp</i> <tt>of</tt> <tt>{}</tt></td><td>(with <tt>-XEmptyCase</tt>)</td>
    <tr>
        <td><i>alts</i></td><td>&rarr;</td><td><i>alt<sub>1</sub></i> <tt>;</tt> &hellip; <tt>;</tt> <i>alt<sub>n</sub></i></td><td>(<i>n</i> &ge; 1)</td>
    </tr>
    <tr>
        <td><i>alt</i></td><td>&rarr;</td><td><i>pat</i> <b>[ <tt>\</tt> <i>apat<sub>1</sub></i> &hellip; <i>apat<sub>n</sub></i> ]</b> <tt>-&gt;</tt> <i>exp</i> [ <tt>where</tt> <i>decls</i> ]</td><td><b>(<i>n</n> &ge; 1)</b></td>
    </tr>
    <tr>
        <td></td><td>|</td><td><i>pat</i> <b>[ <tt>\</tt> <i>apat<sub>1</sub></i> &hellip; <i>apat<sub>n</sub></i> ]</b> <i>gdpat</i> [ <tt>where</tt> <i>decls</i> ]</td><td><b>(<i>n</n> &ge; 1)</b></td>
    </tr>
    <tr>
        <td></td><td>|</td><td></td><td>(empty alternative)</td>
    </tr>
    <tr>
        <td><b><i>nalts</i></b></td><td><b>&rarr;</b></td><td><b><i>nalt<sub>1</sub></i> <tt>;</tt> &hellip; <tt>;</tt> <i>nalt<sub>n</sub></i></b></td><td><b>(<i>n</i> &ge; 1)</b></td>
    </tr>
    <tr>
        <td><b><i>nalt</i></b></td><td><b>&rarr;</b></td><td><b><tt>\</tt> <i>apat<sub>1</sub></i> &hellip; <i>apat<sub>n</sub></i><tt> -&gt;</tt> <i>exp</i> [ <tt>where</tt> <i>decls</i> ]</b></td><td><b>(<i>n</n> &ge; 0)</b></td>
    </tr>
    <tr>
        <td></td><td><b>|</b></td><td><b><tt>\</tt> <i>apat<sub>1</sub></i> &hellip; <i>apat<sub>n</sub></i> <i>gdpat</i> [ <tt>where</tt> <i>decls</i> ]</b></td><td><b>(<i>n</n> &ge; 0)</b></td>
    </tr>
    <tr>
        <td></td><td><b>|</b></td><td></td><td><b>(empty alternative)</b></td>
    </tr>
<table>

### Alternative (4): Multi-pattern `\case` with parentheses

Example:

```haskell
filter = \case _ []                 -> []
               p (x:xs) | p x       -> x : filter p xs
                        | otherwise ->     filter p xs
```

Regular function definition syntax requires parentheses around patterns that
consist of more than one token. The same could be done with `\case`. This would
make the two syntaxes more consistent, and allow easy refactoring from one to
the other. It also doesn't introduce any new syntactic constructs that have to
be maintained.

However, it it behaves differently from the current `-XLambdaCase` extension,
which doesn't require parentheses around patterns. This would seem to make it
non-backwards-compatible, especially if it still uses the same `-XLambdaCase`
extension name. This can be mitigated in various ways.

First, a refactoring tool could be provided to update existing code and
introduce parentheses where necessary, which would massively lower the effort
required to update old code to be compatible with the new extension. Note that
while the current `-XLambdaCase` extension doesn't *require* parentheses, it
    doesn't prohibit them, either. Thus, code updated with such a tool would
    work with both versions of the extension.

Second, a special case could be introduced to have the compiler handle `\case`
differently if there is only one pattern. More specifically, this means that
the type checker detects when the first pattern in a clause is a solitary,
non-nullary constructor. If this is the case, the AST is reconstructed such
that the remaining patterns in the clause become arguments of this constructor.

When this special case is triggered, the compiler would produce a warning
(`-Wdeprecated-lambda-case`), which would be on by default, and warn the user
that they're using syntax which will be deprecated at some future point. This
would make it possible to remove the special casing and warning after a few
releases have passed.

#### BNF Changes for (4)

**Bold** indicates changes to the existing BNF.

<table>
    <tr>
        <td><i>lexp</i></td><td>&rarr;</td><td>&hellip;</td>
    </tr>
    <tr>
        <td></td><td><b>|</b></td><td><b><tt>\case</tt> <tt>{</tt> <i>nalts</i> <tt>}</tt></b></td><td><b>(<tt>\case</tt> expression)</b></td>
    </tr>
    <tr>
        <td><b><i>nalts</i></b></td><td><b>&rarr;</b></td><td><b><i>nalt</i><sub>1</sub> <tt>;</tt> &hellip; <tt>;</tt> <i>nalt</i><sub>n</sub></b></td><td><b>(n &ge; 1)</b></td>
    </tr>
    <tr>
        <td><b><i>nalt</i></b></td><td><b>&rarr;</b></td><td><b>[ <i>apat</i><sub>1</sub> &hellip; <i>apat</i><sub>n</sub> ] <tt>-></tt> <i>exp</i> [ <tt> where </tt> <i>decls</i> ]</b></td><td><b>(n &ge; 0)</b></td>
    </tr>
    <tr>
        <td></td><td><b>|</b></td><td><b>[ <i>apat</i><sub>1</sub> &hellip; <i>apat</i><sub>n</sub> ] <i>gdpat</i> [ <tt> where </tt> <i>decls</i> ]</b></td><td><b>(n &ge; 0)</b></td>
    </tr>
    <tr>
        <td></td><td><b>|</b></td><td></td><td><b>(empty alternative)</b></td>
    </tr>
<table>

## Summary

Here is a comparison of the four alternatives:

<table>
<tr>
  <th>Approach</th>
  <th>Example</th>
  <th>Pros</th>
  <th>Cons</th>
</tr>
<tr>
  <td>(1) Keyword (<tt>\mcase</tt>, <tt>\mcase</tt>, etc.)</td>
  <td>
     <pre style="display: inline">
       <code>
\mcase (Just a) (Left b) -> ...
    _        _        -> ...</code>
    </pre>
  </td>
  <td>
    <ul>
      <li>Parity with function equation syntax</li>
    </ul>
  </td>
  <td>
    <ul>
      <li>Disagreement over which keyword to use</li>
      <li>Adds yet another similar construct and hence disagreements about deprecations</li>
    </ul>
  </td>
</tr>
<tr>
  <td>(2) Comma-separated <tt>\case</tt></td>
  <td>
     <pre style="display: inline">
       <code>
\case Just a, Left b -> ...
      _     , _      -> ...</code>
    </pre>
    <pre style="display: inline">
       <code>
case Just 34, Right [] of
    Just a, Left b -> ...
    _     , _      -> ...</code>
    </pre>
  </td>
  <td>
    <ul>
      <li>Conceptually, the smallest change that achieves the goal: Just a minor extension to one or two existing constructs</li>
      <li>That means no demand for or concerns about potential deprecation</li>
      <li>Parity with extended <tt>case ... of</tt> syntax</li>
      <li>Like current <tt>\case</tt>, single pattern uses are concise due to lack of parentheses</li>
    </ul>
  </td>
  <td>
    <ul>
      <li>Different from function equation syntax and from function application syntax: you apply as <tt>f (Just 34) (Right [])</tt> but pattern match with <tt>\case Just 23, Right []</tt></li>
    </ul>
  </td>
</tr>
<tr>
  <td>(3) One lambda per clause <tt>case of</tt></td>
  <td>
     <pre style="display: inline">
       <code>
case of
  \(Just a) (Left b) -> ...
  \_        _        -> ...</code>
    </pre>
     <pre style="display: inline">
       <code>
case [1,2,3] of
  (x:xs) \(Just a) (Left b) -> ...
  _      \_        _        -> ...</code>
    </pre>
  </td>
  <td>
    <ul>
      <li>Allows combining pattern matching on scrutinees and function arguments in one expression</li>
      <li>Parity with function equation syntax</li>
    </ul>
  </td>
  <td>
    <ul>
      <li>While it extends an existing construct, this extension makes it overlap with <tt>\case</tt> functionality</li>
      <li>Not as obvious an extension from existing syntax as the other options (i.e. starts with <tt>case</tt>, not <tt>\</tt>, even though it takes arguments)</li>
    </ul>
  </td>
</tr>
<tr>
  <td>(4) Multi-pattern <tt>\case</tt> with parens</td>
  <td>
     <pre style="display: inline">
       <code>
\case
  (Just a) (Left b) -> ...
  _        _        -> ...</code>
    </pre>
  </td>
  <td>
    <ul>
      <li>Doesn't introduce a new construct, and doesn't introduce any overlap with others</li>
      <li>Parity with function equation syntax</li>
    </ul>
  </td>
  <td>
    <ul>
      <li>Not backwards compatible - can be mitigated by using (possibly temporary) compiler magic to allow single-scrutinee <tt>\case</tt> without parens, as well as providing an automatic refactoring tool to update existing code</li>
    </ul>
  </td>
</tr>
</table>

## Further Examples

Guards can be used instead of `-XMultiWayIf` inside lambda expressions:

```Haskell
{-# LANGUAGE MultiWayIf, BlockArguments #-}
take' :: Int -> [a] -> [a]
take' = flip $ flip foldr (const [])
  \x more n -> if | n > 0 -> x : more (n - 1)
                  | otherwise -> []

-- becomes

-- (1)
take' :: Int -> [a] -> [a]
take' = flip $ flip foldr (const [])
  \mcase x more n | n > 0 -> x : more (n - 1)
                  | otherwise -> []

-- (2)
take' :: Int -> [a] -> [a]
take' = flip $ flip foldr (const [])
  \case x, more, n | n > 0 -> x : more (n - 1)
                   | otherwise -> []

-- (3)
take' :: Int -> [a] -> [a]
take' = flip $ flip foldr (const [])
  case of
    \x more n | n > 0 -> x : more (n - 1)
              | otherwise -> []

-- (4)
take' :: Int -> [a] -> [a]
take' = flip $ flip foldr (const [])
  \case x more n | n > 0 -> x : more (n - 1)
                 | otherwise -> []
```

The new syntax could be used instead of `-XMultiWayIf`:

```Haskell
foo = bar baz if | g1 -> a
                 | g2 -> b

-- with -XBlockArguments becomes

-- (1)
foo = bar baz \mcase | g1 -> a
                     | g2 -> b

-- (2)
foo = bar baz \case | g1 -> a
                    | g2 -> b

-- (3)
foo = bar baz case of \ | g1 -> a
                      \ | g2 -> b

-- (4)
foo = bar baz \case | g1 -> a
                    | g2 -> b
```

`\case` can be replaced by the new syntax:

```Haskell
\case Bar baz -> Just baz
      Quux -> Nothing

-- becomes

-- (1)
\mcase (Bar baz) -> Just baz
       Quux -> Nothing

-- (2) (no change)
\case Bar baz -> Just baz
      Quux -> Nothing

-- (3)
case of \(Bar baz) -> Just baz
        \Quux -> Nothing

-- (4)
\case (Bar baz) -> Just baz
      Quux -> Nothing

```

Unlike current `\case`, multiple patterns can be matched:

```Haskell
-- \case can't be used here!
-- At least not easily
\foo bar baz -> case (foo, bar, baz) of
  (Just 4, 3, False) -> 42
  _ -> 0

-- becomes

-- (1)
\mcase
  (Just 4) 3 False -> 42
  _ _ _ -> 0

-- (2)
\case
  Just 4, 3, False -> 42
  _, _, _ -> 0

-- (3)
case of
  \(Just 4) 3 False -> 42
  \_ _ _ -> 0

-- (4)
\case
  (Just 4) 3 False -> 42
  _ _ _ -> 0
```

The new syntax can be used instead of regular function declaration syntax,
potentially resulting in more concise definitions:

```Haskell
extremelyLengthyFunctionIdentifier (Just a) False = Just 42
extremelyLengthyFunctionIdentifier (Just a) True  = Just (a / 2)
extremelyLengthyFunctionIdentifier _        _     = Nothing

-- becomes

-- (1)
extremelyLengthyFunctionIdentifier = \mcase
  (Just a) False -> Just 42
  (Just a) True  -> Just (a / 2)
  _        _     -> Nothing

-- (2)
extremelyLengthyFunctionIdentifier = \case
  Just a, False -> Just 42
  Just a, True  -> Just (a / 2)
  _     ,  _    -> Nothing

-- (3)
extremelyLengthyFunctionIdentifier = case of
  \(Just a) False -> Just 42
  \(Just a) True  -> Just (a / 2)
  \_        _     -> Nothing

-- (4)
extremelyLengthyFunctionIdentifier = \case
  (Just a) False -> Just 42
  (Just a) True  -> Just (a / 2)
  _        _     -> Nothing
```

This also makes it possible to have `where` bindings that scope over multiple
equations

```Haskell
-- have to repeat the definition of `magicNumber` or place it outside the definition of
-- foo
foo (Just x) p | x < 0 = ...
               | let y = blah + 1 = ...
  where blah = x + magicNumber
        magicNumber = 5
foo Nothing _ = magicNumber
  where magicNumber = 5

-- becomes

-- note that the first `where` clause belongs to the first clause, rather than the
-- function declaration, because it is indented further

-- (1)
foo = \mcase
  (Just x) p | x < 0 -> ...
             | let y = blah + 1 -> ...
    where blah = x + magicNumber
  Nothing _ -> magicNumber
  where
    magicNumber = 5

-- (2)
foo = \case
  Just x, p | x < 0 -> ...
            | let y = blah + 1 -> ...
    where blah = x + magicNumber
  Nothing, _ -> magicNumber
  where
    magicNumber = 5

-- (3)
foo = case of
  \(Just x) p | x < 0 -> ...
             | let y = blah + 1 -> ...
    where blah = x + magicNumber
  \Nothing _ -> magicNumber
  where
    magicNumber = 5

-- (1)
foo = \case
  (Just x) p | x < 0 -> ...
             | let y = blah + 1 -> ...
    where blah = x + magicNumber
  Nothing _ -> magicNumber
  where
    magicNumber = 5
```

To illustrate with some real-world examples, this section shows
how some snippets found on hackage would look if they used this new syntax:

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

-- becomes

-- (1)
printGender :: Language -> Gender -> Text
printGender = \mcase
  English Male   -> "Male"
  English Female -> "Female"
  Chinese Male   -> "男性"
  Chinese Female -> "女性"

-- (2)
printGender :: Language -> Gender -> Text
printGender = \case
  English, Male   -> "Male"
  English, Female -> "Female"
  Chinese, Male   -> "男性"
  Chinese, Female -> "女性"

-- (3)
printGender :: Language -> Gender -> Text
printGender = case of
  \English Male   -> "Male"
  \English Female -> "Female"
  \Chinese Male   -> "男性"
  \Chinese Female -> "女性"
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
   to indicate how many arguments a given `\mcase`-expression matches on, as otherwise, it would
   be ambiguous.
   The number of arguments a `\mcase`-expression pattern matches on becomes obvious from the
   clauses, e.g. `\mcase a b -> ...` clearly matches on two arguments. Without clauses, this remains
   unclear. This means it would also be unclear whether the patterns are non-exhaustive:
   Consider the expression `f = \mcase {} :: Bool -> Void -> a`. If the expression is supposed to match on
   both arguments, the patterns are exhaustive. If it is only supposed to match on the first argument
   and evaluate to a funtion of type `Void -> a`, it is not exhaustive. Moreover, in the former case,
   ``f undefined `seq` ()`` evaluates to `()`, whereas in the latter case, it evaluates to bottom.
   With `\case {}` this problem doesn't arise, since it always matches on exactly one argument,
   and similarly for `case x of {}`, which only matches on `x`.
   A syntax to resolve this has been proposed in the discussion: `(\mcase)` for matching on no arguments,
   `(\mcase _)` for one, `(\mcase _ _)` for two, and so on.

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

 - `\mcase`-expressions with zero patterns could only be allowed if the expression contains guards.
   This would make them somewhat less consistent, but it is how lambda expressions work
   (i.e. `\ -> ...` is illegal) and only disallows expressions that are needlessly verbose (i.e.
   `\mcase -> exp` can always be replaced by `exp`).

 - `\case` could be deprecated, since all its use cases would be subsumed by `\mcase`, albeit with additional
   parentheses around patterns that consist of more than one token. However, the discussion of this proposal
   has shown that such a deprecation would be a controversial
   change of its own and that some working out has to be done as to the exact details of it, thus,
   this might be better suited to being its own, separate proposal. Combined with this, an alternative to the keyword would be
   to reuse `\case` and change the way it works to the behaviour described in this proposal.

 - There are also other alternatives for the keyword that have been raised: `\cases` and `\mcase`.

 - The possibility to have a construct similar to `-XMultiWayIf` but without the keyword, i.e. using guards directly as
   an expression, was also raised in the discussion. If this were to be used instead of `\mcase` expressions, any pattern
   matching would have to be done with pattern guards.

## Implementation Plan

I (Jakob Brünker) will implement this proposal.
