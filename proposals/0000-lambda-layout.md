---
author: Jakob Brünker
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/302).

# Lambda extension for `case`

This proposal expands the functionality of the `-XLambdaCase` extension, by
extending the `case` syntax to allow lambda expression functionality.

## Motivation

In Haskell 2010, there are two syntaxes to define a function: via the function
definition syntax, or via lambda expressions. The most obvious difference is
that the former assigns a name to the function, whereas the latter can be used
for anonymous functions. However, the differences go significantly beyond that,
for example:

 - Lambda expressions can only have a single clause, function declarations
   can have an arbitrary non-zero number of equations
 - Lambda expressions cannot have guards

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
      it would be strictly more powerful than the existing `-XLambdaCase`,
      since it would be able to match on multiple patterns.
 2. The extension `-XMultiWayIf` essentially introduces standalone guards,
    simplifying the use of guards that aren't at the outermost level of a
    function declaration or case-expression. Among other things, this made it
    easier to use guards inside of lambda expressions.
    - If there were an expression similar to lambda expressions that could have
      guards, it could be used instead of using `-XMultiWayIf` as a workaround
      for guards in lambda expressions in those cases.
 3. During the implementation of `-XLambdaCase`,
    [some suggested](https://gitlab.haskell.org/ghc/ghc/issues/4359#note_51110)
    allowing lambda expressions to have multiple clauses. This was not
    implemented: The most obvious approach of turning `\` into a layout herald
    had the disadvantage of making some common idioms invalid.
    - The syntax extension introduced in this proposal is fully backwards
      compatible with those idioms.

This proposal, then, aims to overcome the shortcomings of lambda expressions
and allows the `case` expression to have the same capabilities as function
declarations, which can be used instead of `-XLambdaCase`, many instances of
`-XMultiWayIf`, and most of function declaration syntax, if a user wishes to
use the new expression instead. Furthermore, it can be used in situations that
are not conveniently covered by existing constructs (see Example section). As
an example of how the `case` construct is extended, here is a definition of
filter using it:

```haskell
filter = case of
  \_ []                 -> []
  \p (x:xs) | p x       -> x : rest
            | otherwise -> rest
```

By combining the functionality of several features into one feature in
a way that's consistent with the rest of the language, it enables users who
wish to use this functionality to work with a simpler and more consistent
language.

## Proposed Change Specification

The functionality of `-XLambdaCase` is extended, according to the following
schema (for a more formal treatment, see BNF below):

```haskell
case [ scrutinee ] of
  [ Pattern_0a ] \ Pattern_1a ... Pattern_na -> Expression_a
  [ Pattern_0b ] \ Pattern_1b ... Pattern_nb -> Expression_b
  ...
```

This is equivalent to

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

Once the [*Binding type variables in lambda-expressions*](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0155-type-lambda.rst)
proposal is being implemented, with `-XTypeAbstractions`, `case`-expressions will also be able to bind type
variables.

### BNF of changed syntax

**Bold** indicates changes to the existing BNF.

<table>
    <tr>
        <td><i>lexp</i></td><td>&rarr;</td><td><tt>case</tt> <i>exp</i> <tt>of</tt> <tt>{</tt> <i>alts</i> <tt>}</tt></td>
    </tr>
    <tr>
        <td></td><td><b>|</b></td><td><b><tt>case</tt> <tt>of</tt> <tt>{</tt> <i>nalts</i> <tt>}</tt></b></td>
    </tr>
    <tr>
        <td></td><td>|</td><td><tt>case</tt> <i>exp</i> <tt>of</tt> <tt>{}</tt></td><td>(with <tt>-XEmptyCase</tt>)</td>
    <tr>
        <td><i>alts</i></td><td>&rarr;</td><td><i>alt<sub>1</sub></i> <tt>;</tt> &hellip; <tt>;</tt> <i>alt<sub>n</sub></i></td><td>(<i>n</i> &ge; 1)</td>
    </tr>
    <tr>
        <td><i>alt</i></td><td>&rarr;</td><td><i>pat</i> <b>[ <tt>\</tt> <i>apat<sub>1</sub></i> &hellip; <i>apat<sub>n</sub></i><tt> ]</b> -&gt;</tt> <i>exp</i> [ <tt>where</tt> <i>decls</i> ]</td><td>(<i>n</n> &ge; 1)</td>
    </tr>
    <tr>
        <td></td><td>|</td><td><i>pat</i> <b>[ <tt>\</tt> <i>apat<sub>1</sub></i> &hellip; <i>apat<sub>n</sub></i><tt> ]</b> <i>gdpat</i> [ <tt>where</tt> <i>decls</i> ]</td><td>(<i>n</n> &ge; 1)</td>
    </tr>
    <tr>
        <td></td><td>|</td><td></td><td>(empty alternative)</td>
    </tr>
    <tr>
        <td><b><i>nalts</i></b></td><td><b>&rarr;</b></td><td><b><i>nalt<sub>1</sub></i> <tt>;</tt> &hellip; <tt>;</tt> <i>nalt<sub>n</sub></i></b></td><td><b>(<i>n</i> &ge; 1)</b></td>
    </tr>
    <tr>
        <td><b><i>nalt</i></b></td><td><b>&rarr;</b></td><td><b><tt>\</tt> <i>apat<sub>1</sub></i> &hellip; <i>apat<sub>n</sub></i><tt> -&gt;</tt> <i>exp</i> [ <tt>where</tt> <i>decls</i> ]</b></td><td><b>(<i>n</n> &ge; 1)</b></td>
    </tr>
    <tr>
        <td></td><td><b>|</b></td><td><b><tt>\</tt> <i>apat<sub>1</sub></i> &hellip; <i>apat<sub>n</sub></i><tt> <i>gdpat</i> [ <tt>where</tt> <i>decls</i> ]</b></td><td><b>(<i>n</n> &ge; 1)</b></td>
    </tr>
    <tr>
        <td></td><td><b>|</b></td><td></td><td><b>(empty alternative)</b></td>
    </tr>
    <tr>
        <td><i>gdpat</i></td><td>&rarr;</td><td><i>guards</i> <tt>-&gt;</tt> <i>exp</i> [ <i>gdpat</i> ]</td>
    </tr>
    <tr>
        <td><i>guards</i></td><td>&rarr;</td><td><tt>|</tt> <i>guard<sub>1</sub></i><tt>,</tt> &hellip;<tt>,</tt> <i>guard<sub>n</sub></i></td><td>(<i>n</i> &ge; 1)</td>
    </tr>
    <tr>
        <td><i>guard</i></td><td>&rarr;</td><td><i>pat</i> <tt>&lt;-</tt> <i>infixexp</i></td><td>(pattern guard)</td>
    </tr>
    <tr>
        <td></td><td>|</td><td><tt>let</tt> <i>decls</i></td><td>(local declaration)</td>
    </tr>
    <tr>
        <td></td><td>|</td><td><i>infixexp</i></td><td>(boolean guard)</td>
    </tr>
<table>

Aside from the explicit layout using `{`, `}`, and `;`, implicit layout as described in the Haskell
report can also be used.

## Examples

`case` expressions are now more powerful than lambda expressions since they can
have multiple clauses:

```Haskell
-- \case can't be used here!
-- At least not easily
\foo bar baz -> case (foo, bar, baz) of
  (Just 4, 3, False) -> 42
  _ -> 0

-- becomes

case of
  \(Just 4) 3 False -> 42
  \_ _ _ -> 0
```

`case`-expressions can be used instead of regular function declaration syntax,
potentially resulting in more concise definitions:

```Haskell
extremelyLengthyFunctionIdentifier ma       False | isJust ma -> Just 42
extremelyLengthyFunctionIdentifier (Just a) True  = Just (a / 2)
extremelyLengthyFunctionIdentifier _        _     = Nothing

-- becomes

extremelyLengthyFunctionIdentifier = case of
  \ma       False | isJust ma -> Just 42
  \(Just a) True  -> Just (a / 2)
  \_        _     -> Nothing
```

`case` expressions can be used to give guards to lambda expressions:
```Haskell
-- with -XMultiWayIf
\a (MkFoo b) c -> if | a > b -> c
                     | otherwise -> a

-- with this proposal
case of \a (MkFoo b) c | a > b -> c
                       | otherwise -> a
```
Which of these is preferable is mainly a matter of taste, however, the latter
can be conveniently adapted to use multiple clauses, should the need arise, for
example to

```Haskell
case of \a (MkFoo b) c | a > b -> c
                       | otherwise -> a
        \_ (MkBar d) _ -> d
```

This proposal also makes it possible to have `where` bindings that scope over multiple
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

-- note that the first `where` clause belongs to the first `case` expression
-- clause, rather than the function declaration, because it is indented further
foo = case of
  \(Just x) | x < 0 -> ...
            | let y = blah + 1 -> ...
    where blah = x + magicNumber
  \Nothing -> magicNumber
  where
    magicNumber = 5
```

```Haskell
-- Mock example of a server connection
sendToServer :: Hostname -> Port -> Message -> IO ()
sendToServer host port message = connect host port >>= case message of
  TextMsg  text  \ (Connection conn) | isTooLong text = putStrLn $ "Message too long: " + show text
                                     | otherwise      = sendText conn text
  VoiceMsg audio \ (Connection conn) -> sendAudio conn audio defaultBitrate
  _              \ (ConnErr err) -> putStrLn $ "Could not send message: " + show err
```

To illustrate with some real-world examples, this section shows
how some snippets found on hackage would look if they used this new syntax:

roc-id-0.1.0.0, Gender.hs
```Haskell
-- With today's -XLambdaCase
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

-- With this proposal - this makes use of the capability to have multiple parameters
printGender :: Language -> Gender -> Text
printGender = case of
  \English Male   -> "Male"
  \English Female -> "Female"
  \Chinese Male   -> "男性"
  \Chinese Female -> "女性"
```

process-1.6.10.0, System.Process
```Haskell
readCreateProcess cp input = do
    (ex, output) <- withCreateProcess_ "readCreateProcess" cp_opts $
      \mb_inh mb_outh _ ph ->
        case (mb_inh, mb_outh) of
          (Just inh, Just outh) -> do

            -- fork off a thread to start consuming the output
            output  <- hGetContents outh
            withForkWait (C.evaluate $ rnf output) $ \waitOut -> do
              {...}
          (Nothing,_) -> error "readCreateProcess: Failed to get a stdin handle."
          (_,Nothing) -> error "readCreateProcess: Failed to get a stdout handle."

    {...}

-- The lambda and case expressions can be combined
readCreateProcess cp input = do
    (ex, output) <- withCreateProcess_ "readCreateProcess" cp_opts $
      case of
        \(Just inh) (Just outh) _ ph -> do
          -- fork off a thread to start consuming the output
          output  <- hGetContents outh
          withForkWait (C.evaluate $ rnf output) $ \waitOut -> do
            {...}
        \(Nothing,_) _ _ -> error "readCreateProcess: Failed to get a stdin handle."
        \(_,Nothing) _ _ -> error "readCreateProcess: Failed to get a stdout handle."

    {...}
```

## Effect and Interactions

The proposed functionality has some use case overlap with the current
`-XLambdaCase` functionality. However, it is more powerful, since it can accept
multiple arguments, and is integrated with the existing `case` functionality.

No currently allowed syntax is stolen by this extension, and the behavior of no
currently legal program would be changed with the extension enabled.

## Costs and Drawbacks

It adds complexity to an existing syntactic construct, which increases the
maintenance burden, however, the added maintenance should be similar to that
which is necessary for other existing constructs like `\case`, which
means the added maintenance cost should be fairly low.

While this also means a more complex construct to learn for beginners, the
syntax is consistent with similar constructs in the existing language like
lambda expressions, and `case` expressions can continue to be taught the way
they have been, while ignoring the added functionality for as long as necessary
or desired.

## Alternatives

 - The syntax extension could be given its own extension name instead of
   being integrated into `-XLambdaCase`.

 - Zero clauses could be permitted even without scrutinee, given `-XEmptyCase`.
   In this case, however, a way would have to be found to indicate how many
   arguments a given `case` expression accepts, as otherwise, it would be
   ambiguous. The obvious and most useful case would be one argument, so this
   could be chosen. Another more general alternative would be to add absurd
   patterns to the language.

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

 - zero patterns after the `\` could be allowed, however, there would be no
   use cases for this that aren't better covered by `-XMultiWayIf`.

 - `\case` could be deprecated, since all its use cases would be subsumed by `case`, albeit with additional
   parentheses around patterns that consist of more than one token. However, the discussion of this proposal
   has shown that such a deprecation would be a controversial change.

 - The `\case` functionality could be copied to a construct with a different
   keyword (e.g. `\cases` or `\mcase`), which can have multiple patterns which
   must have parentheses.

 - The possibility to have a construct similar to `-XMultiWayIf` but without the keyword, i.e. using guards directly as
   an expression, was also raised in the discussion. If this were to be used, any pattern
   matching would have to be done with pattern guards.

## Implementation Plan

I (Jakob Brünker) will implement this proposal.
