---
author: Las Safin
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/545>).

# Deterministic operator parsing

Introduce `DeterministicOperatorParsing` language extension
to determine associativity and precedence from the operator name
instead of from its definition.

## Motivation

Haskell supports custom operators and allows you to specify custom precedences and
associativities for the custom operators.
However, this means you can _not_ parse Haskell code without knowing how the operators
were defined, e.g. `a $$$ b $$$ c` could mean either `(a $$$ b) $$$ c` or `a $$$ (b $$$ c)`.

This means formatterse like Ormolu need to keep a database of all common operators,
and support specifying custom operators in the configuration file, rather than Just Working™.
See e.g. https://github.com/tweag/ormolu/issues/927

Nim already does a similar thing: https://nim-lang.org/docs/manual.html#syntax-associativity

## Proposed Change Specification

When Haskell is parsed with this `-XDeterministicOperatorParsing` enabled,
all associativities and precedences will be determined from the name, and name only.

Operators from `base` (as of 4.18.0.0) are special-cased and retain their current behaviour.
New operators in `base` are subject to the same rules as for custom operators.

### Associativity

Associativity for custom operators will be determined from the first symbol in the name.
The table for which associativity is assigned is listed in the appendix.

### Predence

Determining the relative precedence of two custom operators in an expression
`x A y B c` is done via "comparing" them according to the order in the table,
such that if A's first symbol is higher than B's first symbol, then A is higher,
if they're equal, then look at the next symbol, or in the reverse case, B is higher.

When determining the relative precedence of a custom operator and a special-cased one,
the same algorithm as above is used, but the special-cased operator is considered to consist
of an infinite repetition of the same pseudo-symbol that has as precedence the operators precedence.

That is, if the first symbol in the custom operator has a precedence different from that of
the special-cased one, then the relative order of those two precedences are used,
but if the first symbol has a precedence _equal_ to the one of the special-cased operator,
then the next symbol of the operator is considered.

## Examples

`a +| b *| c` is equivalent to `a +| (b *| c)`.
`a +| b -> c` is equivalent to `(a +| b) -> c`.
`a $$$ b $$$ c` is equivalent to `a $$$ (b $$$ c)`.
`a ~~~ b ~~~ c` is invalid.
`a +* b + c` is equivalent to `(a +* b) + c`.

## Effect and Interactions

Given _only_ the precedences and associativities of the operators in `base` as of now,
any Haskell file can be successfully parsed, formatted, and manipulated,
if `-XDeterministicOperatorParsing` is enabled.

Formatters would be able to take advantage of this fact if told to use it,
or if it's enabled via a language pragma.

## Costs and Drawbacks

Adding a new language extension adds complexity. In addition, the
same code can mean _different_ things depending on whether the extension
is enabled, e.g. `a ++++ b ++++ c` could be `True` or `False` depending
on whether the extension is enabled.

## Alternatives

You could entirely disallow custom operators, but that seems less useful.
This wouldn't need a language extension, just a warning when you use
an operator not in `base` without `()`-ing it.

## Unresolved Questions

The specific values in the tables.

## Implementation Plan

I, Las Safin, would implement this change, assuming minor guidance is given (wrt. where to look in GHC).

## Endorsements

None

# Appendix

The below tables have been designed to make existing Haskell code
parse the same way as much as possible.
All symbols from ASCII have been accounted for.
Symbols from Unicode not in ASCII are subject to the default.

## Associativity table for custom operators not in `base`

| First symbol in operator name  | Associativity |
|:------------------------------:|:-------------:|
| !  | left  |
| #  | right |
| $  | right |
| %  | left  |
| &  | left  |
| *  | left  |
| +  | left  |
| -  | left  |
| .  | right |
| /  | left  |
| :  | right |
| <  | none  |
| =  | none  |
| >  | none  |
| ?  | right |
| @  | none  |
| \\ | none  |
| ^  | right |
| \| | left  |
| ~  | none  |
| otherwise | left |

## Precedence table for custom operators not in `base`

| Symbol    | Precedence (higher is tighter) |
|:---------:|:------------------------------:|
| ! .       | 9 |
| # ^       | 8 |
| % * /     | 7 |
| - +       | 6 |
| : \\      | 5 |
| < > = ? ~ | 4 |
| \|        | 2 |
| & @       | 1 |
| $         | 0 |
