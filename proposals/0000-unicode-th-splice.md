---
author: Oleg Grenrus
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/647)

# Unicode Syntax for Template Haskell splice

I propose to add `Integral U+222B` ∫, and `Double Integral U+222C` ∬ unicode characters as [`UnicodeSyntax`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/unicode_syntax.html) variants for Template Haskell and Typed Template Haskell splices respectively.

---

## Motivation

The [`UnicodeSyntax`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/unicode_syntax.html) has variants for most ASCII character sequences in Haskell.
However there are none for the Template Haskell splices.

The S-shape of integral sign is perfect to indicate the *splice*, and not too different from currently used dollar sign `$`.

A specific character `∫` is distinct from `$`, therefore removing syntactical ambiguity. `∫` would unambiguously mean only the Template Haskell splice.

## Proposed Change Specification

In addition to a splice syntax `$x`, where `x` is an arbitrary expression,
we propose to add `∫x` syntax.

Similarly for Typed Tempalate Haskell a typed expression splice may be written `∬x`, where `x` is is an arbitrary expression.

As with dollar syntax there may not be whitespace between the symbol and the expression. The `t ∫ s` would be parsed as infix application of `∫` operator, because `∫` is `Math Symbol (Sm)`.

## Proposed Library Change Specification

There are no library changes.

## Examples

```haskell
g :: (Quote m, C m) => m Exp
g = ⟦ ∫f + ∫f ⟧
```

```haskell
h :: (Quote m, C m) => Code m Int
h = [|| ∬a + ∬(liftTyped $ b * 10) ||]
```

## Costs and Drawbacks

Development and maintenance costs are minimal.
The specific splice symbol may make examples in learning materials and academic papers clearer in the long run.


## Backward Compatibility

Technically using `∫` steals syntax, but it's uncommon. In mathematics integral sign isn't used infix, so its usage should be extremely rare.

There could be a period when the usage of `∫` is warned about, before its introduction as splice operator.
This is similar to [`-Wforall-identifier`](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wforall-identifier) warning.

## Alternatives

An alternative is to always treat `∫` as splice prefix-operator, i.e. allow

```haskell
g :: (Quote m, C m) => m Exp
g = ⟦ ∫ f + ∫ f ⟧
```

Another option is to make `∫` always extend as far right as possible, then we'd
need to write

```haskell
g :: (Quote m, C m) => m Exp
g = ⟦ (∫ f) + (∫ f) ⟧
```

or have extension behaviour only when there is a whitespace following the integral sign.
Then we could write:

```haskell
myfun :: Int -> Int -> Int
myfun x y = ∫ generateMyFun ⟦ x ⟧ ⟦ y ⟧ 

-- but still be able to write

g :: (Quote m, C m) => m Exp
g = ⟦ ∫f + ∫f ⟧
```

while tempting, this kind of whitespace sensitivity might end up being too confusing.

## Unresolved Questions

None.

## Implementation Plan

A preliminary implemention is in [MR !12341](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/12341).
This implementation always treats `∫` as splice operator (no whitespace sensitivity).

## Endorsements

None at the moment.
