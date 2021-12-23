---
author: Ignat Insarov
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/477).

# Proposal title

Unicode ellipsis.

## Motivation

We already have beautiful Unicode make-up for all reserved symbols. And they look great!

But no, not for all. The poor ellipsis `..` is left without. It wants to look like this: `…`.

## Proposed Change Specification

Make `…` accepted wherever `..` is accepted.

## Examples

```haskell
{-# language UnicodeSyntax #-}
{-# language RecordWildCards #-}

module X where

import Prelude (Bool (…))

data Record = Record {value ∷ Bool}

function ∷ Record → Bool
function Record {…} = value

booleans ∷ [Bool]
booleans = [False…]
```

## Effect and Interactions

Ellipsis gets its beautiful Unicode make-up.

## Costs and Drawbacks

There are no costs and no drawbacks.

## Alternatives

There are no alternatives.

## Unresolved Questions

There are no unresolved questions.

## Implementation Plan

Apply this patch:

```diff
diff --git a/compiler/GHC/Parser/Lexer.x b/compiler/GHC/Parser/Lexer.x
index d74d17be8f..4135372d31 100644
--- a/compiler/GHC/Parser/Lexer.x
+++ b/compiler/GHC/Parser/Lexer.x
@@ -1063,6 +1063,7 @@ reservedSymsFM = listToUFM $
        ,("-<<", ITLarrowtail NormalSyntax,  NormalSyntax,  xbit ArrowsBit)
        ,(">>-", ITRarrowtail NormalSyntax,  NormalSyntax,  xbit ArrowsBit)
 
+       , ("…",  ITdotdot,                   UnicodeSyntax,  0 )
        ,("∷",   ITdcolon UnicodeSyntax,     UnicodeSyntax, 0 )
        ,("⇒",   ITdarrow UnicodeSyntax,     UnicodeSyntax, 0 )
        ,("∀",   ITforall UnicodeSyntax,     UnicodeSyntax, 0 )
```

## Endorsements

There are no endorsements.

