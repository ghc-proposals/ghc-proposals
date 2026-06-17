---
author: Oleg Grenrus
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/689>).

# Permutation import syntax

This proposal adds a new extension `-XPermutationImportSyntax` greatly
relaxing the import syntax, and therefore extends `-XImportQualifiedPost`.


## Motivation

Syntax is a topic which people hardly ever agree upon.
People have various taste, and arguments to have syntactic structure
way or another are often sound. The `-XImportQualifiedPost` extension
added some freedom in import syntax, we can have

```hs
import qualified Foo as F
-- or
import Foo qualified as F
```

However, the import syntax was recently extended by [explicit level imports](https://github.com/ghc-proposals/ghc-proposals/pull/682),
and it's very possible it will be extended again in some other way.
In discussion of [explicit level imports](https://github.com/ghc-proposals/ghc-proposals/pull/682) is a fair bit of syntax bikeshedding.

We propose to essentially allow any permutation of import statement parts to be a valid import statement, for example

```hs
import Foo as F qualified
```

## Proposed Change Specification

The [explicit level imports](https://github.com/ghc-proposals/ghc-proposals/pull/682) is at the moment the latest accepted proposal
amending the import syntax. The import statement rule looks like

```
importdecl :: { LImportDecl GhcPs }
   : 'import' maybe_src maybe_safe optlevel optqualified maybe_pkg modid optlevel optqualified maybeas maybeimpspec
```

As we can see, the rule now is already allowing `optlevel` (for explicit level imports) and `optqualified` (for `quantified`)
in two places, with an extra side-condition that `optlevel` and `optqualified` can occur at most once in the import statement.

We propose to essentially simplify the `importdecl` to be

```
importdecl :: { LImportDecl GhcPs }
   : 'import' π(maybe_src | maybe_safe | optlevel | optqualified | maybe_pkg | modid | maybeas | maybeimpspec)
```

where `π(A|B|C)` is a notation for *any permutation of A, B and C*.

The rule could be expanded into a tree (or more precisely a forest) of ordinary rules,
or parsed as `(A|B|C)*` with a side-condition that each sub-production can (should) occur only once.
This is however an implementation detail. It might be possible to extend `happy` to allow
permutation rules directly.

## Examples

This section illustrates the specification through the use of examples of the
language change proposed.

The following are examples from [explicit level imports](https://github.com/ghc-proposals/ghc-proposals/pull/682) proposal:

```haskell
import splice A
import qualified A splice
import quote qualified B as QB
import C splice
import qualified C splice as SC
-- The following are accepted provided ImportQualifiedPost is also enabled:
import quote B qualified as QB
import D quote qualified as QD
```

and will be allowed with `-XPermutationImportSyntax`.

The `-XPermutationImportSyntax` will then allow a lot more freedom:
For example, `import qualified Data.A as A (foo, bar)` can be permuted in 4! = 24 ways:

```hs
import qualified Data.A as A (foo, bar)
import Data.A qualified as A (foo, bar)
import as A Data.A qualified (foo, bar)
import Data.A as A qualified (foo, bar)
import as A qualified Data.A (foo, bar)
import qualified as A Data.A (foo, bar)
import (foo, bar) as A Data.A qualified
import as A (foo, bar) Data.A qualified
import as A Data.A (foo, bar) qualified
import (foo, bar) Data.A as A qualified
import Data.A (foo, bar) as A qualified
import Data.A as A (foo, bar) qualified
import (foo, bar) qualified Data.A as A
import qualified (foo, bar) Data.A as A
import qualified Data.A (foo, bar) as A
import (foo, bar) Data.A qualified as A
import Data.A (foo, bar) qualified as A
import Data.A qualified (foo, bar) as A
import (foo, bar) qualified as A Data.A
import qualified (foo, bar) as A Data.A
import qualified as A (foo, bar) Data.A
import (foo, bar) as A qualified Data.A
import as A (foo, bar) qualified Data.A
import as A qualified (foo, bar) Data.A
```

If we add `splice` / `quote`, package name, `{-# SOURCE #}`, the amount of permutation grows exponentially.
We can mention that

```haskell
import Foo {-# SOURCE #-} (foo, bar)
```

would be spiritually like `-XImportQualifiedPost`, but currently isn't allowed by any extension.

## Effect and Interactions

This extension supersets `-XImportQualifiedPost`.

## Costs and Drawbacks

One potential consequence of this proposal would be proliferation
of very different import statement permutations, which may turn out to be confusing.
On the other hand, ecosystem might converge to use just few permutation schemes,
which would feel natural in hindsight, but aren't allowed at the moment.

## Backward Compatibility

This proposal doesn't change behaviour of any existing programs.

## Alternatives

One suggested altenative is to relax import syntax even further, allowing `import` to be permuted as well:

```
importdecl :: { LImportDecl GhcPs }
   : π('import' | maybe_src | maybe_safe | optlevel | optqualified | maybe_pkg | modid | maybeas | maybeimpspec)
```

This would allow import statements like

```hs
qualified import A
```

At first this looks like potentially stealing syntax, but as long as `import` is a true keyword and occurs only in `importdecl` rule, the `importdecl` grammar rule doesn't overlap with any other rule, so there are no ambiguity. It may be greater engineering challenge to implement though.

## Unresolved Questions

TBW

## Implementation Plan

TBW

## Endorsements

TBW
