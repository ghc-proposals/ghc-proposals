---
author: Christian Gram Kalhauge
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/0>).
**After creating the pull request, edit this file again, update the number in
the link, and delete this bold sentence.**

# Namespaces

One of the pain-points for building structured, succinct, and easy to read Haskell code is name conflicts. 
The lack of a fine-grained namespace controls lead to developers either creating many small files or ad-hoc namespace measures as prefixing names to variable names.

Many prior proposals have tried to come up with a solution to this, especially [#283](https://github.com/ghc-proposals/ghc-proposals/pull/283) and [#295](https://github.com/ghc-proposals/ghc-proposals/pull/295), however they have all gone dormant.
This proposal, is yet another try.
Contrary to the previous approaches, this proposal does not want to change the module system, but instead introduce a new construct: the namespace.
A namespace is a (named) entity which captures the names of other entities (data types, types, and classes), so that they can easily be exported and referenced at will. 
This proposal also introduces using another access qualifier for
namespaces to differentiate them from modules (`:`).

## Motivation

The proposal will enable us to write succinct code and have better control of the exports.
The motivation is a list of examples mirroring [#283](https://github.com/ghc-proposals/ghc-proposals/pull/283)

### Example 1: Compact code

If we want associate the same named function with two data structures, we'll have 
to create two files, create a type class, or prefix the names of the function.

With namespaces you can write this:

```haskell
module Pretty where

space V2 where
  data V2 = Mk { x :: Int, y :: Int }
  pretty :: V2 -> Text
  pretty = ...

space V3 where
  data V3 = Mk { x :: Int, y :: Int, z :: Int }
  pretty :: V3 -> Text
  pretty = ...

main = print (V2:pretty (V2:Mk 0 1) <> V3:pretty (V3:Mk 0 1 0)) 
```

### Example 2: Associated Functions

By using namespaces, we can associate specific functions with the data it operates
on. The `= Set` notation means that, the namespace `Set` will act like the type 
or class `Set` when not used as a qualifier, e.g., `Set:a`.

```haskell
-- in Data/Set.hs
module Data.Set (Set) where

space Set = Set (Set, fromList) where
  data Set = ..

  fromList :: [a] -> Set a
  fromList = ... 

-- in Main.hs
import Data.Set (Set)

mySet :: Set String
mySet = Set:fromList ["A", "B"]
```

### Example 3: MyPrelude

By using namespaces, we can collect names used often in a prelude and export them qualified.

```haskell
-- In MyPrelude.hs
module MyPrelude ( BL, BS ) where

import Data.ByteString.Lazy named as BL
import Data.ByteString named as BS

-- In Lib.hs
module Lib where

import MyPrelude (BL)

toByteString :: Bool -> BL:ByteString
toByteString b = case b of
  True -> pack "True"
  False -> pack "False"
  where open BL (pack)
```

Also using the Hedgehog library could look like:
```haskell
module Spec where

import Hedgehog (forAll, Gen, Range)
```

### Example 4: Local Data Types

We can declare data types locally to a function without polluting the namespace.

```haskell
module Spec where

open X (f) -- ^ instantly export f to the scope
space X where
  f = getCollection . foldMap MkCollection
  data Collection = MkCollection { getCollection :: ... }
```
or if auto namespaces is turned on:
```haskell
module Spec where

open X (f) -- ^ instantly export f to the scope
space X where
  f = Collection:get . foldMap Collection:Mk
  data Collection = Mk { get :: ... }
```

## Proposed Change Specification

Note: We're using the nomenclature and notation introduced by
section 2 in [#283](https://github.com/ghc-proposals/ghc-proposals/pull/283), the section could be moved
here for context if necessary.

This proposal suggest doing the following in a nutshell:
```haskell
space A where
  x, y :: Int
  x = 0
  y = 10
```

Create a new top-level-declaration, `space A where ...`, which can declare a new namespace.
Every declaration inside the where clause is hidden from the rest of the module behind 
the named-qualifier `A`. We can access the variables under `A` either by using 
named quantification `A:x` or by opening the namespace. 
Opening the namespace is a declaration which loads the variables into the relevant scope.
It can be limited an import specification, e.g.:
```haskell
f = let open A in x
f = x where open A
f = x where open A (x)
f = x where open A hiding (y)
```

A namespace is an entity so it can be parsed around and referenced like everything else in the module system. 
This enables sharing of collections of names without real changes to the module system.

### Details

Adding a new language extension `-XNamespaces`, which activates the extension.
This will enable the following syntactic changes. 
We'll use pointers to [Haskell 2010](https://www.haskell.org/onlinereport/haskell2010/haskellpa1.html#haskellch5.html) syntax, when needed.

#### Extensions to the Lexer ($2) and Expressions ($3)

We'll add a new token `namid` to namespace which has the following production
```
namid → conid (namespace)
```

We'll add a lexing rule to that recognizes `nameid:varid` as three tokens 
`namid`, `lookup_namespace`, and `varid`, and not as `namid`, `cons`, `varid`.
Furthermore changes to all qualified names so they accept being qualified by namespaces.
E.g., `A.B:X.z` is a valid qualified identifier, which should be read as the variable 
`z` in the namespace `X`. 

A Note on `Data.List.:` and on constructor operators. The choice of `:` as the delimiter is the 
least worst choice. We'll use the same semantics as in Haskell 2010. `A::+` means the type operator 
`:+` from `A:`, but we expect that people will mostly use operators unqualified.

#### Extensions to Declarations and Bindings ($4)

We'll add a new top-level namespace construct of the form:
```
topdecl 
  → ...
 | 'space' nameid ['=' conid] [exports] 'where' topdecls;
```

This will have the semantics of capturing the declarations in `topdecls` and encapsulating them from the rest of the module.
The declarations in the `topdecls` can access the variables of the `topdecls`.

It will then create a fresh variable `nameid` which is the namespace, which is limited by the `exports` 
specification. 
The `= conid` syntax allows the user to bind a type or class to the `nameid`.

Example, borrowing the notation from [#283](https://github.com/ghc-proposals/ghc-proposals/pull/283), 
where environments are mappings from qualified variables to original name `{ X |-> (M, m)}`.
```haskell
module M where
space Truth = Bool (True, False) where
  data Bool = True | False

-- Environment outside Thruth is 
-- { Truth |-> (M, Truth:Bool)
-- , Truth:True -> (M, Truth:True)
-- , Truth:False |-> (M, Truth:False)
-- }
```

As is customs: It's okay to declare two namespaces of the same name, but referring to either would give an ambiguity error.

We'll add a new declaration `open` construct of the form:
```
decl 
  → ...
 | 'open' nameid [impspec];
```

This will have the semantics of including everything in the namespace (restricted by the optional `impspec`) into the scope of the declaration.

Continuing the example, opening truth gives the following:
```haskell
import M (Truth)
open Truth (False);

-- Environment is 
-- { M.Truth |-> (M, Truth:Bool)
-- , M.Truth:True -> (M, Truth:True)
-- , M.Truth:False |-> (M, Truth:False)
-- , Truth |-> (M, Truth:Bool)
-- , Truth:True -> (M, Truth:True)
-- , Truth:False |-> (M, Truth:False)
-- , False |-> (M, Truth:False)
-- }
```

#### Extensions to Modules ($5)

We'll introduce a new import statement, of the form:

`impdecl → 'import' modid 'named' ['as' nameid] [impspec]`

Which saves the content of the module into a namespace.
The `as nameid` is only optional if the `modid` is without dots.
This namespace follows the same rules as if it was opened.

Continuing the example from before we get:
```haskell
import M named as M
-- { M:Truth |-> (M, Truth:Bool)
-- , M:Truth:True -> (M, Truth:True)
-- , M:Truth:False |-> (M, Truth:False)
-- }
```

The semantics of `imspecs` changes a little, `import A (X(y))` will no longer import `y`, but import 
`X` with access limited to `y`. 
This change only applies to namespaces, unless everything is namespaces (see the optional extensions).

### (Optional) Extensions

We could think of many extensions, the most obvious are:

-  Automatically create namespaces for data, newtype and classes; so that fields and subfunctions do not polute the current space:

   ```haskell
   data Nat = Zero | Succ Nat

   one :: Nat
   one = Nat:Succ Nat:Zero
   ```

-  Empty namespaces:
   ```haskell
   space (x) where
      x = y + 1
      y = 1
   ```
   which is equlivalent to 
   ```
   space _Unbindable (x) where
      x = 0
   open _Unbindable
   ```
 
-  Auto open qualifier, used in conjunction with automatic namespaces:
   ```haskell
   open data Nat = Zero | Succ Nat
   ```
   is equivelent to
   ```haskell
   data Nat = Zero | Succ Nat
   open Nat
   ```

-  Renaming variables at open, imports, and on exports, seem valuable, but might be better in another proposal.

## Examples

### Basic usage

Given a namespace `S`:
```haskell
space S where
  x = 0
```

We can either use it like:
```haskell
open S (x);
f = x
```

Or like this:
```haskell
f = S:x
f a = let open S in x 
f a = x where open S (x)
```

### Nested namespaces

```haskell
space S where
  space Y where
    x = 0

-- This will bring x into scope.
open S:Y (x);

-- This will bring Y into scope, limited to x.
open S (Y (x)); 
```

### Merging Namespaces

It's impossible to extend a namespace; but we can merge two namespaces under a new name.
```haskell
space S1 where
  x = 0

space S2 where
  y = 0

space S where
  open S1
  open S2
```

Conficts between S1 and S2 are handled as usual.

### Conflicting Modules and Namespaces

The difference between `:` and `.` allows us to differentiate between
modules and namespaces, so they can co-exists.

```haskell
import M
space M where
  x = 1
-- both valid M.x M:x 
```

This might not be ideal, but it simplified a lot.

## Effect and Interactions

The goal of the proposal is to make a incremental step towards a better modules system.
This system enables all the examples in the motivation without changing the existing module system.
Hopefully this will allow a possible adoption

Interactions:

1.  Backpack?

## Costs and Drawbacks

1. Adding a new language feature is costly, and a feature that is close to but not quite a module, might be
   confusing to newcommers. 

2. Using the delimiter `:` will break some list code; and maybe code which uses type operators `a:|x`.
   But this should only be a problem if the extension is on.

## Alternatives

1. Compared to [#283](https://github.com/ghc-proposals/ghc-proposals/pull/283), this proposal 
   uses a simpler/cleaner syntax. Which is what [#295](https://github.com/ghc-proposals/ghc-proposals/pull/295) 
   set out to do.

2. Compared to [#295](https://github.com/ghc-proposals/ghc-proposals/pull/295), this proposal is a
   compromise which allows for incremental addoption.

## Unresolved Questions

1. The choice of seperator. I have chosen `:` mostly as a starting point but other operators could be chosen.
   Choosing `.` as the seperator seems to create a lot of edge cases when working with modules and
   namespaces.

   ```haskell
   import Data.Set 
   import Data

   -- Is this a module lookup of 'x' from Data.Set or 
   -- a namespace lookup of 'Set.x' from Data. 
   Data.Set.x
   ```

1. Adding two new tokens `open` and `space` will shadow some names possible used by developers.
   Using `import` and `module` would reduce this problem but make the semantics difference, harder to notice for
   newcomers.

1. Should the spaces be explicitly disallowed to export modules `space X (module Y) where .. `? I lean towards yes.
   The same effect can be gained by the cleaner:
   ```haskell
   import Y named as NY
   space X where
    open NY
   ```

## Implementation Plan

(Optional) If accepted who will implement the change? Which other resources
and prerequisites are required for implementation?

## Endorsements

(Optional) This section provides an opportunity for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.

