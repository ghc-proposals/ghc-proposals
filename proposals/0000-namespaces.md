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
A namespace is a entity which captures the names of other entities (data types, types, and classes), so that they can easily be exported and referenced at will. 
This proposal also introduces using another access qualifier for
namespaces to differentiate them from modules (`:`).

## Motivation

The proposal will enable us to write succinct code and have better control of the exports.

**The goal** is to 
1. add first-class namespaces to Haskell, 
2. with semantic and syntactic changes so small that it can be enabled by default, 
3. which enables incremental adoption of more advanced features.

**Why?** currently, due to the lack of namespaces, if you have two constructors, types, functions, patterns,
or type classes named the same, you have one of three options:
1.  Use the module system and move the different entities to different modules and import them qualified.
    This have several down-sides. 
    One, it promotes many small files which can be hard to get an overlook at.
    Two, it is hard to teach and communicate as some example are spread over multiple files. 
    Three, this is not an option when using Haskell as a scripting language, as single files are often a requirement.

2.  Use type classes to capture similarities. This leads to developers creating type classes without any meaning, purely
    to capture similar named functions. This solution also breaks down fast if the similar named items have
    different arity or arguments.

3.  Renaming things. This is the most common solution. Pre- or Suffixing namespace identifies to solutions; 
    e.g. `prettyExpr`, `prettyType`, `prettyVar`, `MkT`, `runState`, `applyEndo`, and so on. All of these 
    ad-hoc namespaces can often be infered by context, which makes them aqward to use. Also this trick does
    not work for operators.

The rest of the motivation is a list of examples mirroring [#283](https://github.com/ghc-proposals/ghc-proposals/pull/283)

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

### Example 5: Sane operators

It's often not the case that we want to reuse operators for other things
but overloading a common used operator requires the user to either not use the other 
operator or import ours qualified. 

Example, `FilePath` uses the `</>` operator to avoid classes with the standard library, 
but if we have namespaces it could use `/`:

```haskell
import System.FilePath named

main = do 
  write path (show ( 1.0 / 2.0 :: Float))
 where
  path = "my" / "interesting" / "half.txt"
   where open FilePath ((/))
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

A note on `Data.List.:` and on constructor operators. The choice of `:` as the delimiter is the 
least worst choice. We'll use the same semantics as in Haskell 2010. `A::+` means the type operator 
`:+` from `A:`, but we expect that people will mostly use operators unqualified.

#### Extensions to Declarations and Bindings ($4)

We'll add a new top-level namespace construct of the form:
```
topdecl 
  → ...
 | 'space' [nameid ['=' conid]] [exports] 'where' topdecls;
```

This will have the semantics of capturing the declarations in `topdecls` and encapsulating them from the rest of the module.
The declarations in the `topdecls` can access the variables of the `topdecls`.

It will then create a fresh variable `nameid` which is the namespace, which is limited by the `exports` 
specification. 
The `= conid` syntax allows the user to bind a type or class to the `nameid`.

If the 'nameid' is omitted, then we'll refer to the space as anonymous, and loads 
everything limited by the (x) into scope.
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
 | 'open' qnameid [impspec];
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
The `as nameid` uses the name `nameid` as the name of the namespace, otherwise use the 
name after the last `.` in the `modid`.

Continuing the example from before we get:
```haskell
import M named as X
-- { X:Truth |-> (M, Truth:Bool)
-- , X:Truth:True -> (M, Truth:True)
-- , X:Truth:False |-> (M, Truth:False)
-- }
```

The semantics of `imspecs` does not change, `import A (X(y))` will import 
`X` and put `y` into scope. 

The semantics of `exports` does not change, `module A (X(y)) where` will export
`X` and limit its arguments to `y`. 

### (Optional) Extensions

We could think of many extensions, the most obvious are:

-  `-XAutoNamespaces`: Automatically create namespaces for data, newtype and classes; so that fields and subfunctions do not polute the current space:

   ```haskell
   data Nat = Zero | Succ Nat

   one :: Nat
   one = Nat:Succ Nat:Zero
   ```

   Crucially, this extension is not part of `Namespaces`, as it changes the semantics of the code by removing 
   `Zero` and `Nat` from the environment.

 
-  Auto open qualifier, used in conjunction with `-XAutoNamespaces`:
   ```haskell
   open data Nat = Zero | Succ Nat
   ```
   is equivelent to
   ```haskell
   data Nat = Zero | Succ Nat
   open Nat
   ```

-  Renaming variables at open, imports, and on exports, seem valuable, but might be better in another proposal.
   For example: 
   ```
   open FilePath ((</>) as (/))
   ```

## Examples

Here are a list of illustrative examples of edge cases:

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
    space Z where
      z = 0;

-- This will bring x into scope.
open S:Y (x);

-- This will bring Y and x into scope.
open S (Y (x)); 

-- This is currently illegal:
open S (Y (Z (x))); 

-- This is fine
open S:Y:Z (x))); 
```

### Merging Namespaces

It's impossible to extend a namespace; but we can merge two namespaces under a new name.
```haskell
space S1 where
  ...

space S2 where
  ...

space S where
  open S1
  open S2
```

Conflicts between S1 and S2 are handled as usual.


### Conflicting Modules and Namespaces

The difference between `:` and `.` allows us to differentiate between
modules and namespaces, so they can co-exists, like modules and types currently co-exist.

```haskell
import M
space M where
  x = 1
-- both valid M.x M:x 
```

This might not be ideal, but it simplifies a lot of edge cases, and using the language extension 
would cause minimal conflicts. 

## Effect and Interactions

The goal of the proposal is to make a incremental step towards a better modules system.
This system enables all the examples in the motivation without changing the existing module system.
Hopefully this will allow a possible incremental adoption.

Interactions:

1.  Backpack?

1.  TH?


### Future Work

This section is only to demonstrate that this is not the end station of the proposal, just an initial 
stepping stone to get the ball rolling.
If this proposal is adopted and used in the community, a purely syntactic change could be adopted, 
which eliminates modules and have them replaced by namespaces, e.g. `-XLocalModules`.

```haskell
import Module.A  
import qualified Module.A  
-- stops to exist

import Module.A named as X
-- could be written
from "package" 
  include "Module/A" as X

-- namespaces now canibalize the module syntax.
space A () where
  ..
-- ->
module A () where
  ..

open A
-- ->
import A

-- And A:x becomes A.x, as '.' is no more used in modules.

-- Files which limits their exports are wrapped in an empty namespace, now module:
module (f, g) where
...
```
These changes would not be visible outside of the module, but might cause confusion when read.

## Costs and Drawbacks

1. Adding a new language feature is costly, and a feature that is close to but not quite a module, might be
   confusing to newcommers. 

2. Using the delimiter `:` will break some list code; and maybe code which uses type operators `a:|x`.
   But this should only be a problem if the extension is on.

3. Since libraries can export namespaces, they might proliferate the extension to the projects that uses the
   libraries.
   We don't want to force people to use namespaces against their will.
   However, the adoption cost is fairly limited (putting spaces around `:`).
   For library developers it might be an extra burden to maintain both a namespace version and a namespace free
   version. However this can be mitigated, for example to regain the syntax from the original `Hedgehog` (see 
   example), the developer (or user) of the library can add two files, to emulate the old style:
   ```
   -- Hedgehog/Gen.hs
   {- LANGUAGE Namespaces -}
   module Hedgehog.Gen where import Hedgehog (Gen(..))
   -- Hedgehog/Range.hs
   {- LANGUAGE Namespaces -}
   module Hedgehog.Range where import Hedgehog (Range(..))
   ```

## Alternatives

1. Compared to [#283](https://github.com/ghc-proposals/ghc-proposals/pull/283), this proposal 
   uses a simpler/cleaner syntax. Which is what [#295](https://github.com/ghc-proposals/ghc-proposals/pull/295) 
   set out to do.

2. Compared to [#295](https://github.com/ghc-proposals/ghc-proposals/pull/295), this proposal is a
   compromise which allows for incremental addoption.

3. Rebuilding the module system from scratch. This properly requires a new version of Haskell.

## Unresolved Questions

1. The choice of seperator. I have chosen `:` mostly as a starting point but other operators could be chosen.
   Using the `::` seperator might be a soloution, but is problematic with types. `A::B :: C` is less clear
   than `A:B :: C`, and `A:a : []` not much worse than `A::a : []`. In a normal file `::` is used more than `:`.
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
     other things.
   ```

1. Should we be able to rebind namespaces in this proposal, both in imports/exports but also 
   in namespaces?
   ```haskell
   space M = NY 
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
