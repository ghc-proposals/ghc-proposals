---
author: Christian Gram Kalhauge
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/564).

# Namespaces

One of the pain-points for building structured, succinct, and easy to read Haskell code is name conflicts. 
The lack of a fine-grained namespace controls lead developers to either creating many small files, unwanted type classes, or ad-hoc namespace measures as prefixing names to variable names.

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
    This have several down-sides:
    
    a) it promotes many small files which can be hard to get an overlook at;
    
    a) modules cannot have cyclic dependencies, eliminating some cases;
    
    b) it is hard to teach and communicate as some example are spread over multiple files; and
    
    c) this is not an option when using Haskell as a scripting language, as single files are often a requirement.

2.  Use type classes to capture similarities. This leads to developers creating type classes without any meaning, purely
    to capture similar named functions. This solution also breaks down fast if the similar named items have
    different arity or arguments.

3.  Renaming things. This is the most common solution. Pre- or Suffixing namespace identifies to solutions; 
    e.g. `prettyExpr`, `prettyType`, `prettyVar`, `MkT`, `runState`, `applyEndo`, and so on. 
    Besides (subjectivly) being akward to use and read, it has multiple drawbacks.
    
    a) It prompotes unqualified import of modules, e.g., `State.runState` seems redundant.
       This is turn will pull more items into scope making it harder to present good completion
       options for the user of an IDE.
    
    b) With suffix notation is hard to discover the abilities of a types using an IDE, e.g., 
       typing `pretty` will yield `{prettyExpr, prettyType, prettyVar}`, but typing 
       `Expr:` could yield `{Expr:pretty, Expr:evaluate, Expr:normalize, ...}`.
    
    c) The trick does not work for operators.

The rest of the motivation is a list of examples mirroring [#283](https://github.com/ghc-proposals/ghc-proposals/pull/283)

### Example 1: Compact code

If we want associate the same named function with two data structures, we'll have 
to create two files, create a type class, or prefix the names of the function.

With namespaces you can write this:

```haskell
module Vec where

namespace V2 where
  data V2 = Mk { x :: Int, y :: Int }
  fromV2 :: V2 -> V2

namespace V3 where
  data V3 = Mk { x :: Int, y :: Int, z :: Int }
  fromV2 :: V2:V2 -> Int -> V3

-- Creates the vector (V3:Mk 0 1 1) 
vec_0_1_1 :: V3:V3
vec_0_1_1 = V3:fromV2 (V2:fromV2 (V2:Mk 0 1)) 1
```

This example creates two namespaces to contain the V2 and V3 data structures, 
their field accessors, constructors, and their
associated functions `fromV2`. 
We can access the inners of `V2` and `V3` using the namespace operator, `:`, 
this means that `V2:fromV2` points to the `fromV2` function in `V2`.

This illustrates a common problem that you cannot easily fix with type classes, multiple functions
with the same name, but different arguments. I that case you have to use adhoc-namespaces:

```haskell
module Vec where

data V2 = MkV2 { v3x :: Int, v3y :: Int }
fromV2V2 :: V2 -> V2

data V3 = MkV3 { v3x :: Int, v3y :: Int, v3z :: Int }
fromV2V3 :: V2 -> Int -> V3

vec_0_1_1 :: V3
vec_0_1_1 = fromV2V3 (fromV2V2 (MkV2 0 1)) 1
```

Or split it over multiple files:

```haskell
-- src/Vec/V2.hs
module Vec.V2 where
data V2 = Mk { x :: Int, y :: Int }
fromV2 :: V2 -> V2
```

```haskell
-- src/Vec/V3.hs
module Vec.V3 where
import Vec.V2 qualified as V2
data V3 = Mk { x :: Int, y :: Int, z :: Int }
fromV2 :: V2.V2 -> Int -> V3
```

```haskell
-- src/Vec.hs
module Vec where
import Vec.V2 qualified as V2
import Vec.V3 qualified as V3

vec_0_1_1 :: V3.V3
vec_0_1_1 = V3.fromV2 (V2.fromV2 (V2.Mk 0 1)) 1
```

However, in the last case you can't write a function `fromV3` in `Vec.V2`
directly as it would create a cyclic dependency. In this case you would have 
to follow [5.8.10](https://downloads.haskell.org/ghc/latest/docs/users_guide/separate_compilation.html#how-to-compile-mutually-recursive-modules) in the user guide; create a `hs-boot` file for `Vec.V3`, containing only 
the datatype `V3` and written in a subset of Haskell; then you would have to import
it using the `{#- SOURCE #-}` pragma. Needless to say this is a little complicated.

### Example 2: Associated Functions

By using namespaces, we can associate specific functions with the data it operates
on. The `= Set` notation means that, the namespace `Set` will act like the entity `Set` when not used as a qualifier, e.g., `Set:a`.

```haskell
-- in Data/Set.hs
module Data.Set (Set) where

namespace Set = Set (Set, fromList) where
  data Set = ..

  fromList :: [a] -> Set a
  fromList = ... 

-- in Main.hs
import Data.Set (Set)

mySet :: Set String
mySet = Set:fromList ["A", "B"]
```

Using the `namespace Set = Set` notation is done to enable users to use the common Haskell
pattern. 
```haskell
import Data.Set (Set)
import qualified Data.Set as Set
```
Furthermore, it enables a straight forward syntactic description of turning all entities 
into namespaces, see the `-XAutoNamespaces` extension.

### Example 3: MyPrelude

By using namespaces, we can collect names used often in a prelude and export them qualified.

```haskell
-- In MyPrelude.hs
module MyPrelude ( BL, BS ) where

import namespace Data.ByteString.Lazy as BL
import namespace Data.ByteString as BS

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

namespace (f) where
  f = get . foldMap Mk
  data Collection = Mk { get :: ... }
```

### Example 5: Local imports and sane operators

It's often the case that we want to reuse operators
but overloading a common used operator, 
requires the user to either not use the other operator or import one 
of them qualified. This is especially awkward with operators.

Example, `FilePath` uses the `</>` operator to avoid classes with the standard library, 
but if we have namespaces it could use `/`:

```haskell
import namespace System.FilePath 

main = do 
  write path (show ( 1.0 / 2.0 :: Float))
 where
  path = "my" / "interesting" / "half.txt"
   where open FilePath ((/))
```

Another good example is the `lens` library. When importing lens, we have to import 
it unqualified to use the operators, but not all code needs 
all the operators:

```haskell
import namespace Control.Lens 

nonLensCode = do 
  --- this code can use ^. for other things.
  ...

lensCode = do 
  name .= "hello"
 where
  open Lens
```


## Proposed Change Specification

Note: We're using the nomenclature and notation introduced by
section 2 in [#283](https://github.com/ghc-proposals/ghc-proposals/pull/283), the section could be moved
here for context if necessary.

This proposal suggest doing the following in a nutshell:
```haskell
namespace A where
  x, y :: Int
  x = 0
  y = 10
```

Create a new top-level-declaration, `namespace A where ...`, which can declare a new namespace.
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

We'll add a new `open` expression construct of the form:
```
lexp
  → ...
 | 'open' qnameid [impspec] 'in' exp      
```

This expression makes the content of the namespace available in the 
exp. 

```haskell
namespace X where
  x = 0

one = open X (x) in x + 1
```

Note: This is just a lets bloated from of `let open X (x) in x + 1` or 
`x + 1 where open X (x)`, introduced later

We'll and a `open` statement construct of the form:
```
stmt
  → ...
 | 'open' qnameid [impspec] ';'
```

Which does the same, but in do-notation.

```haskell
main = do 
  open X (x)
  print x
```

#### Extensions to Declarations and Bindings ($4)

We'll add a new top-level namespace construct of the form:
```
topdecl 
  → ...
 | 'namespace' [nameid ['=' qconid]] [exports] ['where' topdecls];
```

This will have the semantics of capturing the declarations in `topdecls` and encapsulating them from the rest of the module.
The declarations in the `topdecls` can access the variables of the `topdecls`.

It will then create a fresh variable `nameid` which is the namespace. 
The exported content of the namespace is limited by the `exports` 
specification. 
The `= qconid` syntax allows the user to bind a qualified type or class to the `nameid`.

If the `['where' topdecl]` is omitted then the `qconid` must be a reference to another namespace. 
And essentially acts as a rename.
```haskell
namespace C = Containers
-- Now C is an alias for Containers.
```


If the 'nameid' is omitted, then we'll refer to the namespace as anonymous, and loads everything limited by the exports into scope.
 ```haskell
 namespace (x) where
    x = y + 1
    y = 1
 ```
 which is equivalent to 
 ```
 namespace _Unbindable (x) where
    x = 0
 open _Unbindable
 ```

Example, borrowing the notation from [#283](https://github.com/ghc-proposals/ghc-proposals/pull/283), 
where environments are mappings from qualified variables to original name `{ X |-> (M, m)}`.
```haskell
module M where
namespace Truth = Bool (True, False) where
  data Bool = True | False

-- Environment outside Thruth is 
-- { Truth |-> (M, Truth:Bool)
-- , Truth:True -> (M, Truth:True)
-- , Truth:False |-> (M, Truth:False)
-- }
```

As is customs: It's okay to declare two entities (and in this case namespaces) of the same name, but referring to either would give an ambiguity error.

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

Note: `open` is explicitly chosen to be small, so that it fits inside most
expressions. This will hopefully encourage more quantified imports.

#### Extensions to Modules ($5)

We'll introduce a new import statement, of the form:

`impdecl → 'import' 'namespace' modid ['as' nameid] [impspec]`

Which saves the content of the module into a namespace.
The `as nameid` uses the name `nameid` as the name of the namespace, otherwise use the 
name after the last `.` in the `modid`.

Continuing the example from before we get:
```haskell
import namespace M as X
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

We could think of many extensions, though not part of this proposal, the most obvious are:

-  `-XAutoNamespaces`: Automatically create namespaces for data, newtype and classes; so that fields and subfunctions do not polute the current namespace:

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

-  Extended namespaces, used in conjunction with `-XAutoNamespaces`, 
   allows users to assign extra names to an auto namespace:

   ```haskell
   data Nat = Zero | Succ Nat
    where
     pattern One = Succ Zero
     (+) :: Nat -> Nat -> Nat
     a + b = case a of 
      Zero -> b
      Succ a -> a + Succ a

   check a = case a of
    Nat:One -> print "Is One!"
    _ -> print "It's not one!"
   ```

-  Renaming variables at open, imports, and on exports, seem valuable, but might be orthogonal to this proposal.
   For example: 
   ```haskell
   open FilePath ((</>) as (/))
   ```

## Examples

Here are a list of illustrative examples of edge cases:

### Basic usage

Given a namespace `S`:
```haskell
namespace S where
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
namespace S where
  namespace Y where
    x = 0
    namespace Z where
      z = 0;

-- This will bring x into scope.
open S:Y (x);

-- This will bring Y and x into scope.
open S (Y (x)); 

-- This is currently illegal:
open S (Y (Z (z))); 

-- This is fine
open S:Y:Z (z))); 
```

### Nested assigns

```haskell
module T where
namespace X = Y where
  namespace Y = Z where
    data Z; 

-- The scope is now
-- { X |-> (T, X:Y:Z)
-- , X:Y |-> (T, X:Y:Z)
-- , X:Y:Z |-> (T, Z:Y:Z)
-- }

-- But this is illegal, as X does not inherit the scope of Y.
type V = X:Z
```

### Merging Namespaces

It's impossible to extend a namespace; but we can merge two namespaces under a new name.
```haskell
namespace S1 where
  ...

namespace S2 where
  ...

namespace S where
  open S1
  open S2
```

Conflicts between the names in S1 and S2 are handled as usual.


### Conflicting Modules and Namespaces

The difference between `:` and `.` allows us to differentiate between
modules and namespaces, so they can co-exists, like modules and types currently co-exist.

```haskell
import M
namespace M where
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

1.  TH, due to the way this extension is build, splices should work
    as they would with defining type classes.

1.  Haddock, new syntax and/or document producers would need to be made to correctly document namespaces. Maybe an extension to the current type-class layout could be enough.

2.  `OverloadedRecordDot` should not be affected by this language feature, even if
    `AutoNamespaces` is enabled, since it esentially creates class instances of 
    the `HasField` class:

    ```haskell
    data V2 = Mk { x :: Int, y :: Int }

    V2:x :: V2 -> Int
    (.x) :: HasField "x" a Int => a -> Int

    validExpr = (V2:Mk 1 0).x :: Int
    alsoValidExpr = V2:x (V2:Mk 1 0) :: Int
    ```

3. `NoFieldSelectors` and `DuplicateRecordFields` could be considered obsolete by the `AutoNamespaces` feature.

1.  Backpack?




### Future Work

This section is only to demonstrate that this is not the end station of the proposal, just an initial 
stepping stone to get the ball rolling.
If this proposal is adopted and used in the community, a purely syntactic change could be adopted, 
which eliminates modules and have them replaced by namespaces, e.g. `-XLocalModules`.

```haskell
import Module.A  
import qualified Module.A  
-- stops to exist

import namespace Module.A as X
-- could be written
from "package" 
  include "Module/A" as X

-- namespaces now canibalize the module syntax.
namespace A () where
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
These changes would not be visible outside of the module.

## Costs and Drawbacks

1. Adding a new language feature is costly, and a feature that is close to, but not quite a module, might be confusing to newcommers. 
   We have tried to make the syntax clear and break as little code if enabled by default.

2. Using the delimiter `:` will break some list code; and maybe code which uses type operators `a:|x`.
   But this should only be a problem if the extension is on.

3. Since libraries can export namespaces, they might proliferate the extension to the projects that uses the
   libraries.
   We don't want to force people to use namespaces against their will.
   However, the adoption cost is fairly limited (putting spaces around `:`) and not using `open`, and `namespace` as identifiers.
   For library developers it might be an extra burden to maintain both a namespace version and a namespace free
   version. However this can be mitigated, for example to regain the syntax from the original `Hedgehog` (see 
   example), the developer (or user) of the library can add two files, to emulate the old style:
   ```haskell
   -- Hedgehog/Gen.hs
   {- LANGUAGE Namespaces -}
   module Hedgehog.Gen where import Hedgehog (Gen(..))
   -- Hedgehog/Range.hs
   {- LANGUAGE Namespaces -}
   module Hedgehog.Range where import Hedgehog (Range(..))
   ```

1. When introducing a new namespace is "qualified", and does not leak its internals. This is
   contrary to the current haskell mindset where `import A` will export everything except if it is qualified. 
   We believe that this choice makes sense regardless, as you can
   always open a namespace after.

1. Adding namespaces might make the code a little harder to read as the name at definition
   point might not be the same as at use point. 

1. Automatic documentation might also be less clear. Thought needs to go into a good way of
   pressenting namespaces to developers.

## Alternatives

1. Compared to [#283](https://github.com/ghc-proposals/ghc-proposals/pull/283), this proposal 
   uses a simpler/cleaner syntax. Which is what [#295](https://github.com/ghc-proposals/ghc-proposals/pull/295) 
   set out to do.

2. Compared to [#295](https://github.com/ghc-proposals/ghc-proposals/pull/295), this proposal is a
   compromise which allows for incremental addoption.

3. Rebuilding the module system from scratch. This properly requires a new version of Haskell, which would cause a fork between old and new developers of the language.

4. Doing nothing. We fell like this is unsatisfactory, for the
   reasons given in the motivation.

## Unresolved Questions

1. The choice of separator. We have chosen `:` mostly as a starting point but other operators could be used instead.
   - Choosing `.` as the separator seems to create a lot of edge cases when working with modules and namespaces.

     ```haskell
     import Data.Set 
     import Data

     -- Is this a module lookup of 'x' from Data.Set or 
     -- a namespace lookup of 'Set.x' from Data. 
     Data.Set.x
     ```
   - Using the `::` separator might be a solution, but is problematic with types. `A::B :: C` is less clear
     than `A:B :: C`, and `A:a : []` not much worse than `A::a : []`. In a normal file `::` is used more than `:`.

   - Using the `#` separator might be a solution: it's used mostly with `MagicHash` and the `CPP` extensions, however only
     in suffix and prefix mode.
     A module selector would be infix.
     Example:
     ```haskell
     fromSet :: Set#Set -> [Int]
     fromSet = Set#foldr (List#:) []
     ```

   - Using the `\` separator might also be a solution. It's used for lambdas which are rarely not prepended with a space, 
     e.g., `a\x ->b` is an illegal construction unless BlockArguments are enabled (Credit to AntC for the suggestion).
     Example:
     ```haskell
     fromSet :: Set\Set a -> [a]
     fromSet = Set\foldr (List\:) []
     ```

1. Adding two new tokens `open` and `namespace` will shadow some names possible used by developers.
   Using `import` and `module` would reduce this problem but make the semantics difference harder to notice for newcomers.
   Furthermore, `import` cannot be used at the top level as it clashes with `import` statements.
   We could use `namespace import` to mean `open`, however it is verbose, and that would make less people use the feature.
   ```haskell
   consIncr a l = 
    fmap (+1) (open List in a : l)
   ```
   ```haskell
   consIncr a l = 
    fmap (+1) (namespace import List in a : l) 
   ```

   Potentially we could suffix `open` or `import` with the separator to make conflicts impossible `open:` or `import:`:
   ```haskell
   open:Functor
   consIncr a l = 
    fmap (+1) (open:List in a : l)
   ```
   ```haskell
   import:Functor
   consIncr a l = 
    fmap (+1) (import:List in a : l)
   ```


1. Should the namespaces be explicitly disallowed to export modules `namespace X (module Y) where .. `? We lean towards yes.
   The same effect can be gained by the cleaner:
   ```haskell
   import namespace Y as NY
   namespace X where
     open NY
     -- other things.
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

