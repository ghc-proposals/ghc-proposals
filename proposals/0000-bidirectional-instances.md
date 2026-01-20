---
author: Koen Pauwels
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/284).

# Bidirectional Type Class Instances

The interaction between type classes and GADTs can lead to some unpleasant
surprises. The source of this problem is the fact that GHC treats
instance contexts only as sufficient conditions, while any
(non-overlapping) instance is also automatically a necessary one.

## Motivation

Consider the following GADT:
```haskell
data Term a where
  Con :: b -> Term b
  Tup :: Term b -> Term c -> Term (b,c)
```

A (simplified) implementation for a `Show` class instance might look like this:
```haskell
instance Show d => Show (Term d) where
  show (Con x)   = show x
  show (Tup x y) = concat ["(", show x, ",", show y, ")"]
```

Although the implementation seems quite reasonable at first glance, it is
rejected by the compiler. The wanted constraints from the `Tup` clause
`(Show b, Show c)` 
cannot be deduced from the given constraints `(Show d, d ~ (b,c))`.
This is because GHC considers instance declarations to be unidirectional
implications from the instance context to the instance head. So the `Show`
instance for pairs only introduces an axiom of the form
`∀ a b. (Show a, Show b) => Show (a,b)`.
But since overlapping instances are
disallowed by default, instance declarations are in fact bi-implications (the
conditions in the instance context are not only sufficient but also necessary).
Our proposal is to allow the compiler to treat them as such (by way of a
per-instance declaration pragma), which would be enough to resolve the constraints
for the given example and type check the program.

## Proposed Change Specification

A detailed account of the changes to type system and elaboration required to
support this extension can be found in our paper
[Bidirectional Type Class Instances](https://arxiv.org/abs/1906.12242).
We summarize the key ideas here.

### Type Inference
Each instance declaration tagged as bidirectional will give rise to an additional
"converse" instance axiom for each constraint in its instance context.
More concretely,
an instance declaration of the form `instance (C1, ..., Cn) => C t`
(where `t` is a metavariable for any monotype, and
`C`, `C1`, ..., `Cn` are metavariables that stand in for constraints)
gives rise to the `n` converse axioms `C t => C1`, ..., `C t => Cn`.

These converse instance axioms are treated in much the same way as superclass
axioms for the purpose of type checking; that is, they are used to extend
the set of given constraints.

### Elaboration
Each type class declaration
```haskell
class ... => C a where
  f :: T
```
(where `T` is a metavariable)
generates a type declaration declaration `type F_C a` in the intermediate language.
In Core, the dictionary data type associated with each type class is extended
with an additional "context" field, the type of which is given by the
type family declaration.
In pseudo-Core:
```haskell
data Dict_C a = Dict_C { f   :: T'    -- T' is the elaborated version of T
                       , ctx :: F_C a
                       }
```

Each instance declaration `instance (C1, ..., Cn) => C t` generates a type
equality axiom asserting the equality between the types `F_C t` and a product of
dictionary data types `(D1, ..., Dn)` corresponding to the constraints `C1, ... Cn`.

The context field is used to define the dictionary transformers associated with the
converse instance axioms. 
To construct a dictionary transformer for, say, the axiom `C t => C1`, the context
field is extracted from the dictionary for `C t` and then coerced to the type of the context
dictionary tuple `(D1, ... Dn)`, from which the relevant dictionary (`D1`) is projected.

### Syntax
We propose the addition of a new pragma `{-# BIDIRECTIONAL #-}` which is used 
in a similar way to the `{-# OVERLAPPING #-}` pragma: it tags an individual
instance declaration as bidirectional.

## Examples

### Structural Induction Over Indexed Data Types
The problem with the motivating example is solved when the `Show` instances for 
pair types and `Term` types are tagged as bidirectional.
(We use a simplified `Show` class for brevity, where `show :: a -> String`
is the only method).
```haskell
instance {-# BIDIRECTIONAL #-} (Show a, Show b) => Show (a,b) where
  show (a,b) = concat ["(", show a, ",", show b, ")"]
```

This gives rise the following type equality axiom in the intermediate language:
```haskell
axiom gShowPair a b : F_Show (a,b) ~ (Dict_Show a, Dict_Show b)
```

The `Show` instance for pairs is elaborated in the following dictionary,
where the `gShowPair` axiom is used to store the instance context in the dictionary:
```haskell
dShowPair :: Dict_Show a -> Dict_Show b -> Dict_Show (a,b)
dShowPair da db = Dict_Show
  { show = \(a,b) -> concat ["(", show da a, ",", show db b, ")"]
  , ctx  = (da, db) ▷ sym gShowPair
  }
```

The `Show` instance for the `Term` GADT is then elaborated into:
```haskell
dShowTerm :: Dict_Show d -> Dict_Show (Term d)
dShowTerm d = Dict_Show
  { show = \t -> case t of 
      Con x   -> show d x
      Tup x y -> let (da, db) = ctx d ▷ gShowPair
                 in concat ["(", show da x, ",", show db y, ")"]
  , ctx = undefined -- only defined if the Show instance for Term is made bidirectional
  }
```

### Example: Functional Dependencies and Associated Types
Bidirectionality also raises the expressive power of functional dependencies.
Consider this definition of length-indexed vectors:
```haskell
data Nat :: * where
  Z :: Nat
  S :: Nat -> Nat

data Vec :: Nat -> * -> * where
  VN :: Vec Z a
  VC :: a -> Vec n a -> Vec (S n) a
```

In order to define an `append` function for this vector data type, we have to
define addition at the type level. This can be encoded with a multi-parameter
type class and a functional dependency (a.k.a. playing "type-level Prolog").
```haskell
class Add (n :: Nat) (m :: Nat) (k :: Nat) | n m -> k
instance              Add Z     m m
instance Add n m k => Add (S n) m (S k)

append :: Add n m k => Vec n a -> Vec m a -> Vec k a
append VN        ys = ys
append (VC x xs) ys = VC x (append xs ys)
```
However, this also fails to compile without bidirectional instances, because
the second clause of the `append` definition requires the constraint
`Add n' m k'`, while the type signature provides `Add (S n') m (S k')`.

This is easily resolved with bidirectionality; the only change needed is to
add bidirectionality to the "successor" instance of `Add`:
```haskell
instance {-# BIDIRECTIONAL #-} Add n m k => Add (S n) m (S k)
```

`append` then type checks, and it is elaborated (approximately) into:
```haskell
axiom gAddSucc n m k : F_Add (S n) m (S k) ~ Dict_Add n m k

append :: Dict_Add n m k -> Vec n a -> Vec m a -> Vec k a
append _ VN        ys = ys
append d (VC x xs) ys = VC x (append (ctx d ▷ gAddSucc) xs ys)
```

Although there exist alternative ways of expressing type-level addition
of natural numbers, we believe it is nevertheless desirable that
functional dependencies can be used to their full potential.

## Effect and Interactions

### Effect
For a more technically detailed account of all the effects of the proposal, we
refer to the paper.

### Interactions.
An instance tagged as bidirectional must not overlap with any other instances:
in the paper we show that overlapping bidirectional instances produce unsound
code.
Therefore, this situation should be rejected by the compiler.

Our proposal to have bidirectionality declared on a per-instance basis is
motivated by the fact that we wish to avoid unexpected breaking changes in
client modules. Tagging an instance as bidirectional should be considered a
breaking interface change.

## Costs and Drawbacks

### Runtime Performance
The possibility of bidirectional instances occurring somewhere means that
all type class dictionary types need to be extended with the additional context
field. This means dictionaries will be lugging around this pointer,
also where it is not actually needed.
It will in all likelihood not be possible to opt-out of this overhead.
The performance impact should be measured.

### Learnability
The proposal should make the language easier to learn (in particular if many
standard library instances were made bidirectional) by avoiding unpleasant
surprises such as the one given in the motivation section. However, in the
case where the user is working with overlapping instances, they might hit upon
unexpected roadblocks when interacting with code that uses bidirectional
instances.

### Development and Maintenance Costs
It was an explicit design goal to re-use existing infrastructure
as much as possible, with all proposed changes being fairly straightforward
extensions of existing algorithms.
Implementation in a
[prototype compiler](https://github.com/KoenP/bidirectional-instances)
was straightforward and quick.
Further discussion on developments and maintenance costs is welcome.

## Alternatives

Workarounds for the motivating example exist; we list a few here.
We believe all of them are unsatisfactory.

### Constrain the GADT Constructors
This is probably the most straightforward workaround. We simply add the type class
constraints that we fail to deduce from the given constraints to the constructors.
```haskell
data Term a where
  Con :: b -> Term b
  Tup :: (Show b, Show c) => Term b -> Term c -> Term (b,c)
```
It is obvious why this approach is not satisfactory: now we can _only_ make compound
`Terms` from types which have an instance for `Show`.
Furthermore, the approach does not scale: one needs to redefine the data type for
every set of constraints one ends up needing.

With `ConstraintKinds` these issues can be partially alleviated:
```haskell
data Term (q :: * -> Constraint) a where
  Con :: b -> Term q b
  Tup :: (q b, q c) => Term q b -> Term q c -> Term q (b,c)

instance Show d => Show (Term Show d) where
  show (Con x)   = show x
  show (Tup x y) = concat ["(", show x, ",", show y, ")"]
```
Nevertheless, this particular encoding still has its drawbacks;
for instance, it is not free to convert between values of, say, the types
`Term Show Int` and `Term Eq Int`, because they do not have the same
runtime representation. Also, working with multiple constraints seems
quite cumbersome.

### Use Overlapping Instances
The following workaround can be constructed with overlapping instances:
```haskell
{-# language GADTs #-}
{-# language OverlappingInstances #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}

data Term a where
  Con :: b -> Term b
  Tup :: Term b -> Term c -> Term (b,c)

instance (Show (Term d), Show (Term e), Show (d,e)) => Show (Term (d,e)) where
  show (Con x) = show x
  show (Tup x y) = concat ["(", show x, ",", show y, ")"]

instance {-# OVERLAPS #-} Show d => Show (Term d) where
  show (Con x) = show x
  show (Tup x y) = undefined -- we rely on "most specific instance first" rule to avoid this case
```
The drawbacks of this approach are
1. It is quite boilerplate-heavy, and the amount of boilerplate grows with the number of constructors that need bidirectionality.
1. It is not obvious to the average programmer.
1. `Show a` no longer implies `Show (Term a)` in general, the user must explicitly give `Show (Term a)` as a constraint which can be surprising and inelegant. For instance 
```haskell
g :: Show (Term a) => Term a -> String
g x = show x
```
needs the `Show (Term a)` constraint to type check, just providing `Show a` doesn't work.

### Using Constraint Kinds
The following workaround was e-mailed to us by Guillaume Allais.
```haskell
{-# LANGUAGE GADTs, TypeFamilies, UndecidableInstances #-}

import Data.Constraint

data Term a where
  Con :: a -> Term a
  Tup :: Term a -> Term b -> Term (a, b)

type family Shows (a :: *) :: Constraint where
  Shows (a, b) = (Show a, Show b, Shows a, Shows b)
  Shows a      = ()

instance (Show a, Shows a) => Show (Term a) where
  show x = case x of
    Con a   -> show a
    Tup a b -> show a ++ show b
```
Although this workaround works,
it is decidedly nontrivial, and some boilerplate is still required,
which needs to be written for every instance.

### A Workaround for the Append Example
This workaround was suggested by [Oleg Grenrus](https://github.com/phadej).
```haskell
{-# LANGUAGE GADTs, FunctionalDependencies #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

data Nat :: * where
  Z :: Nat
  S :: Nat -> Nat
data Vec :: Nat -> * -> * where
  VN :: Vec Z a
  VC :: a -> Vec n a -> Vec (S n) a

class Add (n :: Nat) (m :: Nat) (k :: Nat) | n m -> k where
instance              Add Z     m m -- one could also have m ~ k => Add Z m k instance, which behaves better
instance Add n m k => Add (S n) m (S k)

class Add n m k => Append (n :: Nat) (m :: Nat) (k :: Nat) | n m -> k where
    append :: Vec n a -> Vec m a -> Vec k a

instance Append 'Z m m where
    append VN        ys = ys

instance Append n m k => Append ('S n) m ('S k) where
    append (VC x xs) ys = VC x (append xs ys)
```

## Unresolved Questions

As mentioned in the "Costs and Drawbacks" section, runtime performance might be
adversely affected due to the addition of a field to each dictionary record.
Until an implementation is written and measured, it is hard to estimate whether
this impact will be significant. Nevertheless, any ideas for how to avoid this
additional field where it is not needed are welcome.

## Implementation Plan

The author (Koen Pauwels) will spend about one workday per week working on the
implementation until it is done.
