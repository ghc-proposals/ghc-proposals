---
author: John Ericson and Cale Gibbard on behalf of Obsidian Systems
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/0>).
**After creating the pull request, edit this file again, update the number in
the link, and delete this bold sentence.**

# Defaults and backpack/hs-boot

Correct/clarify the meaning of instance declarations in hs-boot files and backpack signatures in the situations where an instance is to be required, but the corresponding class has a default type family instance.

## Motivation

Presently, if one creates a class with an associated type that has a default, such as:

```haskell
-- in A.hs
module A where
class C a where
  type T a
  type T a = Int
  f :: a -> Int
  f _ = 0
```

and then in an `hs-boot` file would like to demand an instance of that class for some type:

```haskell
-- in B.hs-boot
module B where
import A
data X
instance C X
```

This results in a compiler panic, because GHC "can't handle family instances in boot files".
Evidently, we've snuck a type family instance into what was meant to be an interface.

This problem also occurs for backpack signatures, which are rather similar in many ways to hs-boot files.

In both cases, trying to explicitly specify anything about the type family instance in an hs-boot file or backpack signature is currently an error (a more explicit one rather than a compiler panic).

At the term level, the default is ignored, because `hsig`/`hs-boot` are not allowed to contain methods period:

```haskell
-- in B.hs-boot
module B where
import A
data X
instance C X
   -- T is unconstrained
```

```haskell
-- in B.hs-boot
module B where
import A
data X
instance C X
  f _ = 7 -- error
```

This a blatantly inconsistent, with 3 ways to make it consistent:

 1. Never allow (term or type level) items in instances, nor infer them from class defaults, in `hsig`/`hs-boot` files

 2. Do allow items in instances, but apply defaults in `hsig`/`hs-boot` files
 
 3. Do allow items in instances, but ignore defaults in `hsig`/`hs-boot` files

(1) seems needlessly restrictive, there are tons of valid reasons why one would want to constrain an associated type in a `hsig`/`hs-boot` file, just as they would constrain it in the type of an item in a module.

(2) is awkward because it makes the default mandatory, without some extra syntax to opt *out* of the default. 
This seems backwards.
The default provided by the class probably is not something that a programmer intends to make part of the interface when they omit any mention of the type family from the instance---the programmer should be opting *in*.

(3) seems the best option; the user could always repeat the default definition in the method if they would like to constrain.

Now implementing instance item definitions in `hsig`/`hs-boot` files is a decent chunk of work which is why it hasn't been done already.
But before that, we can at least fix type level defaults to match term level defaults and not attempt to turn them into currently-disallowed/broken instance definitions, in other words starting with (1) and then going to (3) is not a breaking change. 

## Proposed Change Specification

In backpack signatures and hs-boot files, any instance which doesn't mention an item in the instantiated class places no constraint on that item, regardless of whether the class has a default for that item.

## Examples

Given the `module A` from the motivation:

This is valid:

```haskell
-- in B.hs-boot
module B where
import A
data X
instance C X
  -- T is unconstrained
  -- f is unconstrained
```

This is not:

```haskell
-- in B.hs-boot
module B where
import A
data X
instance C X
  type T x = [Bool] -- error for now, not yet implemented
  f _ = 7 -- error for now, not yet implemented
```

## Effect and Interactions

The proposed change addresses the issue raised in the motivation section directly. I don't immediately foresee any negative interactions with other language features.

## Costs and Drawbacks

If you do want the default you have to repeat it. This is currently unsupported anyway at the type level, but we may wish to allow specifications of type family instances in `hs-boot` and `hsig` files in the future.

Defaults in regular code allow what would have been a breaking change to be a non-breaking change.
They won't accomplish that for signatures and hs-boot files.
However, the complicated variances surrounding backpack and classes (sigs and classes both being in negative position) makes this tricky to think about and of debatable value.

There are better, more intentional ways to "upcast" libraries to an interface that the downstream library expects. 
We should explore extra-linguistic techniques for API evolution, rather than relying on cruder in-language-like defaulting for this, side-stepping this drawback.

## Alternatives

There's an option to eventually support specification of type family instances in backpack signatures and hs-boot files, where one would have the option of then regarding the default from the class as a specification which must be obeyed when an empty instance is given in the signature. That would mean that any non-default instance would be forced to be provided by the specification, and seems unhelpful at best.

## Unresolved Questions

On one hand, there are no term level equality constraints.
On the other, we can simply require a module define any term-level method that the signature/boot file defines, or check for crude syntactic equality as a sound conservative approximation.
8
## Implementation Plan

John Ericson has already implemented this. [Pull Request #1776](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/1776).

## Endorsements

Obsidian Systems has been working on this on behalf of MIRI.
