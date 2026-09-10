---
author: Thomas Bagrel
date-accepted: ""
ticket-url: "https://github.com/ghc-proposals/ghc-proposals/pull/683"
implemented: ""
---

This proposal is [discussed at pull request #683](https://github.com/ghc-proposals/ghc-proposals/pull/683).

# Primitives for zero-copy compact regions

This proposal introduces new primops in GHC to enable direct data structure building in compact regions (without temporary storage in the regular garbage-collected heap), which is useful for performance as it can greatly reduce GC cost.

## Motivation

[Compact regions](https://github.com/ezyang/compact) have been supported in GHC since 2017; they provide a way to allocate memory in a region that can be garbage-collected all at once instead of being traversed fully by the GC at each collection cycle. Compact regions are useful in some performance-critical use-cases, especially when dealing with large long-lived datasets.

So far however, there is no way to allocate in a compact region directly. One has to first build data structures in the regular GC heap, and then copy them to a compact region.

I propose a new structure building method for compact regions, mainly through the new RTS primitive [`compactAddHollow#`](#allocate-hollow-data-constructor-in-region-rts-primop-compactaddhollow). `compactAddHollow#` allocates a data constructor heap object directly into a compact region, without setting values for its fields yet. That lets us build structures both in an _inside-out_ AND _outside-in_ fashion directly into a region, matching the pace and order in which input is processed, and bypassing the GC heap.

The main intent of `compactAddHollow#` is to be encapsulated in a pure, memory-safe API. Alongside this proposal, I showcase a destination-passing style (DPS) interface ([code](https://github.com/tweag/linear-base/pull/450), [article](https://inria.hal.science/hal-04406360/document)) using the proposed changes, where normal functions can receive _write references_, aka _destinations_, pointing to yet-uninitialized fields of the data structures in the compact region and use them to complete the structures. I ensure safety with [linear types](https://github.com/ghc-proposals/ghc-proposals/pull/111).

A real-world application of the proposal would be a program that parses a large serialized document and keep it in memory during the whole execution. Today such a program can already move the parsed document to a compact region to save on garbage collection time, but the document still has to be parsed into the GC heap first. As illustrated in the [**Effect and Interactions**](#effect-and-interactions) section, by deserializing directly into a compact region, the implementation can get twice as fast, thanks to garbage collection time saved.

Performance results are quite promising, and most of the features required for such example are implemented on the library side, so only a few changes to GHC are needed (1 RTS primop, 1 compile-time primop, 1 built-in type family, and minor technicalities).

### Resources

Further details about the context and goal of the proposed changes can be found in [the talk I gave at HIW 2024](https://www.youtube.com/live/uMurx1a6Zck?si=kNR2Qsj-6hVGdlP2&t=10579) and the related [JFLA 2024 article](https://inria.hal.science/hal-04406360/document).

The aforementioned destination-passing style (DPS) API that leverages `compactAddHollow#` is implemented [here](https://github.com/tweag/linear-base/pull/450). It is part of [`linear-base`](https://github.com/tweag/linear-base) as linear types are a key part of the safety of the interface. More formal considerations about the safety of the API should follow soon.

The prototype GHC branch that implements the changes described in the proposal is available [here](https://gitlab.haskell.org/tweag/ghc/-/tree/tbagrel1/dps-compact-regions-prims?ref_type=heads) ([diff](https://gitlab.haskell.org/tweag/ghc/-/compare/master...tbagrel1%2Fdps-compact-regions-prims?from_project_id=1)).

## Proposed Change Specification

1. [Allocate hollow data constructor in region: RTS primop `compactAddHollow#`](#allocate-hollow-data-constructor-in-region-rts-primop-compactaddhollow)
2. [Retrieving info table pointer of a constructor: primtype `InfoTablePtrOf#` and compile-time primop `reifyInfoTablePtr#`](#retrieving-info-table-pointer-of-a-constructor-primtype-infotableptrof-and-compile-time-primop-reifyinfotableptr)
3. [Obtain data constructor name as a `Symbol`: `LiftedCtorToSymbol` built-in type family](#obtain-data-constructor-name-as-a-symbol-liftedctortosymbol-built-in-type-family)

### Allocate hollow data constructor in region: RTS primop `compactAddHollow#`

The new RTS primitive `compactAddHollow#` allocates space for a new constructor heap object in the specified compact region, set its header with the supplied info table pointer, but doesn't set any of its fields (so the constructor is said to be _hollow_). The user is responsible for setting the fields of the constructor heap object before reading it. The primitive returns the newly allocated constructor heap object, but also the size of the header of the object, and the size of the words on the platform so that the user can later set the fields of the returned object.

```haskell
compactAddHollow#
  :: Compact#         -- ^ the compact region where the constructor heap object will be added
  -> Addr#            -- ^ the info table pointer that will be set as header for the new constructor heap object
  -> State# RealWorld -- ^ state token
  -> (# State# RealWorld
      , a             -- ^ the allocated constructor heap object
      , Word#         -- ^ size of the header of the allocated heap object, in bytes
      , Word#         -- ^ size of the words, in bytes, on the platform
     #)
```

### Retrieving info table pointer of a constructor: primtype `InfoTablePtrOf#` and compile-time primop `reifyInfoTablePtr#`

New compile-time primitive `reifyInfoTablePtr#` reifies the info table pointer of a given data constructor into a static value at compile-time.

```haskell
type InfoTablePtrOf# :: forall k. k -> TYPE AddrRep

reifyInfoTablePtr# :: forall k (a :: k). (# #) -> InfoTablePtrOf# a
```

New primtype `InfoTablePtrOf#` shares the same representation as `Addr#` and serves both as a sort of input at type level to select the data constructor that `reifyInfoTablePtr#` should operate on, and as an output at value level to carry the resulting info table pointer.

We pass the desired data constructor as a type to `reifyInfoTablePtr#`, using type ascription for the return value:

```haskell
let leftInfoPtr :: Addr# = unsafeCoerceAddr (reifyInfoTablePtr# (# #) :: InfoTablePtrOf# 'Left)
```

`reifyInfoTablePtr#` inspects the type of its return value:

- if it is of shape `InfoTablePtrOf# 〈s〉`, where `〈s〉` is a `Symbol`, it compiles down to the info table pointer for the Stg object `stg_〈s〉`
- if it is of shape `InfoTablePtrOf# '〈d〉`, where `〈d〉` is a data constructor, it compiles down to the info table pointer for data constructor `〈d〉`
- otherwise, it raises a compile-time error (I've yet to find how to do that in practice)

### Obtain data constructor name as a `Symbol`: `LiftedCtorToSymbol` built-in type family

New built-in type family `LiftedCtorToSymbol` translates a data constructor lifted as a type into the `Symbol` corresponding to its _occurence name_.

```haskell
type family LiftedCtorToSymbol (liftedCtor :: k) :: Symbol
```

For example:

```haskell
ghci> :set -XDataKinds
ghci> import GHC.TypeLits
ghci> import qualified Data.Either
ghci> :kind! LiftedCtorToSymbol 'Data.Either.Left
LiftedCtorToSymbol 'Data.Either.Left :: Symbol
= "Left"
```

## Proposed Library Change Specification

- `compactAddHollow#`, `reifyInfoTablePtr#`, and `InfoTablePtrOf#` are exposed in `GHC.Prim`
- `LiftedCtorToSymbol` is exposed in `GHC.TypeLits`

## Examples

### Primitive usage

#### Allocate a hollow `Left` constructor in region `compactRegion` and complete it with a `()` constructor

```haskell
example :: Compact# -> State# RealWorld -> (# State# RealWorld, Either () () #)
example compactRegion s0 =
  let leftInfoPtr :: Addr#
      leftInfoPtr = unsafeCoerceAddr (reifyInfoTablePtr# (# #) :: InfoTablePtrOf# 'Left)
      unitInfoPtr :: Addr#
      unitInfoPtr = unsafeCoerceAddr (reifyInfoTablePtr# (# #) :: InfoTablePtrOf# '())
   in case compactAddHollow# compactRegion leftInfoPtr s0 of
        (# s1, leftCtor, headerSize, wordSize #) -> case anyToAddr# leftCtor s1 of
          (# s2, pLeftCtor #) -> case compactAddHollow# compactRegion unitInfoPtr s2 of
            (# s3, unitCtor, headerSize, wordSize #) -> case anyToAddr# unitCtor s3 of
              (# s4, pUnitCtor #) -> case writeAddrOffAddr# pLeftCtor (word2Int# headerSize) pUnitCtor s4 of
                s5 -> (# s5, leftCtor #)
```

First we start by reifying the info table pointer for the two constructors we'll need, `Left` and `()`.

Then we allocate a hollow `Left` constructor in the compact region, and get a pointer to it. We do the same for a `()` constructor.

Finally we set the (only) field of the `Left` constructor, located at offset `headerSize` from base address of that constructor object, to point to the `()` constructor, using existing primop `writeAddrOffAddr#`.

The returned object of type `Either () Int` can be read safely as it no longer has any unspecified field.

#### Allocate hollow `stg_IND` object in region `compactRegion`

```haskell
let indInfoPtr = unsafeCoerceAddr (reifyInfoTablePtr# (# #) :: InfoTablePtrOf# "IND")
 in compactAddHollow# compactRegion indInfoPtr state
```

As in the previous example, we reify the info table pointer for the `stg_IND` object, and then allocate a hollow `stg_IND` object in the compact region. Same can be done with other special STG objects.

#### Get info about fields of a data constructor using `GHC.Generics` and `LiftedCtorToSymbol`

Here I present how to extract information about a data constructor (type of its fields) in a type-level structure, starting from its type-level (lifted) representation and the type `a` that it belongs to.

Such information about a constructor is derivable from the `GHC.Generics` representation of its belonging type, given we know the name of the constructor as a type-level `Symbol`. So the information is _almost there_, and the new type family `LiftedCtorToSymbol` is just here to bridge that gap and allow reusing existing `Generics` machinery at little cost instead of adding a more complex built-in type family.

We first use `LiftedCtorToSymbol` to get the name of the specified data constructor as a `Symbol`, and then uses it to filter the generic representation `Rep a ()` via type families implemented in user land.

We assume existence of type families `(++)`, `(<|>)` and `FromJust :: Maybe k -> k`, with the same behavior as their value-level homonyms.

```haskell
-- filters out Generic representation of `a` to find the types of fields of the data constructor whose type-lifted representation is `lCtor`
type family FTypesFromLiftedCtor lCtor (a :: Type) :: [Type] where
  FTypesFromLiftedCtor lCtor a =
    FromJust (FTypesFromSymCtor (LiftedCtorToSymbol lCtor) (Rep a ()))
--                                        ^
--                                        | Where we use the new type family

-- ****************************************************************************
-- Helper type families
-- ****************************************************************************

-- try to find a data constructor `C1` with name `symCtor` in `repA`, and return its field's types
type family FTypesFromSymCtor (symCtor :: Symbol) (repA :: Type) :: Maybe [Type] where
  FTypesFromSymCtor symCtor (C1 ('MetaCons symCtor x y) f p) = 'Just (FTypesFromAllSels (f p))
  FTypesFromSymCtor symCtor (C1 ('MetaCons _ _ _) _ _) = 'Nothing
  FTypesFromSymCtor symCtor ((f :+: g) p) = FTypesFromSymCtor symCtor (f p) <|> FTypesFromSymCtor symCtor (g p)
  FTypesFromSymCtor symCtor (V1 _) = 'Nothing
  FTypesFromSymCtor symCtor (M1 _ _ f p) = FTypesFromSymCtor symCtor (f p)
  FTypesFromSymCtor _ _ = TypeError ('Text "No match for FTypesFromSymCtor")

-- find every record selector `S1` in `repA`, and collect their types
type family FTypesFromAllSels (repA :: Type) :: [Type] where
  FTypesFromAllSels (S1 meta f p) = '[ FTypeFrom (f p)]
  FTypesFromAllSels (U1 _) = '[]
  FTypesFromAllSels ((f :*: g) p) = FTypesFromAllSels (f p) ++ FTypesFromAllSels (g p)
  FTypesFromAllSels (M1 _ _ f p) = FTypesFromAllSels (f p)
  FTypesFromAllSels _ = TypeError ('Text "No match for FTypesFromAllSels")

-- expect to find a type-level constant `K1` in `repA`, and extract its payload Type
type family FTypeFrom (repA :: Type) :: Type where
  FTypeFrom (K1 _ c _) = c
  FTypeFrom (M1 _ _ f p) = FTypeFrom (f p)
  FTypeFrom _ = TypeError ('Text "No match for FTypeFrom")
```

Usage:

```haskell
ghci> :set -XDataKinds
ghci> :kind! FTypesFromLiftedCtor 'Left (Either a b)
= '[ a]
ghci> :kind! FTypesFromLiftedCtor '(,) (a, b)
= '[ a, b]
```

### Library side: what the safe interface looks like

In this proposal I've tried to limit the changes to GHC to a minimum, and make most of the work on the library side. Here's a snapshot of what safe, direct structure building in compact region could look like (using [a destination-passing style API](https://github.com/tweag/linear-base/pull/450)).

I define a custom `IdCard` type, and then build an instance of it directly in a compact region, using destinations (variables `d_`) to set the fields of the data structure little by little:

```haskell
data IdCard = MkIdCard {name :: String, address :: Address}
  deriving (Eq, Generic, Show)
data Address = MkAddress {no :: Int, street :: String, city :: String, zipCode :: Int}
  deriving (Eq, Generic, Show)

testBuildDirectlyInRegion = do
  let actual :: IdCard
      !actual = alloc' (\d ->
        case fill @'MkIdCard d of
          (dN, dA) ->
            fillLeaf "John Doe" dN
              `lseq` (case fill @'MkAddress dA of
                        (dNo, dSt, dC, dZ) ->
                          fillLeaf 42 dNo
                            `lseq` fillLeaf "Elm Street" dSt
                            `lseq` fillLeaf "Springwood" dC
                            `lseq` fillLeaf 12345 dZ
                      )
          )
      expected :: IdCard
      expected = MkIdCard "John Doe" (MkAddress 42 "Elm Street" "Springwood" 12345)
  assertEqual "John Doe ID is the same in both building schemes" expected actual
```

The syntax looks quite verbose compared to naive structure building, because the API is designed to allow efficient structure building, little-by-little, from dynamic data, instead of building the whole structure at once from static data (where it is no more efficient than a copy using existing `compactAdd#`).

The API uses linear types to ensure (at compile-time) that every field of the data structure is wrote to exactly once before the structure can be read.

## Effect and Interactions

The `compactAddHollow#` primitive allows for direct allocation in a compact region, without copy, as stated in the [Motivation](#motivation) section. It needs to be combined with `reifyInfoTablePtr#` to properly allocate requested constructor objects with the right info table pointer, because as a RTS primitive, `compactAddHollow#` doesn't have access itself to the info table pointer to set for the constructor (all information about info tables has long been inlined/erased when the primitive is actually executed).

As the fields of the allocated constructor object aren't set after calling `compactAddHollow#`, they need to be set later by the user, so it is crucial to have a way to know the types of the constructor's fields if we want to design a type-safe API around this new structure building method. The new type family `LiftedCtorToSymbol` is introduced to take advantage of existing `GHC.Generics` reflection capabilities and fulfil this need at minimum cost, as demonstrated in the [previous example](#get-info-about-fields-of-a-data-constructor-using-ghcgenerics-and-liftedctortosymbol).

With a [safe user-facing API](https://github.com/tweag/linear-base/pull/450), as mentioned in the [Motivation](#motivation), the described changes can be used to implement more efficient deserializers for long-lived documents. For instance, [I implemented an S-expression parser]((https://github.com/tweag/linear-base/blob/93dd7769d86a4c918934be110151faf382a35f38/examples-version-changes/ghc-dps-compact/after/Compact/SExpr.hs)) in two different fashions: the first one `parseWithoutDest` deserializes into the usual GC heap, and the second `parseWithDest` uses this proposal's primitives to directly deserialize data into a compact region. Here's the benchmark for a 4MB S-expression file:

```text
parseWithoutDest (parse into GC heap, stay in GC heap ***): OK
  358  ms ± 6.8 ms, 196 MB allocated, 402 MB copied, 441 MB peak memory

parseWithoutDest (parse into GC heap, then copy into compact region): OK
  332  ms ± 4.4 ms, 327 MB allocated, 362 MB copied, 377 MB peak memory

parseWithDest (parse directly into compact region): OK
  136  ms ± 5.4 ms, 283 MB allocated,  37 MB copied, 269 MB peak memory
```

(`***`_The  program only parses the document and then shutdowns, so in a real-world use case doing something non trivial with the parsed data, the time spent by the GC traversing the parsed dataset again and again in `parseWithoutDest (parse into GC heap, stay in GC heap` would be significantly more important._)

The code for the parser is very similar and equally verbose for both the naive and proposal-powered version, so there isn't much extra effort needed to get these performance benefits.

### Interactions

I don't think the proposed changes will have any specific interaction with other GHC features. The implementation of the new primitives makes very little assumptions on the existing internals of GHC.

`compactAddHollow#` soundness relies on the fact that the GC doesn't follow pointers inside a compact region, so it is theoretically not unsound to let parts of a data structure be uninitialized for some time as long as the user doesn't read them.

The library code makes many assumptions on the representation of Haskell heap objects, but it isn't in the scope of this proposal.

## Costs and Drawbacks

The changes in this proposal should have a very minimal impact on GHC maintenance, and are quite localized.

Defining `reifyInfoTablePtr#` requires support for kind-polymorphic type variables in primop generation. I include the following changes in the proposal, following what has been done for levity-polymorphic type variables:

```haskell
kind1TyVar, kind2TyVar :: TyVar
(kind1TyVar : kind2TyVar : _)
  = mkTemplateKindVars (repeat liftedTypeKind)

kind1TyVarInf, kind2TyVarInf :: TyVarBinder
kind1TyVarInf = mkTyVarBinder Inferred kind1TyVar
kind2TyVarInf = mkTyVarBinder Inferred kind2TyVar

kind1Ty, kind2Ty :: Type
kind1Ty = mkTyVarTy kind1TyVar
kind2Ty = mkTyVarTy kind2TyVar

kindPolyAlphaTyVar, kindPolyBetaTyVar :: TyVar
[kindPolyAlphaTyVar, kindPolyBetaTyVar] =
  mkTemplateTyVars
    [ kind1Ty
    , kind2Ty]

kindPolyAlphaTyVarSpec, kindPolyBetaTyVarSpec :: TyVarBinder
kindPolyAlphaTyVarSpec = mkTyVarBinder Specified kindPolyAlphaTyVar
kindPolyBetaTyVarSpec  = mkTyVarBinder Specified kindPolyBetaTyVar

kindPolyAlphaTy, kindPolyBetaTy :: Type
kindPolyAlphaTy = mkTyVarTy kindPolyAlphaTyVar
kindPolyBetaTy  = mkTyVarTy kindPolyBetaTyVar
```

## Backward Compatibility

0. No breakage should be expected for existing code.

## Alternatives

1. [More intuitive signature for `reifyInfoTablePtr#` primitive](#more-intuitive-signature-for-reifyinfotableptr-primitive)
2. [Replace `LiftedCtorToSymbol` with a type family that directly returns the type-level list of fields of the constructor](#replace-liftedctortosymbol-with-a-type-family-that-directly-returns-the-type-level-list-of-fields-of-the-constructor)
3. [Remove `LiftedCtorToSymbol` built-in typeclass entirely](#remove-liftedctortosymbol-built-in-typeclass-entirely)
4. [Integrate the library code for safe DPS structure building into GHC](#integrate-the-library-code-for-safe-dps-structure-building-into-ghc)
5. [Add nothing to GHC, and relie on unsafe user-side tricks to achieve roughly the same behavior](#add-nothing-to-ghc-and-relie-on-unsafe-user-side-tricks-to-achieve-roughly-the-same-behavior)
6. [Access header size and word size of a heap object through new dedicated primops instead of returning them from `compactAddHollow#`](#access-header-size-and-word-size-of-a-heap-object-through-new-dedicated-primops-instead-of-returning-them-from-compactaddhollow)

### More intuitive signature for `reifyInfoTablePtr#` primitive

`reifyInfoTablePtr#` has a rather counterintuitive signature:

```haskell
reifyInfoTablePtr# :: forall k (a :: k). (# #) -> InfoTablePtrOf# a
```

This signature is motivated by the fact that in `emitPrimOp` in `compiler/GHC/StgToCmm/Prim.hs`, we only have access to the type of the return value of the primop, and not to the types of the arguments. So in the current design, I feed the type-level information of the chosen constructor as a (phantom) type argument on the return type of the primop.

It would be clearer to have signature like:

```haskell
reifyInfoTablePtrAlt# :: forall k (a :: k). Proxy# a -> Addr#
```

but that would require more important restructuring of the primop code generation pipeline, and I don't think it's worth it.

### Replace `LiftedCtorToSymbol` with a type family that directly returns the type-level list of fields of the constructor

`LiftedCtorToSymbol` is only used as a way to extract information about a constructor's fields from the `GHC.Generics` representation of the associated type. We could devise a typeclass that does all of that directly into the compiler, without requiring `GHC.Generics`.

I expect that it would require to sort of reimplement part of `GHC.Generics` somewhere else in the compiler. I haven't looked into that yet.

### Remove `LiftedCtorToSymbol` built-in typeclass entirely

With the proposed changes, the library code can have syntax like

```haskell
fill @'MkIdCard d
```

and still recover information about `MkIdCard` from the `GHC.Generics` representation of `IdCard`.

Without the new built-in typeclass, the syntax would become:

```haskell
fill @'MkIdCard @"MkIdCard" d
```

It would make the user interface way less ergonomic to use, as the user would have to specify the constructor name twice. It would also raise the question of what to do when the specified symbol doesn't match the constructor's name.

I think it's better to keep `LiftedCtorToSymbol` as a new built-in typeclass, or improve it as suggested in the previous section, but we can theoretically still achieve the fulfill our stated goal without it (only the interface syntax would be impacted).

### Integrate the library code for safe DPS structure building into GHC

We can imagine moving [part or whole of the library code for DPS structure building](https://github.com/tweag/linear-base/pull/450) into GHC itself. That would allow for a more ergonomic and complete interface for the user, instead of adding a few smaller changes that are not particularly usable alone. DPS building in compact regions could then be a first-class feature of GHC.

However, it would have a fairly large development and maintenance cost. Also the library is still fairly experimental, it needs improvements both on the interface side and on the implementation side. Stabilizing it now would be premature in my opinion. I think it's better to integrate the changes from this proposal into GHC now, so that the library can be used, tested and improved by the community, and maybe later integrate the library code into GHC.

### Add nothing to GHC, and relie on unsafe user-side tricks to achieve roughly the same behavior

I first experimented with the idea of "copy-less" allocation inside compact regions using purely user-land code.

Because I couldn't allocate hollow constructors directly into the region, I would use `GHC.Generics` to allocate a chosen constructor in the normal GC heap, with fields set to a unsafe-casted `()` value. Then I would copy the _almost hollow_ constructor to the compact region using existing primops, and use `writeAddrOffAddr#` to overwrite the fields of the constructor in the compact region later.

I don't think relying on these tricks is a good idea though if we want to officially support direct structure building in compact regions. First, the copy is still less efficient than allocation + setting the header as in `compactAddHollow#`. Secondly, it was much more difficult to make the code generic for all data types; I had to write specific versions of `fill` functions for each constructor I wanted to support. Finally, the code was much less safe, as I had to rely on unsafe coercions and unsafe casts to make it work.

### Access header size and word size of a heap object through new dedicated primops instead of returning them from `compactAddHollow#`

Once a constructor is allocated in a compact region using `compactAddHollow#`, the user needs to set the fields of the constructor.

For that it can use existing primops like `writeAddrOffAddr#`, that require the address the field to set. We can retrieve the base address of the constructor object using `anyToAddr#`, but we need to know the offset of the field to set. For constructors with only pointer fields, the offset is `headerSize + wordSize * i`, where `i` is the index of the field in the constructor.

In the proposed design, `compactAddHollow#` primitive returns the size of the header of the allocated object, and the size of the words on the platform at each call. Instead, we could have new, dedicated primops to get these values, and have a simpler return type for `compactAddHollow#`.

## Unresolved Questions

1. I want `reifyInfoTablePtr#` to raise a compile-time error when the type argument it inspects doesn't have the right shape. I'm not sure how to do that yet.
2. How should `LiftedCtorToSymbol` behave when it is given a type that doesn't correspond to a data constructor lifted as a type.
3. On the safe API side, I do not plan to support the direct allocation in a region of constructors with `DecidedUnpack` fields. It's much easier for such constructors to be copied from the GC heap using existing `compactAdd#` that knows how to build them safely, than trying to imitate the same logic in userland with pointer offsets. I don't know if that deserve an explicit mention in other sections, given that such constructors can still be allocated with `compactAddHollow#` with no trouble. Just I won't provide a fancy, safe way of setting their fields.

## Implementation Plan

I have already implemented the changes in [a branch of GHC](https://gitlab.haskell.org/tweag/ghc/-/tree/tbagrel1/dps-compact-regions-prims?ref_type=heads) ([diff](https://gitlab.haskell.org/tweag/ghc/-/compare/master...tbagrel1%2Fdps-compact-regions-prims?from_project_id=1)). However I'm not sure about the quality of the code, as it would be my first contribution to GHC. I took inspiration from existing functions in the codebase to design mines, but my understanding for some part is quite shallow.

I want to state that the new primitives are very simple to implement, as they are mostly reusing existing GHC logic and exposing it.

## Endorsements

None.
