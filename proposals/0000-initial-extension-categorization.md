---
author: Trevis Elser
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/669).

# Extension Categorization

We propose to concretely categorize a subset of the Haskell language extensions as defined by a previously accepted [proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0601-extension-lifecycle-framework.md).

We propose a categorization scheme for Haskell language extensions. This scheme is simple, in that there are few categories that are described in terms of the user-relevant aspects.  The purpose of this proposal is not to classify all extensions - rather, we seek to establish a basis upon which to carry out that work subsequently. Example classifications are provided to illustrate the proposed framework, and acceptance of this proposal should not count as acceptance of these examples.

## 1. Motivation

With the acceptance of [proposal 601](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0601-extension-lifecycle-framework.md), extensions must be categorized. Here we propose to categorize a selection of the currently supported extensions. The purpose for a selection is simple, there are simply too many to host in a single discussion. With that, we intend to focus on those extensions with the most broad support so that significant progress can be made towards the categorization effort quickly while leaving more in-depth conversations for a later time.

## 2. Proposed Change Specification

The entirity of the changes proposed are categorization of extensions. We provide the categorization of several extensions and where applicable the categorization of the negation of the extension.

In the tables below we include for each extension the categorization and the "negative categorization" which is for the `No` prefixed version. This is important because while most extensions are themselves the item to be concerned with, there are cases such as `FieldSelectors` where the negative form, `NoFieldSelectors` is the "new" thing added by having the extension.

### 2.1 Extensions Not Impacting Stability Categorization

These are extensions which both the usual statement and the `No` prefixed version have the same categorization. Which is to say that the flag does not have any impact on stability, it is for the purpose of categorization, the same to enable or disable the extension.

#### 2.1.1 Stable Extensions
First we provide a set of extensions where the categorization is `Stable` for both the extension itself and the inversion. It is expected that these extensions are among the easiest to document because of the lack of differentiation between the on and off states

The following table contains the language extension, the proposed categorization for the extension being turned on, and the proposed categorization for the extension being turned off.

| Extension                  | Categorization | Negative Categorization |
|----------------------------|----------------|-------------------------|
| BangPatterns               | Stable         | Stable                  |
| BinaryLiterals             | Stable         | Stable                  |
| BlockArguments             | Stable         | Stable                  |
| CApiFFI                    | Stable         | Stable                  |
| ConstrainedClassMethods    | Stable         | Stable                  |
| ConstraintKinds            | Stable         | Stable                  |
| CPP                        | Stable         | Stable                  |
| DataKinds                  | Stable         | Stable                  |
| DefaultSignatures          | Stable         | Stable                  |
| DeriveAnyClass             | Stable         | Stable                  |
| DeriveDataTypeable         | Stable         | Stable                  |
| DeriveFoldable             | Stable         | Stable                  |
| DeriveFunctor              | Stable         | Stable                  |
| DeriveGeneric              | Stable         | Stable                  |
| DeriveLift                 | Stable         | Stable                  |
| DeriveTraversable          | Stable         | Stable                  |
| DerivingStrategies         | Stable         | Stable                  |
| DerivingVia                | Stable         | Stable                  |
| DisambiguateRecordFields   | Stable         | Stable                  |
| EmptyCase                  | Stable         | Stable                  |
| EmptyDataDecls             | Stable         | Stable                  |
| EmptyDataDeriving          | Stable         | Stable                  |
| ExistentialQuantification  | Stable         | Stable                  |
| ExplicitForAll             | Stable         | Stable                  |
| ExtendedDefaultRules       | Stable         | Stable                  |
| FlexibleContexts           | Stable         | Stable                  |
| FlexibleInstances          | Stable         | Stable                  |
| ForeignFunctionInterface   | Stable         | Stable                  |
| GADTs                      | Stable         | Stable                  |
| GADTSyntax                 | Stable         | Stable                  |
| GeneralizedNewtypeDeriving | Stable         | Stable                  |
| HexFloatLiterals           | Stable         | Stable                  |
| ImplicitParams             | Stable         | Stable                  |
| ImplicitPrelude            | Stable         | Stable                  |
| ImportQualifiedPost        | Stable         | Stable                  |
| InstanceSigs               | Stable         | Stable                  |
| InterruptibleFFI           | Stable         | Stable                  |
| KindSignatures             | Stable         | Stable                  |
| LambdaCase                 | Stable         | Stable                  |
| LiberalTypeSynonyms        | Stable         | Stable                  |
| MagicHash                  | Stable         | Stable                  |
| MonadComprehensions        | Stable         | Stable                  |
| MonomorphismRestriction    | Stable         | Stable                  |
| MultiParamTypeClasses      | Stable         | Stable                  |
| MultiWayIf                 | Stable         | Stable                  |
| NamedFieldPuns             | Stable         | Stable                  |
| NamedWildCards             | Stable         | Stable                  |
| NumDecimals                | Stable         | Stable                  |
| NumericUnderscores         | Stable         | Stable                  |
| OverloadedLabels           | Stable         | Stable                  |
| OverloadedLists            | Stable         | Stable                  |
| ParallelListComp           | Stable         | Stable                  |
| PatternGuards              | Stable         | Stable                  |
| PatternSynonyms            | Stable         | Stable                  |
| PolyKinds                  | Stable         | Stable                  |
| PostfixOperators           | Stable         | Stable                  |
| QualifiedDo                | Stable         | Stable                  |
| RankNTypes                 | Stable         | Stable                  |
| RecordWildcards            | Stable         | Stable                  |
| RecursiveDo                | Stable         | Stable                  |
| RoleAnnotations            | Stable         | Stable                  |
| StandaloneDeriving         | Stable         | Stable                  |
| StandaloneKindSignatures   | Stable         | Stable                  |
| StrictData                 | Stable         | Stable                  |
| TraditionalRecordSyntax    | Stable         | Stable                  |
| TypeApplications           | Stable         | Stable                  |
| TypeOperators              | Stable         | Stable                  |
| TypeSynonymInstances       | Stable         | Stable                  |
| UnboxedSums                | Stable         | Stable                  |
| UnboxedTuples              | Stable         | Stable                  |
| UnicodeSyntax              | Stable         | Stable                  |
| UnliftedNewtypes           | Stable         | Stable                  |

#### 2.1.2 Deprecated Extensions

The following are extensions that are to be categorized as `Deprecated` for both the enabled and `No` versions.

| Extension                         | Categorization | Negative Categorization |
|-----------------------------------|----------------|-------------------------|
| AlternativeLayoutRule             | Deprecated     | Deprecated              |
| AlternativeLayoutRuleTransitional | Deprecated     | Deprecated              |
| DoRec                             | Deprecated     | Deprecated              |
| NullaryTypeClasses                | Deprecated     | Deprecated              |
| OverlappingInstances              | Deprecated     | Deprecated              |
| ParallelArrays                    | Deprecated     | Deprecated              |
| PolymorphicComponents             | Deprecated     | Deprecated              |
| Rank2Types                        | Deprecated     | Deprecated              |
| RecordPuns                        | Deprecated     | Deprecated              |
| TypeInType                        | Deprecated     | Deprecated              |

In particular it is the flag that is `Deprecated` in both cases for the extensions. The negation of all of these extensions are behaviors as if the positive flag had not been specified, and that behavior is not changed. So while for each extension, the `No*` flag is deprecated, we do not change the behavior of not specifying the positive flag.

As a concrete example, the flag `NoRank2Types` is deprecated, but the behavior from the lack of `Rank2Types` is not changed.

### 2.2 Extensions Impacting Stablity Categorization

Here we list extensions where the categorization is different based on if the extension is enabled or not. This separation is specifically to make reading the tables of extensions easier. We have further divided this section into `Legacy` and `Experimental` extensions.

#### 2.2.1 Legacy Extensions

The following are all extensions that are to be categorized as `Legacy` when enabled, and for the `No` version are categorized as `Stable`. As a reminder, the [categorizations](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0601-extension-lifecycle-framework.md#21-categories) state that `Legacy` extensions are not recommended for new code but are expected to be supported indefinitely.

The following table contains the language extension, the proposed categorization for the extension being turned on, and the proposed categorization for the extension being turned off.

| Extension                | Categorization | Negative Categorization |
|--------------------------|----------------|-------------------------|
| CUSKs                    | Legacy         | Stable                  |
| DatatypeContexts         | Legacy         | Stable                  |
| DeepSubsumption          | Legacy         | Stable                  |
| NondecreasingIndentation | Legacy         | Stable                  |
| NPlusKPatterns           | Legacy         | Stable                  |

#### 2.2.2 Experimental Extensions

The following are extensions that are to be categorized as `Experimental` when enabled, and for the `No` or negative version are categorized as `Stable`. As a reminder, the [categorizations](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0601-extension-lifecycle-framework.md#21-categories) state that `Experimental` extensions "are undergoing active development, or have consequences that make it impossible in practice to avoid breaking changes".

The following table contains the language extension, the proposed categorization for the extension being turned on, and the proposed categorization for the extension being turned off.

| Extension              | Categorization | Negative Categorization |
|------------------------|----------------|-------------------------|
| GHCForeignImportPrim   | Experimental   | Stable                  |
| ExtendedLiterals       | Experimental   | Stable                  |
| ImpredicativeTypes     | Experimental   | Stable                  |
| JavaScriptFFI          | Experimental   | Stable                  |
| LinearTypes            | Experimental   | Stable                  |
| OverloadedRecordUpdate | Experimental   | Stable                  |
| RequiredTypeArguments  | Experimental   | Stable                  |
| TypeAbstractions       | Experimental   | Stable                  |

### 2.3 Uncategorized Extensions

The following are the extensions that are uncategorized as of this proposal. They are left uncategorized to allow progress overall on categorization by reducing the amount of discussion on any one proposal. They will be categorized with further discussion in some following proposal(s).

| Extension               |
|-------------------------|
| AllowAmbiguousTypes     |
| ApplicativeDo           |
| Arrows                  |
| AutoDeriveTypeable      |
| DoAndIfThenElse         |
| DuplicateRecordFields   |
| FieldSelectors          |
| FunctionalDependencies  |
| IncoherentInstances     |
| LexicalNegation         |
| ListTuplePuns           |
| MonoLocalBinds          |
| NegativeLiterals        |
| OverloadedRecordDot     |
| OverloadedStrings       |
| PackageImports          |
| PartialTypeSignatures   |
| PatternSignatures       |
| QuantifiedConstraints   |
| QuasiQuotes             |
| RebindableSyntax        |
| RelaxedLayout           |
| RelaxedPolyRec          |
| Safe                    |
| ScopedTypeVariables     |
| StarIsType              |
| StaticPointers          |
| Strict                  |
| TemplateHaskell         |
| TemplateHaskellQuotes   |
| TransformListComp       |
| Trustworthy             |
| TupleSections           |
| TypeData                |
| TypeFamilies            |
| TypeFamilyDependencies  |
| UndecidableInstances    |
| UndecidableSuperClasses |
| UnliftedDatatypes       |
| UnliftedFFITypes        |
| Unsafe                  |
| ViewPatterns            |

## 3. Effect and Interactions

Extensions that are marked as `Deprecated` must be added to `-Wdeprecated-flags` to emit an appropriate warning on their use.

For all categorizations except for `Deprecated`, the only expected user-facing impact of this proposal is increased documentation and visibility for users. On it's own, this proposal does not impact availability of any feature. Categorization of any particular extension could result in additional consideration per the policy set forth [previously]((https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0601-extension-lifecycle-framework.md)).

## 4. Costs and Drawbacks

The immediate cost of this proposal is in documenting each extension with it's categorization. There are potential costs if the categorizations of `Stable` are too aggressive. First, undesirable behavior could be "locked-in" for support. Second, if a `Stable` extension is changed there could be user confusion. Lastly, if a `Stable` extension needs to be re-categorized shortly after this proposal that is additional churn and discussion for the GHC Steering Committee.

## 5. Backward Compatibility

Since there is no impact to feature availability from this proposal, it is completely backward compatible.

## 6. Related Work

As previously mentioned this builds on a previously accepted [proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0601-extension-lifecycle-framework.md). Further the categorizations here have been influenced heavily by a [discussion thread](https://github.com/ghc-proposals/ghc-proposals/discussions/635) and [another proposal](https://github.com/ghc-proposals/ghc-proposals/pull/636).

## 7. Alternatives

Much of this proposal works within the constraints of the previously accepted [proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0601-extension-lifecycle-framework.md). That said, any of the chosen categorizations could be picked differently and be an alternative.

## 7. Unresolved Questions

There are still a number of extensions left to categorize. These are left out solely to limit the scope of discussion.

## 8. Implementation Plan

The author(s) volunteer to amend the GHC User's Guide to add the categorization documentation to each extension.
