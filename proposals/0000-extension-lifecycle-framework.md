---
author: Trevis Elser, Chris Dornan, David Thrane Christiansen
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/601).

# Extension Lifecycle Framework

We propose a categorization scheme for Haskell language extensions. This scheme is simple, in that there are few categories that are described in terms of the user-relevant aspects, and it is actionable, in that it suggests concrete changes to the warning system of GHC that allow users to express their own risk tolerance and get guidance as they upgrade their compiler.  The purpose of this proposal is not to classify all extensions - rather, we seek to establish a basis upon which to carry out that work subsequently. Example classifications are provided to illustrate the proposed framework, and acceptance of this proposal should not count as acceptance these examples.

## 1. Motivation

Since its inception, the Haskell programming language and GHC, its principal toolchain/development platform, has been open to innovation in programming language design through Haskell language extensions. The creation of those extensions has been well managed, giving rise to a programming language that is both coherent and still evolving and vital after more than three decades of astonishing development.

However, once an extension clears the review process and gets implemented in the compiler, there is currently no framework for understanding or managing its evolution. The strength of the development model is its openness, allowing experimental features to be tried out in the community. Features that the community discovers to be problematic in practice can be not enabled and the community can, over time, reach a consensus about the value of these extensions.

The weakness of this model is that, over time, the accumulation of programming extensions in various states of use and disuse can create confusion, especially for developers being introduced to Haskell. There is not one Haskell, but many, and each individual or team must necessarily choose their own. Not everyone has yet had the opportunity to accumulate the experience and knowledge that will allow them to quickly determine whether or not to use a given language extension, leading to various points of stress. 

The authors of this proposal believe that a clear lifecycle for these programming language extensions would make it easier for newcomers, those responsible for onboarding new Haskell developers, and the community more broadly, to make sense of the Haskell language extensions. This framework would make it clear which extensions are experimental and prone to rapid evolution, which of them are mature and thus relatively stable and suitable for serious use, and those that are generally not considered suitable for new development. Extending GHC's warnings to account for these categories allows projects to find out when features they use have been deprecated in a new compiler version as well as to specify general policies about language stability that can be checked mechanically.

## 2. Proposed Change Specification

### 2.1. Categories and Lifecycle
Language extensions will be classified into the following categories
 * `Experimental` extensions are undergoing active development. The syntax and semantics that are enabled by the extension are likely to change regularly. It is expected that most new language extensions will begin as experimental. At the time of writing, `LinearTypes` and `OverloadedRecordUpdate` seem to be in this category.
 
 * `Mature` extensions are considered to be finished and are not expected to undergo regular changes. These features can be used without worry of unexpected changes, and they are not known to contain serious design or implementation deficiencies. Any breaking change to a mature extension will be announced well in advance of the change being made, with a migration path provided if possible. Some extensions in this category might be `MultiWayIf`, `MonoLocalBinds`, and `ViewPatterns`.

 * `Deprecated` extensions are considered to be design or implementation dead ends, and should probably not be used in new code. Deprecating an extension _must_ come with a statement about the longevity of the extension. This need not be a commitment to remove it at a concrete time, but is a commitment to either remove it or transition it to another state at a time stated by GHC HQ when it is marked as `Deprecated`. Any extension will be deprecated prior to removal. Deprecated extensions are expected to include `IncoherentInstances`, `OverlappingInstances`, `Rank2Types`, `RecordPuns`, and `TypeInType`.
 
 * `Legacy` extensions are explicitly not recommended for new code, much like `Deprecated`, however, they are expected to be supported indefinitely. This may be to maintain an older language set, maintain a bridge to a newer feature, or otherwise keep backward compatibility. Expected legacy extensions include `CUSKs`, `DeepSubsumption`,  and `NPlusKPatterns`.

Above we list expected categorizations of several extensions based on statements in the user's guide. We do not attempt to prescribe any particular categorizations as part of this proposal and simply provide a small number of expectations from the perspective of someone reading the existing documentation.

The expected extension lifecycle includes the following transitions:
 * `Experimental` -> `Mature`
 * `Experimental` -> `Deprecated`
 * `Mature` -> `Legacy`
 * `Mature` -> `Deprecated`
 * `Legacy` -> `Deprecated`
 * `Deprecated` -> `Legacy`

However, it also seems plausible that new knowledge might from time to time cause a mature extension to once again be considered experimental, e.g. in the face of soundness bugs or subtle interactions with other features. We also do not rule out any transition explicity to allow for unforeseen circumstances. 

For existing, or future, language sets such as `GHC2021` or `Haskell98`, it is expected that none of the contained extensions would be `Experimental`.

### 2.2. User Interface

Haskell users have different tolerances for risks related to language change. Some derive great value from using the newest features, while others are conservative. GHC should serve the whole spectrum.

There will be an additional set of warnings:
 * `-WXDeprecated`: Issue a warning when a deprecated extension is used
 * `-WXMature`: Issue a warning when a mature extension is used
 * `-WXExperimental`: Issue a warning when an experimental extension is used 
 * `-WXLegacy`: Issue a warning when a legacy extension is used.

Each category will also support the usual`-Wno-` syntax, so `-Wno-XDeprecated` will turn off warnings for deprecated extensions. Additionally, each extension should have a configurable warning that can be individually enabled or disabled, allowing users and teams to inform GHC about their local policies and needs. For instance, `-WXDeprecated -Wno-XRank2Types` would warn about all deprecated extensions except `Rank2Types`.

Furthermore, `-WXDeprecated` and `-WXExperimental` would be added to `-Wcompat` for the next release to allow the community time to adjust prior to being on by default. However, the goal is for `-WXDeprecated` and `-WXExperimental` to be on by default.

Note that this warning syntax rules out the names `Deprecated`, `Mature`, `Experimental`, and `Legacy` for future language extensions.

### 2.3. Documentation

The user's guide for each extension documents where it is in the extension lifecycle. We expect this to be next to the "Since" and "Implied by" fields in extension documentation. Additionally, the user's guide should provide a history of the extension's sojourn through the various states, and the GHC versions in which it went from being experimental to mature, or from mature to deprecated.

## 3. Examples

With the use of `-Wcompat`, and presuming the categorization of `IncoherentInstances` as `Deprecated`, matching the statements in the user's guide, consider the following code snippet:
```haskell
{-# LANGUAGE IncoherentInstances #-}
module Foo where
```
This would result in a warning at compilation stating the use of a `Deprecated` extension.

## 4. Effect and Interactions

The tension between large-scale industrial users of Haskell who desire stability and predictability and GHC's traditional role as a venue for language experiments is very real. This system addresses some of the industrial concern by providing clear communication to users that sets realistic expectations for language extensions. By providing users with an explicit way to indicate their own tolerances for legacy, experimental, and deprecated features, this proposal aims to ease the tension in a way that requires as little as possible additional investment on the part of the GHC team.

The primary interaction that we anticipate from this change is with existing warning flags and build configurations. Because this proposal does not change the semantics of any programs, we expect that it will not be a major disturbance.


## 5. Costs and Drawbacks

Development and ongoing costs are expected to exist primarily in the form of additional time to sufficiently warn users of upcoming changes, creating and updating the status of extensions, and maintenance of the warning flags.

Organizations that use `-Werror` may incur a one-time cost in updating their compiler flags to disable experimental or deprecated extension warnings.

## 6. Alternatives

One alternative design is to not add warnings to the compiler, and communicate about the status of extensions only in the User's Guide. We consider this to be unlikely to provide as much value, because users typically do not return to sections of the User's Guide that cover features that they already understand. The migration of an extension into a warning set provides a reason for them to consult the documentation.

Another alternative consists of not having an explicit lifecycle for extensions, but only adding deprecation warnings. This has the advantage of being a lighter-weight process that requires less from the developers of language extensions. Also, because any model necessarily neglects certain details, an implicit discussion of an extension's status provides space for more nuance. However, we believe that a simpler model that captures the world well enough is able to provide the vast majority of the information that's relevant to make decisions without incurring nearly as much of a cost in time and training as a fully-detailed and nuanced view (which can still be available in the documentation).


## 7. Unresolved Questions



## 8. Implementation Plan

The Haskell Foundation will either recruit volunteers or pay for the technical work of implementing this proposal. The classifications of individual language extensions will be left to a future proposal, and those discussions can proceed while implementation is ongoing, should this proposal be accepted.


## 9. Endorsements
