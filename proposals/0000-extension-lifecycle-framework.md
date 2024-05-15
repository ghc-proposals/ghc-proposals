---
author: Trevis Elser, Chris Dornan, David Thrane Christiansen
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/601).

# Extension Lifecycle Framework

We propose a categorization scheme for Haskell language extensions. This scheme is simple, in that there are few categories that are described in terms of the user-relevant aspects.  The purpose of this proposal is not to classify all extensions - rather, we seek to establish a basis upon which to carry out that work subsequently. Example classifications are provided to illustrate the proposed framework, and acceptance of this proposal should not count as acceptance these examples.

## 1. Motivation

Since its inception, the Haskell programming language and GHC, its principal toolchain/development platform, has been open to innovation in programming language design through Haskell language extensions. The creation of those extensions has been well managed, giving rise to a programming language that is both coherent and still evolving and vital after more than three decades of astonishing development.

However, once an extension clears the review process and gets implemented in the compiler, there is currently no framework for understanding or managing its evolution. The strength of the development model is its openness, allowing experimental features to be tried out in the community. Features that the community discovers to be problematic in practice can be not enabled and the community can, over time, reach a consensus about the value of these extensions.

The weakness of this model is that, over time, the accumulation of programming extensions in various states of use and disuse can create confusion, especially for developers being introduced to Haskell. There is not one Haskell, but many, and each individual or team must necessarily choose their own. Not everyone has yet had the opportunity to accumulate the experience and knowledge that will allow them to quickly determine whether or not to use a given language extension, leading to various points of stress.

The authors of this proposal believe that a clear lifecycle for these programming language extensions would make it easier for newcomers, those responsible for onboarding new Haskell developers as well as managers, and the community more broadly, to make sense of the Haskell language extensions. This framework would make it clear which extensions are prone to rapid evolution, which of them are stable and suitable for serious use, and which of them are generally not considered suitable for new development.

## 2. Proposed Change Specification

### 2.1. Categories and Lifecycle
Language extensions will be classified into the following categories
 * `Stable` extensions are considered to be finished and are not expected to undergo regular changes. These features can be used without worry of unexpected changes, and they are not known to contain serious design or implementation deficiencies. Any breaking change to a stable extension will be announced well in advance of the change being made, with a migration path provided if possible. For an extension to be classified as `Stable` it must be considered `Stable` when used in combination of all possible, non-mutually exclusive, extensions that are already `Stable`. This ensures any extension that is `Stable` will be have the same stability properties when combined with any other extension in the set, while allowing for mutually exclusive extensions to be added. Ideally, no breaking change will be made to a `Stable` extension, with incompatible changes resulting instead in a new, related extension, that is possibly mutually exclusive with the existing one, to enable smooth migration. Some extensions in this category might be `MultiWayIf`, `MonoLocalBinds`, and `ViewPatterns`.

 * `Experimental` extensions are undergoing active development, or have consequences that make it impossible in practice to avoid breaking changes. The syntax and semantics that are enabled by the extension are likely to change regularly.
   * It is expected that most new language extensions will begin as experimental, while the developers and the community gain experience with their use.
   * Despite being open to breaking change, an `Experimental` extension must be `Deprecated` prior to removal.
   * Some extensions in this category might be `RequiredTypeArguments`, `RebindableSyntax`, and `Arrows`.

 * `Deprecated` extensions are considered to be design or implementation dead ends, and should not be used in new code. Deprecating an extension means that GHC may remove support for it in a future release (though this decision may be revised in the light of user feedback).
   * When an extension is deprecated, the user's guide and the compiler warning _must_ include a statement about the longevity of the extension, though this need not necessarily commit to a concrete time for removal. Additionally, the user's guide and the warning should direct users to information about migrating away from the deprecated extension.
   * Any extension will be deprecated prior to removal.
   * Some extensions in this category might be `OverlappingInstances`, `Rank2Types`, `RecordPuns`, and `TypeInType`.

 * `Legacy` extensions are explicitly not recommended for new code, much like `Deprecated`, however, they are expected to be supported indefinitely. This may be to maintain an older language edition, maintain a bridge to a newer feature, or otherwise keep backward compatibility. Extensions that are `Legacy` may have some conflict with `Stable` extensions and explicitly do not need to be considered for an extension to be `Stable`. Some extensions in this category might be `CUSKs`, `DeepSubsumption`,  and `NPlusKPatterns`.

Above we list expected categorizations of several extensions based on statements in the user's guide. We do not attempt to prescribe any particular categorizations as part of this proposal and simply provide a small number of expectations from the perspective of someone reading the existing documentation.

The following transitions are included:
 * (does not exist) -> `Experimental`
 * `Experimental` -> `Stable`
 * `Experimental` -> `Deprecated`
 * `Experimental` -> `Legacy`
 * `Stable` -> `Deprecated`
 * `Stable` -> `Legacy`
 * `Legacy` -> `Deprecated`
 * `Deprecated` -> (does not exist)

These transitions are explicitly excluded:
 * `Stable` -> (does not exist)
 * `Legacy` -> (does not exist)

This is a small restriction, such that `Stable` or `Legacy` extensions require a deprecation cycle prior to removal.

The following transitions are possible but left at the discretion of implementors as to be included or not:

 * (does not exist) -> `Deprecated`
 * (does not exist) -> `Legacy`
 * (does not exist) -> `Stable`
 * `Experimental` -> (does not exist)
 * `Stable` -> `Experimental`
 * `Legacy` -> `Experimental`
 * `Legacy` -> `Stable`
 * `Deprecated` -> `Experimental`
 * `Deprecated` -> `Stable`
 * `Deprecated` -> `Legacy`

However, it also seems plausible that new knowledge might from time to time cause a stable extension to once again be considered experimental, e.g. in the face of soundness bugs or subtle interactions with other features. We explicitly allow for more transitions to allow for unforeseen circumstances. Restrictions are places on removal only. With this we aim to balance predictibility and flexibility. The consideration of both is important as extension classifications are first and foremost a communications channel between language designers, compiler developers, and users.

For existing, or future, language editions such as `GHC2021` or `Haskell98`, it is expected that none of the contained extensions would be `Experimental`. However, this proposal does not seek to impose any particular policy on the inclusion of extensions into language editions - the developers and the steering committee are always in the best position to make a decision about a concrete extension and extension set.

Moving an extension through the lifecycle will require a GHC proposal. This will give users a chance to provide their input, it will help catch unexpected interactions (such as an `Stable` extension implying an `Experimental` one, and thus potentially allowing breakage at any  moment), and it provides a way for language implementers and users to clarify their mutual expectations.

### 2.2. Documentation

The user's guide for each extension documents where it is in the extension lifecycle. We expect this to be next to the "Since" and "Implied by" fields in extension documentation. Additionally, the user's guide should provide a history of the extension's sojourn through the various states, and the GHC versions in which it transitioned, for example from being experimental to stable, or from stable to deprecated.

## 3. Effect and Interactions

The tension between large-scale industrial users of Haskell who desire stability and predictability and GHC's traditional role as a venue for language experiments is very real. This system addresses some of the industrial concern by providing clear communication to users that sets realistic expectations for language extensions.

Because this proposal does not change the semantics of any programs, we expect that it will not be a major disturbance.

## 4. Costs and Drawbacks

### 4.1. Development Costs

Development and ongoing costs are expected to exist primarily in the form of additional time to sufficiently warn users of upcoming changes, as well as creating and updating the status of extensions. We expect that the costs associated with moving an extension from one category to another will be small, but the discussion around doing so might take time - in particular, this may result in an increased workload for the GHC Steering Committee to process lifecycle transitions.

## 5. Alternatives

### 5.1. Include Another Categorization
Another categorization has been discussed at length to provide additional gradient for stability. An additional categorization could allow to further refine the notion of stability. Similarly,  we could categorize in many different ways. However, adding any additional categories is left as a possible enhancement for the future, should a need or desire arise..

### 5.2. Adding Compiler Warnings

One alternative design is to add warnings to the compiler, instead of communicating about the status of extensions only in the User's Guide. We consider this to be likely to provide value, because users typically do not return to sections of the User's Guide that cover features that they already understand. The migration of an extension into a warning set provides a reason for them to consult the documentation. This is left as future work to be covered in other extensions.

### 5.3. Implement Warnings and Categorizations Elsewhere

Warnings could additionally be implemented with a canonical configuration for a tool such as `hlint` or `stan`. Warnings from GHC itself have a number of advantages, however. First off, the categorization of extensions is maintained in one place, so teams don't need to construct and then maintain their own set. It's much easier to have a minor local deviation from an established standard than it is to create one. Secondly, compiler updates that change the status of an extension will trigger warnings without someone needing to remember to modify the configuration of an external linter, and projects that support a range of GHC versions will get the appropriate warnings for each version they run in CI without any extra configuration. And, perhaps most importantly, defaults matter most for less-experienced users. How would a new team know that they should install an extra linter and configure it this way? How would they decide between competing linter configurations? It seems valuable to provide them with something that basically reflects the consensus of the experts that comes by default.

### 5.4. Use Only Deprecations

Another alternative consists of not having an explicit lifecycle for extensions, but only adding deprecation warnings. This has the advantage of being a lighter-weight process that requires less from the developers of language extensions. Also, because any model necessarily neglects certain details, an implicit discussion of an extension's status provides space for more nuance. However, we believe that a simpler model that captures the world well enough is able to provide the vast majority of the information that's relevant to make decisions without incurring nearly as much of a cost in time and training as a fully-detailed and nuanced view (which can still be available in the documentation).

### 5.5. Lazily Add the `Deprecated` Set

It has been extremely rare that GHC actually removes an extension. This has been loudly signaled to the authors as a negative experience. It would be possible to refrain from adding the `Deprecated` category until it actually becomes needed. However, having this category (even if unused) serves important purposes:

 1. Right now, the GHC documentation uses the term "deprecated" to refer both to things that are expected to disappear soon and to language extensions that are expected to remain for the forseeable future. Making it clear that these extensions are not facing imminent removal is valuable, and having the codified distinction helps communicate this.

 2. The definition of an extension being `Stable` requires a deprecation process before removing it.

### 5.6 Conservativity Warnings and Syntactic vs Semantic Warnings

It would be possible to further classify extensions along two more dimensions:
  * Whether they change the meaning of a program when enabled (such as `ScopedTypeVariables` and `MonoLocalBinds`) and those that add features without changing existing programs (such as `MultiWayIf`, `NPlusKPatterns`, and `LinearTypes`)
  * Extensions that merely enable syntactic sugar (such as `MultiWayIf`, `NPlusKPatterns`, `DoAndIfThenElse`) and those that enrich the type system with new features (such as `DataKinds`, `TypeFamilies`, or `PolyKinds`).

This is clearly something that's desirable for users to know. However, this information is orthogonal to the stability, so we believe that mechanisms to communicate these aspects should be designed in a separate proposal.

### 6. Related Work

There are several other proposals and discussions that are related. There is a general [discussion thread](https://github.com/ghc-proposals/ghc-proposals/discussions/635) around features that are stable or not. Also a much more broadly sweeping [proposal](https://github.com/ghc-proposals/ghc-proposals/pull/636) includes a notion of stability. This proposal is now aiming to be very targeted in effort to make progress.

## 7. Unresolved Questions


## 8. Implementation Plan

The Haskell Foundation will either recruit volunteers or pay for the technical work of implementing this proposal. The classifications of individual language extensions will be left to a future proposal, and those discussions can proceed while implementation is ongoing, should this proposal be accepted.


## 9. Endorsements

A number of endorsements from industry users were provided for the prior version of this proposal in its [discussion thread](https://github.com/ghc-proposals/ghc-proposals/pull/601).
