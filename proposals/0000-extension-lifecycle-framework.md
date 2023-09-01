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

The authors of this proposal believe that a clear lifecycle for these programming language extensions would make it easier for newcomers, those responsible for onboarding new Haskell developers, and the community more broadly, to make sense of the Haskell language extensions. This framework would make it clear which extensions are prone to rapid evolution, which of them are stable and suitable for serious use, and which of them are generally not considered suitable for new development. Extending GHC's warnings to account for these categories allows projects to find out when features they use have been deprecated in a new compiler version as well as to specify general policies about language stability that can be checked mechanically.

## 2. Proposed Change Specification

### 2.1. Categories and Lifecycle
Language extensions will be classified into the following categories
 * `Unstable` extensions are undergoing active development, or have consequences that make it impossible in practice to avoid breaking changes. The syntax and semantics that are enabled by the extension are likely to change regularly. It is expected that most new language extensions will begin as unstable, while the developers and the community gain experience with their use. At the time of writing, `LinearTypes` and `OverloadedRecordUpdate` seem to be in this category.
 
 * `Stable` extensions are considered to be finished and are not expected to undergo regular changes. These features can be used without worry of unexpected changes, and they are not known to contain serious design or implementation deficiencies. Any breaking change to a stable extension will be announced well in advance of the change being made, with a migration path provided if possible. Ideally, no breaking change will be made to a `Stable` extension, with incompatible changes resulting instead in a new, related extension to enable smooth migration. Some extensions in this category might be `MultiWayIf`, `MonoLocalBinds`, and `ViewPatterns`.

 * `Deprecated` extensions are considered to be design or implementation dead ends, and should not be used in new code. Deprecating an extension means that GHC intends to remove support for it in a future release (though this decision may be revised in the light of user feedback). When an extension is deprecated, the user's guide and the compiler warning _must_ include a statement about the longevity of the extension, though this need not necessarily commit to a concrete time for removal. Additionally, the user's guide and the warning should direct users to information about migrating away from the deprecated extension. Any extension will be deprecated prior to removal. Deprecated extensions are expected to include `OverlappingInstances`, `Rank2Types`, `RecordPuns`, and `TypeInType`.
 
 * `Legacy` extensions are explicitly not recommended for new code, much like `Deprecated`, however, they are expected to be supported indefinitely. This may be to maintain an older language set, maintain a bridge to a newer feature, or otherwise keep backward compatibility. Expected legacy extensions include `CUSKs`, `DeepSubsumption`,  and `NPlusKPatterns`.

Above we list expected categorizations of several extensions based on statements in the user's guide. We do not attempt to prescribe any particular categorizations as part of this proposal and simply provide a small number of expectations from the perspective of someone reading the existing documentation.

Non-normatively, the expected extension lifecycle includes the following transitions:
 * `Unstable` -> `Stable`
 * `Unstable` -> `Deprecated`
 * `Unstable` -> `Legacy`
 * `Stable` -> `Legacy`
 * `Legacy` -> `Deprecated`

However, it also seems plausible that new knowledge might from time to time cause a stable extension to once again be considered unstable, e.g. in the face of soundness bugs or subtle interactions with other features. We explicitly do not rule out any transition to allow for unforeseen circumstances. The extension classifications are first and foremost a machine-checked communications channel between language designers, compiler developers, and users.

For existing, or future, language sets such as `GHC2021` or `Haskell98`, it is expected that none of the contained extensions would be `Unstable`. However, this proposal does not seek to impose any particular policy on the inclusion of extensions into language sets - the developers and the steering committee are always in the best position to make a decision about a concrete extension and extension set.

Moving an extension through the lifecycle will require a GHC proposal. This will give users a chance to provide their input, it will help catch unexpected interactions (such as a `Stable` extension implying an `Unstable` one, and thus triggering warnings by default), and it provides a way for language implementers and users to clarify their mutual expectations.

### 2.2. User Interface

Haskell users have different tolerances for risks related to language change. Some derive great value from using the newest features, while others are conservative. GHC should serve the whole spectrum. Similarly, users typically do not revisit the documentation for a language feature unless the compiler's behavior surprises them, so users should be able to specify their extension use policies and then receive feedback from the compiler when their code violates said policies, whether this violation is due to changes in the code or changes in the compiler.

There will be an additional set of warnings:
 * `-WXDeprecated`: Issue a warning when a deprecated extension is enabled
 * `-WXStable`: Issue a warning when a stable extension is enabled
 * `-WXUnstable`: Issue a warning when an unstable extension is enabled 
 * `-WXLegacy`: Issue a warning when a legacy extension is enabled.

Each category will also support the usual`-Wno-` syntax, so `-Wno-XDeprecated` will turn off warnings for deprecated extensions. Additionally, each extension should have a configurable warning that can be individually enabled or disabled, allowing users and teams to inform GHC about their local policies and needs. For instance, `-WXDeprecated -Wno-XRank2Types` would warn about all deprecated extensions except `Rank2Types`.

Furthermore, `-WXDeprecated` and `-WXUnstable` would be added to `-Wcompat` for the next release to allow the community time to adjust, and then they will be on by default. This is because these two categories describe extensions that are likelier to lead to incompatibilities with future releases of GHC. `-WXLegacy` should be added to `-Wall`, so that only those who request extra feedback will get it.

Note that this warning syntax rules out the names `Deprecated`, `Stable`, `Unstable`, and `Legacy` for future language extensions.

#### 2.2.1. Warnings are for Enabled Extensions

Extension warnings will be issued when an extension is enabled, not when the language features that it supports are used in a program. The following program should issue a warning, even though no multi-way conditionals occur in it:
```
{-# OPTIONS_GHC -WXMultiWayIf #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

main :: IO ()
main = pure ()
```

There are two main reasons to do this, rather than wait for the associated code path in the compiler to be invoked:

 1. Users don't need to think about when one extension implies features from another. The mental model for warnings is simple and straightforward, and it's easier to see the connection between a change in the code and a new warning in CI.
 
 2. The implementation is likely to be simpler, because the extension lifecycle warnings can presumably be issued without needing to scatter them throughout the parser and type checker.

Warnings should be issued for extensions no matter how they are enabled. For instance, the following program should also issue a warning:
```
{-# OPTIONS_GHC -WXBangPatterns #-}
module Main where
```
This is because GHC defaults to `GHC2021`, which enables the `BangPatterns` extension. The extension lifecycle warnings issued for language extension sets such as `GHC2021` and `Haskell98` should be equivalent to the union of the warnings issued for the included extensions. In other words, `-WXGHC2021` is equivalent to `-WXMultiParamTypeClasses -WXDeriveFunctor ...`.

#### 2.2.2. Warnings and Extensions Commute

Warning flags should commute with extensions, whether at the command line or in pragmas. The user interface should be as if the complete warning configuration is computed, followed by the set of enabled extensions, after which the set of displayed warnings is determined.

In other words, both of the following programs should be equivalent with respect to warnings:
```
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -WXMultiWayIf #-}
module Main where
...
```

```
{-# OPTIONS_GHC -WXMultiWayIf #-}
{-# LANGUAGE MultiWayIf #-}
module Main where
...
```

In particular, both should emit a warning.

#### 2.2.3. Comments 

The warning feature should allow the GHC developers to easily add comments about an extension's lifecycle status that are then conveyed to the user with the warning. For instance, if `OverlappingInstances` is `Deprecated`, the compiler should be able to direct the user to the `{-# OVERLAPPING #-}` and `{-# OVERLAPPABLE #-}` pragmas, and if `Rank2Types` ends up being a legacy synonym for `RankNTypes`, the compiler should inform users of this fact in its warning about the extension's `Legacy` status. The ability to add these comments about extension lifecycles does not impose an obligation to do so, unless the extension is deprecated - it should be done when the developers deem it helpful to users.

#### 2.2.4. Identifying Flags

This section is non-normative, in the sense that an implementation of this proposal need not fulfill these criteria. However, we believe that the use case served by this section's specification is important, and that the feature will be more useful if this is done.

When possible, the implementation should internally distinguish between extension stability warnings that result from `-WXUnstable`, `-WXStable`, `-WXLegacy`, or `-WXDeprecated`, and those that result from single-extension warnings flags (such as `-WXLinearTypes`), making it possible to phrase the warning differently. This is because the individual extension warnings may be used for reasons other than stability - a company might decide to warn on an extension simply because the team has decided that it doesn't fit the house style, so an error message mentioning that the extension is stable would be misleading.

Warnings that result from one of the four lifecycle warning flags (`-WXUnstable`, `-WXStable`, `-WXLegacy`, or `-WXDeprecated`) should mention the lifecycle in their text, while warnings that result from a single extension flag (e.g. `-WXBlockArguments`) should not.

See section 3.2 for examples of how the warning text may usefully distinguish between these cases.

#### 2.2.5 Extension Warnings vs Extension Category Warnings

For the sake of consistency, extension warnings and their categories should use the usual GHC mechanisms to communicate to users which flags control the warnings. In particular, GHC should show the specific extension warning, and then show which category it is in if [`-fshow-warning-groups`](https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/using-warnings.html#ghc-flag--fshow-warning-groups) is enabled.

### 2.3. Documentation

The user's guide for each extension documents where it is in the extension lifecycle. We expect this to be next to the "Since" and "Implied by" fields in extension documentation. Additionally, the user's guide should provide a history of the extension's sojourn through the various states, and the GHC versions in which it went from being unstable to stable, or from stable to deprecated.

## 3. Examples

With the use of `-Wcompat`, and presuming the categorization of `IncoherentInstances` as `Deprecated`, matching the statements in the user's guide, consider the following code snippet:
```haskell
{-# LANGUAGE IncoherentInstances #-}
module Foo where
```
This would result in a warning on the language pragma at compilation time stating the use of a `Deprecated` extension.

### 3.1. Ordering of Warning Flags and Extensions

Even though extension pragmas and flags and warning flags should commute with one another, ordering of extensions and warning flags is still important, with later flags overriding earlier ones.

Just as
```
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wall #-}

module Foo where

foo x = 5
```
emits a warning for the missing signature for `foo`, while
```
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Foo where

foo x = 5
```
does not, none of the following three programs should emit an extension warning:
```
{-# OPTIONS_GHC -WXLegacy #-}
{-# OPTIONS_GHC -Wno-XNPlusKPatterns #-}
{-# LANGUAGE NPlusKPatterns #-}
module Main where
```
```
{-# LANGUAGE NPlusKPatterns #-}
{-# OPTIONS_GHC -WXLegacy #-}
{-# OPTIONS_GHC -Wno-XNPlusKPatterns #-}
module Main where
```
```
{-# OPTIONS_GHC -WXLegacy #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# OPTIONS_GHC -Wno-XNPlusKPatterns #-}
module Main where
```

These three should:
```
{-# OPTIONS_GHC -Wno-XNPlusKPatterns #-}
{-# OPTIONS_GHC -WXLegacy #-}
{-# LANGUAGE NPlusKPatterns #-}
module Main where
```
```
{-# OPTIONS_GHC -Wno-XNPlusKPatterns #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# OPTIONS_GHC -WXLegacy #-}
module Main where
```
```
{-# LANGUAGE NPlusKPatterns #-}
{-# OPTIONS_GHC -Wno-XNPlusKPatterns #-}
{-# OPTIONS_GHC -WXLegacy #-}
module Main where
```

### 3.2. Warnings for Extension Flags vs Extension Set Flags

These examples illustrate section 2.2.4.

Assuming that `BlockArguments` is `Stable`, a team may nonetheless wish to rule it out simply because it doesn't fit the house style. One way to do this is to enable a warning for it in either `stack.yaml`:
```
ghc-options:
  "$locals": -WXBlockArguments
```
or `cabal.project`:
```
program-options
  ghc-options: -WXBlockArguments
```

While this use case is not a primary motivating factor for this proposal, it seems inevitable that it will occur. Thus, the warning message should _not_ be something like:
```
Foo.hs:3:5: warning: [-WXBlockArguments (in -WXStable)]
    The extension BlockArguments is stable
```
but rather
```
Foo.hs:3:5: warning: [-WXBlockArguments (in -WXStable)]
    Use of extension BlockArguments
```
or
```
Foo.hs:3:5: warning: [-WXBlockArguments (in -WXStable)]
    Use of (stable) extension BlockArguments
```


On the other hand, when the warnings are used to express a stability policy, the messages should support this. On the assumption that `LinearTypes` is `Unstable` and a build system is configured as with the following `stack.yaml` snippet:
```
ghc-options:
  "$locals": -WXUnstable -WXLegacy -WXDeprecated
```
or `cabal.project` snippet:
```
program-options
  ghc-options: -WXUnstable -WXLegacy -WXDeprecated
```
then a module that enables `LinearTypes` should get a warning like:
```
Foo.hs:3:5: warning: [-WXLinearTypes (in -WXUnstable)]
    The extension `LinearTypes` is experimental and subject to breaking changes.
```

If this can't be accomplished for technical reasons, then the message should be written such that it makes sense for both use cases.


## 4. Effect and Interactions

The tension between large-scale industrial users of Haskell who desire stability and predictability and GHC's traditional role as a venue for language experiments is very real. This system addresses some of the industrial concern by providing clear communication to users that sets realistic expectations for language extensions. By providing users with an explicit way to indicate their own tolerances for legacy, unstable, and deprecated features, this proposal aims to ease the tension in a way that requires as little as possible additional investment on the part of the GHC team.

The primary interaction that we anticipate from this change is with existing warning flags and build configurations. Because this proposal does not change the semantics of any programs, we expect that it will not be a major disturbance.


## 5. Costs and Drawbacks

### 5.1. Development Costs

Development and ongoing costs are expected to exist primarily in the form of additional time to sufficiently warn users of upcoming changes, creating and updating the status of extensions, and maintenance of the warning flags. We expect that the costs associated with moving an extension from one category to another will be small, but the discussion around doing so might take time - in particular, this may result in an increased workload for the GHC Steering Committee to process lifecycle transitions.

### 5.2. Transition Costs

Organizations that use `-Werror` may incur a one-time cost in updating their compiler flags to disable warnings about unstable or deprecated extensions.

### 5.3. Experts vs Less-Expert Users

There is a trade-off in the selection of default warnings. Making `-WXUnstable` be on by default (after a suitable period in `-Wcompat`) means that some Haskell users will need to carry out two steps to use still-experimental features: after turning on the extension, the warning will need to be suppressed. For instance, a user might introduce a scoped wombat pun into their file, which requires enabling the hypothetical unstable extension `ScopedWombatPunning`. First, GHC will advise that they enable this extension in an error message. Upon following this advice, the user will then be presented with a warning on their `LANGUAGE` pragma along the lines of:
```
SomeModule.hs:1:4-1:32: warning: [-WXScopedWomatPunnin (in -WXUnstable)]
The ScopedWombatPunning extension is experimental, and the features that it enables may have breaking changes in an upcoming version.

You can suppress this warning with the `-Wno-XScopedWombatPunning` option.
```
If this user additionally uses `-Werror` or simply prefers to not have warnings in their code, then they must then add a further `GHC_OPTIONS -Wno-XScopedWombatPunning` pragma to the program, or turn on "advanced mode" by adding `-Wno-XUnstable` to the compiler configuration in their build system.

If this user is a Haskell expert, then this extra step may be unwelcome - the compiler already told them to use `LANGUAGE ScopedWombatPunning`, why should it then chide them for doing so? However, if the user is not yet an expert, then this default warning is valuable because it can prevent them from depending on less-stable behavior without realizing it. This can reduce the difficulty of deciding which parts of the language to use in a project, especially one with other people. If the warning were off by default, then this user would need to first find out how to enable it, and this may not happen until after lots of code was written that depended on scoped wombat punning. A series of relatively small annoyance to expert Haskellers seems preferable to expensive code migrations.


## 6. Alternatives

### 6.1. Omitting Compiler Warnings

One alternative design is to not add warnings to the compiler, and communicate about the status of extensions only in the User's Guide. We consider this to be unlikely to provide as much value, because users typically do not return to sections of the User's Guide that cover features that they already understand. The migration of an extension into a warning set provides a reason for them to consult the documentation.

### 6.2. Implement Warnings Elsewhere

Warnings could additionally be implemented with a canonical configuration for a tool such as `hlint` or `stan`. Warnings from GHC itself have a number of advantages, however. First off, the categorization of extensions is maintained in one place, so teams don't need to construct and then maintain their own set. It's much easier to have a minor local deviation from an established standard than it is to create one. Secondly, compiler updates that change the status of an extension will trigger warnings without someone needing to remember to modify the configuration of an external linter, and projects that support a range of GHC versions will get the appropriate warnings for each version they run in CI without any extra configuration. And, perhaps most importantly, defaults matter most for less-experienced users. How would a new team know that they should install an extra linter and configure it this way? How would they decide between competing linter configurations? It seems valuable to provide them with something that basically reflects the consensus of the experts that comes by default.

### 6.3. Use Only Deprecations

Another alternative consists of not having an explicit lifecycle for extensions, but only adding deprecation warnings. This has the advantage of being a lighter-weight process that requires less from the developers of language extensions. Also, because any model necessarily neglects certain details, an implicit discussion of an extension's status provides space for more nuance. However, we believe that a simpler model that captures the world well enough is able to provide the vast majority of the information that's relevant to make decisions without incurring nearly as much of a cost in time and training as a fully-detailed and nuanced view (which can still be available in the documentation).

### 6.4. Lazily Add the `Deprecated` Set

It is extremely rare that GHC actually removes an extension. It would be possible to refrain from adding the `Deprecated` category until it actually becomes needed. However, having this category (even if unused) serves important purposes:

 1. Right now, the GHC documentation uses the term "deprecated" to refer both to things that are expected to disappear soon and to language extensions that are expected to remain for the forseeable future. Making it clear that these extensions are not facing imminent removal is valuable, and having the codified distinction helps communicate this.
 
 2. The definition of an extension being `Stable` requires a deprecation process before removing it. The presence of`-WXDeprecated` allows teams to prepare for this future eventuality.
 
### 6.5 Conservativity Warnings and Syntactic vs Semantic Warnings

It would be possible to further classify extensions along two more dimensions:
  * Whether they change the meaning of a program when enabled (such as `ScopedTypeVariables` and `MonoLocalBinds`) and those that add features without changing existing programs (such as `MultiWayIf`, `NPlusKPatterns`, and `LinearTypes`)
  * Extensions that merely enable syntactic sugar (such as `MultiWayIf`, `NPlusKPatterns`, `DoAndIfThenElse`) and those that enrich the type system with new features (such as `DataKinds`, `TypeFamilies`, or `PolyKinds`).

This is clearly something that's desirable for users to know. However, this information is orthogonal to the stability, so we believe that mechanisms to communicate these aspects should be designed in a separate proposal.


## 7. Unresolved Questions



## 8. Implementation Plan

The Haskell Foundation will either recruit volunteers or pay for the technical work of implementing this proposal. The classifications of individual language extensions will be left to a future proposal, and those discussions can proceed while implementation is ongoing, should this proposal be accepted.


## 9. Endorsements

A number of endorsements from industry users were provided for the prior version of this proposal in its [discussion thread](https://github.com/ghc-proposals/ghc-proposals/pull/601).
