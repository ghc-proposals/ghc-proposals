---
author: Hécate Moonlight
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/417).

# GHC Maintainer preview

This GHC Proposal introduces the concept of a GHC Maintainer Preview (name can be moderately bikeshedded).

The principle is to provide library authors and tooling maintainers an RC version of each new GHC release that would be fully identical to the final version.  
The aim is to give library and tooling authors the means to update their code to accomodate the new version, without publishing a user-oriented GHC release _yet_.

## Motivation

As things stand nowadays, major GHC releases (X.Y versions) are far from being ready to be immediately used in projects not because of the GHC dev team,
but because both final users **and** library maintainers and tooling authors are given the final release at the same time.  

It appears that releasing final versions for those different audiences erases the dependency that users have on maintainers,
but it does not change the fact that users should reasonably expect a working ecosystem for new major GHC versions.

On a side note, I think this may also help relieving the burden off the Hackage Trustees' shoulders who have to revise the metadata of packages to alter their bounds. 

## Proposed Changes

I suggest that we implement the following strategy: Once we arrive at a final RC of the next GHC version, we publish a "Maintainer Preview" version,
make it available in ghcup, the Ubuntu PPA (so that it can be easily used by CI systems like GitHub Actions or Travis), and send a call to maintainers that (in substance) say:

> Hi, the Maintainer Preview version of GHC X.Y has just come out! You have <period of time> to update your libraries before we release it for the users!
If you feel overwelmed by your maintainer duties, please reach out for help on this <public forum|mailing list|secret society meeting> and we will make sure that
the code is ready for the next release!  
>
>With love,  
The GHC dev team

Then, we mobilise a team (volunteers, people paid by Haskell Foundation partners, etc) to **actively** enact the process of helping out maintainers move their libraries and packages to the new version. I want to focus on their role as people to whom maintainers reach for help. We do not want to create friction with maintainers by taking away their agency. We can still fallback on metadata revision if needs be, but I would like to foster a better environment so that maintainers who burn-out can reach-out for help (this is a larger problem in our ecosystem and there are many angles from which to tackle it, this is one).

## Operating costs

### Communication
* We will need to communicate and interact with this subset of the community that are the package maintainers.
* We could start by publishing the news on Reddit, Lobste.rs, Discourse.

### Migration

* We need a team of people who will be able to give a hand to the maintainers who are in need of help to move their libraries. Maybe a Haskell Foundation strike team?
* This strike force needs some tooling, but I suggest we start with "here is an email address, send us an email and we'll help".

## Points to debate
I would like to have a discussion about the following points:

1. What should be the period of time during which we give maintainers a fully-compatible release to update their libraries and tool?
I think one month is good but I'd like some more points of view
1. Should we send an email to every package maintainer on Hackage to warn them of the publishing of a Maintainer Preview?
2. Should we allow them to unsuscribe from such emails?
3. How could we provide social incentives to motivate maintainers to keep their package up-to-date? 

## Implementation Plan

1. We add a fixed (or flexible?) period of time to the calendar of the GHC release team between the last RC release and the final, user-focused release.
2. We create a strike-force whose duties explicitly involve being responsible for reaching out to maintainers in public community spaces, and help maintainers migrate. There are resources such as the [Hackage Trustees cookbook](https://github.com/haskell-infra/hackage-trustees/blob/master/cookbook.md), and [head.hackage](https://ghc.gitlab.haskell.org/head.hackage/), as well as the GHC Wiki page for each release, that are already available. I suggest we make better use of those.

## What this proposal is not:

* A technical proposal
* Ensuring that the entirety of Hackage is ready for each major GHC release
* Turning Hackage into Stackage
* Stigmatising maintainers who cannot assume their duties anymore

## Alternatives

The current system. 
