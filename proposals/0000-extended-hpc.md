---
author: David Binder, Matthías Páll Gissurarson
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](<https://github.com/ghc-proposals/ghc-proposals/pull/539>).

Note this proposal is currently a WIP, and we would like to request feedback from the community.

**After creating the pull request, edit this file again, update the number in
the link, and delete this bold sentence.**

# Extended Haskell Program Coverage (HPC)

Haskell Program Coverage has been around for ~15 years now [1], and is a 
scalable, easy to use, and low cost way to generate coverage information
for Haskell programs.

However, sometimes coverage is not quite enough for the analysis required,
e.g. for fault-localization in automatic program repair [2]. Being able
to *trace* a program, i.e. get information about not only which parts
were touched, but rather in *which order* would be useful for dynamic
dataflow analysis and spectrum-based fault-localization.

In this proposal, we propose the following:
 + Adding tracing capabilities to GHC's HPC to allow for better fault-localization.
 + Changing the current `.tix` format to be more extensible, and include the
   new trace information.
 + Split HPC the binary out of the GHC repository from HPC the library, to make
   more rapid development easier.
 + (Optionally) extending GHC's runtime error messages with trace information.


## Motivation

... talk about HAT [3], how useful it would be for improving error messages
and automatic program repair, fuzzing techniques and generators for property-based
testing.


## Proposed Change Specification

Point to the fork of GHC that already implements tracing of *recently* evaluated expressions,
[https://github.com/Tritlo/ghc/tree/extended-ticks](https://github.com/Tritlo/ghc/tree/extended-ticks),
which will be extened to address some of the suggestions in [alternatives](#alternatives)


## Examples


Show some example programs, both simple cases where data is recently evaluated,
and more difficult cases that involve less recently evaluated expressions,
strictness and sharing.

## Effect and Interactions

The technique based on recently evaluated expressions has minimal effects
on performance, however, the technique based on storing the entire trace
and having thunks point to that trace is a more fundamental change.

See [alternatives](#alternatives) for more details.


## Costs and Drawbacks

+ For the recently evaluated expressions the cost is minimal,
  but more development time would be required for the thunk
  based approach.
+ Tools that consume `.tix` files will have to be able to
  understand the new format.


## Alternatives

### Keeping entire trace and storing pointer in thunks

This is a more heavy handed approach that is more useful
in real-life scenarios where the evaluation of data and
consumption of data is further apart. 


## Unresolved Questions

... lots


## Implementation Plan

Matthías Páll Gissurarson and David Binder will work together,
with MPG focusing on the RTS and DB on HPC the library and binary.

## Endorsements

+ Agustín Mista has expressed support that would benefit his work on generators.


## References

[1] Andy Gill and Colin Runciman. 2007. Haskell Program Coverage. In Proceedings of the ACM SIGPLAN Workshop on Haskell Workshop (Freiburg, Germany) (Haskell ’07). Association for Computing Machinery, New York, NY, USA, 1–12. 
[2] Matthías Páll Gissurarson, Leonhard Applis, Annibale Panichella, Arie van Deursen, and David Sands. 2022. PropR: Property-Based Automatic Program Repair. In The 44th IEEE/ACM International Conference on Software Engineering (ICSE). IEEE/ACM.
[3] Olaf Chitil, Colin Runciman, and Malcolm Wallace. 2002. Transforming Haskell for tracing. In Symposium on Implementation and Application of Functional Languages. Springer, 165–181.

