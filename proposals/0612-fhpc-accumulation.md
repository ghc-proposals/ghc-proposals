---
author: David Binder
date-accepted: "2024-02-26"
ticket-url: "https://gitlab.haskell.org/ghc/ghc/-/issues/23955"
implemented: ""
---

This proposal was [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/612).

# Change the semantics of -fhpc

If I compile a program with the `-fhpc` option and run it, I expect to obtain a `.tix` file which contains information about the code covered during the execution of that run.
This, however, is surprisingly not the behaviour that is implemented.
Instead, the generated `.tix` file contains the accumulated coverage of this execution of the program and all previous runs.
I propose to change the semantics to only generate the coverage information for one run of the program.

## Motivation

The `-fhpc` option compiles a program with additional instrumentation to generate coverage information.
When the instrumented executable is run, the collected coverage information is written to a `.tix` file.
Tools such as `hpc` can read the information contained in a `.tix` file and generate useful information, such as
a html file with markup for the covered parts of the program. Other tools can generate information in other
formats which are used in various CI systems.

Surprisingly, an instrumented executable not only automatically writes a `.tix` file at the end of its execution, it also
automatically tries to read a `.tix` file at the beginning of the execution.
If it successfully reads a `.tix` file at the beginning, then the datastructures that the RTS keeps to track coverage
are initialized with the data from the `.tix` file. The user guide describes this behaviour as follows:

> The program may be run multiple times (e.g. with different test data), and the coverage data from the separate runs is accumulated in the .tix file.

If there already is a `.tix` file, but this file was created for a different version of the program, then the program crashes at the time the RTS is started.
Both behaviours are demonstrated in the example section below.

This behaviour is only useful for a completely manual way of interacting with instrumented executables.
It is a nuisance for tool developers who have to manually ensure to always remove
`.tix` files, and for users not familiar with this behaviour.
For example, here is an example on [Stack Overflow](https://stackoverflow.com/questions/28416827/cabal-and-hpc-and-errors-when-running-tests-with-code-coverage) of a user bitten by
the fact that the program crashes if there is a `.tix` file still around. None of the other users was able to correctly identify the underlying problem.
And here is an issue that describes that even `cabal` was not able to correctly implement the logic for enabling code coverage in a project: [Cabal Issue #7384](https://github.com/haskell/cabal/issues/7384)

If a user wants to combine the results of multiple runs, then this behaviour can be implemented in userland. 
The `hpc` binary has the subcommand `hpc combine` which allows to do this on the commandline, and the HPC library  on [Hackage](https://hackage.haskell.org/package/hpc) provides the tools to do that programmatically from within Haskell programs.


## Proposed Change Specification

The GHC user guide currently specifies the effect of `-fhpc` as follows:

> The program may be run multiple times (e.g. with different test data), and the coverage data from the separate runs is accumulated in the .tix file. To reset the coverage data and start again, just remove the .tix file. 

This paragraph will be removed.
Instead, a new RTS flag `--read-tix-file=<yes|no>` will be introduced which guards this behaviour.
The old behaviour will still be available using the flag `--read-tix-file=yes`, but the default behaviour will be to not read the tix file and to always initialize the tix data structures with zeroes.

## Examples

I will give two examples of the current behaviour which illustrate the surprising behaviour
and the failure mode this feature entails.

### Example 1: Coverage of multiple runs is accumulated

```console
> cat Example.hs
module Main where
main = print "hello"
> ghc -fhpc Example.hs
[1 of 2] Compiling Main             ( Example.hs, Example.o )
[2 of 2] Linking Example
> ./Example
"hello"
> cat Example.tix
Tix [ TixModule "Main" 2243069736 3 [1,1,1]]
> ./Example
"hello"
> cat Example.tix
Tix [ TixModule "Main" 2243069736 3 [2,2,2]]
```

### Example 2: Tix File with Different Hash Crashes Program

```console
> cat Example.hs
module Main where
main = print "hello"
> ghc -fhpc Example.hs
[1 of 2] Compiling Main             ( Example.hs, Example.o )
[2 of 2] Linking Example
> ./Example
"hello"
> cat Example.tix
Tix [ TixModule "Main" 2243069736 3 [1,1,1]]
```

When I now change the definition of the program...

```console
> cat Example.hs
module Main where
main = print "world"
>  ghc -fhpc Example.hs
[1 of 2] Compiling Main             ( Example.hs, Example.o ) [Source file changed]
[2 of 2] Linking Example [Objects changed]
> ./Example
in module 'Main'
Hpc failure: module mismatch with .tix/.mix file hash number
(perhaps remove Example.tix file?)
```

The crash occurs during the startup phase of the RTS, when it tries to initialize the
tix data structures with the information from the `.tix` file in the directory, but finds out that the hashes
don't match.

## Effect and Interactions

The behaviour of programs compiled with `-fhpc` becomes more predictable, we get rid
of a failure mode which perplexes users and prevents better integration in tools.

## Costs and Drawbacks

People relying on the aggregation of multiple runs will have to explicitly use the `--read-tix-file=yes` RTS option to get the old behaviour. It is also possible to sum up multiple tix files by hand, using the `hpc combine` command.


## Backward Compatibility

This is a breaking change w.r.t. to the semantics of `-fhpc`.
It will only affect users of `-fhpc` which rely on the described functionality,
namely that the coverage collected in the `.tix` file is accumulated over multiple
program runs. It is very hard to be sure, but my guess is that very few people are currently
using `-fhpc` in this way. They will only notice the change of behaviour in that
their `.tix` files contain less ticks than they expected. The deprecation strategy outlined in the next
section will warn users that this behaviour will change.

## Deprecation Strategy

The old behaviour will be changed over two consecutive releases:

- In the first release, GHC continues with the current behaviour, but
  if it finds an old file (of any kind, even in the wrong format) it emits
  a warning (before attempting to read it) saying

  I am reading in the existing tix file, and will add hpc info from this run
  to the existing data in that file. GHC 9.12 will cease looking for an
  existing tix file. If you positively want to add hpc info to the current
  tix file, use `--read-tix-file=yes`

- In the next release, it stops reading the file


## Alternatives

Do nothing and leave the semantic as it is.
It is also possible to completely remove the tix file parser from the runtime system.
This would lead to a simplification in the RTS codebase, but the old behaviour would no longer
be available.

## Unresolved Questions

None.

## Implementation Plan

I will implement this change: the change is mostly localized to the file [rts/Hpc.c](https://gitlab.haskell.org/ghc/ghc/-/blob/master/rts/Hpc.c) and to the files related to RTS flags.
The startup logic in the function `startupHpc()` will be modified and will take the `--read-tix-file` flag into account.

## Endorsements

None.
