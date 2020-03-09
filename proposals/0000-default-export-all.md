---
author: Chris Smith
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/316).

# Export all symbols from modules with no header

This proposal adds an option to export everything from a module with no module
header.  This is useful for some educational uses of GHC.

## Motivation

The Haskell Report currently says that if a module has no module header, a header
is implied of the form `module Main (main) where`.  That is, the module is named
`Main`, and the only export is the symbol `main`.  This works great for writing
Haskell applications starting from a blank slate.  However, in certain educational
contexts, it is convenient to ask students to write and submit parts of a
pre-existing project.

In those cases, the person designing the educational experience has several
choices, all of them imperfect.

1. Instructors could ask students to edit a template that has been provided to
   them. This requires that students ignore a large amount of boilerplate, and
   avoid accidentally modifying the wrong parts of the file. It forces
   instructors who test their students' code to manually verify that the
   student has not modified the test harness to always succeed, or something
   of the sort.
2. Instructors could implement `main` in a different module, and ask students to
   add a module header without really understanding why.  This boilerplate is
   similar to the well known "first day of Java" problem, except that students
   must blindly type `module Main where`, instead of
   `public class Main { public static void main(String[] args) { } }`.
3. Instructors could modify the code submitted by the student to add a module
   header themselves.  However, it's actually very tricky to edit a Haskell AST,
   rewrite the source, compile it, and match up resulting error messages back to
   their locations in the original source code.  It would be ideal if the student
   code could be used as is.

All of this is an unfortunate consequence of the Haskell Report's apparently
arbitrary decision that a default module header will only export `main`.  There
is no harm in the normal case to exporting everything (that, is, making the
implied module header just `module Main where` without the export list).

## Proposed Change Specification

A new language extension will be defined called `DefaultExportAll`.  When this is
enabled and GHC compiles a module that does *not* contain an explicit module header,
the default module header will become `module Main where` rather than
`module Main (main) where` as in the Haskell Report.

If the module has an explicit module header, the extension has no effect.

## Examples

The proposal is so trivial that I don't see how to write an example that
would make it clearer.

## Effect and Interactions

I am aware of no unwanted interactions.

## Costs and Drawbacks

The implementation itself should be approximately a single-digit number of
lines of code.  The documentation and test cases will be slightly larger.
The greatest cost is probably just keeping track of yet another language
extension name.

## Alternatives

One interesting alternative is to not add a language extension, and instead
just have GHC diverge from the Haskell Report.  Obviously, this is not ideal.
However, I think a reasonable argument can be made for it.

First of all, the only time this can have an effect is when something else
imports `Main`.  Since `Main` is the standard entry point for a Haskell
program, this is unusual to say the least.  In particular, it can only happen
in these cases:

1. There is a circular dependency involving `Main`.
2. The project was compiled using `-main-is` to change the standard entry point.

The first seems unlikely, but the second is known to happen.  A typical case
involves something like this:

``` haskell
-- Main.hs
main = putStrLn "Doing work..."
```

``` haskell
-- Test.hs
module Test where

import Main
import System.Environment

testMain = do
  putStrLn "Setting up the test..."
  withArgs ["test", "args"] main
  putStrLn "Check assertions"
```

The test can be run with `-main-is Test.testMain`.

This pattern is used by some people who notice when it's broken.  For instance, see https://gitlab.haskell.org/ghc/ghc/issues/15702.  Neither the examples here nor there
would be broken outright by this change, but they would become vulnerable to name
collisions after this change is made.

So it's possible this might break some code, but we cannot find any examples of code
that would actually break.  If it did break, then there are two easy fixes: either
add an explicit module header, or add an import list to the import.  Either one will
avoid the breakage, and is backward compatible.

One could imagine adding a warning for cases where this would happen.  This is not
straight-forward, since it means a compiled module needs to remember whether it had
an explicit module header or not.  This seems like overkill for something that I
would guess is going to break a single-digit (where that digit might be 0) number
of people's code.

## Unresolved Questions

None.

## Implementation Plan

The author (cdsmith) volunteers to implement the change, if accepted.

## Endorsements

None
