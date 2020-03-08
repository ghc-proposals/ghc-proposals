---
author: Chris Smith
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/316).

# Default Export All

This proposal adds an option to export everything from a module with no module
header.  This is useful for some educational uses of GHC.

## Motivation

The Haskell Report current states that if a module has no module header, it is
assumed to be called `Main`, and to export a symbol called `main`.  This works
great for writing Haskell applications starting from a blank slate.  However, in
certain educational contexts, it is convenient to ask students to write and
submit parts of a pre-existing project.

In those cases, the person designing the educational experience has several
choices, all of them imperfect.

1. Instructors could ask students to edit a template that has been provided to
   them. This requires that students ignore a large amount of boilerplate, and
   avoid accidentally modifying the wrong parts of the file. It forces
   instructors who test their students' code to manually verify that the
   student has not modified the test harness to always succeed, or something
   of the sort.
2. Instructors could implement `main` in a different module, and ask students to
   submit their own module with a module header that explicitly lists their
   exports.  This works, but requires extra work by students to name their module
   correctly, remember all the export names, avoid typos in extra code, etc.
   Students are sometimes not prepared to appreciate the need for this extra code.
   (This is akin to the well known "first day of Java" problem, except that
   students must blindly type `module Main (foo, bar, baz) where` instead of
   `public class Main { public static void main(String[] args) { } }`.)
3. Instructors could modify the code submitted by the student to add a module
   header themselves.  However, it's actually very tricky to edit a Haskell AST,
   rewrite the source, compile it, and match up resulting error messages back to
   their locations in the original source code.  It would be ideal if the student
   code could be used as is.

All of this is an unfortunate consequence of the Haskell Report's apparently
arbitrary decision that a default module header will only export `main`.  There
is no harm in the normal case to exporting everything (that, is, making the
implied module header just `module Main where` instead, without the export list).

## Proposed Change Specification

A new language extension will be defined called `DefaultExportAll`.  When this is
enabled and GHC compiles a module that does *not* contain an explicit module header,
the default module header will become `module Main where` rather than
`module Main (main) where` as it is in the Haskell Report.

If the module has an explicit module header, there is no change in behavior.

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

See the motivation section for non-GHC alternatives for solving the identified
use case.

Implementation alternatives:

1. Don't add a language extension; just export everything all the time.  Even
   if this change were made blindly, this would have minimal effect, because
   it's rare to import a `Main` module.  However, it still happens sometimes
   in test code (usually when using `-main-is` to change the entry point to a
   new one that imports and calls the old one).  Hence the decision to add a
   language extension and avoid the risk.
2. If the decision were made to migrate the language in this direction in the
   long term, one could imagine adding a warning to encourage fixing the rare
   cases where it might break something.  This is not straight-forward, and I
   am not proposing it.

## Unresolved Questions

None.

## Implementation Plan

The author (cdsmith) volunteers to implement the change, if accepted.

## Endorsements

None
