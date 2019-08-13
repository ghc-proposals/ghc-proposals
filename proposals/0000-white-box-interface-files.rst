White-box interface files
=========================

.. author:: Michael Peyton Jones
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/262>`_.
.. sectnum::
.. contents::

GHC is a tremendous resource for language designers to work with. GHC is very
friendly, and allows users to reuse many parts of it, especially with the advent
of compiler plugins. But support is not so good for users who want *white-box*
access to compiled modules.

This proposal suggests a new kind of interface file to remedy that issue. It is
a resurrection and evolution of Edward Yang’s `"fat interface files" proposal
<https://gitlab.haskell.org/ghc/ghc/issues/10871>`_.

Motivation
----------

GHC Haskell programs are typically put together by compiling together some
number of modules. These can be local source modules, or they can be separately
compiled.

Separately compiled modules look rather different to source modules. We have two
files:

1. An object file, which provides compiled code for the target platform that can
   be linked against.
2. An interface file, which provides the information that the typechecker needs
   to typecheck against the compiled module.

   - Interface files can contain other things, in particular we will have more
     to say about unfoldings later.

This is all well and good so long as those are the only two things that you want
to do with a separately compiled module: link against it and typecheck against
it.

However, sometimes we want to do more with our dependencies than this. A
particularly common desire is to see the *Core* for a compiled module. This
allows us to do *white-box compilation*.

White-box compilation
^^^^^^^^^^^^^^^^^^^^^

A white-box compiler relies on seeing more information about compiled modules
than is typically provided. [1]_

.. [1] Some people talk about “whole program” compilation or optimization. I
   haven’t used this term because I find it somewhat ambiguous: if you do
   link-time optimization when
   linking your object files, are you now using a whole-program compiler? What
   about if you do static linking? "White-box" indicates the key thing, which is
   that we want to see into parts of the compilation process that are normally
   opaque. This is still a matter of degree: after all, even "black-box" normal
   compilation has interface files.

Here are some examples which I claim do some kind of white-box compilation. I
have attempted to give a brief summary of some very complicated projects based
on conversations I have had: any errors are my own.

CLaSH
"""""

CLaSH compiles Haskell code into a low-level hardware description language. It
works from Core.

CLaSH currently gets this information from unfoldings, compiling dependencies
``-fexpose-all-unfoldings`` to ensure they are present.

GRIN
""""

GRIN is an experimental backend for GHC, which converts STG into its own IR. It
works from STG.

GRIN currently adds a GHC compiler plugin, which generates its own, new,
interface files during compilation of dependencies. They patch Cabal to ensure
that their interface files are installed. Then at the end they can load the GRIN
interface files for dependencies and assemble the final result.

Plutus
""""""

The Plutus compiler compiles Haskell into Plutus Core, a small System F-derived
target language. This is done by a compiler plugin, which compiles targeted
subsets of the Haskell program and embeds the compiled programs inside the main
Haskell program as data, where they can be used at runtime. It works from Core.

The Plutus compiler currently gets this information from unfoldings.
Dependencies must explicitly mark functions which are to be used as ``INLINABLE``
to ensure that unfoldings are present.

Backpack-that-was
"""""""""""""""""

In the original issue for fat interface files, Edward Yang describes a system
whereby indefinite units could be separately compiled and installed. But to
actually instantiate the indefinite unit we need to do some work, and this
requires more information than is in interface files, indeed it requires
recompiling the indefinite unit.

Backpack avoided this problem by becoming more interdependent with Cabal, so
that particular instantiations of units could be requested and Cabal would
handle rebuilding the unit and instantiating it.

Other use cases
^^^^^^^^^^^^^^^

Dumping splices
"""""""""""""""

Obsidian Systems have `patched
<https://gitlab.haskell.org/obsidiansystems/ghc/commits/wip/abrar/splices-8.6.5>`_
GHC so that dumped splices (as from ``-ddump-splices``) can be loaded in when
compiling to a non-native platform.

This technique could also be used to "snapshot" compilation after TH has been
eliminated. This would allow build systems to be more incremental.

If there were support for compiling modules to Core and loading them, then we
would avoid having to write additional code for this use case.


Stage -1 Template Haskell
"""""""""""""""""""""""""

The `Staged TH proposal
<https://github.com/ghc-proposals/ghc-proposals/pull/243>`_ would benefit from
the ability to compile dependencies to Core and interpret them directly.
Interpreting core is necessary for TH to be usable in GHC, due to ABI changes
between stage 0 and stage 1.

White-box interface files
^^^^^^^^^^^^^^^^^^^^^^^^^

I think Edward and GRIN have the right idea here: if we want to preserve
information about separately compiled dependencies then we should add a new kind
of artifact that contains it. This is an approach we have taken before:
interface files and profiling outputs are prominent examples.

We call this new kind of interface file a "white-box interface file", and it
will have extension ``.hi-wb``. (Name and extension completely up for grabs!)

What should go in a white-box interface file? I propose the following slogan:
    
    If a module has been loaded from a white-box interface file it should be as
    if that module had been compiled from source.

This avoids us trying to pick some set of information that will suffice in all
cases. Instead, we preserve *everything*.

I suspect this slogan will be unattainable in practice. However I think it is
better to have the slogan be true with some exceptions than to have a grab bag
of properties without a unifying goal.

Proposed Change Specification
-----------------------------

GHC
^^^

GHC learns the ``-fwrite-white-box-interface-files`` flag. This will cause it to
write out a white-box interface file for each file that it compiles, next to the
file.

The exact content of a white-box interface file file is an implementation detail
which we will not specify here, but it should correspond more-or-less to a
serialization of a ``ModGuts``.

There is no expectation that the file format is stable, or that tools other than
a GHC of the same version which created the file should be able to read it.

If a white-box interface file is present, GHC will still load the module as a
``ModIface`` during compilation. This ensures that using white-box interface files
does not influence normal Haskell compilation in any way.

The GHC API will provide a function that loads a ``Module`` as a ``ModGuts`` from a
white-box interface file, if one is present. This can be used by a client of the
GHC API, such as a plugin.

Cabal
^^^^^

Cabal must learn to install white-box interface files if they are present. (But
see `Fat .hi files`_ for an alternative that doesn't require
altering Cabal.)

Cabal CLI support can come later after this feature has proven itself to be
useful. In the mean time, users can use
``--ghc-options=-fwrite-white-box-interface-files`` to compile their dependencies
with white-box interface files.

Examples
--------

I will give an example using the Plutus compiler, since that is the one that I
am most familiar with.

At the moment, when the Plutus compiler sees a ``Name``, it checks to see if it
has an unfolding. If it does, it creates a new binding in its own state and then
compiles the right-hand side of the unfolding (the details are not important
here).

With white-box interface files, it would instead look up the module that the
``Name`` comes from, and request to load it as a ``ModGuts`` (probably this should
be cached, either inside GHC or in the client). It would then look through the
``ModGuts`` to find where the binding for the ``Name`` occurs, and extract its
right-hand side. At that point it would proceed as before.

If a white-box interface file for the module in question is missing, it would
terminate and give an error to the user instructing them to recompile their
dependencies with white-box interface files enabled.

Effect and Interactions
-----------------------

This feature should be entirely
orthogonal to other language features. It does not affect Haskell the language,
and it does not affect the behaviour of normal Haskell compilation in any way.

Costs and Drawbacks
-------------------

This feature would add a moderate maintenance and testing burden.

- ``ModGuts`` (or whatever subset of it we decide to work with) must remain
  serializable in the face of future changes.
- White-box interface files are unlikely to be used by any of the main workflows
  that the compiler tests, so we will need to add significant additional testing
  to ensure that the feature continues to work.

  - Had Backpack-that-was been implemented this would be less true, but it
    seems unlikely that it would be worth redesigning at this stage.
  - But see `Should we support compilation from white-box interface files?`_ for
    a feature that would make testing easier.

Full compliance with the slogan may be excessively onerous. However, I don’t
think it is a problem if there are minor qualifications. For example, it would
probably be fine to have less good source provenance information in modules
loaded from white-box interface files.

Alternatives
------------

Unfoldings
^^^^^^^^^^

GHC does provide some information about the Core for bindings in separately
compiled modules. "Unfoldings" are right-hand-sides of bindings, which are
included in interface files to allow them to be inlined when compiling a
dependent module.

Unfoldings, when present, provide a way to get access to the Core for a binding.
However, they are not intended for this purpose, and this has a number of
knock-on effects:

- There are very few guarantees about whether unfoldings will be present and for
  what.

  - `Issue 16615 <https://gitlab.haskell.org/ghc/ghc/issues/16615>`_ revealed a case where
    one of two mutually-recursive ``INLINABLE`` functions did not have an
    unfolding at the start of the compilation pipeline. In the end, we may be
    unable to change this if it affects optimization poorly.

- Changing which unfoldings are present has serious effects on optimization.
  This means:

  - Even fixing something that looks like a "bug" in unfoldings might be
    undesirable if it affects optimization adversely.
  - Adding more unfoldings to support white-box compilation will influence the
    optimization done in the normal case as well.

- There are no guarantees that unfoldings will continue to be suitable for
  white-box compilation.

  - The GHC developers could reasonably change the behaviour of unfoldings in
    pursuit of better optimization such that other use cases were broken.
  - The GHC developers should not be restricted from making such changes
    because people are (ab)using unfoldings in an unusual way.

Overall unfoldings are one of the most practical approaches at the moment, but I
think they are fundamentally unsuitable in the long run.

HIE files
^^^^^^^^^

HIE files have recently been added to GHC. They aim at providing additional
information about compiled modules to developer tooling. Amongst other things
they contain an annotated copy of the AST and the source.

At first glance, it might look like there is some overlap between white-box
interface files and HIE files. However, using HIE files instead of white-box
interface files would face the same conceptual problem as using unfoldings: the
purpose of HIE files is to support developer tooling, and so trying to use them
for white-box access is likely to produce bad outcomes for either or both use
cases.

To give a couple of examples:

- Because HIE files aim to be consumed by external developer tooling,
  it is desirable for them to have a stable binary format, whereas this would be
  unnecessary and inconvenient for white-box interface files.
- The HIE AST is simplified to make life easier for consumers. This would be at
  odds with the need to have it be fully faithful if they were to fill the role
  of white-box interface files.

Fat .hi files
^^^^^^^^^^^^^

Adding a new file is easy in some ways, but harder
in others. Tools such as Cabal have to know about them and do the right thing,
there is another file extension for people to worry about. In other words: a
large number of small costs.

We could instead make ``.hi`` files "fat" by restructuring ``.hi`` files so that
they have an arbitrary series of named sections. Tools could extract the
sections they care about and deserialize them, without necessarily even knowing
how to read the other sections. [2]_

This would allow us to extend interface files arbitrarily, with white-box data
being one such case.

.. [2] Thanks to Moritz Angermann for this suggestion.

Write a proper backend
^^^^^^^^^^^^^^^^^^^^^^

If one is writing a compiler for Haskell code, we already *have* an output that
is supposed to contain all the generated code that the target system needs: the
object files. Why not write a proper GHC backend doing the whole-program work
at the end in whatever corresponds to the "linking" phase?

There are a few reasons why this is not universally a good way forward.

- Writing a full backend is hard work! There is a lot to do, and there are a lot
  of assumptions about what backends are like, and that they will be producing
  something roughly like C object files.
- Some users, such as Plutus or Backpack-that-was, may want to work alongside
  normal Haskell compilation rather than replacing it.

However, this might be a good route in the long run for something like GRIN.

Recompile modules for white-box access
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

An alternative approach is to fully re-compile modules if we want white-box
access to them.

In the original issue, Simon Marlow `says
<https://gitlab.haskell.org/ghc/ghc/issues/10871#note_108500>`_ "Isn't this scheme just a very
elaborate way to avoid re-typechecking things in some cases?".

I would answer that it is not -- fully recompiling modules is actually very
difficult. There is a *lot* of work that happens in the frontend, and many flags
that would need to be recorded. The modules might even have been compiled on
another machine.

Many of these difficulties are solvable with sufficient work: e.g. to handle
pre-processing we can intercept and save the post-processed source. But overall
they amount to a lot (indeed, an unclear amount of) work.

This is analogous to the benefit that we get from separate compilation. An
interface file and an object file are sufficient information to work with a
module, conveniently packaged up. This tremendously simplifies installing,
packaging, and working with such modules.

We want the same convenient workflow for white-box module access.

Core-only interface files
^^^^^^^^^^^^^^^^^^^^^^^^^

Couldn't we get by with just the Core? Why do we need to serialize the *whole*
``ModGuts``? I have two answers to this:

- Clients such as GRIN want to work from STG.

  - This means that they not only need to load the Core for a module, they
    need to run the compiler pipeline on that module until the point that it
    produces STG.
  - Realistically, this will be much more likely to work if we have exactly a
    ``ModGuts``.

- Adherence to the slogan gives us future-proofing.

  - If a white-box module really looks *just like* a source module, then we
    can be confident that *whatever* people want to get from such a module,
    they will be able to do it.

Unresolved Questions
--------------------

Would this actually be used by other users?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
I am fairly sure that we would use this feature in Plutus, but part of the goal
of this proposal was to create something that would be useful for multiple
users. That increases the value we get from this work, and also increases the
likelihood that it continues to be used and maintained in the future.

Should we support compilation from white-box interface files?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This was supported in Edward Yang’s original patch, which allowed one-shot
compilation to take a fat interface file, and had ``ghc --make`` support for
building from fat interface files.

There are at least two reasons to want this:

- It gives us a lot of easy tests: for every test in the GHC test suite, can we
  compile it to a white-box interface file, resume compilation, and check if it
  succeeds. In this sense we can "prove" we are abiding by the slogan.
- The splice dumping use-case depends on this.

When compiling with white-box interface files, should they be mandatory for dependencies?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Most of the listed use-cases will fail if their dependencies have not all been
compiled with white-box interface files. We have two options here:

1. Allow white-box interface files to be optional on a per-package level, and
   let the tool fail if it does not have what it needs (this proposal, currently).
2. Require that dependencies have been built with white-box interface files.

My inclination is to stick with the first approach unless there is a clear need
for the second one.

Are there complications with foreign dependencies?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

I don’t know, but they always seem to be difficult.

Are there performance implications?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Adding data to the ``.hi`` files (or other files) will require
serialization overhead. Can this be made fast? Is the
performance hit acceptable so long as it only affects "non-standard" users?

Implementation Plan
-------------------
Edward Yang wrote a patch for "fat interface files" that got quite far (`Github
<https://github.com/ezyang/ghc/tree/ghc-fat-interface>`_, `Phabricator
<https://phabricator.haskell.org/D1318>`_). The plan would be to revive that.

The work would be done by IOHK with assistance from Well-Typed.
