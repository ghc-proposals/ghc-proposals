Stage Hygiene for Template Haskell
==============

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/243>`_.
.. sectnum::
.. contents::

Template Haskell currently doesn't work well with cross compilation.
This is a big nuisance for anyone making phone or browser software.
To fix this, enforce stronger separation between the stages.

Motivation
------------

Hacking TH today
~~~~~~~~~~~~~~~~

In the olden days, one couldn't use Template Haskell with cross compilation at all.
This is bad in itself, but also bad from an ecosystem perspective.
Haskell has an excellent culture of code reuse, but that means that arbitrary software is very likely to depend on Template Haskell.
That lack of being able to write idiomatic Haskell code with popular libraries for all platform is as big of a loss as not being able to Template Haskell itself.
The risk of library fragmentation between between those writing code for weird platforms and those not is also bad for everyone.

The external interpreter made it at least possible, relatively automatically, but doesn't work if you explicitly want side effects to be run on the platform doing the compiling (build platform).
Also, while same-OS cross can sometimes be fairly lightweight
— e.g. by having QEMU translate syscalls so the native kernel can be used —
different-OS requires harder to provision virtual machines or real devices.

Another alternative is dumping and loading splices.
This is where one first builds natively (host ≔ build) so TH can be evaluated, dumps the evaluated splices, and the builds cross (for the original host platform) by splicing in those dumped pre-evaluated splices rather than evaluating anything afresh.
This became easier with `this patch <https://github.com/reflex-frp/reflex-platform/blob/master/splices-load-save.patch>`_.
Still, this requires building every package twice, since we redo the entire compilation on both platforms, and worse doesn't work if a top-level splice is target specific.
For example, imagine some code like
::
  #ifdef ios_HOST_OS
  data SomeIosFfiType
  $(iosBoilerplateHelper ''SomeIosFfiType)
  #endif
If the splice is within the ``ifdef``, it won't be dumped.
When we compile to dump splices, compilation occurs on the native platform, and so the splice will be removed at preprocessing time before dumping.
And deleting the CPP is no quick fix — if it is outside the ``ifdef``, the native one won't build!
If it is outside the ``ifdef``, then when we compile to dump splices we will actually fail, since we're now trying to build ios-specific code on the wrong platform.

What we need instead is a way to say different code runs on different platforms.

Referencing platforms
~~~~~~~~~~~~~~~~~~~~~

First, an aside about naming platforms.
Long ago, the autoconf invented the terms "build" platform, "host" platform, and "target" platform:

- the build platform of some code is where it is built.
- the host platform of some code is where it runs.
- the target platform of some code is the host platform of code produced by this code.

Yes, the names are vague, and yes, the naming of one inductive step (target) is awkward, but the beauty of these names is they work for native and cross compilation alike.
Rather than thinking about concrete platforms and where they are used, they turn the problem around by thinking about the holes where concrete platforms go in.
These "abstract" platforms are thus parameters.
And whereas most designs are native only, and then hurridly retrofitted for cross, these names naturally lead to not assuming any of the 3 platforms are the same.
In other words, native compilation is the special case, not cross.

Explicit Staging
~~~~~~~~~~~~~~~~

With that in mind, what we are looking for is:

- Splices alone run on the build platform.
- Normal code, as usual, runs on the host platform.
- Quoted code runs on the target platform.

This solves all the problems of the first section:

- No need to emulate any other platforms.
  Recall TH-induced evaluation only happens within top-level splices; splices within brackets just build bigger expressions.
  That means only TH only induces build platform splicing, which is native by defintion!

- No need to build everything twice.
  Just what is needed in each phase is built, and just when it's needed.

- No risk of CPPing away the splice, as with dumping and loading, because we aren't faking it with build == host native builds.
  ``ios_HOST_OS`` is independent of any ``*_BUILD_OS`` macro.

To do this, we need to cleanly separate the stages induced by quoting and splicing.
This is not a new idea for programming languages in general.
Racket (and probably some schemes) do this.
The work-in-progress (?) `OCaml macro system <https://github.com/ocamllabs/ocaml-macros>`_ does this.
It has even been informally proposed for Haskell by @ezyang in `<http://blog.ezyang.com/2016/07/what-template-haskell-gets-wrong-and-racket-gets-right/>`_.

Enforcing that separation means restricting programs we currently allow,
by assigning bindings to stages and restricting what kinds of references between stages are allowed.
Least surprisingly, normal bindings, and normal imports in the module cannot be used in splices or quotes.
But there are other constructs that more surprisingly tangle stages too.
Typed Templated Haskell is one.
First of all, there is name leakage.
::
  [|| ... :: IosOnlyType ||] :: Q (TExp IosOnlyType)
This can't work unless we are building *on* and *for* iOS.
Otherwise the ``IosOnlyType`` will be out of scope in one of its two usage sites.
If we aren't compiling for iOS (iOS is not host OS), then ``IosOnlyType`` is not in scope in the quote.
If we aren't compiling on iOS (iOS is not the build OS), the ``IosOnlyType`` is not in scope as the argument for ``TExp``.
The latter one is the show-stopper, presumably we are compiling for ``iOS`` if we want to do this.
Typed TH in affect assumes that any host type can be mapped back to a build type for sake of the phantom param.
As shown, this is not always the the case.

But even if we work around that, there's also will be semantic leakage.
In the near future there would be
::
  AppE <$> [|| ... :: foreach (x :: Int) -> F x ||] <*> [|| 2 ^ 36 :: Int ||] :: Q (TExp (F ???))
How do we type the whole expression, or ``AppE`` in particular?
``F (2 ^ 36)``?
But say the platform the compiler runs on (build platform) has a 32-bit ``Int``, while the platform the spliced code runs on (host platform) has a 64-bit ``Int``?
The code when eventually spliced will have a type of ``F (2 ^ 36)``, but the quote has a type of ``TExp (F 0)``.
This ruins the guarantees of typed Template Haskell.
Even today with CPP'd type families:
::
  #if mingw_HOST_OS
  type instance F Bool = []
  #else
  type instance F Bool = Tree
  #endif
Say we are compiling the following from linux to mingw.
::
   AppE <$> [|| ... :: forall a. a -> F a ||] <*> [|| True ||] :: Q (TExp (F Bool))
We'll have ``F Bool = []`` when the code is eventually spliced, but ``TExp (F Bool) = TExp Tree`` for the quote itself.

Finally, ``Lift`` is problematic for similar reasons.
Consider
::
  lift (linuxOnlyValue)
This will evaluate through something like
::
  lift (LinuxOnlyConstructor arg0 ...argn)
All good so far.
But that in turn evaluates as
::
  [| LinuxOnlyConstructor $(lift arg0) ...$(lift argn) |]
If we aren't compiling to ``Linux``, ``LinuxOnlyConstructor`` will be out of scope.
The overflowing issue doesn't break type safety, but is still nastily non-confluent.
::
  lift (2 ^ 25 >= 0) /= [| $(lift $ 2 ^ 25) >= 0 |]
  ==>
  [| 0 >= 0 {- on 32-bit build platform -} |] /= [| 1 >= 0 {- on 64-bit host platform -} |]
There's no non-determinism since ``lift`` doesn't automatically commute like that,
but the lack of bijectivity is still a foot-gun.

I would love to, instead of outright banning Typed Template Haskell and ``Lift``, come up with a flexible way to associate types and terms between stages.
For ``Lift``, at a minimum, we just need to map *values* preserving type, though bijectivity is still nice, at least as something one can but manually opts out of.
For typed Template Haskell, I think we additionally need to map type *expressions* such that evaluation commutes with the mapping.
Adding language support for such mappings is lots of extra work—borderline research—for a proposal which already is no small task.
I therefore think banning these constructs for now to start solving the problems people have with cross compilation as soon as possible is prudent.
Because this proposal is breaking change vs Template Haskell today, a variant extension is used anyways, so no program breaks.
Instead, users are just temporarily presented with a choice to either support cross compilation or have ``Lift`` and typed TH.

As a final side benefit, now that Template Haskell will be defined and implemented in terms of stages, we can relax ``-XTemplateHaskellQuotes``.
For example, the following current prohibited:
::
  [| $(x) |]
But actually imposes no problems.
This is the same as
::
  x
and likewise
::
  [| f $(x) b |]
and is the same as
::
  AppE <$> [| f |] <*> x <*>  [| b |]
Since the splices all can be desugared away without the evaluation of user-written code, there is no reason to penalize them.

Macro systems have often been judged by their (lack of) hygiene.
Macros that delay all name resolution post splicing are deemed unhygienic.
It has been argued in [InferringScope]_ that hygiene just is alpha-equivalence from a better vantage point,
a point which was obscured by the early Scheme macro systems (and TH's) use of renaming and gensym in lieu of a more principled formalism.
It is my hope that a lack of stage separation comes to be viewed as unhygienic in the same way.
It should be immaterial whether build time "base" has any identifiers in common with the run-time "base", and nothing should be improperly captured or dangling either way.

Proposed Change Specification
------------

GHC
~~~~~~~~~~~~

#. Let there be a notion of stages assigned to the integers.
   All existing rules outside of TH on binding/name resolution are retaken to act independently per stage.
   (i.e. identifiers in stage *n* resolve to bindings in stage *n*, all syntax in the rule is parameterized with the stage.)
   The top level is always stage 0.
   A consequence of the above is all non-TH syntax in is also stage 0.

#. Redefine quoting and splicing as acting on adjacent stages. Specifically, quoting quotes code from the next stage:
   ::
     G ⊢(n + 1) syntax
     -----------------------
     G ⊢(n) [| syntax |]
   and splicing splices code from the previous stage:
   ::
     G ⊢(n - 1) syntax
     -----------------------
     G ⊢(n) $(syntax)

   The existing side conditions, which restrict nested quotes and splices (i.e. stages outside of -1, 0, and 1) remain in place, but are ripe for removal in https://github.com/ghc-proposals/ghc-proposals/pulls/204.

#. Add new syntax for stage-offset imports and bindings:
   ::
     <impdecl> ::= $import <integer-literal> <<existing syntax>>
   This means import a module in stage *n* instead of stage 0 as per normal.
   ::
     <decl> ::= $let <integer-literal> <<existing syntax>> = <<existing syntax>>
   In both case the ``$`` must not be followed by whitespace, both to avoid conflicts with other syntax and to be consistent with splices.
   The means bind identifiers in stage *n* instead of stage 0 as per normal.
   Module exports however are restricted to stage 0.

#. The current "stage restriction" on splices using bindings from the current module is abolished.
   Any stage n + 1 binding in a stage n splice is fair game.

#. Relax ``-XTemplateHaskellQuotes`` to instead allow Template Haskell constructs, but restrict their usage so all syntax is in stages >= 0.

#. Introduce ``-XTemplateStagePersistence``.
   Which is implied by ``-XTemplateHaskellQuotes`` (and thus plain ``-XTemplateHaskell``) for backwards compatibility.
   It allows the current behavior where we blur the distinction between stages.
   In particular, with `TemplateStagePersistence` enabled:

   - Stage 0 identifiers bound in another module can be used in stage 1 (quotes) and stage -1 (splices).
   - Stage 0 identifiers bound anywhere can be used in stage 1, and are automatically.
   - Typed template haskell is allowed.
   - The ``Lift`` type class and all its associated definitions are made available.

   These are always permitted today.
   But with ``-XNoTemplateStagePersistence``, overriding the default, all of those are *disabled*.

#. Extend the command line [TODO bikeshed!!] with a way to specify per-stage package dependencies and the like.
   If/when GHC becomes multi-target, by default stages >= 0 take GHC's target platform / the packages host platform (where compiled code runs), while stages < 0 take GHC's host platform / the packages build platform (where GHC runs).
   But, the emitted platform can still be specified per-stage like the other flags.
   This is needed when building TH functions to be used from cross compiled code.

#. When importing modules/packages, after applying the import offset ensure that the platforms match.
   Note that while each module only has bindings in its own stage 0, those bindings can contain quotes from stages greater than 0.
   All such quoted platforms need to match.

#. Just as GHC defines ``*_HOST_OS`` and similar CPP identifiers today, it would define ``*_BUILD_*`` ones if you have any stage -1 package imports, and ``*_BUILD_*`` if you have any stage 1 package imports.

Cabal
~~~~~~~~~~~~

#. Extend the ``build-depends`` syntax with a stage integer offset parameter.
   N.B ``build-tool-depends`` can be thought of as a stage -1 executable dependencies list.
   `<https://github.com/haskell/cabal/issues/5411>`_ asks for a ``run-tool-depends`` which would be nothing but a stage 0 executable depends.
   ``setup-depends`` can also be thought of as a stage -1 executable dependencies list.

#. Likewise extend ``other-modules`` with a stage integer offset parameter, to support intra-package ``$import``.
   Leave ``exposed-modules`` as is, however. Libraries should only expose stage 0 modules, just as modules only expose stage 0 definitions.
   Restrict the ``other-modules`` offset to be <= 0, as positive stage code is either pointless or would escape via references from quotes causing build system havoc.
   Unexposed negative stage modules need not be installed at all, as there is no way for stage 0 to reference them (splices eliminate references).

#. Connect today's "qualified goals" to stages.
   [TODO exact formalism, is it in scope?]
   Some properties that must be true in the brave new world:

   - Executable dependencies are cross-stage and private, they are maximally qualified in that they introduce the fewest cross-stage constraints.

   - Regular library dependencies are public and same stage.
     They carry their transitive closure in the form of mandatory unification constraints.

   - Cross-stage library dependencies are still public.
     The stages can be independent since cross-stage types don't ever unify, but *within* each stage everything works as usual.
     Compositions of cross-stage dependencies can result in same-stage dependencies, and their public closure unification "burdens" will combine.

   - Intra-package dependencies regardless of stage must resolve within the same version of the package.
     This is already the case so the setup component knows what library it's building.
     Now it is also the case so the TH library knows what types are used in its quotes.
     These only arise from immediate dependencies.
     The unification obligation is propagated like all the others, but there's no magic beyond that.
     When the same package is transitively visible in two stages, there is no same-version constraint across the two stages that arises out of thin air.

Effect and Interactions
-----------------------

Here is an example of many of the features used together, rewriting the code from the motivation.
Hypothetical ``ios-th`` package:
::
  {-# LANGUAGE TemplateHaskell #-}
  {-# LANGUAGE NoTemplateStagePersistence #-}
  module Ios.Macros where

  #ifndef ios_TARGET_OS
  # error Module shouldn't be built. Fix Cabal file!
  #endif

  import Language.Haskell.TH
  $import 1 Ios.Types (Foo(..))

  iosBoilerplateHelper :: Name -> Q Expr
  iosBoilerplateHelper name = ... [| ... :: Foo |] ...
end user code:
::
  {-# LANGUAGE TemplateHaskell #-}
  {-# LANGUAGE NoTemplateStagePersistence #-}
  module MyApp.Ios where

  #ifndef ios_HOST_OS
  # error Module shouldn't be built. Fix Cabal file!
  #endif

  import Ios.Types
  $import -1 Ios.Macros

  data SomeIosFfiType

  $let -1 unneededBinding = iosBoilerplateHelper ''SomeIosFfiType

  $(unneededBinding)

A few misc implementation notes:

Bindings interleave stages
  Note that ``$let`` can appear outside the top-level, including in contexts where a variable of later stage is bound.
  At first glance, binding a compile-time variable within a run-time variable might seem like a staging violation:
  ::
    f x = $huh
       where foo = ...
             $let -1 huh ... = ... [| x |] ... [| foo |] ...
  But remember that later stage syntax can just be used in quotes; it is inert and cannot be evaluated.
  ``huh`` is trivially lifted outside of ``f`` since it captures the syntactic ``x`` which is static at compile-time.
  Nothing passed into ``f`` at any call site is available to ``huh``.

Forward references across splices
   The intra-module staging restriction is gone, but that's separate from the prohibition on referencing bindings.
   It just avoids the need to topologically sort splices based on references from the quotations inside them, or break cycles à la ``*.hs-boot``.
   Nevertheless, implementing that is not trivial so it is good to decouple relaxing the restriction from this already-large proposal.
   Hopefully a future proposal will tackle this.

Speeding up builds
  Modules and libraries are relative in that their exported stage 0 may not be imported at stage 0.
  There is no notion of a global "true" stage 0.
  This is good in that we can share build artifacts without breaking abstractions.
  For example, in the common native case, a library that needs another library in stage 0 and stage -1 can load the *same* build of the library in both of those stages.
  By virtue of the explicit stage attached to the import, the definitions do not unify even though the underlying build is the same.
  This can be compared to repeated abstract interfaces in backpack being instantiated with the same concrete module.

  In the cross case, there is no getting around needing separate builds for the different platform used in each stage, but there are still performance improvements.
  As said in the motivation, we only need what is needed when it is needed, versus everything twice with splice dumping and loading.
  This reduces the size and improves the parallelism of the build plan.
  More subtly, and perhaps more importantly, are benefits with rebuilds during development.
  Let's say because of this proposal, splices (stage -1 code) are now used in a core library like `containers`.
  Let's say also that the stage -1 code depends on code which depends transitively on `containers`.
  Because of stage isolation, while developing `containers` we are free to use the old version of containers in the -1 stage.
  That means we don't have to rebuild all our dependencies each bug cycle.
  This is comparable to today's trick of renaming `containers` to `kontainers` so we can tune it and re-benchmark without rebuilding criterion and friends.

  There are *still* more tricks we can do for overall build size and parallelism.
  Stage 1 code doesn't need to be evaluated, just composed correctly.
  As such, we just need the interface of imports, and don't care about the definitions behind those declarations.
  That means we just need to build as far as today's `hi` files to resolve those imports.
  Stage -1 code does need to be run, but still not compiled in the final binary since it cannot be exported.
  To satisfy that, we just `hi` files with `-fexpose-all-unfoldings` file, along with a `"naive" Core interpreter`_ which can evaluate those unfoldings.
  Splices are typically small and numerous, so it seems likely that the lower latency of starting the interpreter is worth the cost of slower evaluation once it is started.
  https://gitlab.haskell.org/ghc/ghc/issues/10871, originally made for Backpack, enshrines `hi` files with `-fexpose-all-unfoldings` as a separate "fat" interface file format.
  This ideal duel to the "naive" core interpreter to ensure errors are caught as soon as possible.

Template Haskell in GHC
  The motivation evokes the specter of ecosystem splits.
  Well, we already have one with GHC in that it cannot use Template Haskell or depend on arbitrary packages.
  Cross compilation is one issue, but also ABI changes, where a newly built stage 1 compiler uses and older ABI than code it compiles.
  In a worst imagine a simultaneous ``hi``/``ho``-file format change, ABI change, and trying to cross compile a new GHC to run on a different platform.
  The GHC doing the building can neither load stage0 compiled code, since the file formats are different, nor load its own compiled code since the ABI is different.

  This proposal out of the box only solves the cross compilation issue, but it does get us closer on the other.
  The first missing piece is multi-target support for GHC.
  This allows the same new binary to create native and foreign ``hi``/``ho`` files in the new format, for TH stages -1 and 0 of the bootstrapping stage 2 GHC and its dependencies.
  [Core is multi-platform, but the resolution of CPP, cabal conditions, and other miscellanea is platform-specific.]
  I've already been working on making GHC multi-target, building on earlier work by @angerman and others, and am almost done.
  The second missing piece is the `"naive" Core interpreter`_, as described in the previous subsection.
  File formats are independent of RTS ABIs, and so the stage 1 compiler can always load a "fat" interface file it itself created and interpret it.
  Putting everything together, the stage 1 compiler makes native "fat" interface files for "stage 2, TH stage -1", and splices their evaluations into the "stage 2, TH stage 0" code to make the stage 2 GHC that will run on the foreign platform.
  [N.B. In the easy case when we don't change the ABI, compiler bootstrapping stages and TH stages coincide!
  Stage 2 - 1 = Stage 1.]

  Switching all existing ``derive-*`` code generators to TH would probably make them lighter and easier to maintain.
  It should also allow building GHC the binary with plain `cabal`.
  Hadrian would be one step closer to being another implementation of Cabal/cabal-install without GHC-specific logic.

``*_BUILD_*`` and ``*_HOST_*`` not always defined
  The conditional definition of the CPP macros ensures they don't pollute the purity of the build when they don't matter.
  This is important for highly pure build systems like Nix to not have to needless rebuild stuff when the target platform changes.
  It will also cut down on people improperly using "target" when they meant "host".

``Lift`` and qualified goals
  When we carefully introduce lifting to stage-hygienic goals, we need to ensure that the type being lifted is the same or "close enough".
  This means we will need to introduce a intra-package constraint on package defining that type across the stage pair where Lift is made available.
  Conceptually, there might be an auto-generated package with the orphan ``Lift`` instance which imposes the same version constraint on it's library dependency in both stages.

  In particular, existing qualified dependencies from ``setup-depends`` and ``build-tool-depends`` are from stage *n* to *n - 1*;
  that the stages are different alone explains why versions are allowed to differ.
  In particular this means given a dependency edge where the needed and needing components are in the same package regardless of their relative stage indices,
  the same version of the package must be used for both.


Costs and Drawbacks
-------------------

- This is a huge amount of work.
  But I am fine chipping away it over a long period of time.

- Even a temporary conflict between typed TH and this could slow typed TH's adoption.

- I don't know of precedent for extensions that prevent modules from being linked together.

- Most existing libraries with commonly used TH helpers (`lens`, `aeson`) have the TH in the same call component but in a different module.
  To leverage this proposal, we would have to refactor them to put those modules in a separate library component.
  It would take decent amount of conditional code to still support old GHCs, and even more to not be a breaking change on those old libraries.

Alternatives
------------

There is no fundamental reason modules couldn't export non-stage-0 items, and libraries expose non-stage-0 modules.
At the cost of more complexity, there could be a `.lib` or `.so` for each exposed stage, and imports would be offset to match the ``#import <offset>`` literal.
But in fairness, this might allow a smoother transition form how libraries are structured today.
For example, one could do ``#import 1 Control.Lens.Lens`` in ``Control.Lens.TH`` while exposing ``Control.Lens.TH`` from the same library just like today.
I decided against this as a matter of taste.
I think it good to enforce the normal form that the "main" stage is stage 0.
As to the specific example, I would rather packages leverage public Cabal sub-libraries for Template Haskell anyways;
I think that's a cleaner way to package code.

Unresolved Questions
--------------------

Quotes in ``-XTemplateStagePersistence`` modules cannot reliably be used from ``-XNoTemplateStagePersistence`` modules without introducing scoping errors.
Need some way to prevent that outright, or catch those errors early, perhaps by tainting any quote with cross-stage persisted syntax.
[Thankfully the other direction is fine.
Libraries can experiment with this extension without forcing an ecosystem split.]

Implementation Plan
-------------------

I volunteer to chip away at this, thought it will take quite a while for one person to do it all.
Here is a rough plan.

#. Make GHC multi-target. I am almost done with this.

#. Land `<https://gitlab.haskell.org/ghc/ghc/merge_requests/935>`_, refactoring GHC to allow there being more than one "home package" per session.
   This PR also may help with the 2019 GSOC around `<https://gitlab.haskell.org/ghc/ghc/wikis/Multi-Session-GHC-API>`_.

#. Parameterize dependency data types (for module and package dependencies) to track dependencies per stage.

#. Refactor the implementation of Template Haskell to use the per-stage data-types.

.. _`"naive" Core interpreter`: https://github.com/ghc-proposals/ghc-proposals/issues/162

.. [InferringScope] https://cs.brown.edu/~sk/Publications/Papers/Published/pkw-inf-scope-syn-sugar/paper.pdf
