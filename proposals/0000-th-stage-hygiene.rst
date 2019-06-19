Stage Hygiene in Template Haskell
==============

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

Here you should write a short abstract motivating and briefly summarizing the proposed change.


Motivation
------------

Template Haskell currently doesn't work well with cross compilation.

The external interpreter made it at least possible, relatively automatically, but doesn't work if you explicitly want side effects to be run on the compiler's platform.
Also, while same-OS cross can sometimes be fairly lightweight
—e.g. by having QEMU translate syscalls so the native kernel can be used—
differnet-OS requires harder to provision virtual machines or real devices.

Another alternative is dumping and loading splices, where one builds natively, dumping splices, and the builds cross with those dumped splices.
This became easier with `this patch <https://github.com/reflex-frp/reflex-platform/blob/master/splices-load-save.patch>`_.
Still, this requires building every package twice, and worse doesn't work if the macro is target specific.
For example, imagine some code like
::
  #ifdef ios_HOST_OS
  data SomeIosFfiType
  $(iosBoilerplateHelper ''SomeIosFfiType)
  #endif
If the splice is within the ``ifdef``, it won't be dumped.
But if it is outside the ``ifdef``, the native one won't build!

What we need instead is a way to say is different platforms:

- Splices alone run on the build platform (of the module being built, not GHC).
- Normal code, as usual, runs on the host platform.
- quoted code runs on the target platform.

This solves all the above problems:

- No need to emulate any other platforms, since evaluation only happens within splices.
- No need to build everything twice.
- No all-or-nothing CPP problem.

To do this, we need to cleanly separate the stages induced by quoting and splicing.
This is not a new idea for programming languages in general.
Racket (and probably some schemes) do this.
The work-in-progress (?) `OCaml macro system <https://github.com/ocamllabs/ocaml-macros>`_ does this.
It has even been informally proposed for Haskell by @ezyang in `<http://blog.ezyang.com/2016/07/what-template-haskell-gets-wrong-and-racket-gets-right/>`_.

Enforcing that separation means restricting programs we currently allow.
Least surprisingly, normal bindings, and normal imports, in the module cannot be used in splices or quotes.
But there are other constructs that more surprisingly tangle stages too.
Typed Templated Haskell is one.
First of all, there is name leakage.
::
  [|| ... :: IosOnlyType ||] :: Q (TExp IosOnlyType)
This can't work unless we are building *on* and *for* iOS.
But even if we work around that, there's also will be semantic leakage.
In the near future there would be
::
  AppE <$> [|| ... :: foreach (x :: Int) -> F x ||] <*> [|| 2^36 :: Int ||] :: Q (TExp (F ???))
How do we type the whole expression, or ``AppE`` in particular?
And say the compiling platform has 32-bit `Int`s?
The dependent function will have different result types due to overflow, which ruins the guarantees of typed Template Haskell.
Even today we have similar problems with CPP'd type families:
::
  #if mingw_HOST_OS
  type instance F Bool = []
  #else
  type instance F Bool = Tree
  #endif
``Lift`` is similarly problematic.
::
  lift (.. :: LinuxOnlyType)
This relies on native compilation to Linux or a scoping violation must also be induced.
::
  lift (2^^25)
This though is fine as regardless of overflowing on either side an ``Int`` can be kept an ``Int``, and overflowing is already defined behavior.

I would love to, instead of outright banning Typed Template Haskell and ``Lift``, come up with a flexible way to associate types and terms between stages.
To be "complete" in that module scoping everything is complete is still possible, we would need slightly different requirements for each.
For ``Lift`` we just need to map *values* preserving type, while typed Template Haskell we need to map type *expressions* such that evaluation commutes with the mapping.
Adding language support for such a mapping is lots of extra work—borderline research—for a proposal which already is no small task.
I therefore think banning for now to start solving the problems people have with cross compilation as soon as possible is prudent.
Because this is breaking change no matter what, a variant extension is used anyways, so no program breaks.
Instead, users a just temporarily presented with a choice to either support cross compilation or have ``Lift`` and typed TH.

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

   The existing side conditions, which restricting nested quotes and splices (i.e. stages outside of -1, 0, and 1) remain in place, but are ripe for removal in #204.

#. Add new syntax for stage-offset imports and bindings:
   ::
     $import <integer-literal> <<existing syntax>>
   This means import a module in stage *n* instead of stage 0 as per normal.
   ::
     $let <integer-literal> <<existing syntax>> = <<existing syntax>>
   In both case the ``$`` must not be followed by whitespace, both to avoid conflicts with other syntax and to be consistent with splices.
   The means bind identifiers in stage *n* instead of stage 0 as per normal.
   Module exports however are restricted to stage 0.

#. The current "stage restriction" on splices using items from module is abolished.
   Any stage n + 1 binding in a stage n splice is fair game.
   The prohibition on referencing bindings can stay for now, but hopefully will be removed in a future proposal.
   (It just avoids the need to topologically sort splices based references from the quotations inside them.
   Nevertheless, implementing that is not trivial so its good to decouple from this already-large proposal.)

#. Relax ``-XTemplateHaskellQuotes`` to instead allow Template Haskell constructs, but restrict their usage so all syntax is in stages >= 0.

#. Introduce ``-XTemplateStagePersistence``.
   Which is implied by ``-XTemplateHaskellQuotes`` (and thus plain ``-XTemplateHaskell``) for backwards compatibility.
   It allows the current behavior where we blur the distinction between stages.
   In particular, with this enabled:

   - Stage 0 identifiers bound in another module can be used in stage 1 (quotes) and stage -1 (splices).
   - Stage 0 identifiers bound anywhere can be used in stage 1, and are automatically.
   - Typed template haskell is allowed.
   - The ``Lift`` type class and all its associated definitions are made available.

   With ``-XNoTemplateStagePersistence``, overriding the default, all of those are *disabled*.

#. Extend the command line [TODO bikeshed!!] with a way to specify per-stage package dependencies and the like.
   If/when GHC becomes multi-target, by default stages >= 0 take GHC's target platform / the packages host platform (where compiled code runs), while stages < 0 take GHC's host platform / the packages build platform (where GHC runs).
   But, the emitted platform can still be specified per-stage like the other flags.
   This is needed when building TH functions to be used from cross compiled code.

#. Add a Core "way" to GHC, which basically amounts to `-fexpose-all-unfoldings` but no need to compile pass core [TODO bikeshed/clarify].
   Positive stage imports can be satisfied with the core way alone, as no code needs to be run.
   (With the `"naive" core interpreter`_, negative stage imports can also use this, as those stages, while run, and discarded after and not included machine code.)
   [TODO Cross reference with the backpack ``hi``-only steps for type checking.]

#. When importing modules/packages, after applying the import offset ensure that the platforms match.
   Note that while each module only has bindings in its own stage 0, those bindings can contain quotes from stages greater than 0.
   All such quoted platforms need to match.

#. Just as GHC defines ``*_HOST_OS`` and similar CPP identifiers today, it would define ``*_BUILD_*`` ones if you have any stage -1 package imports, and ``*_BUILD_*`` if you have any stage 1 package imports.

Cabal
~~~~~~~~~~~~

#. Extend the ``build-depends`` syntax with a stage integer offset parameter.
   N.B ``build-tool-depends`` can be thought of as a stage -1 executable dependencies list.
   `https://github.com/haskell/cabal/issues/5411`_ asks for a ``run-tool-depends`` which would be nothing but a stage 0 executable depends.
   ``setup-depends`` can also be thought of as a stage -1 executable dependencies list.

#. Likewise extend ``other-modules`` with a stage integer offset parameter, to support intra-package ``$import``.
   Leave ``exposed-modules`` as is, however. Libraries should only expose stage 0 modules, just as modules only expose stage 0 definitions.
   Restrict the ``other-modules`` offset to be <= 0, as positive stage code is either pointless or would escape via references from quotes causing build system havoc.
   Unexposed negative stage modules need not be installed at all, as there is no way for stage 0 to reference them (splices eliminate references).

#. Replace today's "qualified goals" with a notion "per-stage coherence".
   In particular, existing qualified dependencies from ``setup-depends`` and ``build-tool-depends`` are from stage *n* to *n - 1*;
   that the stages are different alone explains why versions are allowed to differ.
   However a *-n* dependency composed with an *n* dependency create a 0 dependency, which as all the usual version coherence restrictions.
   As an exception to this, we keep today's same-package version constraint.
   In particular this means given a dependency edge where the needed and needing components are in the same package regardless of their relative stage indices,
   the same version of the package must be used for both.

Effect and Interactions
-----------------------

Here is an example of many the features used together, rewriting the code from the motivation.
Hypothetical ``ios-th`` package:
::
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
  module MyApp.Ios where

  #ifndef ios_HOST_OS
  # error Module shouldn't be built. Fix Cabal file!
  #endif

  import Ios.Types
  $import -1 Ios.Macros

  data SomeIosFfiType

  $let -1 unneededBinding = iosBoilerplateHelper ''SomeIosFfiType

  $(unneededBinding)

This proposal, in conjunction with a `"naive" core interpreter`_ should make it permitted to use Template Haskell in GHC.
Stage 1 GHC even today could use Template Haskell.
Stage 2 was the sticking point, if stage 1 is a cross compiler or the ABI was changed.
But those cases are now OK too.
Consider the "worst case", where the ``ho``/``hi`` format and ABI are both changed, and we are building stage 2 for a different platform.
The stage 1 compiler can load ``-fexpose-all-unfoldings`` stage 2 interface files it built for the native platform,
and naively interpret them (which avoids any coupling with the stage 0 RTS, ABI, etc).

The conditional definition of the CPP macros ensures they don't pollute the purity of the build when they don't matter.
This is important for highly pure build systems like Nix to not have to needless rebuild stuff when the target platform changes.
It will also cut down on people improperly using "target" when they meant "host".

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

#. Land `https://gitlab.haskell.org/ghc/ghc/merge_requests/935`_, refactoring GHC to allow there being more than one "home package" per session.
   This PR also may help with the 2019 GSOC around `https://gitlab.haskell.org/ghc/ghc/wikis/Multi-Session-GHC-API`.

#. Parameterize dependency data types (for module and package dependencies) to track dependencies per stage.

#. Refactor the implementation of Template Haskell to use the per-stage data-types.

.. "naive" core interpreter: #162
