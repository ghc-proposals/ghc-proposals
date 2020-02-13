Refining the plugin recompilation API
==============

.. author:: Matthew Pickering
.. date-accepted:: 2018-05-15
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/7414
.. implemented:: 8.6
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/108>`_.
.. contents::

Modules compiled with plugins are always recompiled even if the source file is
unchanged. This most conservative option is taken due to the ability of plugins
to perform arbitrary IO actions.

If the result of the plugin is a pure function of the source file
then such recompilation is unnecessary.  This proposal proposes a method for
plugins to inform the compiler of its intentions and how it should affect
recompilation avoidance.


Motivation
----------

The type of a core to core plugin is::

  ModGuts -> CoreM ModGuts

The ``CoreM`` monad gives the user access to the current state of the simplifier
when the pass is run but also can perform any IO actions. As a result, it is impossible
to conclude that the plugin acted purely and didn't consult the outside world
impurely for information about how to perform the analysis.

It is however the case that most plugins are pure and should only be rerun
when the source code of the file changes.


Proposed Change
---------------

We identify three possible different ways which could affect whether a plugin
should trigger recompilation.

1. The referent of the module specified by ``-fplugin`` changes.
2. The options passed by  ``-fplugin-opts`` are modified.
3. The plugin is unchanged but acts impurely.

In case (1) we recompile the module.

We handle cases (2) and (3) by the same mechanism.

We augment the plugin data type with an additional field for calculating a fingerprint
for the current module. It follows the same modular style as existing plugins.::

  pluginRecompile :: [CommandLineOption] -> IfG PluginRecompile

If GHC would otherwise not recompile a module, then (we assume) the input
passed by GHC to the plugin would be the same if it did recompile it. So the
only reason to recompile would be if the plugin is impure (``pluginRecompile`` returns
``ForceRecompile``) or its input flags have changed (``pluginRecompile`` returns a different
fingerprint to last time). So GHC checks that each plugin (applied to its
flags) returns the same fingerprint as on the previous compilation. If the
fingerprint differs, or returns ``ForceRecompile``, recompilation is triggered.

This function is then lifted appropriately to work as the other recompilation
checking functions in ``MkIface`` and run after the other recompilation checks.

Precise change to ``Plugin``
----------------------------

We add an additional field ``pluginRecompile`` to ``Plugin``. ::

  data Plugin = Plugin {
                   ....
                   , pluginRecompile :: [CommandLineOption] -> IfG PluginRecompile
                   }

The ``PluginRecompile`` data type records the three different posibly purities of
a plugin.::

  data PluginRecompile = ForceRecompile | NoForceRecompile | MaybeRecompile Fingerprint

A plugin which declares itself impure using ``ForceRecompile`` will always
trigger a recompilation of the current module. ``NoForceRecompile`` is used
for "pure" plugins which don't need to be rerun unless a module would ordinarily
be recompiled. ``MaybeRecompile`` computes a ``Fingerprint`` and if this ``Fingerprint``
is different to a previously computed ``Fingerprint`` for the plugin, then
we recompile the module.

For the common case of a pure plugin, we can provide a function which appropiately
lifts a function to a ``PluginPass``.::

  purePlugin :: [CommandLineOption] -> IfG PluginRecompile
  purePlugin args = return NoForceRecompile

The advantage of using ``NoForceRecompile`` rather than a constant ``MaybeRecompile``
is that an end user doesn't have to concern themselves with the details of
what a ``Fingerprint`` is or how to construct one. An alternative is to
provide a smart constructor wrapping ``fingerprint0``.

By default, the field is initialised to always return ``ForceRecompile``
in order to maintain backwards compatible behaviour.

Specification of Purity
-----------------------

A plugin ``P`` is pure iff for modules ``M`` and ``N`` and a finger printing function
``F``, ``F(M) = F(N) => P(M) = P(N)``. This definition means that a user has
to be aware of the fingerprinting algorithm ``F`` but if they want to be precise
about when to recompile, this is somewhat necessary anyway.

Calculating fingerprints
------------------------

Users can use the same functions that GHC uses internally to compute fingerprints.
The `GHC.Fingerprint<https://hackage.haskell.org/package/base-4.10.1.0/docs/GHC-Fingerprint.html>`_ module provides useful functions for constructing fingerprints. For example, combining
together ``fingerprintFingerprints`` and ``fingerprintString`` provides an easy to
to naively fingerprint the arguments to a plugin.::

  pluginFlagRecompile :: [CommandLineOption] -> IfG PluginRecompile
  pluginFlagRecompile =
    return . MaybeRecompile . fingerprintFingerprints . map fingerprintString . sort




Drawbacks
---------

A plugin author must carefully consider how their arguments should affect recompilation.
However, the generality is not oppressive. In the simplest case where there
are no arguments, an author can supply a constant ``Fingerprint``. If they need
recompilation, ``ForceRecompile``. It could be desirable to provide some combinators
for the more complicated cases.

It is possible that an author specifies the incorrect recompilation behaviour
but this is not the responsibility of GHC to enforce. Specifying correct
recompilation behaviour could depend on knowing details about how the fingerprinting
function is calculated but this is not disimiliar to a normal plugin  where you have
to know the semantics of core or the constraint solver.

There are also complicated hypothetical scenarios such as a plugin reading a certain
file depending on which file is being compiled. Ideally, we want to compute the hash
of this input file to work out whether it has changed but this is difficult to achieve
without access to the source code. This seems over-elaborate, in order to maintain
simplicity, if a user wants to write a plugin like this they should always trigger
recompilation.


Alternatives
------------

There are three simpler alternatives which I can imagine.

1. We statically, at initialisation time say whether a plugin is pure or not.
   If it is pure, we never recompile because of it, if it is impure we always
   recompile. This has the disadvantage of author's of advanced plugins not being
   able to pass complicated options to plugins which might not affect the program output.

2. We dynamically return a boolean value rather than a fingerprint to indicate whether
   we should recompile with the plugin in future. For example, a plugin might try to access
   a webpage, if it fails to access the resource it may fail gracefully but the next time we
   run the compilation pipeline it should try and access the resource again.  After fetching
   the resource, we don't need to run the plugin again so it would return ``False``.

3. For (3), the most complex case, we could envisage an over-engineered API which
   tracked which functions in ``CoreM`` acted impurely and ultimately decided
   whether the plugin was pure or not. However, we propose to shift this responsibility
   onto the plugin author to decide.

It has been suggested that each plugin function returns a fingerprint itself,
indicating what work it has done. However, this defeats the point of the proposal
as you must then run the plugin in order to decide whether to run the plugin!

An earlier proposal proposed a single hashing function added as a field to the ``Plugin``
data type. This has now been changed to this more fine-grained approach where each
pass computes a suitable hash. It was finally decided by the committee to revert
 to the backwards compatible version.


Unresolved Questions
--------------------

It should be considered how compilation avoidance complicates or simplifies the
concurrent source plugin proposal (#107).


Additional Links
----------------

* https://gitlab.haskell.org/ghc/ghc/issues/7414
* https://gitlab.haskell.org/ghc/ghc/issues/12567


