.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

Refining the plugin recompilation API
==============

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

The `CoreM` monad gives the user access to the current state of the simplifier
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

We handle case (2) and (3) by the same mechanism.

We augment the plugin data type with an additional field for calculating a fingerprint
for the module. It follows the same modular style as existing plugins.::

  pluginHash :: [CommandLineOption] -> IfG (Maybe Fingerprint)

If GHC would otherwise not recompile a module, then (we assume) the input
passed by GHC to the plugin would be the same if it did recompile it. So the
only reason to recompile would be if the plugin is impure (``pluginHash`` returns
``Nothing``) or its input flags have changed (``pluginHash`` returns a differnet
fingerprint to last time). So GHC checks that each plugin (applied to its
flags) returns the same fingerprint as on the previous compilation. If the
fingerprint differs, or returns Nothing, recompilation is triggered.


This function is then lifted appropriately to work as the other recompilation
checking functions in ``MkIface`` and run after the other recompilation checks.

The default value of ``pluginHash`` can be the constant function returning ``Nothing``
which will retain backwards compatibility with the existing behaviour.

Users can use the same functions that GHC uses internally to compute fingerprints.

Specification of Purity
-----------------------

A plugin ``P`` is pure iff for modules ``M`` and ``N`` and a finger printing function
``F``, ``F(M) = F(N) => P(M) = P(N)``.


Drawbacks
---------

A plugin author must carefully consider how their arguments should affect recompilation.
However, the generality is not oppressive. In the simplest case where there
are no arguments, an author can supply a constant ``Fingerprint``. If they need
recompilation, ``Nothing``. It could be desirable to provide some combinators
for the more complicated cases.

It is possible that an author specifies the incorrect recompilation behaviour
but this is not the responsibility of GHC to enforce. Specifying correct
recompilation behaviour could depend on knowing details about how the fingerprinting
function is calculated but this is not disimiliar to a normal plugin  where you have
to know the semantics of core or the constraint solver.

There are also complicated hypothetical scenarios such as a plugin reading a certain
file depending on which file is being compiled. Ideally, we want to computer the hash
of this input file to work out whether it has changed but this is difficult to achieve
without access to the source code. This seems over-elaborate, in order to maintain
simplicity, if a user wants to write a plugin like this they should always trigger
recompilation.


Alternatives
------------

There are two simpler alternatives which I can imagine.

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

There is also the possibility of having one hashing function for each
plugin type.  This would allow the recompilation checker to give more accurate information
about why recompilation was required.


Unresolved Questions
--------------------

It should be considered how compilation avoidance complicates or simplifies the
concurrent source plugin proposal (#107).


Additional Links
----------------

* https://ghc.haskell.org/trac/ghc/ticket/7414
* https://ghc.haskell.org/trac/ghc/ticket/12567

