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

This function is then lifted appropriately to work as the other recompilation
checking functions in ``MkIface``. A value of ``Nothing`` can be returned
in order to indicate that a plugin should always trigger recompilation. This
is intended to be used when a plugin acts impurely. Returning a constant hash
indicates that unless the plugin source changes then the plugin shouldn't ever
cause recompilation.


The default value of ``pluginHash`` can be the constant function returning ``Nothing``
which will retain backwards compatibility with the existing behaviour.

Users can use the same functions that GHC uses internally to computer fingerprints.


Drawbacks
---------

A plugin author must carefully consider how their arguments should affect recompilation.
However, the generality is not oppressive. In the simplest case where there
are no arguments, an author can supply a constant ``Fingerprint``. If they need
recompilation, ``Nothing``. It could be desirable to provide some combinators
for the more complicated cases.

It is possible that an author specifies the incorrect recompilation behaviour
but this is not the responsibility of GHC to enforce.


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


Unresolved Questions
--------------------

It should be considered how compilation avoidance complicates or simplifies the
concurrent source plugin proposal (#107).


Additional Links
----------------

* https://ghc.haskell.org/trac/ghc/ticket/7414
* https://ghc.haskell.org/trac/ghc/ticket/12567

