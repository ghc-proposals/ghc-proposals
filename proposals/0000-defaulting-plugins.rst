Defaulting plugins
==================

.. author:: Andrei Barbu
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

Defaulting has many useful applications, and is particularly critical for large
APIs with many type parameters like those used for machine learning or
games. Instead of burdening the language with syntax that might be too divisive
or limited, we propose a defaulting plugin extension. Like type plugins,
defaulting plugins run while type checking. They offer up new hypotheses before
GHC declares a type to be ambiguous.

Motivation
----------

There is clear wide interest in more powerful defaulting behavior. For years we
have had discussions in the community about how to extend the current defaulting
mechanism. Haskell prime had at least `four initial proposals
<https://web.archive.org/web/20150810061936/https://ghc.haskell.org/trac/haskell-prime/wiki/Defaulting>`_
and a `subsequent one <https://github.com/haskell/rfcs/pull/18>`_.

Every proposal put forth so far has had significant shortcomings. Many proposals
are not compatible with the current behavior. They are often complex adding
significant burden to GHC and its maintainers. Since no principled theoretical
notion of what should be defaulted when has been put forth, defaulting
mechanisms proposed so far have been ad-hoc, and so unlikely to be robust to
future language changes.

This proposal overcomes every one of these shortcomings. It leaves the current
behavior and syntax completely unchanged. The maintenance burden is minimal,
being implemented in one evening, and coming in at only about 50 lines of
code. Any behavior that we want in the future can easily be implemented.

The issue of defaulting is becoming more urgent with machine learning libraries
that introduce new numeric types, exacerbating exactly the kinds of problems
that the original defaulting mechanism was introduced for. Moreover, new
programming styles that lean heavily on type variables force authors to make
many onerous irrelevant decisions to get anything working. This form of
defaulting is in large part why Python ML APIs are more usable today.

For example, hasktorch and haskell-torch specify the device a computation runs
on at the type level using a promoted constructor; a simplified variant of which
looks like::

   data Device = CPU | GPU
   forward :: forall device. Tensor Int device
   forward = ...

Of course ``forward`` and ``Tensor`` typically have many more parameters. Often,
one does not want to specify this ``device`` type parameter upfront, it doesn't
matter while debugging when you just want to see the result immediately. Having
to constantly apply it with ``@ CPU`` is tedious and confusing for beginners. It
also makes the API difficult to use in ghci. This gets worse when one considers
that every single numeric operation may produce ambiguous numeric types, just as
they do without the defaulting mechanism in Haskell98. Again forcing users to
specify something that is irrelevant and has a reasonable default. A simple
defaulting extension could target such use cases and help the users of these
libraries. Such an extension is comes in at only 100 lines of code, and is easy
to implement on top of the defaulting plugins proposed here.

Proposed Change Specification
-----------------------------

Extend the GHC plugins API adding a defaulting plugin to Plugins.hs. The
interface for this plugin is specified as::

  -- | A plugin for controlling defaulting.
  type FillDefaulting =  WantedConstraints -> TcPluginM DefaultingPluginResult
  
  data DefaultingPlugin = forall s. DefaultingPlugin
    { dePluginInit :: TcPluginM s
      -- ^ Initialize plugin, when entering type-checker.
    , dePluginRun :: s -> FillDefaulting
      -- ^ Default some types
    , dePluginStop :: s -> TcPluginM ()
     -- ^ Clean up after the plugin, when exiting the type-checker.
    }
  
  -- | Propose the following types to fill this type variable in the selected
  -- constraints.
  type DefaultingPluginResult = [([Type],(TcTyVar,[Ct]))]

Defaulting plugins, like other type checker plugins have an init and stop for
setup and teardown. They can maintain plugin-specific state in `s`. When an
ambiguity is imminent and is about to cause an error, ghc will invoke
``dePlugin`` to ask the plugin to propose defaults. The wanted constraints that
should be defaulted are provided to the plugin. The plugin reports which type
variables in which constraints could be filled in with a given type. These are
merely proposals, just as with the current defaulting mechanism, and GHC is free
to reject them. The behavior after a proposal is made remains entirely
unchanged, and proceeds as if that proposal had been made by the existing
defaulting mechanism.

Examples
--------

Defaulting plugins are easy to write and are quite short. For example, a plugin
which addresses the issue with defaulting promoted type constructors in cases
like specifying the device a ``Tensor`` should be computed on, comes in at just
100 lines of code. The vast majority of which is tedious but generic
housekeeping common to type plugins. The plugin consists of two components
exported from one module, the plugin which will be used by GHC and a class which
with which users can register plugins::

    class DefaultType x (y :: x)

Defaults can be registered as::
    
    instance DefaultType Device GPU
    instance DefaultType Device CPU

Users can then write code without worrying about which device will be used. At
the same time, enabling warnings on type defaulting, provides the best of both
worlds and informs them both about the fact that defaulting is happening and
what the chosen default is. This could help API discoverability.

One might imagine many other features such as adding a priority to the defaults
or adjusting them based on the GPUs available on a specific machine. Note that
this is just an example of how defaulting plugin might interface with the
user. Nothing in the design restricts the space of how defaulting plugins can be
used. Defaulting plugins need not provide any extension mechanism at all, or can
take information from users through other channels such as plugin parameters
instead of the code.

The core of this defaulting plugin looks like::

   solveDefaultType :: PluginState -> [Ct] -> TcPluginM DefaultingPluginResult
   solveDefaultType _     []      = return []
   solveDefaultType state wanteds = do
     envs <- getInstEnvs
     insts <- classInstances envs <$> tcLookupClass (defaultClassName state)
     let defaults = foldl' (\m inst ->
                              case is_tys inst of
                                [matchty, replacety] ->
                                  M.insertWith (++) matchty [replacety] m) M.empty insts
     let groups =
           foldl' (\m wanted ->
                     foldl' (\m var -> M.insertWith (++) var [wanted] m)
                            m
                            (filter (isVariableDefaultable defaults) $ tyCoVarsOfCtList wanted))
                  M.empty wanteds
     M.foldMapWithKey (\var cts ->
                       case M.lookup (tyVarKind var) defaults of
                         Nothing -> error "Bug, we already checked that this variable has a default"
                         Just deftys -> do
                           pure [(deftys, (var, cts))])
       groups
     where isVariableDefaultable defaults v = isAmbiguousTyVar v && M.member (tyVarKind v) defaults

The current defaulting rules are extracted. Then wanteds are filtered and
grouped by variable. Finally, defaults are proposed.

Effect and Interactions
-----------------------
This proposal opens up defaulting using a plugin, and so has no impact on the
language itself. The impact on GHC is minimal as it reuses virtually all of the
code used to support the current type defaulting mechanism.


Costs and Drawbacks
-------------------
The development costs are minimal. An implementation of this `proposal already
exists
<https://github.com/abarbu/ghc/commit/33e66adc01c8c95521aecd5189d1d8ce5f360775#diff-ba7020811708ea16e92cddffa71f32bd0a0e2eeb9665381295dcc4db0858db44>`_.
The vast majority of that implementation consists of an addition to the test
suite and documentation. The changes to GHC proper only amount to about 50 lines
of code, all of them straightforward. The implementation provided could be made
more efficient, but likely suffices as is.

The major drawback of this approach is also one of its strengths: defaulting
mechanisms may become fairly library specific because they are so simple to
implement. A library will be able to implement any defaulting rules it wishes
for its own purposes, perhaps leading to some surprising behavior in the
future. By hiding away the defaulting into a plugin we may make understanding
why a default was chosen more difficult. Although this problem is significantly
alleviated by warnings when defaulting, which are already produced and work
correctly in the presence of this new plugin API.

Language users may come to abuse this mechanism. We probably don't want to end
up with a Haskell that relies too heavily on defaulting. This plugin opens the
box to everything. Of course, one could say this about virtually any of the GHC
plugin APIs.

Alternatives
------------
There are no alternatives which have reached any kind of consensus for many
years now. Simply put, defaulting is stuck with no theory on the horizon
to rescue us. Many other alternative defaulting implementations have
been proposed. Haskell prime had at least `four initial proposals
<https://web.archive.org/web/20150810061936/https://ghc.haskell.org/trac/haskell-prime/wiki/Defaulting>`_
and a `subsequent one <https://github.com/haskell/rfcs/pull/18>`_.

Unresolved Questions
--------------------
None

Implementation Plan
-------------------
This proposal is `already implemented
<https://github.com/abarbu/ghc/commit/33e66adc01c8c95521aecd5189d1d8ce5f360775#diff-ba7020811708ea16e92cddffa71f32bd0a0e2eeb9665381295dcc4db0858db44>`_,
including updating the GHC docs and the test suite.
