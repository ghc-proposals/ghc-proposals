GHCi command for recompilation-aware evaluation
==============

.. author:: Alexandre Esteves
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/720>`_.
.. contents::

This proposal adds a new ``:watch`` command to GHCi to automatically evaluate
bindings on reload.

Whereas GHCI-based file watcher tools
(`ghcid <https://github.com/ndmitchell/ghcid>`_, `ghciwatch <https://github.com/MercuryTechnologies/ghciwatch>`_)
enable reload on changes to source files, ``:watch`` enables evaluation on
reload.
It should be noted that ``ghcid --test`` and ``ghciwatch --test-ghci`` also
allow evaluation on reload, but do not take into account *what* was reloaded.
By adding this functionality directly on GHCi, we can track which modules actually
were recompiled (or have a transitive dependency that was) and thus re-evaluate
only the things whose meaning was changed.

Motivation
----------

The original motivation came from working on Haskell full-stack webapps, and
having the frontend state (DOM state, in-memory values, etc) be constantly
discarded when doing changes that only required re-evaluating backend bindings,
such as tweaking a query or request handler while preserving the API.
Avoiding this waste requires that the watched bindings be evaluated
independently.

While this motivation remains relevant (especially as the JS and WASM
targets gain adoption) the same benefits apply to any project where tested
bindings should not be constantly evaluated, be that because they're
long-running stateful tasks (e.g. background daemons, servers), or use scarce
resources (e.g. expensive computation, rate-limited operations).

Now that multiple-unit support is maturing, it's increasingly common to feed
several components/packages to the same GHCi session, for faster feedback
cycles, which means there is more potential benefit for the extra
granularity offered by ``:watch``.

Proposed Change Specification
-----------------------------
I propose adding a ``:watch`` command to GHCi, which takes as arguments any
number of module-qualified binding names, to be independently re-evaluated on
their respective re-compilations.

Since we allow watching multiple bindings, where any of them can be long-running,
we must perform concurrent evaluation. Meaning, if the type of a binding
is ``IO a``, we need to spawn a thread for it, which also allows it to persist
across GHCi ``:reload``.
Rather than adding special handling for ``IO`` )as is the case when performing
manual evaluation at the repl), we simply require this type of all bindings.
Given there's no obvious way to use the resulting ``a``, we further
restrict to ``IO ()``.

Issuing a ``:watch`` command merely registers the bindings for subsequent
ocurrences of ``:load`` or ``:reload``. In particular, on adjacent invocations
of ``:watch``, all but the last one will have no effect on evaluation.

Whenever compilation happens, the bindings *requested to watch*
must be reconciled with the *currently watched* ones.
As such, for each binding name we take the following non-exclusive actions:

#. ``killThread`` if watched
#. ``forkIO`` if requested

Examples
--------

Motivation
^^^^^^^^^^^^

The given motivation scenarios are satisfied by doing something in the vein of

::

  > :watch M1.expensiveComputation M2.rateLimitedAccess M3.frequentlyIteratedOn
  > :reload

or

::

  > :watch Backend.main Frontend.main
  > :reload

If we follow with
::

  > :watch Backend.main
  > :reload

the thread running ``Frontend.main`` will be killed.

If we then

::

  > :watch Frontend.main
  > :reload
the thread running ``Backend.main`` is killed and a thread for
``Frontend.main`` is spawned again.

Pure expressions
^^^^^^^^^^^^

As for pure expressions, we can ``print`` them like GHCi does on manual evaluation

::

  module M where
  watched = print 123

::

  > :watch M.watched

Arbitrary expressions
^^^^^^^^^^^^

Watching arbitrary expressions is done by giving them a top-level name

::

  module M where
  watched = foo . bar . baz

followed by

::

  > :watch M.watched

If one wants to watch keep their module API clean of testing code, it's
sufficient to use a separate module for the effect.

::

  module Test where
  import M

  watched = foo . bar . baz

::

  > :watch Test.watched

so there is no loss of generality by restricting to identifiers and ``IO``.

Multiple bindings in the same module
^^^^^^^^^^^^

Given the whole idea is to have a hook into the compilation event, and AFAICT
each module is compiled in one go, there's no smaller granularity to hook into.
On attempt to specify multiple bindings from the same module, rather than
arbitrarily pick one or otherwise combine them some, we error and force the
user to explicitly compose them.

That is, there's no point allowing

::

  > :watch M.x M.y

since the same can be handled with more flexibility via

::

  > :watch M.z

and user code

::

  module M where
  z = forkIO x *> forkIO y

so there is no loss of generality by restricting to one binding per module.

Effect and Interactions
-----------------------

Both ghcid and ghciwatch respect ``.ghci`` and also allow passing GHCi commands
on a per-invocation basis, so I expect no change is needed for them to take advantage
of this feature. The same goes for any other haskell file-watcher that wraps
GHCi usage without restricting access to commands.

Costs and Drawbacks
-------------------
The prototype took a few days of work so far.
Remaining development time depends on how much the spec/implementation need to change.
As for maintenance costs, I think long-time GHC devs would know better than me.

Backward Compatibility
----------------------
1. Breakage only in extremely rare cases (e.g. for specifically-constructed
   examples, but probably no packages published in the Hackage package repository)

This will likely break any ``.ghci`` scripts doing ``:def watch ...``.
I'm not sure if there's a reliable way to do impact assessment or mitigation,
but this use-case also sounds obscure enough that I wonder if there'll be any
actual breakage in practice.

Alternatives
------------

Status quo
^^^^^^^^^^^^

It is already possible to avoid redundant evaluation by launching separate
file-watcher/GHCi sessions - one for each binding of interest. However that then means
redundant compilation for modules included in multiple sessions, with the
slowdown growing both with the size of the codebase and the number of sessions,
since that likely brings more overlap.

When dealing with a single GHCi session, current watcher tools allow observing
the value of multiple bindings by either

#. composing them into a new top-level binding
#. specifying them individually

With (1), when any watched binding changes, everything gets re-evaluated, which
is wasteful as any internal state (UI, metrics, cache, etc) will be reset.

While (2) can avoid said waste in principle, this requires each tool to have
logic to figure out which modules were reloaded (say, via reverse engineering
of GHCi terminal output), and other associated bookkeeping. Rather than have a
multitude of abstraction-inversion efforts, it's easier to add support upstream
where the relevant structure are already available, that is, in GHCi itself.


Extended syntax
^^^^^^^^^^^^
One possible extension to the command syntax would be allowing arbitrary
expressions (as is the case with ghcid/ghcidwatch), from which the relevant
dependencies can be recovered. For example

::

  > :watch (print =<< A.parseFile "foo.txt")

would behave the same as

::

  module B where
  import qualified A
  watched = print =<< A.parseFile "foo.txt"

and

::

  > :watch B.watched

which naturally generalises to expressions involving bindings from multiple
modules. While this is more convenient for users, as it avoids any need for
``:watch``-specific modules, it adds significant complexity to the
implementation.
Since this is a strictly more permissive syntax, it remains possible to later
adopt it without breaking compatibility.

Granularity
^^^^^^^^^^^^

Lastly, if it were possible to detect changes to individual bindings (or their
transitive dependencies), that would allow for even more granular re-evaluation,
and call for removing the restriction to one binding per module as

::

  > :watch M.a M.b

would evaluate only when ``M.a`` or ``M.b`` changed meaning, ignoring any other
irrelevant parts of the module.

Unresolved Questions
--------------------
It is not always enough to qualify a name with a module name since using multiple
units makes it possible to have two modules with the same name.

My understanding is that since this is

- fairly rare
- `already a problem <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/14427#note_624644>`_

this proposal can ignore the concerns of that layer.

Implementation Plan
-------------------
I have a working prototype in a `draft PR <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/14440>`_
and am available for adapting/rewriting it as the proposal process develops.

Endorsements
-------------
None (yet?)

Acknowledgements
-------------
I'd like to thank

- `Alp Mestanogullari <https://github.com/alpmestan>`_ for the more general alternative syntax
- `Daniel Oliveira <https://github.com/drdo>`_ for significant proof-reading and feedback
