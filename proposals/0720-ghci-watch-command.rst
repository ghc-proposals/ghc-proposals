GHCi command for recompilation-aware evaluation
==============

.. author:: Alexandre Esteves
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/720>`_.
.. contents::

Add a GHCi command to automatically (re-)evaluate bindings when they are (re-)compiled.

Motivation
----------
Haskell file watcher tools (e.g. `ghcid <https://github.com/ndmitchell/ghcid>`_, `ghciwatch <https://github.com/MercuryTechnologies/ghciwatch>`_)
are frequently used to continuously type-check and evaluate bindings on
success, as a quick way to test some behavior. Now that multiple-unit support
is maturing, it's common to feed several components/packages to the same GHCi
session, for faster feedback cycles. Often, this means one would like the
ability to evaluate several bindings on every reload.

The original motivation came from working on Haskell full-stack webapps, and
having the frontend state (e.g. DOM state, in-memory values) be constantly
discarded when doing changes that only required re-evaluating backend bindings,
such as tweaking a query or request handler while preserving the API.
Avoiding this waste requires that the watched bindings be evaluated
independently. This motivation remains relevant, especially as the JS and WASM
targets gain adoption, but the same benefit applies to any project testing
bindings that shoud not be constantly evaluated, be that because they're
long-running stateful tasks (e.g. background daemons, servers), or use scarce
resources (e.g. expensive computation, rate-limited operations).

Proposed Change Specification
-----------------------------
I propose adding a ``:watch`` command to GHCi, which takes as arguments any
number of module-qualified binding names, to be independently re-evaluated on
their respective re-compilations.

Since we allow multiple bindings, where any of them can be long-running,
we must perform concurrent evaluation. Meaning, if the type of a binding
is ``IO a``, we need to spawn a thread for it, which also allows persisting
across GHCi ``:reload``.
Rather than adding special handling for ``IO`` as is the case when performing
manual evaluation at the repl, we simply require this type of all bindings.
Given there's no obvious way to use the resulting ``a``, we further
restrict to ``IO ()``.

Whenever compilation happens, the bindings *requested to watch*
must be reconciled with the *currently watched* ones.
As such, we check, in order, if a binding name is
- watched: we call ``killThread`` on it
- requested: we call ``forkIO`` on it

Examples
--------

The given motivation scenarios are satisfied by doing something in the vein of

::

  > :watch Backend.main Frontend.main

or

::

  > :watch M1.expensiveComputation M2.rateLimitedAccess M3.frequentlyIteratedOn


Watching arbitrary expressions is done by naming them at the top-level

::

  module M where
  watched = foo $ bar $ baz

followed by

::

  > :watch M.watched

If one wants to watch keep their module API clean of testing code, it's
sufficient to use a separate module for the effect.

::

  module Test where
  import M

  watched = foo $ bar $ baz

::

  > :watch Test.watched

so there is no loss of generality by restricting to identifiers.

Continuing from the backend/frontend example, if we now
::

  > :watch Backend.main
  > :reload

the thread running `Frontend.main` will be killed.

If we follow with

::

  > :watch Frontend.main
  > :reload
the thread running `Backend.main` is killed and a thread for
`Frontend.main` is spawned again.

Effect and Interactions
-----------------------

Whereas file watcher tools trigger compilation on source change, ``:watch``
triggers evaluation on compilation, so issuing the command merely registers
the bindings for subsequent ocurrences of ``:load`` or ``:reload``.
In particular, on adjacent invocations of ``:watch``, all but the last one
will have no effect on evaluation.

Given the whole idea is to have a hook into the 'compiled' event,
and AFAICT each module is compiled in one go, there's no smaller granularity
to hook into. On attempt to specify multiple bindings from the same module,
rather than arbitrarily pick one or otherwise combine them some, we should
error and force the user to explicitly compose them.

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

Both ghcid and ghciwatch respect ``.ghci`` and also allow passing GHCi commands
on a per-invocation basis, so I expect no change is needed for them to take advantage
of this feature. The same goes for any other haskell file-watcher that wraps
GHCi usage without restricting access to commands.

Costs and Drawbacks
-------------------
The prototype took a few days of work so far.
Remaining development time depends on how much the spec/implementation need to change.
As for maintenance costs, I think GHC devs would know much better than me.

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
It is already possible to avoid redundant evaluation by launching separate
tool/GHCi sessions - one for each binding of interest. However that then means
redundant compilation for modules included in multiple sessions, with the
slowdown growing both with the size of the codebase and the number of sessions,
since that means more overlap.

When dealing with a single GHCi session, current watcher tools allow observing
the value of multiple bindings by either

#. composing them into a new top-level binding
#. specifying them individually

With (1), when any watched binding changes, everything gets re-evaluated, which
is wasteful as any internal state (UI, metrics, cache, etc) will be reset.

While (2) can avoid said waste in principle, this requires each tool to have
logic to figure out which modules were reloaded (e.g. via reverse engineering
of GHCi terminal output), and other associated bookkeeping. Rather than have a
multitude of abstraction-inversion efforts, it's easier to add support upstream
where the relevant structure are already available, that is, in GHCi itself.

Unresolved Questions
--------------------
It is not always enough to qualify a name with a module name since using multiple
units makes it possible to have two modules with the same name.

My understanding is that since this is

* fairly rare
* `already a problem <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/14427#note_624644>`_

this proposal can ignore the concerns of that layer.

Implementation Plan
-------------------
I have a working prototype in a `draft PR <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/14440>`_
and am available for adapting/rewriting it as the proposal process develops.

Endorsements
-------------
