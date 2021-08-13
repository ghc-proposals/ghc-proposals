Fine-Grained Unused Warnings
============================

.. author:: Jakob Brünker
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/434>`_.
.. contents::

This proposal aims to make warnings about unused names more precise,
consistent, and configurable.


Motivation
----------

::

  a :: Int
  a = 4
    where bar = foo
          foo = 3

Compiling the above code with ``-Wall`` results in the
these warnings:

::

      Defined but not used: `bar'
      Defined but not used: `foo'

Sometimes, I look at a warning like this and think "I could have sworn I used
``foo`` somewhere" - but of course, what the warning is trying to say isn't
actually that ``foo`` is unused, but rather that all of ``foo``'s usage sites
are in unused bindings.

Furthermore, while it's useful to see such a warning for CI, to immediately
know all of the bindings that have to be removed to eliminate the warnings,
during development, it would be enough for me to see a warning about the
binding that's actually unused; the rest serves mostly to clutter up valuable
screen real estate in ghcid.

On the other hand, for a module like

::

  module Main (main) where

  import Control.Monad (when)

  main = pure ()

  foo cond = when cond (putStrLn "foo")

GHC will warn about ``foo`` being unused, but it will not warn about the import
being unused, even though all it's usages are in unused bindings. But here,
too, it would be useful to know that the import is unused during CI.

A further inconsistency is that in

::

  import Control.Monad (when, when)
  import Control.Monad (when, when)

  foo cond = when cond (putStrLn "foo")

GHC will warn about the second import, but not about the second occurrence in
the import list of ``when``.

Proposed Change Specification
-----------------------------
The overarching idea is to provide a consistent set of warnings about unused
names, that are disabled by default, enabled by ``-Wall``, and can be
configured to only show genuinely unused names.

This affects the following warnings:

* ``-Wunused-top-binds``: Warn if a top-level binding is not exported and does
  not appear in another definition.
* ``-Wunused-local-binds``: Warn if a local binding does not appear in another
  definition.
* ``-Wunused-pattern-binds``: Warn if a pattern does not bind any names and is
  not a wild-card or bang pattern.
* ``-Wunused-binds``: Alias for the three warnings above.
* ``-Wunused-matches``: Warn if a name bound in a term-level pattern binding
  does not appear in the right-hand side of a definition.
* ``-Wunused-imports``:

  * Warn if no bindings from an import are used and the import has no import
    list or a non-empty import list.
  * Warn if any individual name in an import list is not used.
  * Warn if a name or module is imported twice. This does not apply if the
    imports have different effects, e.g. one qualified and one unqualified
    import. This is a change from the status quo, in which only repeated
    modules would trigger this warning, not repeated individual names in the
    import list. Furthermore, if a module has already been imported, the status
    quo is to not trigger this warning if a second import has an empty import
    list. Under this proposal, such a situation will also trigger this
    warning.

For each of these warnings, this proposal adds a transitive version, denoted by
the name followed by a ``-transitive`` suffix (e.g. the transitive version of
``-Wunused-binds`` is ``-Wunused-binds-transitive``. Rather than being
triggered when a name is not used, this warnings will be triggered when a name
is "transitively unused". A name is transitively unused if all of its usage
sites are in unused or transitively unused definitions.

An exception is ``-Wunused-pattern-binds``. A transitive version would not make
sense, since the warning is about *not* binding to a name.

The warning messages will be worded identically to their non-transitive
counterparts, except that occurrences of "not used", "unused", and "redundant"
will be replaced by "used only in unused or unreachable bindings", potentially
accompanied by minor adjustments to the phrasing to make the message work
grammatically. "Unreachable" in this case is effectively another way of saying
"transitively unused but not (directly) unused".

These warnings will have no effect if the corresponding non-transitive warning
isn't enabled.

Furthermore, the warning ``-Wunused-transitive`` is provided as an alias for

* ``-Wunused-binds-transitive``
* ``-Wunused-matches-transitive``
* ``-Wunused-foralls-transitive``
* ``-Wunused-imports-transitive``

This means that all transitive warnings can be turned off at once with
``-Wno-unused-transitive``.

Related warnings that are not affected by this proposal are
``-Wunused-packages`` and ``-Wunused-type-patterns``. These warnings are not
enabled by ``-Wall``, and don't require a transitive counterpart.

``-Wunused-do-bind`` also remains unaffected, and does not need a transitive
version for the same reason as ``-Wunused-pattern-binds`` above.

Examples
--------

::

  module Main (main) where

  import Prelude (Bool, IO, putStrLn, putStrLn)
  -- warning: [-Wunused-imports]
  -- The import of `putStrLn' from module `Prelude' is redundant

  import Prelude ()
  -- warning: [-Wunused-imports]
  -- The import of `Prelude' is redundant

  import Control.Monad (when)
  -- warning: [-Wunused-imports-transitive]
  -- `when', imported from module `Control.Monad`, is only used in unused or
  -- unreachable bindings

  foo :: forall a . (forall (b :: a) . Bool -> IO ())
  -- warning: [-Wunused-foralls-transitive]
  -- Quantified variable `a' is only used in unused or unreachable bindings

  -- warning: [-Wunused-foralls]
  -- Unused quantified variable `(b :: a)'

  foo cond = when cond (putStrLn "foo")
  -- warning: [-Wunused-top-binds-transitive]
  -- Defined but only used in unused or unreachable bindings: `foo'

  bar = foo
  -- warning: [-Wunused-top-binds]
  -- Defined but not used: `bar'

  main = putStrLn "Hello, World!"

Effect and Interactions
-----------------------
For the most part, the effects of this proposal are minor. Adding the
transitive warnings to ``-Wall`` means that the only differences for existing
code-bases are that the warning message GHC prints for the transitive warnings
is different, and that GHC will print a few more warnings about unused names
to be consistent with the previously existing warnings.

Costs and Drawbacks
-------------------
None that I can see.

Alternatives
------------
* It would be possible to (assuming ``-Wall``) make the transitive warnings
  opt-in rather than opt-out. This would make the proposal less
  backwards-compatible, since most of the transitive warnings are currently
  part of the unused warnings and thus enabled by ``-Wall``.

* Instead of having no effect when the corresponding unused warning is turned
  off, transitive warnings could imply their non-transitive counterparts, or
  GHC could show only transitive warnings (but no warnings about geniwuinely
  unused bindings). Which of these behaviors would be the least surprising is
  somewhat subjective.

* Instead of having the ``-transitive`` suffix, the word ``unused`` in the
  transitive warnings could be replaced by ``unreachable``.

Implementation Plan
-------------------
I can implement it.
