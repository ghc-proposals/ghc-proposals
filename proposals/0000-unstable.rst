``-XUnstable``
==============

.. author:: Richard Eisenberg
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/524>`_.
.. sectnum::
.. contents::

There are frequently new features in GHC that the GHC development team know are subject
to change. Though we try to advertise features as unstable in e.g. the user manual and
in release notes, we recognize it is impractical to expect every GHC user to read these
resources.

This proposal introduces a new extension ``-XUnstable``. Unstable features
will be available only when ``-XUnstable`` is specified. Unstable features might
change without notice, without a migration plan, and without a proposal, though
notice of any changes would appear in release notes. In addition, we might choose
to use this proposal process about unstable features, if collecting community
feedback would be helpful.

Moving
an unstable feature out from the unstable umbrella would require a proposal.

This proposal marks importing any module from ``ghc-prim`` as requiring ``-XUnstable``,
though we expect to expand the set of features affected by ``-XUnstable`` over time.

This proposal also includes a separate extension, ``-XStable``, which prevents
``-XUnstable`` from being used. A project's ``.cabal`` file might then specify
``-XStable`` to prevent any of the project's modules from using ``-XUnstable``.
There is no ``-XNoStable``, unlike most other extensions. This ``-XStable`` component
is optional and may be removed from this proposal without affecting the
utility of ``-XUnstable``.

Credit: This idea came up in a conversation between Richard Eisenberg and Simon
Peyton Jones, though Richard fleshed out the details and wrote this proposal.

Motivation
----------

``-XUnstable``
~~~~~~~~~~~~~~

It is frequently the case that the GHC developers know a feature is likely to
change soon (or not-so-soon), and yet it is hard to spread this knowledge to
the wider Haskell community. Examples:

* For years, we GHC developers knew that ``-XImpredicativeTypes`` was more of
  a bug than a feature. It had no specification, and any code that type-checked
  because of it was simply lucky. The feature was in essence unstable. Since
  GHC 9.2, however, a new, well-specified ``-XImpredicativeTypes`` has been
  available which would not be unstable.

* Even though the ``TYPE`` design has been in GHC for some time, it is still
  settling into place somewhat. This can be seen in the change from having
  ``LiftedRep`` and ``UnliftedRep`` as constructors of ``RuntimeRep`` instead
  of today's design of having them be synonyms for ``BoxedRep Lifted`` and
  ``BoxedRep Unlifted``, respectively. Behind the scenes, we knew this all
  might change, but GHC users built on these features expecting them to
  continue working.

* The first few versions of the ``-XPolyKinds`` extension were known to be
  incomplete. The definition of this extension evolved over time and became
  stable only after a full release cycle or two. A similar progression happened
  some years later with ``-XTypeInType``; the latter extension ended up getting
  deprecated, in the end.

These are all examples of features we introduced despite their known instability.
However, we might imagine that, with the ability to guard some features behind
a scary flag like ``-XUnstable``, we would have been even more bold in rolling
out other ideas. In many cases, we gain knowledge over time by having a feature
available; this knowledge can help inform the design of the feature. An excellent
example is ``-XOverloadedRecordDot``. This extension was essentially implemented
within Digital Asset's internal version of Haskell for some time before being proposed in GHC.
Through years of experimentation and user feedback, the design was refined. GHC then
benefited from this history in getting a new, yet battle-tested extension. If Digital
Asset had not invested in this way, though, we would have no way of gaining experience
without trapping ourselves in a potentially backward-incompatible corner. With ``-XUnstable``,
we might be able to roll out more features, learn from their use, and then refine.

``-XStable``
~~~~~~~~~~~~

Unstable features are, well, unstable. Consequently, some developers and enterprises
may wish to forbid these features in their code. It works best to have an automated
way of enforcing this prohibition. The ``-XStable`` extension does exactly this: it prevents
the use of ``-XUnstable`` (only). Now, a developer might put ``-XStable`` in its ``default-extensions``
field of a ``.cabal`` file to be sure that a package uses no unstable features of GHC.
Perhaps even more powerfully, a potential client perusing libraries on Hackage can view
``.cabal`` files and choose to use only those that have ``-XStable`` in their ``.cabal`` file,
suggesting that these libraries are more likely to continue compiling in future GHC versions.

Proposed Change Specification
-----------------------------

There are two components of this proposal: the ``-XUnstable`` extension and the
``-XStable`` extension. The components are separable (``-XUnstable`` makes sense
even without ``-XStable``), but ``-XStable`` depends on ``-XUnstable``.

``-XUnstable``
~~~~~~~~~~~~~~

Technical specification
#######################

1. Introduce a new extension ``-XUnstable``. This extension is off by default and
   not a part of any extension set.

#. When a module from the ``ghc-prim`` package is imported, if ``-XUnstable`` is not
   specified, report an error informing the user that modules from ``ghc-prim`` are
   unstable. After warning the user of the consequences, the error would inform users
   that specifying ``-XUnstable`` makes the error go away.

#. Remove ``FUN`` from the export list of ``GHC.Exts`` (so that ``FUN`` becomes unstable).

#. (Technically beyond the scope of this proposal process) The module-level documentation
   for each of the exported modules of ``ghc-prim`` will include a note that users
   should not import that module directly; instead, users will be directed where to
   find the imports they need.

#. (Technically beyond the scope of this proposal process) Tooling that helps
   users automatically insert extensions (i.e. HLS) would not automatically enable
   ``-XUnstable`` from this message; users would have to add it manually.

Non-technical specification
###########################

1. Features available only with ``-XUnstable`` are exempt from the GHC proposals process.
   That is, if a change to GHC is visible only to users that enable ``-XUnstable``, the
   change need not be a part of an approved proposal.

#. Features available only with ``-XUnstable`` may change between minor releases and without
   a migration strategy. Any such changes will be included in the release notes.

``-XStable``
~~~~~~~~~~~~

1. Introduce a new extension ``-XStable``.

#. Unlike the vast majority of other extensions, there is no ``-XNoStable``.
   Once ``-XStable`` is specified, the extension stays on.

#. If ``-XStable`` and ``-XUnstable`` are in force in the same module, report
   an error to the user and stop compilation.

Examples
--------

1. ::

     module Main where
     import GHC.Types
     main = putStrLn "Hello, world!"

   This would report an error because ``GHC.Types`` is a module in ``ghc-prim`` and ``-XUnstable`` is not specified.
   Here is a potential wording of the error::

     Import of `GHC.Types` not allowed.
     The `GHC.Types` module comes from the `ghc-prim` package, which is
     part of the internal, evolving implementation of GHC. Its exports may
     change between releases, and importing this module is not recommended.
     Many of its exported definitions are available through importing
     `GHC.Exts` instead. You may also enable the Unstable extension to
     suppress this message.

2. ::

     {-# LANGUAGE Unstable #-}
     module Main where
     import GHC.Types
     main = putStrLn "Hello, world!"

   This module is accepted.

3. ::

     {-# LANGUAGE Unstable #-}
     module Main where
     import GHC.Types
     main = putStrLn "Hello, world!"

     > ghc -XStable Main.hs

   This reports an error, because ``-XStable`` and ``-XUnstable`` have mixed.
   Here is a potential wording of the error::

     The extension Stable was enabled on the command-line. This
     prevents the extension Unstable (enabled in a LANGUAGE pragma)
     from being enabled. Enabling Stable is often done to prevent
     modules from using Unstable features; please consider removing
     Unstable from your file.

Effect and Interactions
-----------------------

1. Users can now discover that ``ghc-prim`` exports are unstable, and how
   to get the definitions they want without importing a ``ghc-prim`` module.

#. GHC developers gain more latitude to experiment with the part of the
   language available only with ``-XUnstable``.

#. Though this proposal discusses only ``ghc-prim`` modules, it is expected
   that the set of features controlled by ``-XUnstable`` will grow, possibly
   also expanding to accommodate user-directed instability annotations. Any
   such changes would have to go through subsequent proposals, because they
   remove the ability of code without ``-XUnstable`` to use a feature.

#. The current modules exported from ``ghc-prim`` are::

     GHC.CString
     GHC.Classes
     GHC.Debug
     GHC.Magic
     GHC.Magic.Dict
     GHC.Prim
     GHC.Prim.Ext
     GHC.Prim.Panic
     GHC.Prim.Exception
     GHC.Prim.PtrEq
     GHC.PrimopWrappers
     GHC.Tuple
     GHC.Types

   The recommended way to access exports from many of these is to get them from ``GHC.Exts`` or
   other modules in the ``base`` package. However,
   the following identifiers are exported from ``ghc-prim`` but not elsewhere::

     GHC.Debug.debugLn
     GHC.Debug.debugErrLn
     GHC.Prim.Panic.absentSumFieldError
     GHC.Prim.Panic.panicError
     GHC.Prim.Panic.absentError
     GHC.Prim.Exception.raiseOverflow   -- other similar functions are exported by "ghc-bignum".GHC.Num.Primitives
     GHC.Tuple.getSolo                  -- other similar definitions are exported by "base".GHC.Tuple

   Any user of any of these exports will have no backward-compatible way forward if
   this proposal is accepted and implemented, other than specifying ``-XUnstable``
   (which isn't backward compatible). If we believe that any of the above functions
   are used beyond GHC itself, we should introduce a migration period.

#. The ``FUN`` type is currently exported from ``GHC.Exts``, but this export would
   be removed as part of this proposal. Accordingly, any user of ``FUN`` will have
   a hard breakage, where they have to now import ``GHC.Prim`` (to get ``FUN``) and
   have to use CPP to specify ``-XUnstable``.

   ``FUN :: forall (m :: Multiplicity) -> forall {r1 :: RuntimeRep} {r2 :: RuntimeRep}. TYPE r1 -> TYPE r2 -> Type``
   is the primitive function type, but its current kind is likely to change.

Costs and Drawbacks
-------------------

1. Some have argued that GHC language extensions should be a way of guarding features
   that have not yet made it into the next language standard. Instead, the ``-XUnstable``
   and ``-XStable`` extensions are more like configuration options than language extensions.
   Accepting this proposal moves us further from the vision that some future version of
   Haskell will subsume all extensions.

#. The naming of these extensions may be confusing. In particular, users may wonder
   what the difference between ``-XNoUnstable`` and ``-XStable`` is, or think that
   (the non-existent) ``-XNoStable`` is the same as ``-XUnstable``.

Alternatives
------------

1. Instead of making a new language extension, we could imagine a new warning ``-Wunstable-features``.
   This warning would be werror-by-default (no warnings are like this today) but could be disabled
   with ``-Wno-unstable-features``. Similarly, ``-XStable`` could become something like ``-fstable``,
   but this seems harder to specify in a ``.cabal`` file.

Unresolved Questions
--------------------

1. Can we implement this without a migration plan? That is, are any of the identifiers
   listed above used in practice in a way that would be painful if this proposal were
   to be implemented?
