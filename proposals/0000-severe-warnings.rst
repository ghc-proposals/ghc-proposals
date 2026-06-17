-Wsevere – erroring warnings
============================

.. author:: Oleg Grenrus, Joachim Breitner
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/571>`_.
.. sectnum::
.. contents::

It seems helpful for some warnings (e.g. ``missing-methods``) to be
*errors* by default (as if ``-Werror=missing-methods`` was given), but still
allow them to be turned into warnings (``-Wwarn=missing=methods``) or being disabled
(``-Wno-missing-methods``). This proposal
adds a new warning group ``-Wsevere`` (like ``-Wextra`` or ``-Wdefault``) for such warnings
and upgrades ``missing-methods``,  ``missing-fields`` from ``-Wdefault`` to ``-Wsevere``
(fixing `#544 <https://github.com/ghc-proposals/ghc-proposals/issues/544>`_).

Motivation
----------

* We can start for the motivation for making ``missing-methods`` an error by default, copying from @phadej’s
  `#544 <https://github.com/ghc-proposals/ghc-proposals/issues/544>`_:
  
    Currently omitting method implementations in type-class instances
    is just a warning, but it is enabled by default.
    Therefore one could argue that adding a new method to existing
    type-class is not a breaking change.
    However, it very much is, but

    *  ecosystem is not forced to catch up in any timely matter
    *  the breakage is hard to assess

  and (to me even more compelling):
  
    FWIW, base-4.18.0.0 is going to have

    Add default implementation of `(<>)` in terms of `sconcat` and `mempty` in terms of `mconcat`.
    which makes it possible to write::

      instance Semigroup Foo
      instance Monoid Foo

    and that instance's mempty will loop.
    Currently GHC will just warn if someone writes such (new) code by accident.

  It seems to me we are doing our users a service if we make that particular foot gun
  a little less easy to fire accidentally.
  
* The motivation for ``-Werror=missing-fields`` follows by duality – instances are just records for implicitly passed parameters, in a way.

* Making these things errors is a deviation from the Haskell Report, and now some Haskell2010 programs will no longer compile out of the box.
  We nevertheless propose to make that change even with ``-XHaskell2010`` (i.e. don’t introduce a language extension ``NoMissingFieldsOrMethods`` for
  this behavior). One reason is given by Oleg:
  
    not having ``-Wsevere=missing-methods`` by default essentially prevents any (true) breakage assessment of adding new, non-defaulted members to
    existing type-classes.
  
  Users who want to continue using idioms involving undefined fields and methods will be told by the error message that they can use
  ``-Wno-missing-methods`` to get that back. Also, any Haskell2010 program that continues to compile has its semantics unchanged, it “just” means
  that GHC by default rejects some Haskell2010 programs.
  
  This is a deviation from the usual “deviations from the report must be guarded by language extension flags“ rule, so extra committee scrutiny
  is advised.
   
* From the need to have *some* warnings being errors by default it follows that it’s helpful to give them a name, for conceptualization.
  Hence we introduce the ``-Wsevere`` warning group (like ``-Wdefault``). This implies that ``-Wwarn=severe`` can be used to demote them
  to mere warnings, ``-Wno-severe`` to turn them off, and ``-Werror=severe`` to get back to the default state.
  
* Given that we have custom warning categories (``x-foo``, `#541 <https://github.com/ghc-proposals/ghc-proposals/pull/541>`_),
  we anticipate that users of custom warning categories will want a way to add “their” warning category to the ``-Wsevere`` group as well.
  
  Because there is no registry of custom categories, we let the prefix indicate the group. The prefix ``x-`` indicates a custom warning in ``-Wdefault``.
  This proposal allows the prefix ``xs-`` to indicate that this group should be part of the severe group.
   
  
Proposed Change Specification
-----------------------------

* GHC learns a new warning group flag ``-Wsevere``, next to the existing ``-Wdefault``, ``-Wextra``, ``-Wall`` and ``-Wcompat``.
  See `“5.2. Warnings and sanity-checking” <https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html>`_.

  It behaves like the ``compat`` group in that ``-Wno-severe``, ``-Wwarn=severe`` and ``-Werror=severe`` behave as if the flag
  had been specified for each warning in the groups.

  Warning categories in this group are on and erroring by default (as if ``-Werror=severe`` was the first argument).
  
* The warning group is initialized with the following warning categories:

  * ``missing-methods``
  * ``missing-fields``

  They are no longer part of ``-Wdefault``.

*  The set of allowed names of custom warning categories (`#541 <https://github.com/ghc-proposals/ghc-proposals/pull/541>`_) is
   extended by those prefixed with ``xs-*`` (for “custom extended – severe”). The flags
   ``-Wno-severe``, ``-Wwarn=severe`` and ``-Werror=severe``
   also affect all custom warnings with a name starting in ``xs-*``.

* For uniformity, the warning groups ``default``, ``extra``, ``all`` and ``everything`` can also be used in ``-Wno-<group>``,
  ``-Wwarn=<group>``  and ``-Werror=<group>``. (This is already implemented in `MR9679 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/9679>`_).

Examples
--------

Consider file ``Test.hs``::

  module Test where
  data Foo = Foo { a :: Int, b :: Bool }
  foo = Foo { a = 1 }

We would get::

  $ ghc Test.hs
  [1 of 1] Compiling Test             ( Test.hs, Test.o )
  
  Test.hs:3:9: error: [-Wmissing-fields, -Werror=missing-fields]
      • Fields of ‘Foo’ not initialised:
          b :: Bool
      • In the expression: Foo {a = 1}
        In an equation for ‘foo’: foo = Foo {a = 1}
    |
  3 |   foo = Foo { a = 1 }
    |         ^^^^^^^^^^^^^
  $ echo $?
  1
  $ ghc -Wwarn=missing-fields Test.hs
  [1 of 1] Compiling Test             ( Test.hs, Test.o )
  
  Test.hs:3:9: warning: [-Wmissing-fields]
      • Fields of ‘Foo’ not initialised:
          b :: Bool
      • In the expression: Foo {a = 1}
        In an equation for ‘foo’: foo = Foo {a = 1}
    |
  3 |   foo = Foo { a = 1 }
    |         ^^^^^^^^^^^^^
  $ echo $?
  0
  $ ghc -Wno-missing-fields Test.hs
  $ echo $?
  0

Disabling the ``default`` warnings, but not the ``severe`` warnings, will leave the ``severe`` warnings on::

  $ ghc Test.hs -Wno-default
  [1 of 1] Compiling Test             ( Test.hs, Test.o )
  
  Test.hs:3:9: error: [-Wmissing-fields, -Werror=missing-fields]
      • Fields of ‘Foo’ not initialised:
          b :: Bool
      • In the expression: Foo {a = 1}
        In an equation for ‘foo’: foo = Foo {a = 1}
    |
  3 |   foo = Foo { a = 1 }
    |         ^^^^^^^^^^^^^

The semantics of when these warnings are triggered are unaffected. In particular, `{-# MINIMAL -#}` pragmas are still taken into account::

  $ cat Test.hs
  module Test where
  class Foo a where
      foo :: a
      foo = bar
      bar :: a
      bar = foo
      {-# MINIMAL foo | bar #-}
  instance Foo Int
  instance Foo () where foo = ()
  $ ghc Test.hs
  [1 of 1] Compiling Test             ( Test.hs, Test.o )
  
  Test.hs:8:10: error: [-Wmissing-methods, -Werror=missing-methods]
      • No explicit implementation for
          either ‘foo’ or ‘bar’
      • In the instance declaration for ‘Foo Int’
    |
  8 | instance Foo Int
    |          ^^^^^^^
  

Effect and Interactions
-----------------------
None yet.


Costs and Drawbacks
-------------------
In terms of GHC development, this is a modest extension of the existing warning category and group infrastructure.

Backward Compatibility
----------------------
We assess the expected impact on existing code as follows:

3. Breakage in uncommon cases (e.g. a few Stackage packages may break)

@phadej has made an impact analysis for ``-Werror=missing-methods`` <https://github.com/ghc-proposals/ghc-proposals/issues/544#issue-1410125536>
and one for ``-Werror=missing-fields``` <https://github.com/ghc-proposals/ghc-proposals/issues/544#issuecomment-1279948737>.

  22 packages out of nearly 3000 in the build plan [had] missing methods.
  In other word the impact isn't huge, and in most cases easy to fix.

This breakage may be warranted by the gains from this change, assuming it’s better for builds to begin to fail with an error after a dependency change, than to compile but error and loop at runtime. 


Alternatives
------------

* Naming the group.

  I suggest to use the name ``severe``, which does not have ``error`` in it (e.g. ``errors-by-default``), because else
  ``-Werror=errors-by-default`` or something looks kinda strange, and after someone says ``-Wwarn=severe``, the ``severe`` group still
  exists, but it not an error.
  
  I briefly considered ``-Wfatal``, but that’s a lie – these errors are *not* ``fatal``, else we couldn’t turn them
  into warnings.

* We could leave out ``missing-fields``.

  It is less severe than ``missing-methods`` (no possibly recursive default methods, clear runtime error), so we could leave it out if we
  want to tread more carefully.
  
* We could add more warnings to the group right away.

* In particular, we could make ``partial-type-signatures`` a normal warning, in ``-Wsevere`` by default, and ``-XPartialTypeSignatures``
  becomes a synonym for ``-Wwarn=partial-type-signatures``.
  
  (Or should ``-XPartialTypeSignatures`` be a flag that changes the *warning group* of ``partial-type-signatures``? Not sure).

* An additional motivation is the currently circulating idea that some features that are language *extensions* right now (``-XFoo``) can
  simply become part of the default “normal” language, together with a new warning category (``foo``) that’s off by default normally, and 
  the extension flags ``-XNoFoo`` or ``-XHaskell2010`` simply have the same effect as ``-Werror=foo``.
  
  Although now that I write it out, it seems that we don't actually need ``-Wsevere`` for that. Maybe it’s useful to *strongly deprecate* features,
  by introducing a warning about their use, and raising its severity from ``-Wcompat`` to ``-Wdefault`` to ``-Werror`` over time.

* We could guard this change behind a suitable language extension, so that ``Haskell2010`` stays untouched. It could be the default eventually, but
  would not affect code under ``Haskell2010`` or ``GHC2021``.
  
  It would set precedent for language extensions changing the default mode (warning vs. errors) of warnings, and I’d propose that the semantics
  would be that all language flags (``-X``) are processed, from that the default on/off and error/warn sets are derived, and then all ``-W`` flags
  are processed, so that ``-X`` and ``-W`` flags commute.
  
  But as explained in the motivation the motivation comes from imposing this more rigid discipline on existing code, and so no language extension
  is being proposed at this point.

Unresolved Questions
--------------------
None yet.

Implementation Plan
-------------------
None yet.
