-Wsevere – erroring warnings
============================

.. author:: Joachim Breitner
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/571>`_.
.. contents::

It seems helpful if some warning categories (e.g. ``missing-methods``) to be
*errors* by default (as if ``-Werror=missing-methods`` was given), but still
allow them to be turns to warnings (``-Wwarn=missing=methods``) or quiet
(``-Wno-missing-methods``). This proposal
adds a new warning group ``-Wsevere`` (like ``-Wextra`` or ``-Wdefault``) for such warnings
and upgrades ``missing-methods``,  ``missing-fields`` from ``-Wdefault`` to ``-Wsevere``
(fixing `#544 <https://github.com/ghc-proposals/ghc-proposals/issues/544>`_)

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

  It seems to me we are doing our users a service if we make that particular foot a little less easy to fire accidentially
  
* The motivation for ``-Werror=missing-fields`` follows by dualty – instances are just records for implicitly passed parameters, in a way.

* From the need to have *some* warnings being errors by default follows the need for the suitable infrastructure, hence the
  ``-Wsevere`` warning group.
  
* Given that we have custom warning categories (`x-foo`, `#454 <https://github.com/ghc-proposals/ghc-proposals/pull/454>`_),
  we anticipate that users of custom warning categories will want a way to add “their” warning group to ``-Wsevere`` as well.
  
  Because there is no registry of custom categories, we let the prefix indicate the group. The prefix ``x-`` indicates a custom warning in ``-Wdefault``.
  This proposal allows the prefix ``xs-` to indicate that this group should be part of the severe group.
   
  
Proposed Change Specification
-----------------------------

* GHC learns a new warning group flag ``-Wsevere``, next to the existing ``-Wdefault``, ``-Wextra``, `-Wall`` and ``-Wcompat``.
  See _“5.2. Warnings and sanity-checking” <https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html>_.
  
  It behaves like the other groups in that ``-Wno=severe``, ``-Wwarn=severe`` and ``-Werror=severe`` behave as if the flag
  had been specified for each warning in the groups.
  
* The warning group is initialized with the following warning categories:

  * ``missing-methods``
  * ``missing-fields``

*  The set of allowed names of custom warning categories (`#541 <https://github.com/ghc-proposals/ghc-proposals/pull/541>`_) is
   extended by those prefixed with ``xs-*` (for “custom extended – severe”). The flags
   ``-Wno-severe``, ``-Wwarn=severe`` and ``-Werror=severe``
   also affect all custom warnings with a name starting in ``xs-*``.
 

Examples
--------
TODO


Effect and Interactions
-----------------------
TODO

Costs and Drawbacks
-------------------
In terms of GHC development, this is a modest extension of the existing warning category and group infrastructure.

In terms of breakage, @phadej has made an impact analysis at <https://github.com/ghc-proposals/ghc-proposals/issues/544#issue-1410125536>.


Alternatives
------------

* Naming the group.

  I suggest to use the name ``severe``, which does not have ``error`` in it (e.g. ``errors-by-default``), because else
  ``-Werror=errors-by-default`` or something looks kinda strange, and after someone says ``-Wwarn=severe``, the ``severe`` group still
  exists, but it not an error.
  
  I briefly considered ``-Wfatal``, but that’s a lie – these errors are *not* ``fatal``, else we couldn’t turn them
  into warnings.
  
* We could add more warnings to the group right away.

* In particular, we could make ``partial-type-signatures`` a normal warning, in ``-Wsevere`` by default, and ``-XPartialTypeSignatures``
  becomes a synonym for ``-Wwarn=partial-type-signatures``.
  
  (Or should ``-XPartialTypeSignatures`` be a flag that changes the *warning group* of ``partial-type-signatures``? Not sure).

* An additional motivation is the currently circulating idea that some features that are language *extensions* right now (``-XFoo``) can
  simply become part of the default “normal” language, together with a new warning category (``foo``) that’s off by default normally, and 
  the extension flags ``-XNoFoo`` or ``-XHaskell2010`` simply have the same effect as ``-Werror=foo``.
  
  Although now that I write it out, it seems that we don't actually need ``-Wsevere`` for that. Maybe it’s useful to *strongly deprecate* features,
  by introducing a warning about their use, and raising its severity from ``-Wcompat`` to ``-Wdefault`` to ``-Werror`` over time.

Unresolved Questions
--------------------
None yet.

Implementation Plan
-------------------
TODO
