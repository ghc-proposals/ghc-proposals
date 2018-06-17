Remove the * kind syntax
========================

.. proposal-number::
.. trac-ticket::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/143>`_.
.. sectnum::
.. contents::

The Haskell Report uses ``*`` to denote the kind of inhabited types. As we move
towards DependentHaskell, it is increasingly painful to support this historical
choice of name. We propose to slowly and carefully remove this syntactic oddity
from the language.


Motivation
----------

Before GHC 8.0, there used to be three completely separate syntactic categories:
terms, types, and kinds. There also was a fourth layer (sometimes called
"sorts") that classified kinds and it had only one thing in it, called ``BOX``.
The addition of ``-XTypeInType`` allowed us to significantly simplify the
language by collapsing the tower of types, kinds, and ``BOX`` into just types.
Unfortunately, there are
little bits of complexity left over from the three layers showing up here and
there:

* We have to keep using words "type" or "kind" in error messages, so GHC
  continues to keep track of what level it is dealing with.
* Kind variables and type variables are treated differently in ``forall`` — this
  oddity is dealt with in an accepted proposal,
  `#24 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0024-no-kind-vars.rst>`_.
* Parsing ``*`` is different in types and kinds: in types it is a regular binary
  operator, but in kinds it denotes inhabited types (unless ``-XTypeInType`` is
  enabled and then it must be imported from ``Data.Kind``) — this oddity is
  dealt with in an accepted proposal, `#20
  <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0020-no-type-in-type.rst>`_.
* ... etc

So, what is the deal with ``*`` here? Before GHC 8.6 there were complicated
workarounds in the parser and the renamer to support it. At the time of
submitting this proposal, we have a more principled solution, an extension
called ``-XStarIsType`` that controls whether
``*`` is used to denote the kind of inhabited types or not, regardless of the
syntactic category (types/kinds) and scope (what is imported from
``Data.Kind``). The problem here is that ``-XStarIsType`` is going to
be enabled by default.

* The ``*`` syntax may be confusing to some
  beginners. There are cases where people familiar with regular
  expressions mistake ``*`` for a wildcard, assuming a subtyping (subkinding)
  relationship between ``*`` and other kinds (we actually used to have subkinding
  in the form of ``OpenKind``, but it was another beast entirely and is now
  replaced by runtime representation polymorphism).

* ``*`` conflicts with ``-XTypeOperators``. We can have infix operators
  like ``+`` or ``-`` in types and kinds, and yet ``*`` is not infix in kinds, so
  ``Either * Bool`` actually parses as ``Either (*) Bool``, not as ``(*) Either
  Bool``. At the same time, in types ``*`` *is* an infix operator, so we can write
  ``type Ten = 2 * 5``. In order to truly unify types and kinds, we have to give
  up either ``*`` as an infix operator (and that would be rather odd) or give up
  ``*`` as syntax for ``Data.Kind.Type``. Having an extension, ``-XStarIsType``,
  to alternate between these decisions, is a smart solution in the short term, but
  unnecessarily creates two incompatible language dialects if we decide to keep it.

* If we have any hope in merging the parsers for terms and types (which
  would be definitely a good thing for DependentHaskell), having ``-XStarIsType``
  on by default would mean that ``*`` would be no longer available even for
  term-level multiplication, which is hard to justify.

* ``-XStarIsType`` creates an unfortunate lexical inconsistency,
  demonstrated in the following example by `@takenobu-hs <https://github.com/takenobu-hs>`_::

    {-# LANGUAGE TypeOperators, PolyKinds, DataKinds #-}

    -- The `*` is a kind for lifted types.
    data T1 :: Either * Bool -> *

    -- The `+` is an infix type operator.
    data T2 :: Either + Bool -> *
    data a + b

Therefore, we have two groups of programmers, both of which would benefit from
the removal of ``*``: beginners, trying to make sense of kinds, and experienced
programmers using type operators.

Sadly, we cannot simply pull the plug and remove the ``*`` kind from the
language. The amount of code and literature that uses ``*`` is truly immense.
That is why we propose a slow migration on the timescale of a decade. Assuming
two releases of GHC per year (which is the currently accepted schedule), we will
be able to get rid of ``*`` in 8 years.

Proposed Change Specification
-----------------------------

In GHC 8.6, the ``-XStarIsType`` extension is enabled by default, but disabled
by ``-XTypeOperators``. There is a warning, ``-fwarn-star-is-type``, disabled
by default. This warning is triggered whenever ``*`` is used to denote the
kind of inhabited types::

    ghci> :k *
    <interactive>:1:1: warning: [-Wstar-is-type]
        Using ‘*’ (or its Unicode variant) to mean ‘Data.Kind.Type’
        relies on the StarIsType extension, which will be deprecated
        in the future. Use ‘Type’ from ‘Data.Kind’ instead.

We specify the deprecation schedule in both release count and amount of time
passed since GHC 8.6 has been released. In case releases are delayed, the
time-based schedule takes precedence.

* In the next release (or 0.5 years in), GHC 8.8, add ``-fwarn-star-is-type`` to ``-Wcompat``.
* For one more release, do nothing. At this point, the warning has been
  available for three releases (GHC 8.6, GHC 8.8, GHC 8.10), and included in
  ``-Wcompat`` for the last two.
* In the next release (or 1.5 years in), add ``-fwarn-star-is-type`` to ``-Wall``.
* For two more releases, do nothing.
* In the next release (or 3 years in), enable ``-fwarn-star-is-type`` by default.
* For seven more releases, do nothing.
* In the next release (or 7 years in), disable ``-XStarIsType`` by default and deprecate it.
* For two more releases, do nothing.
* In the next release (or 8.5 years in), remove ``-XStarIsType`` from GHC
  to simplify the internals.

Effect and Interactions
-----------------------

Breakage estimation
^^^^^^^^^^^^^^^^^^^

We estimate that less than 25% of packages published on Hackage will be affected
by this breaking change (see the discussion for the methods used).

* The breakage is not silent: the compiler will output error messages with useful hints.
* There will be a point in time when packages can support the last 7 years of GHC releases
  and all future releases without `-XCPP`. Packages that only support GHC 8.0 and higher can
  migrate right away without any use of `-XCPP`.

Adjustment to `#20 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0020-no-type-in-type.rst>`_
^^^^^^^^^^^^^^^^^^

As it stands, we have the following plan in `#20 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0020-no-type-in-type.rst>`_:

  For two releases, ``-XTypeOperators`` will imply ``-XNoStarIsType``

this gets simplified to:

  ``-XTypeOperators`` will imply ``-XNoStarIsType``

as it is counter-productive to re-enable ``-XStarIsType`` in two releases if
the end-goal is to remove it from the language.

Costs and Drawbacks
-------------------

Existing literature becomes outdated. However, on the proposed timescale and with
good hints from the compiler, we believe this will be a non-problem.

Some people consider ``Type`` too long and importing it from ``Data.Kind`` too
bothersome. Shortening it is a matter of a type synonym, for instance Agda
programmers can define ``type Set = Type``. As to the annoying import, adding
``Type`` to the ``Prelude`` can be discussed separately.

Alternatives
------------

* Keep ``-XStarIsType`` enabled by default forever, effectively maintaining two dialects
  of Haskell with different meaning of ``*``.

Unresolved questions
--------------------

None.

Implementation Plan
-------------------

Both ``-XStarIsType`` and ``-fwarn-star-is-type`` are already implemented and
will hopefully land in GHC 8.6, the question is to when to enable or disable
these, which requires no real implementation effort.
