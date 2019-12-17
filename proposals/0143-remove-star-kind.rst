Remove the * kind syntax
========================

.. author:: Vladislav Zavialov
.. date-accepted:: 2018-08-04
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/143>`_.
.. contents::

The Haskell Report uses ``*`` to denote the kind of lifted types. As we move
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
Unfortunately, little bits of complexity remained. Here are a few examples:

* We have to keep using words "type" or "kind" in error messages, so GHC
  continues to keep track of what level it is dealing with.
* Kind variables and type variables are treated differently in ``forall`` — this
  oddity is dealt with in an accepted proposal,
  `#24 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0024-no-kind-vars.rst>`_.
* Parsing ``*`` is different in types and kinds: in types it is a regular binary
  operator, but in kinds it denotes the kind of lifted types (unless ``-XTypeInType`` is
  enabled and then it must be imported from ``Data.Kind``) — this oddity is
  dealt with in an accepted proposal, `#20
  <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0020-no-type-in-type.rst>`_.

The point of interest for us is the ``*`` syntax. Before GHC 8.6 there were complicated
workarounds in the parser and the renamer to support it. The proposal
`#20 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0020-no-type-in-type.rst>`_,
implemented in GHC 8.6, changed this: now we have a more principled solution, an extension
called ``-XStarIsType`` that controls whether ``*`` is used to denote the kind
of lifted types or not, regardless of the syntactic category (types/kinds) and
scope (what is imported from ``Data.Kind``).

So here is the situation we are in:

* Before GHC 8.0: the only way to refer to the kind of lifted types was the
  ``*`` syntax.
* Since GHC 8.0, there is another way to call it: ``Type``. This is a regular
  Haskell identifier and not special syntax, ``Type`` is exported from
  ``Data.Kind``. The availability of the ``*`` syntax depends on the syntactic
  category (types or kinds), enabled extensions (is ``-XTypeInType`` on or off?)
  and scope (is ``type (*)`` imported from ``Data.Kind`` or not?).
* Since GHC 8.6, the rules are simple: with ``-XStarIsType``, unqualified ``*``
  is syntactic sugar for ``Data.Kind.Type``; with ``-XNoStarIsType``, it is a
  regular type operator.

The problem here is that ``-XStarIsType`` is enabled by default:

a) it creates a lexical inconsistency
b) it stands in the way of type/term unification
c) it creates two language dialects
d) it requires more background knowledge to read and understand

Let us now expand on each of these points.

a) The lexical inconsistency is demonstrated by the following example (courtesy of `@takenobu-hs <https://github.com/takenobu-hs>`_)::

      {-# LANGUAGE TypeOperators, StarIsType, PolyKinds, DataKinds #-}

      data T1 :: Either * Bool -> *

      data T2 :: Either + Bool -> *
      data a + b

   To an untrained eye, ``Either * Bool`` and ``Either + Bool`` look quite similar.
   However, ``Either * Bool`` is parsed as ``Either (*) Bool``; at the same time,
   ``Either + Bool`` is parsed as ``(+) Either Bool``.

   Furthermore, when ``-XTypeOperators`` and ``-XStarIsType`` are enabled at the
   same time, it is not possible to define the ``*`` operator or use it
   unqualified. This is problematic because even ``base`` defines ``*`` as
   type-level multiplication of natural numbers in ``GHC.TypeNats``.

b) Unification of terms and types is one of the goals of ``-XDependentHaskell``.
   Dependently typed languages such as Agda, Idris, or Coq, can freely use terms in
   their types. However, if we attempt to unify terms and types in Haskell, having
   ``-XStarIsType`` on by default means that ``*`` would be no longer available for
   multiplication on the term level (this is the same conflict as between
   ``-XTypeOperators`` and ``-XStarIsType`` on the type level). Removing ``*`` as a
   binary operator from the language would be a major breaking change, and one that
   is hard to justify. Therefore, ``-XStarIsType`` creates a syntactic conflict
   that holds back the development of a more important feature,
   ``-XDependentHaskell``.

c) The problem of two language dialects is summarized as follows. Code that
   uses type-level features heavily is likely to prefer ``-XNoStarIsType`` for its
   lack of conflict with ``-XTypeOperators`` and due to ``Type`` having precedent
   in other languages like Idris. At the same time, literature and code that tries
   to minimize the use of extensions will keep using ``*`` because it is the
   default, perhaps also out of habit. The end result is that no one will be able
   to tell how ``a * b`` parses in a particular module without looking at the
   enabled extensions (which are not necessarily in the module header).

d) The knowledge background point boils down to ``Type`` being a regular
   English word and a regular Haskell identifier which is not subject to special
   parsing rules. Without learning anything about it, an English-speaking person
   can pronounce it correctly and mentally parse a Haskell expression that uses it.
   With basic familiarity of Haskell syntax, anyone can deduce that if ``5 :: Int``
   means that ``5`` is an ``Int``, then ``Int :: Type`` must mean that ``Int`` is a
   ``Type`` (unlike ``Maybe``, which is not a type but a type constructor).

   At the same time, reading ``*`` requires prior introduction to this syntax.
   Novel syntax may be intimidating, and it does not help that in other contexts
   ``*`` stands for wildcards (in regular expressions), bullet points (in
   Markdown), multiplication (in arithmetic), and so on. It does take some time to
   rewire the brain to read ``*`` as ``Type``. Several people in the discussion
   thread of this proposal shared that their teaching and/or learning experience
   could be improved if instead of ``*`` we had ``Type``.

We therefore conclude that making ``-XStarIsType`` disabled by default and
eventually removing it from the language would:

a) make the language more lexically consistent
b) unblock further development in the direction of advanced type-level programming
c) avoid the mental overhead associated with having more language dialects
d) make the language more approachable for some people

Of course, there are costs we must consider.
The amount of code and literature that uses ``*`` is truly immense.
That is why we propose a slow migration on the timescale of a decade. Assuming
two releases of GHC per year (which is the currently accepted schedule), we will
be able to get rid of ``*`` in 8 years.

Proposed Change Specification
-----------------------------

In GHC 8.6, the ``-XStarIsType`` extension is enabled by default.
There is a warning, ``-fwarn-star-is-type``, disabled
by default. This warning is triggered whenever ``*`` is used to denote ``Type``::

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
* In the next release (or 8.5 years in), the ``-XStarIsType`` extension may be
  removed from GHC to simplify the internals.

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

* We can also reclassify ``★`` as an alphanumeric identifier. This will
  sacrifice the point about "no background knowledge", but we still get "lexical
  consistency", "no language dialects", and "no type/term conflicts". The
  advantages of ``★`` are its brevity and precedence in literature.

Unresolved questions
--------------------

None.

Implementation Plan
-------------------

Both ``-XStarIsType`` and ``-fwarn-star-is-type`` are already implemented
in GHC 8.6, the question is to when to enable or disable
these, which requires no real implementation effort.
