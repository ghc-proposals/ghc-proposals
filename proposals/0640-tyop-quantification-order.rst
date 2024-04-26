Fix quantification order for (a `op` b) and (a %m -> b)
=======================================================

.. author:: Vladislav Zavialov
.. date-accepted:: 2024-04-17
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/23764
.. implemented:: 9.12
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/640>`_.
.. sectnum::
.. contents::

This proposal changes the order of implicit quantification for type variables
occurring (1) as type operators ``a `op` b``, (2) in multiplicity annotations ``a %m -> b``.

It is essentially a bugfix. However, it is also a breaking change, so we wish
to bring the attention of the committee to it in order to discuss the
appropriate migration strategy.

Motivation
----------
The order of implicit quantification is supposed to be left-to-right. For
example, ``f :: a -> b`` is a shorthand for ``f :: forall a b. a -> b`` rather
than ``f :: forall b a. a -> b``.

The order of quantification matters in the context of ``TypeApplications``. The decision to quantify variables in left-to-right order can be traced back to the original paper `Visible Type Application <https://richarde.dev/papers/2016/type-app/visible-type-app.pdf>`_ (Richard A. Eisenberg, Stephanie Weirich, and Hamidhasan Ahmed. ESOP 2016, Eindhoven, The Netherlands). See section 3.1 of the said paper, that states the following:

  Haskell allows programming to omit variable quantification, allowing a type signature like
  ::

    const :: a → b → a          -- NB: no ∀

  Are these variables specified? We have decided that they are. There is a very
  easy rule at work here: just order the variables left-to-right as the user wrote
  them. We thus consider variables from type signatures to be specified, even when
  not bound by an explicit ``∀``.

This is confirmed by the GHC User's Guide, section 6.4.16. "Visible type application":

  Only specified type variables are available for instantiation with visible
  type application. An example illustrates this well:
  ::

    f :: (Eq b, Eq a) => a -> b -> Bool

  When GHC is figuring out how to process a visible type application, it must
  know what variable to instantiate. It thus must be able to provide an ordering
  to the type variables in a function’s type.

  If the user has supplied a type signature, as in ``f``, then this is easy: we
  just take the ordering from the type signature, going left to right and using
  the first occurrence of a variable to choose its position within the
  ordering. Thus, the variables in ``f`` will be ``b``, then ``a``.

The problem this proposal aims to fix is that the actual implementation of
implicit quantification in GHC does not adhere to the left-to-right rule in two
cases:

1. Type operators ``a `tyop` b``.
   Expected quantification order (left-to-right): ``forall a tyop b.``;
   actual quantification order: ``forall tyop a b.``.

2. Multiplicity annotations ``a %p -> b``.
   Expected quantification order (left-to-right): ``forall a p b.``;
   actual quantification order: ``forall a b p.``.

It is rather clear that this is nothing more than a bug. It is also the case
that fixing this bug constitutes a breaking change. The fix is rather
straightforward and available at `GHC MR !11036 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/11036/>`_.

The question is whether we want to ship the fix right away or introduce a
compatibility warning first and wait a few releases.

We argue that there is no need for the warning, as there is practically no code
in the wild that relies on the current buggy behavior. See the impact
assessment in the "Backward Compatibility" section.

Proposed Change Specification
-----------------------------
* Use left-to-right order in implicit quantification of type variables,
  including variables that occur as type operators or multiplicity annotations.

* Effect the fix immediately, without a compatibility warning or a wait period.


Examples
--------

1. Type variable used as a type operator:
   ::

     f :: a `op` b

     vta_old = f @(,) @Int @Bool     -- before the change
     vta_new = f @Int @(,) @Bool     -- after the change


2. Type variable used as a multiplicity annotation:
   ::

     h :: a %m -> b
     h = ...

     vta_old = h @Int @Bool @One     -- before the change
     vta_new = h @Int @One @Bool     -- after the change

Effect and Interactions
-----------------------
The fix makes the implementation adhere to the specification laid out in the
paper and the User's Guide.

Costs and Drawbacks
-------------------
No known costs or drawbacks, other than potential breakage in exotic cases.

Backward Compatibility
----------------------
Technically, this is a breaking change, as demonstrated by the examples in the
"Examples" section. At the same time, we expect that practically no code in the
wild relies on the current buggy behavior.

To estimate the actual impact, we patched GHC 9.6 to use the proposed implicit
quantification rules and compiled 3337 packages (`full list <https://gist.github.com/int-index/e9d305961d5540f1519492c956e3e65c>`_).
There have been no build failures, confirming our hypothesis that the breakage
is observed only in artificial examples.

Alternatives
------------
1. Keep the current (incorrect) quantification order. This means adding a
   special case to the specification instead of changing the implementation.

2. Do the busywork of implementing a warning, waiting a few releases, only then
   fixing the bug, and finally deprecating the compatibility warning.

3. Use different strategies for ``a %m -> b`` and ``a `op` b``, since
   ``LinearTypes`` is an experimental extension, whereas ``TypeOperators`` is not.

Implementation Plan
-------------------
`GHC MR !11036 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/11036/>`_ implements the fix.
