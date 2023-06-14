Disallow constraints in kinds
=============================

.. author:: Richard Eisenberg and Simon Peyton Jones
.. date-accepted:: 2022-11-25
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/22298
.. implemented:: 9.8
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/547>`_.
.. sectnum::
.. contents::

GHC currently allows types like ``T`` here::

  type family F a where
    F Int = False
    F _   = True

  type T :: forall a -> (F a ~ True) => Type
  data T a = MkT a a

Note that ``T``\'s kind includes an equality. This declaration means that we
disallow ``T Int`` while allowing ``T`` to be instantiated at any other type
(of kind ``Type``).

This proposal seeks to remove this syntax, as it barely works, is hard to maintain,
and is not so useful.

Motivation
----------

A kind equality like the one in the introduction is very much like a datatype
context: the equality is required to be satisfied in order to work with the type,
the equality proof itself is never used. Put another way, an equality like this
only restricts the programs you can write, never allowing new programs to be
written.

While it might be nice to eagerly forbid a type like ``T Int`` from being written,
the benefit is slight (but real).

On the other hand, the challenge in continuing to accept this syntax is real,
and currently biting. For example, the declaration in the introduction is accepted,
but the following is currently rejected::

  type T :: forall a -> (F a ~ True) => Type
  data T a where
    MkT :: a -> a -> T a

The reason this is rejected is that it tries to kind-check ``T a``. But because we
don't know that ``F a ~ True``, the application ``T a`` is rejected. The "obvious"
remedy (``MkT :: forall a -> F a ~ True => a -> a -> T a``) isn't currently available,
as GADT syntax doesn't support visible dependent quantification (yet). Even if the
syntax were available, it wouldn't work, because `GHC can't use an equality constraint <https://gitlab.haskell.org/ghc/ghc/-/issues/15710>`_
in the same type signature it is introduced. We can imagine machinations that would
accept ``T`` here, but it seems hard in the general case, most likely requiring a new
algorithm to implement constraint generalization in kinds, a feature that is not otherwise
needed.

Even without trying to accept GADT-syntax definitions, it is challenging to
maintain this feature. A concrete example is that Simon PJ and I (Richard)
had to spend maybe a cumulative 8-10 hours of meetings and calls trying to
figure out how to keep this feature working with the `work to separate
Constraint and Type <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8750>`_.
Instead of continuing down that path, we simply want to jettison the
feature.

It might also be interesting to note the history of this feature: I (Richard)
added it as I was implementing ``-XTypeInType`` because it seemed easy to do
so. No one asked for it. It just seemed like an easy addition. However, it has
proved anything but easy. (This is not the first time we've been stymied supporting it.)
So let's just send it back to the void whence it came.

Ryan Scott's `blog post <https://ryanglscott.github.io/2021/08/01/equality-constraints-in-kinds/>`_
provides additional background and gotchas around the existing feature. He even has to
say "They're not totally useless" to defend the feature.

Proposed Change Specification
-----------------------------

Disallow the constraint arrow ``=>`` in the following contexts:

* A kind annotation within a type: ``f ::  Int -> (Bool :: <<here>>) -> Double``
* A visible kind application within a type: ``f :: Proxy @(<<here>>) True -> ...``
* A kind annotation on a type variable binder: ``f :: forall (a :: <<here>>). a -> a``
* A kind annotation on a ``data`` declaration: ``data T a b :: <<here>> where ...``
* A kind annotation on a family declaration: ``type family F a b :: <<here>>`` (or ``data family``)
* A standalone kind signature: ``type T :: <<here>>``

In addition, any data constructor whose type includes a constraint is
not available for use in types.

Note that class constraints are already forbidden in exactly these places,
so this proposal changes only the status of equality constraints (both
homogeneous, with ``~``, and heterogeneous, with ``~~``).

Examples
--------
In addition to the example in the introduction, which would now be rejected,
this proposal stops the use of any constrained data constructor in a type.
For example::

  data S a where
    MkS :: F a ~ Int => a -> a -> S a
  -- definition of S is OK

  p :: Proxy (MkS True False)  -- this is rejected
  p = Proxy

This proposal does *not* affect normal GADT constructors. So this still works::

  data T a where
    MkT :: Bool -> T Bool

  p :: Proxy (MkT True)
  p = Proxy

Effect and Interactions
-----------------------

* Much code can be deleted. A partial list is on the `ticket tracking the implemenration of this proposal <https://gitlab.haskell.org/ghc/ghc/-/issues/22298#checklist-of-things-to-remove>`_.

* The inconsistency in the Motivation_ section is gone.

Costs and Drawbacks
-------------------

* Putting a constraint on a kind is very modestly useful, and this proposal destroys that use case. Instead, users are welcome to constrain data constructors instead of the type itself.

* If and when we get full dependent types -- including the ability to have type class constraints in kinds (which is not permitted today) -- we may need to reintroduce
  this feature, thus making the work of removing it redundant. However, it is quite likely that the new implementation will be much more well-grounded than the
  current one. It is even possible that it will be easier to implement this feature correctly after the current incorrect implementation is ripped out.

Alternatives
------------

* We don't have to do this. We could live with the inconsistency in the Motivation_, and
  Simon and I have indeed worked out how to land ``Constraint``\ -vs-\ ``Type`` even with
  this feature. But this is a poor plan, in my opinion.

* We could fully implement the feature. However, this is quite a lot of work with very
  little payoff.

Unresolved Questions
--------------------
None at this time.

Implementation Plan
-------------------
I (Richard) will implement.

Endorsements
-------------
The ideas behind this proposal were developed in concert with Simon PJ, though
I wrote up this text without his review.
