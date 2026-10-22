``SPECIALIZABLE`` pragma
==============

.. author:: Francesco Gazzetta (TODO should I add the mentors too?)
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/357>`_.
.. contents::


We propose a new ``SPECIALIZABLE`` pragma that has the effect of exposing the
unfolding of the marked binding for specialization purposes, and does not
conflict with inline pragmas.


Motivation
----------

One of the many optimizations that GHC can perform is specialization.
GHC can create multiple versions of generic code specialized to particular
types, avoiding to pass typeclass dictionaries around and opening possibilities
for further optimizations.

To tell GHC to specialize a function, one can use the ``SPECIALIZE`` pragma.
To be able to specialize, GHC needs the unfolding of the function to be
available, either from the module itself or from interface files.

To ensure that the unfolding is available to library users, authors tend to use
the
```INLINEABLE`` <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#inlinable-pragma>`_
pragma, which does have the effect of exposing the unfolding,
but has an only tangentially related meaning.

Moreover, one may not actually want to inline the function, but specifying both
``INLINEABLE`` and ``NOINLINE`` is not possible.
This can lead to increase of code size or other unwanted behaviors.


As an example, the |GHC.List.elem function|_
has a ``NOINLINE`` pragma, or early inlining could prevent the ``RULE`` to fire,
but then it has to resort to phase control to allow specialization, which is
implicit/indirect and prone to breakage.

.. |GHC.List.elem function| replace:: ``elem`` function from ``GHC.List``
.. _GHC.List.elem function: https://hackage.haskell.org/package/base-4.14.0.0/docs/src/GHC.List.html#elem


Proposed Change Specification
-----------------------------

We add a new ``SPECIALIZABLE`` pragma, with the following syntax:

::

 {-# SPECIALIZABLE binding #-}

This pragma has the effect of exposing the unfolding of the specified binding
in the interface file, making it available for specialization in other modules.

This is similar to the current effect of ``INLINEABLE``, except for the fact
that it can be combined with other inline pragmas, in particular ``NOINLINE``.

When a binding has both ``SPECIALIZABLE`` and ``NOINLINE`` pragmas, its
unfolding and all the specialized versions are marked as not inlinable.
This way, specialization is allowed but inlining is not.
This is the only case where the new pragma is expected to produce a different
result from ``INLINEABLE``.

Examples
--------

Here is a (not so useful) example which illustrates how the pragma works:

::

 module Specializable where
 {-# SPECIALIZABLE myBind #-}
 {-# NOINLINE myBind #-}
 myBind :: Integral i => i -> i
 myBind n = n + 2

 module SpecUser where
 import Specializable
 specUser :: Int
 specUser = myBind 4

The unfoldig of ``myBind`` will be exposed, and in the ``SpecUser`` module
``myBind`` will be specialized to ``Int -> Int``, but it will not be inlined
to become ``4 + 2``.
Having just the ``SPECIALIZABLE`` pragma would have the same behaviour as
the current ``INLINEABLE``, that is, specialization and possibly inlining.

As another example, the ``elem`` function from the end of the Motivation section
could be marked as ``SPECIALIZABLE`` ``NOINLINE``.

Effect and Interactions
-----------------------

The new pragma will make it possible to meaningfully express the need to ensure
that specialization is possible.

We don't expect interactions with other compiler features. Rather, a fundamental
part of the proposal is the non-interaction with inline pragmas.
Of course, though, the compiler is free to use the newly exposed unfoldings
for any other purposes it deems necessary.

Costs and Drawbacks
-------------------

A prototype was already implemented, and we are finishing a cleaner version.
The maintainance cost shouldn't be high, since it's just a new pragma that only
interacts with inlining and specialization.

Alternatives
------------

The existing alternative is ``INLINEABLE``, which as explained before is not
expressive enough.

There are some possible additions to this proposal:

* phase control
  ::
   
   {-# SPECIALIZABLE[activation] binding #-}
* more eager specialization of specializable unfoldings
* (default) warnings on missed specializations (like ``-Wmissed-specializations``)


Unresolved Questions
--------------------

None :)


Implementation Plan
-------------------

`GHC MR #3630 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3630>`_

(old prototype at `this branch <https://gitlab.haskell.org/fgaz/ghc/-/tree/specializable/basic1try2>`_)

Endorsements
-------------

`GHC Ticket #12463 <https://gitlab.haskell.org/ghc/ghc/-/issues/12463>`_

