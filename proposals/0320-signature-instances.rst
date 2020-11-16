Defaults and backpack/hs-boot
=============================

.. author:: John Ericson and Cale Gibbard on behalf of Obsidian Systems
.. date-accepted:: 2020-05-14
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/17190
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/320>`_.
.. contents::


Correct/clarify the meaning of instance declarations in hs-boot files
and backpack signatures in the situations where an instance is to be
required, but the corresponding class has a default type family
instance.

Motivation
----------

Presently, if one creates a class with an associated type that has a
default, such as:

.. code:: haskell

   -- in A.hs
   module A where
   class C a where
     type T a
     type T a = Int
     f :: a -> Int
     f _ = 0

and then in an ``hs-boot`` file would like to demand an instance of that
class for some type:

.. code:: haskell

   -- in B.hs-boot
   module B where
   import A
   data X
   instance C X

This results in a compiler panic, because GHC “can’t handle family
instances in boot files”. Evidently, we’ve snuck a type family instance
into what was meant to be an interface.

This problem also occurs for backpack signatures, which are rather
similar in many ways to hs-boot files.

In both cases, trying to explicitly specify anything about the type
family instance in an hs-boot file or backpack signature is currently an
error (a more explicit one rather than a compiler panic).

At the term level, the default is ignored, because ``hsig``/``hs-boot``
are not allowed to contain methods period:

.. code:: haskell

   -- in B.hs-boot
   module B where
   import A
   data X
   instance C X
      -- T is unconstrained

.. code:: haskell

   -- in B.hs-boot
   module B where
   import A
   data X
   instance C X
     f _ = 7 -- error

This is blatantly inconsistent, with 3 ways to make it consistent:

1. Never allow (term or type level) items in instances, nor infer them
   from class defaults, in ``hsig``/``hs-boot`` files

2. Do allow items in instances, but apply defaults in
   ``hsig``/``hs-boot`` files

3. Do allow items in instances, but ignore defaults in
   ``hsig``/``hs-boot`` files

Our approach is essentially to go with solution (1) here, with an
understanding that (3) might eventually be desired if future proposals
want to allow some type equality specifications in signatures. (2) seems
needlessly confusing and restrictive.

Proposed Change Specification
-----------------------------

We propose that any instance in backpack signatures and hs-boot files,
places no constraint on the terms and associated type family instances
which may be specified by the corresponding class, regardless of whether
there are defaults provided by the class specification.

Examples
--------

A.hs

::

   module A where

   class C a where
     type F a
     type F a = a -> a    -- Default instance
     op :: a -> a

     foo :: C a => [a] -> F a
     foo = blah

B.hs-boot

::

   module B where
   import A
   instance C Int

C.hs

::

   module C where
   import {-# SOURCE #-} B

   wim :: [Int] -> F Int
   wim = foo                 -- OK

   wam :: [Int] -> Int -> Int
   wam = foo                 -- NOT OK

B.hs

::

   module B where
   import A
   instance C Int where
     type F Int = [Int]
     op = reverse

The instance ``C Int`` in B.hs-boot is legal, does not imply
``instance F Int = Int -> Int``, despite the default associated type
declaration in the original definition of the class ``C``. Indeed, the
final ``instance C Int`` in B.hs (shown above) might use an entirely
different type instance than the default.

This is all the same as class methods. The existence of a default method
for ``op`` in the definition of ``C`` does not imply that the ``C Int``
instance (defined in B.hs presumably) uses that default method.

Effect and Interactions
-----------------------

The proposed change addresses the issue raised in the motivation section
directly. I don’t immediately foresee any negative interactions with
other language features.

It’s not the intention of this proposal to specify anything with regards
to whether type family instances, specifications of type equalities, or
ordinary definitions (to be matched on some sort of equivalence
perhaps), are permitted in hs-boot and backpack signatures. As of this
writing, they’re not supported, but there are open GHC issues discussing
adding support along these lines
`#8441 <https://gitlab.haskell.org//ghc/ghc/issues/8441>`__ and
`#12680 <https://gitlab.haskell.org//ghc/ghc/issues/12680>`__.

Costs and Drawbacks
-------------------

If you do want the default you have to repeat it. This is currently
unsupported anyway at the type level, but we may wish to allow
specifications of type family instances in ``hs-boot`` and ``hsig``
files in the future.

Defaults in regular code allow what would have been a breaking change to
be a non-breaking change. They won’t accomplish that for signatures and
hs-boot files. However, the complicated variances surrounding backpack
and classes (sigs and classes both being in negative position) makes
this tricky to think about and of debatable value.

There are better, more intentional ways to “upcast” libraries to an
interface that the downstream library expects. We should explore
extra-linguistic techniques for API evolution, rather than relying on
cruder in-language-like defaulting for this, side-stepping this
drawback.

Alternatives
------------

There’s an option to eventually support specification of type family
instances in backpack signatures and hs-boot files, where one would have
the option of then regarding the default from the class as a
specification which must be obeyed when an empty instance is given in
the signature. That would mean that any non-default instance would be
forced to be provided by the specification, and seems unhelpful at best.

Unresolved Questions
--------------------

On one hand, there are no term level equality constraints. On the other,
we can simply require a module define any term-level method that the
signature/boot file defines, or check for crude syntactic equality as a
sound conservative approximation.

Implementation Plan
-------------------

John Ericson has already implemented this. `Pull Request
#1776 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/1776>`__.

Endorsements
------------

Obsidian Systems has been working on this on behalf of MIRI.
