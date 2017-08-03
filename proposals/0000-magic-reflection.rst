.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/69>`_.

.. contents::

Add built-in support for class-based reflection
===============================================

The ``reflection`` package allows users to inject runtime values of arbitrary
types into the constraint system. Other packages allow similar injection for
singleton types. Unfortunately, these packages need to use ``unsafeCoerce`` to
do their work, and as a result miss out on certain optimizations (such as
inlining and unboxing). Furthermore, these packages rely on single-method
classes being represented as newtypes. Furthermore, there is talk among GHC
developers of changing that representation, which would break all the packages.
For both these reasons, I believe built-in support is justified. The ideas in
this proposal were previously discussed on the GHC Wiki, in
`MagicalReflectionSupport
<https://ghc.haskell.org/trac/ghc/wiki/MagicalReflectionSupport>`_.


Motivation
------------

Motivation for the core idea behind the ``reflection`` package can be found in
"`Functional Pearl: Implicit Configurations — or, Type Classes Reflect the Values of Types <http://okmij.org/ftp/Haskell/tr-15-04.pdf>`_",
by Oleg Kiselyov and Chung-chieh Shan.

The specific fragments of the package covered by this proposal are ``Reifies``,
``reify``, ``reifyNat``, ``reifySymbol``, ``Given``, and ``give``. The package
also offers ``reifyTypeable``, which works using a completely different
mechanism.

The actual ``reflection`` package currently uses proxy-based type passing,
but to make it easier to think about the generated Core, I will use a
``Tagged``-based version. Translating between the two is straightforward.

.. code-block:: haskell

  newtype Tagged s a = Tagged { unTagged :: a }

  class Reifies s a | s -> a where
    reflect :: Tagged s a

  reify :: forall a r. (forall s. Reifies s a => Tagged s r) -> a -> r
  reifyNat :: forall r. (forall n. KnownNat n => Tagged n r) -> Integer -> r
  reifySymbol :: forall r. (forall n. KnownSymbol n => Tagged n r) -> String -> r

  class Given a where
    given :: a
  give :: (Given a => r) -> a -> r

I will discuss ``Reifies`` and ``reify``; the other reification functions
work similarly. ``Given`` and ``give`` are immediately implementable.

From the singleton side, if we have

.. code-block:: haskell

  data Nat = Z | S Nat
  data SNat n where
    Zy :: SNat 'Z
    Sy :: SNat n -> SNat ('S n)
  class KnownUnaryNat n where
    known :: SNat n

then we would like to be able to write functions like

.. code-block:: haskell

  reifyUnaryNat :: forall n r. SNat n -> (KnownUnaryNat n => r) -> r

Note the difference in quantification: whereas ``reifyNat`` takes a function
polymorphic in the natural number, the inherent coherence of the
``KnownUnaryNat`` class allows ``reifyUnary`` to accept a monomorphic value.
Haskell programmers currently use both these styles, and therefore both should
be supported.

Proposed Change Specification
-----------------------------

Offer a new derivable class

.. code-block:: haskell

  class Reflectable (c :: Constraint) where
    type DictRep c :: *
    reify# :: (c => r) -> DictRep c -> r

derivable for single-method classes without superclass constraints.

Given

.. code-block:: haskell

  class TheClass a where
    method :: T

  deriving instance Reflectable (TheClass a)

we would produce an instance

.. code-block:: haskell

  instance Reflectable (TheClass a) where
    type DictRep (TheClass a) = T
    reify# = ...


Operationally, ``reify# f x`` will package up ``x`` in a dictionary
and pass that dictionary to ``f``. Currently, that means ``reify#``
will actually just be a coercion. Later, it may mean something else.

Effect and Interactions
-----------------------

We can implement what we want
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The above-described mechanism can implement singleton reflection
directly. We could simply write, for example,

.. code-block:: haskell

  deriving instance Reflectable (KnownUnaryNat n)

  reifyUnaryNat :: forall n r. (KnownUnaryNat n => r) -> SNat n -> r
  reifyUnaryNat = reify# @(KnownUnaryNat n)

Implementing ``reflection``'s ``reify`` function is a bit hairier, but it
only needs to be done once.

.. code-block:: haskell

  class Reifies s a | s -> a where
    reflect :: Tagged s a

  deriving instance Reflectable (Reifies s a)

  reify :: forall a r. (forall s. Reifies s a => Tagged s r) -> a -> r
  reify f a = unTagged (reify# @(Reifies s a) (f :: Tagged s r) a
                          :: forall s. Tagged s r)

The optimizer has to be careful
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

My main concern with regard to implementation has to do with
specialization in the ``reflection`` case. ``reify`` desugars to a term
that uses ``$fReflectableReifies @ Any @ a``. It's critical
that the specializer never specializes this; ``Any`` must not equal ``Any``
in this context! If I write ``reify (... (reify ... (3 :: Int))) (4 :: Int)``,
it's impermissible to change 3 into 4 or vice versa.


Costs and Drawbacks
-------------------

The proposed mechanism allows users to subvert class coherence by
implementing fundamentally illegitimate functions like ``give``.
Coherence is only guaranteed when reflection is carefully protected
by polymorphism (as in ``reify``) or when the reified type is truly
a singleton. Others have told me that library authors, rather than
GHC, should be responsible for using this mechanism responsibly.
I think they are probably right.


Alternatives
------------

Use a data family
~~~~~~~~~~~~~~~~~

.. code-block:: haskell

  class Reflectable (s :: Symbol) (c :: Constraint) where
    data DictRep s c :: *
    reify# :: (c => r) -> DictRep s c -> r

In this version, the dictionary is represented by a data family
instead of a type family. By using a data family, the newtype
constructor on the argument to ``reify#`` determines the class we are
reifying, avoiding the need to use explicit type application to
specify the class. The ``Symbol`` allows the user to name the
newtype constructor for the ``DictRep``. This all seems reasonably
convenient, but I think it's probably actually overkill. Using
a bit of explicit type application to fix the constraint doesn't
seem like a huge price to pay for simplicity.

.. code-block:: haskell

Build the constraint from the representation type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A very different approach suggested earlier by Simon Peyton Jones uses a class
of reifiable types rather than reflectable classes.

.. code-block:: haskell

  class Reifiable (a :: *) where
    type RC a :: Constraint
    reify# :: (RC a => r) -> a -> r

As Iceland_Jack noted, it should be possible to make it levity-polymorphic:

.. code-block:: haskell

  class Reifiable (a :: TYPE rep) where
    type RC a :: Constraint
    reify# :: (RC a => r) -> a -> r

I doubt that polymorphism is really *useful*, however, since constraints are
lifted anyway, but perhaps there's some way to do something with it.

The big downside I see to this approach is that it can't be retrofitted
around existing classes; those must be *replaced*. For example, in order
to implement ``reifyUnary``, we would need to write something like

.. code-block:: haskell

  class RC (SNat n) => KnownUnaryNat n
  instance RC (SNat n) => KnownUnaryNat n

whereas with the proposed approach we can use an existing user-defined
``KnownUnaryNat`` class.

Unresolved questions
--------------------

I have the nagging feeling that there should be some way to extend
this proposal to include classes with superclasses or multiple methods
in some fashion. I have not yet been able to come up with a design
for that.


Implementation Plan
-------------------
