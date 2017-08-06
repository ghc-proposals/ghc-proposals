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
do their work. As a result:

1. They miss out on certain optimizations (such as
inlining and unboxing).

2. They rely on single-method
classes being represented as newtypes. It is conceivable that this
representation could change in the future, which would break all the packages. Such
a change was considered in a previous version of
`Separate Constraint from Type <https://github.com/ghc-proposals/ghc-proposals/pull/32>`_
but seems to have been rejected.

For both these reasons, I believe built-in support is justified. The ideas in
this proposal were previously discussed on the GHC Wiki, in
`MagicalReflectionSupport
<https://ghc.haskell.org/trac/ghc/wiki/MagicalReflectionSupport>`_.


Motivation
------------

Motivation for the core idea behind the ``reflection`` package can be found in
"`Functional Pearl: Implicit Configurations — or, Type Classes Reflect the Values of Types <http://okmij.org/ftp/Haskell/tr-15-04.pdf>`_",
by Oleg Kiselyov and Chung-chieh Shan. Much of that paper discusses a very
different (and much less efficient) implementation approach; only the general
discussion and examples are really relevant. In determining the importance
of supporting this idea, the committee is urged to consider the
`reverse dependencies of the reflection package <http://packdeps.haskellers.com/reverse/reflection>`_.

Perhaps the canonical example of singleton reflection is the ``withSingI``
function
`in the singletons package <http://hackage.haskell.org/package/singletons-2.3.1/docs/src/Data-Singletons.html#withSingI>`_.

The specific fragments of ``reflection`` covered by this proposal are
``Reifies``, ``reify``, ``reifyNat``, ``reifySymbol``, ``Given``, and ``give``.
The package also offers ``reifyTypeable``, which works using a mechanism
based on Kiselyov and Shan's.

The rest of this proposal will focus on one class and one function from each
of ``singletons`` and ``reflection``. In each case, I am using slightly
different types than those in the actual packages, in order to make a
cleaner, more direct (and in some cases more efficient) translation to Core.
It is straightforward to apply the same mechanism to the exact existing
APIs.

From the ``singletons`` side
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: haskell

  -- A family of singleton types
  data family Sing (a :: k)

  class SingI (a :: k) where
    sing :: Sing a

  withSingI :: (SingI n => r) -> Sing n -> r

From the ``reflection`` side
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: haskell

  newtype Tagged s a = Tagged { unTagged :: a }

  -- Reifies s a => X can be read, roughly speaking, as
  --
  -- \(s :: a) -> X
  --
  -- but rather than binding a *term variable* to a value, it binds
  -- a *type variable* to a value.
  class Reifies s a | s -> a where
    reflect :: Tagged s a

  -- reify f x can be read, roughly speaking, as "apply f to a".
  reify :: forall a r. (forall s. Reifies s a => Tagged s r) -> a -> r

Note the difference in quantification: whereas ``reify`` takes a function
polymorphic in the type variable ``s`` (representing the fact that ``s``
may be "bound" to multiple values), the inherent coherence of the
``SingI`` class allows ``withSingI`` to accept a monomorphic value. Both
these styles are used "in the wild" and should be supported.


Proposed Change Specification
-----------------------------

Offer a new derivable class

.. code-block:: haskell

  class Reflectable (c :: Constraint) where
    type MethodType c :: *
    reify# :: (c => r) -> MethodType c -> r

derivable for single-method classes without superclass constraints
whose method type does not quantify over any type variables.

Given

.. code-block:: haskell

  class TheClass a where
    method :: T

  deriving instance Reflectable (TheClass a)

we would produce an instance

.. code-block:: haskell

  instance Reflectable (TheClass a) where
    type MethodType (TheClass a) = T
    reify# = ...

Operationally, ``reify# f x`` will package up ``x`` in a dictionary
and pass that dictionary to ``f``. Currently, that means ``reify#``
will actually just be a coercion. It could be implemented as a function today
(perhaps disabling some optimizations, as ``Data.Reflection`` does):

.. code-block:: haskell

  newtype Magic c a = Magic (c => a)

  reify#default :: forall c r a . (c => r) -> a -> r
  reify#default f = unsafeCoerce (Magic f :: Magic c r)

That is, we take a function that expects a *dictionary* argument and coerce
it to a function expecting a regular argument.

As Simon Peyton Jones pointed out, making ``reify#default`` (with its entirely
over-general type) a primop would require giving it special typing rules. On
the other hand, System FC is perfectly capable of handling a ``reify#`` function
for each single-method class.


Effect and Interactions
-----------------------

We can implement what we want
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Singleton reflection
""""""""""""""""""""

The above-described mechanism can implement singleton reflection
directly. We could simply write, for example,

.. code-block:: haskell

  deriving instance Reflectable (SingI n)

  withSingI :: forall n r. (SingI n => r) -> Sing n -> r
  withSingI = reify# @(SingI n)


Reify
"""""

Implementing ``reflection``'s ``reify`` function is a bit hairier, but it only
needs to be done once.

.. code-block:: haskell

  deriving instance Reflectable (Reifies s a)

  reify :: forall a r. (forall s. Reifies s a => Tagged s r) -> a -> r
  reify f = unTagged #. (reify# @(Reifies s a) (f :: Tagged s r)
                          :: forall s. a -> Tagged s r)

Neither ``withSingI`` nor ``reify`` actually does anything; they're just
coercions.

The optimizer has to be a bit careful
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

My main concern with regard to implementation has to do with
specialization in the ``reify`` case. Suppose we write

.. code-block:: haskell

  reify# f (A :: T)
  ...
  reify# f (B :: T)

Inlining ``reify#`` could lead to something like

.. code-block:: haskell

  f (coerce A :: Reifies Any T)
  f (coerce B :: Reifies Any T)

We need to recognize that there can be multiple *different* dictionaries of
type ``Reifies Any T``, and avoid replacing the one built from ``A`` with the
one built from ``B`` and vice versa.


Costs and Drawbacks
-------------------

The proposed mechanism allows users to subvert class coherence.
For example, suppose

.. code-block:: haskell

  f :: C Int => R
  instance C Int where
    m = ...

Then

.. code-block:: haskell

  reify# @(C Int) f a

will pass ``f`` a dictionary built from ``a``, but ``f`` is free
to ignore it completely and use the dictionary from the ``C Int``
instance.

Coherence (up to bottoms) is trivially ensured when the reified
type is truly a singleton.

``reify`` ensures safety primarily through rank-2 polymorphism:
the function it is passed must be polymorphic in the type variable
to be "bound". The existence of even a single concrete instance
of ``Reifies`` prevents anyone from making trouble by writing
an overly polymorphic instance.

Others have said they believe that library authors, rather than
GHC, should be responsible for using this mechanism responsibly.
I think they are probably right.


Alternatives
------------

Use a data family
^^^^^^^^^^^^^^^^^

A first draft of this approach looks like this:

.. code-block:: haskell

  class Reflectable (c :: Constraint) where
    data MethodType c :: *
    reify# :: (c => r) -> MethodType c -> r

In this version, the dictionary is represented by a data family instead of a
type family. By using a data family, the data constructor on the argument to
``reify#`` determines the class we are reifying, avoiding the need to use
explicit type application to specify the class. This alternative has two
benefits:

1. We no longer need to prohibit the method type from quantifying
   over type variables, as that quantification will be under
   the newtype constructor. For example,

   .. code-block:: haskell

     class Foo a where
       foo :: a -> b -> a

     deriving instance Reflectable (Foo a)

   would produce something like

   .. code-block:: haskell

     instance Reflectable (Foo a) where
       newtype MethodType (Foo a) = FooCon (forall b. a -> b -> a)
       reify# = ...

2. The constraint in question is fixed by the newtype constructor,
   so it does not have to be given using visible type application.

The big difficulty is that we need to deal with *naming* the data constructor.
A first-draft approach looks like this:

.. code-block:: haskell

  class Reflectable (s :: Symbol) (c :: Constraint) where
    data MethodType s c :: *
    reify# :: (c => r) -> MethodType s c -> r

where the ``Symbol`` represents the desired name. A user could write

.. code-block:: haskell

  deriving instance Reflectable "MyWrapper" MyClass

to derive

.. code-block:: haskell

  instance Reflectable "MyWrapper" MyClass where
    data MethodType "MyWrapper" MyClass = MyWrapper T

where ``T`` is the type of ``MyClass``'s method.

Unfortunately, having ``s`` in the type of ``reify#`` is annoying—it's really
only supposed to be used by the deriving mechanism! So the final draft (for
now) of the data family version looks like this:

.. code-block:: haskell

  class s ~ ConName c => Reflectable (s :: Symbol) (c :: Constraint) where
    data MethodData c :: *
    type ConName c :: Symbol
    reify# :: (c => r) -> MethodData c -> r

This may actually be more usable.


Build the constraint from the representation type
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
to implement ``withSingI``, we would need to write something like

.. code-block:: haskell

  class RC (Sing n) => SingI n
  instance RC (Sing n) => SingI n

whereas with the proposed approach we can use an existing user-defined
``SingI`` class.

Unresolved questions
--------------------

I have the nagging feeling that there should be some way to extend
this proposal to include classes with superclasses or multiple methods
in some fashion. I have not yet been able to come up with a design
for that.


Implementation Plan
-------------------
