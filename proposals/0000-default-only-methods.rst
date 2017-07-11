.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/61>`_.

.. contents::

Default-only methods
====================

There are several situations in which it makes sense for a class
to have a method that, nevertheless, is never actually stored in
the instance dictionaries for that method. I propose that users
be allowed to indicate this by a pragma in a ``class`` definition.


Motivation
------------

The need for this proposal is motivated by several standard classes,
but there are many other examples in the ecosystem. We have, for
example:

.. code-block:: haskell

  class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f = sequenceA . fmap f

    sequenceA :: Applicative f => t (f a) -> f (t a)
    sequenceA = traverse id

    mapM :: Monad m => (a -> m b) -> t a -> m (t b)
    mapM = traverse

    sequence :: Monad m => t (m a) -> m (t a)
    sequence = sequenceA

There is rarely a substantial performance advantage to writing custom
implementations of any methods other than ``traverse``, most especially
because few people will ever actually use them. These extra methods
aren't free: they increase the size of ``Traversable`` dictionaries
at run-time if they're not specialized away, and they increase the amount
of code that must be compiled and stored in object files.

Another notable example is

.. code-block:: haskell

  class Applicative m => Monad m where
    return :: a -> m a
    return = pure

    (>>=) :: m a -> (a -> m b) -> m b

There are two problems here: the ``return`` method is redundant, now
that we have AMP, and the ``join`` method many people would like to
be able to use to write their ``Monad`` instances is absent. Removing
the ``return`` method will break any code that defines a ``return``
method, and adding ``join`` will break code that uses
`GeneralizedNewtypeDeriving` to derive ``Monad`` instances.


Proposed Change Specification
-----------------------------

Allow users to specify that a class method is not to be stored
in class dictionaries. I do not have a good pragma name, but for
the rest of the discussion I will call it ``DEFAULT_ONLY``.
We could then write

.. code-block:: haskell

  class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    {-# DEFAULT_ONLY sequenceA, mapM, sequence #-}
    sequenceA :: Applicative f => t (f a) -> f (t a)
    sequenceA = traverse id
    ...

  class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    m >>= f = join (fmap f m)

    {-# DEFAULT_ONLY return, join #-}
    return :: a -> m a
    return = pure

    join :: m (m a) -> m a
    join = (>>= id)

A ``DEFAULT_ONLY`` method is *required* to have a default definition.
Everywhere other than the instance declaration, the ``DEFAULT_ONLY``
method takes its default definition. Within the instance declaration,
the user-provided definition is in scope. The user-provided definition
is also used when filling in default definitions. ``DEFAULT_ONLY``
methods would be ignored by ``GeneralizedNewtypeDeriving``.


Effect and Interactions
-----------------------

With the proposed change in place, it becomes much cheaper to
add alternative ``MINIMAL`` methods to a class, and to retain
legacy methods.


Costs and Drawbacks
-------------------

I believe the main challenge/drawback is that this proposal requires
potentially tricky changes to the way instance declarations are
compiled. I don't know what the compiler might need to do to determine
whether a method referenced in an instance declaration refers to
the one for the class in question.

I suspect that in most cases, all will be clear after type checking.
But there may be cases when it is not. It would seem reasonable to
reject the pragma under those circumstances.


Alternatives
------------

A much more limited alternative Edward Kmett mentioned to me is to
simply allow extra bindings to be "bundled" with class exports,
much like patterns can be bundled with datatype exports. This could
help with backwards compatibility to some extent, but would not
allow alternative methods.


Unresolved questions
--------------------

As mentioned above, I'm not sure whether this proposal can actually be
implemented.


Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other ressources and prerequisites are required for implementation?
