.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/78>`_.

.. contents::


Generalized Do-Notation
==============

Do-Notation is a wonderful tool for Haskell programmers, since it allows for cleanly expressing a common pattern when
using types of the ``Monad`` class, and recently ``Applicative`` class using the ``ApplicativeDo`` extension. This proposal
proposes to extend the usefulness of do-notation, by allowing the user to specify himself which operations do-notation de-sugars
into.

Motivation
------------
An example can be found here, `https://github.com/Tritlo/SizedMonads/blob/48c112cb02d70ddc9e8f37782d975c96b2f7e74f/Main.hs`.
As you can see, I'm using a "Monad-like" type called `SizedMonad` to track the number of operations, increasing a counter
within the type. However, due to it not being properly monadic, I have to manually de-sugar it into the monad-like operations
from the ``SizedMonad``. I imagine this is not a unique case. 

Being able to do this cleanly via implementing an instance of the ``Doable`` and ``ApplicativeDoable`` type classes would allow
me to do without the manual de-sugaring, and also allow me to tap into the dependency analysis and automatic "Applicativation"
enabled by the ``ApplicativeDo`` extension.

Proposed Change Specification
-----------------------------

We propose adding a new extension, `-XGeneralizedDoNotation`. When this extension is enabled,
two new type classes would be added to base,

::

  class ApplicativeDoable f where
    pure :: a
    (<*>) :: a

and a second class,

::

  class ApplicativeDoable m => Doable m where
    (>>=) :: a

Which have no constraints on the types of the functions themselves.

To preserve backwards compatability, instances would be included,

::

  instance Applicative f => ApplicativeDoable f where
    pure = Applicative.pure
    (<*>) = (Applicative.<*>)
    
  instance Monad m => Doable m where
    (>>=) = (Monad.>>=)

Then, instead of de-sugaring to the ``Monadic`` and ``Applicative`` operations when de-sugaring do-notation, the do-notation
would be de-sugared into the operations from the ``Doable`` and ``ApplicativeDoable`` type class. This allows the user to
override these operation, while still being able to have the clean syntax of do-notation and proper ``Monad`` and ``Applicative``
instances.

Effect and Interactions
-----------------------

This might interact with the ``ApplicativeDo`` proposal, especially since we will be removing some
constraints on how the current do-notation works. However, since use of the new do-notation will require
defining a new instance of the ``Doable`` and ```ApplicativeDoable`` type classes, accidental activation of
the new feature will be minimal.

Costs and Drawbacks
-------------------

This should be reasonably easy to implement, and should mostly require swapping explicit references to ``<*>`` and ``>>=`` in
the desugarer for the operators defined by the type class, and adding instances of the ``Doable`` and ``ApplicativeDoable`` to
``Applicative`` and ``Monad`` instances, using default implementations.

This will make it a bit harder for novices to learn, since there will be one more layer and one more thing to explain.

Alternatives
------------
A current alternative is to do this manually via template Haskell or by hand. However, this
forces users to duplicate the work already implemented in ``ApplicativeDo`` for automatically making things
``Applicative``, and a potential source for errors.

Another potential alternative is to use ``RebindableSyntax``. This however clobbers everything in scope,
which might not be what the user wanted. Being able to do this on a case by case basis for each type
would allow users to specify the syntax de-sugaring for their type in e.g. a library, without having to ask the user to
use rebindable syntax in the entire file.

Unresolved questions
--------------------
There is a question of what the operators and type classes themselves should be called. I'm inclined to call them ``Doable``
and ``ApplicativeDoable`` and using the same operators (with possibly minor changes), since they are already familiar to most
users.

A point of contention is the decision to have no constraints on the types of the operators. This is to allow the user the
most freedom in how they'd like to implement their de-sugaring. However, it might be more prudent to have more constraints
than presented here.

Another point to consider is whether to overwrite do-notation, or whether to allow the user to select somehow which operations
he wants to use when de-sugaring in each case. 

The "default implementation" via an instance from Applicative will not work, due to the type inferencer not backtracking. However,
we could possibly derive a ``Doable`` and a ``ApplicativeDoable`` instance automatically from ``Applicative`` and ``Monad`` classes, if the extension is enabled.

Implementation Plan
-------------------
I would make an attempt to implement this myself, hopefully with some help from previous implementers of the do-notation desugarer.
