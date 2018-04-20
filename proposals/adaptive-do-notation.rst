Make ``>>=`` great again
========================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/127>`_.
.. sectnum::
.. contents::

The ``Prelude.>>=`` combinator limit the return type as a ``m b``, which cause Monads not composable. In order to ease this restrictions, this proposal introduces another type class ``Dsl`` for do notation.

Motivation
----------

Monads do not compose. Normally a ``do`` block cannot contains operators defined in different monad instances.

The state of art solution is `using only one monad <http://okmij.org/ftp/Haskell/extensible/>`_ , ``Eff``, which forwards all monadic bind operations to custom effect handlers, instead of defining new monads.

However, the ``Eff`` approach is heavy weight than ordinary monad. It's not very convenient to create an additional indirect layer for simple use cases.

The ``Eff`` approach of bypassing ``>>=`` combinator is quite embarrassing. Since ``>>=`` settles on our logo, we should make it great again. This proposal aims to port the approach used in `Dsl.scala <https://github.com/ThoughtWorksInc/Dsl.scala>`_ to GHC. This approach improves the extensibility of `>>=`.

Proposed Change Specification
-----------------------------

=======================
Changes in the compiler
=======================

Desugaring ``<-`` to ``Dsl.>>=`` instead of ``Prelude.>>=``.

======================
Changes in the library
======================

----------------------------
Add a new type class ``Dsl``
----------------------------

::

  module Dsl where
  {-# LANGUAGE FlexibleInstances #-}
  {-# LANGUAGE MultiParamTypeClasses #-}
  class Dsl m a d where
    (>>=) :: m a -> (a -> d) -> d
    (>>) :: m a -> d -> d
    (>>) ma d = ma Dsl.>>= \a -> d

The ``>>=`` combinator of the ``Dsl`` type class is similar to ``Prelude.>>=``, except ``m b`` is replaced to ``d``. As a result, multiple ``<-`` expressions in a ``do`` block can have the same ``d`` and different ``m``.


----------------------------
Add a forwarder to ``Monad``
----------------------------

::

  instance Monad m => Dsl m a (m b) where
    (>>=) = (Prelude.>>=)

Effect and Interactions
-----------------------

Suppose you want to create a random number generator. The generated numbers should be stored in a lazily evaluated infinite list, which can be built with the help of adaptive do notation.

::

  import Data.Int
  import Data.List
  import Data.Bits

  newtype Yield a = Yield { element :: a }

  instance Dsl Yield a [a] where
    interpret (Yield a) handler = a : handler a

  xorshiftRandomGenerator :: Int -> [Int]
  xorshiftRandomGenerator seed =
    do let tmp1 = seed ^ (shiftL seed 13)
       let tmp2 = tmp1 ^ (shiftR tmp1 17)
       let tmp3 = tmp2 ^ (shiftL tmp2 5)
       Yield(tmp3)
       xorshiftRandomGenerator tmp3

This ``Yield`` type should be similar to the ``yield`` keyword in C#, ECMAScript and Python. This is impossible for the current implementation of do notation because the return type ``[Int]`` and ``Yield Int`` are not match.

See `Dsl.scala: creating library-defined keywords from ad-hoc polymorphic delimited continuations <https://thoughtworksinc.github.io/Dsl.scala/ldk.pdf>`_ for more use cases and benchmarks.

The forwarder to ``Monad`` should keep the backward compatibility to current implementation of do notation.

Costs and Drawbacks
-------------------

``Dsl`` does not have a law.

Alternatives
------------

Eff and mtl are known workarounds for this problem. Unfortunately, mtl is inefficient and Eff is too heavy-weight.

``Dsl.>>=`` combinator can be used for do notation with the help of RebindableSyntax extension. However, RebindableSyntax does not make ``>>=`` great again by default.

Unresolved questions
--------------------

Implementation Plan
-------------------
