Add ``sumToTag#`` primop
========================

.. author:: David Feuer
.. date-accepted:: 2021-02-18
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/369>`_.
.. contents::


Add a ``sumToTag#`` primop indicating which sum alternative is
represented by a particular unboxed sum value. For example,

.. code:: haskell

   sumToTag# (# a | #) = 0#
   sumToTag# (# | | b #) = 2#

Motivation
----------

It’s sometimes useful to get the numerical value of an alternative. For
example, I might write

.. code:: haskell

   newtype Trit# = Trit# (# (##) | (##) | (##) #)

   tritToInt# :: Trit# -> Int#
   tritToInt# (Trit# (# (##)|| #)) = 0#
   tritToInt# (Trit# (# |(##)| #)) = 1#
   tritToInt# (Trit# (# ||(##) #)) = 2#

Under the hood, unboxed sums are actually represented by ``Int#`` tags
alongside the “payload” values, often with some padding. For example,
``(# (##) | Bool #)`` is represented as something like
``(# Int#, Bool #)``. The values look like this:

.. code:: haskell

   (# (##) | #)   ~=   (# 1#, padding #)
   (# | b #)      ~=   (# 2#, b #)

Note: the choice to make the first alternative ``1#`` rather than ``0#``
is poorly motivated, and should probably be changed, but that’s beyond
the strict scope of this proposal.

If we could only get our hands on the ``Int#`` tag, then we could
calculate ``tritToInt#`` very efficiently. The C– code would look much
like the code that would be produced by

.. code:: haskell

   tritToInt'# :: (# Int# #) -> Int#
   tritToInt'# (# t #) = t - 1

Unfortunately, that’s *not* the sort of code that ``tritToInt#`` will
actually produce. Rather, it will produce code like this:

.. code:: haskell

   tritToInt''# :: (# Int# #) -> Int#
   tritToInt''# (# 3# #) -> 2#
   tritToInt''# (# 2# #) -> 1#
   tritToInt''# _ -> 0#

This seems pretty silly, considering that the result of ``tritToInt#``
is likely to be fed straight into some numerical computation. There will
be extra code and unnecessary conditional branches when all that’s
needed is a simple decrement operation.

Proposed Change Specification
-----------------------------

Add a ``sumToTag#`` inline primop, analogous to ``dataToTag#``, with the
following type.

.. code:: haskell

   sumToTag# :: forall (xs :: [RuntimeRep]) (a :: TYPE ('SumRep xs)). a -> Int#

``sumToTag#`` will return the 0-based index of the sum alternative of
its argument.

It may seem bizarre to have a function that is polymorphic in the
runtime representation of its argument; we could only call such a
function when we know the concrete representation of its argument,
i.e. when the ``xs :: [RuntimeRep]`` type argument is *closed*. However,
GHC already imposes a universal ban on levity-polymorphic arguments (see
Section 5.1 of `Levity
Polymorphism <https://www.microsoft.com/en-us/research/publication/levity-polymorphism/>`__)
so we do not have to worry about this case.

Examples
--------

.. code:: haskell

   sumToTag# ((# | 2# | #)
                :: (# (# Int, Char #) | Int# | Bool #)) = 1#

   -- No need to pin down specific types
   sumToTag# ((# | x | #)
                :: (# a | x | b :: TYPE 'UnliftedRep #)) = 1#

This will compile, but can never ever be called:

.. code:: haskell

   foo :: forall (a :: TYPE ('SumRep '[])). a -> Int#
   foo = sumToTag#

This will compile, but cannot be called with current primitives; if that
changes in the future, it will always return ``0#``.

.. code:: haskell

   bar :: forall (a :: TYPE ('SumRep '[ 'IntRep])). a -> Int#
   bar = sumToTag#

Effect and Interactions
-----------------------

It will be possible to extract tags efficiently. With the current
implementation of unboxed sums, we would compile to code looking
something like

.. code:: haskell

   sumToTag# (# tg1, ... #) = tg1 -# 1#

It would be beneficial to implement a special rule for ``case`` of
``sumToTag#``, in case that should appear in the course of optimization.

.. code:: haskell

   case sumToTag# a of
     0# -> e0
     1# -> e1
     _ -> e2

   ===>

   case a of
     (# _|| #) -> e0
     (# |_| #) -> e1
     (# ||_ #) -> e2

One long-term option this primop gives us is to stop returning ``Int#``
from primops whose values represent only two or three specific values.
For example, we would have the option of writing

.. code:: haskell

   (==#) :: Int# -> Int# -> (# (##) | (##) #)

which more faithfully represents what the return value can be, while
still being able to work with the ``Int#`` representation directly using
``sumToTag#``. I do *not* propose making such a change at this time.

Costs and Drawbacks
-------------------

Implementing this primop should be extremely cheap. Implementing the
special ``case`` rule will probably not be terribly hard; its structure
should generally follow that of the rule for ``case`` of ``dataToTag#``.

The main (theoretical) drawback I see is that ``sumToTag#`` weakens
parametricity. Today, we can reason that a function polymorphic over its
type can’t inspect its argument. With ``sumToTag#``, it can do so, to a
limited extent. In practice, however, primops already break
parametricity, and users who don’t have access to principled ones like
``tagToSum#`` may well reach for more dangerous unsafe coercions to get
the job done.

Alternatives
------------

Joachim Breitner asks, “Could this be solved by an optimization in a
later phase that detects this pattern, and produces the code that you
want?” Perhaps. Such an optimization would have to be in the C– stage,
which is rather late in the game. It would have to be reminiscent of
common subexpression elimination, but I believe it would be considerably
more complicated. Imagine what this would look like in Haskell: If we
saw

.. code:: haskell

   case a of
     (# (##) | #) -> b
     (# | (##) #) -> b + 1

then we’d have to somehow figure out that we should transform to

.. code:: haskell

   case a of
     (# (##) | #) -> b + 0
     (# | (##) #) -> b + 1

so we can achieve the equivalent of

.. code:: haskell

   b + sumToTag# a

This all seems much too hard.

Unresolved Questions
--------------------

Should we also offer ``tagToSum#``, mirroring ``tagToEnum#``?
Conceptually,

.. code:: haskell

   tagToSum#
     :: forall (a :: TYPE ('SumRep '[ 'TupleRep '[]
                                    , 'TupleRep '[], ...])).
     Int# -> a

   -- For example:

   tagToSum# :: Int# -> (# (##) | (##) | (##) #)
   tagToSum# 0# = (# (##) || #)
   tagToSum# 1# = (# | (##) | #)

where the type-level list has at least one element. Unlike
``sumToTag#``, ``tagToSum#`` would be unsafe in two different ways:

1. The user could supply an out-of-bounds ``Int#`` value.
2. The user could supply an uninhabited type:

   .. code:: haskell

      newtype Uninhab :: TYPE ('SumRep '[ 'TupleRep '[]
                                        , 'TupleRep '[]]) where
        Uninhab :: Uninhab -> Uninhab

Including ``tagToSum#`` seems nice for symmetry’s sake, but I have not
yet found a situation where it’s truly necessary. In the ``Trit#``
example above, it would be a natural way to deal with certain arithmetic
results:

.. code:: haskell

   lowTrit :: Int# -> Trit#
   lowTrit x = tagToSum# (x `remInt#` 3#)

But when would we really benefit from this? Most of the time we’ll just
perform a ``case`` match, and case-of-case will clean everything up. The
rest of the time, we’ll probably be allocating memory and the conversion
will be the least of our worries.

Implementation Plan
-------------------

(Optional) If accepted who will implement the change? Which other
resources and prerequisites are required for implementation?

Endorsements
------------

(Optional) This section provides an opportunty for any third parties to
express their support for the proposal, and to say why they would like
to see it adopted. It is not mandatory for have any endorsements at all,
but the more substantial the proposal is, the more desirable it is to
offer evidence that there is significant demand from the community. This
section is one way to provide such evidence.
