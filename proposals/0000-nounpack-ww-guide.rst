.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull requst <https://github.com/ghc-proposals/ghc-proposals/pull/46>`_.

.. contents::

NOUNPACK pragmas for function type signatures
==============

The worker/wrapper transformation is one of GHC's most important optimizations.
By unboxing values, it can dramatically reduce memory allocation, eliminate
slow indirection, and allow temporary values to be stored directly in machine
registers. Unfortunately, the transformation can occasionally go rather badly
wrong. In some cases, it might be possible for compiler improvements to
fix the trouble. In other cases, however, there is a legitimate judgement
call requiring domain-specific knowledge. I therefore propose that users
be allowed to guide the worker-wrapper transformation through pragmas.


Motivation
------------

I first noticed the problem when working on the ``insert`` function for
``Data.Map.Lazy`` (for which the consequences are very severe), but a
simpler example is an internal function called ``insertR`` which we use to
implement ``union``:

.. code-block:: haskell

  insertR :: Ord k => k -> v -> Map k v -> Map k v

The ``insertR`` function inserts a key-value pair, but if the given
key is already present in the map, it returns the map unchanged.

Maps are represented as balanced binary search trees:

.. code-block:: haskell

  data Map k v = Bin !Int !k v !(Map k v) !(Map k v)

To insert a key-value pair, we need to compare the key to be inserted with each
of the keys along the insertion path before finally (perhaps) installing it in
an appropriate ``Bin`` constructor and rebalancing the tree.

This all works quite well for sum types, but for products and primitive types,
the worker/wrapper transformation does something unfortunate.  Consider the
case where the keys are of type `(Int, Int)`. The worker/wrapper transformation
implements ``insertR`` as a wrapper around a worker, roughly like this:

.. code-block:: haskell

  insertR :: (Int, Int) -> v -> Map (Int, Int) v -> Map (Int, Int) v
  insertR (I# k1, k2) v m = $winsert k1 k2 v m

  $winsertR :: Int# -> Int -> v -> Map (Int, Int) v -> Map (Int, Int) v

The key pair is unboxed, and its first component (which is used in every
comparison) is unboxed as well. This is great for finding the correct insertion
position. If that position is already occupied by an equal key, everything is
fine; we just stop. If, however, we actually need to install the key-value
pair, we have a bit of a problem: we have to build the key from its pieces:
``Bin 1 (I# k1, k2) v Tip Tip``. If the keys were coming from the user, it's
possible that this would actually be okay; perhaps this transformation allows
us to avoid ever building the key pair to begin with. But in fact, ``insertR``
is always used in a context where the key pair *has already been constructed*.

The correct fix, in this case, is to modify the way the worker and wrapper
split. We want to get something more like

.. code-block:: haskell

  insertR k@(I# k1, k2) v m = $winsert k k1 k2 v m

  $winsertR :: (Int, Int) -> Int# -> Int -> v -> Map (Int, Int) v -> Map (Int, Int) v

Unfortunately, the only way I was able to convince GHC to do this was a horrible
and likely fragile hack using the magical function ``GHC.Exts.lazy``. I'd
like to get a better story.


Proposed Change Specification
-----------------------------

I would like to allow users to annotate type signatures to guide the
worker/wrapper transformation. Specifically, I would like to allow
them to use a ``NOUNPACK`` pragma to indicate that a certain argument
should not be unboxed. ``insertR`` could then be written

.. code-block:: haskell

  insertR :: Ord k => {-# NOUNPACK #-} k -> v -> Map k v -> Map k v
  insertR k v m = go k k v m
    where
      go :: Ord k => {-# NOUNPACK #-} k -> k -> v -> Map k v -> Map k v
      go kp k v m = ...

The syntax is inspired by the way ``NOUNPACK`` pragmas can be used in GADT
constructors. It would inform the worker-wrapper split that the first
argument to ``insertR``, and the first argument to ``go``, must be actual
pointers to the specified key type.

Effect and Interactions
-----------------------

Hopefully, this additional user input would be sufficient to prevent GHC from
throwing away boxes that the user wants to be sure to preserve.


Costs and Drawbacks
-------------------

I have no estimate of development or maintenance costs. I think it will be
about as easy for users as ``NOUNPACK`` pragmas for datatypes.


Alternatives
------------

I know of no existing alternatives to the proposal. There are very good reasons
for GHC to use worker/wrapper the way it does to implement unboxing, and I know
of no general alternative approach that would allow users to help make these
difficult judgement calls. Whole-program compilation could try to use detailed
information about how a function is used to determine what to unbox, but that
is not an option for GHC.


Unresolved questions
--------------------


Implementation Plan
-------------------
