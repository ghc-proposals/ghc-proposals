Offer more array resizing primitives
====================================

.. author:: David Feuer
.. date-accepted:: 2018-07-02
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/121>`_.
.. contents::

At present, we have ``shrinkMutableByteArray#`` and ``resizeMutableByteArray#``.
We lack resizing primitives for other types of arrays: ``MutableArray#``,
``SmallMutableArray#``, the soon-to-be-deprecated ``MutableArrayArray#``, and
the soon-to-be-implemented ``UnliftedArray#`` (see
`Proposal 21 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0021-unlifted-array.rst>`_).

I propose we add them (with the exception of the ``MutableArrayArray#``
version), along with analogues of ``getSizeofMutableByteArray#`` to match.

Motivation
------------

Suppose I want to convert a list to an array. One option is to take the length
of the list, allocate an array, and then copy the list into the array. This is
simple, but it involves a lot of pointer-chasing and cannot release any list
conses until the list has been fully realized. Another option is to use array
doubling: allocate a small array, copy elements into it until it is full, then
expand the array and continue.

At present, the array doubling approach is somewhat inefficient. The main
problem, and certainly the one most susceptable to a solution, is that once the
list runs out, we're likely left with a partially filled array. We have to
allocate *another* array of the correct size, then copy the elements over. If
we could simply shrink the array at the end, we could avoid this useless
operation and let the garbage collector clean up the mess the next time it runs.

Proposed Change Specification
-----------------------------
Add ``shrinkMutableArray#``, ``shrinkSmallMutableArray#``, ``shrinkUnliftedArray#``,
``getSizeofMutableArray#``, ``getSizeofSmallMutableArray#``, and
``getSizeofMutableUnliftedArray#`` to match the ``ByteArray#`` versions.

Add a resizing operation for each array type. Unlike ``ByteArray#``, these
can't simply fill with zeros. The simplest option, and I suspect the most
useful, would be to take a filler value to use if the array expands ::

 resizeArray# :: MutableArray# s a -> Int# -> a -> State# s -> (#State# s, MutableArray# s a#)

Effect and Interactions
-----------------------

The most obvious downside is that ``sizeofMutableArray#``, ``sizeofSmallMutableArray``,
and ``sizeofMutableUnliftedArray#`` will cease to be reliable and we will have to add
analogues of ``getSizeofMutableByteArray#`` to supplant them. This will break existing
user code, but I doubt most of it will be very hard to fix.

Costs and Drawbacks
-------------------
I wouldn't anticipate terribly expensive development or maintenance. Learnability
shouldn't be affected much.

I see two downsides:

* There's simply one more mutable thing programmers might have to
  keep track of.

* Whereas ``sizeofMutableArray#`` can be reordered arbitrarily,
  ``getSizeofMutableArray#`` could not be. Most of the time, this
  isn't an issue, but I imagine it might reduce performance of
  bounds-checked array operations under some circumstances.

Alternatives
------------
Doing nothing is always a popular alternative.

Unresolved questions
--------------------

Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other ressources and prerequisites are required for implementation?
