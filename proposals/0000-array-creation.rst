Add more ways to create arrays of pointers
==========================================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/130>`_.
.. sectnum::
.. contents::

Add primops to create new arrays from one or more existing ones.

Motivation
------------
Creating arrays of pointers (``Array#``, ``SmallArray#``, ``ArrayArray#``,
and the proposed ``UnliftedArray#``) is a bit tricky to do efficiently.
The fundamental challenge is that we must never run the garbage collector
when there is an array of pointers anywhere that has an uninitialized element.
As a result, every array creation primop must fill the entire new array,
even if many (or even all) of its elements will be overwritten immediately.
The current array creation primitives are quite limited, which causes
some reasonably common operations (particularly for ``SmallArray#``, but for
brevity I will discuss only ``Array#`` throughout) to be less efficient than
they could be. For example:

- Insert an element into an array at an arbitrary position. The only way to
  do this is to use ``newArray#`` to create a fresh array, then copy the
  beginning and end of theh old array into it.

- Delete an element from an arbitrary position in an array. For this, we
  can use ``thawArray#`` to copy all but the last element of the array, then
  ``copyArray#`` to recopy all the elements after the delete point.

- Append two arrays. Much like insertion, our only option is to use ``newArray#``
  and then use ``copyArray#`` twice.

- Concatenate more than two arrays. Again, we have to create and then copy.

András Kovács has written the
`array-primops <https://hackage.haskell.org/package/array-primops>`_
package to try to fill in some of these gaps, but there are a couple
issues:

1. Its cmm implementations won't be inlined, so they have a performance
   penalty for very short arrays (like the ones used in ``Data.HashMap``,
   for example).

2. Since it's written in cmm, it needs to track GHC internals particularly
   carefully, and it's not available to non-cmm-based backends. Both of
   these aspects make it an unattractive dependency.

Proposed Change Specification
-----------------------------
There are two fairly obvious additions:

1. thaw/clone variants that copy an arbitrary segment of an array into a
   fresh array of arbitrary size, starting at an arbitrary offset. It would
   take two values, one to fill in the beginning of the array and one to
   fill in the end. This would be sufficient for ``cons`` and ``snoc``
   operations, and any situation where an array needs to be extended generally.

2. Primops to copy segments of two (possibly) different arrays into a fresh
   array, taking three values to fill in gaps in the beginning,
   middle, and end. These would allow efficient implementations of ``insert``,
   ``delete``, and ``append``.

A third operation, concatenating an array of arrays, would be possible, but I
have no particular application in mind for it, so I don't know if it would
carry its weight.

Effect and Interactions
-----------------------

Costs and Drawbacks
-------------------
There are infinitely many array initialization operations we might
potentially want. The proposed primitive operations would only cover
a small (but important) subset of them.

The proposed operations take multiple values to fill in gaps that may
or may not exist, and offsets and lengths that may or may not be known
in advance. These pose potential challenges for efficient code generation.

Alternatives
------------
1. Add multiple variants to pin down more precisely where gaps may or
   may not occur. That way we can only supply fill-in logic and arguments
   that will actually be used. I suspect this approach would be more
   efficient, but it may be a bit hard to control the API size.

2. The other alternative I can think of would require substantial research
   and development: come up with a way for users to create their own primop-like
   operations whose total allocation can be calculated (or at least
   bounded) before any allocation occurs. The friendliest way would probably
   be to offer a special declaration form that admits only a limited subset
   of Haskell.

Unresolved questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and specification. Be upfront and trust that the community will help. Please do not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to the steering committee.


Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other ressources and prerequisites are required for implementation?
