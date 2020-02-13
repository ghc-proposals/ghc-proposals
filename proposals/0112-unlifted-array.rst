UnliftedArray
==============

.. author:: Andrew Martin
.. date-accepted:: 2018-05-14
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/112>`_.
.. contents::

GHC provides an unlifted type ``ArrayArray#``. Similar to how ``Array#`` is an unlifted array
of lifted values, ``ArrayArray#`` is an unlifted array of unlifted values. However, ``ArrayArray#``
lacks the type variable it needs to communicated what its element type. This is because historically,
there wasn't a good way to talk polymorphically about unlifted types in GHC. Since the levity
polymorphism proposal landed in GHC 8.0, there is now a way to talk about such things. This proposal
suggests implementing::

    data UnliftedArray# :: TYPE UnliftedRep -> TYPE UnliftedRep

At the same time, it proposes removing the existing ``ArrayArray#`` machinery from ``ghc-prim``
and reimplementing it in ``GHC.Exts`` for backwards compatibility.


Motivation
------------

Several of the primops for ``ArrayArray#`` have two variants. For example::

    indexByteArrayArray# :: ArrayArray# -> Int# -> ByteArray#
    indexArrayArrayArray# :: ArrayArray# -> Int# -> ArrayArray#

The same situation arises for the read and write operations on ``MutableArrayArray#`` except that now there
are four variants::

    readByteArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (#State# s, ByteArray##)
    readMutableByteArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (#State# s, MutableByteArray# s#)
    readArrayArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (#State# s, ArrayArray##)
    readMutableArrayArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (#State# s, MutableArrayArray# s#)

Under the hood, all four of these have the exact same implementation. The GHC `source code`_ makes
this clear::

    emitPrimOp _      [res] ReadArrayArrayOp_ByteArray          [obj,ix]   = doReadPtrArrayOp res obj ix
    emitPrimOp _      [res] ReadArrayArrayOp_MutableByteArray   [obj,ix]   = doReadPtrArrayOp res obj ix
    emitPrimOp _      [res] ReadArrayArrayOp_ArrayArray         [obj,ix]   = doReadPtrArrayOp res obj ix
    emitPrimOp _      [res] ReadArrayArrayOp_MutableArrayArray  [obj,ix]   = doReadPtrArrayOp res obj ix

.. _source code: https://github.com/ghc/ghc/blob/8ae263ceb3566a7c82336400b09cb8f381217405/compiler/codeGen/StgCmmPrim.hs#L407-L416

Despite the all the duplication, this interface still only captures a fraction of what ``ArrayArray#`` can
really offer. The end user must explicitly use ``unsafeCoerce#`` to do a store many things (``MVar#``, ``Array#``, etc.),
and even when they want a ``ByteArray#`` (which the interface does safely allow), they must **implicitly** perform an
unsafe coercion on every access (read/write/index) because the type system doesn't check what's in an ``ArrayArray#``.
In the primitive package, there is a `lot of unsafeCoerce`_ that is required to make it work:

.. _lot of unsafeCoerce: http://hackage.haskell.org/package/primitive-0.6.2.0/docs/src/Data-Primitive-UnliftedArray.html#PrimUnlifted

The current interface interface doesn't take advantage of the type system the rich type system that
GHC offers. It is characterized by:

* Implicit unsafe coercion on every access
* Copying functions (``copyArrayArray#``, etc.) that don't ensure that the elements in both arrays are of the same type.
* Explicit ``unsafeCoerce#`` required whenever the user wants to: store ``Array# a`` inside of ``ArrayArray#``,
  store ``MutableByteArray# s`` inside of ``ArrayArray#``, store ``MutVar# s a`` inside of ``ArrayArray#``, etc.


Proposed Change Specification
-----------------------------

There are two parts to the proposed change: replacing ``ArrayArray#`` with the type constructor
``UnliftedArray#`` and reimplementing the existing ``ArrayArray#`` interface with ``UnliftedArray#``.

The proposed primitives are::

    data UnliftedArray# :: TYPE 'UnliftedRep -> TYPE 'UnliftedRep
    data MutableUnliftedArray# :: TYPE 'LiftedRep -> TYPE 'UnliftedRep -> TYPE 'UnliftedRep

    indexUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). UnliftedArray# a -> Int# -> a
    writeUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). MutableUnliftedArray# s a -> Int# -> a -> State# s -> State# s
    readUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). MutableUnliftedArray# s a -> Int# -> State# s -> (# State# s, a #)
    unsafeFreezeUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). MutableUnliftedArray# s a -> State# s -> (#State# s, UnliftedArray# a#)
    newUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). Int# -> a -> State# s -> (# State# s, MutableUnliftedArray# s a #)
    sameMutableUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). MutableUnliftedArray# s a -> MutableUnliftedArray# s a -> Int#
    sizeofUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). UnliftedArray# a -> Int#
    sizeofMutableUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). MutableArray# s a -> Int#
    copyUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). UnliftedArray# a -> Int# -> MutableUnliftedArray# s a -> Int# -> Int# -> State# s -> State# s
    copyMutableArray# :: forall (a :: TYPE 'UnliftedRep). MutableUnliftedArray# s a -> Int# -> MutableUnliftedArray# s a -> Int# -> Int# -> State# s -> State# s
    cloneUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). UnliftedArray# a -> Int# -> Int# -> UnliftedArray# a
    cloneUnliftedMutableArray# :: forall (a :: TYPE 'UnliftedRep). MutableUnliftedArray# s a -> Int# -> Int# -> State# s -> (#State# s, MutableUnliftedArray# s a#)
    freezeUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). MutableUnliftedArray# s a -> Int# -> Int# -> State# s -> (#State# s, UnliftedArray# a#)
    thawUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). UnliftedArray# a -> Int# -> Int# -> State# s -> (#State# s, MutableUnliftedArray# s a#)

The implementations of most of these functions could be taken from the existing ``ArrayArray#``
function implementations. In GHC.Exts, the existing ``ArrayArray#`` interface could be
reimplemented (this requires the ``UnliftedNewtypes`` extension to be implemented)::

    -- definition of Any from GHC.Types included for clarity
    type family Any :: k where { }

    newtype ArrayArray# = ArrayArray# (UnliftedArray# Any)
    newtype MutableArrayArray# s = ArrayArray# (MutableUnliftedArray# s Any)

    unsafeCoerceUnlifted :: forall (a :: TYPE 'UnliftedRep) (b :: TYPE 'UnliftedRep). a -> b
    unsafeCoerceUnlifted a = unsafeCoerce# a

    indexByteArrayArray# :: ArrayArray# -> Int# -> ByteArray#
    indexByteArrayArray# (ArrayArray# u) i = unsafeCoerceUnlifted (indexUnliftedArray# u i)

    indexArrayArrayArray# :: ArrayArray# -> Int# -> ArrayArray#
    indexArrayArrayArray# (ArrayArray# u) i = unsafeCoerceUnlifted (indexUnliftedArray# u i)

    readByteArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, ByteArray# #)
    readByteArrayArray# (MutableByteArray# u) i s = case readUnliftedArray# u i s of
      (# s', e #) -> (# s', unsafeCoerceUnlifted e #)

    readArrayArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, ArrayArray# #)
    readArrayArrayArray# (MutableByteArray# u) i s = case readUnliftedArray# u i s of
      (# s', e #) -> (# s', unsafeCoerceUnlifted e #)

For brevity, not all of these are included in the proposal. However, the reimplementation is
a straightforward and mechanical process.

Effect and Interactions
-----------------------

The proposed change makes the interface for dealing with unlifted arrays more expressive
than it currently is. At the same time, it reduces the number of builtin primitive functions
that GHC provides. It is entirely backward-compatible for those who import ``GHC.Exts`` instead
of ``GHC.Prim`` (which is a recommended practice).


Costs and Drawbacks
-------------------

Some of the proposed functions do not currently exist for ``ArrayArray#``. They do however
have an implementation for ``Array#``. The cost of implementing them is small, and the
cost of migrating the existing functions should similarly be small. This change
lowers the maintenance costs associated with unlifted arrays in the long run since
it reduces duplicated code in the GHC code base.


Alternatives
------------

With the ``UnliftedNewtypes`` extension, it is possible to go the other way and implement
``UnliftedArray#`` on top of ``ArrayArray#``. This is unsatisfying because it still requires
``unsafeCoerce#`` for every access of the array, blocking potential optimizations. It also
leaves duplicated code for the primops in GHC.


Unresolved questions
--------------------

Is there a way to talk about type variables of kind ``TYPE 'UnliftedRep`` in ``GHC.Prim``?
This isn't done anywhere else in the module; all existing type variables there are kinded
``TYPE 'LiftedRep``. (Sort of, ``unsafeCoerce#`` is fully levity-polymorphic in its input
and its output, but it's more magical than most primitives).


Implementation Plan
-------------------

There is currently no implementation plan. I would be happy to give it a stab if someone
could provide guidance on how to define the two new types. The ``UnliftedNewtypes``
extension must be implemented before this proposal is implemented.
