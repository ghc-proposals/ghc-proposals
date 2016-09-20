.. proposal-number::

.. trac-ticket::

.. implemented::

Mutable Constructor Fields
==========================

We propose an extension to add support for mutable constructor fields.

Motivation
----------

This proposal improves both the runtime and memory overhead for code
that currently uses ``IORef`` or ``STRef``.

The best representation we currently have available for a type that
contains an ``IORef`` is something like::

  data MutPair a b =
    MutPair {-# UNPACK #-} !(IORef a)
            {-# UNPACK #-} !(IORef b)

and the runtime representation of this is 3 heap objects: the outer
``MutPair`` with 2 pointer fields (3 words total), and 2 ``MutVar#``
objects (2 words each).

Under this proposal, we would be able to declare a ``MutPair`` that is
represented by a single 3-word heap object containing 2 directly
mutable pointer fields.


Proposed Change
---------------

We propose the addition of a new primitive type constructor::

  Ref# :: * -> * -> TYPE 'Ref

(``Ref`` is a new ``RuntimeRep``) and two new primitives::

  readRef#  :: Ref# s a -> State# s -> (# State# s, a #)
  writeRef# :: Ref# s a -> a -> State# s -> (# State# s, () #)

A ``Ref#`` represents a mutable field of a constructor.  Although ``Ref#``
appears to be a normal first-class primitive type, its *runtime
representation* will be ``(# Any, Int# #)``, that is, an unboxed pair
of the object that contains the mutable field and the offset of the
mutable field.


Defining a type with mutable fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

First, let's make a couple of handy type synonyms::

  type IOField a = Ref# RealWorld a
  type STField s a = Ref# s a

The only way to create a ``Ref#`` is by declaring a constructor that has a
``Ref#`` field.  For example (using the ``IOField`` synonym)::

  data MutPair a b = MutPair (IOField a) (IOField b)

A ``MutPair`` is a constructor with two mutable fields.  But ``MutPair``
is not a constructor in the ordinary sense; both allocation and
pattern-matching behave in a special way.

The ``MutPair`` constructor has the following type::

  MutPair :: a -> b -> IO (MutPair a b)

Note the ``IO``! This is the only way to create a ``MutPair``.  Neither
the programmer nor the compiler can create one accidentally.  A
``MutPair`` has identity, like other mutable objects.

Specifically, the type of the constructor is derived by
 - replacing each ``Ref# s a`` field with ``a``
 - adding ``IO`` to the result type

As with ordinary constructors, we need a constructor *wrapper*, which
is defined in terms of the constructor worker::

  MutPair = \a b -> IO $ \s -> MutPair# a b s

where the primitive constructor ("worker") is::

  MutPair# :: forall s. a -> b -> State# s -> (# State# s, MutPair a b #)

Pattern matching
~~~~~~~~~~~~~~~~

We can pattern match on a constructor with mutable fields just like
any other constructor.  Suppose we want to define
::
  readMutPair :: MutPair a b -> IO (a, b)

Using the primitives we can implement it like this::

  readMutPair m = IO $ \s0 ->
    case m of { MutPair aref bref ->
    case readRef# aref s0 of { (# s1, a #) ->
    case readRef# bref s1 of { (# s2, b #) -> (# s2, (a, b) #) }}}

(we can make this easier for the programmer to write, but let's come
back to that later)

But what *are* these ``Ref#`` things that are extracted from the
constructor and seemingly passed to ``readRef#``?  The idea is that the
runtime representation of ``Ref#`` is a pair of the containing object
and the offset of the mutable field. Just before code generation,
probably in the Unarise phase, we will manifest the runtime
representation of ``Ref#`` at each pattern match::

  readMutPair m = IO $ \s0 ->
    case m of (v::Any) { MutPair aref bref ->
    let aref = (# v, 0# #) in
    let bref = (# v, 1# #) in
    case readRef# aref s0 of { (# s1, a #) ->
    case readRef# bref s1 of { (# s2, b #) -> (# s2, (a, b) #) }}}

and then propagate the expansion of ``aref`` and ``bref`` to all the
places they are referenced. Function arguments of type ``Ref#`` are
expanded to unboxed pairs of type ``(# Any, Int# #)``.

The ``readRef#`` primitive would be compiled inline to a single read
instruction. Similarly ``writeRef#`` would compile to a primitive write
instruction, but it would also need a memory barrier just like
``writeMutVar#``, and a GC write barrier (the equivalent of
``dirty_MUT_VAR()``).

Simpifying the programmer's API
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To make it easier for the programmer we would wrap the readRef#
primitive::

  readRef :: IOField a -> IO a
  readRef aref = IO $ \s -> readRef# aref

And then we could write::

  readMutPair :: MutPair a b -> IO (a, b)
  readMutPair (MutPair aref bref) = (,) <$> readRef aref <*> readRef bref

Garbage collection
~~~~~~~~~~~~~~~~~~

The garbage collector needs to know that an object is mutable, and
which fields are mutable.  So we have to put this information in the
info table.  Probably:

- new ``MUT_CONSTR_CLEAN`` and ``MUT_CONSTR_DIRTY`` object types

- Each constructor with a mutable field needs two info tables: the
  CLEAN one and the DIRTY one.  From each info table we need to be
  able to get both CLEAN and DIRTY info pointers, for the GC write
  barrier.

- Representation: put the mutable fields first, before the non-mutable
  pointers.

- Store the number of mutable fields in the info table, alongside the
  number of pointers and non-pointers.  (but include the mutable
  fields in the count of pointer fields, so that RTS code that doesn't
  care about mutability can work without changes)

The GC needs to do the same CLEAN/DIRTY and non-eager promotion stuff
that it does with other mutable objects.

Code generation
~~~~~~~~~~~~~~~

We would generate code for the primitive constructor just like we
generate code for other constructors, taking care to add the Void
argument for the ``State#``, and generating an info table with the
correct information about the mutable fields.

Unpacking constructors with mutable fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

UNPACK must be a no-op on constructors with mutable fields.  There's
no sensible way to make UNPACK work with mutable fields.

Can we get rid of ``MutVar#``?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we got rid of ``MutVar#`` and instead defined ``IORef`` like this::

  data IORef a = IORef (IOField a)

then

- This ``IORef`` is faster and uses less memory,
- but it cannot be UNPACKed. Memory-wise this new ``IORef`` is the
  same as an UNPACKed old ``IORef``.  However, it is lifted where
  ``MutVar#`` is unlifted, leading to some extra overhead to access it.

So the conclusion is:

- Provided we use ``IOField`` wherever we currenty UNPACK ``IORef``,
  then this ``IORef`` is an unambiguous improvement over the old
  ``IORef``.


Drawbacks
---------

The GC write barrier for a mutable constructor may be a little less
efficient than the write barrier for a ``MutVar#``, but this is more
than compensated for by losing a layer of indirection.

Types that contain mutable fields cannot be UNPACKed into other
constructors.

Alternatives
------------

Don't do this :)

Unresolved Questions
--------------------


Garbage collection
~~~~~~~~~~~~~~~~~~

The exact details of how we represent the info tables for mutable
constructors and how we get the CLEAN/DIRTY info pointers.

GADT syntax
~~~~~~~~~~~

Because the constructor has a special return type, it's tempting to
use GADT syntax to declare it, but it doesn't quite work out because
the argument types of the constructor are also different from the
declared field types::

  data MutPair a b where
    MutPair :: IOField a -> IOField b -> IO (MutPair a b)

is a lie, because the constructor really has type ``a -> b -> IO
(MutPair a b)``.

But there's no reason to forbid the use of GADT syntax: maybe you want
to have mutable fields in a GADT.  Open question: what should the GADT
syntax look like?

