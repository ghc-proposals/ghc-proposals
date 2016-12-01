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

Removing a layer of indirection can make a big difference.  Currently
``IORef`` consists of two objects: the outer ``IORef`` constructor and
the inner ``MutVar#`` type.  This proposal allows the two layers to be
collapsed. Operations on the collapsed ``IORef`` are approximately 2x
faster than the current ``IORef`` (according to `rough
experiments <https://gist.github.com/simonmar/d8a05797c01799abcb979aacf27164c7>`_).
Note that the outer constructor in the current ``IORef``
representation can be eliminated by using ``UNPACK`` or
sometimes automatically by strictness analysis, but not always.  The proposed
representation is *always* cheap.

This extension is aimed at particularly performance-critical data
structures, where the library writer is prepared to invest some effort
into defining an optimal representation.  This extension makes it
possible to eliminate a layer of indirection in mutable data
structures in a type-safe way, where currently this can only be done
by using ``unsafeCoerce`` tricks; see for example `the structs
package <http://hackage.haskell.org/package/structs>`_.


Proposed Change
---------------

We propose the addition of a new extension::

  {-# LANGUAGE MutableFields #-}

which enables constructor fields to be declared to be directly
mutable. A constructor with mutable fields is defined using GADT
syntax::

  data MutPair a b where
    MutPair :: mutable a -> mutable b -> IO (MutPair a b)

Note:

- The new ``mutable`` keyword declares a mutable field
- The return type is in ``IO``: a constructor with any ``mutable``
  fields *must* have a return type that has one of the forms ``IO t``, ``ST
  s t``, or ``State# s -> (# State# s, t #)``, where ``t`` takes the
  form of the normal return type for the constructor.

Given this declaration, GHC will create a constructor ``MutPair`` that
has the following type::

  MutPair :: a -> b -> IO (MutPair a b)

The ``mutable`` annotation in the definition disappears in the
constructor type.

Operations on mutable fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Two modules provide APIs for operating on mutable fields::

  module Data.Mutable.IO where
    type Mutable a = Ref# RealWorld a
    readRef :: Mutable a -> IO a
    writeRef :: Mutable a -> a -> IO ()

  module Data.Mutable.ST where
    type Mutable s a = Ref# s a
    readRef :: Mutable s a -> ST s a
    writeRef :: Mutable s a -> a -> ST s ()

Pattern matching
~~~~~~~~~~~~~~~~

We can pattern-match on the constructor to extract the mutable fields::

  readMutPair :: MutPair a b -> IO (a, b)
  readMutPair (MutPair aref bref) =
    (,) <$> readRef aref <*> readRef bref

When we pattern-match on a constructor with mutable fields, the fields
have type ``Mutable a``, and so we can use them with ``readRef`` and
``writeRef``.

What about records?
~~~~~~~~~~~~~~~~~~~

We can use record syntax when defining the constructor::

  data MutPair a b where
    MutPair :: { mutFst :: mutable a
               , mutSnd :: mutable b
               } -> IO (MutPair a b}

and then the record selectors are exactly what we'd expect::

  mutFst :: MutPair a b -> Mutable a
  mutSnd :: MutPair a b -> Mutable b

so record selection and pattern matching work out smoothly.  Record
construction is also fine::

  do
    mpair <- MutPair { fst = 3, snd = 4 }
    ...

but record *update* cannot be allowed for records with mutable fields,
so GHC must reject those with an error.

Primitives
~~~~~~~~~~

A mutable field is represented by a primitive type ``Ref#``::

  Ref# :: * -> * -> TYPE 'Ref

(``Ref`` is a new ``RuntimeRep``) and two new primitives::

  readRef#  :: Ref# s a -> State# s -> (# State# s, a #)
  writeRef# :: Ref# s a -> a -> State# s -> (# State# s, () #)

A ``Ref#`` represents a mutable field of a constructor.  Although ``Ref#``
appears to be a normal first-class primitive type, its *runtime
representation* will be ``(# Any, Int# #)``, that is, an unboxed pair
of the object that contains the mutable field and the offset of the
mutable field.

Core
~~~~

For each mutable data constructor ``K`` (where a "mutable data
constructor" is one that is declared with at least on e mutable
field), we get a new primitive mutable constructor function ``$mkK``,
whose type is::

  $mkK :: forall xs s . t1 -> ... -> tn -> State# s -> (# State# s, K v1...vn #)

where ``K`` was defined to have the type::

  K :: forall xs s . u1 -> ... un -> IO (K v1...vn)

and::

  ti = w,  if ui == mutable w
     = ui, otherwise

(``t``, ``u``, ``v`` and ``w`` are types, and ``xs`` is a set of type
variables)

When ``K`` is used in a pattern in a case alternative in Core, the
types of its fields are ``t1...tn``.

There would be a new constraint on Core: *we cannot create an
expresison representing a mutable constructor*.

GHC currently assumes that constructors can be built in a couple of
places:

- In Worker-wrapper, we build an expression representing the re-packed
  constructor.  Worker-wrapper would need to be either disabled
  (easiest) or adapted for mutable constructors.
- When simplifying a case expression like ``case x of y { C a b -> E
  }``, GHC creates the mapping ``y -> C a b`` when simplifying ``E``.
  We will have to avoid creating this mapping If ``C`` is a mutable
  constructor.

Unpacking constructors with mutable fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

While C++ or Rust allow unpacking a mutable product type into another,
this requires a recursive notion of object construction/initialization
and would not fit here. If we tried the following::

  data MutPair2 = MP Int {-# UNPACK #-} MutPair

what is the type of the MP constructor? It cannot be this::

  MP :: Int -> MutPair -> IO MutPair2

because the only way to make that work would be to copy the mutable
record ``MutPair`` into the ``MP`` constructor. This is (1) not what
we'd want for including inner objects as value types, and (2) ruins
the guarantee that ``UNPACK`` is a performance hint rather than
semantically important.

So, ``UNPACK`` cannot do anything when used on a type with mutable
fields.  However, there's nothing preventing ``UNPACK`` from working
as normal in a type definition with mutable fields.

``UNPACK`` would be a no-op on a mutable field itself, just like it is
for other primitive types.

Can we get rid of ``MutVar#``?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we got rid of ``MutVar#`` and instead defined ``IORef`` like this::

  data IORef a where
    IORef :: Mutable a -> (IORef a)

then

- This ``IORef`` is faster and uses less memory,
- but it cannot be UNPACKed. Memory-wise this new ``IORef`` is the
  same as an UNPACKed old ``IORef``.  However, it is lifted where
  ``MutVar#`` is unlifted, leading to some extra overhead to access it.

So the conclusion is:

- Provided we use ``Mutable`` wherever we currenty UNPACK ``IORef``,
  then this ``IORef`` is an unambiguous improvement over the old
  ``IORef``.

Mutable unboxed fields
~~~~~~~~~~~~~~~~~~~~~~

We can extend this to handle mutable unboxed fields too::

  Ref# :: forall r. * -> TYPE 'r -> Type Ref

It's perfectly fine for ``Ref#`` to be parameterised by a
representation-polymorphic type because the representation of ``Ref#``
itself does not depend on this type argument.

However, we now need a family of primitives to work with these::

  readRefInt#  :: Ref# s Int# -> State# s -> (# State# s, Int# #)
  writeRefInt# :: Ref# s Int# -> Int# -> State# s -> (# State# s, () #)

  readRefDouble#  :: Ref# s Double# -> State# s -> (# State# s, Double# #)
  writeRefDouble# :: Ref# s Double# -> Double# -> State# s -> (# State# s, () #)

and so on.

Implementation
~~~~~~~~~~~~~~


Constructors
^^^^^^^^^^^^

As with ordinary constructors, we need a
constructor *wrapper*, which is defined in terms of the constructor
worker::

  MutPair = \a b -> IO $ \s -> MutPair# a b s

where the primitive constructor ("worker") is::

  MutPair# :: forall a b s. a -> b -> State# s -> (# State# s, MutPair a b #)

We would generate code for the primitive constructor just like we
generate code for other constructors, taking care to add the Void
argument for the ``State#``, and generating an info table with the
correct information about the mutable fields.

Primitives
^^^^^^^^^^

``readRef`` is implemented in terms of the primitive ``readRef#``::

  readRef :: Mutable a -> IO a
  readRef aref = IO $ \s -> readRef# aref

But what *are* these ``Ref#`` things that are extracted from the
constructor by pattern matching and seemingly passed to ``readRef#``?
The idea is that the runtime representation of ``Ref#`` is a pair of
the containing object and the offset of the mutable field. Just before
code generation, probably in the Unarise phase, we will manifest the
runtime representation of ``Ref#`` at each pattern match::

  readMutPair = \m ->
    case m of (v::Any) { MutPair aref bref ->
    let aref = (# v, 0# #) in
    let bref = (# v, 1# #) in
    readRef aref >>= \a ->
    readRef bref >>= \b ->
    return (a,b) }

and then propagate the expansion of ``aref`` and ``bref`` to all the
places they are referenced. Function arguments of type ``Ref#`` are
expanded to unboxed pairs of type ``(# Any, Int# #)``.

The ``readRef#`` primitive would be compiled inline to a single read
instruction. Similarly ``writeRef#`` would compile to a primitive write
instruction, but it would also need a memory barrier just like
``writeMutVar#``, and a GC write barrier (the equivalent of
``dirty_MUT_VAR()``).


Garbage collection
^^^^^^^^^^^^^^^^^^

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

TODO: pin down the details of info table representation and the form
of the GC write barrier code.

Drawbacks
---------

The GC write barrier for a mutable constructor may be a little less
efficient than the write barrier for a ``MutVar#``, but this is more
than compensated for by losing a layer of indirection.

Adding new syntax has lots of costs: changes in the parser and
``HsSyn``, the renamer and typechecker, not to mention knock-on effects
on external packages: ``haskell-src-exts`` and clients of that.

Types that contain mutable fields cannot be UNPACKed into other
constructors.

Worker-wrapper doesn't work on mutable constructors, at least not
without some changes.  Perhaps this isn't so bad, since we would never
be able to eliminate the original construction of the mutable
constructor anyway.

Alternatives
------------

Don't do this :)

Unresolved Questions
--------------------

* Should we provide a way to test for reference equality between
  mutable constructors?
* Can we add a way to include mutable arrays in a constructor?
* It would be great to allow STM as an option in addition to IO and
  ST.  The constructor will need to store extra metadata, because
  TVar# is more complex than MutVar#.
* We can declare a constructor with an ``IO`` result type even if it
  doesn't have any mutable fields.  Why is that useful?  One reason is
  that these constructors have identity, which means that things like
  pointer equality and finalizers work more reliably.
