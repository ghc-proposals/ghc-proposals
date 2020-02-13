Unlifted Newtypes
=================

.. author:: Andrew Martin
.. date-accepted:: 2018-02-27
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/15219
.. implemented:: 8.10
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/98>`_.
.. contents::

GHC 8.0 introduced a more sane way to talk about the kind of unlifted types,
levity polymorphism. Following this, the kind of unboxed tuples and sums was
revised. Many of the unlifted kinds have a finite number of inhabitants. For
example, ``TYPE 'IntRep`` is only inhabited by ``Int#``. This proposal provides
a way to create additional types that inhabit the unlifted kinds. With the
``UnliftedNewtypes`` language pragma, the existing ``newtype`` construct would
begin to accept types of unlifted kinds. GHC currently rejects the following
definition::

    newtype PersonId = PersonIdConstructor { getPersonId :: Int# }

With ``UnliftedNewtypes``, this definition would be accepted. The kinds and types
of the generated types and functions would be::

    PersonId :: TYPE 'IntRep
    PersonIdConstructor :: Int# -> PersonId
    getPersonId :: PersonId -> Int#

In GHC Core, the two function shown above would be implemented as casts,
just as they currently are with lifted newtypes.

Motivation: Fewer Allocations on Decode
----------

Consider a data type for representing an interval. It may look like this::

    data Interval = Interval {-# UNPACK #-} !Int {-# UNPACK #-} !Int

The first argument is the lower bound, and the second argument is the
upper bound. An interval in which the lower bound is higher than
the upper bound is meaningless. We may wish to make such intervals
unrepresentable by hiding the data constructor and exporting functions
guaranteed to preserve this invariant::

    -- | The arguments of 'smartInterval' are ordered such that
    --   the smaller one comes first in the interval.
    smartInterval :: Int -> Int -> Interval
    inInterval :: Interval -> Int -> Bool
    translateIntervalByOne :: Interval -> Interval

Assume that we have a ``ByteArray`` of packed ``Interval``. We will
use a simple packing strategy in which each interval takes up two
machine words, one for the lower bound and one for the upper bound. If we
need to map over all the intervals, with ``translateInterval``, we
have to pay a nursery allocation for each of these::

    mapIntervals :: (Interval -> Interval) -> ByteArray -> ByteArray
    shiftFive :: ByteArray -> ByteArray
    shiftFive arr = mapIntervals translateIntervalByOne arr

We could have avoided this by instead defining everything to work on
the equivalent unboxed tuple of unboxed ints::

    type Interval = (# Int#, Int# #)

Now, ``mapIntervals`` can be called without paying an allocation for
every element in the array. But since ``Interval`` is now a type alias,
we are no longer able to hide its internals. Users can easily circumvent
the guarantee the API was originally supposed to provide. With
``UnliftedNewtypes``, we can get the best of both worlds. We can define
``Interval`` as::

    newtype Interval = Interval (# Int#, Int# #)

We can then hide the data constructor as we did in the first example.
However, we can also have a non-allocating implementation of
``mapIntervals``. This gives us the best of both worlds.

Motivation: Typed Unlifted Pointers
----------

Functions that allocate memory often take a callback argument that uses
the pointer. Consider ``alloca`` from ``Foreign.Marshal.Alloc``::

    alloca :: Storable a => (Ptr a -> IO b) -> IO b

The callback takes a lifted argument. This means that if ``alloca``
(or a similar function) is not inlined, the function passed to it
will end up being given a boxed argument at runtime. Most functions
that take a pointer as an argument are strict in that argument.
Typically, such functions have the worker wrapper transformation
applied to them, and the wrapper is inlined into the call site
to eliminate the boxing. However, when the function is passed
as an argument, this does not (and cannot) work.

It would be more performant manually unbox the argument::

    alloca :: Storable a => (Addr# -> IO b) -> IO b

But now we have lost our phantom ``a`` type variable. With ``UnliftedNewtypes``,
we could instead write::

    newtype Ptr# a = Ptr# Addr#
    alloca :: Storable a => (Ptr# a -> IO b) -> IO b

And now we have a variant of ``alloca`` that preseves the phantom
type variable without needlessly boxing the pointer.

Motivation: Typed Unlifted Arrays as a Library
----------

Currently, ``ArrayArray#`` offers an unsafe interface that does not keep track
of the element type. This problem, as well as a proposed solution, is described
in greater detail on the GHC issue tracker (See `this issue`_). Alternatively, the
`primitive`_ package offers a typeclass-based solution. If we ignore the
``PrimMonad`` machinery and specialize to ``ST``, the interface looks
like this::

    data UnliftedArray e
    data MutableUnliftedArray s e

    class PrimUnlifted a

    instance PrimUnlifted ByteArray
    instance PrimUnlifted (Array a)
    instance PrimUnlifted (MutableByteArray s)
    instance PrimUnlifted (MutableArray s a)

    indexUnliftedArray :: PrimUnlifted a => UnliftedArray a -> Int -> a
    readUnliftedArray :: PrimUnlifted a => MutableUnliftedArray s a -> Int -> ST s a
    writeUnliftedArray :: PrimUnlifted a => MutableUnliftedArray s a -> Int -> a -> ST s ()

.. _this issue: https://gitlab.haskell.org/ghc/ghc/issues/14196
.. _primitive: http://hackage.haskell.org/package/primitive-0.6.2.0/docs/Data-Primitive-UnliftedArray.html

However, typeclasses are not guaranteed to specialize. Users working with a
function built on top of these ``PrimUnlifted`` functions need to be
careful to ensure that specialization happens. Consider a function
like::

    -- | The first array is a list of target indices as machine integers.
    --   The length of the first argument must be the length of the second
    --   argument times the size in bytes of a machine integer.
    shuffleUnliftedArray :: PrimUnlifted a => ByteArray -> UnliftedArray a -> UnliftedArray a

Maybe this function is defined in such a way that it can be inlined
and subsequently specialized, or maybe we could add a ``SPECIALIZE`` pragma
to it. But it's madness that we even have to worry about this. All of the
``PrimUnlifted`` dictionaries are just ``unsafeCoerce`` (check the source
code). Specializations of ``shuffleUnliftedArray`` are all going to end
up being the same exact code. In this case, it isn't a big deal since
the implementation of ``shuffleUnliftedArray`` is probably short, but
if the function were larger, this would needlessly bloat the executable.

The solution in the aforementioned GHC issue is a more strongly typed
interface to arrays of unlifted things::

    data UnliftedArray# (a :: TYPE 'UnliftedRep)
    data MutableUnliftedArray# s (a :: TYPE 'UnliftedRep)

    indexUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). UnliftedArray# a -> Int# -> a
    writeUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). MutableUnliftedArray# s a -> Int# -> a -> State# s -> State# s
    readUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). MutableUnliftedArray# s a -> Int# -> State# s -> (# State# s, a #)
    unsafeFreezeUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). MutableUnliftedArray# s a -> State# s -> (#State# s, UnliftedArray# a#)
    newUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). Int# -> a -> State# s -> (# State# s, MutableUnliftedArray# s a #)

Notice that the type signature of ``shuffleUnliftedArray#`` under this scheme
would not have any typeclass constraints::

    shuffleUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). ByteArray# -> UnliftedArray# a -> UnliftedArray# a

However, adding these functions requires modifying GHC and adding
more primops. With ``UnliftedNewtypes``, this interface can be implemented from
the existing ``ArrayArray#`` interface without modifying GHC::

    newtype UnliftedArray# (a :: TYPE 'UnliftedRep) = UnliftedArray# ArrayArray#
    newtype MutableUnliftedArray# s (a :: TYPE 'UnliftedRep) = MutableUnliftedArray# (MutableArrayArray# s)

    indexUnliftedArray# :: forall (a :: TYPE 'UnliftedRep). UnliftedArray# a -> Int# -> a
    indexUnliftedArray# (UnliftedArray# a) i = unsafeCoerce# (indexArrayArrayArray# a i)

The data constructors of ``UnliftedArray#`` and ``MutableUnliftedArray#`` could
be hidden to prevent the user from unsafely casting elements.

Proposal Change Specification
----------

The restriction that a ``newtype`` wrap a type of kind ``TYPE LiftedRep``
would be dropped. It would be replaced by a restriction that the ``newtype``
must kind something of kind ``TYPE (r :: RuntimeRep)``. This proposal
does **not** include the ability for a ``newtype`` to wrap a ``Constraint``.
This does not require any additions to the language's grammar.

This proposal **would** allow a levity-polymorphic type variable to appear
inside a newtype. Such appearances are currently forbidden (and would remain
forbidden) in data constructors, since they violate the levity-polymorphism
binder rule. However, **newtype** constructors and pattern matches become casts.
Consider::

    newtype Id# (r :: RuntimeRep) (a :: TYPE r) = IdC# a

The calling convetion for the ``IdC#`` data constructor does not depend on
``r``, so code generation is still possible. All other restrictions around
levity polymorphism are still in place, so the following would be rejected::

    bad :: forall (r :: RuntimeRep) (a :: TYPE r). (a -> a -> Bool) -> Id# r a -> Id# r a -> Bool
    bad f (IdC# a) (IdC# b) = f a b

However, this would be accepted::

    good :: forall (a :: TYPE IntRep). (a -> a -> Bool) -> Id# IntRep a -> Id# IntRep a -> Bool
    good f (IdC# a) (IdC# b) = f a b

If the user does not specify the kind of an unlifted newtype with GADT syntax,
the kind should be inferred. Newtype that are recursive or
mutually recursive in a way that make them uninhabited will be inferred
to have lifted runtime representation. For example::

    newtype Foo = Foo Foo
    newtype Baz = Baz Tor
    newtype Tor = Tor Baz

All three of the above types are currently happily accepted by GHC, and
with ``UnliftedNewtypes``, they will remain accepted with the same kind
that they already had. Here are the same three types defined
using GADT syntax to illustrate what the inferred kind would be::

    newtype Foo :: TYPE 'LiftedRep where
      Foo :: Foo -> Foo
    newtype Baz :: TYPE 'LiftedRep where
      Baz :: Tor -> Baz
    newtype Tor :: TYPE 'LiftedRep where
      Tor :: Baz -> Tor

If the user wanted the levity-polymorphic variant of the uninhabited
newtype, they could write::

    newtype Bar :: TYPE r where
      Bar :: Bar -> Bar

Recursion in the presence of a changing runtime representation should
be rejected. For example::

   newtype Recurse = Recurse (# Int#, Recurse #)
   newtype Sneak = Sneak (# Sneak #)

Both of these types are ill-kinded, as their kinds would involve an
infinite nested of ``TupleRep``. The inferred kinds would be:

    Recurse :: TYPE (TupleRep [IntRep, TupleRep [IntRep, TupleRep ...]])
    Sneak :: TYPE (TupleRep [TupleRep [TupleRep ...]])

Just as terms cannot have infinite types, types cannot have infinite
kinds. This is only
a problem when a recursion of unlifted types is involved. To illustrate
the issue further::

    newtype BadA = BadA (# Word#, BadB #)
    newtype BadB = BadB (# Word#, BadA #)

    newtype GoodA = GoodA (# Word#, GoodB #)
    newtype GoodB = GoodB (Word#, GoodA)

The types ``BadA`` and ``BadB`` and ill-kinded and should be rejected.
However, ``GoodA`` and ``GoodB`` are well-kinded, and the kinds can
be inferred. More generally, if an unlifted newtype is well-kinded, then its kind
should **always** be inferrable.

Effects and Interactions
------------------------

**Generalized Newtype Deriving**: The interaction with GND is straigtforward.
Since typeclasses (since GHC 8.0) can accept unlifted types (or even
levity-polymorphic types), GND should work exactly for an unlifted newtype
as it does on a lifted newtype.

**GADT Syntax**: It is currently possible, although uncommon in practice, to
use GADT syntax with newtypes. With newtypes, GADT-like analysis of the type variable
is never allowed. The following is an example of a newtype using GADT syntax::

    newtype Foo :: Type -> Type where
      FooC :: a -> Foo a

Unlifted newtypes should be allowed to use GADT syntax as well. The only way this
differs from the status quo, is that kinds other than ``Type`` all now allowed
to the right of the final arrow. All of the following should be accepted::

    newtype PersonId :: TYPE 'IntRep where
      PersonId :: Int# -> PersonId
    newtype Id :: TYPE rep -> TYPE rep where
      Id :: a -> Id a
    newtype Pair# :: TYPE rep -> TYPE rep' -> TYPE (TupleRep '[rep, rep']) where
      Pair# :: (# a, b #) -> Pair# a b
    newtype Maybe# (a :: TYPE r) :: TYPE (SumRep '[r, TupleRep '[]]) where
      Maybe# :: (# a | (# #) #) -> Maybe# a


**Coercible**: Both ``~R#`` and the ``Coercible`` typeclass are already
levity polymorphic. However, the function ``coerce`` is not. This proposal
requires that ``coerce`` become levity polymorphic.

**Type Classes in Base**: This proposal does not change any type classes
in ``base`` or in any of the core libraries. Making typeclasses like ``Num``
levity-polymorphic would help a little with overloading, but no one has
measure what the impact of such a change would be on error message clarity.
Discussion of this issue is best had on the `Levity Polymorphic Type Classes`_
proposal.

.. _Levity Polymorphic Type Classes: https://github.com/ghc-proposals/ghc-proposals/pull/30

**Data Families**: Data families currently do not allow unlifted return kinds.
This means that the following is rejected by the compiler::

    data family Foo (a :: Type) :: TYPE 'IntRep

Under this proposal, this restriction would be lifted, not only in modules
where ``UnliftedTuples`` is enabled, but everywhere. Although defining
the data families itself would not require the extension, defining
instances would. Instances could be defined with ``newtype instance``::

    newtype instance Foo Bool = FooBoolConstructor Int#
    newtype instance Foo (Maybe a) = FooIntConstructor Int#

**Lazy unboxed tuples / Warn on unbanged strict patterns**: This proposal,
currently still under discussion, suggests tweaking the strictness of unboxed
tuple patterns. Regardless of whether that proposal is accepted, a variant of
it is accepted, or it is rejected, there is a simple rule for determining
the strictness of an unboxed newtype pattern. It
should agree with the strictness of an equivalent unboxed one-tuple pattern.
For example suppose we have::

    bar = ()
      where
      foo :: Bool
      (# (# 3#, foo #) #) = undefined

    newtype Wrap = Wrap (# Int#, Bool #)

    baz = ()
      where
      foo :: Bool
      Wrap (# 3#, foo #) = undefined

If ``bar`` throws an exception, then ``baz`` should too. If it doesn't,
then neither should ``baz``.

**Backpack**: Since GHC 8.4, backpack allows module signatures with
`type declarations of unlifted kinds`_. For example::

    signature NumberUnknown where
      import GHC.Types
      data Rep :: RuntimeRep
      data Number :: TYPE Rep
      plus :: Number -> Number -> Number

.. _type declarations of unlifted kinds: https://gitlab.haskell.org/ghc/ghc/issues/13955

Currently, these type can only be implemented by a type synonym,
not by a data declaration. Edward Yang discusses this in a `comment on the
aforementioned issue`_. This proposal would lift this restriction.

.. _comment on the aforementioned issue: https://gitlab.haskell.org/ghc/ghc/issues/13955#note_139218


Costs and Drawbacks
-------------------

Currently, all unlifted types have a hash appended to their name (``Array#``,
``Int#``, etc.). This happened because (1) GHC adopted this naming
convention and (2) no one had any way to define new unlifted types.
Since this proposal eliminates (2), users lose their easy visual cue
for knowing if a type is unlifted.

To the author's understanding (which is not great), the implementation
is not complicated and will be a comparitively small burden on maintainers.

Alternatives
------------

Unlifted newtypes are briefly mentioned in the much further-reaching
`unlifted data types`_ proposal. One alternative would be to wait for
a full implementation of unlifted data types. Then a single ``LANGUAGE``
pragma would enable both unlifted newtypes and unlifted data types.
The drawback of this is that the design of unlifted data types is
non-trivial, and their is no agreement on what they should actually
look like. Additionally, the implementation would be more
complicated than an implementation that only allowed unlifted
newtypes.

.. _unlifted data types: https://gitlab.haskell.org/ghc/ghc/wikis/unlifted-data-types

Alternatively, we could take a step in the other direction and simplify
this proposal. Disallowing levity-polymorphic newtypes might make this
easier to implement. Most of what this proposal has to offer comes
from the ability to work with an unlifted type whose data constructor
is hidden, and restricting users to the realm of the levity-monomorphic
does not take away from this.

Concerns
----------------

Currently, haddock does not indicate the kind of data types. For an unboxed
newtype, this would be desirable. Otherwise, from a cursory scan of a library's
docs, it would be easy to miss that a data type is unlifted (and consequently
cannot be used in most polymorphic functions).

Implementation
--------------

I do not have sufficient knowledge of GHC to implement this. I welcome anyone
else to implement it, or if it's approved and enough time goes by, I may
try to figure out how to implement it.

Richard Eisenburg has indicated that he might be interested in implementing
the proposal.


