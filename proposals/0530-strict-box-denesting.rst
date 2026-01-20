Strict Box Denesting
====================

.. author:: Matthew Craven
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/530>`_.
.. sectnum::
.. contents::


..
  This introduction shares a lot with the proposed change specification.
  When editing one, it is wise to consider editing the other!

This proposal seeks to add a new optimization flag, tentatively named
``-fstrict-box-denesting``, which (when enabled) allows GHC to elide
the allocation and traversal of all heap objects created by some data
constructors.  To see what that means, compare the following two examples:

::

   data    Example1 a = MkExample1 { exampleField1 :: !a }
   newtype Example2 a = MkExample2 { exampleField2 ::  a }

Today, any calls to ``MkExample1`` that remain after simplification
will allocate a heap object at run-time, while calls to ``MkExample2``
are no-ops.  As a result, a call to ``exampleField1`` requires
pointer-chasing to access the field stored in that heap object, while
a call to ``exampleField2`` is again a no-op.  But the semantic
differences between their declarations are minor, and do not actually
prevent a newtype-like elision of the ``MkExample1`` constructor at
run-time.

Under this proposal, when ``-fstrict-box-denesting`` is enabled, GHC
will automatically perform this newtype-like elision for constructors
that satisfy the following four conditions:

1. Among the constructor's fields (after unpacking, counting
   existentials and constraints), exactly one field has
   non-zero-width.  (Otherwise, no field alone has all of the relevant
   information.)
2. That field's type is represented by a GC-managed pointer.
   (Otherwise, passing it in lieu of a constructed heap object makes no
   sense.)
3. If that field is of lifted type, it must be a strict field.
   (Otherwise, ``undefined`` and ``WithLazyField undefined`` would
   need to be distinct.)
4. Either

   1. There is no other constructor in the relevant ``data`` or
      ``data instance`` declaration, or
   2. The underlying heap object for any evaluated expression with the
      field's type has the same tag as the constructor.

Condition 4 ensures that pattern-matching remains efficient.
Condition 4(ii) is checked on a best-effort basis.

Motivation
----------

This proposal begins to pull some weight in situations where no
analogous newtype such as the above ``Example2`` is within easy reach.
There are several reasons this might happen:


1. There may be existential type variables captured by the
   constructor.  In this case, there is a reasonably obvious
   equivalent newtype declaration, but it is simply not legal with any
   existing GHC version.

   ::

      -- Example: Type.Reflection.SomeTypeRep (in base)
      data SomeTypeRep where
          SomeTypeRep :: forall k (a :: k). !(TypeRep a) %1 -> SomeTypeRep

      -- Very desirable, but not yet legal:
      -- newtype SomeTypeRep = SomeTypeRep (exists k (a :: k). TypeRep a)


2. There may be a kind mismatch between the field and the constructed
   result type.  For example:

   ::

      -- Data.ByteString.Short.ShortByteString (in bytestring)
      data ShortByteString = SBS ByteArray#

   Here, the difficulty is that ``ByteArray#`` has kind ``UnliftedType``,
   while ``ShortByteString`` is an ordinary lifted ``Type``.  Coercion from
   ``ByteArray#`` to ``ShortByteString`` can be made zero-cost, but the
   reverse requires evaluation, because a ``ShortByteString`` may be a
   thunk!  We don't yet have a unidirectional coercion mechanism, and a
   language design for a one-way analogue of newtypes seems a long way
   off for now.


3. There may be a zero-width field serving some compile-time-only
   purpose.  For example:

   ::

      type Proof :: Bool -> Type
      data Proof p where Tautology :: Proof True

      type Named :: forall t -> t -> Type
      newtype t `Named` n = MkNamed t

      type SuchThat :: forall t -> (t -> Bool) -> Type
      data t `SuchThat` p where
        WithProof :: forall t (p :: t -> Bool) (n :: t)
                  .  {-# UNPACK #-} !(Proof (p n)) -> !(t `Named` n)
                  -> t `SuchThat` p

   Here, the ``Named`` newtype can be used to somewhat emulate
   dependent types and write interfaces that provide unusually strong
   compile-time guarantees.  The ``Proof (p n)`` field of ``WithProof``
   unpacks to a zero-width field of type ``p n ~# True`` in the
   constructor worker, so ``WithProof`` satisfies  conditions 1-4.
   But that ``Proof (p n)`` field carries compile-time evidence
   that may be necessary to satisfy the typechecker when using an emulated
   dependently-typed interface, so throwing it away by using a newtype
   around ``exists (n :: t). t `Named` n`` would be unacceptable.

   ..
     A wrapper around a more complex type like
     ``exists (n :: t). (# UnboxedRep (Proof (p n)), t `Named` n #)``
     is perhaps plausible in some distant future.


4. With the ``LinearTypes`` extension, a data constructor can have any
   multiplicity on its fields, while a newtype constructor must be
   linear in its field.  Thus, the analogous newtype to this variant on
   ``linear-base``'s ``Ur`` is not allowed:

   ::

     data StrictUr a where
       StrictUr :: !a %Many -> Ur a

   (It is not possible to consume a value of type ``StrictUr a``
   linearly without forcing the contents of its "unrestricted" field,
   making this type slightly less flexible than the lazy ``Ur``.)


5. There may be more than one constructor in the data type.  In this
   case, condition 4(ii) must apply.  The original motivating
   example for this condition is ``GHC.ForeignPtr.ForeignPtrContents``:

   ::

      data ForeignPtrContents
        = PlainForeignPtr !(IORef Finalizers)
        | FinalPtr
        | MallocPtr (MutableByteArray# RealWorld) !(IORef Finalizers)
        | PlainPtr (MutableByteArray# RealWorld)

   This type is used to optionally provide support for finalizers on
   ``ForeignPtr``\ s, as well as to keep alive GC-managed pinned
   buffers that may be stored in ``ForeignPtr``\ s.  Depending on a
   potential re-ordering of its constructors, either ``PlainPtr`` or
   ``PlainForeignPtr`` (but not both) could satisfy condition 4(ii)
   and be made denestable.  The former would remove two words of
   allocation overhead in the buffer creation for any fresh
   ``StrictByteString``, while the latter would make attaching
   finalizers to external buffers received from the FFI more
   efficient.


..
   The links in "many examples of wrapper types" in the next paragraph
   provide almost exactly the 4(i)-denestable constructors found in the
   boot libraries of a prototype GHC detecting them, with three omissions.
   Two are the examples above for reasons 1 and 2: SomeTypeRep and SBS.
   The last one is base:Control.Concurrent.QSemN.QSemN, which I omitted
   because it could have been written as a newtype around IORef, but was not,
   for reasons unknown to me.

All five of these situations are fairly uncommon in general-purpose
Haskell code.  However,
`m <https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.Event.Arr.html#Arr>`_\
`a <https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.Conc.Sync.html#TVar>`_\
`n <https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.Conc.Sync.html#ThreadId>`_\
`y <https://hackage.haskell.org/package/base-4.17.0.0/docs/src/Data.Array.Byte.html#ByteArray>`_
`e <https://hackage.haskell.org/package/bytestring-0.11.3.1/docs/src/Data.ByteString.Short.Internal.html#BA%23>`_\
`x <https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.StableName.html#StableName>`_\
`a <https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.Event.IntVar.html#IntVar>`_\
`m <https://hackage.haskell.org/package/ghc-bignum-1.3/docs/src/GHC.Num.BigNat.html#BN%23>`_\
`p <https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.STRef.html#STRef>`_\
`l <https://downloads.haskell.org/~ghc/9.4.1-alpha3/docs/libraries/ghci/src/GHCi.CreateBCO.html#EmptyArr>`_\
`e <https://hackage.haskell.org/package/base-4.17.0.0/docs/src/Data.Array.Byte.html#MutableByteArray>`_\
`s <https://hackage.haskell.org/package/text-2.0/docs/src/Data.Text.Array.html#ByteArray>`_
`o <https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.MVar.html#MVar>`_\
`f <https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.IOPort.html#IOPort>`_
`w <https://hackage.haskell.org/package/ghc-bignum-1.3/docs/src/GHC.Num.WordArray.html#MutableWordArray>`_\
`r <https://hackage.haskell.org/package/ghc-9.2.4/docs/src/GHC.Data.FastMutInt.html#FastMutInt>`_\
`a <https://hackage.haskell.org/package/bytestring-0.11.3.1/docs/src/Data.ByteString.Short.Internal.html#MBA%23>`_\
`p <https://hackage.haskell.org/package/ghci-8.10.2/docs/src/GHCi.BreakArray.html#BA>`_\
`p <https://hackage.haskell.org/package/text-2.0/docs/src/Data.Text.Array.html#MutableByteArray>`_\
`e <https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.Stack.CloneStack.html#StackSnapshot>`_\
`r <https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.ForeignPtr.html#MyWeak>`_
`t <https://hackage.haskell.org/package/ghc-bignum-1.3/docs/src/GHC.Num.WordArray.html#WordArray>`_\
`y <https://hackage.haskell.org/package/text-2.0/docs/src/Data.Text.Array.html#MutableByteArray>`_\
`p <https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.Weak.html#Weak>`_\
`e <https://hackage.haskell.org/package/bytestring-0.11.3.1/docs/src/Data.ByteString.Builder.RealFloat.Internal.html#ByteArray>`_\
`s <https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.Event.Unique.html#US>`_
for which reason 2 applies can be found in essential libraries and tools.
Some of these are meant mainly for internal use, but others (including
``MVar``, ``STRef`` (and by extension ``IORef``), ``ThreadId``, ``Weak``,
and the aforementioned ``ShortByteString``) are essential parts of the
user-facing API.


GHC is already able to eliminate some packing and unpacking of these
data types using less specific tools like case-of-known-constructor
and the worker/wrapper transformation.  However, these tools share two
limitations: A value can only be unpacked if it is used strictly and
its type is specific enough to determine the relevant constructor.
Notably, the latter implies that the contents of standard containers
such as arrays, lists, sets, and tuples will never be unpacked.

These limitations should be familiar: The same ones apply to unboxing
``Int``\ s.  It is usually possible to work around these limitations,
and libraries defining such types often strive to do so at their
internal use-sites.  However, doing so can be a chore.  The workarounds
can have costs to code readability and are only effective when they
are maintained.  (The workaround of monomorphising containers even has
run-time cost, since it necessarily increases code size.)

..
  There are a few less essential limitations in today's GHC as well.
  A constructor field of an unlifted type is always treated as a lazy
  field and thus not unpacked, even with an explicit {-# UNPACK #-}
  pragma.  Data types that capture an existential type variable are in
  the same way ineligible for unpacking, and are also ignored by CPR
  analysis.

..
  TODO: perhaps add links to past conversations demonstrating demand




Proposed Change Specification
-----------------------------
..
  This section shares a lot with the introduction.
  When editing one, it is wise to consider editing the other!

This proposal introduces a new optimization flag, tentatively named
``-fstrict-box-denesting``, which will be enabled by default with
``-O1``.  It also introduces two new pragmas, ``{-# DENEST #-}`` and
``{-# NODENEST #-}`` which may appear before data constructors. When
the ``-fstrict-box-denesting`` flag is active while compiling a
``data`` or ``data instance`` declaration, its constructors not marked
``{-# NODENEST #-}`` are tested for "denestability." A constructor is
considered denestable when it satisfies these four conditions:

..
  These are identical to the four conditions given in the introduction.

1. Among the constructor's fields (after unpacking, counting
   existentials and constraints), exactly one field has
   non-zero-width.
2. That field's type is represented by a GC-managed pointer.
3. If that field is of lifted type, it must be a strict field.
4. Either

   1. There is no other constructor in the relevant ``data`` or
      ``data instance`` declaration, or
   2. The underlying heap object for any evaluated expression with the
      field's type has the same tag as the constructor.

All constructors are checked for condition 4(i) before condition
4(ii).  Condition 4(ii) is checked on a best-effort basis, but
the following particular cases are expected to work:

* If a constructor is the first in its ``data`` or ``data instance``
  declaration, the field's type corresponds to a ``data`` or ``data
  instance`` declaration with exactly one constructor, and that
  field's constructor was not itself denested with condition 4(i)
* If a constructor is the first in its ``data`` or ``data instance``
  declaration, and the field's type is a boxed unlifted primitive

The latter implies that, under this proposal, the boxed unlifted
primitives have (one-indexed) tag 1.

If a constructor is marked ``{-# DENEST #-}`` but is determined not
to be denestable, a warning is emitted.  This is the only effect of
the ``{-# DENEST #-}`` pragma.

A constructor determined to be denestable will not generate any code
at any of its saturated use sites (even use-sites in modules for which
``-fstrict-box-denesting`` is disabled), so that using or
pattern-matching against this constructor has no run-time cost, except
to the extent that doing so forces evaluation to take place.

Whether a constructor is determined denestable or not has no effect on
the surface-language semantics of that constructor.  In particular,
despite the similarity to newtype constructors, a denestable
constructor does not introduce ``Coercible`` instances and
pattern-matching a value against a denestable constructor does force
evaluation of that value.


Additionally, to mitigate an obscure breaking interaction, the type of
the ``dataToTag#`` primitive (and that of its alias
``GHC.Base.getTag``) will be changed as follows:

::

   -- Today,
   dataToTag#, getTag
     :: a -> Int#

   -- Under this proposal,
   dataToTag#, getTag
     :: forall {l :: Levity} (a :: TYPE (BoxedRep l))
     .  DataToTag a => a -> Int#

``DataToTag`` is a new built-in class with special solving behavior,
similar to existing special behavior for ``Coercible`` and ``WithDict``.
Specifically, the constraint ``DataToTag t`` is always soluble when
``t`` is an algebraic data type (i.e. ``t`` matches a ``data`` or
``data instance`` declaration) with all of its constructors in scope.

Finally, since it is no longer a primop, ``dataToTag#`` will be moved
from ``GHC.Prim`` to ``GHC.Magic``.  It will remain re-exported from
``GHC.Exts``.

..
  TODO: Specify syntax for the (NO)?DENEST pragmas


Examples
--------

In all of the following examples and non-examples of denestable
constructors, the proposed ``-fstrict-box-denesting`` flag is assumed
to be active.



::

   data    Example1 a = MkExample1 { exampleField1 :: !a }
   newtype Example2 a = MkExample2 { exampleField2 ::  a }

The ``MkExample1`` constructor is inferred as denestable.
The ``MkExample2`` constructor is not denestable, because it
corresponds to a ``newtype`` declaration rather than a ``data`` or
``data instance`` declaration.



::

   data Example3 a where
     MkExample3Int  :: ![Int] -> Example3 Int
     MkExample3Bool :: ![Bool]  -> Example3 Bool

   data family Example4 a
   data instance Example4 Int  = MkExample4Int  ![Int]
   data instance Example4 Bool = MkExample4Bool ![Bool]

Examples 3 and 4 may appear similar.  However, ``MkExample3Int`` and
``MkExample3Bool`` violate condition 4 and so are not denestable,
while ``MkExample4Int`` and ``MkExample4Bool`` are found to be
denestable, satisfying condition 4(i) because they do not belong to
the same data instance declaration.

This distinction is appropriate: the data family constructors
provide users less freedom.  Consider this function:

::

   example3Motivator :: Example3 t -> t
   example3Motivator (MkExample3Int  li) = last (14 : li)
   example3Motivator (MkExample3Bool li) = and li

This function type-checks.  When called at run-time, it must be able to
choose the appropriate branch.  Since types are erased at run-time,
the only way it can do so is by distinguishing between the
``MkExample3Int`` and ``MkExample3Bool`` constructors.  This feat
becomes impossible if both constructors are denested, and at least
becomes more difficult if only one of the two constructors is
denested.

The analogous function for ``Example4`` fails to typecheck because
pattern-matching against a data instance constructor is only
possible when the scrutinee type matches the data instance head.



::

   -- isomorphic to Data.Constraints.Dict (in constraints)
   data Example5 (c :: Constraint) where
     MkExample5 :: c => Example5 c

The ``MkExample5`` constructor of ``Example5 c`` has one field, which holds the
run-time evidence for the constraint ``c``, represented by a GC-managed
pointer, usually to an instance-method-dictionary-object.  However,
even with ``-fdicts-strict``, GHC will not make this a strict field, as
doing so is incompatible with the newtype-class optimization.
Today, the only evidence fields GHC makes strict are the implicit
equality evidence fields of a GADT constructor.  So, ``MkExample5``
does not satisfy condition 3 and is therefore not denestable.

..
  TODO: examples with condition 4(ii)
        motivating example for NODENEST
        performance examples

  Do I really need a DataToTag example? I hypothesize nobody is
  actually affected by that change except the proposal implementors.


Effect and Interactions
-----------------------

The main effect of this proposal would be to cause some Haskell
programs to run faster and use less memory.

This proposal interacts nontrivially with the ``dataToTag#`` primop,
which currently always evaluates its argument and returns the index of
the data constructor (if it exists) used to produce the resulting
evaluated heap object.  Since ``-fstrict-box-denesting`` affects the
set of things that produce new evaluated heap objects, it can affect
the result of a call to the current implementation of
``dataToTag#``.  For example, ``dataToTag# (MkExample1 [True])`` with
its current implementation may return either

* the index ``0#`` of the ``MkExample1`` constructor, or
* the index ``1#`` of the ``(:)`` constructor of the underlying
  heap object ``(:) True []``

\...depending on whether or not the ``-fstrict-box-denesting`` flag is
active when in the module containing ``Example1`` is compiled.  This
proposal suggests to retain the current behavior of ``dataToTag#``
regardless of the ``-fstrict-box-denesting`` flag by allowing a
different implementation to be used for types with a denestable
constructor, via run-time evidence of a ``DataToTag`` typeclass
constraint.



Costs and Drawbacks
-------------------
1. Today, most or all of GHC's boxed primitive types such as
   ``ByteArray#`` cannot be safely stored in a value with lifted type,
   as they are zero-tagged pointers (so they will be entered when
   evaluated) but entering them results in a panic.  These panics have
   been useful for GHC developers in the past, but are incompatible
   with this proposal as it is currently written, since it allows
   a wrapper such as ``ShortByteString`` to be represented at run-time
   by its underlying ``ByteArray#``.

   Today, evaluating a zero-tagged lifted object involves (at best)
   two levels of pointer-chasing to read the closure type from the
   info table, while non-zero-tagged pointers are recognized as
   evaluated without any memory accesses.  So, for performance
   reasons, the 'panic-on-eval' problem is best resolved by making the
   proper tag for unlifted primitives non-zero.

   Making the proper tag on these unlifted primitives non-zero also
   has the benefit of making condition 4(ii) applicable to more
   constructors.

   The panicking entry code can very likely be kept, but will now
   detect a different class of bugs: those where pointer tags are not
   preserved or where tag inference incorrectly concludes that a
   pointer must be untagged.  (See also `GHC issue 21792
   <https://gitlab.haskell.org/ghc/ghc/-/issues/21792>`_\ .)

2. This proposal calls for a breaking change to the type of the
   ``dataToTag#`` primitive, making it not applicable in some
   situations where it can currently be used.

3. The heap objects elided at run-time by ``-fstrict-box-denesting``
   may occasionally provide useful information for debugging or profiling.


The implementation of this proposal is not expected to pose major
challenges or incur much ongoing maintenance cost.

This proposal is expected to have almost no impact on Haskell's
learnability: Its only effect on program meaning is a change to the
type of an obscure primitive.  Like almost any optimization, it does
increase the complexity of GHC-Haskell's run-time cost behavior.  But
this proposal should not greatly affect users' optimization decisions
outside of the relatively advanced use-cases described in the
Motivation section, since newtypes will remain more easily optimized
by GHC in Core.  As a result, this proposal should also not contribute
much to the difficulty of learning that run-time cost behavior.



Alternatives
------------
1. To avoid the first listed drawback, it is possible to place
   restrictions on when a constructor with a boxed unlifted
   field of type is considered denestable.  Options include:

   1. Only allow denesting when the constructor's result type is also unlifted.
   2. Only allow denesting when the field's type is an algebraic data type.

   However, the abundance of lifted wrappers around boxed primitives
   in use by Haskellers everywhere suggests that the potential
   performance improvements of allowing denesting in these cases
   outweighs the GHC-internal benefits of the current behavior.

2. Another way of handling the ``dataToTag#`` interaction is to keep
   its current type but simply document the change in its behavior.
   This is a silent breaking change but perhaps an acceptable one: The
   currrent behavior of ``dataToTag#`` at the affected types is to
   evaluate its argument and then unconditionally return ``0#``.  This
   can be expressed far more clearly using other tools and is also
   rather useless.  In a generic programming setting, ``dataToTag#``
   is clearly the wrong tool to reach for:

   * ``Data.Data`` gives the ``AlgConstr`` constructor a ``ConIndex`` field.
   * ``GHC.Generics`` code works with ``Rep`` types that never have
     more than two constructors.

   And in just about any other setting, the types should be clear
   enough to make the uselessness apparent to any human programmer with
   enough arcane knowledge to know what ``dataToTag#`` actually does.

   This user-facing part of this proposal's approach to managing this
   interaction (introducing a built-in typeclass to validate uses of
   ``dataToTag#``) has already been independently suggested and is
   tracked at `GHC issue 20532 <https://gitlab.haskell.org/ghc/ghc/-/issues/20532>`_.
   Selecting an implementation of ``dataToTag#`` based on its argument
   type can also be used to make it more efficient, for example by
   skipping the large-tag check when the relevant type does not have
   too many constructors. (This is tracked at `GHC issue 21710
   <https://gitlab.haskell.org/ghc/ghc/-/issues/21710>`_.)

3. It may be desirable to weaken condition 4(ii) to allow a
   non-zero-cost "tag-changing denesting" in some cases where all
   values of the field's type have the same tag, but this tag does not
   match the constructor's tag.  However, doing so would break some
   invariants in the RTS that are currently used for sanity-checking
   and debugging of GHC and its RTS, so this proposal does not call
   for any form of denesting in these cases.

4. This proposal specifies that, when compiling with optimizations,
   denesting is opt-out rather than opt-in.  The opt-in alternative of
   making ``{-# DENEST #-}`` pragmas mandatory before denesting is
   applied was rejected because it serves little purpose beyond
   creating busy-work for library maintainers before users can take
   full advantage of the performance benefits denesting brings.

5. Another way of allowing efficient lifted wrappers around unlifted
   primitives is to provide an opaque built-in type ``Lazy`` with kind
   ``UnliftedType -> Type`` and primitive operations
   ``toLazy :: a -> Lazy a`` and ``fromLazy :: Lazy a -> a`` that, in
   the same way as this proposal's denestable constructors, perform no
   boxing or unboxing at run-time.

   Although this alternative can provide comparable performance
   advantages in the most important motivating examples and avoids any
   interaction with ``dataToTag#``, existing wrappers must be
   re-written before they can actually take advantage of a blessed
   ``Lazy`` wrapper.  This creates work for downstream maintainers.
   Additionally, this technique does not yet generalize conveniently
   to all of the motivating examples.

   ..
     TODO? Elaborate more on why and how it fails to generalize...


Unresolved Questions
--------------------
1. The closely related field unpacking information is exposed in the
   ``GHC.Generics`` interface as part of the ``DecidedStrictness`` for
   that field.  Why is that?  Does the same reasoning suggest adding
   some ``DecidedDenesting`` field to ``GHC.Generics.MetaCons``?

2. Is there a better/clearer name for this optimization/feature?



Implementation Plan
-------------------

The implementation of this proposal will consist of three largely
independent parts:

1. Wire in and give special solving behavior to a ``DataToTag`` class.
   This is expected to be very similar to the recent work that
   implemented the ``WithDict`` class.
2. Modify the boxed primitives and their operations so that their
   pointers are tagged with 1 instead of 0.
3. Detect denestable constructors and modify the post-Core phases of
   compilation to actually elide them.

If accepted, the proposal author will work to implement this feature.
Toward that end, a fully-functional
`draft implementation <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8912>`_
of the ``DataToTag`` part of this proposal has already been produced.

Andreas Klebinger has also expressed an interest in working on this
feature and has already produced a `prototype implementation
<https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8445>`_ of the
main feature (3) of this proposal.
