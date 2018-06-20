Top-level signatures
====================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/148>`_.
.. sectnum::
.. contents::

Many constructs in Haskell define a new symbol without allowing the user to give
its full type. This proposal recommends a new extension ``-XTopLevelSignatures``
that will allow users to write type signatures for these constructs, defining
their full type. The signature must be inter-compatible with GHC's inferred type
for the construct. Here are some examples::

  data T1 a = forall b. MkT1 b (b -> a)                -- implicitly declares MkT1
  MkT1 :: forall b a. b -> (b -> a) -> T1 a            -- new: signature reorders the b and a variables
  type T1 :: Type -> Type                              -- new: signature gives a kind to T1

  data T2 = MkT2 { unT2 :: Int }
  unT2 :: T2 -> Int                                    -- new: we can see the full type for unT2

  data P a = MkP
  type P :: (Type -> Type) -> Type                     -- new: specifies kind of a phantom parameter

  class C a where
    meth :: a -> b -> b
    meth2 :: a -> a
    meth3 :: a -> b -> b
  meth :: forall b a. C a => a -> b -> b               -- new: reorders type variables and constraint
  meth2 :: forall a. a -> a                            -- rejected: missing constraint C a
  meth3 :: forall a {b}. C a => a -> b -> b            -- new: with proposal #99, makes b *inferred*

This proposal subsumes `#54`_, which proposes this feature, but only for type-level
declarations (which would replace CUSKs).

.. _`#54`: https://github.com/ghc-proposals/ghc-proposals/pull/54
.. _`#129`: https://github.com/ghc-proposals/ghc-proposals/pull/129
.. _`#99`: https://github.com/ghc-proposals/ghc-proposals/pull/99


Motivation
----------
New features of the type system have made available more and more choices for the types/kinds
of constructs. For implicitly-declared identifiers, there is sometimes little or no flexibility
around these choices. By allowing a top-level signature, we can override the defaults.

Along with providing the ability to write the examples above, this feature would subsume `#129`_,
which proposes mechanisms for allowing visible type application to instantiate overloaded literals,
as in ``3 @Int``. Here is how::

  class Num a where
    fromInteger :: Integer -> a
  fromInteger :: Integer -> forall a. Num a => a

In `#129`_, it is explained that the problem is that ``3`` expands to ``fromInteger 3``. Thus,
``3 @Int`` would expand to ``fromInteger 3 @Int``. Sadly, this last expression is ill-typed with
the current type of ``fromInteger``, which is ``forall a. Num a => Integer -> a`` -- the ``@Int``
comes too late. But with the type I propose above, ``fromInteger 3 @Int`` works beautifully.
Note that no Haskell98 code is broken by this change, as only visible type application can observe
the difference between ``fromInteger :: forall a. Num a => Integer -> a`` and
``fromInteger :: Integer -> forall a. Num a => a``.

Further motivation includes the ability to do this type variable shuffling with any implicit
identifier, though I know of no use case quite as compelling as ``fromInteger``.

Top-level signatures can also serve as nice documentation. Particularly for record selectors,
it can sometimes be confusing to see ``data T = MkT { sel :: Int }``, which suggests that
``sel`` has type ``Int``. when really ``sel`` has type ``T -> Int``. A top-level signature
can make this clear.

For type-level signatures, I defer to `#54`_, which this proposal is a direct extension of.

Proposed Change Specification
-----------------------------
Top-level signatures, enabled by ``-XTopLevelSignatures``
are allowed for the following term-level constructs:

 * Haskell98-syntax data constructors
 * Class methods
 * Record selectors

The type in the signature must be equivalent with respect to GHC's subtype relation
to the one GHC would normally assign the construct. That is, the new type may shuffle
the ordering, placement, and specificity of invisible parameters (type variables and
constraints) only. All occurrences of the identifiers in question use the declared
type in the top-level signature.

This proposal also subsumes and extends `#54`_, which I will not re-detail here, as it is already
under consideration by the committee.

Effect and Interactions
-----------------------
This proposal increases the availability of visible type application by allowing users to customize
the types of implicitly declared identifiers, including changing the specificity of some. This means
that `#99`_ need not consider these implicitly declared identifiers.

Accepting this proposal obviates the problems in `#129`_.

If this proposal is accepted, both `#54`_ and these new features should be enabled by
``-XTopLevelSignatures`` (instead of the ``-XTopLevelKinds`` in `#54`_).

Costs and Drawbacks
-------------------
This has a non-trivial implementation burden, supporting a new meaning of type signature
and requiring a new type-equivalence check. The new types will probably also necessitate
inventing new wrappers to swizzle the type. (That is, the new types would be assigned to
new internal top-level definitions which would delegate to the real, original identifier.
This would add to the implementation complexity but should be completely transparent to
users.)

Alternatives
------------
Users are free to define their own top-level wrappers with user-written types. However, these
will have different names than the original constructs.

Unresolved questions
--------------------
None at this time.


Implementation Plan
-------------------
Though I'd be happy to advise someone who wants to implement, I do not plan on implementing
this myself. It would make a decent project for someone who wants to get into GHC and wants
a challenge.
