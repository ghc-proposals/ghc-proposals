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
    meth4 :: b -> a
    
  meth :: forall b a. C a => a -> b -> b               -- new: reorders type variables and constraint
  meth2 :: forall a. a -> a                            -- rejected: missing constraint C a
  meth3 :: forall a {b}. C a => a -> b -> b            -- new: with proposal #99, makes b *inferred*
  meth4 :: C a => b -> a                               -- new: no forall required; this is redundant but allowed

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

 * Haskell98-syntax data/newtype constructors
 * Class methods
 * Record selectors (that are not duplicates)

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

This proposal interacts poorly with ``-XDuplicateRecordFields``, which allows you
to declare multiple record selectors with the same name in the same module. The use
of such a field at top-level would be ambiguous. Thus, this feature would not be
available with duplicate record fields. Here is an example of a rejected program::

  data T = MkT { x :: Int }
  data S = MkS { x :: Bool }
  x :: T -> Int

Note that the ``data`` declarations by themselves would be fine with ``-XDuplicateRecordFields``.
Note also that ``-XDuplicateRecordFields`` does not work with GHC's ``HasField`` mechanism;
this fact is unchanged by this proposal.

Note that this proposal does *not* affect the meaning of ``forall``: ``forall`` is not
required in top-level signatures. In addition, this new proposal does not interact with
``-XScopedTypeVariables``: the variables brought into scope in the top-level signatures
(even with ``forall``) do *not* scope over any definitions. (Instead, the usual rules
for getting type variables into scope still apply for implicitly declared identifiers.)

There is no requirement that the type variables in a top-level signature match up with
the names of the variables used in an identifier's declaration.

Costs and Drawbacks
-------------------
This has a non-trivial implementation burden, supporting a new meaning of type signature
and requiring a new type-equivalence check. The new types will probably also necessitate
inventing new wrappers to swizzle the type. (That is, the new types would be assigned to
new internal top-level definitions which would delegate to the real, original identifier.
This would add to the implementation complexity but should be completely transparent to
users.)

The syntax requires duplication of types and the new signatures can appear arbitrarily
far from the identifiers' definition sites. These drawbacks are real, but they exist
with all type signatures today. Type signatures are still useful as a double-check and
as documentation.

Alternatives
------------
* Users are free to define their own top-level wrappers with user-written types. However, these
  will have different names than the original constructs.

* There have been a few comments in wondering about class method signatures: couldn't we do
  this within the class declaration itself instead of outside? It's unclear to me what the
  syntax for this could be.

  + One suggestion was that signatures that start with a ``forall`` are top-level signatures
    instead of normal method signatures (which do not quantify over class variables or the
    class constraint). However, we can write ``forall`` on class method signatures today without
    changing their interpretation, so this idea is not backward compatible.

  + A refinement on this idea wsa that the new behavior could be triggered if the signature
    quantifies over the class variable. For example::

      class C a where
        meth :: forall b a. C a => a -> a

    Because this ``meth`` quantifies over the class variable, ``a``, it is treated as a
    top-level signature.

    I do not like this proposal overmuch: I would think that the ``a`` in ``meth``\'s signature
    shadows the class variable instead of replaces it. It is also unclear how this would work
    with multi-parameter type classes and functional dependencies. Note that the following
    is accepted today::

      class C2 a where
        meth2 :: forall a. a -> a

    The type of ``meth2`` is ``forall a. C2 a => forall a1. a1 -> a1``, renaming the inner
    variable to avoid shadowing. The definition requires ``-XAllowAmbiguousTypes`` but is
    otherwise sensible. (It is sensible, in that it has a meaning. It may or may not be
    *useful*.)

* We could omit treatment for Haskell98-syntax datatypes. After all, users can always use
  GADT syntax.

* For record selectors, we could require that the new signature be in a ``where`` clause
  (and be available only with GADT syntax). For example::

    data Rec a where
      MkRec :: { sel :: forall b. b -> a } -> Rec a
      sel :: forall b a. Rec a -> b -> a

Unresolved questions
--------------------
None at this time.


Implementation Plan
-------------------
Though I'd be happy to advise someone who wants to implement, I do not plan on implementing
this myself. It would make a decent project for someone who wants to get into GHC and wants
a challenge.
