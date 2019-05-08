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

Several constructs in Haskell define a new symbol without allowing the user to give
its full type. This proposal recommends a new extension ``-XTopLevelSignatures``
that will allow users to write type signatures for these constructs, defining
their full type. The signature must be inter-compatible with GHC's inferred type
for the construct. Here are some examples::

  class C a where
    toplevel meth  :: forall b a. C a => a -> b -> b     -- reorders type variables and constraint
    toplevel meth2 :: forall a. a -> a                   -- rejected: missing constraint C a
    toplevel meth3 :: forall a {b}. C a => a -> b -> b   -- with accepted proposal #26, makes b *inferred*
    toplevel meth4 :: C a => b -> a                      -- no forall required; this is redundant but allowed
    
  data Rec a b where
    Mk :: { field :: forall c. c -> c } -> Rec a b
    field :: forall c a b. Rec a b -> c -> c             -- new: type variables are reordered

  
.. _`#129`: https://github.com/ghc-proposals/ghc-proposals/pull/129
.. _`#126`: https://github.com/ghc-proposals/ghc-proposals/pull/126

Motivation
----------
New features of the type system have made available more and more choices for the types/kinds
of constructs. For implicitly-declared identifiers, there is sometimes little or no flexibility
around these choices. By allowing a top-level signature, we can override the defaults.

Along with providing the ability to write the examples above, this feature would subsume (the rejected) `#129`_,
which proposes mechanisms for allowing visible type application to instantiate overloaded literals,
as in ``3 @Int``. Here is how::

  class Num a where
    toplevel fromInteger :: Integer -> forall a. Num a => a

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

For patterns, in concert with `#126`_, we would normally like existential variables in a
pattern synonym to come *before* universals. Pattern synonyms rigidly do not allow this,
as they require a syntactic distinction between universals and existentials. However, under
this proposal, we can specify a new ordering for the variables and constraints.

Proposed Change Specification
-----------------------------
**Definition:** A type ``ty1`` is a **subtype** of a type ``ty2`` if ``ty1`` is more general
than ``ty2``. (In other words, ``ty1`` is a subtype of ``ty2`` if, when ``x`` has type ``ty1``,
``y :: ty2; y = x`` is accepted.) The **subtype** relation is reflexive.

**Definition:** A type ``ty1`` is **equivalent** to a type ``ty2`` if ``ty1`` is a subtype of
``ty2`` and ``ty2`` is a subtype of ``ty1``.

Top-level signatures, enabled by ``-XTopLevelSignatures`` are allowed for the following constructs:

1. **Class methods:** A class method declaration may now be prefixed with the new pseudo-keyword
   ``toplevel``, with the following rules:

   A. A ``toplevel`` method declaration is parsed just like a normal method declaration.

   B. The type must bring the class variables into scope for the result type of the signature.
      (See examples below.)

   C. The set of constraints on the result type of the signature must imply the enclosing class constraint.
      (See examples below.)

2. **Record field selectors:** A GADT-syntax datatype declaration may now include top-level types
   for field selectors.

   A. A field declaration signature may be included in the ``where`` clause of a GADT-syntax datatype
      definition. A field declaration signature is parsed just like an ordinary type signature; no ``toplevel``
      keyword is necessary. The distinguishing feature of a field declaration signature is that field
      names begin with a lower-case letter (or a symbol other than a ``:``), in opposition to data constructors.

   B. The field name must be used as a field name in one or more constructors of the type.

   C. The inferred type of the field selector (using the normal mechanism for inferring types of field selectors)
      must be equivalent to the type signature of the field selector.

The signatures given for the constructs are definitive: these are the types for those constructs in all occurrences
(including, in the case of class methods, in instance declarations).

In addition, `top-level kind signatures <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0036-kind-signatures.rst>`_
are updated to use ``-XTopLevelSignatures`` instead of ``-XTopLevelKinds``.

Examples
--------

Here are some examples of the rules for top-level signatures::

  class C a b where
    toplevel m1 :: a -> b                      -- rejected: no class constraint
    toplevel m2 :: a -> forall b. C a b => b   -- accepted: the return type has "a" and "b" in scope
    toplevel m3 :: D a b => a -> b             -- accepted: D a b implies C a b
    toplevel m4 :: C c d => c -> d             -- rejected: must use "a" and "b"
    toplevel m5 :: forall c d. C c d => c -> d -- still rejected
    toplevel m6 :: (forall a b. C a b => a -> b) -> ()                -- rejected: "a" and "b" not in scope in return type
    toplevel m7 :: (forall a b. C a b => a -> b) -> C a b => a -> b   -- accepted; "a" and "b" are shadowed in higher-rank type but are in scope at the end

  class C a b => D a b

  data T a b where
    MkT :: { field :: forall c. c -> c } -> T a b
    field :: T a b -> forall c. c -> c          -- accepted but redundant
    field :: forall c a b. T a b -> c -> c      -- accepted: variable order has changed
    field :: forall c {a} {b}. T a b -> c -> c  -- accepted: variable order and specificity has changed
    field :: T a b -> Int -> Int                -- rejected: type not equivalent
    other :: T a b -> Int                       -- rejected: other is not a field name

Note that only one signature for ``field`` above would be accepted.

Effect and Interactions
-----------------------
* This proposal increases the availability of visible type application by allowing users to customize
  the types of implicitly declared identifiers, including changing the specificity of some.

* Accepting this proposal obviates the problems in `#129`_.

* It is a bit regrettable that the ``forall`` in a class-method top-level signature isn't really bringing
  a fresh variable into scope -- or, rather, that we cannot alpha-vary the signature. But any design
  that allows that becomes more painful.

* This proposal does not allow for the use of top-level signatures for Haskell98 data constructors
  or field accessors. This restriction came about as a result of discussion, and the fact that GADT syntax
  was a viable workaround.

* This proposal does not address pattern synonyms, which would benefit from top-level signatures but
  are a hard nut to crack (syntactically).

* This proposal has no meaningful interactions with ``-XDuplicateRecordFields``.

* This proposal has no meaningful interactions with ``-XDefaultSignatures``.

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
* Users are free to define their own top-level wrappers with user-written types. However, these
  will have different names than the original constructs.

* A previous version of the proposal had the new signatures appear outside the class/datatype
  definitions, and had a few other syntactic oddities. I think this new version is superior.

Unresolved questions
--------------------
None at this time.

