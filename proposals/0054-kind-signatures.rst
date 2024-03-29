Standalone kind signatures
==========================

.. author:: Richard Eisenberg
.. date-accepted:: 2018-09-30
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/16794
.. implemented:: 8.10
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/54>`_ and `amended at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/259>`_.
.. contents::


This proposal adds *standalone kind signatures* allowing users to declare the kind of
type-level declarations introduced with ``type``, ``data``, ``newtype``, or ``class``.

For example::

  type MonoTagged :: Type -> Type -> Type
  data MonoTagged t x = MonoTagged x

  type Id :: forall k. k -> k
  type family Id x where
    Id x = x

  type C :: (k -> Type) -> k -> Constraint
  class C a b where
    f :: a b

  type TypeRep :: forall k. k -> Type
  data TypeRep a where
    TyInt   :: TypeRep Int
    TyMaybe :: TypeRep Maybe
    TyApp   :: TypeRep a -> TypeRep b -> TypeRep (a b)

Declarations that have a standalone kind signature (with no wildcards)
can use polymorphic recursion; declarations
without such signatures are understood to have inferred kinds, and polymorphic
recursion is not available. Note that the last example above, ``TypeRep``, uses
polymorphic recursion and would be rejected without the standalone kind signature.

This proposal replaces GHC's current notion of syntactic
CUSKs_.

.. _CUSKs: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#complete-user-supplied-kind-signatures-and-polymorphic-recursion


Motivation
------------
This is a simplification over the current story around CUSKs, which are fiddly and
unpredictable. For example, here_ are_ some_ tickets_ borne of confusion around CUSKs.

.. _here: https://gitlab.haskell.org/ghc/ghc/issues/12928
.. _are: https://gitlab.haskell.org/ghc/ghc/issues/10141
.. _some: https://gitlab.haskell.org/ghc/ghc/issues/13109
.. _tickets: https://gitlab.haskell.org/ghc/ghc/issues/13761

This new proposal makes the choice of whether or not to infer a kind much simpler.
Even better, this proposal makes type-level polymorphic recursion have the same rules
as term-level polymorphic recursion: you *must* have a standalone signature (with no
wildcards).

Proposed Change Specification
-----------------------------

1. Introduce a new top-level declaration form ::

     type <name_1> , ... , <name_n> :: <kind>

   (where ``n >= 1``) It is distinguishable from a type synonym by the lack of an ``=`` on the line. A
   type-level declaration with a standalone kind signature may use polymorphic recursion;
   one without is considered to have an inferred kind and may not use polymorphic recursion. One signature can apply to many names, just like a type signature.

   The kind given is checked against the declaration of the type. All kind generalization
   is done before ever examining the full declaration, just like how GHC treats type
   signatures.

   Associated types may not be given standalone kind signatures. (See "Costs and Drawbacks" for discussion.)

   Standalone kind signatures are enabled with the extension ``-XStandaloneKindSignatures``.

2. With ``-XExtendedForAllScope``, type variables ``tvs`` bound in the
   outermost ``forall tvs.`` in a standalone kind signature also scope over the
   type declaration, as if bound by ``@``-binders: ::

     type T1 :: forall k. k -> Type
     data T1    a = MkT (Proxy (a :: k))   -- equivalent to...

     type T2 :: forall k. k -> Type
     data T2 @k a = MkT (Proxy (a :: k))
     --      ^^
     --      implicit @-binder arising from the use of ExtendedForAllScope

   We say that ``-XExtendedForAllScope`` inserts *implicit ``@``-binders* at
   the beginning of the type declaration header.

   * **Aliasing**. If there are explicit, user-written ``@``-binders at the
     beginning of a type declaration header (i.e. where implicit ``@``-binders
     would have been inserted), aliasing takes place.

     Given a sequence of explicit binders ``[@j1, @j2, ...]`` and a sequence of
     implicit binders ``[@k1, @k2, ...]``, we do **not** concatenate them to
     produce ``[@j1, @j2, ..., @k1, @k2, ...]``. Rather, we overlay (or zip)
     them, producing pairs ``[(@j1, @k1), (@j2, @k2), ...]``, and then each
     pair acts as a single binder, where one name is an alias for another::

       type T3 :: forall k1 k2. k1 -> k2 -> Type
       data T3 @j1 @j2 a = ...  -- `k1` is an alias for `j1`, and `k2` for `j2`
       --      ^^^ ^^^
       --      user-written @-binders

     When there are too few explicit ``@``-binders, only the overlapping prefix
     is aliased, while the remaining variables are bound by implicit
     ``@``-binders::

       type Mixed :: forall k1 k2 k3 k4. k1 -> k2 -> k3 -> k4 -> Type
       data Mixed @j1 @j2 a = ...
          -- `k1` is in scope as an alias for `j1`
          -- `k2` is in scope as an alias for `j2`
          -- `k3` is in scope via an implicit @-binder
          -- `k4` is in scope via an implicit @-binder

   * **The Arity Restriction**. Per GHC Proposal `#425
     <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0425-decl-invis-binders.rst>`_,
     the arity of a non-generative type constructor (type synonym or type
     family) is influenced by the number of ``@``-binders: ::

       type T4 :: forall k. k -> Type
       type T4    = ...   -- arity = 0

       type T5 :: forall k. k -> Type
       type T5 @_ = ...   -- arity = 1

     This is not the case for implicit ``@``-binders arising from
     ``ExtendedForAllScope``. Insertion of implicit ``@``-binders stops as to
     not affect the arity of the type constructor. Names of type variables that
     are brought into scope by ``ExtendedForAllScope`` but fall outside the
     arity of the type constructor are "unusable"::

       type T6 :: forall k. k -> Type
       type T6    = Const k       -- error: the `k` is in scope but not usable (arity = 0)

       type T7 :: forall k. k -> Type
       type T7 @_ = Const k       -- OK (arity = 1)

     An "unusable" type variable name does not stand for any actual type
     variable, as the corresponding ``forall`` is not skolemized.
     An attempt to use such a variable name results in an error.

   * **The Order Restriction**. Type variable names are also "unusable" in
     positions that precede the corresponding implicit ``@``-binder::

       type T8 :: forall a b. blah
       data T8 @(a :: b) _ = ...
       --            ^^^
       --            error: the `b` is in scope but not usable

     The use of ``b`` in ``@(a :: b)`` is illegal because the implicit
     ``@``-binder for ``b`` is inserted at a later position::

       type T9 :: forall a b. blah
       data T9 @(a :: b) @b _ = ...
       --                ^^
       --                implicit @-binder arising from the use of ExtendedForAllScope

  * **GADTs and type families**. Just like explicit ``@``-binders, implicit
    ``@``-binders arising from ``ExtendedForAllScope`` do not scope over GADT
    constructor declarations or type family instances::

      type G :: forall k. k -> Type
      data G (a :: k) where     -- the `k` is in scope here (type declaration header)
        MkG :: ...              -- the `k` is /not/ in scope here (GADT constructor signature)

      type F :: forall k. k -> k
      type family F (a :: k) where    -- the `k` is in scope here (type declaration header)
        F ... = ...                   -- the `k` is /not/ in scope here (type family instance)

3. Introduce a new extension ``-XCUSKs``, on by default, that detects CUSKs as they
   currently exist. A CUSK will be treated identically to a standalone kind signature.

   When ``-XNoCUSKs`` is specified, only a standalone kind signature enables
   polymorphic recursion.

4. Plan to turn ``-XCUSKs`` off by default in GHC 8.8 and to remove it sometime thereafter.

Effect and Interactions
-----------------------
This is largely a simplification over the status quo, eventually eliminating the need for
the fiddly definition and detection of CUSKs. It allows users to control whether they want
inference or specification in a more conspicuous way than CUSKs do.

Note that a standalone kind signature, by itself, is insufficient in describing a type-level
construct in, say, an hs-boot file. The kind signature omits details like

* whether the type is generative and/or injective

* whether the type is open or closed

* whether the type must be applied to a certain prefix of arguments

I don't foresee intricate interactions with other features.

Template Haskell will need to be updated accordingly.

Note that this proposal depends on `#81`_, which adds a bit of syntax necessary
to give, e.g., ``data ProxyVis k (a :: k)`` a kind signature.

.. _`#81`: https://github.com/ghc-proposals/ghc-proposals/pull/81

Costs and Drawbacks
-------------------

Implementation Cost
~~~~~~~~~~~~~~~~~~~

Implementation should be rather straightforward, as this is a new syntactic construct.

Parsing may be slightly complicated by the similarity to a type synonym, but I doubt this
will pose more than an hour's delay in implementation.

Checking and generalizing the kind can be done by already-written code (in TcHsType).

The hardest part will be complicating the code in TcTyClsDecls, which is already somewhat
involved; however, I don't think this change will be invasive, as it will just affect the
code that currently checks for CUSKs.

Associated Types
~~~~~~~~~~~~~~~~

This proposal excludes signatures on associated types, as it was unclear how
best to choose a candidate from the design space.

If we had standalone kind signatures for associated types, would they look

… like this? (OUT) ::

 type T :: Type -> Type
 class C a where
   type T a

… or like this? (IN) ::

 class C a where
   type T :: Type -> Type
   type T a

The (IN) variant is syntactically ambiguous::

 class C a where
   type T :: a   -- standalone kind signature?
   type T :: a   -- declaration header?

The (OUT) variant does not suffer from this issue, but it might not be the
direction in which we want to take Haskell: we seek to unify type families and
functions, and, by extension, associated types with class methods. And yet we
give class methods their signatures inside the class, not outside. Neither do
we have the counterpart of ``InstanceSigs`` for standalone kind signatures.

The need for signatures for associated types is less pressing (they cannot be
recursive, because instances are independent of the family declaration), and so
we live without associated type signatures until a clear design presents
itself.

Alternatives
------------

* Don't do anything. I find the current situation to be confusing, though, generating
  several confused users yearly.

* A previous version of this proposal introduced a new type former ``~>``, which denoted
  *matchable* functions. Using ``~>``, a standalone signature could differentiate
  between the parameters of a type family that are required to be saturated and any others.
  However, this particular choice of syntax was bound to create confusion and disagreement.
  Furthermore, the particular way the syntax was designed was based on issues around
  *future*\-compatibility, and so was likely to end up being wrong, regardless.

* We don't need the ``type`` keyword to introduce non-symbolic kind signatures, as the
  capital letter can tip GHC off. Perhaps omit.

* With standalone kind signatures, some aspects of type declarations are redundant.
  (For example, the ``a b c`` in ``data T a b c where ...``.) One could imagine removing
  these as an extension to this proposal.

* Other transition plans are welcome. We could just abandon CUSKs entirely, asking the
  few users who play in this dark corner to use some CPP.

* Instead of introducing wholly new syntax, we could just tell GHC when to look for a CUSK.
  That is, we could have a new pragma ``{-# CUSK T #-}`` that tells GHC that ``T`` has a
  CUSK. If the pragma is absent, ``T`` does not have a CUSK. (Or, we could have a
  ``NO_CUSK`` pragma to countermand current behavior. This might have an easier transition
  story.) If ``T`` is labeled as having a CUSK, but does not, reject.

  This idea might be an improvement on my original proposal (it was inspired by a comment
  made on the original), but it still means that types have a different treatment from
  terms, which is aesthetically displeasing to me.


Unresolved questions
--------------------
These are essentially considered in the "Alternatives" section.


Implementation Plan
-------------------
I (or a close collaborator) will implement.
