First-class existentials
========================

.. author:: Richard Eisenberg
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/473>`_.
.. contents::
.. sectnum::

This proposal introduces first-class existentials into GHC, where type
inference figures out where to pack and unpack existentials, with no need
for user annotations. It is based on the ICFP'21 paper `An Existential
Crisis Resolved: Type inference for first-class existential types <TODO>`_.

.. _paper: https://richarde.dev/papers/2021/exists/exists.pdf
.. _`#270`: https://github.com/ghc-proposals/ghc-proposals/pull/270
.. _`#285`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0285-no-implicit-binds.rst
.. _`#281`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst
.. _T2T: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst#t2t-mapping
.. _`#17934`: https://gitlab.haskell.org/ghc/ghc/-/issues/17934

Motivation
----------

1. Richly typed programming invariably uses its share of existential types,
   and this proposal makes it vastly easier to work with existentials.
   Currently, every existential must be encoded using its own datatype,
   which is laborious. Furthermore, packing and unpacking these datatypes
   must be done by hand, which is cluttersome.

   .. _filter:

#. A simple example, clearly aided by existentials is ``filter`` over
   length-indexed vectors::

     data Nat = Zero | Succ Nat

     type Vec :: Nat -> Type -> Type
     data Vec n a where
       VNil :: Vec Zero a
       (:>) :: a -> Vec n a -> Vec (Succ n) a
     infixr 5 :>

     filter :: (a -> Bool) -> Vec n a -> Vec ???? a

   Because we cannot know the behavior of the filtering function on every
   element in the input, we cannot know the length of the output of ``filter``.
   We thus need to existentially quantify that length: there exists some length,
   but we do not know what it is.

   Here is how we could write this with this proposal::

     filter :: (a -> Bool) -> Vec n a -> exists m. Vec m a
     filter p VNil = VNil
     filter p (x :> xs)
       | p x       = x :> filter p xs
       | otherwise = filter p xs

   This implementation is simple -- it is exactly what we would write
   without worrying about the type-level index, beyond the use of ``exists``
   in the type.

   A point expanded in the `paper`_ is that this implementation is *lazy*,
   just like ``filter`` should be. I do not believe a lazy implementation
   of ``filter`` over length-indexed vectors is possible in GHC's current
   type system.

#. Suppose we have a pretty-printer based around the following class::

     class Pretty a where
       ppr :: a -> Doc

   We would naturally have ::

     instance Pretty a => Pretty [a] where ...

   Yet, if I have ``woz :: Woz`` and ``wiz :: Wiz`` (with instances for
   both types), I cannot ``ppr [woz, wiz]``, because that creates a
   heterogeneous list.

   With this proposal, I could write ::

     pprList :: [exists a. Pretty a /\ a] -> Doc
     pprList = sep . map ppr

   and then have ``pprList [woz, wiz]``.

   It would be even better to have ``ppr [woz, wiz]``, but that seems
   beyond the abilities of type inference at the moment.

#. The refinement types of Liquid Haskell often look something like this::

     plusNat :: { x :: Nat } -> { y :: Nat } -> { v :: Nat | v >= x && v >= y }

   where the result type has a refinement making a claim about the result
   of running the function.

   It would amplify the power of Liquid Haskell to have its refinement types
   interact with other type system features in Haskell. Accordingly, we might
   want to represent the inputs as pi-types and the output as a sigma-type --
   which is essentially the same as an existential. Here might be one rendering::

     plusNat :: foreach (x :: Nat) (y :: Nat) -> exists (v :: Nat). Proof (v >= x && v >= y)

   Yet we do not want to manually pack and unpack the existential in the
   definition for ``plusNat`` -- and thus need the inference capabilities proposed
   here.

   Note that this proposal does not go "all the way" toward this encoding of
   refinement types, in that we would not be able to write the type above with
   this proposal. Nevertheless, the automatic inference of packing and unpacking
   described here seems necessary if we are to integrate Liquid Haskell with the
   rest of GHC's type system.

Proposed Change Specification
-----------------------------

1. Introduce a new extension ``-XExistentialTypes``.

#. With ``-XExistentialTypes``, ``exists`` is a keyword in both
   types and terms.

#. With ``-XExistentialTypes``, introduce a new type for existentials.

   1. The grammar is modified as follows (baseline: GHC's parser)::

        ctype → forall_telescope ctype
              | context '=>' ctype
              | exists_telescope ctype   -- NEW!
              | ctype
              | ...

          -- just for comparison
        forall_telescope → 'forall' tv_bndrs '.'
                         | 'forall' tv_bndrs '->'

        exists_telescope → 'exists' tv_bndrs '.'

      An existential is a new form of type, not equal to any current form.

   #. The ``ty`` in ``exists tv_bndrs . ty`` is not allowed to be a
      ``forall`` type or a qualified type (headed by ``=>``).

   #. In a type ``exists tv_bndrs . ty``, the ``tv_bndrs`` are in scope
      in the ``ty``.

   #. In a type ``exists tv_bndrs . ty``, the ``ty`` must have kind
      ``TYPE rep`` for some ``rep``. The type ``exists tv_bndrs. ty`` itself
      has the same kind. (This is just like how ``forall`` is kinded.)

   #. Existential quantification is not allowed in the top-level "spine" of
      a GADT data constructor type, nor in a pattern synonym type. Arguments
      of existential type are fine. Example: a data constructor ``Mk :: exists k. T k``
      is disallowed, while ``Mk :: (exists k. Maybe k) -> T`` is fine.

#. When looking up a lower-case identifier in type-syntax, if the name is not
   in scope, look in the term-level namespace before failing. Any term-level
   names are rejected during type-checking. This is much like the treatment
   in `#270`_. (See ``Witness`` below for the motivation for this.)

   Other than perhaps a change in error messages, this change does not affect
   the set of programs that GHC accepts or the meanings of any program. In
   particular, this rule does *not* change GHC's implicit binding of unbound
   type-level lower-case names. To trigger the new behavior, the lower-case
   name would be used in a context where there is no implicit binding, such as
   in a signature with an explicit ``forall`` (according to the forall-or-nothing
   rule) or with ``-XNoImplicitForAll`` from `#285`_.

#. Introduce a new module in ``base`` called ``GHC.Exists``.

#. ``GHC.Exists`` exports a type-level name ``Witness`` that extracts
   out the packed type witness from an existentially-typed expression.
   (I expect ``Witness`` to be used very rarely, and thus the design here
   is optimized for simplicity, backward-compatibility, and forward-compatibility
   more than usability.)

   1. ``Witness`` is not injective and not generative.

   #. The argument to ``Witness`` is an *expression*, not a type. However, in order
      to simplify the implementation, the argument is parsed and renamed as
      as a type. After renaming, it must be interpretable as an expression.
      We can imagine an inverse of the T2T_ translation of `#281`_ that would apply
      here. (If requested, this could be written out in detail in this proposal.)
      Because the argument to ``Witness`` is parsed as a type, it cannot use constructs
      like ``case``; a user would have to name an expression in, say, a ``let``-binding
      and then could use the variable instead.

      See `Optional Extension`_ below to see how to extract the witness from a type.

   #. Here is the typing rule for ``Witness``, where the ``k`` argument is optional::

        ty = exists (a :: k). inner_ty
        e <= ty              -- the "<=" denotes checking mode, not synthesis mode
        ---------------------
        Witness @k ty e : k

   #. The following equality axiom holds for ``Witness``::

        ty = exists (a :: k). inner_ty
        e <= inner_ty[witness_ty/a]
        -------------------------
        Witness @k ty e ~ witness_ty

      We can implement this rule by using a fresh unification variable
      for ``witness_ty`` before checking ``e`` against ``inner_ty``.

   #. ``Witness`` would need to be added to Core, as well, likely as a new
      constructor of GHC's ``Type`` type.

#. ``GHC.Exists`` exports a type operator ``(/\) :: Constraint -> Type -> Type``;
   ``/\`` is injective and generative, like a datatype. It may appear partially
   applied.

   ``GHC.Exists`` exports a type operator ``(/\#) :: forall (rep :: RuntimeRep). Constraint -> TYPE rep -> TYPE (TupleRep [LiftedRep, rep])``.
   The representation for ``(/\#)`` differs from that of ``(/\)``, but is otherwise treated similarly.
   From here on, assume statements about ``(/\)`` apply also to ``(/\#)``.

#. **Type inference.** Type inference for these constructs is addressed at length in the paper_, including
   the extension in Section 9.2. Some
   intuition for the rules appears here in this proposal.

   1. When checking an expression ``e`` against a type ``exists (a :: k). ty``, we create a fresh unification
      variable ``α :: k`` and check ``e`` against ``ty[α/a]``. (This is reflected in rule ``Gen`` at the top
      of Fig. 4 of the paper_.)

   #. When checking an expression ``e`` against a type ``C /\ ty``, continue by checking ``e`` against ``ty``,
      emitting ``C`` as a wanted. That is, we must be able to satisfy the constraint ``C`` in order to accept
      ``e`` as type ``C /\ ty``. (This is reflected in rule ``GenQualified`` in Fig. 11 of the paper_.)

   #. When inferring the type of an expression ``e``, if that type is ``exists (a :: k). ty``, then rewrite
      that type to become ``ty[Witness @k (exists (a :: k). ty) e/a]``. (This is rule ``IExist`` in Fig. 5
      of the paper_.)

   #. When inferring the type of an expression ``e``, if that type is ``C /\ ty``, then rewrite that
      type to become ``ty`` and assume ``C`` as a given, available for use anywhere in the innermost
      case-match or lambda, including invisible lambdas as introduced in the argument to a function
      with a higher-rank type. (This is rule ``IGiven`` in Fig. 11 of the paper_.) Use of such a given
      may have a surprising influence on runtime behavior, see Ambiguity_, below.

   #. When inferring the type of a lambda-expression ``\ pats -> e``, we must ensure that no variable bound
      in ``pats`` leaks into the type of the expression; this is like a skolem-escape.

      (More technically) When inferring the type of a lambda-expression ``\ pats -> e``: We must bump the
      type-checker level before checking ``e``. This is because variables bound in ``pats`` might be mentioned
      in ``Witness``\ es in the type of ``e``. Then, when forming the type of the whole lambda-expression,
      we promote the inferred type of ``e``; if any variables bound in ``pats`` are encountered, issue an
      error.

   #. When trying to satisfy a class constraint ``[W] C (exists (a :: k). ty)``, instead solve
      ``[W] forall (a :: k). C ty``. For multi-parameter type classes, apply this treatment one
      argument at a time. Note that this treatment applies to ``~``, which is solved like any
      other type class (recall that lifted ``~`` is distinct from unlifted ``~#``, to which this
      treatment does not apply).

   #. When trying to satisfy a class constraint ``[W] C (C' /\ ty)``, instead solve
      ``[W] C' => C ty``. That is, assume ``C'`` as a given and then solve ``[W] C ty``.
      For multi-parameter type classes, apply this treatment one argument at a time.

#. ``GHC.Exists`` exports ::

     discardEvidence :: (() => c /\ a) -> a
     discardEvidence x = x

   This function effectively prevents the evidence for ``c`` from being used. Note that
   it has a (trivially) higher-rank type, so that GHC introduces an invisible lambda around
   its argument. Without this invisible lambda, the given would "escape" and be usable.

   The ``discardEvidence`` function itself is completely ordinary and could be defined by
   users, but the small unexpected twist in its type (and its general usefulness) suggests
   it should be ``GHC.Exists``.

#. **Core language.** There are several modifications to the Core language necessary to
   support this proposal. The notes here echo the design in the paper_, Section 5.

   1. The ``exists`` type would need to be added to Core as a new constructor of ``Type``::

        | ExistsTy TyVar Type
          -- typing rule:
          -- Γ ⊢ ki : Type
          -- Γ, tv:ki ⊢ inner_ty : TYPE rep
          -- tv # rep
          -- ------------------------------------
          -- Γ ⊢ ExistsTy (tv:ki) inner_ty : TYPE rep

   #. The ``Witness`` type would need to be added to Core as a new constructor of ``Type``::

        | WitnessTy CoreExpr
          -- typing rule:
          -- Γ ⊢ e : exists (a :: ki). inner_ty
          -- --------------------
          -- Γ ⊢ WitnessTy e : ki

      Note that this embeds expressions in types.

   #. While packing and opening existentials is implicit in Haskell, it is explicit in Core,
      using these two new constructors of ``Expr b``::

        | Pack Type (Expr b) TyVar Type
          -- typing rule:
          -- Γ ⊢ witness_ty : ki
          -- Γ ⊢ exists (bound_tv :: ki). inner_ty : TYPE rep
          -- Γ ⊢ expr : inner_ty[witness_ty/bound_tv]
          -- -----------------------------------------------------------------------
          -- Γ ⊢ Pack witness_ty expr (bound_tv:ki) inner_ty : exists bound_tv. inner_ty

        | Open (Expr b)
          -- typing rule:
          -- Γ ⊢ expr : exists (a :: k). inner_ty
          -- -------------------------------------------------------------------------
          -- Γ ⊢ Open expr : inner_ty[Witness @k (exists (a :: k). inner_ty) expr / a]

      These typing rule are ``CE-Pack`` and ``CE-Open`` from Fig. 7 of the paper_.

   #. Constructs in Core that bind terms need to update their typing rules to check for
      skolem escape. For example, here is the updated rule for lambda::

        Γ ⊢ ty1 : TYPE rep
        rep is a monomorphic representation
        Γ, (var:ty1) ⊢ expr : ty2
        var # ty2            -- this is the new check
        ------------------------------
        Γ ⊢ Lam (var:ty1) expr : ty1 -> ty2

      Similar changes will be necessary for case alternatives and for ``let``. See ``CE-Abs``
      in Fig. 7 of the paper_.

   #. We need to add a new coercion form to sllow for an interpretation for ``Witness``.
      This would be the new constructor for ``Coercion``::

        | WitnessPackCo Type CoreExpr TyVar Type
          -- typing rule:
          -- Γ ⊢ Pack witness_ty expr (bound_tv:ki) inner_ty : exists (bound_tv :: ki). inner_ty
          -- ------------------------------------------------------------------------------------------------------------------------
          -- Γ ⊢ WitnessPackCo witness_ty expr (bound_tv:ki) inner_ty : Witness witness_ty expr (bound_tv:ki) inner_ty ~# witness_ty

      See ``CG-ProjPack`` from Fig. 7 of the paper_.

   #. Several new coercion forms are necessary in order to support ``liftCoSubst``.
      These are all added to ``Coercion``::

        | ExistsCo TyVar Coercion    -- lifts ExistsTy
        | WitnessCo ExprCoercion     -- lifts WitnessTy

        data ExprCoercion                -- witnesses the equality between two expressions
          = GReflEC CoreExpr CoercionR
            -- typing rule:
            -- Γ ⊢ expr |> co : ty
            -- ------------------------------------------
            -- Γ ⊢ GReflEC expr co : expr ~ (expr |> co)

          | TransformEC CoreExpr CoreExpr String
            -- typing rule:
            -- Γ ⊢ e1 : ty
            -- Γ ⊢ e2 : ty
            -- --------------------------------------
            -- Γ ⊢ TransformEC e1 e2 reason : e1 ~ e2

      The ``TransformEC`` form allows us to create coercions witnessing the equality of
      any two expressions. The idea is that we use this when we need to prove that an
      optimization GHC performs is sound. We put the name of the optimization in the
      carried string. Perhaps in the future, this will become more principled.

   #. The ``InstCo`` and ``NthCo`` coercion forms now work on ``ExistsTy`` analogously
      to how they work on ``ForAllTy``.

   #. Suppose ``f args :: C /\ ty`` and the constraint ``C`` is used. GHC will then
      generate bindings that look like ::

        let result :: C /\ ty
            result = f args

            dictC :: C
            dictC = fstC result   -- fstC :: forall c ty. c /\ ty -> c

      in its evidence bindings. Note the separate binding for ``result``. This will
      mean that multiple uses of ``f args`` in the body of a function will get commoned
      up during optimization. This is important in order to avoid unexpected repeated
      evaluation of ``f args`` due to the use of ``C``.

   .. _Ambiguity:

#. **Ambiguity.** When a function call ``f args`` returns a dictionary (with ``/\``),
   any use of that dictionary will require evaluating ``f args``. If multiple such
   expressions return dictionaries of the same type (and the dictionary gets used),
   it is now unclear which expression to evaluate. Here is a contrived example::

     manufacture :: forall a. Int -> Show a /\ ()
     manufacture = ...

     ambig :: forall a. a -> String
     ambig x = let () = manufacture @a 1
                   () = manufacture @a 2
               in show x

   The use of ``show x`` requires a ``Show a`` dictionary, but there are two possible
   such dictionaries: one in the result of ``manufacture @a 1`` and the other in the
   result of ``manufacture @a 2``. Even if these dictionaries are computationally
   identical (that is, coherent), the two calls to ``manufacture`` might have different
   side effects (such as running time or exceptions thrown). Interestingly, neither call to
   ``manufacture`` would be evaluated without the use of the dictionary, because ``let``
   is lazy.

   Accordingly, if there are multiple givens arising from ``/\`` that could solve a wanted,
   we choose none of them. If there are no other ways of solving the wanted, then the
   user's program is rejected. Alternatively, we could work this new flavor of given
   into the scheme outlined in `#17934`_.

#. **Compilation.** The new ``Pack`` and ``Open`` constructs can be erased entirely
   during code generation. This is why the representation of ``exists (a :: k). ty``
   is the same as the representation of ``ty``. This is exactly the same as the treatment
   of ``forall`` and type application.

Optional Extension
------------------

Extend the treatment of existentials to work in types. (Do *not* extend the treatment
of constraints with ``/\``.) This would affect the kind
inference algorithm analogously to the effects on the type inference algorithm above.
The only complication is that we would need ``TWitness`` instead of ``Witness``.
A ``TWitness`` accepts a type, not an expression. (In the glorious future of Dependent
Haskell, ``TWitness`` becomes a synonym for ``Witness``.)

Supporting this feature would require yet more extensions to Core:
* ``PackTy`` and ``OpenTy`` in ``Type``, analogous to ``Pack`` and ``Open`` in ``Expr``.
* ``TWitness`` in ``Type``, analogous to ``Witness``.
* ``TWitnessPackCo`` and ``TWitnessCo`` in ``Coercion``, analogous to ``WitnessPackCo`` and ``WitnessCo``.

I would prefer not to have this extension, but save type-level support for packing
and opening existentials until we have proper dependent types.

Examples
--------

1. See the filter_ example, above.

#. ::

     toVec :: [a] -> exists n. KnownNat n /\ Vec n a
     toVec []     = VNil
     toVec (x:xs) = x :> toVec xs

     -- NB: requires inputs to have the same length
     zipWithV :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
     zipWithV = ...

     -- from GHC.TypeLits:
     sameNat :: forall a b proxy1 proxy2. (KnownNat a, KnownNat b) => proxy1 a -> proxy2 b -> Maybe (a :~: b)

     -- returns first argument iff its length matches that of the second, in O(1) time
     sameLengthAs :: forall m n a b. (KnownNat m, KnownNat n) => Vec m a -> Vec n b -> Maybe (Vec n a)
     sameLengthAs v1 _ = do Refl <- sameNat @m @n Proxy Proxy
                            return v1

     frob :: (a -> b -> c) -> [a] -> [b] -> exists n. Maybe (Vec n c)
     frob f as bs = do let as' = toVec as
                           bs' = toVec bs
                       as'' <- sameLengthAs as' bs'
                       return (zipWithV f as'' bs')

#. Imagine this within GHC::

     -- in GHC.Utils.Outputable
     sep :: [SDoc] -> SDoc
     ppr :: Outputable a => a -> SDoc

     pprs :: [exists a. Outputable a /\ a] -> SDoc
     pprs = sep . map ppr    -- this type-checks because of the rules for class constraints on existentials

     someTypecheckingFunction a b c d e f = do ...
                                               traceTc "herald" (pprs [a, b, c, d, e, f])
                                               -- today, we'd need [ppr a, ppr b, ppr c, ppr d, ppr e, ppr f],
                                               -- which is cluttersome.
                                               ...

Effect and Interactions
-----------------------

1. Programming with existentials is now straightforward and uncluttered. This
   would, in turn, make it easier to write programs with more compile-time
   verification of invariants.

#. Existential quantification can be erased. Currently it requires boxing, and so
   using an existential may have an impact on the running time of a program.
   It is a goal of mine that extra compile-time verification should never have
   a cost at run-time, and this gets us one step closer to that goal.

#. Laziness can be preserved. Currently, operating on existentially quantified values
   requires forcing them. This is important in applications like ``filter``.

#. With these existentials in place, it is a small step to allow existentials in newtypes,
   finally addressing long-time feature request `#1965 <https://gitlab.haskell.org/ghc/ghc/-/issues/1965>`_.

#. With either this feature or existential newtypes, a few library types that are currently
   datatypes could be converted to newtypes. (Example: convert
   ``data SomeTypeRep where SomeTypeRep :: forall k (a :: k). TypeRep a -> SomeTypeRep`` to
   become ``newtype SomeTypeRep = SomeTypeRep (exists k (a :: k). TypeRep a)``.) This would
   give a performance improvement to users who do not use this feature directly.

#. This proposal does not currently interact with datatype-based existentials (as they have
   existed since the inclusion of GADTs in GHC). It might be desirable for ``Witness`` to work
   on legacy existentials. However, this is left as future work.

Costs and Drawbacks
-------------------

1. This is a significant change, including somewhat invasive changes to Core,
   with expression variables appearing in types. The paper_ proves that these
   changes are type-safe, but changing Core is always a reason for pause.

#. No language has a feature like this yet. It is possible that it will not
   be so easy to use in practice. It may take an iteration or two before we
   settle on just the right presentation to users.

#. Because expressions now appear in types, any transformation GHC makes on
   terms (i.e. optimizations) might now change types. (Specifically, if the
   expression in an ``Open`` expression is optimized, its type changes.) GHC
   would then have to relate the old expression with the new one via a
   ``TransformEC`` expression-coercion -- which is why that constructor is
   so general. (It is used essentially to assert the correctness of the
   optimization.)

   One particular wrinkle is that the optimization may eliminate the usage
   of some free variable, and then the expression might be subject to floating.
   But the now-eliminated variable would still appear in the ``TransformEC``
   coercion, blocking the floating.

   One solution to this problem would be to take a pass that let-binds all
   ``Open`` expressions. Then the right-hand side of the let-binding could
   be optimized without changing the type of the ``Open``; indeed, this idea
   of let-binding might be a nice simplification regardless, because (I think)
   it might mean that all ``Open`` expressions could contain only variables,
   not arbitrary expressions.

Alternatives
------------

1. The current proposal allows the argument to ``Witness`` to be an arbitrary
   expression. We could imagine a restriction requiring this to be a variable,
   not an expression. This would mean that users would have to manually bind
   the results of function calls that return existentials.

   I do not favor this restriction, but it is plausible.

#. We could imagine just adding ``exists`` without ``/\`` (using e.g. ``Dict``
   to accomplish the goal of ``/\``). However, I think these features go nicely
   together: would we want ``forall`` without ``=>``?

#. Is ``/\`` a bad name? It has a name-clash with the widely used ``lattices``
   package (though that package uses the name in the term-level namespace), and
   it looks symmetrical, though it is not. An alternative might be ::

     SuchThat :: Type -> Constraint -> Type

   expected to be used infix. This would mean we have e.g.
   ``pprs :: [exists a. a `SuchThat` Outputable a] -> SDoc``. I don't love that
   the ordering between the constraint and the type is opposite to that of ``=>``,
   but maybe that's OK.

Unresolved Questions
--------------------

1. The proposed ``exists`` erases the existential witness. What if we want to keep
   it around, as suggested on `GitHub <https://github.com/ghc-proposals/ghc-proposals/pull/473#issuecomment-991433531>`_? I propose not solving this problem now, but instead
   returning to it when we figure out exactly how to make ``forall`` relevant. Is it
   worthwhile reserving a keyword though? An alternative would be to have some marker
   on the bound variable to indicate its relevance (this could work for ``forall`` to,
   meaning we wouldn't have to introduce the ``foreach`` keyword).

#. Should this happen before or after we have dependent types? Maybe the effort put
   into designing/implementing this would be better spend doing dependent types.
   I think this is largely orthogonal to dependent types; it's not clear which one
   would be easier to implement first. I think this one is easier, and could plausibly
   let us gain experience with expressions in types without the major changes inherent
   in supporting dependent types in full, but it's not obvious.

Implementation Plan
-------------------

I will implement.

Endorsements
-------------

Add yourself here, please!