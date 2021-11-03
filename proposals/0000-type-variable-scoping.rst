Modern Scoped Type Variables
============================

.. sectnum::
.. author:: Richard Eisenberg
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/448>`_.
.. contents::

This proposal updates the treatment of scoped type variables in GHC, tying
together many existing proposals:

.. _`#99`: https://github.com/ghc-proposals/ghc-proposals/pull/99
.. _`#119`: https://github.com/ghc-proposals/ghc-proposals/pull/119
.. _`#126`: https://github.com/ghc-proposals/ghc-proposals/pull/126
.. _`#128`: https://github.com/ghc-proposals/ghc-proposals/pull/128
.. _`#155`: https://github.com/ghc-proposals/ghc-proposals/pull/155
.. _`#228`: https://github.com/ghc-proposals/ghc-proposals/pull/228
.. _`#238`: https://github.com/ghc-proposals/ghc-proposals/pull/238
.. _`#270`: https://github.com/ghc-proposals/ghc-proposals/pull/270
.. _`#281`: https://github.com/ghc-proposals/ghc-proposals/pull/281
.. _`#285`: https://github.com/ghc-proposals/ghc-proposals/pull/285
.. _`#291`: https://github.com/ghc-proposals/ghc-proposals/pull/291
.. _`#378`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst
.. _`#402`: https://github.com/ghc-proposals/ghc-proposals/pull/402
.. _`#420`: https://github.com/ghc-proposals/ghc-proposals/pull/420
.. _Type Variables in Patterns: https://richarde.dev/papers/2018/pat-tyvars/pat-tyvars.pdf
.. _Kind Inference for Datatypes: https://richarde.dev/papers/2020/kind-inference/kind-inference.pdf
.. _`Haskell 2010 Report`: https://www.haskell.org/onlinereport/haskell2010/haskellch10.html

* `#126`_: Accepted, implemented proposal on accepting type arguments to constructor
  patterns, allowing constructions like ``f (Just @Int x) = x + 5``
  and ``g (Dynamic @a x) = show (typeOf (x :: a))``.
* `#128`_: Accepted, implemented proposal that allows type variables in certain contexts
  to refer to *types*, not just other type variables. Example: ``f (True :: b) = (False :: b)``;
  here, ``b`` becomes a synonym for ``Bool``. (Previously, a type variable was required to remain
  abstract.)
* `#155`_: Accepted, not implemented proposal on accepting type arguments to
  lambdas, allowing constructions like ``\ @a (x :: a) -> x``.
* `#238`_: Not yet accepted proposal that updates and extends `#155`_ to
  include a reshuffling of extensions around scoped type variables, as well
  as describing clearer rules mediating between the old ``-XScopedTypeVariables``
  bahavior and the new proposed behavior.
* `#285`_: Accepted, not implemented proposal introducing ``-XNoImplicitForAll``
  and ``-XNoPatternSignatureBinds``, restricting where type variables may implicitly
  be brought into scope.
* `#291`_: Not yet accepted proposal that is an amendment to `#126`_ to change
  the way type variables are brought into scope to be more like term variables,
  and less like pattern signatures.
* `#420`_: Not yet accepted proposal that is an amendment to `#285`_, with a
  very tiny, technical tweak to a definition.

This proposal supersedes all of the proposals above, including the accepted
ones. The goal of writing this is to provide a unified framework, so that
we can design our language with cohesive treatment of scoped type variables,
instead of simply an agglomeration of features.

Motivation
----------

With GHC's powerful type-level programming features, we need powerful abilities
to bring type variables into scope. The proposal defers to the individual proposals
linked above for motivation for why we generally want these type-level features.
Individual aspects of this unifying proposal are motivated near where they are
introduced.

Principles
----------

The design here is guided by several high-level principles. These principles are not
sacro-sanct, such that they admit no exceptions. Instead, we strive to uphold these
principles, but relax them when doing so is well motivated.

.. _LSP:

1. **Lexical Scoping Principle (LSP)**. For every occurrence of an identifier, it is possible to uniquely identify its binding site, without involving the type system. (From `#378`_.)

   The LSP implies

   .. _LSPC:

   **Lexical Scoping Principle Corollary (LSPC)**. For every appearance of an identifier
   in a Haskell program, it is possible to determine whether that appearance is a
   binding site or an occurrence, without involving the type system.

   Motivation: These principles mean that we can understand the binding
   structure of a program without relying on type inference, important both for the
   implementation of GHC and the sanity of programmers.

   .. _LLSP:

#. **Local Lexical Scoping Principle (LLSP)**. For every occurrence of an identifier, it is possible to determine
   whether that appearance is a binding site or an occurrence, without looking to see what identifiers are
   already in scope.

   This is a variant of the LSPC_ that also prevents us from looking at the set of in-scope identifiers for determining
   whether something is a binding site.

   Motivation: Tracking the set of in-scope variables is laborious for human readers. (The compiler is already
   doing this during name resolution.) This fact becomes even more poignant if we consider the possibility
   of mixing the term-level and type-level namespaces (`#270`_) and need to think about clashes between type
   variables and imported term variables.

   .. _SUP:

#. **Syntactic Unification Principle (SUP)**. In the absence of punning, there is no difference between type-syntax and term-syntax.
   (From `#378`_.)

   Motivation: The SUP keeps us forward-compatible with a possible future where the
   distinction between term-syntax and type-syntax is removed.

   .. _EVP:

#. **Explicit Variable Principle (EVP)**. It is possible to write out all (specified)
   type arguments in every polymorphic function application,
   give the type for every bound variable,
   and write a type signature for every expression. This requires the ability to
   bring type variables into scope. These variables can be brought into scope
   without relying on matching or unification.

   Examples::

     const :: a -> b -> a
     const x y = ...    -- there must be some way to name the types of x and y here
     -- using `const (x :: a) (y :: b) = ...` is not powerful enough, because it relies
     -- on matching the pattern signature with the argument type from the type signature

     data Ex = forall a. Ex a
     f (Ex x) = ...     -- there must be some way to name the type of x here

     hr :: (forall a. a -> a -> a) -> ...
     hr = ...
     g = hr (\ x y -> ...)   -- there must be some way to name the type of x or y here

   Once we have the EVP, there will never be a need for ``Proxy``.

   Motivation: As GHC supports more and more type-level programming, the ability
   to write out type signatures, arguments, and annotations has become increasingly
   important. With ``-XScopedTypeVariables``, GHC allows us to bring type variables
   into scope, but often requires us to do so by cumbersome matching. If we have
   a type ``Maybe (Either (Int, Bool, a) Double)``, that's a lot to type just to
   be able to, say, bind ``a``. The EVP says we do *not* have to resort to matching,
   ever.

   .. _EBP:

#. **Explicit Binding Principle (EBP)**. Through the right combination of extensions and/or warning flags, it is possible
   for a Haskell programmer to ensure that all identifiers in a program have an explicit binding site.

   Examples::

     id :: a -> a    -- the variable `a` has no explicit binding site, but we can write `forall a.` to provide one

     f :: (Bool, Bool) -> Bool
     not (x :: (b, b)) = ...   -- the variable `b` is bound to `Bool` by this
                               -- pattern signature. But either this is done by
                               -- matching (in violation of the EVP) or the binding
                               -- site is implicit (in violation of the EBP).

   Motivation: The EBP allows programmers to control exactly how variables come into
   scope. It also prevents the possibility of typos that accidentally introduce new
   variables.

   .. _VOP:

#. **Visibility Orthogonality Principle (VOP)**. Whether an argument is visible or
   invisible should affect only its visibility, not other properties.

   A consequence of the VOP is that these two programs should have the same meaning::

     f1 :: forall a -> ...
     f1 blah1 = ...

     g1 = ... f1 blah2 ...

     -------

     f2 :: forall a. ...
     f2 @(blah1) = ...

     g2 = ... f2 @(blah2) ...

   The only difference between these is the visibility.

   Motivation: Visibility should be just that: a superficial property that describes
   (only) whether an argument is visible in the user-written source code.

   .. _PEDP:

#. **Pattern/Expression Duality Principle (PEDP)**. The syntax for patterns mimics
   that of expressions, allowing an expression headed by a constructor to be pattern-matched
   against a pattern of the same syntactic structure.

   Motivation: This is the essence of pattern-matching, where we can deconstruct data
   that was constructed by an expression.

.. _universals-and-existentials:

Universals and existentials
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The combination of the VOP_ and the LSPC_ lead to a surprising conclusion: we must distinguish
between universal variables and existential variables of GADT constructors. This section explains
why this is the case, despite the fact that this goes against conclusions in the `Type Variables
in Patterns`_ paper.

To make these ideas concrete, let's consider this running example::

  type UnivEx :: Type -> Type
  data UnivEx a where
    MkUE :: a -> b -> UnivEx a

**Inputs vs outputs**. The first key observation in this analysis is that, in a pattern, universal type variables are
*inputs*, while existential type variables are *outputs*. That is, when see a pattern such as
the one in ``f (MkUE x y) = ...``, we must already know the instantiation for ``a`` (taken from
the declared type of ``MkUE``), but the pattern itself tells us the value for ``b``. (In this way,
universals are like the *required* constraints of a pattern synonym, while existentials are like
the *provided* constraints.) If we have ``f :: UnivEx Int -> ...``, then it might be sensible, for example,
to write ``f (MkUE @Int x y) = ...``, (redundantly) saying how to instantiate the ``a`` from
``MkUE``\ 's type. On the other hand, it would never work to say ``f (MkUE @Int @Bool x y) = ...``,
where the ``@Bool`` is meant to "instantiate" the ``b``. The pattern match *informs* the choice for
``b``: it cannot presuppose it (without doing run-time type matching, which we don't).

Because universals are inputs, it makes sense to mention in-scope variables in universal-variable
instantiations. For example::

  f :: forall c. UnivEx c -> ...
  f (MkUE @c x y) = ...

Here, we have used the in-scope (assuming today's ``-XScopedTypeVariables``) type variable ``c``
to say how to instantiate ``MkUE``. On the other hand, we would never want to mention in-scope
variables in an existential binding::

  f :: forall c d. UnivEx c -> d -> ...
  f (MkUE @c @d x y) = ...

This is quite strange: we're somehow suggesting that the existential type packed in ``MkUE`` is
precisely ``d``. That's impossible. Instead, we might imagine that this binds a new type variable
``d`` that *shadows* the existing ``d``. In any case, something is definitely strange here.

In the context of the LSPC_, though, we see that the treatment for universal type arguments
and existential type arguments must be identical -- at least as far as whether variable mentions
are bindings or occurrences. The reason is that the LSPC_ forbids us from considering ``MkUE``\ 's
type when determining whether the mentions of ``c`` and ``d`` here are bindings or occurrences --
and thus the choice must be the same for both.

**Current solution**. One way out of this is taken in accepted proposal `#126`_, where the choice between binding site
and occurrence is made depending on whether a type variable is already in scope. For the last
``f`` example, `#126`_ rejects because we cannot know that the existential type variable will
be ``d``. (That is, there is no shadowing.) Using in-scopedness to choose between binding site
and occurrence is not in violation of the LSPC_, but it is in violation of the otherwise-respected LLSP_.

**Treatment of term variables**. However, consider how this design compares with the treatment of ordinary expression patterns.
For example::

  g x (Just x) = ...
  h x = case frob x of
    Just x -> ...

No type variables here. Instead, we have an illegal ``g`` -- rejected because one sequence
of patterns binds the same term variable twice. And we have an accepted ``h``, but this
``h`` has a shadowed binding for ``x`` -- no equality comparison here. In terms, any variable
occurrence in a pattern (except in the expression part of a view pattern) is a binding site,
with no questions asked.

**Visible dependent quantification**. Now, let's consider what happens once we have visible
dependent quantification (VDQ) in the types of GADT constructors. This is proposed in `#281`_, which
amends `#402`_ to allow VDQ in GADT constructors (among supporting VDQ more generally). ::

  data VDQ a where
    MkVDQ :: forall a b -> a -> b -> VDQ a

The type ``VDQ`` is like ``UnivEx``, except that it uses VDQ in its constructor. A pattern might
look like ``f (MkVDQ a b x y) = ...``. According to the analysis above, we might want variable
*occurrences* in the first argument of the pattern ``MkVDQ`` (because ``a`` is universal, and
hence an *input* to the pattern), while we definitely want binding sites for ``x`` and ``y``, the
ordinary term arguments. Yet, having different binding treatment for ``a`` than for ``x`` and ``y``
is in violation of the LSPC_, which forbids us from looking at the type of ``MkVDQ`` in making this
decision.

In order to keep backward compatibility with the current treatment for term-level patterns, we must
treat ``a`` and ``b`` as *binding sites* not occurrences.

**Visible vs. invisible type arguments**. The VOP_ tells us that the presence or absence of the
``@`` sign should not affect the binding-site treatment of a region of code. Accordingly, we now
realize that all type arguments -- whether visible or invisible -- must treat variable mentions
as binding sites, not occurrences. Here is an example::

  data VOP a b where
    MkVOP :: forall a -> forall b. a -> b -> VOP a b

Here, we have one visible type argument and one invisible one. A pattern might look like
``f (MkVOP a @b x y) = ...``. The VOP_ compels us to treat ``a`` and ``b`` identically.
Furthermore, the LSPC_ compels us to treat ``a`` and ``x``\ /\ ``y`` identically, too.
Accordingly, all of these must be binding sites -- none can be occurrences.

**Term-level inputs to patterns**. Pattern synonyms currently allow us to declare two
flavors of input: universal type variables and required constraints. We also can declare
three forms of output: existential type variables, provided constraints, and normal term-level arguments.
We're clearly missing something here: term-level input arguments. This proposal does *not*
address this deficiency, but it seems desirable to leave the door open to such an extension in
the future. And when we do, the treatment for term-level inputs should be -- in accordance with the
SUP_ -- the same as the treatment for universal type variables.

**But we want occurrences**. Earlier, we discovered that we sometimes want occurrences
of type variables when instantiating a universal in a pattern. And yet, we see by the
argument here that we cannot support this desire -- at least without new syntax to separate
out inputs from outputs (we could use a modifier, for example).

**Bottom line**. We must not allow universal-variable instantiations in patterns. (This is in
direct conflict with `#126`_ and the `Type Variables in Patterns`_ paper.) There is no way
to safely distinguish such parts of a pattern from the parts of the pattern that bind new variables,
and so until we invent new syntax to allow universal-variable instantiation in patterns, we
must stop accepting these instantiations.

**Corollary**. We must be able to tell the difference between universal variables and existential
variables in GADT constructors. As `Type Variables in Patterns`_ observes, this is not always so
easy in the presence of equality constraints. Accordingly, this proposal introduces a `new way
of understanding GADT syntax <#gadt-syntax>`_ that clearly does define which variables are
universal and which are existential.

Proposed Changes
----------------

Extension shuffling
~~~~~~~~~~~~~~~~~~~

1. Re-purpose deprecated extension ``-XPatternSignatures``. With ``-XPatternSignatures``, we
   allow type signatures in patterns. These signatures can mention in-scope
   type variables as variable occurrences, but can not bind type variables.

   The current ``-XPatternSignatures`` is just a synonym for ``-XScopedTypeVariables``.
   This change is thus not backward-compatible, but given that the existing extension
   is deprecated, I think this change is acceptable.

   Motivation: See rejected proposal `#119`_, which contains the motivation here.

#. Introduce ``-XPatternSignatureBinds``. With ``-XPatternSignatureBinds``, any
   out-of-scope type variables written in a pattern signature would be bound there
   and would remain in scope over the
   same region of code that term-level variables introduced in a pattern scope
   over. In addition, any out-of-scope type variable introduced in a type signature of a
   ``forall``\ -bound variable of a ``RULES`` pragma is similarly scoped over the
   rule. This extension is on by default.
   (This extension is a part of accepted, unimplemented proposal
   `#285`_.)

   Motivation for "on by default": The effect on pattern signatures requires
   ``-XPatternSignatures`` to be witnessed, and so having this be on by default
   does not change the meaning of Haskell98. However, ``RULES`` allows scoped
   type variables even without any extensions today, and so ``-XPatternSignatureBinds``
   is on by default to accommodate this use-case.

#. Introduce ``-XMethodTypeVariables``. With ``-XMethodTypeVariables``, type
   variables introduced in an instance head would scope over the bodies of
   method implementations. Additionally, type variables introduced in a class
   head would scope over the bodies of method defaults.

#. Introduce ``-XScopedForAlls``. With ``-XScopedForAlls``, any type variables
   mentioned in an explicit ``forall`` scopes over an expression. This applies
   to the following constructs:

   * Function bindings
   * Pattern synonym bindings (including in any ``where`` clause)
   * Expression type signatures

   (Alternative name: ``-XExtendedForAllScope``. See `Vlad's comment <https://github.com/ghc-proposals/ghc-proposals/pull/448#discussion_r738276607>`_.).

#. The extension ``-XScopedTypeVariables`` would imply all of the above
   extensions; this way, ``-XScopedTypeVariables`` does not change from its
   current meaning.

#. Introduce ``-XImplicitForAll``, on by default. With ``-XImplicitForAll``,
   an out-of-scope type variable mentioned in various constructs (listed below)
   is implicitly brought into scope over the construct. With ``-XNoImplicitForAll``,
   this implicit scoping does not happen, and the use of the variable is an error.

   Constructs affected:

   1. Type signatures for variable declarations, methods, and foreign imports & exports.
      Example: ``let f :: a -> a; f = ... in ...`` becomes
      ``let f :: forall a. a -> a; f = ... in ...``.

   #. Kind signatures. Example: ``type T :: k -> Type`` becomes ``type T :: forall k. k -> Type``.

   #. GADT constructor declarations. Example: ``MkG :: a -> Maybe b -> G (Either Int b)``
      becomes ``MkG :: forall a b. a -> Maybe b -> G (Either Int b)``.

   #. Pattern synonym signatures. Example: ``pattern P :: a -> Maybe a`` becomes
      ``pattern P :: forall a. a -> Maybe a``. Implicit quantification in pattern synonyms
      always produces *universal* variables, never existential ones.

   #. Type annotations in expressions and ``SPECIALISE`` pragmas. Example:
      ``Right True :: Either a Bool`` becomes ``Right True :: forall a. Either a Bool``.

   #. Types in a ``deriving`` clause. Example: ``data T deriving (C a)`` becomes
      ``data T deriving (forall a. C a)``.

   #. Instance heads, including standalone-deriving instances.
      Example: ``instance Show a => Show (Maybe a)`` becomes
      ``instance forall a. Show a => Show (Maybe a)``.

   #. Type and data family instances, as well as closed type family equations.
      Example: ``type instance F (Maybe a) = Int``
      becomes ``type instance forall a. F (Maybe a) = Int``.

   This extension is part of accepted, unimplemented proposal `#285`_; there
   is no intended change in the description here.

   Being able to turn off this extension is necessary to uphold the EBP_.

.. _gadt-syntax:

GADT syntax to distinguish universals and existentials
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This component of this proposal revises the scoping of variables in GADT declaration syntax.

Motivation
^^^^^^^^^^

1. This new version makes the distinction between universals and existentials in constructors
   very clear, as required by the analysis above (`universals-and-existentials`_).

#. Principled kind inference is, I believe, impossible using the current syntax. This is argued
   in `Appendix B.8 <https://richarde.dev/papers/2020/kind-inference/kind-inference-supplement.pdf#subsection.B.8>`_ of the `Kind Inference for Datatypes`_ paper. The current approach allows some examples of polymorphic
   recursion, but not others, and I doubt there is a declarative specification
   of what we accept today. Here is an example of surprisingly allowed polymorphic
   recursion::

     data Poly a where
       MkPoly :: forall k1 k2 (a :: k1) (b :: k2). Poly a -> Poly b -> Poly a

   Note that the use of ``Poly b`` instantiates ``Poly`` at a different
   kind (``k2``) than the instantiation in the result (``k1``).

   If we change the ``Poly b`` above to ``Poly Maybe``, the definition is
   rejected: only when the polymorphic recursion instantiates a kind variable
   *with a variable* is the definition accepted.

   Another oddity here happens when we ask what the inferred kind of ``Poly``
   is, before generalization. It must be ``k1 -> Type``... but ``k1`` is not
   even in scope in the declaration of ``Poly``. It's all very strange.

Proposed Change Specification
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

1. Allow all variables -- call them ``a1 .. an`` -- in the header of a GADT declaration (including variables introduced implicitly) to scope over
   all constructor declarations. Call the GADT ``G``.

#. If there is no standalone kind signature for the GADT, each ``ai`` is
   assigned a kind meta-variable. When checking the constructors, this kind
   meta-variable is unified following the usual rules for meta-variables.
   In particular, this meta-variable will not be able to unify with any kind
   variable locally quantified in a constructor declaration, because the scope
   of the locally quantified kind variable is smaller than the kind meta-variable.

   If a variable kind signature (e.g. ``data G (a :: Type -> Type) where ...``)
   is given in the GADT header, the kind given for the kind
   variable is unified with the kind meta-variable, as usual.

   In declarative terms, this means that we can simply "guess" a monokind for
   each type variable ``ai``.

#. For each constructor, we must determine the universal variables and the
   existential variables. To do this, look at the result type ``G ty1 .. tyn``.
   For each ``i`` such that ``tyi`` is exactly ``ai``, ``ai`` is labeled as
   a universal variable for that constructor. All other type variables in
   that constructor's type are existentials. The distinction between universals
   and existentials matters only in patterns.

#. If a constructor explicitly quantifies over an in-scope type variable
   (example: ``data G a where MkG :: Int -> forall a. a -> G a``), that
   quantification does *not* introduce a new variable. Instead, during
   kind inference, any kind
   signature in the ``forall`` is unified with the kind of the variable,
   but the quantification is otherwise ignored (during kind inference).

#. After kind inference is complete, we must assign types to each
   constructor. The type of the constructor is unchanged from today:
   the order of quantified variables is as given by the user (so, for example,
   existentials might precede universals).

Examples
^^^^^^^^

::

  data G1 a where
    MkG1 :: a Int -> G1 a

Inference for ``G1`` is now easier. We assign ``a :: kappa`` and then unify
``Type -> Type`` with ``kappa``. Today's algorithm instead unifies the kind
of ``G1`` with ``(Type -> Type) -> Type``, from the result type.

::

  data G2 a where
    MkG2 :: forall k (a :: k). G2 a

This definition is now rejected, because the kind for ``a`` cannot mention
locally quantified ``k``. This could be accepted with a standalone kind signature
for ``G2``.

::

  data G3 a where
    MkG3 :: G3 a

This definition is accepted, with ``G3 :: forall k. k -> Type``.
At the end of kind inference, there is no restriction on the kind of ``a``,
so it is generalized.

::

  data G4 (a :: k) where
    MkG4 :: forall k (a :: k). G4 a

This definition is accepted. The ``k`` in the header becomes an implicit
type argument to ``G4``. The ``forall k`` is then ignored during kind
inference, and so the kind annotation on ``a`` in the constructor does
not cause trouble.

::

  data G5 k a where
    MkG5 :: forall k (a :: k). G5 k a

This is also accepted, because the ``k`` is introduced in the header.

::

  data G6 a b where
    MkG6 :: a -> b -> G6 b b

The constructor ``MkG6`` has a universal argument ``b`` and an existential
argument ``a``. Its type is ``forall a b. a -> b -> G6 b b``. Pattern-matching
a scrutinee of type ``G6 ty1 ty2`` against ``MkG6`` introduces an existential
variable ``a`` and assumes an equality constraint ``ty1 ~ ty2``.

::

  data G7 a where
    MkG7 :: b -> G7 b

The constructor ``MkG7`` has only an existential variable ``b``.
Pattern-matching a scrutinee of type ``G7 ty`` against ``MkG7``
introduces the existential ``b`` and an equality constraint that
``b ~ ty``. This equality, however, will not affect type inference,
because it is "let-like".
See ``Note [Let-bound skolems]`` in ``GHC.Tc.Solver.InertSet``.
The choice of making ``b`` existential *does* affect the shape of
the data constructor worker for ``MkG7``, but this will not affect
Haskell users.

::

  data G8 a where
    MkG8_1 :: a Int -> G8 Bool
    MkG8_2 :: a -> G8 a

This definition is accepted today but will be rejected under this proposal.
The problem is that the kind of ``a`` is *different* in the different constructor
types. Today, these ``a``\ s are considered independent, and so there is no
trouble. Under this proposal, though, these ``a``\ s are considered the same,
and thus cannot have different kinds. I argue that ``G8`` here is as confusing
to human readers as it would be to GHC under this proposal, and so rejection
seems sensible.

Effects
^^^^^^^

1. Kind inference becomes more principled, allowing information to flow from
   constructor types back to the declared type arguments.

#. Some definitions accepted today will be rejected under this new treatment,
   when accepting the definition requires unifying the kind of a type argument
   with a locally quantified kind variable. This rejection is a *desired* outcome
   of this change, as the current acceptance is in violation of our plan
   not to infer polymorphic recursion.

   Any newly rejected definition can be fixed with a standalone kind signature.
   This fix is backward compatible.

#. Other definitions accepted today are like ``G8`` in that they use the same
   name for multiple different variables in different constructor types. These
   definitions will have to rename some variables, which is a completely local
   change and will not affect downstream users.

#. A `separate part <#pattern-type-args>`_ of this proposal describes
   how the choice of universals and existentials affects pattern-matching.

#. Constructor uses in expressions are completely and utterly unchanged,
   because the assigned types of constructors are unchanged.

#. Currently, the kind inference algorithm requires two full passes over
   every datatype that lacks a standalone kind signature. I believe this
   change would mean we could reduce this to one pass, though the simplification
   would require some significant refactoring within GHC. (Without this proposal,
   I believe we are tied to keeping the second pass.) The `Kind Inference
   for Datatypes`_ paper shows how kind inference can be done in one pass
   (followed by a straightforward substitution).

#. This change allows some simplification in the kind-inference code.
   It would nullify ``Note [Using TyVarTvs for kind-checking GADTs]`` in
   ``GHC.Tc.TyCl``. It would also mean that the result kind of a GADT
   now makes sense when checking constructors, simplifying logic in ``kcConDecl``
   and making aspects of supporing unlifted newtypes easier (see the
   wrinkle around #17021 in ``Note [Implementation of UnliftedNewtypes]``
     in ``GHC.Tc.TyCl``.

#. This design violates the LLSP_, in that, if we see ``Mk :: forall a. a -> T a``,
   the first ``a`` is a binder if ``a`` is not already in scope, but is not a binder
   if ``a`` is already in scope. See the `alternative approach <#gadt-alternative>`_
   below.

Drawbacks
^^^^^^^^^

1. This change may annoy some users whose definitions are newly rejected.
   The fixes are easy and fully backward-compatible.

Alternatives
^^^^^^^^^^^^

.. _gadt-alternative:

1. Instead of having the variables in the header directly scope over
   the constructors, we could instead compute a variable renaming for
   each constructor. It would work like this, operating over each
   constructor ``K`` of a type ``T`` separately:

   * Let ``R`` represent a *renaming*, mapping variables in scope
     in the result type of ``K`` (domain) to variables mentioned in the declaration
     header (codomain). ``R`` starts empty.

   * Let ``tv1 .. tvn`` represent the variables mentioned in the
     declaration header for ``T``.

   * Look at the result type of ``K`` and extract out the
     type arguments to ``T``. Call these arguments ``arg1 .. argn``,
     where ``T`` has arity ``n``.

   * For each ``argi``: if ``argi`` is a bare variable ``v`` that is not
     already included in the domain of ``R``, add a mapping to ``R``
     from ``v`` to ``tvi``.

   * The key set of ``R`` is precisely the set of universal variables
     of ``K``; any other variable introduced in ``K``\ 's type is an
     existential.

   * During kind inference, treat an occurrence of a variable ``v``
     in the key set of ``R`` as if it were an occurrence of ``R(v)``.

   Note that the algorithm above does *not* depend on type or kind inference;
   it is a straightforward pass over the abstract syntax of the constructor.

   This version preserves today's scoping rules (and upholds the LLSP_). It
   is backward-compatible. But it is subtler. Maybe it is better, regardless.

#. We could offer users a migration period, where we warn about this
   impending change. I see no easy way of implementing such a check,
   and I see relatively little value in doing so, given that the fixes
   are really quite easy. This opinion may change in the light of experience,
   if this feature is implemented and we see trouble in the wild.

.. _pattern-type-args:

Type arguments in constructor patterns
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is an update to accepted, implemented proposal `#126`_ that changes
its treatment of universals. It incorporates the logic of not-yet-accepted
amendment `#291`_.

Specification
^^^^^^^^^^^^^

1. Introduce a new extension ``-XTypeAbstractions``.

#. When ``-XTypeAbstractions`` is enabled, allow type application syntax
   in constructor patterns.

   Concretely, the grammar goes from ::

     pat → gcon apat1 … apatk
         …

   to ::

       pat → gcon tyapp_or_pat1 … tyapp_or_patk
           …

       tyapp_or_pat → '@' atype    -- '@' is in prefix position
                    → pat

#. Type applications in constructor patterns do *not* affect whether
   the pattern-match is successful.

#. Type applications in constructor patterns must correspond to ``forall … .``
   quantifications in the declared constructor or pattern synonym type.
   (Right now, pattern synonyms require all such quantifications to occur
   before any term arguments, but accepted proposal `#402`_ allows these
   quantifications to occur in any order in data constructors.

#. Each quantification in a data constructor or pattern synonym brings
   into scope either a universal variable or an existential variable.
   Telling these apart is easy in pattern synonym types; `see above <#gadt-syntax>`_ for how to determine this property of data constructor
   types.

   1. A type argument corresponding to a universal variable, if given,
      must be ``_``. No exceptions.

   #. A type argument corresponding to an existential variable, if given,
      must be a bare variable or a ``_``. If a variable, this variable is
      unconditionally brought into scope (possibly shadowing any existing
      type variable with the same spelling), bound to the existential type
      packed in the datatype.

#. A wildcard ``_`` as a type argument says simply to skip that argument;
   it does not trigger any behavior associated with partial type signatures.
   In particular, ``-XPartialTypeSignatures`` is not necessary, and no
   diagnostic is produced.

#. As with term variables, it is an error to bring the same type variable
   into scope in two (or more) places within the same pattern.

Examples
^^^^^^^^

::

  f1 (Just @Int x) = x + 1

This is accepted under `#126`_ but rejected under this current proposal,
because we do not allow instantiation of universals. See the
`universals and existentials <#universals-and-existentials>`_ section
for a discussion.

If you want this behavior under this proposal, write a type signature.

::

  {-# LANGUAGE ScopedTypeVariables #-}
  data Ex = forall a. MkEx a
  f2 :: forall b. b -> Ex -> Int
  f2 y (MkEx @b z) = ...

This is rejected under `#126`_,
as it appears to insist that the existential
type packed in ``MkEx`` is the same as the type argument passed to ``f2``.
On the other hand, this is accepted by the current proposal, allowing the
existential ``b`` to shadow the ``b`` brought into scope by the ``forall``.

This shadowing behavior mimics what happens with term variables in patterns.

Effects
^^^^^^^

1. The ability to bind existential variables via a construct such as this
   is necessary to support the EVP_.

#. Forbidding instantiation of universals is to uphold the VOP_ and LSPC_.

#. Having type variables have the same behavior as term variables with
   respect to shadowing (and repeated binding) upholds the VOP_. In addition,
   the fact that type variables are unconditionally brought into scope upholds
   the LLSP_.

#. Allowing users to write ``@_`` for universal arguments upholds the PEDP_.
   An alternative would be simply to skip universals in patterns (as Coq does,
   for example), but this violates the PEDP_. I expect a future proposal to
   arrive eventually that will allow a syntax for instantiating universals;
   the current treatment would be forward compatible with any such syntax.

Type arguments in lambda patterns
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a restatement of accepted proposal `#155`_, as amended by not-yet-accepted
`#238`_. For motivation, please see `#238`_.

Specitification
^^^^^^^^^^^^^^^

A. With ``-XTypeAbstractions``, introduce a new form of pattern (cf. The `Haskell 2010 Report`_)::

     apat → … | '@' tyvar | '@' '_'   -- '@' is a prefix occurrence

   Conveniently, ``apat``\ s are used both in function left-hand sides
   and in lambda-expressions, so this change covers both use-cases.

#. A type variable pattern would not be allowed in the following contexts:

   1. To the right of an as-pattern
   #. As the top node in a lazy (``~``) pattern
   #. As the top node in a ``lpat`` (that is, to the left of an infix
      constructor, directly inside a parenthesis, as a component of
      a tuple, as a component of a list, or directly after an ``=``
      in a record pattern)

#. Typing rules for the new construct are as in a `recent paper
   <https://richarde.dev/papers/2021/stability/stability.pdf>`_: see
   ETm-InfTyAbs, ETm-CheckTyAbs, Pat-InfTyVar, and Pat-CheckTyVar, all in
   Figure 7. While the typeset versions remain the official typing rules,
   I will summarise the different rules below.

   **Background**. GHC implements *bidirectional* type-checking, where
   we sometimes know what type to expect an expression to have. When we
   know such a type (for example, because we have a type signature, or
   an expression is an argument to a function with a known type), we say
   we are in *checking* mode. When we do not know such a type (for example,
   when we are inferring the type of a ``let``\ -binding or the type of
   a function applied to arguments), we say we are in *synthesis* mode.
   The `Practical Type Inference <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/putting.pdf>`_ paper gives a nice, Haskell-oriented introduction.

   1. In synthesis mode, when examining ``\ @a -> expr``, we simply put
      ``a`` in scope as a fresh skolem variable (that is, not equal
      to any other type) and then check ``expr``. (Presumably, ``expr``
      uses ``a`` in a type signature.) When we infer that ``expr`` has
      type ``ty``, the expression ``\ @a -> expr`` has type ``forall a. ty``.
      Example: ``\ @a (x :: a) -> x`` infers the type ``forall a. a -> a``.
      (For this example, we note that ``\ @a (x :: a) -> x`` is a short-hand
      for ``\ @a -> \ (x :: a) -> x``.)

   #. In checking mode, when examining ``\ @a -> expr`` against type ``ty``,
      we require that ``ty`` has the shape ``forall a. ty'``, where
      ``a`` is a *specified* variable (possibly
      after skolemising any *inferred* variables in ``ty``), renaming the
      bound variable as necessary to match the name used in the expression.
      We then check ``expr`` against type ``ty'``.

   #. In synthesis mode, when examining a function argument ``@a`` to
      a function ``f``, we
      bring ``a`` into scope as a fresh skolem variable and check the
      remainder of the arguments and the right-hand side. In the type
      of ``f``, we include a ``forall a.`` in the spot corresponding
      to the type variable argument.

      If there are multiple equations, each equation is required
      to bind type variables in the same locations. (If this is
      burdensome, write a type signature.) (We could probably do
      better, by inferring the maximum count of bound type
      variables between each required argument and then treating
      each set of bound type variables as a prefix against this
      maximum, but there is little incentive. Just write a type
      signature!)

   #. In checking mode, when examining a function argument ``@a`` to
      a function ``f`` with type signature ``ty``, we require the corresponding
      spot in the type signature to have a ``forall a`` (possibly renaming
      the bound variable). The type variable ``a`` is then brought
      into scope and we continue checking arguments and the right-hand side.

      Multiple equations can bind type variables in different places,
      as we have a type signature to guide us.

#. Typing rules for pattern synonym bindings are complicated, as usual.

   1. A visible type abstraction in a pattern synonym binding that lacks
      a type signature is rejected. (While we could, at some cost, work
      out what should happen here, please just use a type signature.)

   #. (Background information; no new specification here.)
      Pattern synonym type signatures have a restricted form that looks
      like this::

         pattern P :: forall universal_tvs.   required_context =>
                      forall existential_tvs. provided_context =>
                      arg1 -> arg2 -> ... ->
                      result

      `The GHC manual <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/pattern_synonyms.html#typing-of-pattern-synonyms>`_ has the details for how parts
      of this signature can be left out; I will not repeat these rules here.
      The key observation is that all quantified type variables occur
      *before* any required term-level arguments.

      Furthermore, pattern synonym bindings may be specified in two parts,
      for explicit bidirectional pattern synonyms::

         pattern P <- pat
           where P = expr

      Call the top line the *pattern synonym pattern binding*, while
      the second line is the *pattern synonym expression binding*.

      In an implicitly bidirection pattern synonym binding, the
      pattern synonym pattern binding and pattern synonym expression
      binding are written with one bit of syntax. For the purposes
      of this proposal, though, we consider type-checking this
      bit of syntax *twice*, once as a pattern synonym pattern binding,
      and once as a pattern synonym expression binding.

   #. With ``-XTypeAbstractions``, a pattern synonym pattern binding may
      include any number of type abstractions (such as ``@a`` or ``@_``)
      directly after the pattern synonym name. (Such a binding must be written
      in prefix notation, not infix.)
      These bindings correspond to a prefix of the *specified* *universal* type variables
      in the pattern synonym's type. It is an error to write more type
      abstractions than there are specified universal variables.

      Each type abstraction binds a local name to the corresponding
      universal type variable. These names are available in the right-hand
      side (after the ``<-`` or ``=``).

      (Existentials are excluded here because an existential type variable
      is bound by the pattern in the right-hand side. There appears to be
      no motivation for being able to name these on the left.)

      The rules for the usage of such variables on the right-hand side are
      unchanged from the way scoped type variables work in pattern synonyms
      today.

   #. With ``-XTypeAbstractions``, a pattern synonym expression binding
      may include any number of type abstractions (such as ``@a`` or ``@_``)
      directly after the pattern synonym name. (Such a binding must be written
      in prefix notation, not infix.) These correspond to a prefix of
      the concatentation of the specified universal and specified existential type variables
      written in the pattern synonym type signature. It is an error
      to write more type abstractions than there are specified universal
      and specified existential type variables.

      Each type abstraction binds a local name to the corresponding
      universal or existential type variable. These names are available in the
      right-hand side (after the ``=``).

      (Existentials are included here because a pattern synonym used as an
      expression takes existentials as arguments from call sites, and it is
      sensible to bind these on the left.)

      The rules for the usage of such variables on the right-hand side are
      just as they exist for ordinary function bindings.

#. ``-XTypeAbstractions`` and ``-XScopedForAlls`` have a fraught relationship,
   as both are trying to accomplish the same goal via different means. Here are
   the rules keeping this sibling rivalry at bay:

   1. ``-XScopedForAlls`` does not apply in expression type signatures. Instead,
      if users want a type variable brought into scope, they are encouraged to
      use ``-XTypeAbstractions``. (It would not be hard to introduce a helpful
      error message instructing users to do this.)

   #. If ``-XScopedForAlls`` is enabled,
      in an equation for a function definition for a function ``f`` (and similar
      for pattern synonym pattern bindings and pattern synonym expression bindings):

      * If ``f`` is written with no arguments or its first argument is not
        a type argument (that is, the next token after ``f``
        is not a prefix ``@``), then ``-XScopedForAlls`` is in effect and
        brings type variables into scope.

      * Otherwise, if ``f``\'s first argument is a type argument, then
        ``-XScopedForAlls`` has no effect. No additional type variables
        are brought into scope.

#. (Optional extra) If ``-XTypeAbstractions`` is in effect, then a function
   binding may use ``@(..)`` on its left-hand side. Here is the BNF (cf. the
   `Haskell 2010 Report`_, Section 4.4.3), recalling that braces mean "0 or more"::

     funlhs  →  var apat { apat }
             |  pat varop pat
             |  '(' funlhs ')' apat { apat }
             |  funlhs '@' '(' '..' ')'

   The last line is new, and we assume the ``@`` is in prefix form. This construct
   is available only when the function being defined has a type signature.
   The new construct brings into scope all type variables brought into scope
   at that point in the signature. Note that implicitly quantified type variables
   are brought into scope at the top of a signature, and so ::

     f :: a -> b -> a
     f @(..) = -- RHS

   would have ``a`` and ``b`` in scope in the ``RHS``.

   The ``@(..)`` construct works for both *specified* and *inferred* variables,
   and is additionally available in pattern synonym pattern bindings (where it
   brings into scope only universals) and pattern synonym expression bindings
   (where is brings into scope both universals and existentials). (In an implicitly
   bidirectional pattern synonym, the ``@(..)`` brings into scope only universals.)

Examples of new behavior of scoped type variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

   f :: forall a. a -> a
   f @b x = (x :: a)   -- rejected, because -XScopedForAlls is disabled here

   g :: forall a. a -> a
   g @a x = (x :: a)   -- accepted with -XTypeAbstractions

   h = ((\x -> (x :: a)) :: forall a. a -> a)
     -- accepted with previous -XScopedTypeVariables, but rejected
     -- now

   i = ((\ @a x -> (x :: a)) :: forall a. a -> a)
     -- accepted with -XTypeAbstractions

Note that turning off ``-XScopedForAlls`` with ``-XTypeAbstractions`` is necessary if we
think about where type variables are brought into scope. Are they brought into
scope by the ``forall``? Or by the ``@a``? It can't be both, as there is no
sensible desugaring into System F. Specifically, if we have ``expr :: forall a. ty``,
that gets desugared into ``/\ a -> expr``. If we have ``(\ @a -> expr) :: forall b. ty``,
what does it get desugared into? It would have to be ``/\ b -> /\ a -> expr``, but then
``b`` and ``a`` are different.

Here might be another way of thinking about it. Suppose we're checking ``expr`` against
the pushed-down (known) type ``forall a. ty``. If we bring ``a`` into scope, what type
do we check ``expr`` against? Is it ``forall a. ty`` again? That's very awkward if ``a``
is *already* in scope. If we check ``expr`` against ``ty`` and ``expr`` looks like
``\ @b -> expr'``, then we check ``\ @b -> expr'`` against ``ty`` -- not against
``forall a. ty``.

Effects
^^^^^^^

1. This delivers the EVP_, meaning we can rid of ``Proxy``.

2. The optional extra ``@(..)`` notation seems like a convenient middle ground,
   allowing for an easy transition from the old-style ``-XScopedTypeVariables``
   to the newer ``-XTypeAbstractions``. It brings the *inferred* variables (from `#99`_)
   into
   scope, quite conveniently. This new notation also allows type variables to
   be brought into scope without the ``forall`` keyword in the type, in case
   the user does not want to trigger ``forall``\ -or-nothing behavior.

   Note that this notation is forward compatible with visible dependent quantification
   in terms (`#281`_)::

     f :: foreach (count :: Int) (label :: String) (is_paid_for :: Bool) -> Invoice
     f (..) = -- here, count, label, and is_pair_for are all in scope

   This style allows for more perspicuous types while avoiding redundancy. The particular
   example here uses ``foreach`` to denote arguments that are available at runtime, but
   nothing about ``foreach`` is required to make this all work (as far as scoping is
   concerned).

   Accepting the ``@(..)`` syntax does *not* entail accepting this new, separate
   ``(..)`` syntax, though it is good to know that the idea is forward compatible.

   A ``@(..)`` argument counts as a type argument when asking whether ``-XScopedForAlls``
   affects a function equation.

   The new ``@(..)`` notation does *not* work with expression type signatures,
   lambda-expressions, or anywhere other than a function binding with a type
   signature. This is because doing so would require propagating type
   information into scoping, which is problematic.

   Some have argued on GitHub that it may be best to hold off the ``@(..)`` until
   we gain more experience here: adding new features is easier than removing them.
   While I agree that this could be done, the ``@(..)`` construct makes for a very
   easy migration from today's ``-XScopedTypeVariables`` and is thus tempting to
   be around from the start. I don't feel strongly.

#. (technical) The `Visible Type Applications`_ (VTA) paper defines the behavior about what to
   do when checking against a polytype: it says to deeply skolemize. However, eager deep
   skolemization will spell trouble for this extension, as we need the lambdas to see
   the ``forall``\s. The end of the Section 6.1 in the `extended VTA <https://cs.brynmawr.edu/~rae/papers/2016/type-app/visible-type-app-extended.pdf>`_ paper discusses
   why we do eager deep skolemization: essentially, the alternative would be to do
   type generalization at inflection points between checking and inference mode,
   right before doing the subsumption check. Type generalization is hard in GHC, though,
   and so the paper avoided it. In order to implement this proposal, we'll have to work
   out how to do this.

Costs and Drawbacks
^^^^^^^^^^^^^^^^^^^

1. This part of the proposal
   is *not* backward-compatible with today's ``-XScopedTypeVariables``,
   because it rejects expressions like ::

     ((\x -> (x :: a)) :: forall a. a -> a)

   which are accepted today. No migration period is proposed, because it is
   very hard to imagine how ``-XTypeAbstractions`` and ``-XScopedForAlls`` should
   co-exist peacefully here. Instead, we can issue a specific error message telling
   users how to migrate their code in this case.

   My hope is that constructs such as this one are rare and would not impact many
   users.

   If necessary, we could imagine taking the expression ``expr :: forall ... . ty``
   and looking proactively to see whether ``expr`` ever uses a type variable
   pattern from this proposal. If not, ``-XScopedForAlls`` could trigger (and we
   issue a warning with ``-Wcompat``). But, if a type argument appears anywhere
   in ``expr``, then ``-XScopedForAlls`` is disabled. This would be backward-compatible,
   but unfortunately non-local and annoying. I prefer just to skip this
   migration step.


.. _type-let:

``let``-binding types
~~~~~~~~~~~~~~~~~~~~~

This segment of the proposal goes beyond previous proposals in describing a mechanism
to use ``let`` to bind type synonyms.

Motivation
^^^^^^^^^^

1. Users have, from time to time, requested the ability to make local type synonyms.
   GHC even has a little support for synonyms via equality constraints (e.g., writing
   ``f :: (a ~ Some Big Type With Lots Of Parts) => Maybe a -> a -> Maybe a``). Instead
   of encoding this idea via equality constraints, though, it would be nice to support
   it directly.

#. Now that type variables can stand for types, we can write code like ::

     f :: Maybe Bool -> Bool
     f (x :: Maybe b) = (True :: b)

   Note that the pattern signature binds ``b`` to ``Bool``. This is, essentially, a ``let``\ -bound
   type variable: in the scope of ``b``, ``b`` is synonymous with ``Bool``. Yet the only way
   to make such a ``b`` is via a pattern (or result, `#228`_) signature. Why force users
   to use matching instead of binding the variable directly.

#. Doing this helps uphold the EBP_.

Specification
^^^^^^^^^^^^^

1. Create a new extension ``-XExtendedLet``.

#. With ``-XExtendedLet``, add two new productions for ``decl`` (from the `Haskell 2010 Report`_), ::

     decl → 'type' simpletype '=' type
          → 'type' tyvar '=' type

   and remove the production ``topdecl → 'type' simpletype '=' type`` from ``topdecl``.

   Note that the second form allows a local binding for a lower-case ``tyvar``; these
   synonyms may not be parameterized.

#. The form ``decl → 'type' tyvar '=' type`` is not allowed at top-level.

#. These new declaration forms introduce local type synonyms in terms, which scope over the same
   region of code that other declarations in the same ``let`` / ``where`` clause scope over.

   Like other type synonyms, local type synonyms may not be recursive.

#. Wildcards are allowed in the right-hand side of local synonyms. At usage sites of the
   synonym, the synonym is expanded. It is an error if that location does not allow wildcards.
   The wildcard is understood to stand for just one type shared among all the expansions.

Effects
^^^^^^^

1. We can now bind local type synonyms, avoiding the need to do so via pattern or result
   signatures.

#. One challenge is how to present these local synonyms in error messages. It might be
   best to aggressively expand (unlike top-level type synonyms), especially because these
   local synonyms might refer to other local type variables that are in scope. As we gain
   experience with this new form, we can refine their appearance in error messages.

#. Note that this proposal does *not* allow for top-level lower-case type synonyms. There
   is nothing stopping us from doing so, but it would seem to violate expectations of Haskellers
   and would be the first instance of a lower-case type variable being in scope at the top level.

``let`` in patterns
~~~~~~~~~~~~~~~~~~~

This part of this proposal allows introducing a ``let``\ -binding in a pattern.
The bound variable(s) scope over the same region of code as the pattern-bound
variables do.

The syntax for this feature is a bit awkward, and so this proposal presents
two alternatives for discussion.

Motivation
^^^^^^^^^^

1. A careful reader will note that allowing ``let`` for `type synonyms <#type-let>`_
   does not, by itself, replace a binding such as that in ``f (True :: b) = ...``
   because the current form binds the type variable in the pattern. This part
   of the proposal closes this gap. See `examples <#let-in-pattern-example>`_ below.

#. Though admittedly a weakish motivation, there is currently no way to share
   expressions used in common in multiple view patterns. See `examples <#let-in-pattern-example>`_
   below.

Specification Alternative 1
^^^^^^^^^^^^^^^^^^^^^^^^^^^

1. With ``-XExtendedLet``, add a new form of pattern as follows::

     pat → 'let' decls 'in' pat

#. Any entites bound in ``decls`` scope over the same region of the program
   that pattern-bound variables scope over, with the addition of the ``decls``
   themselves (that is, the declarations can be recursive).

Specification Alternative 2
^^^^^^^^^^^^^^^^^^^^^^^^^^^

0. **Background**. Here are some productions from the `Haskell 2010 Report`_::

     funlhs → var apat {apat}
            | pat varop pat
            | '(' funlhs ')' apat {apat}

     apat → gcon
          | literal
          | …

     lexp → '\' apat1 … apatn '->' exp   (n ≧ 1)
          | …

     lpat → apat
          | '-' (integer|float)
          | gcon apat1 … apatn

   (Recall that braces mean "0 or more".)

1. With ``-XExtendedLet``, change the grammar to be ::

     funlhs → var apats1
            | pat varop pat
            | '(' funlhs ')' apats

     apats1 → apat
            | apat apats
            | '(' 'let' decls ')' apats

     apats →
           | apats1

     lexp → '\' apats1 '->' exp
          | …

     lpat → apat
          | '-' (integer|float)
          | gcon apats

   This allows phrases like ``f x (let y = g x x) (frob y -> True) = ...``, where we can include
   a ``let`` construct in the middle of a list of patterns. The pattern grammar itself is unaffected.

2. Any entities bound in ``decls`` scope over the same region of the program
   that pattern-bound variables scope over, with the addition of the ``decls``
   themselves (that is, the declarations can be recursive).

.. _let-in-pattern-example:

Examples
^^^^^^^^

1. Instead of ::

     f :: Maybe Bool -> Bool -> Bool
     f (x :: Maybe b) (y :: b) = ...

   we can write (Alternative 1) ::

     f :: Maybe Bool -> Bool -> Bool
     f (let type b = Bool in x) (y :: b) = ...

   or (Alternative 2) ::

     f :: Maybe Bool -> Bool -> Bool
     f (let type b = Bool) x (y :: b) = ...

   Note that the ``b`` is in scope in the type signature for ``y``.

   If we instead say (Alternative 1) ::

     f (let type b = _ in (x :: Maybe b)) (y :: b) = ...

   or (Alternative 2) ::

     f (let type b = _) (x :: Maybe b) (y :: b) = ...

   now the choice ``b ~ Bool`` is inferred, but we have an explicit binding
   site for ``b``, in accordance with the EBP_.

#. Instead of ::

     f x y z (frob x y z -> True) (frob x y z -> False) = ...

   we can write (Alternative 1) ::

     f x y z (let test = frob x y z in (test -> True)) (test -> False) = ...

   or (Alternative 2) ::

     f x y z (let test = frob x y z) (test -> True) (test -> False) = ...

   avoiding some repetition.

``let`` in types
~~~~~~~~~~~~~~~~

This part of the proposal allows ``let`` to be used in types.

Motivation
^^^^^^^^^^

1. The careful reader will notes that the `secction above <#type-lets>`_ defining
   the ability to bind type synonyms in ``let`` expressions does not actually address
   a motivating example. This component of this proposal allows us to avoid repetition
   within a type signature.

Specification
^^^^^^^^^^^^^

1. With ``-XExtendedLets``, expand the grammar for types to include the following::

     type → 'let' tdecls 'in' type

     tdecls → '{' tdecl1 ';' ... ';' tdecln '}'
     tdecl → simpletype '=' type
           → tyvar '=' type

   Note that we do not include the ``type`` keyword in the grammar above, because
   we are already in type-syntax.

#. The type synonyms introduced in a ``let`` in types scope over the type after the
   ``in``.

#. As above, the synonyms may mention wildcards, and the definitions may not be recursive.

Examples
^^^^^^^^

1. Instead of ::

     f :: forall a b. (c ~ Very Big Type a b) => c -> c -> c

   we can write ::

     f :: forall a b. let c = Very Big Type a b in c -> c -> c

   which more directly expresses what we mean.

Effects
^^^^^^^

1. This step further unifies term-level and type-level syntax, at low cost.

#. An initial version of this feature will likely want to expand the synonyms
   aggressively. We can think about ways to preserve synonyms as we gain experience
   with the feature.

#. This part of the proposal does not directly serve any of the principles outlined
   at the top of this proposal, but now seems a convenient time to introduce this
   extension, which should be relatively easy to implement.

Effects and Interactions
------------------------

The effects of this proposal are written out in the individual sections. Here,
I summarize the effects on the principles_ laid out above.

1. The LSPC_ is upheld. Binders occur in patterns, after ``forall``, in
   ``let`` declarations, and a few other discrete places in the AST -- and
   nowhere else. In particular, binders do not occur in pattern signatures.
   Instead, with ``-XPatternSignatureBinds``, an occurrence of an out-of-scope
   variable ``a`` induces a ``let type a = _ in`` to be prefixed to the pattern.

#. The LLSP_ is made to hold, by describing pattern-signature binds as occurrences
   and making type applications in patterns unconditionally bring new variables
   into scope.

#. The SUP_ is supported. The new ``let`` syntax in types is a strict subset
   of its syntax in terms, and the semantics are compatible.

#. The EVP_ is made to hold, by allowing explicit binders for type variables
   for existentials and the variables bound by an inner ``forall`` in a higher-rank
   type.

#. The EBP_ is made to hold, by introducing ``-XNoImplicitForAll`` and
   ``-XNoPatternSignatureBinds``.

#. The VOP_ is made to hold, by ensuring that types and terms are treated identically
   in patterns.

#. The PEDP_ is respected, by allowing space for universals in patterns. It is up
   to a future proposal to figure out how universals can be instantiated in patterns,
   but this current proposal is future-compatible with other ideas, and it retains
   the correspondence between arguments in patterns and arguments in expressions.

Costs and Drawbacks
-------------------

1. This proposal, if accepted in full, is a pretty drastic change to the way
   scoped type variables are described and implemented in GHC. However, it is
   designed to be mostly backward compatible and should affect downstream users
   rather little.

Alternatives
------------

1. It is possible to break this proposal up into smaller pieces. In particular,
   any of the changes to ``let`` are completely separable from the rest of the
   proposal and from each other. These pieces are included here only because
   they fit nicely with the other ideas in this proposal and it would seem to
   be less jarring to users to get this all done at once. At a minimum, if these
   pieces are left off, we see here how the design of this proposal is forward
   compatible with these additions.

Unresolved Questions
--------------------

None at this time.

Implementation Plan
-------------------

I am very keen to get this implemented and would be happy to support others
taking on this work or to do it myself.

Endorsements
-------------

Please feel free to submit a PR against this one to add your name here!