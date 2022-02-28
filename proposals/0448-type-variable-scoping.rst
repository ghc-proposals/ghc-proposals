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

.. _`#99`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0099-explicit-specificity.rst
.. _`#119`: https://github.com/ghc-proposals/ghc-proposals/pull/119
.. _`#126`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0126-type-applications-in-patterns.rst
.. _`#128`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0128-scoped-type-variables-types.rst
.. _`#155`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0155-type-lambda.rst
.. _`#228`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0228-function-result-sigs.rst
.. _`#238`: https://github.com/ghc-proposals/ghc-proposals/pull/238
.. _`#270`: https://github.com/ghc-proposals/ghc-proposals/pull/270
.. _`#281`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst
.. _`#285`: https://github.com/ghc-proposals/ghc-proposals/pull/285
.. _`#291`: https://github.com/ghc-proposals/ghc-proposals/pull/291
.. _`#378`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst
.. _`#402`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0402-gadt-syntax.rst
.. _`#420`: https://github.com/ghc-proposals/ghc-proposals/pull/420
.. _Type Variables in Patterns: https://richarde.dev/papers/2018/pat-tyvars/pat-tyvars.pdf
.. _Kind Inference for Datatypes: https://richarde.dev/papers/2020/kind-inference/kind-inference.pdf
.. _`Haskell 2010 Report`: https://www.haskell.org/onlinereport/haskell2010/haskellch10.html
.. _`Visible Type Applications`: https://richarde.dev/papers/2016/type-app/visible-type-app.pdf
.. _`principles`: ../principles.rst
.. _`Contiguous Scoping Principle`: ../principles.rst#contiguous-scoping-principle
.. _`Explicit Binding Principle`: ../principles.rst#explicit-binding-principle
.. _`Explicit Variable Principle`: ../principles.rst#explicit-variable-principle
.. _`Visibility Orthogonality Principle`: ../principles.rst#visibility-orthogonality-principle
.. _`Local Lexical Scoping Principle`: ../principles.rst#local-lexical-scoping-principle
.. _`Syntactic Unification Principle`: ../principles.rst#syntactic-unification-principle
.. _`Pattern/Expression Duality Principle`: ../principles.rst#pattern-expression-duality-principle
.. _`Lexical Scoping Principle`: ../principles.rst#lexical-scoping-principle

* `#126`_: Accepted, implemented proposal on accepting type arguments to constructor
  patterns, allowing constructions like ``f (Just @Int x) = x + 5``
  and ``g (Dynamic @a x) = show (typeOf (x :: a))``.
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

How to read this proposal
-------------------------

This is a large proposal, with a number of moving parts. The essential reason
all these moving parts are glued together in just one proposal is so that they
can be unified by their desire to uphold the principles added to our
`principles`_ document. Individual
components of this proposal can be designed, debated, and implemented separately,
yet are presented in one document as they are meant to dovetail together nicely.

As currently written, this proposal is not self-contained, in that motivation for
some individual pieces was not copied from their source proposals. In all cases,
when this proposal refers to others as inspiration, seeking more information there
will likely be helpful.

If this proposal is accepted, it may be a good idea to incorporate that motivation,
etc., right in this proposal here, to make it self-contained. I am happy to do this
at the direction of the committee.

Extension shuffling
-------------------

Right now, ``-XScopedTypeVariables`` does a lot of heavy lifting. This proposal
breaks up ``-XScopedTypeVariables`` into its components. This enables finer-grained
control, and the ability for e.g. the ``a`` in ``f :: forall a. a -> a`` not to
scope over the definition of ``a``.

The new meaning of ``-XScopedTypeVariables`` is the same as the old one. The only
backward-incompatible part of this is that, today, ``-XPatternSignatures`` is a deprecated
synonym of ``-XScopedTypeVariables``. Under this change, that would no longer be true.

With the exception of ``-XImplicitForAll`` and ``-XPatternSignatureBinds``,
this component of this proposal is taken
from the not-yet-accepted proposal `#238`_, with no change in meaning compared to
that proposal. ``-XImplicitForAll`` and ``-XPatternSignatureBinds`` come from accepted
proposal `#285`_, and differ from that proposal only in the default setting of
``-XPatternSignatureBinds``. I advocate here that it should be on by default, while
`#285`_ takes the opposite view. More explanation on this point below.

Motivation for ``-XPatternSignatures``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is taken from `#119`_. "I" and "me" here is Joachim Breitner, aka @nomeata.

Originally, ``PatternSignatures`` was a an extension on its own, but at some point it started to imply
``ScopedTypeVariables`` and eventually was deprecated in favor of the latter. This has always bothered me
and I often find myself in situations where I need to use a pattern signature without having any need for scoped
type variables. This need has increased with more polymorphic functions in ``base`` (e.g. post FTP).

I too often thoughts “I should have raised this point when it was time, but it is too late now”. But maybe it is not
too late… hence this proposal.

The concrete motivation is to be able to write something like this::

   {-# LANGUAGE OverloadedStrings #-}
   foo :: Monad m => m Int
   foo = do
     list <- return ""
     return $ length list

Currently, this fails with (much shortened)::

    Test.hs:4:18: error:
        • Could not deduce (Data.String.IsString (t0 a0))
            arising from the literal '""'
    Test.hs:5:12: error:
        • Could not deduce (Foldable t0) arising from a use of 'length'

Ah, the FTP strikes again. So to fix this, I have to specify ``list``\ 's type.
In Haskell98 I can add a type signature to the use of ``list``, but that is ugly: Types should
be declared where stuff is brought into scope! So I want to write::


   {-# LANGUAGE OverloadedStrings #-}
   foo :: Monad m => m Int
   foo = do
     list :: String <- return ""
     return $ length list

but I get::

    Test.hs:4:3: error:
        Illegal type signature: 'String'
          Type signatures are only allowed in patterns with ScopedTypeVariables

Ok, that works, but why am I bothered with ``ScopedTypeVariables``? Furthermore, ``ScopedTypeVariables`` is not
conservative; it may actually break my program somewhere!

What I really want in this case is a pattern signature, and it would be nice if I could
just state that ``PatternSignatures``.

Proposed Change Specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Points below up to and including the new (backward-compatible) definition of
``-XScopedTypeVariables`` come from not-yet-accepted proposal `#238`_. The point
about ``-XImplicitForAll`` is a restatement of (part of) accepted proposal `#285`_.
The other part of `#285`_ is about ``-XPatternSignatureBinds``, as noted in that bullet point.

1. Re-purpose deprecated extension ``-XPatternSignatures``. With ``-XPatternSignatures``, we
   allow type signatures in patterns. These signatures can mention in-scope
   type variables as variable occurrences, but can not bind type variables.

   The current ``-XPatternSignatures`` is just a synonym for ``-XScopedTypeVariables``.
   This change is thus not backward-compatible, but given that the existing extension
   is deprecated, I think this change is acceptable.

#. Introduce ``-XPatternSignatureBinds``. With ``-XPatternSignatureBinds``, any
   out-of-scope type variables written in a pattern signature would be bound there
   and would remain in scope over the
   same region of code that term-level variables introduced in a pattern scope
   over. This extension is on by default.
   (This extension is a part of accepted, unimplemented proposal
   `#285`_.)

   Motivation for "on by default": The effect on pattern signatures requires
   ``-XPatternSignatures`` to be witnessed, and so having this be on by default
   does not change the meaning of Haskell98. Furthermore, this makes it easier
   to use ``-XPatternSignatures``, without needing to manually enable another
   extension to be able to bind type variables.

#. Introduce ``-XMethodTypeVariables``. With ``-XMethodTypeVariables``, type
   variables introduced in an instance head would scope over the bodies of
   method implementations. Additionally, type variables introduced in a class
   head would scope over the bodies of method defaults.

#. Introduce ``-XExtendedForAllScope``. With ``-XExtendedForAllScope``, any type variables
   mentioned in an explicit ``forall`` scopes over an expression. This applies
   to the following constructs:

   * Function bindings
   * Pattern synonym bindings (including in any ``where`` clause)
   * Expression type signatures

   Separating out ``-XExtendedForAllScope`` gets us closer to the `Contiguous Scoping Principle`_.

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

   #. ``RULES`` pragmas.
      Example: ``{-# RULES "name" forall (x :: Maybe a). foo x = 5 #-}``
      becomes ``{-# RULES "name" forall a. forall (x :: Maybe a). foo x = 5 #-}``.
      (The double-\ ``forall`` syntax separates type variables like ``a`` from
      term variables like ``x``.)

   This extension is part of accepted, unimplemented proposal `#285`_; the only change is including ``RULES`` pragmas, which @Ericson2314 simply forgot to include in `#285`_ (his own admission).

   Being able to turn off this extension is necessary to uphold the `Explicit Binding Principle`_.

Alternatives
~~~~~~~~~~~~

1. We could have ``-XPatternSignatureBinds`` off by default. This would mean less effort
   for users to activate the `Explicit Binding Principle`_. However, it would also mean that the new ``-XPatternSignatures`` deviates from the old, pre-\ ``-XScopedTypeVariables`` meaning
   of ``-XPatternSignatures``.

   My inclination is to keep this aspect as written in the proposal, with ``-XPatternSignatureBinds`` on by default. Pattern signatures are convenient and intuitive.
   Users wishing for the `Explicit Binding Principle`_ can be expected to work a little harder to get it.

   On the other hand: currently ``-XPatternSignatures`` is deprecated, and so we have a rare window to "reset" things. We can always decide later ``-XPatternSignatureBinds`` should be implied by ``-XPatternSignatures``, and just except more programs.
   The converse is not true: if ``-XPatternSignatureBinds`` is implied by ``-XPatternSignatures`` it would be a breaking change to remove that implication.
   It may be prudent to start conservatively, taking maximal advantage of the current deprecated state of ``-XPatternSignatures`` to leave our options open for the future.

Type arguments in constructor patterns
--------------------------------------

.. _pattern-type-args:

This is an update to accepted, implemented proposal `#126`_,
incorporating the logic of not-yet-accepted amendment `#291`_.

The original proposal `#126`_ is indeed implemented and released,
but the implementation is not faithful to the specification around
type variables that are already in scope. The original proposal says
that, if ``a`` is already in scope, then ``f (Just @a x) = ...`` is an *occurrence*
of the in-scope ``a``. By contrast, the implementation errors in this case.

Not-yet-accepted amendment `#291`_ says that type variables scope
just like term variables: they can be shadowed. Accordingly, ``f (Just @a x) = ...``
would always, unconditionally bind a new type variable ``a``, possibly
shadowing any in-scope type variable ``a``. This design supports the
`Visibility Orthogonality Principle`_, which states that the presence of
an ``@`` should affect only whether a thing is visible or not, not other
characteristics (like its shadowing and scoping behavior). Additionally,
this choice edges us closer to the `Local Lexical Scoping Principle`_,
because we no longer have to check whether ``a`` is in scope before identifying
the ``a`` in ``f (Just @a x) = ...`` is a binding site or an occurrence.

The other change in this restatement is the use of new extension ``-XTypeAbstractions``
instead of the current status of piggy-backing on ``-XTypeApplications``.

Motivation
~~~~~~~~~~

This is taken directly from `#126`_.

``TypeApplications`` are a convenient and natural way to specifying types of polymorphic functions. Consider::

  data Foo a where MkFoo :: forall a. a -> Foo a

With ``TypeApplications``, I can replace the somewhat clumsy ``MkFoo (x :: ty)`` with ``MkFoo @ty x``. Seen this way,
explicit type applications are merely an alternative syntax for type signatures.

At the moment, this only works in terms, but not in patterns: We can use type signatures in patterns
(if ``PatternSignatures`` or ``ScopedTypeVariables`` are enabled), but not type applications. Given the strong
relation between these syntactic forms, this is odd – why can I write::

    foo (MkFoo (x :: ty)) = …

but not::

    foo (MkFoo @ty x) = …

This proposal fills this gap: It allows type applications in patterns, and specifies them to behave “just like type signatures”.

The intention of the following specification is that the following holds: For a constructor with type like ``C :: forall a. a -> …`` the meaning of ``C @ty x`` should coincide with the existing form ``C (x :: ty)``.

Proposed Change Specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Introduce a new extension ``-XTypeAbstractions``, implied by ``-XScopedTypeVariables``.

#. When ``-XTypeAbstractions`` is enabled, allow type application syntax
   in constructor patterns.

   Concretely, the grammar goes from ::

     pat → gcon apat1 … apatk
         …

   to ::

       pat → gcon tyapp_or_pat1 … tyapp_or_patk
           …

       tyapp_or_pat → '@' atype    -- '@' is in prefix position
                    → apat

#. Type applications in constructor patterns do *not* affect whether
   the pattern-match is successful.

#. Type applications in constructor patterns must correspond to ``forall … .``
   quantifications in the declared constructor or pattern synonym type.
   (Right now, pattern synonyms require all such quantifications to occur
   before any term arguments, but accepted proposal `#402`_ allows these
   quantifications to occur in any order in data constructors.)

#. Any type variables mentioned in a type application are considered
   binding sites, shadowing any in-scope type variables.

#. Typing follows the rules in `Type Variables in Patterns`_. In particular,
   see Figure 7, which we modify here in two ways:

   1. Ignore the ``isInternalTypeVar`` premise, which was done
      away with by accepted proposal `#128`_.

   #. Change the ``cs = ftv(τ's) \ dom(Γ)`` premise to be ``cs = ftv(τ's)``
      and ``cs # dom(Γ)``. That is, instead of making the new type variables
      ``cs`` be only those that are not already in scope, require all the
      type variables to be fresh (shadowing is possible, but left implicit
      here).

#. A wildcard ``_`` as a type argument says simply to skip that argument;
   it does not trigger any behavior associated with partial type signatures.
   In particular, ``-XPartialTypeSignatures`` is not necessary, and no
   diagnostic is produced.

#. As with term variables, it is an error to bring the same type variable
   into scope in two (or more) places within the same pattern.

Examples
~~~~~~~~

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
~~~~~~~

1. The ability to bind existential variables via a construct such as this
   is necessary to support the `Explicit Variable Principle`_.

#. The previous proposal `#126`_ followed the paper more closely, bringing into
   scope only those variables that are not already in scope. However, given that
   this behavior is triggered only by a ``@``, doing this is in violation of the
   `Visibility Orthogonality Principle`_. This newer version instead labels all variables as binding sites.

#. Having type variables have the same behavior as term variables with
   respect to shadowing (and repeated binding) upholds the `Visibility Orthogonality Principle`_. In addition,
   the fact that type variables are unconditionally brought into scope upholds
   the `Local Lexical Scoping Principle`_.

#. It may be useful to write a variable occurrence to instantiate a universal
   argument. This proposal prevents this possibility. We expect a future proposal
   to remedy this problem, with either a modifier or some symbol. For example,
   perhaps we would say e.g. ``f (Just @(*a) x) = ...`` to denote an occurrence
   of already-in-scope type variable ``a``.

#. Because ``-XScopedTypeVariables`` implies ``-XTypeAbstractions``, people using
   ``-XScopedTypeVariables`` would have access to the new features without enabling
   a new extension. This is backward-compatible with the current implementation,
   which requires both ``-XScopedTypeVariables`` and ``-XTypeApplications`` to be
   in effect. (With this proposal, ``-XScopedTypeVariables`` alone would be enough.)

Type arguments in lambda patterns
---------------------------------

This is a restatement of accepted, unimplemented proposal `#155`_, as amended by not-yet-accepted
`#238`_. It introduces the ability to bind type variables by a lambda, controlled by the
``-XTypeAbstractions`` extension.

Motivation
~~~~~~~~~~

This is adapted from `#238`_.

There are several motivating factors for this addition:

1. There are cases where a ``Proxy`` is necessary in order for a higher-rank function argument
   to access a type variable, such as::

     type family F a

     higherRankF :: (forall a. F a -> F a) -> ...

     usage = higherRankF (\ (x :: F a) -> ...)

   The ``(x :: F a)`` pattern signature does not work, because ``F`` is not injective. There
   is no way to be sure that the ``a`` in ``usage`` is meant to match the ``a`` in
   ``higherRankF``. Currently, there is simply no way for ``usage`` to get access to the
   type variable written in the signature for ``higherRankF``. This code would have to
   be rewritten to use ``Proxy``. Under this proposal, however, ``usage`` could be simply ::

     usage = higherRankF (\ @a x -> ...)

   Ah. That's better.

2. With `#126`_, we can bind type variables in constructor patterns, allowing us to easily
   capture existentials. The only other place a type variable can enter scope is in a
   function definition, and so it's only logical to extend `#126`_ to do so. Furthermore,
   doing so is necessary to uphold the `Explicit Variable Principle`_.

3. ``-XExtendedForAllScope``\'s mechanism for binding type variables using a ``forall`` in
   a signature has never sat well with some. (I'm in the some, but I'm not the only one.)
   A type signature can appear arbitrarily far away from a function definition, and
   (to me) the use of ``forall`` to induce scoping over the function definition is far
   from intuitive. Using this new syntax, all the action happens in the function
   definition. This allows for the possibility of usefully disabling ``-XExtendedForAllScope``
   while still binding type variables, helping to support the `Contiguous Scoping Principle`_.

4. See crowd-sourced example `here <https://github.com/ghc-proposals/ghc-proposals/pull/155#issuecomment-459430140>`_.

Proposed Change Specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. With ``-XTypeAbstractions``, introduce a new form of pattern (cf. The `Haskell 2010 Report`_)::

     apat → … | '@' tyvar | '@' '(' tyvar '::' kind ')' | '@' '_'   -- '@' is a prefix occurrence

   Conveniently, ``apat``\ s are used both in function left-hand sides
   and in lambda-expressions, so this change covers both use-cases.

   (Note that this does not subsume the new grammar for constructor patterns, which allow
   *types*, not just variables.)

#. A type variable pattern is not allowed in the following contexts:

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
      as we have a type signature to guide us. *Exception:* The number
      of type variables bound after all term patterns must be the same
      for all equations; discussion below.

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

#. ``-XTypeAbstractions`` and ``-XExtendedForAllScope`` have a fraught relationship,
   as both are trying to accomplish the same goal via different means. Here are
   the rules keeping this sibling rivalry at bay:

   1. ``-XExtendedForAllScope`` does not apply in expression type signatures. Instead,
      if users want a type variable brought into scope, they are encouraged to
      use ``-XTypeAbstractions``. (It would not be hard to introduce a helpful
      error message instructing users to do this.)

   #. If ``-XExtendedForAllScope`` is enabled,
      in an equation for a function definition for a function ``f`` (and similar
      for pattern synonym pattern bindings and pattern synonym expression bindings):

      * If ``f`` is written with no arguments or its first argument is not
        a type argument (that is, the next token after ``f``
        is not a prefix ``@``), then ``-XExtendedForAllScope`` is in effect and
        brings type variables into scope.

      * Otherwise, if ``f``\'s first argument is a type argument, then
        ``-XExtendedForAllScope`` has no effect. No additional type variables
        are brought into scope.

Examples of new behavior of scoped type variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   f :: forall a. a -> a
   f @b x = (x :: a)   -- rejected, because -XExtendedForAllScope is disabled here

   g :: forall a. a -> a
   g @a x = (x :: a)   -- accepted with -XTypeAbstractions

   h = ((\x -> (x :: a)) :: forall a. a -> a)
     -- accepted with previous -XScopedTypeVariables, but rejected
     -- now

   i = ((\ @a x -> (x :: a)) :: forall a. a -> a)
     -- accepted with -XTypeAbstractions

Note that turning off ``-XExtendedForAllScope`` with ``-XTypeAbstractions`` is necessary if we
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

Further examples
~~~~~~~~~~~~~~~~

Here are two real-world examples of how this will help, courtesy of @int-index:

1. It would be useful to eliminate ``Proxy`` in this style of proof::

     class WithSpine xs where
       onSpine ::
         forall r.
         Proxy xs ->
         ((xs ~ '[]) => r) ->
         (forall y ys.
           (xs ~ (y : ys)) =>
           WithSpine ys =>
           Proxy y ->
           Proxy ys ->
           r) ->
         r

   Code taken `from here <https://github.com/int-index/caps/blob/2f46fc6d5480bdef0a17f64359ad6eb29510dba4/src/Monad/Capabilities.hs#L273>`_.

   Compare:

   a. ``@``\-style: ``withSpine @xs (onNil ...) (\ @y @ys -> onCons ...)``
   b. ``Proxy``\-style: ``withSpine (Proxy :: Proxy xs) (onNil ...) (\(Proxy :: Proxy y) (Proxy :: Proxy ys) -> onCons ...)``

2. From `reflection <https://hackage.haskell.org/package/reflection-2.1.4/docs/Data-Reflection.html#v:reify>`_::

     reify :: forall a r. a -> (forall (s :: *). Reifies s a => Proxy s -> r) -> r

   Compare:

   a. ``@``\-style: ``reify (\ @s -> ...)``
   b. ``Proxy``\-style: ``reify (\(Proxy :: Proxy s) -> ...)``

Examples of varying type variables among equations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. _varying-type-lambda-examples:

::

     f1 @a (x :: a) = x    -- accepted

     f2 @a True  x (y :: a) = x
     f2 @_ False x y        = y   -- accepted

     f3 @a True  x (y :: a) = x
     f3    False x y        = y   -- rejected: too confusing to have different type variable bindings

     f4 :: Bool -> a -> a -> a
     f4 @a True  x (y :: a) = x
     f4    False x y        = y   -- accepted: the type signature allows us to do this

     f5 :: Bool -> forall a. a -> a -> a
     f5 True @a x (y :: a) = x
     f5 False   x y        = y    -- accepted

     f6 :: Bool -> forall a. a -> a -> a
     f6 True  @a = const @a @a
     f6 False @_ = flip const     -- accepted: the type variables after term variables line up

     f7 :: Bool -> forall a. a -> a -> a
     f7 True  @a = const @a @a
     f7 False    = flip const     -- rejected: variable tail of type variables

Effects
~~~~~~~

1. An astute reader will note that I put spaces after all my lambdas. That is because
   ``\@`` is a valid name for a user-defined operator. This proposal does not change that.
   If you want to bind a type variable in a lambda, you must separate the ``\`` from the
   ``@``.

#. This proposal makes abstracting over type variables the dual of applying types with
   visible type application.

#. Accepted proposal `#99`_ introduces the possibility of user-written
   specificity annotations (``forall {k} ...``). An *inferred* variable, including one
   written by the programmer using this new notation, is not available for use with
   any form of visible type application, including the one proposed here. If you have
   a function ``f :: forall {k} (a :: k). ...``, you will have to rely on the behavior
   of ``-XExtendedForAllScope`` to bring ``k`` into scope in ``f``\'s definition, or
   you will have to use a pattern signature. This is
   regrettable but seems an inevitable consequence of the ``{k}`` notation.

#. This delivers the `Explicit Variable Principle`_, meaning we can rid of ``Proxy``.

#. The `last set of examples <#varying-type-lambda-examples>`_ above show how we deal
   with functions with multiple equations with varying type variable bindings.

   No variation
   is allowed when there is no type signature, as doing so seems challenging (though possible),
   and we can just encourage a type signature.

   With a type signature, variation is allowed (example ``f4``, with one exception: the
   tail of arguments must be consistent. The reason for this restriction can be understood
   in thinking about ``f7``: in the right-hand side of the second equation, is the expected
   type ``forall a. a -> a -> a`` or ``a -> a -> a``, with ``a`` already bound? This choice
   matters: perhaps the right-hand side is ``\ @a -> flip (const @a @a)``. Or, if we have
   a type like ``Bool -> forall a b. ...``, are both ``a`` and ``b`` bound to the left of the
   ``=``? We could, for example, look at all equations and bind a number of variables equal
   to the maximum number of type variables across all equations. But re-consider ``f7``
   again: if we just wrote the second equation without the first, that would have a different
   meaning than writing the equation along with the first. That is, we might imagine this
   being accepted::

     f7' :: Bool -> forall a. a -> a -> a
     f7' False = \ @a -> flip (const @a @a)

   but this being rejected as ill-typed::

     f7'' :: Bool -> forall a. a -> a -> a
     f7'' False   = \ @a -> flip (const @a @a)
     f7'' True @a = const @a @a

   This is strange, where the addition of a new equation violates the typing of a previous
   one (that was otherwise fine). To avoid this strangeness, we simply forbid varying
   the number of bound variables in the tail.

   Note that we do not want to forbid binding variables in the tail generally, because
   someone might want ::

     myId :: forall a. a -> a
     myId @a = id @a

   which binds a variable in the tail. Happily, definitions like this will have only one
   equation.

#. (technical) The `Visible Type Applications`_ (VTA) paper defines the behavior about what to
   do when checking against a polytype: it says to deeply skolemize. However, eager deep
   skolemization will spell trouble for this extension, as we need the lambdas to see
   the ``forall``\s. The end of the Section 6.1 in the `extended VTA <https://richarde.dev/papers/2016/type-app/visible-type-app-extended.pdf>`_ paper discusses
   why we do eager deep skolemization: essentially, the alternative would be to do
   type generalization at inflection points between checking and inference mode,
   right before doing the subsumption check. Type generalization is hard in GHC, though,
   and so the paper avoided it. In order to implement this proposal, we'll have to work
   out how to do this.

Costs and Drawbacks
~~~~~~~~~~~~~~~~~~~

1. This part of the proposal
   is *not* backward-compatible with today's ``-XScopedTypeVariables``,
   because it rejects expressions like ::

     ((\x -> (x :: a)) :: forall a. a -> a)

   which are accepted today. No migration period is proposed, because it is
   very hard to imagine how ``-XTypeAbstractions`` and ``-XExtendedForAllScope`` should
   co-exist peacefully here. Instead, we can issue a specific error message telling
   users how to migrate their code in this case.

   My hope is that constructs such as this one are rare and would not impact many
   users.

   If necessary, we could imagine taking the expression ``expr :: forall ... . ty``
   and looking proactively to see whether ``expr`` ever uses a type variable
   pattern from this proposal. If not, ``-XExtendedForAllScope`` could trigger (and we
   issue a warning with ``-Wcompat``). But, if a type argument appears anywhere
   in ``expr``, then ``-XExtendedForAllScope`` is disabled. This would be backward-compatible,
   but unfortunately non-local and annoying. I prefer just to skip this
   migration step.

Alternatives
~~~~~~~~~~~~

1. We could add the following specification item if we like:

   **Specification**

   If ``-XTypeAbstractions`` is in effect, then a function
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
   (where it brings into scope both universals and existentials). (In an implicitly
   bidirectional pattern synonym, the ``@(..)`` brings into scope only universals.)

   **Discussion**

   This new notation seems like a convenient middle ground,
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

   A ``@(..)`` argument counts as a type argument when asking whether ``-XExtendedForAllScope``
   affects a function equation.

   The new ``@(..)`` notation does *not* work with expression type signatures,
   lambda-expressions, or anywhere other than a function binding with a type
   signature. This is because doing so would require propagating type
   information into scoping, which is problematic.

   Some have argued on GitHub that it may be best to hold off the ``@(..)`` until
   we gain more experience here: adding new features is easier than removing them.
   While I agree that this could be done, the ``@(..)`` construct makes for a very
   easy migration from today's ``-XScopedTypeVariables`` and is thus tempting to
   be around from the start. I don't feel strongly but would personally vote for
   inclusion.

#. We could simply make ``-XExtendedForAllScope`` and ``-XTypeAbstractions`` incompatible.
   If the user specifies both, reject the program.

   I find this approach less convenient, as it prevents an easy migration from the
   status quo (with ``-XScopedTypeVariables`` enabled often, including in ``-XGHC2021``)
   to a future relying more on ``-XTypeAbstractions``. The approach described in this
   proposal means that enabling ``-XTypeAbstractions`` affects nothing about ``-XExtendedForAllScope``,
   until a user tries to actually use a type abstraction. That's a nice property.

``let``-binding types
---------------------

.. _type-let:

This segment of the proposal goes beyond previous proposals in describing a mechanism
to use ``let`` to bind type synonyms.

Motivation
~~~~~~~~~~

1. Users have, from time to time, requested the ability to make local type synonyms.
   GHC even has a little support for synonyms via equality constraints (e.g., writing
   ``f :: (a ~ Some Big Type With Lots Of Parts) => Maybe a -> a -> Maybe a``). Instead
   of encoding this idea via equality constraints, though, it would be nice to support
   it directly.

#. Type variables can stand for types, and so we can write code like ::

     f :: Maybe Bool -> Bool
     f (x :: Maybe b) = (True :: b)

   Note that the pattern signature binds ``b`` to ``Bool``. This is, essentially, a ``let``\ -bound
   type variable: in the scope of ``b``, ``b`` is synonymous with ``Bool``. Yet the only way
   to make such a ``b`` is via a pattern (or result, `#228`_) signature. Why force users
   to use matching instead of binding the variable directly?

Proposed Change Specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Create a new extension ``-XExtendedLet``.

#. With ``-XExtendedLet``, add two new productions for ``decl`` (from the `Haskell 2010 Report`_), ::

     decl → 'type' simpletype '=' type
          → 'type' tyvar '=' type

   and remove the (now redundant) production ``topdecl → 'type' simpletype '=' type`` from ``topdecl``.

   Note that the second form allows a local binding for a lower-case ``tyvar``; these
   synonyms may not be parameterized.

#. The form ``decl → 'type' tyvar '=' type`` is not allowed at top-level.

#. These new declaration forms introduce local type synonyms in terms, which scope over the same
   region of code that other declarations in the same ``let`` / ``where`` clause scope over.

   Semantically, these type synonyms are just shorthand for their right-hand sides. They can
   always be eagerly expanded. Accordingly, and like other type synonyms, local type synonyms
   may not be recursive.

#. Wildcards are allowed in the right-hand side of local synonyms. At usage sites of the
   synonym, the synonym is expanded. It is an error if that location does not allow wildcards.
   The wildcard is understood to stand for just one type shared among all the expansions.

Effects
~~~~~~~

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
-------------------

This part of this proposal allows introducing a ``let``\ -binding in a pattern.
The bound variable(s) scope over the same region of code as the pattern-bound
variables do.

The syntax for this feature is a bit awkward, and so this proposal presents
two alternatives for discussion. I have a slight preference for Alternative 2.

This goes beyond any previous proposal.

Motivation
~~~~~~~~~~

1. This is needed to uphold the `Explicit Binding Principle`_.

#. Though admittedly a weakish motivation, there is currently no way to share
   expressions used in common in multiple view patterns. See `examples <#let-in-pattern-example>`_
   below.

Specification Alternative 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. With ``-XExtendedLet``, add a new form of pattern as follows::

     pat → 'let' decls 'in' pat

#. Any entites bound in ``decls`` scope over the same region of the program
   that pattern-bound variables scope over, with the addition of the ``decls``
   themselves (that is, the declarations can be recursive).

Specification Alternative 2
~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Examples
~~~~~~~~

.. _let-in-pattern-example:

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
   site for ``b``, in accordance with the `Explicit Binding Principle`_.

#. Instead of ::

     f x y z (frob x y z -> True) (frob x y z -> False) = ...

   we can write (Alternative 1) ::

     f x y z (let test = frob x y z in (test -> True)) (test -> False) = ...

   or (Alternative 2) ::

     f x y z (let test = frob x y z) (test -> True) (test -> False) = ...

   avoiding some repetition.

Effects
~~~~~~~

1. In concert with binding type variables in a ``let``, this helps uphold the `Explicit Binding Principle`_.
   Without this feature, a line such as ``f (x :: Either b b) = ...``
   has no binding site for ``b``. (Alternatively, we could say that the first ``b`` is a binding site,
   but then we lose the `Local Lexical Scoping Principle`_, as the binding-site-vs-occurrence distinction depends on what is in scope.)

   The ability to bind variables that would otherwise be pattern-bound is why this feature
   allows binding type variables.

``let`` in types
----------------

This part of the proposal allows ``let`` to be used in types. This part goes
beyond any previous proposal.

Motivation
~~~~~~~~~~

1. The careful reader will notes that the `secction above <#type-let>`_ defining
   the ability to bind type synonyms in ``let`` expressions does not actually address
   a motivating example. This component of this proposal allows us to avoid repetition
   within a type signature.

Proposed Change Specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
~~~~~~~~

1. Instead of ::

     f :: forall a b. (c ~ Very Big Type a b) => c -> c -> c

   we can write ::

     f :: forall a b. let c = Very Big Type a b in c -> c -> c

   which more directly expresses what we mean.

Effects
~~~~~~~

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
I summarize the effects on the principles_.

1. The `Lexical Scoping Principle`_, part (a), is upheld. Binders occur in patterns, after ``forall``, in
   ``let`` declarations, and a few other discrete places in the AST -- and
   nowhere else. In particular, binders do not occur in pattern signatures.
   Instead, with ``-XPatternSignatureBinds``, an occurrence of an out-of-scope
   variable ``a`` induces a ``let type a = _ in`` to be prefixed to the pattern.

#. The `Local Lexical Scoping Principle`_ is made to hold, by describing pattern-signature binds as occurrences
   and making type applications in patterns unconditionally bring new variables
   into scope.

#. The `Syntactic Unification Principle`_ is supported. The new ``let`` syntax in types is a strict subset
   of its syntax in terms, and the semantics are compatible.

#. The `Explicit Variable Principle`_ is made to hold, by allowing explicit binders for type variables
   for existentials and the variables bound by an inner ``forall`` in a higher-rank
   type.

#. The `Explicit Binding Principle`_ is made to hold, by introducing ``-XNoImplicitForAll`` and
   ``-XNoPatternSignatureBinds``.

#. The `Visibility Orthogonality Principle`_ is made to hold, by ensuring that types and terms are treated identically
   in patterns.

#. The `Pattern/Expression Duality Principle`_ is respected, by allowing space for universals in patterns. It is up
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
