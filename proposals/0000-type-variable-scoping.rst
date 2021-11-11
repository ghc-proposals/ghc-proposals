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

   Put another way: two programs that are the same except for visibility markers (such as
   the ``.`` vs ``->`` in a ``forall`` or the presence or absence of a ``@``) should desugar
   to the same Core program.

   Motivation: Visibility should be just that: a superficial property that describes
   (only) whether an argument is visible in the user-written source code.

   Currently, the design for `#281`_ (along with the design for ``-XTypeApplications``)
   violates the VOP_, because the visibility marker ``@`` also affects the difference between
   term-syntax and type-syntax. However, given the SUP_, we strive to uphold the VOP_ when
   there is an absence of punning.

   .. _PEDP:

#. **Pattern/Expression Duality Principle (PEDP)**. The syntax for patterns mimics
   that of expressions, allowing an expression headed by a constructor to be pattern-matched
   against a pattern of the same syntactic structure.

   Motivation: This is the essence of pattern-matching, where we can deconstruct data
   that was constructed by an expression.

   .. _CSP:

#. **Contiguous Scoping Principle (CSP)**. The region of a program for which an identifier
   is in scope is contiguous.

   Motivation: The CSP_ makes programs easier to read, in that a reader can add a variable
   to their internal tracking of in-scope variables then
   remove that variable from their in-scope set just once.

   The CSP is *not* respected by Haskell 2010 nor some of GHC's extensions. Here are some places
   where the CSP is violated:

   1. ``do``\ -notation. Example: ``do (x, (f x -> Pat)) <- action; blah``. ``x`` is in scope in
      its pattern, to the right of its binding site, but then not in ``action``. It is in scope
      again in ``blah``. Example of potential confusion: ``f x = do x <- x; g x``.

   #. List comrephensions. Example: ``[ (x, y) | x <- thing1, y <- thing2, condition3 ]``. The
      variable ``y`` is in scope in ``condition3`` and the ``(x, y)`` at the
      beginning, but nowhere else. Example of potential confusion:
      ``f x y = [ (x, y) | x <- y, y <- x ]``.

   #. Arrow notation. Example: ``proc x -> do y <- task1 -< input1; task2 -< input2``. The variable
      ``x`` is in scope in ``input1`` and ``input2`` but not in ``task1`` or ``task2``.
      Example of potential confusion: ``f x = proc x -> x -< x``. The two ``x``\ s at the end
      refer to *different* variables.

   #. ``-XScopedTypeVariables``. Example: ``f :: forall a. a -> a; x :: Int; f y = (y :: a)``. The
      type variable ``a`` is in scope in the definition of ``f`` but not in
      the type signature for ``x``.

   #. GADT header variables. Example of potential confusion:
      ``data G a where MkG :: a Int -> G Bool deriving C a``. The ``a`` in the type of ``MkG`` is
      completely unrelated to the ``a`` toward the beginning and in the deriving
      clause.

   There may be others beyond this. The goal here is *not* to establish the CSP_ (indeed, I'm not
   sure I'd want ``do`` notation to be any different here, because reusing variable names in ``do``
   notation is convenient), but to get one step closer to it for those programmers who want it.

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

   Separating out ``-XScopedForAlls`` gets us closer to the CSP_.

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

Type arguments in constructor patterns
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. _pattern-type-args:

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
^^^^^^^^

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

#. The previous proposal `#126`_ followed the paper more closely, bringing into
   scope only those variables that are not already in scope. However, given that
   this behavior is triggered only by a ``@``, doing this is in violation of the
   VOP_. This newer version instead labels all variables as binding sites.

#. Having type variables have the same behavior as term variables with
   respect to shadowing (and repeated binding) upholds the VOP_. In addition,
   the fact that type variables are unconditionally brought into scope upholds
   the LLSP_.

#. It may be useful to write a variable occurrence to instantiate a universal
   argument. This proposal prevents this possibility. We expect a future proposal
   to remedy this problem, with either a modifier or some symbol. For example,
   perhaps we would say e.g. ``f (Just @(*a) x) = ...`` to denote an occurrence
   of already-in-scope type variable ``a``.

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

``let``-binding types
~~~~~~~~~~~~~~~~~~~~~

.. _type-let:

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

Examples
^^^^^^^^

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

1. The careful reader will notes that the `secction above <#type-let>`_ defining
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