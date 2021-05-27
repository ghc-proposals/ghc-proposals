Binding type variables in lambda-expressions
============================================

.. sectnum::
.. author:: Richard Eisenberg
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/155>`_
            and is being `amended at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/238>`_.
.. contents::

.. _`#126`: https://github.com/ghc-proposals/ghc-proposals/pull/126
.. _`#128`: https://github.com/ghc-proposals/ghc-proposals/pull/128
.. _`#119`: https://github.com/ghc-proposals/ghc-proposals/pull/119
.. _`Haskell 2010 Report`: https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5
.. _`#285`: https://github.com/ghc-proposals/ghc-proposals/pull/285

Proposal `#126`_ allows us to bind scoped type variables in patterns using an ``@a`` syntax.
However, the new syntax is allowed only in *constructor* patterns ``K @a @b x y``. This proposal
extends this idea to lambda-expressions, allowing ``\ @a x -> ...``. Here are some examples::

  id :: a -> a
  id @a x = (x :: a)                      -- no forall needed

  prefix :: a -> [[a]] -> [[a]]
  prefix @a x yss = map xcons yss
    where xcons :: [a] -> [a]             -- this signature is impossible to write without ScopedTypeVariables
          xcons ys = x : ys

  const :: a -> b -> a
  const @c x _ = (x :: c)                 -- names can change; you do not have to bind every type variable

  fconst :: a -> b -> b
  fconst @_ @d _ x = (x :: d)             -- order matters

  pair :: forall a. a -> (a, a)           -- no longer brings a into scope
  pair @b x = (x :: b, x :: b)            -- brings b into scope
  
  higherRank :: (forall a. a -> a -> a) -> ...
  higherRank = ...

  ex = higherRank (\ @a x _y -> (x :: a)) -- it works in a higher-rank situation, too

  wrong @a x = x                          -- we can't do this without a type signature

  stillWrong @a (x :: a) = x              -- even here we can't

In addition, this proposal splits up the current ``-XScopedTypeVariables`` extension into
component pieces which may be mixed and matched.
  
Motivation
----------

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
   function definition, and so it's only logical to extend `#126`_ to do so.

3. ``ScopedTypeVariables``\'s mechanism for binding type variables using a ``forall`` in
   a signature has never sat well with some. (I'm in the some, but I'm not the only one.)
   A type signature can appear arbitrarily far away from a function definition, and
   (to me) the use of ``forall`` to induce scoping over the function definition is far
   from intuitive. Using this new syntax, all the action happens in the function
   definition.

4. See crowd-sourced example `here <https://github.com/ghc-proposals/ghc-proposals/pull/155#issuecomment-459430140>`_.

5. The behavior of ``ScopedTypeVariables`` appears incompatible with the
   ability to bind type variables using a ``@a`` pattern. (What would ``(\ @a
   -> expr) :: forall b. ty`` mean? What would its desugaring be? What's the
   relationship between ``a`` and ``b``?)
   This proposal thus reshuffles the meaning of a number of extensions
   in a backward-compatible way. Along the way, this proposal satisfies the
   motivations behind `#119`_, which was rejected for insufficient merit,
   yet has continued to look attractive from time to time.

6. John Ericson has also `provided <https://github.com/ghc-proposals/ghc-proposals/pull/238#issuecomment-824134181>`_
   some relevant motivation for splitting up
   ``-XScopedTypeVariables``:

       To me the motivation is that, with ``-XTypeAbstractions``, ``-XScopedForAlls``
       and ``-XPatternSignatureBinds`` become arguable misfeatures with replacements,
       but ``-XMethodTypeVariables`` is (perhaps unfortunately) still needed. It would
       be possible to skip ``-XScopedForAlls`` and have that just be the remaining
       core feature of ``-XScopedTypeVariables``, but I do sort of
       like ``-XScopedTypeVariables`` just enabling other features, so we have more
       decomposition into composable parts. I think that might help with teaching.

       ``-XPatternSignatures`` is also nice in order to separate out the least
       controversial part of all of this. It's a knob we don't need only because I
       very much hope it can uncontroversial become part of the next version of
       Haskell (likewise ``-XExplicitForAll``) while the rest of this stuff would
       probably would get more haggling.
   
Proposed Change Specification
-----------------------------

There are two parts of this proposal: a refactoring of extensions around binding
type variables, and a new construct for binding type variables in function definitions
(both in function binds and in anonymous functions). While they are separable,
I think they make a nice whole. We could choose to do either part, or both together.

Refactoring extensions around type variable binding
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

1. Introduce ``-XPatternSignatures``. With ``-XPatternSignatures``, we would
   allow type signatures in patterns. These signatures could mention in-scope
   type variables as variable occurrences, but could not bind type variables.

2. Introduce ``-XPatternSignatureBinds``. With ``-XPatternSignatureBinds``, any
   out-of-scope type variables written in a pattern signature would be bound there
   and would remain in scope over the
   same region of code that term-level variables introduced in a pattern scope
   over. This extension is off by default.
   (This extension is a part of accepted, unimplemented proposal
   `#285`_; the only change is that this proposal makes it off by default.)

3. Introduce ``-XMethodTypeVariables``. With ``-XMethodTypeVariables``, type
   variables introduced in an instance head would scope over the bodies of
   method implementations. Additionally, type variables introduced in a class
   head would scope over the bodies of method defaults.

4. Introduce ``-XScopedForAlls``. With ``-XScopedForAlls``, any type variables
   mentioned in an explicit ``forall`` scopes over an expression. This applies
   to the following constructs:
   
   * Function bindings
   * Pattern synonym bindings (including in any ``where`` clause)
   * Expression type signatures

5. The extension ``-XScopedTypeVariables`` would imply all of the above
   extensions; this way, ``-XScopedTypeVariables`` does not change from its
   current meaning.

Type abstractions
^^^^^^^^^^^^^^^^^
   
Introduce ``-XTypeAbstractions``. With ``-XTypeAbstractions``, users
could write a pattern like ``@a`` to the left of the ``=`` in a function
binding or between the ``\`` and ``->`` in a lambda-expression. 

A. Here is the BNF of the new form (cf. The `Haskell 2010 Report`_)::

     apat ::= ... | '@' tyvar

   Conveniently, ``apat``\ s are used both in function left-hand sides
   and in lambda-expressions, so this change covers both use-cases.

   In the BNF, we assume the ``@`` is a `*prefix occurrence* <https://github.com/ghc-proposals/ghc-proposals/blob/79f48248ae31b1a490deb1b019c206efa0be89da/proposals/0229-whitespace-bang-patterns.rst#id3>`_.

B. A type variable pattern would not be allowed in the following contexts:

   i. To the right of an as-pattern
   ii. As the top node in a lazy (``~``) pattern
   iii. As the top node in a ``lpat`` (that is, to the left of an infix
        constructor, directly inside a parenthesis, as a component of
        a tuple, as a component of a list, or directly after an ``=``
        in a record pattern)

C. Typing rules for the new construct are as in a `recent paper
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

   i. In synthesis mode, when examining ``\ @a -> expr``, we simply put
      ``a`` in scope as a fresh skolem variable (that is, not equal
      to any other type) and then check ``expr``. (Presumably, ``expr``
      uses ``a`` in a type signature.) When we infer that ``expr`` has
      type ``ty``, the expression ``\ @a -> expr`` has type ``forall a. ty``.
      Example: ``\ @a (x :: a) -> x`` infers the type ``forall a. a -> a``.
      (For this example, we note that ``\ @a (x :: a) -> x`` is a short-hand
      for ``\ @a -> \ (x :: a) -> x``.)

   ii. In checking mode, when examining ``\ @a -> expr`` against type ``ty``,
       we require that ``ty`` has the shape ``forall a. ty'``, where
       ``a`` is a *specified* variable (possibly
       after skolemising any *inferred* variables in ``ty``), renaming the
       bound variable as necessary to match the name used in the expression.
       We then check ``expr`` against type ``ty'``.

   iii. In synthesis mode, when examining a function argument ``@a`` to
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

   iv. In checking mode, when examining a function argument ``@a`` to
       a function ``f`` with type signature ``ty``, we require the corresponding
       spot in the type signature to have a ``forall a`` (possibly renaming
       the bound variable). The type variable ``a`` is then brought
       into scope and we continue checking arguments and the right-hand side.

       Multiple equations can bind type variables in different places,
       as we have a type signature to guide us.

#. Typing rules for pattern synonym bindings are complicated, as usual.

   i. A visible type abstraction in a pattern synonym binding that lacks
      a type signature is rejected. (While we could, at some cost, work
      out what should happen here, please just use a type signature.)

   ii. (Background information; no new specification here.)
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
       
   iii. With ``-XTypeAbstractions``, a pattern synonym pattern binding may
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

   iv. With ``-XTypeAbstractions``, a pattern synonym expression binding
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

   i. ``-XScopedForAlls`` does not apply in expression type signatures. Instead,
      if users want a type variable brought into scope, they are encouraged to
      use ``-XTypeAbstractions``. (It would not be hard to introduce a helpful
      error message instructing users to do this.)

   ii. If ``-XScopedForAlls`` is enabled,
       in an equation for a function definition for a function ``f`` (and similar
       for pattern synonym pattern bindings and pattern synonym expression bindings):

       * If ``f`` is written with no arguments or its first argument is not
         a type argument (that is, the next token after ``f``
         is not a prefix ``@``), then ``-XScopedForAlls`` is in effect and
         brings type variables into scope.

       * Otherwise, if ``f``\'s first argument is a type argument, then
         ``-XScopedForAlls`` has no effect. No additional type variables
         are brought into scope.

E. (Optional extra) If ``-XTypeAbstractions`` is in effect, then a function
   binding may use ``@(..)`` on its left-hand side. Here is the BNF (cf. the
   `Haskell 2010 Report <https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-800004.4>`_, Section 4.4.3), recalling that braces mean "0 or more"::

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
   and is additionally avaialable in pattern synonym pattern bindings (where it
   brings into scope only universals) and pattern synonym expression bindings
   (where is brings into scope both universals and existentials). (In an implicitly
   bidirectional pattern synonym, the ``@(..)`` brings into scope only universals.)

Examples of new behavior of scoped type variables
-------------------------------------------------

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
     
Motivating Examples
-------------------

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

Effect and Interactions
-----------------------

* An astute reader will note that I put spaces after all my lambdas. That is because
  ``\@`` is a valid name for a user-defined operator. This proposal does not change that.
  If you want to bind a type variable in a lambda, you must separate the ``\`` from the
  ``@``.

* This proposal makes abstracting over type variables the dual of applying types with
  visible type application.

* This proposal is meant to dovetail nicely with other recent proposals in this space
  (`#126`_, `#128`_), but all the proposals are orthogonal. Any can usefully be accepted
  without the others.

* Accepted proposal `#99`_ introduces the possibility of user-written
  specificity annotations (``forall {k} ...``). An *inferred* variable, including one
  written by the programmer using this new notation, is not available for use with
  any form of visible type application, including the one proposed here. If you have
  a function ``f :: forall {k} (a :: k). ...``, you will have to rely on the old behavior
  of ``-XScopedTypeVariables`` to bring ``k`` into scope in ``f``\'s definition, or
  you will have to use a pattern signature. This is
  regrettable but seems an inevitable consequence of the ``{k}`` notation.

.. _`#99`: https://github.com/ghc-proposals/ghc-proposals/pull/99

* This proposal dovetails with accepted proposal `#285`_ in its use of ``-XPatternSignatureBinds``.
  The goal of `#285`_ is to allow the user to more carefully control whether type variables
  may be brought into scope "automatically". This proposal broadly continues in that tradition
  by giving users finer-grained control of which features they wish to enable.

* (technical) The `Visible Type Applications`_ (VTA) paper defines the behavior about what to
  do when checking against a polytype: it says to deeply skolemize. However, eager deep
  skolemization will spell trouble for this extension, as we need the lambdas to see
  the ``forall``\s. The end of the Section 6.1 in the `extended VTA <https://cs.brynmawr.edu/~rae/papers/2016/type-app/visible-type-app-extended.pdf>`_ paper discusses
  why we do eager deep skolemization: essentially, the alternative would be to do
  type generalization at inflection points between checking and inference mode,
  right before doing the subsumption check. Type generalization is hard in GHC, though,
  and so the paper avoided it. In order to implement this proposal, we'll have to work
  out how to do this.

* The optional extra ``@(..)`` notation seems like a convenient middle ground,
  allowing for an easy transition from the old-style ``-XScopedTypeVariables``
  to the newer ``-XTypeAbstractions``. It brings the *inferred* variables (from `#99`_)
  into
  scope, quite conveniently. This new notation also allows type variables to
  be brought into scope without the ``forall`` keyword in the type, in case
  the user does not want to trigger ``forall``\ -or-nothing behavior.

  Note that this notation is forward compatible with visible dependent quantification
  in terms::

    f :: foreach (count :: Int) (label :: String) (is_paid_for :: Bool) -> Invoice
    f (..) = -- here, count, label, and is_pair_for are all in scope

  This style allows for more perspicuous types while avoiding redundancy. The particular
  example here uses ``foreach`` to denote arguments that are available at runtime, but
  nothing about ``foreach`` is required to make this all work (as far as scoping is
  concerned).

  Accepting the ``@(..)`` syntax does *not* entail accepting this new, separate
  ``(..)`` syntax, though it is good to know that the idea is forward compatible
  with.

  A ``@(..)`` argument counts as a type argument when asking whether ``-XScopedForAlls``
  affects a function equation.

  The new ``@(..)`` notation does *not* work with expression type signatures,
  lambda-expressions, or anywhere other than a function binding with a type
  signature. This is because doing so would require propagating type
  information into scoping, which is problematic.
  
Costs and Drawbacks
-------------------
* The new small zoo of flags is yet more flags to be aware of. It is possible
  to do away with this part of the proposal if the cost appears to outweigh
  the benefit. (Earlier versions of this proposal had ``-XTypeAbstractions``
  imply ``-XNoScopedForAlls``. In any case, this historical fact is why these
  two ideas are bundled in one proposal. They could be separated, but I want
  both parts, so I've kept them both here.)

* Note that the second part of this proposal (introducing ``-XTypeAbstractions``)
  is *not* backward-compatible, because it rejects expressions like ::

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

* ``-XTypeAbstractions`` is another feature to specify and maintain, and
  that's always a burden. It will take some creative thought about how to do
  generalization properly (last point in previous section), but I don't
  actually think the code will be all that challenging there.

* There is a potential confusion with as-patterns.

Alternatives
------------
* If we want to bind type variables in lambda-expressions, I think this is the
  only way to do it. We don't have to, of course, but then there will still be
  one area in GHC/Haskell that requires ``Proxy``, and that's unfortunate.

* We do not have to split up all the extensions as described here, but I think
  it makes the design cleaner.

Unresolved questions
--------------------
* None at this time.

Implementation Plan
-------------------
I'm happy to advise and support a volunteer who wishes to implement. I might do it myself
or work with an intern on this someday, as well.
