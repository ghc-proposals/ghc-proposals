Modern Scoped Type Variables
============================

.. sectnum::
.. author:: Richard Eisenberg
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

This proposal updates the treatment of scoped type variables in GHC, tying
together many existing proposals:

.. _`#119`: https://github.com/ghc-proposals/ghc-proposals/pull/119
.. _`#126`: https://github.com/ghc-proposals/ghc-proposals/pull/126
.. _`#155`: https://github.com/ghc-proposals/ghc-proposals/pull/155
.. _`#238`: https://github.com/ghc-proposals/ghc-proposals/pull/238
.. _`#281`: https://github.com/ghc-proposals/ghc-proposals/pull/281
.. _`#285`: https://github.com/ghc-proposals/ghc-proposals/pull/285
.. _`#291`: https://github.com/ghc-proposals/ghc-proposals/pull/291
.. _`#378`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst
.. _`#402`: https://github.com/ghc-proposals/ghc-proposals/pull/402
.. _`#420`: https://github.com/ghc-proposals/ghc-proposals/pull/420
.. _Type Variables in Patterns: https://richarde.dev/papers/2018/pat-tyvars/pat-tyvars.pdf
.. _Kind Inference for Datatypes: https://richarde.dev/papers/2020/kind-inference/kind-inference.pdf

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

* **Lexical Scoping Principle (LSP)**. For every occurrence of an identifier, it is possible to uniquely identify its binding site, without involving the type system. (From `#378`_.)

  The LSP implies

.. _LSPC:

  **Lexical Scoping Principle Corollary (LSPC)**. For every appearance of an identifier
  in a Haskell program, it is possible to determine whether that appearance is a
  binding site or an occurrence, without involving the type system.

  Motivation: These principles mean that we can understand the binding
  structure of a program without relying on type inference, important both for the
  implementation of GHC and the sanity of programmers.

.. _SUP:

* **Syntactic Unification Principle (SUP)**. In the absence of punning, there is no difference between type-syntax and term-syntax.
  (From `#378`_.)

  Motivation: The SUP keeps us forward-compatible with a possible future where the
  distinction between term-syntax and type-syntax is removed.

.. _EVP:

* **Explicit Variable Principle (EVP)**. It is possible to write out all
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

* **Explicit Binding Principle (EBP)**. Through the right combination of extensions and/or warning flags, it is possible
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

* **Visibility Orthogonality Principle (VOP)**. Whether an argument is visible or
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

* **Pattern/Expression Duality Principle (PEDP)**. The syntax for patterns mimics
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
and occurrence is not in violation of the LSPC_.

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

1. Introduce ``-XPatternSignatures``. With ``-XPatternSignatures``, we
   allow type signatures in patterns. These signatures can mention in-scope
   type variables as variable occurrences, but can not bind type variables.

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

.. _gadt-syntax:

GADT syntax to distinguish universals and existentials
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This component of this proposal revises the scoping of variables in GADT declaration syntax.

Motivation
^^^^^^^^^^

1. This new version makes the distinction between universals and existentials in constructors
   very clear, as required by the analysis `above <#universals-and-existentials>`_.

#. Principled kind inference is, I believe, impossible using the current syntax. This is argued
   in `Appendix B.8 <https://richarde.dev/papers/2020/kind-inference/kind-inference-supplement.pdf#subsection.B.8>`_ of the `Kind Inference for Datatypes`_ paper.

Specify the change in precise, comprehensive yet concise language. Avoid words
like "should" or "could". Strive for a complete definition. Your specification
may include,

* BNF grammar and semantics of any new syntactic constructs
  (Use the `Haskell 2010 Report <https://www.haskell.org/onlinereport/haskell2010/>`_ or GHC's ``alex``\- or ``happy``\-formatted files
  for the `lexer <https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Parser/Lexer.x>`_ or `parser <https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Parser.y>`_
  for a good starting point.)
* the types and semantics of any new library interfaces
* how the proposed change interacts with existing language or compiler
  features, in case that is otherwise ambiguous

Strive for *precision*. The ideal specification is described as a
modification of the `Haskell 2010 report
<https://www.haskell.org/definition/haskell2010.pdf>`_. Where that is
not possible (e.g. because the specification relates to a feature that
is not in the Haskell 2010 report), try to adhere its style and level
of detail. Think about corner cases. Write down general rules and
invariants.

Note, however, that this section should focus on a precise
*specification*; it need not (and should not) devote space to
*implementation* details -- there is a separate section for that.

The specification can, and almost always should, be illustrated with
*examples* that illustrate corner cases. But it is not sufficient to
give a couple of examples and regard that as the specification! The
examples should illustrate and elucidate a clearly-articulated
specification that covers the general case.

Examples
--------
This section illustrates the specification through the use of examples of the
language change proposed. It is best to exemplify each point made in the
specification, though perhaps one example can cover several points. Contrived
examples are OK here. If the Motivation section describes something that is
hard to do without this proposal, this is a good place to show how easy that
thing is to do with the proposal.

Effect and Interactions
-----------------------
Your proposed change addresses the issues raised in the motivation. Explain how.

Also, discuss possibly contentious interactions with existing language or compiler
features. Complete this section with potential interactions raised
during the PR discussion.


Costs and Drawbacks
-------------------


Alternatives
------------
List alternative designs to your proposed change. Both existing
workarounds, or alternative choices for the changes. Explain
the reasons for choosing the proposed change over these alternative:
*e.g.* they can be cheaper but insufficient, or better but too
expensive. Or something else.

The PR discussion often raises other potential designs, and they should be
added to this section. Similarly, if the proposed change
specification changes significantly, the old one should be listed in
this section.

Unresolved Questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and
specification. Be upfront and trust that the community will help. Please do
not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to
the steering committee.


Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other resources
and prerequisites are required for implementation?

Endorsements
-------------
(Optional) This section provides an opportunty for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.
