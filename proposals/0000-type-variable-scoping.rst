Modern Scoped Type Variables
============================

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

.. _`#126`: https://github.com/ghc-proposals/ghc-proposals/pull/126
.. _`#155`: https://github.com/ghc-proposals/ghc-proposals/pull/155
.. _`#238`: https://github.com/ghc-proposals/ghc-proposals/pull/238
.. _`#285`: https://github.com/ghc-proposals/ghc-proposals/pull/285
.. _`#291`: https://github.com/ghc-proposals/ghc-proposals/pull/291
.. _`#378`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst
.. _`#420`: https://github.com/ghc-proposals/ghc-proposals/pull/420

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

Proposed Change Specification
-----------------------------

Extension Shuffling
~~~~~~~~~~~~~~~~~~~

1. Introduce ``-XPatternSignatures``. With ``-XPatternSignatures``, we
   allow type signatures in patterns. These signatures can mention in-scope
   type variables as variable occurrences, but can not bind type variables.

#. Introduce ``-XPatternSignatureBinds``. With ``-XPatternSignatureBinds``, any
   out-of-scope type variables written in a pattern signature would be bound there
   and would remain in scope over the
   same region of code that term-level variables introduced in a pattern scope
   over. This extension is off by default.
   (This extension is a part of accepted, unimplemented proposal
   `#285`_; the only change is that this proposal makes it off by default.)

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
Give an estimate on development and maintenance costs. List how this effects
learnability of the language for novice users. Define and list any remaining
drawbacks that cannot be resolved.


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
