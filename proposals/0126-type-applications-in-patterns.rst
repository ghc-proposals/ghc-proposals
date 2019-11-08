Type Applications in Patterns
=============================

.. proposal-number:: 31
.. author:: Ryan Scott
.. date-accepted:: 2018-08-16
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/15530
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/126>`_.
.. sectnum::
   :start: 126
.. contents::

We have ``TypeApplications`` in terms. This proposal brings them to patterns in a way that preserves analogy to type signatures.


Motivation
------------

Symmetry between terms and patterns
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

This proposal fills this gap.

Simpler scoping rules
~~~~~~~~~~~~~~~~~~~~~

The scoping rules from ``ScopedTypeVariables`` are quite complicated. With plain::

    foo :: Foo -> SomethingElse ty …
    foo (MkFoo (x :: ty)) = …

``ty`` is freshly bound. But with::

    foo :: forall ty. Foo -> SomethingElse ty …
    foo (MkFoo (x :: ty)) = …

``ty`` on the second line is now captured by the implicit binding on the first line.
I made the first line’s implicit binding explicit with a ``forall``::
But, ``ScopedTypeVariables`` gives me nothing analogous to ``forall`` to write an explicit, uncapturable binding in patterns.
We cannot change ``ScopedTypeVariables`` without breaking lots of code, but we can give ``@`` that role.::

    foo :: forall ty. Foo -> SomethingElse ty -> …
    foo (MkFoo @ty x) = …

Here, the ``ty`` in ``@ty`` is always a fresh binding, in this case shadowing the ``ty`` from above.

Future collapsing of types and terms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With Dependent Haskell, ``@a`` vs plain ``a`` is not supposed to distinguish between type and terms, but rather the visibility of the quantifier.
Simply put, ``@a`` is supposed to mean an optional ``a``, no more, no less.
Baring namespace issues that are being sorted out other proposals, this is ready to be true for terms.
I think this is especially salient in the case of the relevent, dependent quantification (``foreach x.``).
When bind types today (say with pattern signatures), which are erased, we cannot do much computation and mainly are "guiding" inference.
It can feel more like annotations than the program proper, and thus separate scoping rules don't feel so off.
When pattern matching a constructor with such a field, we really pattern matching a runtime value we can then compute with perfectly normally.

With this proposal, it we are ready to say ``@a`` is just an optional ``a`` in patterns too.
Baring the affected namespace, the scoping for ``@a`` and ``a`` is exactly the same.

Proposed Change Specification
-----------------------------

When ``TypeApplications`` is enabled, then type application syntax is
available in constructor patterns. Concretely, the grammar for constructor pattern is extended from::

  pat   → gcon apat1 … apatk --  (arity gcon  =  k, k ≥ 1)
        …

to::

  pat   → gcon tyapp1 … tyappn  apat1 … apatk --  (arity gcon  =  k, k ≥ 1, n ≥ 0)
        …
  tyapp → @ atype

However, without ``ScopedTypeVariables``, no variables can be bound in the type, only closed patterns (which include underscores) are permitted.

A pattern ``C @a``, where ``C`` is a data constructor and ``a`` is a type variable that is not yet in scope, matches if ``C`` matches.
It binds a fresh ``a`` so that ``a`` stands for the corresponding type that was passed to ``C`` upon construction.

Just like with regular (term) identifiers, it is an error to bind the same type variable twice in a sequence of patterns.

More complicated cases such as ``C @ty``, where ``ty`` is not just a plain type variable, are handled like `type signatures in patterns <https://downloads.haskell.org/~ghc/8.4.3/docs/html/users_guide/glasgow_exts.html#pattern-type-sigs>`_. A full and formal description of the typing rules for this feature can be found in `“Type variable in pattern” by Richard Eisenberg, Joachim Breitner and Simon Peyton Jones <https://arxiv.org/abs/1806.03476>`_.
However, note the scoping rules are simplified from what is in the paper.

An underscore in a type application is not treated as a partial type signature (i.e. does not cause warnings with ``-Wpartial-type-signatures``).
A pattern signature however is treated as a partial type signature, as that is a signature first, which only contains bindings by analogy to the normal implicit free variable binding rule that applies to regular signatures.

Examples
--------

Here is an example (taken from _#15050 <https://gitlab.haskell.org/ghc/ghc/issues/15050#note_152286>_)::

    type family F a where F Bool = Int
    data T a where MkT :: forall b a. b ~ F a => b -> T a
    foo :: T Bool -> ()
    foo (MkT @Int _) = ()

This should type-check, because the following code does::

    foo :: T Bool -> ()
    foo (MkT (_ ::Int _)) = ()

Note that the data constructor expects up-to two type arguments (``forall b a.…``), but we are passing only one type argument, which then corresponds to the *first* type argument of of the data constructor.

A more complex example is this (also inspired by `#15050 <https://gitlab.haskell.org/ghc/ghc/issues/15050>`_)::

    data T a where
      MkT1 :: forall a.              T a
      MkT2 :: forall a.              T (a,a)
      MkT3 :: forall a b.            T a
      MkT4 :: forall a b. b ~ Int => T a
      MkT5 :: forall a b c. b ~ c => T a
      MkT6 :: forall a b. ClassWithFunDep a b => T a

    class ClassWithFunDep a b | a -> b

    foo :: T (Int, Int) -> ()
    foo (MkT1 @(Int,Int))  = ()
    foo (MkT2 @x)          = (() :: x ~ Int => ())
    foo (MkT3 @_ @x)       = (()) -- x is unconstrained
    foo (MkT4 @_ @x)       = (() :: x ~ Int => ())
    foo (MkT4 @_ @Int)     = ()
    foo (MkT5 @_ @x @y)    = (() :: x ~ y => ())
    foo (MkT6 @x @y)       = (() :: ClassWithFunDep x y => ()) -- hard to bind b otherwise!

All of these equations type-check.

Note that the ``@_`` are not treated like partial type signatures.

Note that it is usually a type error to supply a non-tyvar type, or an in-scope tyvar, in an existential position (e.g. ``MkT3 @_ @Int`` is wrong), unless the data constructor has constraints that equate the existential type variable to some type (as in the equations involving ``MkT4`` and ``MkT5`` above).

Scoping
~~~~~~~

The scoping works just like regular variables, but in the type namespace::

 f1 :: forall a b. ([a], b) -> Int
 f1 ((,) @[c] x y) = ...           -- c ~ a
 f2 :: forall a b. ([a], b) -> Int
 f2 ((,) @[b] x y) = ...           -- c ~ b
 f3 :: forall a b. ([a], b) -> Int
 f3 ((,) @[c] @c x y) = ...        -- error, c bound twice
 f3 :: forall a b. ([a], b) -> Int
 f3 ((,) @[c] c d) = ...           -- c ~ a

Effect and Interactions
-----------------------
We answer the question “what should ``@ty`` mean in patterns” based on the analogous features (“what should ``ty`` mean in patterns”, and “what does ``@ty`` means in terms”.
This “completes the square”, so we have visible and invisible constructor application and decomposition.

Furthermore, type application arguments to ``C`` refer to the corresponding parameters in both terms and types, irregardless of whether they are considered universal or existential variables (this is not the case for alternative proposals, as explained below under “Alternatives”).

This proposals allows the binding of existential type variables of constructors, and hence subsumes `Proposal #96`_.

There is almost a syntactic ambiguity with as-patterns, but in fact there is not: The grammar of as-pattern is::

  apat  →   var [ @ apat]       (as pattern)
        …

so it always has a variable on its left, whereas a type application is always headed by a constructor.

`Proposal #238`_ would introduce an ``-XTypeAbstractions``, which completes this in adding “top level” ``@ty`` patterns, while this proposal adds nested ``@ty`` patterns.
It also restricts ``-XScopeTypeVariables`` so that ``forall`` no longer binds type variables over the term with the ``forall`` type.
`Proposal 285`_ would introduce a ``-XNoImplcitForAll``, which would prohibit the implicit binding of free variables in regular and pattern signatures.
The combination of these proposals is rather nice; we can “write System F”, with no superfluous free var binding sugar or missing explicit syntactic constructs.
This should be good for teaching, and also get us closer to a fully faithful resugaring of GHC’s core.

Costs and Drawbacks
-------------------
Given that the specification is inspired by an existing feature, I expect the implementation cost to be low; mostly work in the parser. I believe that learners will benefit from the homogenousness that this proposals preserves.

For users who want this mainly to instantiate existential variables may find that they have to write ``C @_ @x`` to
go past the universial variables, which is mildly inconvenient. It may be fixed in some cases by changing the order
of the type variables of ``C``. This is unavoidable if we want to preserve the symmetry between terms and types, though. A mitigation for this is offerend in `proposal #99`_.

A possible future proposal that extends as-patterns to allow patterns on both sides of the ``@`` would now introduce ambiguities, e.g. in ``Nothing @ a``, and will require disambiguation. This disambiguation could be

* extra parenthesis: ``(Nothing) @ a`` is an as-pattern, vs. ``Nothing @ a`` is a type application.
* using a helper pattern synonym::

        pattern And p q = p@q

        foo (Nothing `And` a) = …

  These questions will have to be resolve if and when such extended as-patterns are requested.


Open Questions
------------
* How is the ambiguity with as-patterns resolved?

Alternatives
------------
`Proposal #96`_ proposes a variant where ``@x`` may only mention type variables and only existential type variables may be
bound this way. See there for an in depth discussion; a summary of the main criticism that the proposal at hand tries
to fixes preserving the symmetry between type applications in terms and patters, and preserving the analogy between
type applications and type signatures, and also in Section 6.1 of `the paper <https://arxiv.org/abs/1806.03476>`_. Furthermore, it does not introduce new concepts (e.g. the distinction between
existential and universal parameters) to the Haskell programmer.

The existing restriction of ``ScopedTypeVariables`` that type variables in pattern signatures may only be bound to type variables, and not types, carries over to type variables in type applications. One could discuss lifting this restriction, but this question is completely orthotogonal to the proposal at hand, and should be discussed elsewhere (e.g. in `Proposal #128 <https://github.com/ghc-proposals/ghc-proposals/pull/128>`_ and `ticket #15050 <https://gitlab.haskell.org/ghc/ghc/issues/15050#note_152286>`_).

We could allow ``-XTypeAbstractions`` to bind variables even without ``-XScopeTypeVariables``.
Unlike turning ``-XScopeTypeVariables`` on by default, this would not break any Haskell 98 programs as there are no type application patterns in Haskell 98.
This allows strictly more programs, so it seems fine to consider later.

A previous version of this proposal had the scoping rules work just like that for pattern signatures.
This had the benefit of allowing so-called “non-linear patterns”, where the variables must be proven equal for GHC to accept the program, and also prohibited potentially-confusing shadowing.
The downside is the scoping rules are more complex, and widen the gulf that we claim we want to shrink between visible and invisible quantifiers.
They also break the “write System F” result of this + ``-XTypeAbstractions`` + ``-XNoImplcitForAll``.

I will concede with respect to the old version of the proposal that permitting shadowing allows strictly more programs, giving us less room in the future to maneuver.
If we disallow non-linear patterns and shadowing, we keep the door open to both the old and current version of the proposal.

As final alternative based on the old version of the proposal, in the further future when we have equality constraints, we could create a new warning (or extension) that requires all shadowing and non-linear patterns of any sort to obligate that the two variables be constrained to be equal.
This would effectively apply the old version of the proposal to all bindings, also restoring the symmetry between visible and invisible quantifiers.

.. _`Proposal #96`: https://github.com/ghc-proposals/ghc-proposals/pull/96
.. _`Proposal #238`: https://github.com/ghc-proposals/ghc-proposals/pull/238
