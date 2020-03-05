Type Applications in Patterns
=============================

.. author:: Ryan Scott
.. date-accepted:: 2018-08-16
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/11350
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/126>`_.
.. contents::

We have ``TypeApplications`` in terms. This proposal brings them to patterns in a way that preserves analogy to type signatures.


Motivation
------------

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
-----------------------------

When both ``TypeApplications`` and ``ScopedtypeVariables`` are enabled, then type application syntax is
available in constructor patterns. Concretely, the grammar for constructor pattern is extended from::

  pat   → gcon apat1 … apatk --  (arity gcon  =  k, k ≥ 1)
        …

to::

  pat   → gcon tyapp1 … tyappn  apat1 … apatk --  (arity gcon  =  k, k ≥ 1, n ≥ 0)
        …
  tyapp → @ atype

A pattern ``C @a``, where ``C`` is a data constructor and ``a`` is a type variable that is not yet in scope, matches if ``C`` matches. It brings ``a`` into scope so that ``a`` stands for the corresponding type that was passed to ``C`` upon construction.

More complicated cases such as ``C @ty``, where ``ty`` is not just a plain type variable, are handled like `type signatures in patterns <https://downloads.haskell.org/~ghc/8.4.3/docs/html/users_guide/glasgow_exts.html#pattern-type-sigs>`_. A full and formal description of the scoping and typing rules for this feature can be found in `“Type variable in pattern” by Richard Eisenberg, Joachim Breitner and Simon Peyton Jones <https://arxiv.org/abs/1806.03476>`_.

An underscore in a type signature or type application in a pattern is allowed, does not bind any variables, and is not treated as a partial type signature (i.e. does not cause warnings with ``-Wpartial-type-signatures``).

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

    foo :: T (Int, Int) -> ()
    foo (MkT1 @(Int,Int))  = ()
    foo (MkT2 @x)          = (() :: x ~ Int => ())
    foo (MkT3 @_ @x)       = (() :: x ~ x => ()) -- nb: x is in scope
    foo (MkT4 @_ @x)       = (() :: x ~ Int => ())
    foo (MkT4 @_ @Int)     = ()
    foo (MkT5 @_ @x @x)    = (() :: x ~ x => ())

All of these equations type-check (just like they would if added value arguments of type ``a``, ``b``,... to the constructors and turned the type applications into type signatures).

Note that the ``@_`` are not treated like partial type signatures.

Note that it is usually a type error to supply a non-tyvar type, or an in-scope tyvar, in an existential position (e.g. ``MkT3 @_ @Int`` is wrong), unless the data constructor has constraints that equate the existential type variable to some type (as in the equations involving ``MkT4`` and ``MkT5`` above).

Scoping
~~~~~~~

The scoping works just like with ``ScopedTypeVariables``. Just for reference, here are some examples of how that feature works now::

 f1 :: forall a b. ([a], b) -> Int
 f1 (x :: [c], y) = ...

brings ``c`` into scope, together with ``a`` and ``b``, which are already in scope. The type variables ``c`` and ``a`` refer to the same type.

But the pattern in::

 f2 :: forall a b. ([a], b) -> Int
 f2 (x :: [b], y) = ...

does not bring ``b`` into scope; here ``b`` refers to the ``b`` from the type signature. This leads to an type error, because in general ``a`` and ``b`` do not refer to the same types.

And the pattern in::

 f3 :: forall a b. ([a], b) -> Int
 f3 (x :: [c], y :: c) = ...

brings one ``c`` into scope; the second occurence in the pattern does not shadow the first one, but rather refers to the same type. This example, too,  leads to a type error because ``c`` needs to be equal to both ``a`` and ``b``.

The same rules apply for type applications, so the above examples could be re-written as follows, with identical behaviour::

 f1 :: forall a b. ([a], b) -> Int
 f1 ((,) @[c] x y) = ...
 f2 :: forall a b. ([a], b) -> Int
 f2 ((,) @[b] x y) = ...
 f3 :: forall a b. ([a], b) -> Int
 f3 ((,) @[c] @c x y) = ...

Effect and Interactions
-----------------------
We answer the question “what should ``@ty`` mean in patterns” based on an existing feature (“what should ``_ :: ty`` mean in patterns”. This fills an obvious hole in the syntax in a way that is consistent with existing features. In particular analogy between type applications and type signatures that we currently have in terms will now hold the same way in patterns.

Furthermore, type application arguments to ``C`` refer to the corresponding parameters in both terms and types, irregardless of whether they are considered universal or existential variables (this is not the case for alternative proposals, as explained below under “Alternatives”).

This proposals allows the binding of existential type variables of constructors, and hence subsumes `Proposal #96 <https://github.com/ghc-proposals/ghc-proposals/pull/96>`_.

There is almost a syntactic ambiguity with as-patterns, but in fact there is not: The grammar of as-pattern is::

  apat 	→ 	var [ @ apat] 	    (as pattern)
        …

so it always has a variable on its left, whereas a type application is always headed by a constructor.

Costs and Drawbacks
-------------------
Given that the specification is inspired by an existing feature, I expect the implementation cost to be low; mostly work in the parser. I believe that learners will benefit from the homogenousness that this proposals preserves.

For users who want this mainly to instantiate existential variables may find that they have to write ``C @_ @x`` to
go past the universial variables, which is mildly inconvenient. It may be fixed in some cases by changing the order
of the type variables of ``C``. This is unavoidable if we want to preserve the symmetry between terms and types, though. A mitigation for this is offerend in `proposal #99 (explicit specificity) <https://github.com/ghc-proposals/ghc-proposals/pull/99>`_.

A possible future proposal that extends as-patterns to allow patterns on both sides of the `@` would now introduce ambiguities, e.g. in `Nothing @ a`, and will require disambiguation. This disambiguation could be

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
`Proposal #96 <https://github.com/ghc-proposals/ghc-proposals/pull/96>`_ proposes a variant where ``@x`` may only mention type variables and only existential type variables may be
bound this way. See there for an in depth discussion; a summary of the main criticism that the proposal at hand tries
to fixes preserving the symmetry between type applications in terms and patters, and preserving the analogy between
type applications and type signatures, and also in Section 6.1 of `the paper <https://arxiv.org/abs/1806.03476>`_. Furthermore, it does not introduce new concepts (e.g. the distinction between
existential and universal parameters) to the Haskell programmer.

The existing restriction of ``ScopedTypeVariabes`` that type variables in pattern signatures may only be bound to type variables, and not types, carries over to type variables in type applications. One could discuss lifting this restriction, but this question is completely orthotogonal to the proposal at hand, and should be discussed elsewhere (e.g. in `Proposal #128 <https://github.com/ghc-proposals/ghc-proposals/pull/128>`_ and `ticket #15050 <https://gitlab.haskell.org/ghc/ghc/issues/15050#note_152286>`_).
