Type Applications in Patterns
=============================

.. proposal-number::
.. trac-ticket::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/126>`_.
.. sectnum::
.. contents::

We have ``TypeApplications`` in terms. This proposal brings them to patterns in a way that preserves analogy to type signatures.


Motivation
------------

``TypeApplications`` are a convenient and natural way to specifying types of polymorphic functions. Consider::

 data Foo a where MkFoo :: forall a. a -> Foo a
 
With ``TypeApplications``, I can replace the somewhat clumsy ``MkFoo (x :: ty)`` with ``MkFoo @ty x``. Seen this way,
explicit type applications are merely alternative syntax for type signatures.

At the moment, this only works in terms, but not in patterns: We can use type signatures in patterns
(if ``PatternSignatures`` or ``ScopedTypeVariables`` are enabled), but not type applications. Given the strong
relation between these syntactic forms, this is odd – why can I write::

    foo (MkFoo (x :: ty)) = …
   
but not::

    foo (MkFoo @ty x) = …

This proposal fills this gap, by allowing type applications in pattern syntax and specifying it to behave
“just like type signatures”.

Proposed Change Specification
-----------------------------

Preliminary remark 1: The intention of the following specification is that the following holds: For a constructor with type ``C :: forall a. a -> …`` the meaning of ``C @ty x`` should coincide with the existing form ``C (x :: ty)``. If that is not the case, then this is likely a bug in the specification. In any case, this principle should answer most, if not all, questions about how this proposal should be interpreted.

Preliminary remark 2: The ``ScopedTypeVariables`` is not very well specified. The following attempts to specify it, and may make more programs type check, but should not change the meaning of existing programs. If so, then that is likely a bug in the specification.

When both ``TypeApplications`` and ``PatternSignatures`` are enabled, then type application syntax is
available in patterns. 

If ``ScopedTypeVariables`` is enabled, then type applications and type signatures in pattern may mention type variables.

A type variable mentioned in a pattern (in a type signature or a type application) that is not already in scope is brought into scope. The scope spans the pattern and the corresponding right-hand side.

Typechecking a type application in a pattern follows the same rules as typechecking a pattern signature. This proposal does not propose any changes to the mechanism. In particular

* type variables in type applications in patterns can only bind type variables, just as it is the case for type variables in pattern signatures (unless #128 gets accepted first).
* patterns are type-checked from left to right. This means when type-checking the type application in a pattern ``(MkT @ty)``, type equatlities from matches further to the left, as well as the type equalities from the context of ``MkT`` itself are in scope, but not type equalities from constructors further to the right (see below for an example).

A more formal description of these rules can be found in “Type variable in pattern” by Richard Eisenberg, Joachim Breitner  and Simon Peyton Jones <https://arxiv.org/abs/1806.03476>_.

An underscore in a type signature or type application in a pattern is not treated as a hole.

Further changes to ``ScopedTypeVariables`` (e.g. _#15050 <https://ghc.haskell.org/trac/ghc/ticket/15050#comment:10>_) should apply analogously to type applications in patterns.

Examples
--------


Here is an example (taken from _#15050 <https://ghc.haskell.org/trac/ghc/ticket/15050#comment:10>_)::

    type family F a where F Bool = Int
    data T a where MkT :: forall b a. b ~ F a => b -> T a
    foo :: T Bool -> ()
    foo (MkT @Int _) = ()

This should type-check, because the following code does::

    foo :: T Bool -> ()
    foo (MkT (_ ::Int _)) = ()


A more complex example is this (also inspired by _#15050 <https://ghc.haskell.org/trac/ghc/ticket/15050>_)::

    data T a where
      MkT1 :: forall a.              T a
      MkT2 :: forall a.              T (a,a)
      MkT3 :: forall a b. b ~ Int => T a
      MkT4 :: forall a b.            T a
      MkT5 :: forall a b c. b ~ c => T a
      
    foo :: T (Int, Int) -> ()
    foo (MkT1 @(Int,Int))  = ()
    foo (MkT2 @x)          = (() :: x ~ Int => ())
    foo (MkT3 @_ @Int)     = ()
    foo (MkT4 @_ @x)       = (() :: x ~ x => ()) -- (these constraints here just to
    foo (MkT5 @_ @x @x)    = (() :: x ~ x => ()) --  demonstrate that x is in scope)

All of these equations type-check (just like they would if added value arguments of type ``a``, ``b``,... to the constructors and turned the type applications into type signatures).

This example demonstrated why we need to typecheck nested patterns left-to-right::

 data T a where
   T1 :: T Int
   T2 :: T a

 f :: Int -> Char -> Bool

 g :: (a, Char, T a) -> blah
 g (x :: Int, (f x -> True), T1) = ..

``g`` must not be accepted: Until we match on ``T1`` we have no idea if ``a ~ Int``.
And, with Haskell's left-to-right pattern matching we'll
match the view pattern ``(f x -> True)`` first. It looks ok, because
you can see that ``x :: Int``; but it will seg-fault in a call of
``g`` involving ``T2`` and a first argument that is (say) a list.

Scoping
~~~~~~~

The scoping works just like with ``ScopedTypeVariables``. Just for reference, here are some examples of how that feature works now::

 f :: forall a b. ([a], b) -> INt
 f (x :: [v], y) = ...

brings ``v`` into scope, together with ``a`` and ``b``, which are already in scope.

But the pattern in::

 f :: forall a b. ([a], b) -> INt
 f (x :: [b], y) = ...

does not bring ``b`` into scope; here ``b`` refers to the ``b`` from the type signature.

And the pattern in::
 
 f :: forall a b. ([a], b) -> INt
 f (x :: [v], y :: v) = ...

brings one ``v`` into scope; the second occurence in the pattern does not shadow the first one, but rather refers to the same type (this would lead to a type error because ``v`` needs to be equal to both ``a`` and ``b``, but maybe they are not the same).

The same rules apply for type applications, and similarly to the last example, the following should not type-check:

 data T where
   MkT :: a -> b -> T

 f (MkT @p @p a b) = ...

Effect and Interactions
-----------------------
By reducing the question of “what should ``@ty`` mean in patterns” to an existing feature, we fill an obvious
hole in the syntax in a way that is consistent with existing features: The analogy between type applications
and type signatures will hold the same way in terms as it would in types.

Furthermore, type application arguments to ``C`` refer to the same parameters in both terms and types (which
is not the case for alternative proposals.)

This proposals allows the binding of existential type variables of constructors, and hence subsumes #96.

Costs and Drawbacks
-------------------
Given that we built upon an existing feature, I expect the implementation cost to be less than with other proposals.

I believe that learners will benefit from the homogenousness that this proposals preserves.

A drawback is that it piggy backs on ``ScopedTypeVariables``, which – to some people – has its warts and unprettiness.
This is a fair concern that needs to be weighed against the cost of introducing a meaning for type applciations that does
*not* match the behaviour of type signatures.

For users who want this mainly to instantiate existential variables may find that they have to write ``C @_ @x`` to
go past the universial variables, which is mildly inconvenient. It may be fixed in some cases by changing the order
of the type variables of ``C``. This is unavoidable if we want to preserve the symmetry between terms and types, though. A mitigation for this is offerend in `proposal #99 (explicit specificity) <https://github.com/ghc-proposals/ghc-proposals/pull/99>`_.

Alternatives
------------
`Proposal #96 <https://github.com/ghc-proposals/ghc-proposals/pull/96>`_ proposes a variant where ``@x`` may only mention type variables and only existential type variables may be
bound this way. See there for a in depth discussion; a summary of the main criticism that the proposal at hand tries
to fixes preserving the symmetry between type applications in terms and patters, and preserving the analogy between
type applications and type signatures. Furthermore, it does not introduce new concecpts (e.g. the distinction between
existential and universal parameters) to the Haskell programmer.

The existing restriction of ``ScopedTypeVariabes`` that type variables in pattern signatures may only be bound to type variables, and not types, carries over to type variables in type applications. One could discuss lifting this restriction, but this question is completely orthotogonal to the proposal at hand, and should be discussed elsewhere (e.g. in (e.g. _#15050 <https://ghc.haskell.org/trac/ghc/ticket/15050#comment:10>_).

Unresolved questions
--------------------
This is a very naive attempt at giving ``ScopedTypeVariables`` (and hence this feature) a formal specification, and I am happy to refine it.
