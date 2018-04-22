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

Preliminary remark 1: The intention of the following specification is that the following holds: For a constructor with type ``C :: forall a. a -> …`` the meaning of ``C @ty x`` should coincide with the existing form ``C (x :: ty)``. If that is not the case, then this is likely a bug in the specification.

Preliminary remark 2: The ``ScopedTypeVariables`` is not very well specified. The following attempts to specify it, and may make more programs type check, but should not change the meaning of existing programs. If so, then that is likely a bug in the specification.


When both ``TypeApplications`` and ``PatternSignatures`` are enabled, then type application syntax is
available in patterns. 

If ``ScopedTypeVariables`` is enabled, then type applications and type signatures in pattern may mention type variables.

A type variable mentioned in a pattern (in a type signature or a type application) that is not already in scope is brought into scope. The scope contains the pattern and the corresponding right-hand side.

Consider a general data type and constructor::

  data T a where MkT :: forall b. Ctx => ty1 -> T ty2
  
(using single variables and types as representatives for, in general, multiple variables and types) and the function definition::

   foo :: T ty3 -> …
   foo (MkT @ty4 (x :: ty5)) = e

and assume that ``free_ty_vars(ty4, ty5) = {c}``. This pattern is well-typed if, for a fresh rigid variable ``b'``, there exists a type ``ty6`` (which may depend on ``b'``) such that type equations

* ``ty2[b'/b] ~ ty3, Ctx[b'/b] => ty4[ty6/c] = b'`` and
* ``ty2[b'/b] ~ ty3, Ctx[b'/b] => ty5[ty6/c] = ty1[b'/b]``

hold. The right-hand-side ``e`` is then checked in a contex that contains the type variables  ``b'``, ``c``, the equality ``c ~ ty6`` and the typing judgement ``x :: ty1[b'/b]``.

An underscore in a type signature or type application in a pattern is not treated as a hole.

Further changes to ``ScopedTypeVariables`` should apply analogously to type applications in patterns.

Examples
--------

Here is an example (taken from _#15050 <https://ghc.haskell.org/trac/ghc/ticket/15050#comment:10>_)::

    type family F a where F Bool = Int
    data T a where MkT :: forall b a. b ~ F a => b -> T a
    foo :: T Bool -> ()
    foo (MkT @Int _) = ()

Lining this up with the specification, we have ``Ctx = b ~ F a``, ``ty1 = b``, ``ty2 = a``, ``ty3 = Bool``, ``ty4 = Int`` and ``ty5`` does not appear. This typechecks because the equation ```a ~ Bool, b' ~ F a => Int = b'`` holds. No type variables are bound here.

A more complex example is this (also inspired by _#15050 <https://ghc.haskell.org/trac/ghc/ticket/15050>_)::

    data T a where
      MkT1 :: forall a.              T a
      MkT2 :: forall a.              T (a,a)
      MkT3 :: forall a b. b ~ Int => T a
      MkT4 :: forall a b.            T a
      MkT5 :: forall a b c. b ~ c => T a
      
    foo :: T (Int, Int) -> ()
    foo (MkT1 @(Int,Int))  = ()
    foo (MkT1 @(Int,x))    = (() :: x ~ Int => ())
    foo (MkT1 @x)          = (() :: x ~ (Int,Int) => ())    
    foo (MkT2 @x)          = (() :: x ~ Int => ())
    foo (MkT3 @_ @Int)     = ()
    foo (MkT4 @_ @x)       = (() :: x ~ x => ()) -- (these constraints here just to
    foo (MkT5 @_ @x @x)    = (() :: x ~ x => ()) --  demonstrate that x is in scope)

Why do these equations type-check? Let’s look at each of the 7 equations.

1. We have ``Ctx = ()```, no ``ty1``, ``ty2 = a``, ``ty3 = (Int,Int)``, ``ty4 = (Int,Int)`` and no ``ty5``. This type checks because ``a' ~ (Int,Int) => (Int,Int) = a'`` holds.
2. Here we bring a new type variable ``x`` into scope (this plays the role of ``c`` in the spec). We have ``Ctx = ()```, no ``ty1``, ``ty2 = a``, ``ty3 = (Int,Int)``, ``ty4 = (Int,x)`` and no ``ty5``. This type checks because ``a' ~ (Int,Int) => (Int,x) = a'`` if we assign ``x = Int``. The body of the function is type-checked with ``x ~ Int`` in scope.
3. We bring a new type variable ``x`` into scope. We have ``Ctx = ()```, no ``ty1``, ``ty2 = a``, ``ty3 = (Int,Int)``, ``ty4 = x`` and no ``ty5``. This type checks because ``a' ~ (Int,Int) => x = a'`` if we assign ``x = (Int,Int)``. The body of the function is type-checked with ``x ~ (Int,Int)`` in scope.
4. We bring a new type variable ``x`` into scope. We have ``Ctx = ()```, no ``ty1``, ``ty2 = (a,a)``, ``ty3 = (Int,Int)``, ``ty4 = x`` and no ``ty5``. This type checks because ``(a',a') ~ (Int,Int) => x = a'`` if we assign ``x = Int``. The body of the function is type-checked with ``x ~ Int`` in scope.
5. We have ``Ctx = b ~ Int``, no ``ty1``, ``ty2 = a``, ``ty3 = (Int,Int)``, ``ty4₂ = Int`` and no ``ty5``. This type checks because ``a' ~ (Int,Int), b' = Int => Int = b'``.
6.  We bring a new type variable ``x`` into scope. We have ``Ctx = ()``, no ``ty1``, ``ty2 = a``, ``ty3 = (Int,Int)``, ``ty4₂ = x`` and no ``ty5``. This type checks because ``a' ~ (Int,Int) => x = b'`` for ``x = b'`` The body of the function is type-checked with ``x ~ b'`` in scope (but ``b'`` is not visible to the user).
7.  We bring a new type variable ``x`` into scope. We have ``Ctx = b ~ c``, no ``ty1``, ``ty2 = a``, ``ty3 = (Int,Int)``, ``ty4₂ = x``, ``ty4₃ = x`` and no ``ty5``. This type checks because ``a' ~ (Int,Int), b' ~ c' => x = b'`` and  ``a' ~ (Int,Int), b' ~ c' => x = c'`` hold with the assignemnt ``x = b'``. The body of the function is type-checked with ``x ~ b', b' ~ c'`` in scope.


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
*not* matc the behaviour of type signatures.

For users who want this mainly to instantiate existential variables may find that they have to write ``C @_ @x`` to
go past the universial variables, which is mildly inconvenient. It may be fixed in some cases by changing the order
of the type variables of ``C``. This is unavoidable if we want to preserve the symmetry between terms and types, though.

Alternatives
------------
Proposal #96 proposes a variant where ``@x`` may only mention type variables and only existential type variables may be
bound this way. See there for a in depth discussion; a summary of the main criticism that the proposal at hand tries
to fixes preserving the symmetry between type applications in terms and patters, and preserving the analogy between
type applications and type signatures. Furthermore, it does not introduce new concecpts (e.g. the distinction between
existential and universal parameters) to the Haskell programmer.


Unresolved questions
--------------------
This is a very naive attempt at giving ``ScopedTypeVariables`` (and hence this feature) a formal specification, and I am happy to refine it.
