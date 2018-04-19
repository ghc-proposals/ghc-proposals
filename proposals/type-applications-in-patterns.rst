Type Applications in Patterns
=============================

.. proposal-number::
.. trac-ticket::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

We have ``TypeApplications`` in terms. This proposal brings them to patterns in a way that preserves analogy to type signatures.


Motivation
------------

``TypeApplications`` are a convenient and natural way to specifying types of polymorphic functions. Consider::

 data Foo a where MkFoo :: forall a, a -> Foo a
 
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

When both ``TypeApplications`` and ``PatternSignatures`` are enabled, then type application syntax is
available in patterns.

The meaning of ``C @_`` is simply ``C``, i.e. underscores in type applications in patterns are not treated as holes.

For a constructor with type ``C :: forall a. a -> …`` the meaning of ``C @ty x`` is specified to
coincide with the existing form ``C (x :: ty)``.

Equivalently, for a constructor with type ``C :: forall a. Proxy a -> …`` the meaning of ``C @ty x`` is specified to
coincide with the existing form ``C (x :: Proxy ty)``.

For a constructor with a type variable ``a`` that is not the type of any term argument, the meaning is 
what it would be if it had corresponding proxy arguments.

If ``ScopedTypeVariables`` is enabled, then ``ty`` may bring type variables into scope; the same rules
as for ``ScopedTypeVariables`` apply.

Further changes to ``ScopedTypeVariables`` will apply analogously to type applications in patterns.


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
The specification is a bit vague for constructors who have type variables that are neither the type of a term
parameter, nor the argument to a proxy type in a the type of a term parameter. I hope the intent is still clear, 
but I would appreciate help phrasing it in a more satisfying way.
