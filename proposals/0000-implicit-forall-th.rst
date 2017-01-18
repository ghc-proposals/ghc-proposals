.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

Preserve implicit type variable quantification in Template Haskell
==============

This proposes a new way to handle implicitly quantified type variables
in Template Haskell-quoted type signatures.


Motivation
------------
There are currently a couple of Template Haskell bugs caused by the fact that
Template Haskell is overeager about quantifying type variables in quoted type
signatures. What does that mean? As an example, consider this program from
`GHC Trac #13123 <https://ghc.haskell.org/trac/ghc/ticket/13123>`: ::

  $([d| idProxy :: forall proxy (a :: k). proxy a -> proxy a
        idProxy x = x
      |])

Surprisingly, if you try to compile this, GHC will complain that the kind
variable ``k`` is also used as a type, and that this requires
``-XTypeInType``. That's because after Template Haskell processes the quoted
declaration, it returns this: ::

  idProxy :: forall k proxy (a :: k). proxy a -> proxy a
  idProxy x = x

A subtle change, but one that's enough to make an otherwise acceptable program
become rejected. Even though ``k`` is an implicitly quantified binder,
quoting made it explicitly quantified under the hood. (See also
`GHC Trac #13018 <https://ghc.haskell.org/trac/ghc/ticket/13018>` for another
example in a similar vein.)

Proposed Change Specification
-----------------------------
Change the behavior of Template Haskell quoting so that it does not put
implicitly quantified type variable binders in a ``ForallT`` or a
``ForallC``. Instead, introduce an ``ImplicitForallT`` constructor to ``Type``
and an ``ImplicitForallC`` constructor to ``Con`` in
``Language.Haskell.TH.Syntax``. The purpose of these will be to store any
implicitly bound type variable binders so that they appear at the beginning
of a quoted type signature, allowing Template Haskell users to easily inspect
which type variables are implicitly quantified.

If a type or constructor containing an ``ImplicitForallT`` or
``ImplicitForallC`` is spliced into source code, Template Haskell's will simply
ignore them and fall through to the underlying type or constructor. This is
consistent with the fact that these ``Implicit-`` forms are indicative of
something that the user did not write themselves. Similarly, the pretty printer
in ``Language.Haskell.TH.Ppr`` will not print out any information for the
``Implicit-`` forms, as doing so would be an unfaithful representation of
what the user originally wrote. Reification is also unaffected, so reified
types and constructors will never contain ``Implicit-`` forms.

Effect and Interactions
-----------------------
In the ``idProxy`` example above, the quoted type now accurately reflects the
fact that ``k`` is implicitly quantified. In terms of a Template Haskell AST,
it would look something like this: ::

  SigD idProxy
    (ImplicitForallT [PlainTV k]
    (ForallT [PlainTV proxy,KindedTV a (VarT k)] []
    (AppT (AppT ArrowT (AppT (VarT proxy) (VarT a))) (AppT (VarT proxy) (VarT a)))))

Costs and Drawbacks
-------------------
This proposal adds two data constructors to the ``Type`` and ``Con`` types,
which are very widely used in Template Haskell code in production. It is
inevitable that many libraries will need to be patched in order to peel
beneath ``ImplicitForallT``s and ``ImplicitForallC``s.

Alternatives
------------
We do not have to introduce ``ImplicitForallT`` and ``ImplicitForallC``.
Alternatively, we could just fix the quoting bug and declare victory. However,
this would come at a loss of information, since Template Haskell clients would
no longer have convenient access to quoted types' implicit type variable
binders. They could do a pass over the type to recalculate the implicitly
quantified type variables themselved, but this might feel like an extra
hoop to jump through.

Unresolved questions
--------------------
How many libraries in the wild would break from this change? On one hand,
there's the issue of introducing yet more Template Haskell data constructors
(which always need some patching to account for). There's also the issue of
how many Template Haskell clients use TH to calculate implicitly quantified
type variables. My guess is either "not many" or "none", given that this is
a feature that didn't begin to work reliably until GHC 8.

Implementation Plan
-------------------
I volunteer to implement. I currently have a branch of GHC implementing the
ideas above.
