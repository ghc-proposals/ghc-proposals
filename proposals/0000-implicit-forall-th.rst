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

Note that this only happens in GHC 8.0, after `-XTypeInType` made it possible
to quantify kind variables explicitly like this.

Proposed Change Specification
-----------------------------
Change the behavior of Template Haskell quoting so that it does not put
implicitly quantified type variable binders in a ``ForallT`` or a
``ForallC``. That's it. This restores the way Template Haskell quoting worked
before GHC 8.0.

Effect and Interactions
-----------------------
In the ``idProxy`` example above, the quoted type now accurately reflects the
fact that ``k`` is implicitly quantified. In terms of a Template Haskell AST,
it would look something like this: ::

  SigD idProxy
    (ForallT [PlainTV proxy,KindedTV a (VarT k)] []
    (AppT (AppT ArrowT (AppT (VarT proxy) (VarT a))) (AppT (VarT proxy) (VarT a))))

Notice that ``VarT k`` _only_ appears in a kind, not as its own variable
binder.

Costs and Drawbacks
-------------------
In some sense, this proposal would cause quoting to "lose" information,
since implicitly quantified type variables no longer appear at the front of
``ForallT`` or ``ForallC``. But really, the only thing we'd be losing is a
slight convenience, since it's always possible to compute the implicitly
quantified variables in a ``Type`` by collecting its type variables and
removing those which are bound by a ``ForallT`` or ``ForallC``.

Alternatives
------------
Instead of leaving implicitly quantified variables out completely, we could
instead introduce new ``ImplicitForallT`` and ``ImplicitForallC`` constructors
whose role is precisely to indicate implicit quantification. As an example,
``idProxy`` above would have this Template Haskell AST: ::

  SigD idProxy
    (ImplicitForallT [PlainTV k]
    (ForallT [PlainTV proxy,KindedTV a (VarT k)] []
    (AppT (AppT ArrowT (AppT (VarT proxy) (VarT a))) (AppT (VarT proxy) (VarT a)))))

This has the disadvantage of being not very backwards-compatible, since it
can't be retrofitted with pattern synonyms.

Another alternative that @int-index proposed is to change ``TyVarBndr`` to
include quantification information: ::

  data BndrCls = BndrExplicit | BndrImplicit
  
  data TyVarBndr = PlainTV'  BndrCls Name        -- ^ @a@
                 | KindedTV' BndrCls Name Kind   -- ^ @(a :: k)@
  
  pattern PlainTV name <- PlainTV' _ name where
    PlainTV name = PlainTV' BndrExplicit name

  pattern KindedTV name kind <- KindedTV' _ name kind where
    KindedTV name kind = KindedTV' BndrExplicit name kind

This would be more backwards-compatible than
``ImplicitForallT``/``ImplicitForallC``.

Both ideas, however, are questionable in the sense that they are adding more
information than what users originally wrote in their quoted source code.
And the gain that these API additions would provide is questionable in light
of the fact that a ``Type``'s implicitly quantified variables can be computed
with relative ease (see the Costs and Drawbacks section).

Unresolved questions
--------------------
Are any Template Haskell users relying on this behavior to discover what the
implicitly quantified type variables in a ``Type`` are?. My guess is either
"not many" or "none", given that this is a feature that didn't begin to work
reliably until GHC 8.0.

Implementation Plan
-------------------
I volunteer to implement. I currently have a
`Phabricator Diff <https://phabricator.haskell.org/D2974>`
implementing the ideas above.
