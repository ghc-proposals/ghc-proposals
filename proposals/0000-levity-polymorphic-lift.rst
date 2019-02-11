Levity-polymorphic `Lift`
=========================

.. proposal-number::
.. trac-ticket::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/208>`_.
.. sectnum::
.. contents::

The `Lift` typeclass lets us lift values into typed or untyped Template Haskell expressions.

::
    class Lift t where
      lift :: t -> Q Exp
      liftTyped :: t -> Q (TExp t)

    newtype TExp a = TExp { unType :: Exp }

One shortcoming of this class is that it does not let us lift values whose kind is not `Type`.
We propose to lift this restriction by making `Lift` and `TExp` levity polymorphic.

::
    class Lift (t :: TYPE r) where
      lift :: t -> Q Exp
      liftTyped :: t -> Q (TExp t)

    newtype TExp (t :: TYPE r) = TExp { unType :: Exp }

Motivation
------------
The `Lift` class cannot currently handle unlifted types. This is a somewhat frustrating limitation
since Template Haskell supports representing unlifted values, only they must be constructed
manually.

::
    x :: Q Exp
    x = LitE (IntPrimL 1)  -- yet we'd like to be able to write `lift 1#`

The fact that `TExp` doesn't support unlifted types means that it is less expressive that `Exp`.
`TExp` is generally regarded as a more type-safe approach to Template Haskell, so users find
themselves needing to choose between generating more performant code that uses unlifted types and
using the more typesafe `TExp`. The problems around `TExp` don't end there: if you omit a type
signature, one can trick GHC 8.6.3 (and latest HEAD) into accepting invalid `TExp`!

::
    {-# LANGUAGE MagicHash, TemplateHaskell #-}

    import GHC.Exts
    import Language.Haskell.TH.Syntax

    main = print =<< runQ (unTypeQ [|| I# $$e ||])
      where e = [|| 1# ||] -- :: Q (TExp Int#)
                           --  yet but adding this ^ signature causes a compile error!

The current `DeriveLift` implementation has some special logic for handling data types with unlifted
fields. Generalizing `Lift` would remove the need for any special cases.

Proposed Change Specification
-----------------------------
Change `TExp`, `Lift`, `unTypeQ`, and `unsafeTExpCoerce` to take advantage of levity polymorphism:

::
    newtype TExp (t :: TYPE r) = TExp { unType :: Exp }

    unTypeQ :: forall (a :: TYPE r). Q (TExp a) -> Q Exp
    unTypeQ m = do { TExp e <- m
                   ; return e }

    unsafeTExpCoerce :: forall (a :: TYPE r). Q Exp -> Q (TExp a)
    unsafeTExpCoerce m = do { e <- m
                            ; return (TExp e) }

    class Lift (t :: TYPE r) where
      lift :: t -> Q Exp
      default lift :: (r ~ 'LiftedRep) => t -> Q Exp
      lift = unTypeQ . liftTyped
      
      liftTyped :: t -> Q (TExp t)
      default liftTyped :: (r ~ 'LiftedRep) => t -> Q (TExp t)
      liftTyped = unsafeTExpCoerce . lift

      {-# MINIMAL lift | liftTyped #-}

NB: The `r ~ 'LiftedRep` is needed because GHC doesn't know how to handle levity polymorphic binders.
It does mean that instances of `Lift` over unlifted types will need to manually implement both `lift`
and `liftTyped`.

Add to `Language.Haskell.TH.Syntax` a handful of new instances of `Lift` which are now valid:

::
    instance Lift Int# where ...
    instance Lift Word# where ...
    instance Lift Float# where ...
    instance Lift Double# where ...
    instance Lift Char# where ...
    instance Lift Addr# where ...

Modify `DeriveLift` to generate typed and untyped splices (for `liftTyped` and `lift` respectively).

Effect and Interactions
-----------------------
The proposed change would make it possible to lift values of unlifted kinds and would make it
(officially) possibly for `TExp` to represent expressions of unlifted kinds.

Costs and Drawbacks
-------------------
The main drawback is that uses of `lift`, `TExp`, `unType`, `unTypeQ`, and `unsafeTExpCoerce` which
have visible type applications will be broken (since the type argument would now refer to the
runtime rep). Here is an example:

::
    ghci> let rationalOne = lift @Rational 1    -- no longer works
    
    <interactive>: error:
        • Expected kind ‘GHC.Types.RuntimeRep’, but ‘Rational’ has kind ‘*’
        • In the type ‘Rational’
          In the expression: lift @Rational 1
          In an equation for ‘it’: it = lift @Rational 1

Any such uses should be easily fixable in a backwards compatible fashion by using explicit type
signatures instead of type applications (or, if one wishes to continue using type applications, by
adding an extra `@_` type argument first).

In terms of development and maintainability, the cost is small: the prototype patch that implements
this functionality ends up removing more lines from the compiler than it adds.

The only other drawback is that beginners browsing the documentation for Template Haskell are more
likely to be confused by the complex signature for the default implementations of the `lift` and
`liftTyped` methods.

Alternatives
------------
None known.

Unresolved Questions
--------------------
If the proposal `Explicit specificity in type variable binders <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0026-explicit-specificity.rst>`_
were already implemented, we might be able to avoid breaking code which uses visible type
application (on any of the functions whose signatures we are proposing to generalize). The idea
would be to specify the `RuntimeRep` type variables as inferred. This would come at the cost of
even more complicated type signatures though.

Implementation Plan
-------------------
I (Alec Theriault) will implement this change. The work is already
done in `Phab:D5220 <https://phabricator.haskell.org/D5220>`_.
