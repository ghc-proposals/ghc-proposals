Levity-polymorphic `Lift`
=========================

.. author:: Alec Theriault
.. date-accepted:: 2019-04-17
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/merge_requests/789
.. implemented:: 8.10
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/209>`_.
.. contents::

The ``Lift`` typeclass lets us lift values into typed or untyped Template Haskell expressions.

::

    class Lift t where
      lift :: t -> Q Exp
      liftTyped :: t -> Q (TExp t)

    newtype TExp a = TExp { unType :: Exp }

One shortcoming of this class is that it does not let us lift values whose kind is not ``Type``.
We propose to lift this restriction by making ``Lift`` and ``TExp`` levity polymorphic.

::

    class Lift (t :: TYPE r) where
      lift :: t -> Q Exp
      liftTyped :: t -> Q (TExp t)

    newtype TExp (t :: TYPE r) = TExp { unType :: Exp }

Motivation
------------
The ``Lift`` class cannot currently handle unlifted types. This is a somewhat frustrating limitation
since Template Haskell supports representing unlifted values, only they must be constructed
manually.

::

    x :: Q Exp
    x = LitE (IntPrimL 1)  -- yet we'd like to be able to write `lift 1#`

The fact that ``TExp`` doesn't support unlifted types means that it is less expressive that ``Exp``.
``TExp`` is generally regarded as a more type-safe approach to Template Haskell, so users find
themselves needing to choose between generating more performant code that uses unlifted types and
using the more typesafe ``TExp``. The problems around ``TExp`` don't end there: if you omit a type
signature, one can trick GHC 8.6.3 (and latest HEAD) into accepting invalid ``TExp``!

::

    {-# LANGUAGE MagicHash, TemplateHaskell #-}

    import GHC.Exts
    import Language.Haskell.TH.Syntax

    main = print =<< runQ (unTypeQ [|| I# $$e ||])
      where e = [|| 1# ||] -- :: Q (TExp Int#)
                           --  yet but adding this ^ signature causes a compile error!

The current ``-XDeriveLift`` implementation has some special logic for handling data types with unlifted
fields. Generalizing ``Lift`` would remove the need for any special cases.

Proposed Change Specification
-----------------------------
Change ``TExp``, ``Lift``, ``unTypeQ``, and ``unsafeTExpCoerce`` to take advantage of levity polymorphism:

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

Two important observations:

  1. The ``r ~ 'LiftedRep`` is needed because GHC doesn't know how to handle levity polymorphic
     binders. It does mean that instances of ``Lift`` over unlifted types will need to manually
     implement both ``lift`` and ``liftTyped``.

  2. By not providing a default implementation of ``liftTyped``, any existing empty instances of
     ``Lift`` will crash as opposed to loop. This does have the unpleasant side effect that there
     will be no way to write a CPP-free and backwards-compatible manual instance of ``Lift``.
     However, since the vast majority of instances can (and should!) be derived with
     ``-XDeriveLift``, which *is* backwards compatible, this is not a significant problem.

Add to ``Language.Haskell.TH.Syntax`` a handful of new instances of ``Lift`` which are now valid:

::

    instance Lift Int# where ...
    instance Lift Word# where ...
    instance Lift Float# where ...
    instance Lift Double# where ...
    instance Lift Char# where ...
    instance Lift Addr# where ...

Modify ``-XDeriveLift`` to generate typed and untyped quotes (for ``liftTyped`` and ``lift`` respectively).

Effect and Interactions
-----------------------
The proposed change would make it possible to lift values of unlifted kinds and would make it
(officially) possibly for ``TExp`` to represent expressions of unlifted kinds.

This proposal would also provide a means for trying out levity polymorphism in typeclasses (see
sections 7.3 and 8.1 of the `Levity Polymorphism paper <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/levity-pldi17.pdf>`_).
Although there are `many classes which could benefit from this treatment <https://gitlab.haskell.org/ghc/ghc/issues/12708#note_131157>`_,
``Lift`` is a good first candidate because it isn't too widely used and ``template-haskell``
isn't expected to be as stable and free of breakage as ``base``. There is already `a proposal <https://github.com/ghc-proposals/ghc-proposals/pull/30>`_ which aims to make typeclasses in ``base`` levity-polymorphic.

Costs and Drawbacks
-------------------
The main drawback is that uses of ``lift``, ``TExp``, ``unType``, ``unTypeQ``, and ``unsafeTExpCoerce`` which
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
adding an extra ``@_`` type argument first).

In terms of development and maintainability, the cost is small: the prototype patch that implements
this functionality ends up removing more lines from the compiler than it adds.

The only other drawback is that beginners browsing the documentation for Template Haskell are more
likely to be confused by the complex signature for the default implementations of the ``lift``
method.

Alternatives
------------
None known.

Unresolved Questions
--------------------
If the proposal `Explicit specificity in type variable binders <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0026-explicit-specificity.rst>`_
were already implemented, we might be able to avoid breaking code which uses visible type
application (on any of the functions whose signatures we are proposing to generalize). The idea
would be to specify the ``RuntimeRep`` type variables as inferred. This would come at the cost of
even more complicated type signatures though.

Implementation Plan
-------------------
I (Alec Theriault) will implement this change. The work is already
done in `Phab:D5220 <https://phabricator.haskell.org/D5220>`_.
