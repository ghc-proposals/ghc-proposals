`data` without parameters with -XStandaloneKindSignatures
=========================================================

.. author:: Jakob Brünker
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

When a data declaration with GADT-syntax has been given a standalone kind
signature, any specified parameters are redundant information. This proposal
would enable users to omit them.


Motivation
----------

When writing a `data` declaration with GADT-syntax, there used to two ways to
specify the kind of the type constructor:

1) By directly specifying parameters (optionally with kind signatures), as in

:: haskell

  data Foo a (b :: Bool) where
    MkFoo :: a -> proxy b -> Foo a b

Note that the names of these parameters does not matter, i.e. the following
declaration is equivalent:

:: haskell

  data Foo c (d :: Bool) where
    MkFoo :: a -> proxy b -> Foo a b

With option 2, having to give irrelevant names to these parameters can be
avoided:

2) By Using an inline kind kignature

:: haskell

  data Foo :: Type -> Bool -> Type where
    MkFoo :: a -> proxy b -> Foo a b

Additionally, both options can be combined:

:: haskell

  data Foo a :: Bool -> Type where
    MkFoo :: a -> proxy b -> Foo a b

This information can also be provided by using a standalone kind signature, as
in

:: haskell

  type Foo :: Type -> Bool -> Type
  data Foo where
    MkFoo :: a -> proxy b -> Foo a b

However, the above code, as is, is currently not accepted by GHC - the kind of
``Foo`` has to be additionally and redundantly specified via option 1 or 2 (or
a combination). There is no good reason why giving an inline kind signature
should be enough, yet a standalone kind signature shouldn't be. Hence this
proposal renders the above code acceptable.

Proposed Change Specification
-----------------------------

In ``data`` or ``newtype`` declarations with GADT-syntax and a standalone kind
signature, if no kind signature follows the parameter list following the
``data`` or ``newtype`` keyword, the status quo is that ``n`` parameter names
must be given in this list, assuming the standalone kind signature specifies an
``n``-ary kind. These parameters may have inline kind signatures matching the
standalone kind signature.

The change suggested in this proposal instead allows between zero and ``n``
(inclusive) parameter names to be given.

Examples
--------

Given the standalone kind signature ``type Foo :: Type -> Bool -> Type``, these
are all accepted:

:: haskell

  data Foo
  data Foo a
  data Foo a b
  data Foo :: Type -> Bool -> Type
  data Foo a :: Bool -> Type
  data Foo a b :: Type

As well as any variations that include inline kind signatures for ``a`` or
``b``.

There are some illustrative real-world examples in the singletons library that
already use standalone kind signatures today. Old lines are prefixed with
``-``, the replacing new lines made possible by this proposal are prefixed with
``+``:

:: haskell

    -- GADT data
    type SomeSing :: Type -> Type
  - data SomeSing k where
  + data SomeSing where
      SomeSing :: Sing (a :: k) -> SomeSing k

    -- GADT newtype
    type WrappedSing :: k -> Type
  - newtype WrappedSing a where
  + newtype WrappedSing where
      WrapSing :: forall k (a :: k). { unwrapSing :: Sing a } -> WrappedSing a

    -- Empty data declaration (also GADT-syntax)
    type TyFun :: Type -> Type -> Type
  - data TyFun a b
  + data TyFun

Effect and Interactions
-----------------------

Since the change merely accepts programs that were previously disallowed, it is
fully backward compatible. As it relies on standalone kind signatures, it is
implicitly guarded behind the ``-XStandaloneKindSignatures`` extension.

Costs and Drawbacks
-------------------

The proposed change is a fairly minor one that doesn't affect the Parser and is
essentially identical to already existing behaviour for inline kind signatures,
so the implementation and maintenance costs should be low.

Alternatives
------------
None aside from keeping the status quo.

Unresolved Questions
--------------------
None.


Implementation Plan
-------------------
I (Jakob Brünker) will implement this proposal.
