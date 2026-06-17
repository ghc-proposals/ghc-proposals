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
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/311>`_.
.. contents::

When a data declaration with GADT-syntax has been given a standalone kind
signature, any specified parameters are redundant information. This proposal
would enable users to omit them.


Motivation
----------

When writing a `data` declaration with GADT-syntax, there used to two ways to
specify the kind of the type constructor:

1) By directly specifying parameters (optionally with kind signatures), as in

::

  data Foo a (b :: Bool) where
    MkFoo :: a -> proxy b -> Foo a b

Note that the names of these parameters do not matter, i.e. the following
declaration is equivalent:

::

  data Foo c (d :: Bool) where
    MkFoo :: a -> proxy b -> Foo a b

With option 2, having to give irrelevant names to these parameters can be
avoided:

2) By using an inline kind signature

::

  data Foo :: Type -> Bool -> Type where
    MkFoo :: a -> proxy b -> Foo a b

Additionally, both options can be combined:

::

  data Foo a :: Bool -> Type where
    MkFoo :: a -> proxy b -> Foo a b

This information can also be provided by using a standalone kind signature, as
in

::

  type Foo :: Type -> Bool -> Type
  data Foo where
    MkFoo :: a -> proxy b -> Foo a b

However, the above code, as is, is currently not accepted by GHC - the kind of
``Foo`` has to be additionally and redundantly specified via option 1 or 2 (or
a combination). There is no good reason why giving a standalone kind signature
shouldn't be sufficient, when giving an inline kind signature *is*. Hence, this
proposal renders the above code acceptable.

Proposed Change Specification
-----------------------------

In ``data`` or ``newtype`` declarations with GADT-syntax and a standalone kind
signature, if no kind signature follows the parameter list following the
``data`` or ``newtype`` keyword, the status quo is that ``n`` parameter names
must be given in this list, assuming the standalone kind signature specifies an
``n``-ary kind. These parameters may have individual inline kind signatures, matching the
standalone kind signature.

The change suggested in this proposal instead allows between zero and ``n``
(inclusive) parameter names to be given, regardless of whether or not a kind signature
follows the parameter list.

Alternatively, the inline kind signature of the declaration may be
a wildcard (``_``), which allows users to more explicitly state that the kind is determined by the
standalone kind signature.

In ``data instance`` declarations with GADT syntax, this also works: If a standalone kind signature has been given
to the ``data family`` declaration, then a data instance declaration without a kind signature (or with a
wildcard as kind signature) is assumed
to have the most general kind possible, i.e. the one given by the standalone kind signature. Note that
this will result in an error if the kind specified by the standalone kind signature has a non-* return kind.

In the context of this proposal, declarations without constructors are also treated as GADT-style declarations.

Examples
--------

Given the standalone kind signature ``type Foo :: Type -> Bool -> Type``, these
are all accepted:

::

  data Foo
  data Foo a
  data Foo a b
  data Foo :: _
  data Foo a :: _
  data Foo a b :: _
  data Foo :: Type -> Bool -> Type
  data Foo a :: Bool -> Type
  data Foo a b :: Type

As well as any variations that include inline kind signatures for ``a`` or ``b``.

There are some illustrative real-world examples in the singletons library that
already use standalone kind signatures today. Old lines are prefixed with
``-``, the replacing new lines made possible by this proposal are prefixed with
``+``:

::

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

    -- Empty data declaration (also treated as GADT-syntax)
    type TyFun :: Type -> Type -> Type
  - data TyFun a b
  + data TyFun
  
  
While less useful, this can also be used for data families:

::

  data family F1 :: Bool -> Type
  data instance F1 -- kind: Bool -> Type
  
  data family F2 :: Bool -> k
  data instance F2 -- error due to non-* return kind

Effect and Interactions
-----------------------

Since the change merely accepts programs that were previously disallowed, it is
fully backward compatible. As it relies on standalone kind signatures, it is
implicitly guarded behind the ``-XStandaloneKindSignatures`` extension.

Costs and Drawbacks
-------------------

This is a fairly minor change, since the compiler already has to ensure that the standalone kind
signature matches the inline kind signature. As such, the implementation and maintenance costs
should be low.

Alternatives
------------
- Fully partial kind signatures could be allowed, rather than just wildcards. Given the marginal
  benefit, this does not seem to be worth it.
- Between making it possible to leave off the kind signature, and to have it be a wildcard, one of these
  two could be dropped. The main reason to keep the first is brevity, the main reason to keep the second
  is allowing the user to be explicit.
- These changes could only apply to declarations that have not been given any paramaters, rather than zero to
  ``n`` parameters.

Unresolved Questions
--------------------
Should ``-XPartialTypeSignatures`` be required for Wildcards?


Implementation Plan
-------------------
I (Jakob Brünker) will implement this proposal.
