Overloaded Quotations
=====================

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
.. sectnum::
.. contents::

This proposal is about making quotation brackets polymorphic. The motivation
is so that quotes can be used without being tied to the ``Q`` monad.


Motivation
------------

Quoting an expression ``[| e |]`` yields its representation. In the current implementation
the type of representations is ``Q Exp``. This proposal is changing the type of
the representation to the polymorphic ``Quote m => m Exp``. The ``Quote`` interface
defines the operations which are necessary to construct the code representation::

   class Applicative m => Quote m where
      newName :: String -> m Name
      fail :: String -> m Name

An instance for ``Quote`` can be implemented
for ``Q``, retaining backwards compatibility, but also ``State NameSupply`` to
enable a pure way to extract expressions.

``Q`` is not necessary for the implementation of quotations.
It provides much more power than is necessary. Quotations are used in order to
construct a representation of expressions, the only effect which is used in the
implementation is a name generation effect.

Detaching quotations from ``Q`` makes way for a form of "pure" template haskell
which can be fully evaluated on the host without having to run effects on the target.


Proposed Change Specification
-----------------------------

The goal of the changes is for an expression ``e : T`` to give the representation
``[| e |] : Quote m => m Exp``. Several steps are necessary to make this change possible.

1. Define the interface for ``Quote``::

      class Applicative m => Quote m where
         newName :: String -> m Name
         fail :: String -> m Name

   These are all the operations which are necessary to build the representation
   of expressions.

2. Generalise all the combinators to build syntax in ``Language.Haskell.TH.Lib``.
   Due to an `audit <https://github.com/ghc-proposals/ghc-proposals/issues/211#issuecomment-472092412>`_ conducted by Richard, it was found that the only effect from
   ``Q`` which was used is the ``newName`` function which generates a fresh name.
   All the other combinators can be defined using the ``Applicative`` operations.


3. Generalise the ``Lift`` type class::

      class Lift a where
         lift :: Quote m => a -> m Exp
         liftT :: Quote m => a -> m (TExp a)

   This is necessary so that implicit lifting can continue to work without
   enforcing strong constraints on the type of the bracket.

4. Refine the rules to do with splicing. A top-level splice instantiates ``m ~ Q``
   and operates as before. The use of nested brackets doesn't enforce any
   specific constraints on ``m``. A quotation which contains splices inherits
   the constraints on the representation.

Proposed Change Specification (Typed Template Haskell)
------------------------------------------------------

There is another useful axis of polymorphism for typed quotations.
It is useful to be polymorphic in the type constructor
``p``. This can be used to embed quotations directly into an EDSL and write
instances for typed quotations which isn't convenient without the polymorpism.

Example 1: Creating a partially static data type::

      data Expr m a where
         Leaf :: Code m a -> Expr a
         Int :: Int -> Expr Int
         Add :: Expr Int -> Expr Int -> Expr Int

      data Code m a = Code (m (TExp a))

      instance TypedQuote Expr where
         fromBracket = Leaf . Code

      expr = [|| 5 ||] `Add` [|| 10 ||]

Example 2: A stage polymorphic interpreter::

      data Code m a = Code (m (TExp a))

      instnace TypedQuote Code where

      weaken :: Code WQ a -> WQ (TExp a)

      class Ops r where
         _int :: Int -> r Int
         _plus :: r Int -> r Int -> r Int

      instance Ops (Code WQ) where
         _int x = [|| x ||]
         _plus x1 x2 = [|| $$(weaken x1) + $$(weaken x2) ||]

      instance Ops Identity where
         _int x = return x
         _plus = liftA2 (+)

It was agreed in `proposal 195 <https://github.com/ghc-proposals/ghc-proposals/pull/195>`_
that this was a sensible idea.

5. Overload the type of typed quotations as well. ``[|| e ||] :: (Quote m, TypedQuote p) => p m a``::

      class TypedQuote p where
         fromBracket :: m (TExp a) -> p m a

   ``fromBracket`` is a generalisation of ``unsafeTExpCoerce`` which allows the
   type of resulting representation to be modified. This is not a backwards
   compatible change because the current type of quotatations is ``Q (TExp a)`` for which
   you can't write an instance of ``QuoteT``.



Effect and Interactions
-----------------------

When making an interface more general it is important to think about whether
it will affect type inference. If there are functions where we have to generalise
the argument type but not the result then generalisation can result in ambiguity
in the composition.

It doesn't seem to me that there will be any problems with ambiguity here as the
types of splices is not overloaded in the same manner.


Interaction with Lift
.....................

The main breakage from this patch comes from modifying the type signature for
``lift``.

Instances defined using ``DeriveLift`` will continue to work because they are
defined in terms of quotation brackets.

Instances written in terms of the combinators from ``Language.Haskell.TH.Lib`` will
continue to work because these combinators will be generalised.

Instances written in terms of ``Q`` will no longer work. For users to migrate
an additional class ``LiftQ`` could be defined which has the old interface. This
would mean users need to explicitly lift but there are likely only a few instances
which fall into this category if any at all.

Interaction with Typed Template Haskell
.......................................

The changes to typed template haskell are not
backwards compatible because you are unable to write an ``TypedQuote`` instance for ``Q (TExp a)``
without using ``Compose`` or a specific newtype. There is more discussion about
why making this change is a good idea anyway in `proposal 195 <https://github.com/ghc-proposals/ghc-proposals/pull/195>`_.

Connection with StaticPtrs
..........................

The style of overloading for typed quotations is already implemented for static
pointers (with similar motivation).

Definition of Quote
...................

Richard observes that ``Language.Haskell.TH.Lib.Internal.numTyLit`` calls
``fail`` from the ``Q`` monad.

Therefore ``fail`` should also be added as a member of
``Quote``. Desugaring to ``error`` results in a run-time error rather than a
compile-time error as in the correct implementation.

Other Effects
.............

Vlad points out that you don't need to very strict about the types of
expressions in splices. Each nested splice could have different constraints::

      f :: Quasi m => m Exp
      g :: MonadIO m => m Exp
      [| putStrLn $(f) >> putStrLn $(g) |] :: (Applicative m, Quasi m, MonadIO m) => m Exp

If one of the nested splices has a specific type, for instance ``Q Exp``, then
the type of the whole expression is fixed to be ``Q Exp``.


Costs and Drawbacks
-------------------

* The generalisation of untyped brackets does not seem like it will cause
  any significant breakage but it's hard to predict.
* The modification to the ``Lift`` interface could cause user-written instances
  to break but users should not define their own instances anyway. ``DeriveLift``
  is the blessed manner in which to define a ``Lift`` instance.
* The changes to the typed template haskell quotation is in line with the untyped
  variant but will be a breaking change to user programs. The number of users of
  typed template haskell is however very small so this shouldn't stop the adoption of
  the proposal.

Alternatives
------------

* It could be argued that ``OverloadedSyntax`` should be enabled for these
  extensions.

Unresolved Questions
--------------------

* Carter points out that if you want to achieve "pure" template haskell then
  you still need to deal with the fact that different platforms have different
  representations of primitive data types. This is out of scope of this proposal
  though.

* It would also be possible to make ``Quote`` a superclass of ``Q`` but
  this hierarchy refactoring seems unecessary.

Implementation Plan
-------------------

* I (mpickering) will implement this.
