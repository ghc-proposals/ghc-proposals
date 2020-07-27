Overloaded Quotations
=====================

.. author:: Matthew Pickering
.. date-accepted:: 2019-12-06
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/2247
.. implemented:: 9.0
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/246>`_.
.. contents::


This proposal is about making quotation brackets (typed and untyped) polymorphic. The motivation
is so that quotes can be used without being tied to the ``Q`` monad and to
enable effects during code generation.


Background
------------

(Skip to "Motivation" if you have a good grasp of how Template Haskell quotation brackets work).

A Template Haskell quotation ``[| ... |]`` is just syntactic sugar for
combinators from ``Language.Haskell.TH.Lib``.

For example, the following expressions are equivalent:

* ``[| f 5 $(spl) |]``
* ``appE (appE (varE 'f) (litE (integerL 5))) spl``

A quotation ``[| ... |]`` has type ``Q Exp``, and it is also the type on which
the combinators operate::

  appE :: Q Exp -> Q Exp -> Q Exp
  varE :: Name -> Q Exp
  litE :: Lit -> Q Exp
  integerL :: Integer -> Lit

The ``Q`` monad is defined as follows::

  newtype Q a = Q { unQ :: forall m. Quasi m => m a }

And ``Quasi`` is a class that supports various Template Haskell operations:

* Name generation: ``qNewName :: Quasi m => String -> m Name``
* Error reporting: ``qReport  :: Quasi m => Bool -> String -> m ()``
* Reification: ``qReify :: Quasi m => Name -> m Info``
* Input/output: ``qRunIO :: Quasi m => IO a -> m a``

In order to get the ``Exp`` out of ``[| ... |] :: Q Exp``, the user needs to
run ``Q``, and that requires an instance that would implement all of the
operations above::

  main = do
    exp <- runQ [| 1 + 2 |]
    print exp

  -- output:
  --   InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 2)))

The available instances are ``Quasi Q`` and ``Quasi IO`` (the latter panics on
reification).

The only operation of ``Quasi`` required to desuar quotations is ``qNewName``,
which is used to bind new names (e.g. to generate a fresh ``x`` in ``[| let x =
... in ... |]``). Other operations of ``Quasi`` may be used by the inner
splices, but not by the combinators used to desugar the quotation, such as
``appE``, ``varE``, or ``litE``.

Motivation
------------

Quoting an expression ``[| e |]`` yields its representation. In the current
implementation the type of representations is ``Q Exp``. However, there are a few
issues with this:

1. Given a quote ``[| e ||] :: Q Exp`` then it should be possible to extract the
   representation of ``e`` with nothing more than a name supply. At the moment in
   order to extract the ``Exp`` from the quotation it is necessary to provide a
   "fake" ``Quasi`` instance which stubs out all the methods. This is undesirable
   and unsafe. (see Appendix A: ``PureQ``).

2. Using more effects than ``Q`` provides (e.g. adding a ``ReaderT`` context)
   requires manual wrapping of each quote and manual unwrapping of each splice.

This proposal has three parts to it:

* Define a dedicated class for fresh name generation, with an operation much
  like the existing ``qNewName :: Quasi m => String -> m Name``::

   class Monad m => Quote m where
      newName :: String -> m Name

  The notable difference is that this is not bundled with other operations of
  ``Quasi``. This means that ``Quote`` can be implemented by a mere ``State
  NameSupply``::

    type NameSupply = Int
    instance Quote (State NameSupply) where
      newName s = state $ \i -> (mkNameU s i, i + 1)

* Generalize the types of combinators from ``Language.Haskell.TH.Lib`` to use
  ``Quote``::

    -- old type
    appE :: Q Exp -> Q Exp -> Q Exp

    -- new type
    appE :: forall m. Quote m => m Exp -> m Exp -> m Exp

* Generalize the type of quotation brackets from ``Q Exp`` to
  ``forall m. C m => m Exp``, where the constraint ``C`` is the conjunction of
  ``Quote`` and all constraints required by the splices within the quotation.

  Let's say we have ``spl1 :: MonadState s m => m Exp`` and ``spl2 ::
  MonadReader r m => m Exp``, then::

   [| $(spl1) $(spl2) |] :: (Quote m, MonadState s m, MonadReader r m) => m Exp

  Why this type? Easy: consider the desugared version::

    appE spl1 spl2

  Here, GHC would emit ``Quote m`` from the use of ``appE``, ``MonadState s m``
  from the use of ``spl1``, and ``MonadReader r m`` from the use of ``spl2``,
  resulting in::

    appE spl1 spl2 :: (Quote m, MonadState s m, MonadReader r m) => m Exp

  The same process happens with the ``[| ... |]`` syntactic sugar.

Detaching quotations from ``Q`` makes way for a form of "pure" Template Haskell
so there is no need to invoke ``Q`` in order to create the representation of an
expression. The most immediate application is the ability to purely
manipulate ``Exp`` values in user libraries::

  lamPlus1 :: Exp
  lamPlus1 = (runParse us [| \x -> x + 1 |])



Another benefit is that in a cross compilation setting a "pure" quote can be
fully evaluated on the host and then the generated code compiled for the target.
Certain effects in the ``Q`` monad mean that currently all splices have to be
evaluated on the target which leads to significant complication when
cross-compiling.

In a similar fashion, we can overload the type of a typed quotation::

  lamPlus :: TExp (Int -> Int)
  lamPlus = (runParse us [|| \x -> x + 1 ||])

Due to the implementation of a typed quotation being already in terms of
untyped syntax, the implementation of this is natural.

Motivation 2: Effectful Code Generation
---------------------------------------

Jamie Willis provides additional motivation for the generalisation of the
quotation bracket. Whilst writing multi-stage programs it is almost inevitable
that you will need to perform effects whilst doing the code generation.
For example, this is from the abstract of Kameyama, Kiselyov and Shan's (2014) Combinators
for impure yet hygienic code generation:

    Code generation is the leading approach to making high-performance software reusable. Effects are indispensable in code generators, whether to report failures or to insert let-statements and if- guards.

To be precise, in his parser combinator library which is implemented using
typed template haskell the following effects are necessary.

1. Use the ``Reader`` monad in order to carry around an environment.
2. Use a let-insertion effect to automatically insert lets to avoid code duplication.
3. Use an exception monad to automatically insert missing dependencies for mutually recursive code generation.

Even using the simple ``Reader`` monad is awkward at the moment::

   generateLoop :: String -> Reader CodeMap (Q Exp)
   generateLoop name = ask (\codeMap -> [|
     let loopyCode x =
       $(runReader loopBody (Map.insert name [|loopyCode|] codeMap))
     in loopyCode ...
     |])

The effect must be explicitly run in each splice. For state or other more complicated
effects this approach doesn't work. With the proposal we would hope to write something like::

   generateLoop :: (MonadReader CodeMap m, Quote m) => String -> m Exp
   generateLoop name = [|
     let loopyCode x =
       $(local (Map.insert name [|loopyCode|]) loopBody)
     in loopyCode ...
     |]

and directly use the ``local`` function inside the nested splice just like normal
monadic programming.


Proposed Change Specification
-----------------------------

The goal of the changes is for an expression ``e : T`` to give the
representation ``[| e |] : Quote m => m Exp``. Several steps are necessary to
make this change possible.

1. Define the interface for ``Quote``::

      class Monad m => Quote m where
         newName :: String -> m Name

   These are all the operations which are necessary to build the representation
   of expressions.

2. Generalise all the combinators which build syntax in ``Language.Haskell.TH.Lib``.
   Due to an `audit <https://github.com/ghc-proposals/ghc-proposals/issues/211#issuecomment-472092412>`_
   conducted by Richard, it was found that the only effect from
   ``Q`` which was used is the ``newName`` function which generates a fresh
   name. All the other combinators can be defined using the ``Monad``
   operations.

   For example, the ``appE`` combinator which constructs an application is
   generalised to ``Quote m => m Exp -> m Exp -> m Exp``, the ``varE`` function
   to ``Quote m => Name -> m Exp`` and the ``lamE`` function to ``Quote m => [m Pat] -> m Exp -> m Exp``.
   In general any ``ExpQ`` type is replaced with ``m Exp``, ``PatQ`` with ``m Pat`` and so on.

3. Generalise the ``Lift`` type class::

      class Lift a where
         lift :: Quote m => a -> m Exp

   This is necessary so that implicit lifting can continue to work without
   enforcing strong constraints on the type of the bracket.

4. Refine the rules to do with splicing.  The type of
   a quotation depends on the types of the nested splices inside it::

      -- Add a redundant constraint to demonstrate that constraints on the
      -- monad used to build the representation are propagated when using nested
      -- splices.
      f :: (Quote m, C m) => m Exp
      f = [| 5 | ]

      -- f is used in a nested splice so the constraint on f, namely C, is propagated
      -- to a constraint on the whole representation.
      g :: (Quote m, C m) => m Exp
      g = [| $f + $f |]

   A top-level splice still requires its argument to be of type ``Q Exp``.
   So then splicing in ``g`` will cause ``m`` to be instantiated to ``Q``::

    h :: Int
    h = $(g) -- m ~ Q

5. The types of type, pattern and declaration quotes will also
   be generalised in the same manner.

6. Typed quotations are similarly generalised::

    i :: Quote m => m (TExp (Int -> Int))
    i = [|| \x -> x + 1 ||]


   If at a later point `(Proposal 195) <https://github.com/ghc-proposals/ghc-proposals/pull/195>`_ ``Q (TExp a)`` is turned into a newtype then an extra
   parameter to indicate the monad used will be added to the wrapper::

    i :: Quote m => Code m (Int -> Int)
    i = [|| \x -> x + 1 ||]

   The monad will be exposed in the newtype to support user-defined effects
   during code generation but retaining the newtype so that the typed representation
   can still be placed into maps and instances defined easily for it.

7. The types of ``untypeQ`` and ``unsafeTExpCoerce`` are generalised in the natural
   manner::

    untypeQ :: Quote m => m (TExp a) -> m Exp
    unsafeTExpCoerce :: Quote m => m Exp -> m (TExp a)



Effect and Interactions
-----------------------

When making an interface more general it is important to think about whether it
will affect type inference. If there are functions where we have to generalise
the argument type but not the result then generalisation can result in
ambiguity in the composition.

It doesn't seem to me that there will be any problems with ambiguity here as
the types of splices is not overloaded in the same manner.

Due to the monomorphism restriction, unannotated top-level bindings will no
longer typecheck by default::

  module A where

  -- Fails to typecheck due to unsolved constraint m
  foo = [| 5 |]

It is easy to workaround this in a backwards compatible way by either adding a
type signature or turning on ``NoMonomorphismRestriction``.


Interaction with Lift
.....................

The main breakage from this patch comes from modifying the type signature for
``lift``.

Instances defined using ``DeriveLift`` will continue to work because they are
defined in terms of quotation brackets.

Instances written in terms of the combinators from ``Language.Haskell.TH.Lib``
will continue to work because these combinators will be generalised.

Instances written in terms of ``Q`` will no longer work. For users to migrate
an additional class ``LiftQ`` could be defined which has the old interface.
This would mean users need to explicitly lift but there are likely only a few
instances which fall into this category if any at all. Neither myself (mpickering)
or Ryan Scott know of any instances. If you define a ``Lift`` instance using ``Q``
then it depends on the context where ``lift`` is invoked, for example it may
depend on what identifiers are in scope or the location the splice is run.
This is undesirable anyway for ``Lift``
instances because the compiler inserts calls to ``lift`` in order to resolve
variables used across stages it is very unpredicable the context in which the ``Q``
actions will be invoked.

Definition of Quote
...................

Richard observes that ``Language.Haskell.TH.Lib.Internal.numTyLit`` calls
``fail`` from the ``Q`` monad. This call to ``fail`` can be replaced with
a call to ``error``. It will still be executed at compile-time but with a
potentially slightly worse error message. The alternative is to
also add this effect to the ``Quote`` type class.

Other Effects
.............

Vlad points out that you don't need to very strict about the types of
expressions in splices. Each nested splice could have different constraints::

      f :: Quasi m => m Exp
      g :: MonadIO m => m Exp
      [| putStrLn $(f) >> putStrLn $(g) |] :: (Quote m, Quasi m, MonadIO m) => m Exp

If one of the nested splices has a specific type, for instance ``Q Exp``, then
the type of the whole expression is fixed to be ``Q Exp``.


Costs and Drawbacks
-------------------

* The generalisation of untyped brackets does not seem like it will cause
  any significant breakage but it's hard to predict.
* The modification to the ``Lift`` interface could cause user-written instances
  to break but users should not define their own instances anyway.
  ``DeriveLift`` is the blessed manner in which to define a ``Lift`` instance.

Alternatives
------------

* The main alternative to the design would be to only require a ``Quote``
  constraint when the quotation requires the ``newName`` effect. For example,
  ``[| 5 |] :: Monad m => m Exp``. I am opposed to this direction as it
  breaks abstraction. The implementation detail of how ``[| 5 |]`` is desugared
  leaks to the user.

  It could be argued that this is different to how ``MonadFail`` constraints are
  desugared. In a similar situation the desugaring gives rise to a constraint the
  user has to satisfy. The key difference in this case is that the ``Quote`` constraint
  is very easy to satisfy and can be implemented with a simple name supply.
  If it turns out to be necessary then at a later point relaxing the constraints
  placed on the combinators in a backwards compatible way.

Unresolved Questions
--------------------

* Carter points out that if you want to achieve "pure" template haskell then
  you still need to deal with the fact that different platforms have different
  representations of primitive data types. This is out of scope of this
  proposal.

* It would also be possible to make ``Quote`` a superclass of ``Quasi`` but
  this hierarchy refactoring seems unecessary.

Implementation Plan
-------------------

* I (mpickering) will implement this.

Appendix A: ``PureQ``
---------------------

``PureQ`` is an instance of ``Quasi`` that could be used for extracting ``Exp``
out of a ``Q Exp`` generated by a quotation. It is unsafe due to the error
calls, and would become safe with this proposal implemented::

  module PureQ (runPureQ) where

  import Control.Monad.Trans.State
  import Control.Monad.IO.Class
  import Control.Monad.Fail
  import Language.Haskell.TH (Q, runQ)
  import Language.Haskell.TH.Syntax (Quasi(..), mkNameU)

  newtype PureQ a = MkPureQ (State Int a)
    deriving newtype (Functor, Applicative, Monad)

  runPureQ :: Q a -> a
  runPureQ m = case runQ m of MkPureQ m' -> evalState m' 0

  instance MonadFail PureQ where
    fail = error

  instance MonadIO PureQ where
    liftIO = error "PureQ: liftIO"

  instance Quasi PureQ where
    qNewName s = MkPureQ $ state $ \i -> (mkNameU s i, i + 1)
    qReport = error "PureQ: qReport"
    qRecover = error "PureQ: qRecover"
    qLookupName = error "PureQ: qLookupName"
    qReify = error "PureQ: qReify"
    qReifyFixity = error "PureQ: qReifyFixity"
    qReifyInstances = error "PureQ: qReifyInstances"
    qReifyRoles = error "PureQ: qReifyRoles"
    qReifyAnnotations = error "PureQ: qReifyAnnotations"
    qReifyModule = error "PureQ: qReifyModule"
    qReifyConStrictness = error "PureQ: qReifyConStrictness"
    qLocation = error "PureQ: qLocation"
    qAddDependentFile = error "PureQ: qAddDependentFile"
    qAddTempFile = error "PureQ: qAddTempFile"
    qAddTopDecls = error "PureQ: qAddTopDecls"
    qAddForeignFilePath = error "PureQ: qAddForeignFilePath"
    qAddModFinalizer = error "PureQ: qAddModFinalizer"
    qAddCorePlugin = error "PureQ: qAddCorePlugin"
    qGetQ = error "PureQ: qGetQ"
    qPutQ = error "PureQ: qPutQ"
    qIsExtEnabled = error "PureQ: qIsExtEnabled"
    qExtsEnabled = error "PureQ: qExtsEnabled"
