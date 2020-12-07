Qualified do
============

.. author:: Facundo Domínguez, Arnaud Spiwack, Matthías Páll Gissurarson
.. date-accepted:: 2020-05-21
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/18214
.. implemented:: 9.0
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/216>`_.
.. contents::

This proposal introduces a new extension ``-XQualifiedDo`` which makes it possible to overload the meaning of a do-notation expression *on a case-by-case basis* (as opposed to the global effect of ``-XRebindableSyntax``), by writing ``builder.do``. The design is inspired by F♯'s  `computation
expressions <https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-express
ions>`_.

Motivation
------------

There are many kinds of monad-like things out there:

* monads
* indexed monads
* graded monads
* relative monads
* linear variants of all the above once `linear types
  <https://github.com/ghc-proposals/ghc-proposals/pull/111>`_ are
  implemented

  * in particular, the linear IO monad from the `Linear Haskell paper
    <https://arxiv.org/abs/1710.09756>`_ is an example of a linear
    graded monad
* …

All of these are theoretically compatible with the do-notation. And, in fact, really want the do-notation to work for them. After all, the entire existence of the do-notation can be ascribed to: “it's really annoying to program with ``(>>=)`` all the time”.

The prescribed solution is to use ``-XRebindableSyntax``. But ``-XRebindableSyntax`` is a very blunt instrument:

* It affects many syntactic constructs (numerical literals, the ``if then else`` syntax, … (see the `full list <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RebindableSyntax>`_)).
* It implies ``-XNoImplicitPrelude``
* It uses the same rebinding for all the do-expressions in an entire file.

You may not want all this. For instance, with linear types ``if then else`` syntax cannot be meaningfully rebound to a function. And there is no reason why a file wouldn't have a piece code referring to a monad, one to a graded monad, and one to a linear relative monad.

Proposed Change Specification
-----------------------------

This proposal creates a new language extension ``-XQualifiedDo``.

When ``-XQualifiedDo`` is activated, the syntax of the ``do`` notation is changed to

::

  <lexp> ⟶ [modid.]do

where ``modid`` stands for some module name.

The additional module name is called the qualifier of the do-expression.

The semantics of ``do`` notation statements is given as follows (using
``-XLambdaCase`` notation):

* The ``x <- u`` statement uses ``(modid.>>=)``

  ::

    M.do { x <- u; stmts }  =  u M.>>= \x -> M.do { stmts }

* The ``u`` statement uses ``modid.(>>)``

  ::

    M.do { u; stmts }  =  u M.>> M.do { stmts }

* The a ``pat <- u`` statement uses ``M.fail`` for the failing case,
  if such a case is needed

  ::

    M.do { pat <- u; stmts }  =  u M.>>= \case
      { pat -> M.do { stmts }
      ; _ -> M.fail "…"
      }

  If the pattern cannot fail, then we don't need to use ``M.fail``.

  ::

    M.do { pat <- u; stmts }  =  u M.>>= \case pat -> M.do { stmts }

* ``-XApplicativeDo`` uses ``(M.<$>)``, ``(M.<*>)`` and ``M.join`` (this
  assumes that the applicative-do grouping has been performed)

  ::

    M.do { (x1 <- u1 | … | xn <- un); M.return e }  =
      (\x1 … xn -> e) M.<$> u1 M.<*> … M.<*> un

    M.do { (x1 <- u1 | … | xn <- un); stmts }  =
      M.join ((\x1 … xn -> M.do { stmts }) M.<$> u1 M.<*> … M.<*> un)


  Note that ``M.join`` is only needed if the final expression is
  not identifiably a ``return``.

  When the applicative statements contain nested statements (see the
  `wiki page
  <https://gitlab.haskell.org/ghc/ghc/wikis/applicative-do>`_ for a
  complete description of applicative-do statements), we also need a
  ``M.return``. *e.g.*

  ::

    M.do { ({stmt1; …; stmtn} {x1; …; xn} | y <- u) ; M.return e }  =
      (\(x1,…,xn) y -> e) <$> (M.do { stmt1; …; stmtn; M.return (x1, …, xn)}) <*> u

*  With ``-XRecursiveDo``, ``rec`` blocks use ``M.mfix`` and ``M.return``:

   ::

     M.do { rec { x1 <- u1; … ; xn <- un }; stmts }  =
       M.do
       { (x1, …, xn) <- M.mfix (\~(x1, …, xn) -> M.do { x1 <- u1; …; xn <- un; M.return (x1, …, xn)})
       ; stmts
       }

If a name ``M.op`` is required by the desugaring process (and only if it's required!) but the name is not in scope, an error message like the following is produced:

* “Desugaring statement <stmt> requires <M.op> which is not in scope”

The qualified operations are subject to the same type restrictions as their counterparts with ``-XRebindableSyntax``.

When the qualifier ``modid.`` is omitted, the meaning of ``do { … }`` is the
same as if ``-XQualifiedDo`` is *not* in effect.

Examples
--------

``-XQualifiedDo`` does not affect ``return`` in the monadic ``do`` notation.

::

  import qualified Some.Monad.M as M

  boolM :: (a -> M.M Bool) -> b -> b -> a -> M.M b
  boolM p a b x = M.do
      px <- p x     -- M.>>=
      if px then
        return b    -- Prelude.return
      else
        M.return a  -- M.return

``-XQualifiedDo`` does not affect explicit ``(>>=)`` in the monadic ``do`` notation.

::

  import qualified Some.Monad.M as M
  import Data.Bool (bool)

  boolMM :: (a -> M.M Bool) -> M b -> M b -> a -> M.M b
  boolMM p ma mb x = M.do
      p x >>= bool ma mb   -- Prelude.>>=

Nested ``do`` blocks do not affect each other's meanings.

::

  import qualified Some.Monad.M as M

  f :: M.M SomeType
  f = M.do
      x <- f1                 -- M.>>=
      f2 (do y <- g1          -- Prelude.>>=
             g2 x y
         )
    where
      f1 = ...
      f2 m = ...
      g1 = ...
      g2 x y = ...

An example of linear ``do`` blocks follows, mixed with non-linear
``do``.

::

  {-# LANGUAGE LinearTypes #-}
  {-# LANGUAGE NoImplicitPrelude #-}
  module Control.Monad.Linear (Monad(..)) where

  class Monad m where
    return :: a #-> m a
    (>>=) :: m a #-> (a #-> m b) #-> mb

  -----------------

  module M where

  import qualified Control.Monad.Linear as Linear

  f :: Linear.Monad m => a #-> m b
  f a = Linear.do
    b <- someLinearFunction a Linear.>>= someOtherLinearFunction
    c <- anotherLinearFunction b
    Linear.return c

  g :: Monad m => a -> m b
  g a = do
    b <- someNonLinearFunction a >>= someOtherNonLinearFunction
    c <- anotherNonLinearFunction b
    return c

An example of graded monads follows, mixed with linear monads.

::

  {-# LANGUAGE ConstraintKinds #-}
  {-# LANGUAGE PolyKinds #-}
  {-# LANGUAGE TypeFamilies #-}
  module Control.Monad.Graded (GradedMonad(..)) where

  import Data.Kind (Constraint)

  class GradedMonad (m :: k -> * -> *) where
    type Unit m :: k
    type Plus m (i :: k) (j :: k) :: k
    type Inv  m (i :: k) (j :: k) :: Constraint
    (>>=) :: Inv m i j => m i a -> (a -> m j b) -> m (Plus m i j) b
    return :: a -> m (Unit m) a

  -----------------

  module M where

  import Control.Monad.Graded as Graded
  import Control.Monad.Linear as Linear

  g :: GradedMonad m => a -> m SomeTypeIndex b
  g a = Graded.do
    b <- someGradedFunction a Graded.>>= someOtherGradedFunction
    c <- anotherGradedFunction b
    Graded.return c

  f :: Linear.Monad m => a #-> m b
  f a = Linear.do
    b <- someLinearFunction a Linear.>>= someOtherLinearFunction
    c <- anotherLinearFunction b
    Linear.return c

An example of super monad follows.

::

  {-# LANGUAGE ConstraintKinds #-}
  {-# LANGUAGE PolyKinds #-}
  {-# LANGUAGE TypeFamilies #-}
  module Control.Monad.Super (Bind(..), Return(..)) where

  import Data.Kind (Constraint)

  class (Functor m, Functor n, Functor p) => Bind m n p where
    type BindCts m n p :: Constraint
    type BindCts m n p = ()
    (>>=) :: (BindCts m n p) => m a -> (a -> n b) -> p b

  class Functor m => Return m where
    type ReturnCts m :: Constraint
    type ReturnCts m = ()
    return :: (ReturnCts m) => a -> m a

  -----------------

  module M where

  import qualified Control.Monad.Super as Super
  import qualified Control.Monad.Linear as Linear

  g :: a -> SomeSuperMonad b
  g a = Super.do
    b <- someSuperFunction a Super.>>= someOtherSuperFunction
    c <- anotherSuperFunction b
    Super.return c

  f :: Linear.Monad m => a #-> m b
  f a = Linear.do
    b <- someLinearFunction a Linear.>>= someOtherLinearFunction
    c <- anotherLinearFunction b
    Linear.return c

Other examples
~~~~~~~~~~~~~~

Composing functions

::

  module Control.Category.QualifiedDo where
    import Control.Category

    (>>) :: Category cat => cat a b -> cat b c -> cat a c
    (>>) = (>>>)

  -----------------

  module X where
    import Control.Category.QualifiedDo as Cat

    k = Cat.do f; g; h

    k2 :: Double -> String
    k2 = Cat.do
      (*3)
      show
      map ord
      maximum
      show

Constructing lists

::

  module List where

    (>>) :: a -> [a] -> [a]
    (>>) = (:)

  -----------------

  import qualified List

  list :: [String]
  list = List.do
    "I"
    "am"
    "lazy"
    []

Constructing heterogeneous lists

::

  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE GADTs #-}
  {-# LANGUAGE KindSignatures #-}
  {-# LANGUAGE TypeOperators #-}
  module HList where

    import qualified Data.Kind

    data HList (xs :: [Data.Kind.Type]) where
      HNil :: HList '[]
      HCons :: x -> HList xs -> HList (x ': xs)

    (>>) :: x -> HList xs -> HList (x ': xs)
    (>>) = HCons

  -----------------

  import HList

  list :: HList '[Char, String, Bool]
  list = HList.do
    'c'
    "is smaller than"
    True
    HNil

Monoids

::

  module Data.Monoid.QualifiedDo where

    (>>) :: Monoid a => a -> a -> a
    (>>) = (<>)

  -----------------

  module X where
    import Data.Monoid.QualifiedDo as Monoid
    import Data.Map (singleton)

    f = Monoid.do
      singleton "one" 1
      singleton "two" 2
      singleton "three" 3

  -----------------

  {-# LANGUAGE OverloadedLabels #-}
  module Y where
    import Data.Monoid.QualifiedDo as Monoid
    import SomeFictitiousHTML

    htmlPage :: HTML
    htmlPage = Monoid.do
      #head Monoid.do
        #title "Welcome visitor!"
      #body Monoid.do
        #h1 "This is a webpage"
        #p Monoid.do "(Ugly one, but "; #em "it works!"; ")"


Effect and Interactions
-----------------------

``-XQualifiedDo`` makes it possible to choose, for each of the individual do-expressions, what kind of monad-like notion they are about. Even if the monad-like notion doesn't support all the range of desugaring (for instance it doesn't have a ``fail``), this will still work, as long as the do-expression doesn't use the corresponding feature (in our example: pattern-binders).

For instance we could write operations for monoids:

::

  module Data.Monoid.QualifiedDo where

    (>>) :: Monoid a => a -> a -> a
    (>>) = (<>)

  module X where
    import Data.Monoid.QualifiedDo as Monoid

    f = Monoid.do
      Sum 2
      Sum 3
      Sum 5
      Sum 8

If one would try to use ``x <- u`` with ``Monoid.do``, GHC would
raise an error *even if there is a value for* ``(>>=)`` *in scope*.

Enabling ``-XQualifiedDo`` doesn't change the meaning of existing do-expressions.

When both ``-XQualifiedDo`` and ``-XRebindableSyntax`` are enabled, ``-XQualifiedDo`` only affects qualified ``do``\ s and ``-XRebindableSyntax`` affects the unqualified ``do``\ s.

``-XQualifiedDo`` doesn't affect monad comprehensions. But given some suitable syntax,
it would be possible to extend ``-XQualifiedDo`` to support them.

``-XQualifiedDo`` doesn't affect the `do notation for arrow commands <https://downloads.haskell.org/~ghc/8.8.2/docs/html/users_guide/glasgow_exts.html#do-notation-for-commands>`_ either. We defer analysis and handling of this case for the future.

Costs and Drawbacks
-------------------

The do-expression stores, during type-checking, which expression they will use for ``(>>=)``, etc… So the core infrastructure is actually already there. We anticipate the cost of implementation and maintenance of this feature to be very low.

Alternatives
------------

Do with builders
~~~~~~~~~~~~~~~~

The initial version of the proposal was inspired by F♯'s `computation expressions <https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions>`_.

When ``-XQualifiedDo`` is activated, the syntax of the ``do`` notation would change to

::

  <lexp> ⟶ [<aexp>.]do

``aexp`` means that the notation before the ``.`` is parsed as a variable, unless there are parentheses.

The additional expression is called the *builder* of the do-expression. The following restrictions apply to the builder and its type.

* expr must **have the fully settled type** ``T``.
* There is a type ``R`` such that normalizing ``T`` with respect to type
  families yields a type of the form ``R T0 … Tn``.
* ``R`` must be a datatype with precisely one constructor ``K``.
* ``K`` must be a record constructor, defining fields with any of the following names:
  ``(>>=)``, ``(>>)``, ``fail``, ``return``, ``<*>``, and ``<$>``.

We say that an expression **has the fully settled type** ``T`` when

* it is of the form ``e :: T``, or
* it is an identifier imported from another module with type ``T``, or
* it is of the form ``expr @ty`` where `expr` **has a fully settled type**
  ``forall a. T``, or
* it is of the form ``expr1 expr2`` where ``expr1`` **has a fully settled type** ``T1 -> T``.

The semantics of ``do`` notation statements is given as follows (using
``-XLambdaCase`` notation and fresh variables ``v, v1, …, vn``):

* The ``x <- u`` statement uses the ``(>>=)`` field of the builder

  ::

    b.do { x <- u; stmts }  =  case b of K { (>>=) = v } ->
                                 v u (\x -> b.do { stmts })
* The ``u`` statement uses the ``(>>)`` field of the builder

  ::

    b.do { u; stmts }  =  case b of K { (>>) = v } ->
      v u (b.do { stmts })

* The a ``pat <- u`` statement uses the ``fail`` field of the builder for the
  failing case, if such a case is needed

  ::

    b.do { pat <- u; stmts }  =  case b of K { (>>=) = v1, fail = v2 } ->
                                   v1 u (\case
                                     { pat -> b.do { stmts }
                                     ; _ -> v2 "…"
                                     })

  If the pattern cannot fail, then we don't need to use ``fail`` field in the
  builder.

  ::

    b.do { pat <- u; stmts }  =  case b of K { (>>=) = v } ->
                                   v u (\case pat -> b.do { stmts })

* ``-XApplicativeDo`` uses the ``(<$>)``, ``(<*>)`` and ``join`` fields
  of the builder (this assumes that the applicative-do grouping has been
  performed)

  ::

    b.do { (x1 <- u1 | … | xn <- un); return e }  =
      case b of K { (<*>) = v1, (<$>) = v2 } ->
        (\x1 … xn -> e) `v2` u1 `v1` … `v1` un

    b.do { (x1 <- u1 | … | xn <- un); stmts }  =
      case b of K { (<*>) = v1, (<$>) = v2, join = v3 } ->
        v3 (\x1 … xn -> b.do { stmts }) `v2` u1 `v1` … `v1` un


  Note that a ``join`` field is only needed if the final expression is
  not identifiably a ``return``.

  When the applicative statements contain nested statements (see the
  `wiki page
  <https://gitlab.haskell.org/ghc/ghc/wikis/applicative-do>`_ for a
  complete description of applicative-do statements), we also need a
  ``return`` field. *e.g.*

  ::

    b.do { ({stmt1; …; stmtn} {x1; …; xn} | y <- u) ; return e }  =
      case b of K { (<*>) = v1, return = v2 } ->
        (\(x1,…,xn) y -> e) <$> (b.do { stmt1; …; stmtn; v2 (x1, …, xn)}) `v1` u

* With ``-XRecursiveDo``, ``rec`` blocks use the ``mfix`` and ``return``
  fields of the builder:

  ::

    b.do { rec { x1 <- u1; … ; xn <- un }; stmts }  =
      case b of K { mfix = v1, return = v2 } ->
        b.do
        { (x1, …, xn) <- v1 (\~(x1, …, xn) -> b.do { x1 <- u1; …; xn <- un; v2 (x1, …, xn)})
        ; stmts
        }

It is, crucially, not required that the record projections be in scope unqualified (otherwise projections of various builders would shadow one-another).

If a field is required by the desugaring process (and only if it's required!) but the builder's type doesn't have such a field, an error message is produced:

* “Desugaring statement <stmt> requires <field name> but builder <builder expression> doesn't provide it”

When the qualifier ``<aexp>.`` is omitted, the meaning of ``do { … }`` is the
same as if ``-XQualifiedDo`` is *not* in effect.

Examples of builders
++++++++++++++++++++

``-XQualifiedDo`` does not affect ``return`` in the monadic ``do`` notation.

::

  import qualified Some.Monad.M as M

  boolM :: (a -> M.M Bool) -> b -> b -> a -> M.M b
  boolM p a b x = M.builder.do
      px <- p x     -- M.>>=
      if px then
        return b    -- Prelude.return
      else
        M.return a  -- M.return

``-XQualifiedDo`` does not affect explicit ``(>>=)`` in the monadic ``do`` notation.

::

  import qualified Some.Monad.M as M
  import Data.Bool (bool)

  boolMM :: (a -> M.M Bool) -> M b -> M b -> a -> M.M b
  boolMM p ma mb x = M.builder.do
      p x >>= bool ma mb   -- Prelude.>>=

Nested ``do`` blocks do not affect each other meanings.

::

  import qualified Some.Monad.M as M

  f :: M.M SomeType
  f = M.builder.do
      x <- f1                 -- case M.builder of K { (>>=) } -> (>>=)
      f2 (do y <- g1          -- Prelude.>>=
             g2 x y
         )
    where
      f1 = ...
      f2 m = ...
      g1 = ...
      g2 x y = ...

An example of linear ``do`` blocks follows, mixed with non-linear
``do`` to show what the imports would look like.

::

  {-# LANGUAGE LinearTypes #-}
  {-# LANGUAGE NoImplicitPrelude #-}
  module Control.Monad.Linear.Internal (Monad(..)) where

  class Monad m where
    return :: a #-> m a
    (>>=) :: m a #-> (a #-> m b) #-> mb

  -----------------

  {-# LANGUAGE LinearTypes #-}
  {-# LANGUAGE NoImplicitPrelude #-}
  {-# LANGUAGE RankNTypes #-}
  module Control.Monad.Linear.Builder (linear, LinearBuilder) where

  import qualified Control.Monad.Linear as Linear

  data LinearBuilder m = LinearBuilder
    { (>>=) :: forall a b. m a #-> (a #-> m b) #-> mb
    , return :: forall a. a #-> m a
    }

  linear :: Monad m => LinearBuilder m
  linear = Builder (Linear.>>=) Linear.return

  -----------------

  module Control.Monad.Linear (module X) where

  import Control.Monad.Linear.Builder as X
  import Control.Monad.Linear.Internal as X

  -----------------

  module M where

  import Control.Monad.Linear (linear)
  import qualified Control.Monad.Linear as Linear

  f :: Linear.Monad m => a #-> m b
  f a = linear.do
    b <- someLinearFunction a Linear.>>= someOtherLinearFunction
    c <- anotherLinearFunction b
    Linear.return c

  g :: Monad m => a -> m b
  g a = do
    b <- someNonLinearFunction a >>= someOtherNonLinearFunction
    c <- anotherNonLinearFunction b
    return c

  -- fixing the type to Maybe
  h a = (linear @Maybe).do
    b <- someLinearFunction a Linear.>>= someOtherLinearFunction
    c <- anotherLinearFunction b
    Linear.return c

An example of graded monads follows, mixed with linear monads
to show what the imports would look like.

::

  {-# LANGUAGE ConstraintKinds #-}
  {-# LANGUAGE PolyKinds #-}
  {-# LANGUAGE TypeFamilies #-}
  module Control.Monad.Graded.Internal (GradedMonad(..)) where

  import Data.Kind (Constraint)

  class GradedMonad (m :: k -> * -> *) where
    type Unit m :: k
    type Plus m (i :: k) (j :: k) :: k
    type Inv  m (i :: k) (j :: k) :: Constraint
    (>>=) :: Inv m i j => m i a -> (a -> m j b) -> m (Plus m i j) b
    return :: a -> m (Unit m) a

  -----------------

  {-# LANGUAGE RankNTypes #-}
  module Control.Monad.Graded.Builder (graded, GradedMonadBuilder) where

  import qualified Control.Monad.Graded as Graded

  data GradedMonadBuilder m = GradedMonadBuilder
    { (>>=) :: forall i j a b. Inv m i j => m i a -> (a -> m j b) -> m (Plus m i j) b
    , (>>) :: forall i j a b. Inv m i j => m i a -> m j b -> m (Plus m i j) b
    }

  graded :: GradedMonad m => GradedMonadBuilder m
  graded = GradedMonadBuilder (Graded.>>=) (\a b -> a Graded.>>= const b)

  -----------------

  module Control.Monad.Graded (module X) where

  import Control.Monad.Graded.Builder as X
  import Control.Monad.Graded.Internal as X

  -----------------

  module M where

  import Control.Monad.Graded (graded)
  import qualified Control.Monad.Graded as Graded

  import Control.Monad.Linear (linear)
  import qualified Control.Monad.Linear as Linear

  g :: GradedMonad m => a -> m SomeTypeIndex b
  g a = graded.do
    b <- someGradedFunction a Graded.>>= someOtherGradedFunction
    c <- anotherGradedFunction b
    Graded.return c

  f :: Linear.Monad m => a #-> m b
  f a = linear.do
    b <- someLinearFunction a Linear.>>= someOtherLinearFunction
    c <- anotherLinearFunction b
    Linear.return c

An example of super monad follows.

::

  {-# LANGUAGE ConstraintKinds #-}
  {-# LANGUAGE PolyKinds #-}
  {-# LANGUAGE TypeFamilies #-}
  module Control.Monad.Super.Internal (Bind(..), Return(..)) where

  import Data.Kind (Constraint)

  class (Functor m, Functor n, Functor p) => Bind m n p where
    type BindCts m n p :: Constraint
    type BindCts m n p = ()
    (>>=) :: (BindCts m n p) => m a -> (a -> n b) -> p b

  class Functor m => Return m where
    type ReturnCts m :: Constraint
    type ReturnCts m = ()
    return :: (ReturnCts m) => a -> m a

  -----------------

  {-# LANGUAGE RankNTypes #-}
  module Control.Monad.Super.Builder (super, SuperMonadBuilder) where

  import qualified Control.Monad.Super as Super

  data SuperMonadBuilder = SuperMonadBuilder
    { (>>=) :: forall m n p a b. (Bind m n p, BindCts m n p) => m a -> (a -> n b) -> p b
    , (>>) :: forall m n p a b. (Bind m n p, BindCts m n p) => m a -> n b -> p b
    }

  super :: SuperMonadBuilder
  super = SuperMonadBuilder (Super.>>=) (\a b -> a Super.>>= const b)

  -----------------

  module Control.Monad.Super (module X) where

  import Control.Monad.Super.Builder as X
  import Control.Monad.Super.Internal as X

  -----------------

  module M where

  import Control.Monad.Super (super)
  import qualified Control.Monad.Super as Super

  import Control.Monad.Linear (linear)
  import qualified Control.Monad.Linear as Linear

  g :: a -> SomeSuperMonad b
  g a = super.do
    b <- someSuperFunction a Super.>>= someOtherSuperFunction
    c <- anotherSuperFunction b
    Super.return c

  f :: Linear.Monad m => a #-> m b
  f a = linear.do
    b <- someLinearFunction a Linear.>>= someOtherLinearFunction
    c <- anotherLinearFunction b
    Linear.return c


Comparison with M.do
++++++++++++++++++++

A major difference of ``do`` with a module name (``M.do``), is that no record
of operations needs to be defined. The ``(M.>>=)`` is taken to be whatever
such operation is in scope. For instance ``(M.>>=)`` and ``(M.>>)`` can come
from different modules if they are imported with the same qualifier:

::

  import Some.Module.Defining.Bind as M ((>>=), return)
  import Some.Module.Defining.Then as M ((>>))

  f = M.do
        x <- f
        g
        return x

An advantage of ``M.do`` is that it doesn't need the programmer
to understand a new notion of expressions having fully settled types.
Moreover, no type information is necessary to desugar the do notation.
And lastly, not having to define a builder, ``M.do`` is requires
less from the provider of a monad.

A downside of ``M.do`` is that it requires to bring into scope all the
operations that a ``do`` block needs. In contrast, the builder approach
only requires to bring a single entity into scope: the builder.
This single record is easier to import, export and document.

Another downside is that error messages are less specific. Compare

* “Desugaring statement <stmt> requires a ``fail`` field but builder <builder expression> doesn't provide it”

with

* “Desugaring statement <stmt> requires ``M.fail`` which is not in scope”

In the later case, ``M.fail`` may need a new import statement, or maybe there is
a typo in an import statement, or maybe ``fail`` is just not supported for this
particular use of ``do`` notation. The error in the case of builders admits only
the explanation of ``fail`` not being supported.


More expressions with a fully settled type
++++++++++++++++++++++++++++++++++++++++++

**Having a fully settled type** is a predicate that could be modified
to accept more expressions over time. In particular, the following expressions could
be considered to have a fully settled type:

* Identifiers from before a top-level Template Haskell splice
* Top-level identifiers from previous mutually-recursive groups when there is no monomorphism restriction
* Variables bound with a type signature or arguments to functions defined with a type signature

It has been suggested that the predicate could have other uses as well.
For instance, to identify expressions whose type can be reified in Template
Haskell.


QualifiedDo with operations that are not in scope
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It was suggested in the discussion that we could modify the meaning of
``M.do`` to not require the operations from module ``M`` to be in scope.
The new meaning would be that the name ``M.(>>=)`` in the translation
resolves to any ``(>>=)`` that is exported by any module aliased by the
name ``M``, independently of whether it is in scope (i.e. imported). And
a similar treatment would be given to the other names intervening in the
translation.

The following program, that would have been rejected because ``(Linear.>>=)``
is not in scope, would now be accepted.

::

  module SomeModule where

  import Control.Monad.Linear as Linear ()
  import Control.Monad.Linear as Linear (Monad, return)

  f :: Linear.Monad m => a #-> m b
  f a = Linear.do
    b <- someLinearFunction a
    c <- anotherLinearFunction b
    Linear.return (somePureFunction c)

The purpose of this variation in ``M.do`` would be to increase the set of
programs accepted. The bet is that the compiler could figure out from the module
name alone which operations are meant, without requiring the programmer to bring
them into scope. Some implications of this approach follow.

Firstly, multiple modules can be imported with the same alias ``M``, and more
than one module can export different functions with the same name. In the
following example, ``(M.>>=)`` could refer to either ``(Control.Monad.>>=)``
or ``(Control.Monad.Linear.>>=)``.

::

  import Control.Monad.Linear as M ()
  import qualified Control.Monad as M

  f = M.do
    -- (Control.Monad.>>=) or (Control.Monad.Linear.>>=) ?
    b <- someFunction
    anotherFunction b

  ...

The ambiguity error is a new kind of ambiguity, one which does
not affect explicit uses of ``M.>>=`` but only the ``M.do`` notation.


Another thing to keep in mind is that the programmer can't constrain which
operations are used in her module. Suppose that she wants to get an error
every time a pattern which can fail is used. The following program will be
accepted, regardless of the effort to keep ``Prelude.fail`` hidden. This is
a limitation in the handling of ``do`` with respect to ``RebindableSyntax``.

::

  {-# LANGUAGE NoImplicitPrelude #-}
  module SomeModule where

  import Control.Monad as Prelude (Monad, (>>=), return)

  f :: Prelude.Monad m => a -> m b
  f a = Prelude.do
    [b] <- someFunction a
    anotherFunction b

On a first discussion by the committee, it was noted that the justification
was not strong enough for these modifications. And besides, it would not be
harder to add it later should it be decided in the future that this is worth
the effort.


Desugar to non-standard names
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

During the discussion of this proposal, it was suggested that ``M.do``
could desguar to ``M.qualifiedBind`` instead of ``(M.>>=)``.

Defining ``qualifiedBind`` would make it very clear in the haddocks that
the module is meant to be imported qualified.

On the other hand, using ``(M.>>=)`` would make ``M.do`` more similar to
regular ``do`` expressions, and anyways, ``M`` likely wants to export ``(>>=)``
for explicit use. Thus, there is no need to double export the same operation.

Moreover, an idiom and convention could be established, where modules to be
used in qualified do would have names like ``Control.Linear.QualifiedDo``,
which would provide the desirable “recognizability” that was aimed with
``qualifiedBind``.


Desugar unqualified returns
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Initially, it had been conceived that ``-XQualifiedDo`` should be used
with an unqualified ``return``.

::

  import Control.Monad.Linear as Linear

  g :: Linear.Monad m => a #-> m b
  g a = Linear.do
    b <- someLinearFunction a         -- Linear.>>=
    c <- anotherLinearFunction b      -- Linear.>>=
    return c                          -- Desugared to Linear.return

Unfortunately, it is difficult to characterize the locations at which
return should be desugared or left alone. For instance

::

  import qualified Some.Monad.M as M

  boolM :: (a -> M.M Bool) -> b -> b -> a -> M.M b
  boolM p a b x = M.do
      px <- p x
      y <- if px then
             return b   -- Prelude.return or M.return ?
           else
             return a   -- Prelude.return or M.return ?
      return y          -- Prelude.return or M.return ?

``-XRebindableSyntax`` solves this by affecting every occurrence of
``return``. Following that approach for ``-XQualifiedDo`` would
complicate writing ``do`` blocks where ``return`` is used on a
different monad.

::

  import Control.Monad.Linear (linear)
  import System.IO.Linear (fromSystemIO)
  import qualified System.IO.Linear as Linear

  g :: a #-> Linear.IO b
  g a = linear.do
    b <- fromSystemIO (print () >> return b)   -- Control.Monad.return ?
    return b                                   -- Linear.return

Also, scoping rules would need to be added to deal with nested ``do`` blocks.

::

  import qualified Some.Monad.M as M
  import qualified Some.Monad.N as N

  condMM :: (a -> M.M Bool) -> M b -> M b -> a -> M.M b
  condMM p ma mb x = M.builder.do
      px <- p x
      if px then N.builder.do
        a <- ma
        return a        -- N.return ?
      else do
        b <- mb
        return b        -- M.return ?

This alternative is feasible. But on balance, it is not clear whether it is
worth the cost of working with whatever scoping rules are chosen.

Qualify do with a type class
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It was suggested during the discussion of this proposal, that the ``do``
keyword could be qualified with a type class name like so:

::

  <lexp> ⟶ [<typeclass name>.]do { stmts }

For instance,

::

  f :: [Int] -> m ()
  f xs = MonadFail.do
    [_] <- return xs
    return ()

desugars to

::

  f :: [Int] -> m ()
  f xs = return xs GHC.Base.>>= \case
    [_] -> return ()
    _ -> Control.Monad.Fail.fail "..."

During desugaring of ``TC.do``, the operations ``(>>=)`` and ``fail`` are
looked in ``TC`` and all of its superclasses. In the example,
``Control.Monad.Fail.fail`` is found at ``Control.Monad.Fail.MonadFail``
and ``(GHC.Base.>>=)`` is found at ``GHC.Base.Monad``.

Only the typeclass ``TC`` needs to be in scope. None of its methods, and
none of its superclasses need to be in scope for desugaring to work.

This approach allows to reuse existing type classes for a qualified ``do``,
while still grouping the needed operations in a type class hierarchy.

However, restrictions need to be imposed in the class hierarchies that are
permitted to qualify a ``do``. Otherwise, looking up methods in superclasses
becomes a challenge if ``-XConstraintKinds`` is enabled:

::

  class c => C c where

These restrictions would complicate using the extension.

Another inconvenience of this approach is that when type hierarchies are
not readily available, it would encourage the introduction of type
classes with a single instance only for the sake of qualifying ``do``
blocks. For instance,

::

  class MonoidBuilder m where
    (>>) :: m -> m -> m

  instance Monoid m => MonadBuilder m where
    (>>) = (<>)

Lastly, there is speculation that at some point it could be desirable
to be more flexible about how builders are used. For instance,

::

  (f builder).do { stmts }

where ``f`` is some transformer on builders. It is not possible to be so
succint with ``TypeClass.do``.

Related work
~~~~~~~~~~~~

* One could use ``-XRebindableSyntax`` and use a very general type class which encompasses all monads

  * This was the essence of the `OverloadedDo proposal <https://github.com/ghc-proposals/ghc-proposals/pull/78>`_, though type inference was never solved for this
  * A more recent idea is `supermonads <http://www.cs.nott.ac.uk/~psznhn/Publications/jfp2018.pdf>`_, which solves the type inference issue using a plugin

  It requires somewhat less work (“only” a plugin, rather than a change in GHC's compiler, at least it's more modular), and is more automatic, as the correct functions are picked automatically from the type. But there is no way that this will capture all the desired notion: some restrictions need be imposed for the sake of type inference.

* There is a way to emulate ``-XQualifiedDo`` in current GHC using ``-XRecordWildcards``: have no ``(>>=)`` and such in scope, and import a builder with ``Builder {..} = builder``. It is used in `linear-base <https://github.com/tweag/linear-base/blob/0d6165fbd8ad84dd1574a36071f00a6137351637/src/System/IO/Resource.hs#L119-L120>`_. This is not a very good solution: it is rather a impenetrable idiom, and, if a single function uses several builders, it yields syntactic contortion (which is why shadowing warnings are deactivated `here <https://github.com/tweag/linear-base/blob/0d6165fbd8ad84dd1574a36071f00a6137351637/src/System/IO/Resource.hs#L1>`_)


Extensions
~~~~~~~~~~

Qualified do with parameters
++++++++++++++++++++++++++++

``M.do`` can be extended (or complemented with another language extension)
to pass parameters to the operations during desugaring.

::

  <lexp> ⟶ [<modid>.]do @aexp … @aexp { stmts }

This would allow a user to fix the type of the monad like so

::

  M.do @(@Maybe)
    x <- m
    M.return (x + 1)

which would be equivalent to

::

  (M.>>=) @Maybe m (\x -> M.return @Maybe (x + 1))

Or it could be used to pass information which is available locally

::

  f =
    M.do @x1 @x2
      x <- m
      M.return (x + 1)
    where
      x1 = …
      x2 = …

which would be equivalent to

::

  f =
    (M.>>=) x1 x2 m (\x -> M.return x1 x2 (x + 1))
    where
      x1 = …
      x2 = …


Unresolved Questions
--------------------

None.


Implementation Plan
-------------------

The implementation shouldn't require too much effort. Matthías Páll (`@tritlo <https://github.com/Tritlo>`_) volunteers himself for the attempt, in collaboration with Arnaud (`@aspiwack <https://github.com/aspiwack>`_).
