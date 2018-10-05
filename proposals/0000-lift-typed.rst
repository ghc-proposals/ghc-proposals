Add ``liftTyped`` to the ``Lift`` typeclass
===========================================

.. proposal-number:: 
.. trac-ticket:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/175>`_.
.. sectnum::
.. contents::

The ``Lift`` typeclass provides a way of lifting values into untyped Template Haskell expressions. ::

    class Lift t where
      lift :: t -> Q Exp
      default lift :: Data t => t -> Q Exp

However, there is no easy way to lift values into *typed* Template Haskell expressions (`Trac #14671 <https://ghc.haskell.org/trac/ghc/ticket/14671>`_). This is suboptimal since the type information required to produce such expressions is readily available. We propose to add a new method called ``liftTyped`` to ``Lift`` to take advantage of this. ::

    class Lift t where
      lift :: t -> Q Exp
      liftTyped :: t -> Q (TExp t)

See also `this thread on the libraries list <https://mail.haskell.org/pipermail/libraries/2018-January/028409.html>`_.


Motivation
------------
Typed Template Haskell expressions (see ``TExp`` in `template-haskell <http://hackage.haskell.org/package/template-haskell>`_) are expression ASTs annotated with the type of the expression the AST represents. Using GHC's *typed quotes* and *typed splices* it is possible to combine and manipulate typed expressions while maintaining confidence that the generated code will typecheck properly and have a certain known type. ::

    ghci> :set -XTemplateHaskell +t
    ghci> x = [|| True && False ||]   -- this is a typed quote
    x :: Q (TExp Bool)
    ghci> y = [|| True && "foo" ||]

    <interactive>: error:
        • Couldn't match expected type ‘Bool’ with actual type ‘[Char]’
        • In the second argument of ‘(&&)’, namely ‘"foo"’
          In the Template Haskell quotation [|| True && "foo" ||]
          In the expression: [|| True && "foo" ||]

These are guarantees that we do not get from using untyped Template Haskell expressions (see ``Exp`` in `template-haskell <http://hackage.haskell.org/package/template-haskell>`_), quotes, and splices. ::

    ghci> x' = [| True && False |]   -- this is an untyped quote
    x' :: Q Exp
    ghci> y' = [|| True && "foo" ||]
    y' :: Q Exp

Template Haskell provides an ``unsafeTExpCoerce :: Q Exp -> Q (TExp a)`` function for when a user is only able to produce an untyped ``Exp`` but knows that this expression has type ``a``. Since the ``lift`` function returns an untyped expression, it can't be spliced into a typed quote without first calling ``unsafeTExpCoerce``. This is unfortunate and completely avoidable since ``lift`` actually does know the type of its argument. ::

    ghci> import Language.Haskell.TH.Syntax
    ghci> f = False
    ghci> x'' = [|| True && $$(unsafeTExpCoerce (lift f)) ||]
    x'' :: Q (TExp Bool)

Proposed Change Specification
-----------------------------
Add a ``typedLift`` method to the ``Lift`` typeclass and define default implementations of ``lift`` and ``typedLift`` in terms of each other. ::

    class Lift t where
      lift :: t -> Q Exp
      lift = unTypeQ . liftTyped

      liftTyped :: t -> Q (TExp t)
      liftTyped = unsafeTExpCoerce . lift

      {-# MINIMAL lift | liftTyped #-}


For completeness, here is what ``Lift`` looks like today: ::

    class Lift t where
      lift :: t -> Q Exp
      default lift :: Data t => t -> Q Exp
      lift = liftData


Effect and Interactions
-----------------------
Following the last example from the motivation section, it would now be possible to write: ::

    ghci> f = False
    ghci> x'' = [|| True && $$(liftTyped f) ||]
    x'' :: Q (TExp Bool)

Costs and Drawbacks
-------------------
The main drawback to this proposal is that it can break existing instances of ``Lift`` which rely on the current default implementation of the `lift` method. These cases are:

* empty instances of ``Lift`` ::
  
       {-# LANGUAGE DeriveDataTypeable #-}
       
       import Data.Data (Data, Typeable)
  
       data Foo = MkFoo Int deriving (Typeable, Data)
       instance Lift Foo
  
* instances of ``Lift`` that are derived via ``-XDeriveAnyClass`` ::

       {-# LANGUAGE DeriveDataTypeable, DeriveAnyClass, DerivingStrategies #-}
       
       import Data.Data (Data, Typeable)
  
       data Foo = MkFoo Int deriving (Typeable, Data) deriving anyclass (Lift)

On the other hand, ``-XDeriveLift`` has been the recommended way to derive ``Lift`` instances for a while now. Furthermore, code that would be broken could easily be unbroken in a fully backwards-compatible fashion with ``instance Lift Foo where { lift = liftData }``.

Alternatives
------------
There are a handful of other ways to introduce ``liftTyped``.

* As a standalone top-level funtion ::

      liftTyped :: Lift t => t -> Q (TExp t)
      liftTyped = unsafeTExpCoerce . lift

  This means instances of ``Lift`` can't be directly defined in terms of typed expression quotations.

* As the sole method of the ``Lift`` typeclass ::

      class Lift t where
        liftTyped :: t -> Q (TExp t)
        default liftTyped :: Data t => t -> Q (TExp t)
        liftTyped = unsafeTExpCoerce . liftData

      lift :: Lift t => t -> Q Exp
      lift = unTypeQ . liftTyped
      
  This breaks any existing manually defined ``Lift`` instances in a way that can only be worked around via with CPP.

Unresolved Questions
--------------------
None that I am aware of.

Implementation Plan
-------------------
I (Alec Theriault) will implement this change. The work is already
done in `Phab:D5198 <https://phabricator.haskell.org/D5198>`_.
