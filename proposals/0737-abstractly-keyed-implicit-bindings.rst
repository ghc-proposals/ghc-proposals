Abstractly-keyed Implicit Bindings
==================================

.. author:: Shea Levy
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/737>`_.
.. sectnum::
.. contents::

Currently, implicit parameters are bound and looked up via Haskell identifiers specified concretely in source code. This proposal enables more abstract (but still compile-time) population of and lookup from the dynamic scope, with arbitrarily-kinded keys. In particular, it enables type-driven dynamic scoping.

Motivation
----------

Using only public interfaces, the dynamic scoping enabled by ``ImplicitParams`` does not allow for any abstraction over which parameters are managed implicitly; the binding site, the constraint site, and the use site must hard code the specific variable names to reference. This proposal extends ``ImplicitParams`` to allow for greater abstraction in interfaces using dynamic scoping.

For a motivating example, consider checked exceptions, where each function must declare which exceptions it can throw and callers can only call throwing functions if they also throw the same exceptions or if they handle the exceptions somehow. Today, we have at least one implementation of this in `bluefin <https://hackage-content.haskell.org/package/bluefin-0.2.4.0/docs/Bluefin-Exception.html>`_, but this requires referencing a value-level handle at the throw site. Ideally, we'd like to be able to simply declare "this function throws ``FooException`` s) and then have ``throw MkFooException`` automatically do the right thing in the common case where there is only one handler for a given exception type. Without relying on implementation details of implicit parameters, the best we can do is:

::

 type ThrowsInt e = (?int :: Exception Int e)
 type ThrowsBool e = (?bool :: Exception Bool e)

 throwInt :: forall e es a. (ThrowsInt e, e :> es) => Int -> Eff es a
 throwInt = throw ?int

 throwBool :: forall e es a. (ThrowsBool e, e :> es) => Bool -> Eff es a
 throwBool = throw ?bool

 tryInt :: forall es a. (forall e. (ThrowsInt e) => Eff (e :& es) a) -> Eff es (Either Int a)
 tryInt go = try go'
   where
     go' :: forall e. Exception Int e -> Eff (e :& es) a
     go' cap = let ?int = cap in go
 tryBool :: forall es a. (forall e. (ThrowsBool e) => Eff (e :& es) a) -> Eff es (Either Bool a)
 tryBool go = try go'
   where
     go' :: forall e. Exception Bool e -> Eff (e :& es) a
     go' cap = let ?bool = cap in go

 -- ghci> example
 -- True
 example :: Bool
 example = case runPureEff (tryBool (tryInt $ throwBool True)) of
   Right (Right v) -> absurd v
   Right (Left _) -> False
   Left b -> b

If we rely on the fact that implicits are implemented in terms of ``GHC.Base.IP`` and that there is a ``GHC.Base.WithDict (GHC.Base.IP x a) a`` instance, we can write:

::

 data family CheckedExceptionTag t :: k

 type Throws ex e = (IP (CheckedExceptionTag ex) (Exception ex e))

 throw' :: forall ex e es a. (Throws ex e, e :> es) => ex -> Eff es a
 throw' = throw (ip @(CheckedExceptionTag ex))

 try' :: forall ex es a. (forall e. (Throws ex e) => Eff (e :& es) a) -> Eff es (Either ex a)
 try' go = try go'
   where
     go' :: forall e. Exception ex e -> Eff (e :& es) a
     go' cap = withDict @(Throws ex e) cap go

 -- ghci> example
 -- True
 example :: Bool
 example = case runPureEff (try' (try' $ throw' True)) of
   Right (Right v) -> absurd v
   Right (Left (_ :: Int)) -> False
   Left b -> b

With this proposal, this could instead look like:

::

 -- No need for open-world data family hack
 data CheckedExceptionTag = MkCheckedExceptionTag Type

 -- More descriptive name than IP, and pulled from the public API
 type Throws ex e = (ImplicitParameter (MkCheckedExceptionTag ex) (Exception ex e))

 throw' :: forall ex e es a. (Throws ex e, e :> es) => ex -> Eff es a
 -- More descriptive name than ip, pulled from the public API, uses required type arguments, known to compile the same as a variable lookup
 throw' = throw (implicitParameter (MkCheckedExceptionTag ex)) -- required type arguments makes this nicer

 try' :: forall ex es a. (forall e. (Throws ex e) => Eff (e :& es) a) -> Eff es (Either ex a)
 try' go = try go'
   where
     go' :: forall e. Exception ex e -> Eff (e :& es) a
     -- Doesn't rely on WithDict implementation detail, uses required type arguments, known to compile the same as a variable binding
     go' cap = bindImplicit (MkCheckedExceptionTag ex) cap go

 -- ghci> example
 -- True
 example :: Bool
 example = case runPureEff (try' (try' $ throw' True)) of
   Right (Right v) -> absurd v
   Right (Left (_ :: Int)) -> False
   Left b -> b

Proposed Change Specification
-----------------------------

1. Add a new declarations to ``GHC.Base``

   ::

    -- | A 'Constraint' that 'implicitParameter' @x@ is an implicit parameter of type @a@.
    --
    -- 'ImplicitParameter' constraints are propagated according to dynamic scoping rules.
    -- In other words, GHC always takes the most nested parameter binding from the context
    -- to find the value. @bindImplict x a (bindImplict x b (implicitParameter x))@
    -- is equivalent to @b@ (including resulting in a type error if used where
    -- a term whose type unifies with @a@'s but not @b@'s is expected).
    --
    -- To bind an implicit parameter, use 'bindImplicit'.
    --
    -- When the @ImplicitParams@ extension is on, @ImplictParameter (MkIdent "foo") t@
    -- is equivalent to @?foo :: t@.
    class ImplicitParameter (x :: k) (a :: Type)

    -- | Bind implicit parameter @x@ to the provided @a@.
    --
    -- When the @ImplicitParams@ extension is on, @bindImplicit (MkIdent "foo") x go@ is
    -- equivalent to @let ?foo = x in go@.
    --
    -- Though 'bindImplicit' is called like a function, it is not really a function call, and at runtime
    -- does not result in any overhead beyond what would occur when passing variables to an equivalent
    -- explicitly-parameterized function. 'bindImplicit' must always be called fully applied.
    bindImplicit :: forall x -> a -> ((ImplicitParameter x a) => b) -> b

    -- | Access implicit parameter bound to @x@.
    --
    -- When the @ImplicitParams@ extension is on, @implicitParameter (MkIdent "foo")@ is
    -- equivalent to @?foo@.
    --
    -- Though 'implicitParameter' is called like a function, it is not really a function call, and at runtime
    -- does result in any overhead beyond what would occur when referencing a variable passed in explicitly.
    -- 'implicitParameter' must always be called fully applied.
    implicitParameter :: forall x -> (ImplicitParameter x a) => a

    -- | The kind of type-level representations of Haskell identifiers.
    data Ident = MkIdent Symbol

    {-# DEPRECATED IP, ip "Use ImplicitParameter and implicitParameter" #-}
    type IP (x :: Symbol) a = ImplicitParameter (MkIdent x) a
    ip :: forall x a. (IP x a) => a
    ip = implicitParameter (MkIdent x)

2. Modify the ``ImplicitParams`` specification to specify that:

   1. ``?ident`` in a value context is equivalent to ``implicitParameter (MkIdent "ident")``
   2. ``?ident :: t`` in a constraint context, including by inference, is equivalent to ``ImplicitParameter (MkIdent "ident") t``
   3. ``{ let ?ident1 = x; ?ident2 = y } in expr`` in a binding context is equivalent to ``bindImplicit (MkIdent "ident1") x (bindImplicit (MkIdent "ident2") y (expr))``

Proposed Library Change Specification
-------------------------------------

1. Reexport ``ImplicitParameter``, ``implicitParameter``, ``bindImplicit``, and ``Ident(..)`` from new ``base`` module ``Data.Implicit``

Examples
--------

See `Motivation`_.


Effect and Interactions
-----------------------
Given current implementation, this change merely provides a nicer and more stable/formally supported interface to functionality that already exists. Relative to the specification, the effects and interactions are:

With this change, developers can manipulate the dynamically scoped environment beyond a set of keys specified explicitly in the source code. Compile-time calculated keys can be bound with ``bindImplicit`` and looked up with ``implicitParameter``.

This change modifies ``ImplicitParams`` in a backwards-compatible manner. But it does constrain the implementation
moving forward.

Developers that don't use the new declarations (even if they do use ``ImplicitParams``) should not notice any change,
except that perhaps some libraries implement interfaces that were previously not possible.

By using ``Data.Implicit`` directly without the ``?ident`` syntax, users can access dynamic scoping functionality without ``ImplicitParams``.

``implicitParameter`` and ``bindImplicit`` use ``RequiredTypeArguments``.


Costs and Drawbacks
-------------------
This should be very cheap to implement and maintain, as it is primarily about guaranteeing the continuation of existing implementation details and providing a nicer interface to them.

I don't think this makes things more difficult for novices relative to things like ``HasCallStack`` already permeating the ecosystem. Sensible library interfaces will not expose ``ImplicitParameter`` directly and will instead define their own constraints, or for simple cases use ``?ident :: ty``-style constraints.


Backward Compatibility
----------------------
This proposal adheres to GR1. No existing users import ``Data.Implicit`` from ``base``, and existing usage of ``ImplicitParams`` and even ``GHC.Base.IP`` will still function properly.

Expected impact is no breakage.

Alternatives
------------

Rely on Implementation Details
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As discussed in the `Motivation`_, all of the functionality desired is currently possible given the way ``ImplicitParams`` happen to be implemented today. Developers can simply use those interfaces until/unless a change happens. I have implemented a version of this in the `abstractly-keyed-implicits package <https://hackage.haskell.org/package/abstractly-keyed-implicits>`_. Relative to this proposal, the implementation has the following downsides:

* It may break if future GHC versions change the implementation of implicit parameters
* It doesn't mandate that ``bindImplicit`` and ``implicitParameter`` be fully applied,
  nor guarantee that they don't impose additional runtime cost.
* It uses some auxiliary internal type-level machinery that can't be hidden.
* It is not inferred as safe Haskell (though it is safe).

Unresolved Questions
--------------------
I'm unsure if the specification of ``bindImplicit`` and ``implicitParameter`` as not really functions is clear enough, or necessary for the spec.


Implementation Plan
-------------------
I'm willing to implement this, though I may need support to enforce the full application constraint.

Acknowledgments
---------------

Thanks to Andrei Borzenkov (@s-and-witch) for pointing out that ``WithDict`` allows for abstract binding today and that the data family hack allows for arbitrarily-kinded implicit lookups today.
