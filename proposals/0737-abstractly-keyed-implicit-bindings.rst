Abstractly-keyed Implicit Bindings
==========================

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

Using only public interfaces, the dynamic scoping enabled by ``ImplicitParams`` does not allow for any abstraction over which parameters are managed implicitly; the binding site, the constraint site, and the use site must hard code the specific variable names to reference. If one reaches for the private interface of ``GHC.Base.IP``, one can abstractly specify variable names at the constraint site and use site, but can still only bind based on concretely specified variable names.

This proposal extends ``ImplicitParams`` to allow for greater abstraction in interfaces using dynamic scoping.

For a motivating example, consider checked exceptions, where each function must declare which exceptions it can throw and callers can only call throwing functions if they also throw the same exceptions or if they handle the exceptions somehow. Today, we have at least one implementation of this in `bluefin <https://hackage-content.haskell.org/package/bluefin-0.2.4.0/docs/Bluefin-Exception.html>`_, but this requires referencing a value-level handle at the throw site. Ideally, we'd like to be able to simply declare "this function throws ``FooException`` s) and then have ``throw MkFooException`` automatically do the right thing in the common case where there is only one handler for a given exception type. We can almost get there today with something like this on top of Bluefin's API:

::

 type family IPName t :: Symbol

 type instance IPName Int = "int"
 type instance IPName Bool = "bool"

 type Throws ex e = (IP (IPName ex) (Exception ex e))

 throw' :: forall ex e es a. (Throws ex e, e :> es) => ex -> Eff es a
 throw' = throw (ip @(IPName ex))

 -- Alas, two copies needed
 tryInt :: forall es a. (forall e. (Throws Int e) => Eff (e :& es) a) -> Eff es (Either Int a)
 tryInt go = try go'
   where
     go' :: forall e. Exception Int e -> Eff (e :& es) a
     go' cap = let ?int = cap in go -- boo, hard-coded identifier
 tryBool :: forall es a. (forall e. (Throws Bool e) => Eff (e :& es) a) -> Eff es (Either Bool a)
 tryBool go = try go'
   where
     go' :: forall e. Exception Bool e -> Eff (e :& es) a
     go' cap = let ?bool = cap in go

 -- ghci> example
 -- True
 example :: Bool
 example = case runPureEff (tryBool (tryInt $ throw' True)) of
   Right (Right v) -> absurd v
   Right (Left _) -> False
   Left b -> b

With this proposal, this could instead look like:

::

 -- No need for a type family or manual stringification
 -- Plus we can use a custom kind to avoid conflict with
 -- other implicits
 data CheckedExceptionTag = MkCheckedExceptionTag Type

 -- More descriptive name than IP, and pulled from the public API
 type Throws ex e = (ImplicitParameter (MkCheckedExceptionTag ex) (Exception ex e))

 throw' :: forall ex e es a. (Throws ex e, e :> es) => ex -> Eff es a
 throw' = throw (implicitParameter (MkCheckedExceptionTag ex)) -- required type arguments makes this nicer

 -- Only one definition needed
 try :: forall ex es a. (forall e. (Throws ex e) => Eff (e :& es) a) -> Eff es (Either ex a)
 try go = try go'
   where
     go' :: forall e. Exception ex e -> Eff (e :& es) a
     go' cap = bindImplicit (MkCheckedExceptionTag ex) cap go

 -- ghci> example
 -- True
 example :: Bool
 example = case runPureEff (try (try $ throw' True)) of
   Right (Right v) -> absurd v
   Right (Left (_ :: Int)) -> False
   Left b -> b

Proposed Change Specification
-----------------------------

1. Add a new declarations to ``GHC.Base``

   ::

    class ImplicitParameter (x :: k) a | x -> a

    implicitParameter :: forall x -> (ImplicitParameter x a) => a

    bindImplicit :: forall x -> a -> ((ImplicitParameter x a) => b) -> b

    data IdentImplicitTag = MkIdentImplicitTag Symbol

    {-# DEPRECATED IP, ip "Use ImplicitParameter and implicitParameter" #-}
    type IP (x :: Symbol) a = ImplicitParameter (MkIdentImplicitTag x) a
    ip :: forall x a. (IP x a) => a
    ip = implicitParameter (MkIdentImplicitTag x)

2. Modify the ``ImplicitParams`` specification to specify that:

   1. ``?ident`` in a value context is equivalent to ``implicitParameter (MkIdentImplicitTag "ident")``
   2. ``?ident :: t`` in a constraint context, including by inference, is equivalent to ``ImplicitParameter (MkIdentImplicitTag "ident") t``
   3. ``{ let ?ident1 = x; ?ident2 = y } in expr`` in a binding context is equivalent to:

      ::

       bindImplicit (MkIdentImplicitTag "ident1") x (
         bindImplicit (MkIdentImplicitTag "ident2") y (expr))

Though ``bindImplicit`` is called like a function, it is not really a function call, and at runtime
must not result in any overhead beyond what would occur when passing variables to an equivalent
explicitly-parameterized function. ``bindImplicit`` must always be called fully applied.

Though ``implicitParameter`` is called like a function, it is not really a function call, and at runtime
must not result in any overhead beyond what would occur when referencing a variable passed in explicitly.
``implicitParameter`` must always be called fully applied.

``bindImplicit`` and ``implicitParameter`` interact according to the existing implicit parameters scoping guarantees. Quoting from the GHC user's manual for ``9.15.20251223``:

  GHC always takes the most nested implicit parameter binding from the context to find the value.

Proposed Library Change Specification
-------------------------------------

1. Reexport ``ImplicitParameter``, ``implicitParameter``, ``bindImplicit``, and ``IdentImplicitTag(..)`` from new ``base`` module ``Data.Implicit``

Examples
--------

See `Motivation`_.


Effect and Interactions
-----------------------
With this change, developers can manipulate the dynamically scoped environment beyond a set of keys specified explicitly in the source code. Compile-time calculated keys can be bound with ``bindImplicit`` and looked up with ``implicitParameter``.

This change modifies ``ImplicitParams`` in a backwards-compatible manner. But it does constrain the implementation
moving forward.

Developers that don't use the new declarations (even if they do use ``ImplicitParams``) should not notice any change,
except that perhaps some libraries implement interfaces that were previously not possible.

By using ``Data.Implicit`` directly without the ``?ident`` syntax, users can access dynamic scoping functionality without ``ImplicitParams``.

``implicitParameter`` and ``bindImplicit`` use ``RequiredTypeArguments``.


Costs and Drawbacks
-------------------
I am uncertain about development costs, though given the way ``IP`` works today I'm hopeful this is not too big of a change internally.

Maintenance should be low to zero compared to solely maintaining ``ImplicitParams`` as it exists today.

I don't think this makes things more difficult for novices relative to things like ``HasCallStack`` already permeating the ecosystem. Sensible library interfaces will not expose ``ImplicitParameter`` directly and will instead define their own constraints, or for simple cases use ``?ident :: ty``-style constraints.


Backward Compatibility
----------------------
This proposal adheres to GR1. No existing users import ``Data.Implicit`` from ``base``, and existing usage of ``ImplicitParams`` and even ``GHC.Base.IP`` will still function properly.

Expected impact is no breakage.

Alternatives
------------

Template Haskell
^^^^^^^^^^^^^^^^

Using ``TemplateHaskell``, we can bind variable implicit variable names. The annoyance and complexity of TH rules this out.

Require ``Symbol`` keys
^^^^^^^^^^^^^^^^^^^^^^^

Rather than making ``ImplicitParams`` polykinded, we could simply add ``bindImplicit`` with a ``Symbol -> Type -> Constraint``-kinded ``ImplicitParams``, and use type families to build user constraints out of other kinds. This will require some manual mapping for Type-based lookups (or some new primitives to get a ``Symbol`` from a type), and would not allow for the same sort of parameter namespacing the proposal allows.

Unresolved Questions
--------------------
I'm unsure if the specification of ``bindImplicit`` and ``implicitParameter`` as not really functions is clear enough, or necessary for the spec.


Implementation Plan
-------------------
I'm willing to implement this, though I may need support.
