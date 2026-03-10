Scoped values
=============

.. author:: Lorenzo Tabacchini
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/741>`_.
.. sectnum::
.. contents::

This proposal introduces ``ScopedValues``, a new GHC extension
that provides first-class support for dynamic scoping with robust and predictable semantics.

Motivation
----------
Dynamic scoping is a convenient mechanism to provide context
(configuration, credentials, capabilities, interface implementations...)
to parts of a program that are lexically distant without explicitly threading arguments.

GHC already provides an implementation of dynamic scoping through the
``ImplicitParams`` extension.
However ``ImplicitParams`` suffers from various drawbacks.

Implicit parameters are strings
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Parameters are plain ``Symbol``'s with no namespacing. This means that:

* There is no way to disambiguate
  parameters from different origins that share the same name.
* Parameter names cannot be hidden via the module system.
  They are always exposed.

Consider a database connection library that exports the following definitions:

::

  type DbConfig = (?timeout :: Int, ?otherDbSettings :: ())

  dbConnect :: DbConfig => IO ()

An HTTP client library that exports the following definitions:

::

  type HttpConfig = (?timeout :: Int, ?otherHttpSettings :: ())

  httpConnect :: HttpConfig => IO ()

And a function that relies on both of them:

::

  doStuff :: (DbConfig, HttpConfig) => IO ()
  doStuff = do
    dbConnect
    httpConnect

Now it is impossible to disambiguate between the two timeouts.
We are forced, either knowingly or accidentally, to set both timeouts to the same value.

MonomorphismRestriction
^^^^^^^^^^^^^^^^^^^^^^^
Enabling or disabling the monomorphism restriction changes the semantics
of implicit parameters.

Consider the following example:

::

  foo :: String
  foo = let ?scope = "global" in
    let capture = ?scope
    in let ?scope = "local" in capture

With ``-XMonomorphismRestriction``, ``foo`` returns ``"global"``.
With ``-XNoMonomorphismRestriction``, ``foo`` returns ``"local"``.

This is not ideal, because toggling the extension for reasons unrelated
to implicit parameters can silently change the behavior of existing code.

Proposed Change Specification
-----------------------------

Parameter definition
^^^^^^^^^^^^^^^^^^^^
We introduce a new top-level declaration syntax:

::

  data scoped KEY_CONSTRUCTOR ty_vars = VALUE_TYPE

Keys are nominal. They are type constructors that obey standard Haskell
namespacing and visibility rules.

All keys are inhabitants of the special open kind ``ScopeKey``.

It is possible to declare polymorphic keys, that is key constructors that
expect one or more type variables.
The RHS (the type) can refer to the key variables,
which means that the value type can depend on the type arguments passed to
the key constructor.

The RHS must be a monotype.
Polymorphic value types such as ``data scoped T = forall a. a -> a`` are not allowed.

The new syntax is only allowed when the ``ScopedValues`` extension
is enabled.

Changes in the grammar
""""""""""""""""""""""
We define this as an extension to the `Haskell 2010 grammar
<https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-680004.2>`_.
This proposal adds a new production rule to ``topdecl``:

::

  data scoped simpletype = type

Examples
""""""""

::

  -- A key named `K`, carrying a value of type `Int`
  data scoped K = Int

::

  -- A polymorphic key
  data scoped KK a = [a]

The types
^^^^^^^^^
The constraint ``Scoped :: ScopeKey -> Constraint`` indicates that a value is in scope
for the given key.
Crucially, ``Scoped`` is not a type class but is classified as a distinct predicate type.
For this reason it cannot have instances: it can only be bound to values locally.
Moreover it is not allowed as a superclass, as that would break the global coherence
of type class instances.

The built-in type family ``ValueType :: ScopeKey -> Type`` reduces to the value type
for the given key.

Introduction
^^^^^^^^^^^^
The function ``withValue`` brings a value into the dynamic scope.

::

  withValue :: forall k r. ValueType k -> (Scoped k => r) -> r

Elimination
^^^^^^^^^^^
The function ``value`` retrieves the value from the nearest enclosing scope.

::

  value :: forall k. Scoped k => ValueType k

Examples
""""""""
::

  data scoped K = Int

  >>> withValue @K 1 $ withValue @K 2 (value @K)
  2

In the case of polymorphic keys, the resolution depends on the full
instantiated type, and not only on the key constructor.

::

  data scoped T a = a

  >>> withValue @(T Int) 1 $ withValue @(T Bool) True $ value @(T Int)
  1

Warnings
^^^^^^^^
Two warnings are introduced:

* **Shadowed scoped value**, enabled by default,
  triggered when a value that is already in scope
  is reintroduced with ``withValue``.
* **Redundant scoped value**, enabled through the ``-Wredundant-scoped-values`` flag,
  triggered when a value is introduced with
  ``withValue`` but never retrieved via a corresponding ``value``.

The **shadowed scoped value** warning can be disabled selectively for a specific
key via the ``{-# ALLOW_SHADOWING #-}`` pragma.

Examples
""""""""

::

  data scoped K = Int
  data scoped L {-# ALLOW_SHADOWING #-} = Int

  >>> withValue @K 1 $ withValue @K 2 (value @K)
  -- Shadowed scoped value warning
  2

  >>> withValue @L 1 $ withValue @L 2 (value @L)
  -- No warnings
  2

  >>> withValue @K 1 $ withValue @L 2 $ value @K
  -- Redundant scoped value warning (L)
  1

The warnings consider the full type, and not the key constructor alone.

::

  >>> withValue @(T Int) 1 $ withValue @(T Int) 2 $ value @(T Int)
  -- Shadowed scoped value warning
  2

  >>> withValue @(T Int) 1 $ withValue @(T Bool) True $ value @(T Int)
  -- Redundant scoped value warning (T Bool)
  1

Operational semantics
^^^^^^^^^^^^^^^^^^^^^
The semantics of ``withValue`` and ``value`` are defined in terms of
elaboration to Core (System FC).

Because we want ``Scoped k`` (the constraint) and  ``ValueType k`` (the value) to share
the same runtime representation, we define an axiom that allows
coercing between the two types without allocating a dictionary wrapper.

We define the elaboration relation ``Γ ⊢ e ↝ e'``, where ``e`` is the source Haskell term
and ``e'`` is the System FC term.

The axiom
"""""""""
``Scoped k`` is representationally equivalent to ``ValueType k``.

::

  axScoped(k) :: Scoped k ~R ValueType k

Introduction (withValue)
""""""""""""""""""""""""
The ``withValue`` construct applies the continuation ``f`` to the input value ``v``,
cast into the evidence type.

::

  Γ ⊢ v : ValueType k ↝ v'
  Γ ⊢ f : (Scoped k => τ) ↝ f'
  ----------------------------------------------------
  Γ ⊢ withValue @k v f ↝ f' (v' |> sym(axScoped(k)))

Elimination (value)
""""""""""""""""""""""
The ``value`` construct locates the evidence ``c``
(the constraint variable bound by the implicit function arrow) and casts it back to the value type.

::

  c : Scoped k ∈ Γ
  ------------------------------------
  Γ ⊢ value @k ↝ c |> axScoped(k)

Proposed Library Change Specification
-------------------------------------
We propose the introduction of a new module in ``ghc-experimental``,
named ``GHC.ScopedValues.Experimental``,
exporting all the definitions described in the previous section:

* ``Scoped``
* ``ValueType``
* ``ScopeKey``
* ``withValue``
* ``value``

Examples
--------

Simple keys
^^^^^^^^^^^

::

  data scoped K = Int

  getK :: Scoped K => Int
  getK = value @K

  >>> withValue @K 1 getK
  1

  >>> withValue @K 1 $ withValue @K 2 getK
  -- Shadowed scoped value warning
  2

Polymorphic keys
^^^^^^^^^^^^^^^^

::

  data scoped KK a = [a]

  >>> withValue @(KK Int) [1] $
        withValue @(KK Bool) [True] $
          value @(KK Int)
  -- Redundant scoped value warning (KK Bool)
  [1]

  >>> withValue @(KK Int) [1] $
        withValue @(KK Int) [2] $
          withValue @(KK Bool) [True] $
            value @(KK Int)
  -- Shadowed scoped value warning (KK Int)
  -- Redundant scoped value warning (KK Bool)
  [2]

  data scoped KL (l :: Symbol) = Int

  >>> withValue @(KL "one") 1 $
        withValue @(KL "two") 2 $
          value @(KL "one") + value @(KL "two")
  3

Local capture
^^^^^^^^^^^^^
One important consequence of the chosen operational semantics is that scoped values
can be captured locally or not depending on the signature.
This behavior is not affected by the monomorphism restriction.

::

  data scoped Scope = String

  test1 :: String
  test1 = withValue @Scope "global" $
    let captured :: String -- (this signature can be omitted)
        captured = value @Scope
     in withValue @Scope "local" captured

  >>> test1
  -- Shadowed scoped value warning
  -- Redundant scoped value warning
  "global"

::

  test2 :: String
  test2 = withValue @Scope "global" $
    let nonCaptured :: Scoped Scope => String
        nonCaptured = value @Scope
     in withValue @Scope "local" nonCaptured

  >>> test2
  -- Shadowed scoped value warning
  -- Redundant scoped value warning
  "local"

Scoped values as interfaces
^^^^^^^^^^^^^^^^^^^^^^^^^^^
Scoped values can be used to implement abstract interfaces whose
behaviour is defined at runtime.
As an example, we can implement an interface for time functions that
allows us to replace the real time-reporting behaviour with mocks
(similarly to the ``monad-time`` package).

::

  module TimeEff where

  -- imports...

  data scoped TimeK = Time

  type HasTime = Scoped TimeK

  data Time = Time
    { _currentTime :: IO UTCTime
    , _monotonicTime :: IO Double
    }

  runTime :: Time -> (HasTime => r) -> r
  runTime = withValue @TimeK

  currentTime :: HasTime => IO UTCTime
  currentTime = _currentTime (value @TimeK)

  monotonicTime :: HasTime => IO Double
  monotonicTime = _monotonicTime (value @TimeK)

  defaultTime :: Time
  defaultTime = Time getCurrentTime getMonotonicTime

User of the ``TimeEff`` module can provide mock implementations of ``Time``:

::

  >>> mockTime = Time undefined (pure 42)

  >>> runTime defaultTime monotonicTime
  653934.057901417

  >>> runTime mockTime monotonicTime
  42.0

Hidden keys
^^^^^^^^^^^
We mentioned earlier that one of the advantages of this proposal is
the ability to hide keys via the module system.
The reasons for hiding a key are fundamentally the same
as for hiding any identifier:
providing cleaner interfaces, hiding implementation details
and enforcing invariants.

Consider the example of a fresh integer supply.
If the key is exposed, a user could shadow the counter and cause collisions.
With implicit parameters, there is no way to prevent this:
the parameter `?counter` is a public name that anyone can rebind.
With scoped keys, we can prevent it by hiding the key.

::

  module Counter (HasCounter, runCounter, next) where

  -- CounterK is not exported
  data scoped CounterK = IORef Int

  type HasCounter = Scoped CounterK

  -- Creates a new IORef and runs the computation
  runCounter :: (HasCounter => IO a) -> IO a

  -- Atomically increments the counter and returns the value
  next :: HasCounter => IO Int

Effect and Interactions
-----------------------
The solution presented in this proposal does not conflict with any existing
language feature.

It can serve as a replacement for most use cases of ``ImplicitParams``.
It cannot however cover uses of ``ImplicitParams`` that rely on global instances
of ``IP``.
The lack of support for default values in this proposal is a deliberate
choice that makes both the semantics and the implementation simpler.

This proposal is not intended as a replacement for ``ReaderT`` or effect systems,
which offer different trade-offs.

Costs and Drawbacks
-------------------
This feature is relatively simple to understand and implement.
However it introduces new syntax and new overlapping features to a language that
already presents a multitude of choices that can be overwhelming for beginners
(``where`` vs ``let``, ``pure`` vs ``return``, type families vs functional dependencies, scoped type variables vs type abstractions...).

Backward Compatibility
----------------------
The impact on existing code is 0 (no breakage).

* The ``scoped`` keyword is a "soft keyword",
  so it remains a valid identifier in other contexts
* The ``Scoped`` constraint is a new, distinct form in the solver,
  so it does not overlap with the existing class mechanism.

Alternatives
------------

Functional dependency
^^^^^^^^^^^^^^^^^^^^^
One minor drawback of the proposed design is the lack of consistency
with other GHC features.

If we added an additional parameter to ``Scoped``
and replaced ``ValueType`` with a functional dependency,
the API would be more in line with ``IP`` and ``HasField``:

::

  withValue :: forall k a r. a -> (Scoped k a => r) -> r

  value :: forall k a. Scoped k a => a

However, given the nominal nature of scoped values and the rather
straightforward resolution of ``ValueType``,
this API change doesn't seem to bring much value.
A simple Haddock or HLS lookup is enough to find out the value type for a key
if needed.

Library implementation
^^^^^^^^^^^^^^^^^^^^^^
A similar functionality can be implemented entirely at library level,
by using a combination of type families, ``withDict`` and ``IP``
(a typechecker plugin is needed to implement the warnings).
The trick is based on the fact that GHC currently accepts an unapplied type family
or data family as the first argument of ``IP``,
without requiring a fully solved ``Symbol``.

However such a library would rely on a GHC quirk (or likely a bug).
It would not achieve the level of robustness
and ease of use that this proposal advocates for.
The aim of this proposal is to provide a simple and reliable solution.
This trick is neither of the two.

Moreover typechecker plugins have a high maintenance cost, which
means there is a risk that such a library would go frequently out of sync with the GHC API.

Unresolved Questions
--------------------

* What is the impact on compilation times?
* Is there a way to support unlifted types?
* What about linear scoped values?

Implementation Plan
-------------------
I have already implemented `a prototype of these ideas
<https://gitlab.haskell.org/lortabac/ghc/-/tree/scoped-values?ref_type=heads>`_.

I volunteer to implement the definitive version if the proposal is accepted.

Implementation notes
^^^^^^^^^^^^^^^^^^^^

* A new ``PredType`` is defined, named ``ScopedPred``.
  All ``Scoped`` constraints are classified as ``ScopedPred``.
* When the solver meets a ``Scoped`` wanted, the matching givens
  are sorted by descending ``TcLevel`` and the first one is picked. 
* ``Scoped`` is defined as a ``newtype`` (to allow for the coercion)
  but has kind ``Constraint``.
* Two wired-in functions (``withValue#`` and ``value#``) are defined.
  During desugaring, these two functions are elaborated according to the
  operational semantics.

Endorsements
-------------

Acknowledgments
---------------

Appendix: a lightweight effect system
-------------------------------------
If we go back to the ``TimeEff`` example above, one obvious limitation of the provided
solution is that the interface functions live in ``IO``.
This means that in order to execute ``currentTime`` and ``monotonicTime``
we need unrestricted IO capabilities.

While the distinction between IO and pure code is already more than what the average
programming language provides, some users may want more granular effect tracking.
Luckily, we don't need a complex effect system library for this.

The idea is to consider the scope key as the key that gives
access to IO, and rely on the visibility mechanism
offered by modules to control which functions have access to it.

The core of the effect system is simply a newtype over IO with a smart constructor:

::

  module Eff (Eff(runEff), eff) where

  -- imports...

  -- Important: the 'Eff' constructor is hidden!
  newtype Eff a = Eff {runEff :: IO a}
  deriving (Functor, Applicative, Monad)

  -- In order to construct an 'Eff' action,
  -- you need to provide the key constructor
  eff :: Scoped k => Proxy k -> IO a -> Eff a
  eff _ = Eff

Now we can modify the ``TimeEff`` module to use the effect system:

::

  -- Important: 'TimeK' is hidden!
  -- This ensures that only the functions defined in this module
  -- can use the 'eff' smart constructor with ``HasTime``
  module TimeEff (HasTime, Time (..), runTime, currentTime, monotonicTime, defaultTime) where

  -- other definitions...

  currentTime :: HasTime => Eff UTCTime
  currentTime = eff (Proxy @TimeK) $ _currentTime (value @TimeK)

  monotonicTime :: HasTime => Eff Double
  monotonicTime = eff (Proxy @TimeK) $ _monotonicTime (value @TimeK)

Now arbitrary IO is not allowed anymore:

::

  >>> runEff $ runTime defaultTime monotonicTime
  653934.057901417

Of course, it is possible to "cheat" and export a key constructor to get
unrestricted IO capabilities. However:

* An effect will still show up in the signatures, since
  any IO performed inside an ``Eff`` computation must be tied to a constraint.
* More generally, the purpose of an effect system is not to protect against
  malicious usage. The promise is that -- as long as the effect modules are implemented
  by the rules -- users of such modules will benefit from fine-grained effect tracking
  in the types.

In fact we may even provide an "unrestricted IO" effect intentionally,
in the same vein as ``MonadIO``.

::

  module Eff (Eff(runEff), eff, HasIO, runEffWithIO, performIO) where

  -- Eff definitions...

  data scoped IOK = PerformIO

  type HasIO = Scoped IOK

  -- | Start an 'Eff' computation in which arbitrary IO
  -- is possible through 'performIO'
  runEffWithIO :: (HasIO => Eff a) -> IO a
  runEffWithIO k = withValue @IOK (PerformIO Eff) (runEff k)

  -- | Perform an arbitrary IO action inside an 'Eff' one (the equivalent of 'liftIO')
  performIO :: HasIO => IO a -> Eff a
  performIO = getPerformIO (value @IOK)

  -- Rank-2 helper
  newtype PerformIO = PerformIO {getPerformIO :: forall r. IO r -> Eff r}

Now unrestricted IO is possible, but it must happen within a ``runEffWithIO`` continuation
and requires the ``HasIO`` constraint:

::

  launchMissiles :: HasIO => Eff ()
  launchMissiles = performIO $ putStrLn "Missiles launched!"

  >>> runEffWithIO $ runTime defaultTime (launchMissiles >> currentTime)
  Missiles launched!
  2026-01-23 13:43:03.09638539 UTC
