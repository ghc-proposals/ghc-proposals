Nominal implicit parameters
===========================

.. author:: Lorenzo Tabacchini
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/741>`_.
.. sectnum::
.. contents::

This proposal introduces ``NominalImplicitParams``, a new GHC extension
that provides first-class support for dynamic scoping with robust and predictable semantics.

Motivation
----------
Dynamic scoping is a convenient mechanism to provide context
(configuration, credentials, capabilities, interface implementations...)
to parts of a program that are lexically distant without explicitly threading arguments.

Why not ImplicitParams?
^^^^^^^^^^^^^^^^^^^^^^^
GHC already provides an implementation of dynamic scoping through the
``ImplicitParams`` extension.
However ``ImplicitParams`` suffers from various drawbacks:

* The parameters are plain ``Symbol``'s. There is no way to disambiguate
  parameters from different origins that share the same name.
* Related to the previous point, parameter names cannot be hidden via the module system.
* The resolution strategy is undefined in case of overlap.
* Enabling or disabling the monomorphism restriction changes the semantics.
* The implementation piggybacks on type classes, whose original design assumes global,
  static resolution.
  Re-purposing classes for local constraints complicates the implementation and
  requires ad-hoc logic for the ``IP`` class in various parts of the GHC code base.

Why not ReaderT?
^^^^^^^^^^^^^^^^
``ReaderT`` is a popular alternative to implicit parameters.
It has much more robust semantics but, as a monad, doesn't compose easily
with other effects. Composition is achieved "artificially" by nesting data types (transformers),
which is a clever solution but imposes a certain complexity and
a rigid effect stack.
The interaction between transformers is also subject to `subtle semantic issues
<https://github.com/haskell-effectful/effectful/blob/master/transformers.md>`_.

A common suggestion to avoid this complexity is the pervasive usage of ``ReaderT r IO``
for all computations that require access to the dynamic context.
While this simplifies things, it also has two drawbacks:

* It encourages the usage of a single global environment data-type, bundling
  unrelated pieces of context into a monolithic "God object".
* More importantly, it enables unrestricted IO everywhere,
  even in functions that only need the ``Reader`` part of the ``ReaderT r IO``.

There are solutions to this, notably through the usage of type classes (the so-called "MTL style"),
but they introduce complexity again and suffer from performance issues.

Implicit parameters, on the other hand, are not based on monads, so
they compose naturally with any other effect and preserve the distinction
between IO and pure code.
Moreover the solution presented in this document has no performance
overhead over plain argument passing.

Why not ${MY_FAVOURITE_EFFECT_SYSTEM}?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Effect systems are very powerful tools but they are also very complex.
Since one of the goals of this proposal is to facilitate "Simple Haskell",
we do not consider effect system libraries as viable alternatives.

Nonetheless, ``NominalImplicitParams`` can provide a solid base to implement
a minimalistic yet complete effect system in few lines of code.
See the "lightweight effect system" appendix for a more detailed description
of the idea.

Another idea that is worth exploring is the usage of this extension
*in combination with* an effect system library such as ``Bluefin``,
in which effect handlers are ordinary values.

Why not WithDict?
^^^^^^^^^^^^^^^^^
GHC supports local overriding of class instances via the magic method ``withDict``.
While this mechanism can be used to emulate implicit parameters,
it is not particularly reliable.

* It allows overriding class instances that are already defined statically
  (via standard ``instance`` declarations), thus breaking the expectation of global
  coherence that comes with type classes.
* The resolution strategy is undefined in case of multiple local instances
  for the same type.

See `this GHC issue
<https://gitlab.haskell.org/ghc/ghc/-/issues/25369>`_
for a description of its limitations.

Proposed Change Specification
-----------------------------

Parameter definition
^^^^^^^^^^^^^^^^^^^^
We introduce a new top-level declaration syntax:

::

  data param [univocal] PARAM_CONSTR ty_vars = PAYLOAD_TYPE

Parameters are nominal. They are type constructors that obey standard Haskell
namespacing and visibility rules.

They can have one of two resolution strategies:
"relaxed" (the default) and "univocal"
(defined by adding the ``univocal`` flag to the parameter declaration).

All parameters are inhabitants of the special open kind ``Param``.

It is possible to declare polymorphic parameters, that is parameter constructors that
expect one or more type variables.
The RHS (the type) can refer to the parameter variables,
which means that the payload type can depend on the type arguments passed to
the constructor.

The new syntax is only allowed when the ``NominalImplicitParams`` extension
is enabled.

Changes in the grammar
""""""""""""""""""""""
We define this as an extension to the `Haskell 2010 grammar
<https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-680004.2>`_.
This proposal adds a new production rule to ``topdecl``:

::

  data param [univocal] simpletype = type

Examples
""""""""

::

  -- A parameter named `P`, carrying a value of type `Int`,
  -- with a "relaxed" resolution strategy
  data param P = Int

::

  -- A parameter named `U`, carrying a value of type `Bool`,
  -- with a "univocal" resolution strategy
  data param univocal U = Bool

::

  -- A polymorphic parameter
  data param PP a = [a]

The types
^^^^^^^^^
The constraint ``HasParam :: Param -> Constraint`` indicates that a parameter is in scope.
Crucially, ``HasParam`` is not a type class but is classified as a distinct predicate type.
For this reason it cannot have instances: it can only be bound to values locally.
Moreover it is not allowed as a superclass, as that would break the global coherence
of type class instances.

The built-in type family ``ParamType :: Param -> Type`` reduces to the type of
the values carried by a parameter (the payload type).

Introduction
^^^^^^^^^^^^
The function ``withParam`` brings a value into the dynamic scope.

::

  withParam :: forall p r. ParamType p -> (HasParam p => r) -> r

Elimination
^^^^^^^^^^^
The function ``getParam`` retrieves the value from the nearest enclosing scope.

::

  getParam :: forall p. HasParam p => ParamType p

If the parameter is declared as "univocal", a compilation error is raised
in case multiple values are in scope for the same parameter.

Examples
""""""""
::

  data param P = Int

  >>> withParam @P 1 $ withParam @P 2 $ getParam @P
  2

::

  data param univocal U = Int

  >>> withParam @U 1 $ withParam @U 2 $ getParam @U -- does not typecheck

In the case of polymorphic parameters, the resolution depends on the full
instantiated type, and not only on the parameter constructor.

::

  data param PP a = [a]

  >>> withParam @(PP Int) [1] $ withParam @(PP Bool) [True] $ getParam @(PP Int)
  [1]

Operational semantics
^^^^^^^^^^^^^^^^^^^^^
The semantics of ``withParam`` and ``getParam`` are defined in terms of
elaboration to Core (System FC).

Because we want ``HasParam p`` (the constraint) and  ``ParamType p`` (the value) to share
the same runtime representation, we define an axiom that allows
coercing between the two types without allocating a dictionary wrapper.

We define the elaboration relation ``Γ ⊢ e ↝ e'``, where ``e`` is the source Haskell term
and ``e'`` is the System FC term.

The axiom
"""""""""
``HasParam p`` is representationally equivalent to ``ParamType p``.

::

  axHasParam(p) :: HasParam p ~R ParamType p

Introduction (withParam)
""""""""""""""""""""""""
The ``withParam`` construct applies the continuation ``k`` to the input value ``v``,
cast into the evidence type.

::

  Γ ⊢ v : ParamType p ↝ v'
  Γ ⊢ k : (HasParam p => τ) ↝ k'
  ----------------------------------------------------
  Γ ⊢ withParam @p v k ↝ k' (v' |> sym(axHasParam(p)))

Elimination (getParam)
""""""""""""""""""""""
The ``getParam`` construct locates the evidence ``c``
(the constraint variable bound by the implicit function arrow) and casts it back to the value type.

::

  c : HasParam p ∈ Γ
  ------------------------------------
  Γ ⊢ getParam @p ↝ c |> axHasParam(p)

Proposed Library Change Specification
-------------------------------------
We propose the introduction of a new module in ``ghc-experimental``,
named ``GHC.NominalImplicitParams.Experimental``,
exporting all the definitions described in the previous section:

* ``HasParam``
* ``ParamType``
* ``Param``
* ``withParam``
* ``getParam``

Examples
--------

Relaxed resolution
^^^^^^^^^^^^^^^^^^
::

  data param P = Int

  getP :: HasParam P => Int
  getP = getParam @P

  >>> withParam @P 1 $ withParam @P 2 getP
  2

Univocal resolution
^^^^^^^^^^^^^^^^^^^
::

  data param U = Int

  getU :: HasParam U => Int
  getU = getParam @U

  >>> withParam @U 1 getU
  1

  >>> withParam @U 1 $ withParam @U 2 getU -- doesn't typecheck

Polymorphic parameter
^^^^^^^^^^^^^^^^^^^^^
::

  data param PP a = [a]

  >>> withParam @(PP Int) [1] $
        withParam @(PP Bool) [True] $
          getParam @(PP Int)
  [1]

  >>> withParam @(PP Int) [1] $
        withParam @(PP Int) [2] $
          withParam @(PP Bool) [True] $
            getParam @(PP Int)
  [2]

::

  data param PL (l :: Symbol) = Int

  >>> withParam @(PL "one") 1 $
        withParam @(PL "two") 2 $
          getParam @(PL "one") + getParam @(PL "two")
  3

Local capture
^^^^^^^^^^^^^
One important consequence of the chosen operational semantics is that parameters
can be captured locally or not depending on the signature.

::

  data Param Scope = String

  test1 :: String
  test1 = withParam @Scope "global" $
    let captured :: String -- (this signature can be omitted)
        captured = getParam @Scope
     in withParam @Scope "local" captured

  >>> test1
  "global"

::

  test2 :: String
  test2 = withParam @Scope "global" $
    let nonCaptured :: HasParam Scope => String
        nonCaptured = getParam @Scope
     in withParam @Scope "local" nonCaptured

  >>> test2
  "local"

Parameters as interfaces
^^^^^^^^^^^^^^^^^^^^^^^^
Implicit parameters can be used to implement abstract interfaces whose
behaviour is defined at runtime.
As an example, we can implement an interface for time functions that
allows us to replace the real time-reporting behaviour with mocks
(similarly to the ``monad-time`` package).

::

  module TimeEff where

  -- imports...

  data param TimeP = Time

  type HasTime = HasParam TimeP

  data Time = Time
    { _currentTime :: IO UTCTime
    , _monotonicTime :: IO Double
    }

  runTime :: Time -> (HasTime => r) -> r
  runTime = withParam @TimeP

  currentTime :: HasTime => IO UTCTime
  currentTime = _currentTime (getParam @TimeP)

  monotonicTime :: HasTime => IO Double
  monotonicTime = _monotonicTime (getParam @TimeP)

  defaultTime :: Time
  defaultTime = Time getCurrentTime getMonotonicTime

User of the ``TimeEff`` module can provide mock implementations of ``Time``:

::

  >>> mockTime = Time undefined (pure 42)

  >>> runTime defaultTime monotonicTime
  653934.057901417

  >>> runTime mockTime monotonicTime
  42.0

Effect and Interactions
-----------------------
The solution presented in this proposal does not conflict with any existing
language feature.
However it can constitute a replacement for the existing solutions in some cases.
Let's consider the alternatives one by one.

ImplicitParams
^^^^^^^^^^^^^^
``NominalImplicitParams`` supersedes ``ImplicitParams`` for most use cases.
It cannot however cover uses of ``ImplicitParams`` that rely on global instances
of ``IP``.
The lack of support for default parameter values in this proposal is a deliberate
choice that makes both the semantics and the implementation simpler.

ReaderT
^^^^^^^
The clear distinction between introduction (``runReaderT``)
and modification (``local``) is very hard to replicate in an implementation of implicit parameters.
For this reason, ``NominalImplicitParams`` should not be considered a replacement for ``ReaderT``
but rather an alternative with different trade-offs.

WithDict
^^^^^^^^
We believe that ``NominalImplicitParams`` should be preferred to ``withDict``
in most cases, as the latter relies on incoherent instance resolution
and can lead to unexpected behaviour and bugs.

Costs and Drawbacks
-------------------
This feature is relatively simple to understand and implement.
However it introduces new syntax and new overlapping features to a language that
already presents a multitude of choices that can be overwhelming for beginners
(``where`` vs ``let``, ``pure`` vs ``return``, type families vs functional dependencies, scoped type variables vs type abstractions...).

Backward Compatibility
----------------------
The impact on existing code is 0 (no breakage).

* The keywords ``param`` and ``univocal`` are "soft keywords",
  so they remain valid identifiers in other contexts
* The ``HasParam`` constraint is a new, distinct form in the solver,
  so it does not overlap with the existing class mechanism.

Alternatives
------------

Functional dependency
^^^^^^^^^^^^^^^^^^^^^
One minor drawback of the proposed design is the lack of consistency
with other GHC features.

If we added an additional parameter to ``HasParam``
and replaced ``ParamType`` with a functional dependency,
the API would be more in line with ``IP`` and ``HasField``:

::

  withParam :: forall p a r. a -> (HasParam p a => r) -> r

  getParam :: forall p a. HasParam p a => a

However, given the nominal nature of parameters and the rather
straightforward resolution of ``ParamType``,
this API change doesn't seem to bring much value.
A simple Haddock or HLS lookup is enough to find out the type of a parameter
if needed.

Library implementation
^^^^^^^^^^^^^^^^^^^^^^
A similar functionality can be implemented entirely at library level,
by using a combination of type families, ``withDict`` and ``IP``
(a typechecker plugin is needed for the resolution strategy).

However such a library would not achieve the same level of robustness
and ease of use that this proposal advocates for.
If we want implicit parameters to be a simpler alternative to ``ReaderT``,
then they should be usable without much boilerplate nor advanced tricks.

Moreover typechecker plugins have a high maintenance cost, which
means there is a risk that such a library would go frequently out of sync with the GHC API.

Unresolved Questions
--------------------

* What is the impact on compilation times?
* Is there a way to support unlifted types?
* What about linear parameters?

Implementation Plan
-------------------
I have already implemented `a prototype of these ideas
<https://gitlab.haskell.org/lortabac/ghc/-/tree/nominal-implicit-parameters2?ref_type=heads>`_.

I volunteer to implement the definitive version if the proposal is accepted.

Implementation notes
^^^^^^^^^^^^^^^^^^^^

* A new ``PredType`` is defined, named ``ParamPred``.
  All ``HasParam`` constraints are classified as ``ParamPred``.
* When the solver meets a ``HasParam`` wanted, the matching givens
  are sorted by descending ``TcLevel`` and the first one is picked. 
* ``HasParam`` is defined as a ``newtype`` (to allow for the coercion)
  but has kind ``Constraint``.
* Two wired-in functions (``withParam#`` and ``getParam#``) are defined.
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

The idea is to consider the parameter constructor as the "key" that gives
access to IO, and rely on the visibility mechanism
offered by modules to control which functions have access to the key.

The core of the effect system is simply a newtype over IO with a smart constructor:

::

  module Eff (Eff(runEff), eff) where

  -- imports...

  -- Important: the 'Eff' constructor is hidden!
  newtype Eff a = Eff {runEff :: IO a}
  deriving (Functor, Applicative, Monad)

  -- In order to construct an 'Eff' action,
  -- you need to provide the parameter constructor
  eff :: HasParam p => Proxy p -> IO a -> Eff a
  eff _ = Eff

Now we can modify the ``TimeEff`` module to use the effect system:

::

  -- Important: 'TimeP' is hidden!
  -- This ensures that only the functions defined in this module
  -- can use the 'eff' smart constructor with ``HasTime``
  module TimeEff (HasTime, Time (..), runTime, currentTime, monotonicTime, defaultTime) where

  -- other definitions...

  currentTime :: HasTime => Eff UTCTime
  currentTime = eff (Proxy @TimeP) $ _currentTime (getParam @TimeP)

  monotonicTime :: HasTime => Eff Double
  monotonicTime = eff (Proxy @TimeP) $ _monotonicTime (getParam @TimeP)

Now arbitrary IO is not allowed anymore:

::

  >>> runEff $ runTime defaultTime monotonicTime
  653934.057901417

Of course, it is possible to "cheat" and export a parameter constructor to get
unrestricted IO capabilities. However:

* An effect will still show up in the signatures, since
  any IO performed inside an ``Eff`` computation must be tied to a constraint.
* More generally, the purpose of an effect system is not to protect against
  malicious usage. The promise is that -- as long as the effect modules are implemented
  by the rules -- users of such modules will benefit from fine-grained effect tracking
  in the types.

In fact we may even implement an "unrestricted IO" effect intentionally,
in the same vein as ``MonadIO``.

::

  module IOEff (HasIO, runIO, performIO) where

  -- imports...

  data param univocal IOP = ()

  type HasIO = HasParam IOP

  -- | Run an 'Eff' action in which arbritrary IO is possible through 'performIO'
  runIO :: (HasIO => Eff r) -> Eff r
  runIO = withParam @IOP ()

  -- | Perform an arbitrary IO action inside an 'Eff' one (the equivalent of 'liftIO')
  performIO :: HasIO => IO a -> Eff a
  performIO = eff (Proxy @IOP)

Now unrestricted IO is possible, but it must happen within a ``runIO`` continuation
and requires the ``HasIO`` constraint:

::

  launchMissiles :: HasIO => Eff ()
  launchMissiles = performIO $ putStrLn "Missiles launched!"

  >>> runEff $ runIO $ runTime defaultTime (launchMissiles >> getCurrentTime)
  Missiles launched!
  2026-01-23 13:43:03.09638539 UTC
