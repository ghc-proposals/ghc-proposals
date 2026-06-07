The Grand Plan for Unification of Class Methods and Associated Types
====================================================================

.. proposal-number::
.. ticket-url::
.. implemented::
.. highlight::
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/236>`_.
.. sectnum::
.. contents::

NB. This is a "big picture" proposal, prompted by several other threads on the
GHC Proposals platform. It is not intended to be submitted to the committee in
its current form; rather, it outlines a general direction of travel for other,
less ambitious proposals, to take into account.

The main observation, and premise of this plan, is that with dependent types
(presumably, with ``-XDependentHaskell``), the notion of associated types
should coincide with class methods, just like closed type families coincide
with functions. The stated goal is to design a path toward their convergence.

We propose to add features that allow us to give equal treatment to associated
types and class methods. The reasoning goes like this:

* Class methods are constrained by the parent class, therefore we also
  constrain associated types. The details are worked out in an existing
  proposal, `#177 <https://github.com/ghc-proposals/ghc-proposals/pull/177>`_.
* Associated types may quantify visibly over class variables, while class
  methods cannot. To bridge the gap, we introduce class method headers.
* Visible quantification in class methods implies visible dependent
  quantification in terms, which is a milestone towards Dependent Haskell and
  is mentioned briefy in `#102
  <https://github.com/ghc-proposals/ghc-proposals/pull/102>`_.
* Visible dependent quantification in terms allows us to mix and match terms
  with types, and this entails changes to parsing and namespace resolution.
  Based on `#214 <https://github.com/ghc-proposals/ghc-proposals/pull/214>`_, we
  describe a new namespace resolution strategy, ``-XDefaultNamespace`` and
  namespace-qualified imports.
* Under ``-XDefaultNamespace``, the ``type`` prefix of top-level kind
  signatures becomes unnecessary, so we remove it.

At this point, we end up with the following set of features:

* Visible dependent quantification in terms::

    idv :: forall a -> a -> a
    idv t x = x :: t

    x = idv Double  42     -- x :: Double
    n = idv Integer 42     -- n :: Integer

  The difference with ``-XTypeApplications`` is in the presence of a visibility
  override ``@`` at use sites. This example entails a change to namespace lookup
  rules:

  * In a type position (to the right of ``::`` or in type applications
    ``@``), look in the type namespace first; if the lookup fails, look in
    the data namespace.

  * In a term position (to the right of ``=``), look in the data namespace
    first; if the lookup fails, look in the type namespace.

  Guarded by ``-XVisibleForall`` (old name referenced in the discussion: ``-XVDQ``).

* Default namespace and namespace qualified imports::

    import Data.Proxy type qualified as T
    import Data.Proxy data qualified as D

    f :: forall (a :: k) -> ...

    a = f T.Proxy   -- refers to the type constructor
    b = f D.Proxy   -- refers to the data constructor

    data T = T   -- rejected

  The namespace lookup rules of ``-XVisibleForall`` (outlined in the previous bullet) do
  not allow to specify a namespace explicitly. We solve this with an extended set of rules:

  * In a module with ``-XDefaultNamespace``, all declarations and variables
    are in a single namespace. It is an error to use the same name for a
    type-level entity and a term-level entity. Therefore, there is no
    ambiguity at use sites.

  * In a module with ``-XDefaultNamespace``, entities imported from other
    modules must not result in an ambiguity at use sites. For example, using
    ``Proxy`` is an error, as this name is present in both the type namespace
    and the data namespace.

  * To allow the use of existing code that exports entities in different
    namespaces with the same name, we introduce namespace-qualified imports,
    ``type qualified as`` and ``data qualified as``.

  Guarded by ``-XDefaultNamespace``.

* Class method headers that mirror associated type headers and allow us to
  choose the order and visibility of class variables::

   class C k (a :: k) (b :: k) where
      f k @b @a :: Proxy a -> Proxy b

   -- f :: forall k -> forall b a. C k a b => Proxy a -> Proxy b

  Guarded by ``-XClassMethodHeaders``.

* No ``type`` prefix for top-level kind signatures::

    Functor :: (Type -> Type) -> Constraint
    class Functor f where
      fmap :: (a -> b) -> f a -> f b

  This unifies TLKS and regular, term-level signatures.

  We may also drop the ``type`` prefix for associated types, but without proper
  dependent types, the implementation will need to distinguish associated types
  from class methods. We propose to use name capitalization as a hint::

    class C a where
      F :: a -> Bool   -- associated type
      f :: a -> Bool   -- class method

  This should only affect typechecking (class methods are not promoted), but
  not parsing or name resolution. Dependent types will allow us to do away
  with this distinction completely.

  Guarded by ``-XDefaultNamespace``.

* Constrain the domain of associated types by the class::

    class Container c where
      Item :: Type
      toList :: c -> [Item c]

    Item :: forall c -> Container c => Type

  Note that we use the same "quantify visibly if ambiguous" rule.

Examples
--------

* **Example 1**, ``Storable``. Definition site::

    class Storable a where
      sizeOf a :: Int
      alignmentOf a :: Int

    -- sizeOf, aligmentOf :: forall a -> Storable a => Int

  Use site::

    ghci> sizeOf Int
    8
    ghci> sizeOf Bool
    4

* **Example 2**, tagged accessor class ``HasLens``. Definition site::

    class HasLens tag s a | tag s -> a where
      lensOf tag @s @a :: Lens' s a

    -- lensOf :: forall tag -> forall s a. HasLens tag s a => Lens' s a

  Use site::

    ghci> struct ^. lensOf UserInfo . lensOf UserName
    "Jack Sparrow"

* **Example 3**, ``MonadReader`` in the style of ``monads-tf``. Definition site::

    class MonadReader m where
      Env m :: Type
      ask :: m (Env m)

    -- Env :: forall m -> MonadReader m => Type
    -- ask :: forall m. MonadReader m => m (Env m)

  Use site::

    doStuff :: Env AppM -> AppM r -> IO r


Motivation
----------

TBD.


Proposed Change Specification
-----------------------------

TBD.


Effect and Interactions
-----------------------

* The syntax for VDQ function application will be such that type and term
  arguments can be parsed, name resolved, and disambiguated before the type
  checking of the applied function.

* Under ``-XVisibleForall``, the type and term language are to use the same syntax, so
  only namespace resolution is different. Under ``-XDefaultNamespace``,
  namespace resolution is the same as well.

TBD.

Costs and Drawbacks
-------------------

TBD.


Unresolved Questions
--------------------

TBD.

Implementation Plan
-------------------

I (Vladislav Zavialov) will write proposals for specific features of this plan,
and implement them, as long as I manage to reach consensus with the community
and the committee about the proposed changes.
