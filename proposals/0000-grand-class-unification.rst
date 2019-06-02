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
* Associated types are introduced with a declaration header, class methods are
  introduced with a type signature. To unify, we say that associated types are
  now to be introduced with a kind signature inside the class.
* Without declarations headers, associated types need another way to specify
  arity and injectivity. We add syntax for this.
* Associated types may quantify visibly over class variables, while class
  methods cannot. To bridge the gap, we introduce a new "quantify visibly if
  ambiguous" rule, which deprecates ``-XAllowAmbiguousTypes`` along the way.
* The "quantify visibly if ambiguous" rule is non-syntactic, so we add
  top-level signatures for class methods and associated types to serve as
  machine-checked documentation (based on a `comment under #148
  <https://github.com/ghc-proposals/ghc-proposals/pull/148#issuecomment-491794309>`_
  but not the current iteration of the proposal text).
* The "quantify visibly if ambiguous" rule implies visible dependent
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

* Top-level signatures for class methods that serve as machine-checked
  documentation::

    class Eq a where
      eq :: a -> a -> a

    eq :: forall t. Eq t => t -> t -> t

  The rule for checking the top-level signature is as follows. Let us refer to
  the in-class method as ``$eq``, then we assume the following definition::

    eq = $eq

  This allows us to reorder and rename invisible type variables, and to
  use a subclass instead of the actual parent class.

  Guarded by ``-XTopLevelSignatures``.

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

  Guarded by ``-XVDQ``.

* Default namespace and namespace qualified imports::

    import Data.Proxy type qualified as T
    import Data.Proxy data qualified as D

    f :: forall (a :: k) -> ...

    a = f T.Proxy   -- refers to the type constructor
    b = f D.Proxy   -- refers to the data constructor

    data T = T   -- rejected

  The namespace lookup rules of ``-XVDQ`` (outlined in the previous bullet) do
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

* No ``type`` prefix for top-level kind signatures::

    Functor :: (Type -> Type) -> Constraint
    class Functor f where
      fmap :: (a -> b) -> f a -> f b

  This unifies TLKS and regular, term-level signatures.

  We may also drop the ``type`` prefix for associated types, but without proper
  dependent types, the implementation will need to distinguish associated types
  from class methods. We propose to use name capitalization as a hint::

    class C a where
      F :: Bool   -- associated type
      f :: Bool   -- class method

  This should only affect typechecking (class methods are not promoted), but
  not parsing or name resolution. Dependent types will allow us to do away
  with this distinction completely.

  Guarded by ``-XDefaultNamespace``.

* Syntax for ``-XTypeFamilyDependencies`` in top-level kind signatures::

    H :: forall (a :: Type) -> Type | H a -> a
    type family H a where { ... }

    K :: forall a b -> Type | K a b -> a
    class C a b where
      K :: Type

* Quantify class variables visibly when ambiguous::

    class C a where
      m :: ty

    m :: forall a. C a => ty    -- if  ty  determines  a  (ambiguity check)
    m :: forall a -> C a => ty  -- if  ty  doesn't determine  a

  This removes the need for ``-XAllowAmbiguousTypes``, which are to be deprecated.
  Note that the programmer may give a top-level signature to the methods, so the
  readers of the code need not figure out which class variables are ambiguous.

  Guarded by ``-XVisibleClassVariables``.

* Constrain the domain of associated types by the class::

    class Container c where
      Item :: Type
      toList :: c -> [Item c]

    Item :: forall c -> Container c => Type

  Note that we use the same "quantify visibly if ambiguous" rule.

* Syntax for matchable quantifiers::

    C :: Type -> Constraint
    class C a where
      F :: Bool  -> Bool
      G :: Bool :-> Bool

    F :: forall a -> C a => Bool  -> Bool    -- arity = 3
    G :: forall a -> C a => Bool :-> Bool    -- arity = 2

  Guarded by ``-XMatchableQuantifiers``.


Examples
--------

* **Example 1**, ``Storable``. Definition site::

    class Storable a where
      sizeOf :: Int
      alignmentOf :: Int

    sizeOf, aligmentOf :: forall a -> Storable a => Int

  Use site::

    ghci> sizeOf Int
    8
    ghci> sizeOf Bool
    4

* **Example 2**, tagged accessor class ``HasLens``. Definition site::

    class HasLens tag s a | tag s -> a where
      lensOf :: Lens' s a

    lensOf :: forall tag -> forall s a. HasLens tag s a => Lens' s a

  Use site::

    ghci> struct ^. lensOf UserInfo . lensOf UserName
    "Jack Sparrow"

* **Example 3**, ``MonadReader`` in the style of ``monads-tf``. Definition site::

    class MonadReader m where
      Env :: Type
      ask :: m (Env m)

    Env :: forall m -> MonadReader m => Type
    ask :: forall m. MonadReader m => m (Env m)

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

* Under ``-XVDQ``, the type and term language are to use the same syntax, so
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
