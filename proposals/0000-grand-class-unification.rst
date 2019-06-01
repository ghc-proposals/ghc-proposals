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

The proposed features are:

* top-level signatures for class methods
* remove the ``type`` prefix of top-level kind signatures
* visible dependent quantification in terms
* visible quantification of ambiguous class variables
* constrained type families

In more detail:

* Add top-level signatures for class methods that serve as machine-checked
  documentation::

    class Eq a where
      eq :: a -> a -> a

    eq :: forall t. Eq t => t -> t -> t

  The rule for checking the top-level signature is as follows. Let us refer to
  the in-class method as ``$eq``, then we assume the following definition::

    eq = $eq

  This allows us to reorder and rename invisible type variables, and to
  use a subclass instead of the actual parent class.

* Remove the ``type`` prefix of top-level kind signatures::

    Functor :: (Type -> Type) -> Constraint
    class Functor f where
      fmap :: (a -> b) -> f a -> f b

  This unifies TLKS and regular, term-level signatures. In case of namespace
  ambiguity, reject::

    data T = T
    T :: ...   -- not allowed.

* Allow visible dependent quantification in terms::

    idv :: forall a -> a -> a
    idv t x = x :: t

    x = idv Double  42     -- x :: Double
    n = idv Integer 42     -- n :: Integer

  The difference with ``-XTypeApplications`` is in the presence of a visibility
  override ``@`` at use sites.  This implies a unification of the type and
  expression parsers, and a change to name lookup rules.

* Quantify class variables visibly when ambiguous::

    class C a where
      m :: ty

    m :: forall a. C a => ty    -- if  ty  determines  a  (ambiguity check)
    m :: forall a -> C a => ty  -- if  ty  doesn't determine  a

  This removes the need for ``-XAllowAmbiguousTypes``, which are to be deprecated.
  Note that the programmer may give a top-level signature to the methods, so the
  readers of the code need not figure out which class variables are ambiguous.

* Constrain the domain of associated types by the class::

    class Container c where
      type Item c
      toList :: c -> [Item c]

    Item :: forall c -> Container c => Type

  Note that we use the same "quantify visibly if ambiguous" rule.

Examples
--------

*Example 1*, ``Storable``. Definition site::

  class Storable a where
    sizeOf :: Int
    alignmentOf :: Int

  sizeOf, aligmentOf :: forall a -> Storable a => Int

Use site::

  ghci> sizeOf Int
  8
  ghci> sizeOf Bool
  4

*Example 2*, tagged accessor class ``HasLens``. Definition site::

  class HasLens tag s a | tag s -> a where
    lensOf :: Lens' s a

  lensOf :: forall tag -> forall s a. HasLens tag s a => Lens' s a

Use site::

  ghci> struct ^. lensOf UserInfo . lensOf UserName
  "Jack Sparrow"

*Example 3*, ``MonadReader`` in the style of ``monads-tf``. Definition site::

  class MonadReader m where
    type Env :: Type
    ask :: m (Env m)

  type Env :: forall m -> MonadReader m => Type
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

TBD.


Costs and Drawbacks
-------------------

TBD.


Unresolved Questions
--------------------

TBD.
