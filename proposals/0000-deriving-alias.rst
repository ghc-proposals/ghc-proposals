Deriving aliases
================

.. proposal-number::
.. trac-ticket::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/215>`_.
.. sectnum::
.. contents::


The Deriving mechanism has become increasingly powerful in Haskell. The initial
goal may have been to ease the burden of developers from writing boilerplate
code, but so much code is being derived that this method of removing boilerplate
became boilerplate itself (though in a much more comprehensible way). This
document is a proposal to improve the deriving mechanism by allowing users to
define deriving aliases.


Motivation
------------

Haskell applications usually involve multiple layers of communication between
the components (of the app): database serialization, JSON encoding and decoding,
Mustache templates, CSV serialization and very often much more. It is very
common to use newtypes to squeeze more guarantees from the compiler. The
nice thing about ``newtype`` is that it is possible to derive a lot of
typeclasses almost for free. Here are the examples of some data types from a
real world application: ::

  -- | Wrapper over the text of an email.
  newtype Email = Email { unEmail :: Text }
      deriving (Show, Generic)
      deriving newtype (Eq, Ord, Hashable, FromField, ToField, ToMustache)
      deriving anyclass (FromJSON, ToJSON)

  -- | Subject of the email.
  newtype Subject = Subject { unSubject :: Text }
      deriving (Show, Generic)
      deriving newtype (Eq, Ord, Hashable, FromField, ToField, ToMustache)
      deriving anyclass (ToJSON, FromJSON)

  -- | Wrapper for primary keys in a database table.
  newtype Id a = Id { unId :: Int }
      deriving (Show, Generic)
      deriving newtype ( Eq, Ord, Hashable, FromField, ToField, ToMustache
                       , FromHttpApiData, ToHttpApiData)
      deriving anyclass (FromJSON, ToJSON)

The more data types you have, the more repetitive deriving statements become.
Each time you introduce a new component in the application, you need to manually
change the definition of each data type and add more derivings. This makes
declaring and maintaining new data types a remarkably cumbersome process.

This proposal suggests reducing the problems highlighted above by allowing
aliases for multiple deriving clauses and a new deriving strategy that can make
use of this construct. Using these new features the above code can be simplified
to: ::

  deriving alias CoreClasses =
      (Show, Generic)
      newtype  (Eq, Ord, Hashable, FromField, ToField, ToMustache)
      anyclass (FromJSON, ToJSON)

  newtype Email = Email { unEmail :: Text }
      deriving alias CoreClasses

  newtype Subject = Subject { unSubject :: Text }
      deriving alias CoreClasses

  newtype Id a = Id { unId :: Int }
      deriving newtype (FromHttpApiData, ToHttpApiData)
      deriving alias CoreClasses

Proposed Change Specification
-----------------------------

The name of the language extension: ``DerivingAlias``. This extension, when
enabled, allows us to:

1. Declare new synonyms for reusable deriving definitions.
2. Use the ``alias`` deriving strategy.
3. Export and import deriving aliases.

The syntax of declaring a new ``deriving alias`` is the following:

1. ``deriving`` keyword.
2. Followed by the ``alias`` keyword.
3. Followed by the name of the alias.
4. Followed by zero or more type variables.
5. Followed by the ``=`` operator.
6. Followed by the list of deriving definitions similar to what is used with
   data types but without the ``deriving`` prefix.

Several examples of deriving aliases: ::

  deriving alias EnumBundle = (Show, Read, Eq, Ord, Enum, Bounded, Ix)

  deriving alias FAM = newtype (Functor, Applicative, Monad)

  deriving alias MReader env =
      alias FAM
      newtype (MonadIO, MonadReader env)

  deriving alias ToElm t =
      stock (Generic)
      (Elm, ToJSON, FromJSON) via ElmStreet t

A new strategy named ``alias`` is introduced to allow using these definitions in
both scenarios — inside other deriving aliases (as shown above) and for data
type deriving. ::

  data Status = Approved | Rejected | Pending
      deriving alias EnumBundle

  newtype App a = App { unApp :: ReaderT Env IO a }
      deriving alias MReader Env

Since ``deriving alias`` would be a new entity in the Haskell language, it
should be possible to export and import it. The ``deriving`` keyword would need
to be added as a prefix for exported and imported ``deriving alias`` symbols
(similar to ``pattern`` from `PatternSynonyms <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#pattern-synonyms>`_
and ``type`` from
`ExplicitNamespaces <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#explicit-namespaces-in-import-export>`_): ::

    module Foo (deriving MReader) where

    import Bar (deriving FAM)


Effect and Interactions
-----------------------

The proposal aims to remove even more boilerplate from Haskell projects by
providing the ability to capture common deriving patterns into their own
entities that can be reused later. Additionally, it becomes easier to introduce
changes in large applications: once you realize that you need to derive another
typeclass for all your core data types, you can simply patch relevant deriving
synonym. Moreover, since Haskell already has a huge number of features in the
area of type-level computation, ``deriving`` clauses can get quite complex, like
the one below
(`original example and discussion <https://www.reddit.com/r/haskell/comments/8y98yt/typedriven_safe_derivation_of_tojson_and_fromjson/>`_): ::

  data OtherConfig = OtherConfig { otrNameOfProcess :: Maybe String
                                 , otrArgsToProcess :: [String]
                                 }
    deriving (Read, Show, Eq, Ord, Generic)
    deriving (ToJSON, FromJSON)
         via WithOptions '[ FieldLabelModifier     '[CamelTo2 "-", Drop 3]
                          , ConstructorTagModifier '[CamelTo2 "-", UserDefined Init]
                          , SumEnc                  TwoElemArr
                          , TagSingleConstructors  'True
                          , OmitNothingFields      'True
                          ]
                          OtherConfig

After this proposal is implemented, it will become possible to define common
deriving definitions, put them into packages and export them to increase the
code reusability and make the overall user experience more pleasant.

Costs and Drawbacks
-------------------

The proposed feature increases the power of the
`DerivingStrategies <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-strategies>`_
extension. Like any other feature, it makes the entrance threshold higher for
the language and requires Haskell developers to learn more in order to
understand code that uses this feature. However, since it is very close to mere
syntax-sugar, it shouldn't be too difficult to learn.


Alternatives
------------

CPP macros
^^^^^^^^^^

It's already possible to achieve an almost similar level of the reusability
using the
`CPP macros <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html#standard-cpp-macros>`_.
You can consolidate common parts of ``deriving`` under a new custom CPP macro
and use it wherever you need. However, abusing CPP macro-system for this feature
would likely be a poor solution. Furthermore, it is a well-known fact that
tooling usually handles CPP macros in a bad way. Since GHC is fed with the code
after preprocessing, it's not possible to perform a huge bunch of analysis forms
for macros and their usage.

Template Haskell
^^^^^^^^^^^^^^^^

It is possible to define a
`TH macro <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell>`_
that uses the
`StandaloneDeriving <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#stand-alone-deriving-declarations>`_
extension to derive a lot of typeclasses for you. Like this: ::

    boilerplateAnnigilator :: Name -> Q [Dec]
    boilerplateAnnigilator typeName = do
        let typeCon = conT typeName
        [d|deriving instance Show $(typeCon)
           deriving instance Eq   $(typeCon)
           deriving instance Ord  $(typeCon)
          |]

This approach `was proposed here <https://stackoverflow.com/questions/45113205/is-there-a-way-to-shorten-this-deriving-clause>`_.

It works okay for basic and simple cases. However, it's extremely difficult to
implement generic logic that will work for every typeclass since standalone
deriving requires you to specify the context for type variables. With this
approach, one needs to reimplement internal GHC logic but in Template Haskell.

Reuse type aliases
^^^^^^^^^^^^^^^^^^

`Quite recently <https://www.reddit.com/r/haskell/comments/9dx6s9/proposal_data_deriving_synonyms/>`_
it was discussed to reuse existing ``type`` synonyms of kind ``Constraint``,
like in the example below: ::

  type RS a = (Read a, Show a)

  data Foo = Bar | Baz
      deriving RS

However, this approach is quite limited:

1. It doesn't allow one to specify a deriving strategy.
2. It is not possible to integrate
   `DerivingVia <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-via>`_
   with this approach.
3. It becomes cumbersome to use when multiple type variables are involved.

Unresolved Questions
--------------------

1. Should it be a simple syntax-sugar extension where the definition of
   ``deriving alias`` is checked only syntactically and a more thorough analysis
   is performed only when the alias is actually used?


Implementation Plan
-------------------

I (Dmitrii Kovanikov) will try to implement the proposal.
