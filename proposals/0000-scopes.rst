Scopes
======

.. author:: Rashad Gover
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

This proposal introduces the `Scopes` language extension to GHC.

Motivation
----------

Practical
~~~~~~~~~

A naming ambiguity is when a name has two different meanings, and the compiler can't decide which one is correct.

Haskellers have used modules, naming conventions, types in the case of record field
labels, and type classes to avoid naming ambiguities.

The proposed lanuage extension, ``Scopes``, aims to provide named lexical scoping as another
technique for avoiding naming ambiguities.

.. Give a strong reason for why the community needs this change. Describe the use
.. case as clearly as possible and give an example. Explain how the status quo is
.. insufficient or not ideal.

.. A good Motivation section is often driven by examples and real-world scenarios.

Geometrical
~~~~~~~~~~~

There is limited space in Haskell for choosing names which leads to naming collisions.

Imagine we have a line representing all valid identifiers in Haskell.
Let's call this dimension *I*.
A dot on this line represents an identifer used in a given Haskell program.
A dot that occupies the same spot on the line as another dot is a naming collision.

Now imagine we add another line perpindicular to the first to represent namespaces.
Where the name is located a long this line represents what namespace it is in.
Is it a variable, type, constructor, etc.? For example, ``name`` is a variable, but ``Name`` is a constructor or type.
A valid identifier can exist in any one of the namespaces, so now we have two dimensions: one for the string of symbols that
is the identifier, and one for the namespace it belongs to.
Let's call this new dimension *N*. A valid identifier in a given Haskell program has coordinates ``<I, N>``. 
Just as was the case with the line, a dot occupying the same space on this 2D plane as another dot is a naming collsion.

Now imagine a third line rising outward from the plane, perpindicular to the first two lines, representing
modules. Let's call this new dimension *M*.
An identifier can now be represented by what string it is, what namespace it lives in, and what module it is defined in.
Our program identifiers now exist in 3 dimensions, and have coordinates *<I, N, M>*.

The ``Scopes`` language extension aims to add a 4th dimension, scopes, giving Haskellers
much more "space" to choose names that don't collide with others. Let's call this 4th dimension *S*.
Identifiers can now be recognized by their coordinates *<I, N, M, S>*.

Since a module can contain practically infinite scopes and scopes can be nested,
infinitely many more dimensions to place names are provided by scopes.
Even within the same module.

Identifiers can be recognized by their coordinates *<I, N, M, S₀, ..., Sₙ>* where n represents the number of scopes.

Proposed Change Specification
-----------------------------

The ``Scopes`` language extension adds the following reserved symbols to Haskell:

- ``scope``
- ``open``
- ``/``

A new namespace for scope identifiers in import and export list syntax is also introduced.

``scope``
~~~~~~~~~

The ``scope`` reserved word is used for declaring scopes.

Syntax
******

Scope declarations are added as an extension to the *topdecls* grammar
as defined in `Chapter 4 of the Haskell 2010 Report
<https://www.haskell.org/onlinereport/haskell2010/haskellch4.html>`_::

  topdecls -> ...
            | scope scoid [scoexports] where {scodecls}

A *scoid* (scope identifier) is a *conid*, or capitalized identifier, as defined in `Chapter 2.4 of the Haskell 2010 Report
<https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-180002.4>`_.

The grammar for *scodecls* (scope declarations) is similar to *topdecls*::

  scodecls -> scodecl₁; ...; scodeclₙ
  scodecl  -> type simpletype = type
            | data [context =>] simpletype [= constrs] [deriving]
            | newtype [context =>] simpletype = newconstr [deriving]
            | class [scontext =>] tycls tyvar [where cdecls]
            | scope scoid [scoexports] where {scodecls}
            | decl

*scodecls* includes:

- type synonym declarations
- data declarations
- newtype declarations
- type class declarations
- term declarations
- scope declarations

and excludes:

- instance declarations

I'm still thinking whether or not the following *topdecls* are allowed in *scodecls*:

- default declarations
- foreign declarations

Since *scodecls* includes *scodecls*, scope declarations can be nested within scope
declarations.

I'm still trying to figure out how to formalize the grammar for *scoexports* (scope exports).
For now, know that *scoexports* can only refer to *scodecls*.
Based on the current definition of *scodecls*, this means that *scoexports*
can reference the following constructs:

- type synonyms
- data types

  - constructors
  - record field labels
- newtypes

  - constructor
  - record field label
- type classes

  - type class methods
- scopes

Semantics
*********

Declaring a scope creates a named lexical scope::

  {-# LANGUAGE Scopes #-}
  
  module M where

  scope S where
    x :: Int
    x = 5

The ``x`` identifier is not visible outside of scope ``S``::

  {-# LANGUAGE Scopes #-}

  module M where

  scope S where
    x :: Int
    x = 5

  a = x + 1 -- Error: What is x?

Scopes inherit identifiers from their parent lexical scope::

  {-# LANGUAGE Scopes #-}

  module M where

  f :: Int -> Int
  f x = x * 3

  scope S where
    x :: Int
    x = f 5 -- 15. f was inherited from the global scope

Name shadowing applies to scopes::

  {-# LANGUAGE Scopes #-}

  module M where

  a = x + 1 -- Success: 11

  scope S where
    x :: Int
    x = 5 -- This is a shadowing warning, but OK.

  x :: Int
  x = 10

Scopes can be nested::

  {-# LANGUAGE Scopes #-}

  module M where

  scope S where
    x :: Int
    x = 5
    scope T where
      y :: Int
      y = 10
      scope U where
        z :: Int
        z = 15

        x = 10

        f :: Int -> Int
        f a = x + y + z + a

        result :: Int
        result = f 5 -- 40

Scopes expose all identifiers declared within them by default.
Scope declarations decide what identifiers a scope exposes by using a *export list*, similar to
export lists used for modules::

  {-# LANGUAGE Scopes #-}

  module M where

  scope S (x, T) where
    x :: Int
    x = 5
    scope T (y) where
      y :: Int
      y = 10
      scope U (result) where
        z :: Int
        z = 15

        x = 10

        f :: Int -> Int
        f a = x + y + z + a

        result :: Int
        result = f 5 -- 40

The same export list syntax used for modules is allowed for scopes.

``open``
~~~~~~~~

Open is used for opening scopes. Opening a scope brings all identifiers exposed by the scope
into the lexical scope of the open declaration::

  {-# LANGUAGE Scopes #-}

  module M where

  scope S where
    x :: Int
    x = 5

  open S

  y :: Int
  y = x + 2 -- Evaluates to 7

Identifiers inside a scope are completely hidden from the program, unless an open is used to bring
identifiers that the scope exposes into scope. There's never a situation where a scope is implicitly open.
To use identifiers inside of a scope, an accompanying open must be present.

There is no equivalent of import lists for open declarations. A scope can either be opened,
bringing all identifiers the scope exposes in to scope, or not::

  {-# LANGUAGE Scopes #-}

  module M where

  scope S where
    x :: Int
    x = 5

  y = x + 2 -- Error! What is x?

open declarations can be at the top-level of the module::

  {-# LANGUAGE Scopes #-}
  
  module N where

  import M (scope S)

  open S

  y = x + 2

or within a scope, let statement, or where clause::

  {-# LANGUAGE Scopes #-}

  module N (g) where

  import M (scope S)

  open T

  scope T (g) where
    g :: Int -> Int
    g a = let open S in a * x

Open declarations can take multiple items, e.g.::

  {-# LANGUAGE Scopes #-}

  module M where

  import N (scope S)

  h a =
    let
      open S
    in let
      open T qualified
    in let
      open T/U qualified as U
    in
      a * x + T/y + U/z

can be::

  {-# LANGUAGE Scopes #-}

  module M where

  import N (scope S)

  h a =
    let
      open S
      open T qualified
      open T/U qualified as U
    in
      a * x + T/y + U/z

or just::

  {-# LANGUAGE Scopes #-}

  module M where

  import N (scope S)

  h :: Int -> Int
  h a =
    let
      open
        S
        T qualified
        T/U qualified as U
    in
      a * x + T/y + U/z

Scope Qualification
*******************

Names within a scope, that the scope exposes, can be brought into scope using open.
Scopes that are nested within another scope can be accessed with the ``/`` operator::

  {-# LANGUAGE Scopes #-}

  module M where

  scope S where
    a :: Int
    a = -5
    scope T where
      b :: Int
      b = 5 + a

  f x =
    let open S qualified
    in
      let open S/T
      in x + b

or::

  {-# LANGUAGE Scopes #-}

  module N where

  import M (scope S)

  f x = let open S in let open T in x + b

Scoped Declarations
*******************

The scope keyword can also be used on various top-level declarations directly::

  {-# LANGUAGE Scopes #-}
  
  module M where

  scope data User = User { name :: String, age :: Int, salary :: Float }

  scope data Dog = Dog { name :: String, age :: Int }

  scope class Greeter a where
    name  :: a -> String
    greet :: String

By applying *scope* to a top-level declaration we create a scope for all identifiers generated by the
declaration.
In the case of a data declaration, that will be the type, constructors, and any potential record field labels.
In the case of a class declaration, the class name and methods will be scoped.

Data declarations can have even finer scoping on the constructors::

  {-# LANGUAGE Scopes #-}
  
  module M where

  scope data Entity
    = scope User { name :: String, age :: Int, salary :: Float }
    | scope Dog { name :: String, age :: Int }

This allows two different constructors of the same type to share the same record field labels.

Same for GADT constructors::

  {-# LANGUAGE Scopes #-}
  
  module M where

  scope data Entity where
    scope User :: { name :: String, age :: Int, salary :: Float } -> Entity
    scope Dog :: { name :: String, age :: Int } -> Entity

You cannot place instance delcarations inside of scopes.
Any instances generated by a scoped declaration, e.g.::

  {-# LANGUAGE Scopes #-}
  
  module M where

  scope data User = User { name :: String, age :: Int, salary :: Float }
    deriving (Eq, Show)

are floated outside of the scope until the module top-level is reached, and generated on the
qualifed name. So the above example desugars to::

  {-# LANGUAGE Scopes #-}
  
  module M where

  instance Eq User/User where
    ...

  instance Show User/User where
    ...

  scope User where
    data User = User { name :: String, age :: Int, salary :: Float }

Examples
--------


Effect and Interactions
-----------------------
Your proposed change addresses the issues raised in the motivation. Explain how.

Also, discuss possibly contentious interactions with existing language or compiler
features. Complete this section with potential interactions raised
during the PR discussion.


Costs and Drawbacks
-------------------
Give an estimate on development and maintenance costs. List how this affects
learnability of the language for novice users. Define and list any remaining
drawbacks that cannot be resolved.


Backward Compatibility
----------------------
How well does your proposal meet the stability principles described in our
`GHC stability principles <../principles.rst#3GHC-stability-principles>`_ document?

Will your proposed change cause any existing programs to change behaviour or
stop working? Assess the expected impact on existing code on the following scale:

0. No breakage
1. Breakage only in extremely rare cases (e.g. for specifically-constructed
   examples, but probably no packages published in the Hackage package repository)
2. Breakage in rare cases (e.g. a few Hackage packages may break, but probably
   no packages included in recent Stackage package sets)
3. Breakage in uncommon cases (e.g. a few Stackage packages may break)
4. Breakage in common cases

(For the purposes of this assessment, GHC emitting new warnings is not
considered to be a breaking change, i.e. packages are assumed not to use
``-Werror``.  Changing a warning into an error is considered a breaking change.)

Explain why the benefits of the change outweigh the costs of breakage.
Describe the migration path. Consider specifying a compatibility warning for one
or more compiler releases before the change is fully implemented. Give examples
of error messages that will be reported for previously-working code; do they
make it easy for users to understand what needs to change and why?

When the proposal is implemented, the implementers and/or GHC maintainers should
test that the actual backwards compatibility impact of the implementation is no
greater than the expected impact. If not, the proposal should be revised and the
steering committee approve the change.


Alternatives
------------
List alternative designs to your proposed change. Both existing
workarounds, or alternative choices for the changes. Explain
the reasons for choosing the proposed change over these alternative:
*e.g.* they can be cheaper but insufficient, or better but too
expensive. Or something else.

The PR discussion often raises other potential designs, and they should be
added to this section. Similarly, if the proposed change
specification changes significantly, the old one should be listed in
this section.

Unresolved Questions
--------------------
Explicitly list any remaining issues that remain in the conceptual design and
specification. Be upfront and trust that the community will help. Please do
not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to
the steering committee.


Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other resources
and prerequisites are required for implementation?

Endorsements
-------------
(Optional) This section provides an opportunity for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.

Acknowledgments
---------------

(Optional) This section provides an opportunity to say thanks
to third parties for their contributions to the proposal.
