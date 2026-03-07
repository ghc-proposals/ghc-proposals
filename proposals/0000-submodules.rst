Submodules
==========

.. author:: Rashad Gover
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/745>`_.
.. contents::

Motivation
----------

When a Haskell programmer creates an abstraction
and decides to give it an identifier, they want to choose the best
name or symbol according to their own standards or preferences.

Unfortunately, since modules are the only way to create new *namespaces* in Haskell,
the Haskell programmer must consider other factors outside of their
own for choosing an identifier:

1. Is the identifier already used in this module?
2. Is the identifier already used by an imported module?
3. Is the identifier already used by other modules that
   may depend on the module or be used with it?

The 2 primary issues with modules as namespaces, are:

1. Importing a module brings its identifiers into the entire scope
   of the importing module. There is no way to contain the effect
   that imports have on the importing module's namespace.
2. Every module needs to be defined in its own file, so creating modules
   only for the sake of creating a fresh namespace isn't worth
   it. Creating a new namespace has a higher cost than we would like.

This proposal adds a new language construct besides regular modules called *submodules*
to give programmers a more lightweight, flexible alternative
that addresses these issues. It also allows module import declarations in local
scopes like let statments and where clauses.

Proposed Change Specification
-----------------------------

All changes described in this specification only apply to modules that have the
``-XSubmodules`` language extension turned on.

Syntax
******

Submodules
~~~~~~~~~~

*Submodule declarations* are added as an extension to the ``topdecls`` production rule::

  topdecls -> ...
            | 'module' ['sealed'] [modid] [exports] 'where' {topdecls}
            | 'module' subtopdecl

  subtopdecl -> 'type' simpletype = type
              | 'data' [context =>] simpletype [= constrs] [deriving]
              | 'newtype' [context =>] simpletype = newconstr [deriving]
              | 'class' [scontext =>] tycls tyvar ['where' cdecls]

They start off with the ``module`` keyword, followed by optional *modifiers*:
the ``sealed`` keyword, a *module identifier*, and/or an export list. After the ``module``
keyword and optional modifiers, the ``where`` keyword is used and followed by a
block of declarations. Here's an example of a submodule declaration using all modifiers::

  module sealed C (type T, new, add) where
    data T = T { bar :: Int }

    new :: Int -> T
    new = T

    add :: T -> T -> T
    add (T x) (T y) = T (x + y)

Note that the production rule for submodule declarations is recursive,
so submodules can be nested within other submodules::

  module A where
    module B where
      module C where
        foo :: Int -> Int
        foo x = x * 3

Besides the normal submodule declaration syntax, there's
syntactic sugar for declaring submodules for type, newtype, data, and class declarations.
This is done by prefixing one of these declarations
with the ``module`` keyword. This::

  module data T = T { foo :: Int, bar :: Float }

desugars to::

  module T where
    data T = T { foo :: Int, bar :: Float }

The identifier of the resulting submodule is the same as the identifier of the
wrapped declaration. Since module identifiers exist in a separate namespace from
type identifiers, class identifiers, etc., reusing the identifier for
the submodule doesn't cause any ambiguity.

Submodule Qualifier
~~~~~~~~~~~~~~~~~~~

Submodules can be also be used for name qualification if the submodule is imported::

  module A where
    module B where
      module C where
        foo :: Int -> Int
        foo x = x * 3

  import A

  bar = A.B.C.foo 12

The ``.`` symbol is used as the qualification operator for submodules, just like normal modules,
when ``-XSubmodules`` is on.

Import Declarations
~~~~~~~~~~~~~~~~~~~

This proposal also extends the rules around import declarations::

  decl -> gendecl
        | (funlhs | pat) rhs
        | importdecl

This extension to the declaration production rule allows for import declarations
inside of let statements and where clauses.

We also allow import declarations to appear anywhere in the
global scope of a module, not just at the the top of the module.
This aligns with the semantics of Haskell declarations, which
are order independent.

Submodule Semantics
*******************

Submodules have 3 orthogonal, binary traits:

1. Environment - *Unsealed* or *Sealed*. What environment does the submodule have?
2. Identity - *Anonymous* or *Named*. How are the submodule's declarations exposed?
3. Transparency - *Full* or *Partial*. What declarations does the submodule expose?

There are 2^3 = 8 possible forms of submodule.

Environment
~~~~~~~~~~~

*Unsealed submodules* have standard lexical scoping behavior.

They *inherit* the environment of the scope they're declared in::

  {-# LANGUAGE Submodules #-}

  module M where
  
  import N (module T)

  y :: Int
  y = 32

  module S where
    import T -- exposes z

    f :: Int -> Int
    f x = x + y + z

Identifiers inside an unsealed submodules *shadow* identifiers from the outer scope::

  {-# LANGUAGE Submodules #-}

  module M where

  import N (module T)

  y :: Int
  y = 32

  module S where
    import T

    y :: Int
    y = 10

    f :: Int -> Int
    f x = x + y + z

*Sealed submodules* do not inherit their outer scope like unsealed submodules do::

  {-# LANGUAGE Submodules #-}

  module M where
  
  import N (module T)

  y :: Int
  y = 32

  module sealed S where
    import Prelude
    import T -- exposes z

    f :: Int -> Int
    f x = x + y + z -- Error! y is unknown

Since sealed submodules do not inherit their outer scope,
declarations inside of sealed submodules cannot shadow outer ones::

  {-# LANGUAGE Submodules #-}

  module M where
  
  import N (module T)

  y :: Int
  y = 32

  module sealed S where
    import Prelude
    import T

    y :: Int -- This doesn't shadow the outer y.
    y = 10

    f :: Int -> Int
    f x = x + y + z

The only way to bring an identifier into a sealed submodule is through an explicit import declaration::

  {-# LANGUAGE Submodules #-}

  module M where

  module C where
    ...

  module sealed S where
    import O (module A, module B)
    import A
    import B
    import C
    ...

or by declaring a fresh identifier inside the sealed submodule.

In summary, sealing a submodule is useful if you need a blank slate
where internal declarations only depend on:

- Identifiers brought in scope via explicit imports
- Identifiers declared directly in the submodule

Identity
~~~~~~~~

*Anonymous submodules* cannot be referenced since they don't have an identifier.

This means you cannot export or import an anonymous submodule::

  {-# LANGUAGE Submodules #-}

  module M where -- I can't export the anon submodule...What name?
  
  import N (module T)

  module (f) where
    import T
    f :: ...
    g :: ...

And you cannot explicitly import anonymous submodules::

  {-# LANGUAGE Submodules #-}

  module M where
  
  import N (module T)

  module (f) where -- I can't use import on this submodule...What name?
    import T
    f :: ...
    g :: ...

Instead, anonymous submodules are opened at the point of their declaration::

  {-# LANGUAGE Submodules #-}

  module M where
  
  import N (module T)

  module (f) where
    import T
    f :: ...
    g :: ...

  h x = f x -- f is exposed by the anonymous submodule

Named submodules on the other hand can be exported and imported::

  {-# LANGUAGE Submodules #-}

  module M (module S) where
  
  import N (module T)

  module S where
    import T
    ...

Named submodules also you to qualify the names within them by opening
the submodule and using ``.``::

  {-# LANGUAGE Submodules #-}

  module M (module S, bar) where

  import N (module T)

  module S where
    import T
    foo :: Int -> Int
    foo = \x -> x + 1
    ...

  import S qualified

  bar :: Int
  bar = S.foo 11

In short, naming a submodule means it can be opened
somewhere other than where it was declared, and can be
used as a qualifier for any identifier it exports.

Transparency
~~~~~~~~~~~~

*Fully transparent* submodules expose all identifiers inside of them when imported::

  {-# LANGUAGE Submodules #-}

  module M where

  module S where
    x = 5
    y = 10
    z = 20

  import S

  a = x + y + z

*Partially transparent* submodules have an export list, and only
expose identifiers listed in the export list::

  {-# LANGUAGE Submodules #-}

  module M where

  module S (x, z) where
    x = 5
    y = 10
    z = 20

  import S

  a = x + y + z -- Error! No y in scope.

Export lists can use ``-XExplicitNamespaces``::

  {-# LANGUAGE Submodules, ExplicitNamespaces #-}

  module M where

  module S (type T, type R, class C(..)) where
    type T = T

    type R = R { l :: Int }

    class C where
      m :: Int

Only identifiers that are declared in the submodule can be added to the submodule's export list.
Identifiers brought in via ``import`` cannot be added to the submodule's export list.
Submodules do not allow re-exporting.

Instances
~~~~~~~~~

Instance declarations are allowed inside submodules, but the instances
are associated with the parent module they are declared in.
Instance declarations cannot be added to or excluded from the export list
of a submodule. They are always exported and always imported, if the parent module of the submodule is imported.

The behavior of instances is unaffected by the existence of submodules.
The only difference is that instance declarations can now refer to locally scoped
type and class identifiers. As long as the unique identifiers of the
class and type referenced by the instance are in scope, the instance can be
resolved as usual.

Submodule Algebra
*****************

Submodule Cube
~~~~~~~~~~~~~~

Let the traits of a submodule be described by 3-dimensional, binary coordinates (x, y, z), where:

+------------------+-----------+---------+
|                  | 0         | 1       |
+==================+===========+=========+
| x (Environment)  | Unsealed  | Sealed  |
+------------------+-----------+---------+
| y (Identity)     | Anonymous | Named   |
+------------------+-----------+---------+
| z (Transparency) | Full      | Partial |
+------------------+-----------+---------+

The coordinates (0, 0, 0) are located at the origin of the 3-D space
and represent the *origin submodule*, hence the name.
The origin submodule has the least "power" of the submodule forms.

The other 7 coordinates in this 3-D space can be derived from (0, 0, 0)
by moving in 1 unit increments along the x, y, and/or z axes. These coordinates
are the vertices of the unit cube, each representing a unique
submodule form. This unit cube is called *the submodule cube*.

Origin Submodule
~~~~~~~~~~~~~~~~

The *origin submodule* is the submodule that is unsealed, anonymous, and has full transparency::

  {-# LANGUAGE Submodules #-}

  module M where

  y :: Int
  y = 5

  module where
    f :: Int -> Int
    f x = x + y

The origin submodule is:

- unsealed, so it inherits the environment it was declared in
- anonymous, so the declarations it exposes are added directly to the environment it was declared in
- fully transparent, so it exposes everything inside of it

Semantically, the example above is equivalent to::

  module M where

  y :: Int
  y = 5

  f :: Int -> Int
  f x = x + y

The origin submodule is important because all other forms can be
derived from this one.

The origin submodule does one thing: it allows you to use opens
and imports inside of it without polluting the scope outside of it.

All submodule forms contain the effects of importing a module.
This is their primary power.

End Submodule
~~~~~~~~~~~~~

Just as there is a submodule with the least "power", there is a submodule with the most.
This submodule is represented by the coordinates (1, 1, 1), and is called the *end submodule*.

Turnstile
*********

Another interesting submodule form exists 1 unit along the x-axis from the origin submodule.
It's the sealed, anonymous, fully transparent submodule form::

  {-# LANGUAGE Submodules, NoImplicitPrelude #-}

  module M (f, g) where

  module sealed where
    import Prelude

    x :: String
    x = "String"

    f :: Int -> String
    f _ = x

  y = f 5

  g = \x -> f x <> "!"

We get ``f`` and ``x`` at the top-level of the module, without exposing
the ``Prelude`` import. This works because this submodule form

- is sealed, so it doesn't see any identifiers defined outside of it
  (besides module identifiers for import declarations)
- is anonymous, so its declarations belong to its parent scope
- is fully transparent, so it exposes all of its internal declarations

To make the syntax more convenient let's introduce the *turnstile*
operator, ``⊢``. The turnstile operator takes a series of module
identifiers on the LHS, and a series of one or more declarations
on the RHS::

  {-# LANGUAGE Submodules #-}

  module M (f, g) where

  Prelude ⊢
    x :: String
    x = "String"

    f :: Int -> String
    f _ = x

  y = f 5

  g = \x -> f x <> "!"

The turnstile operator desugars to the submodule form we defined
above and has the same properties.

The turnstile operator desugars to an anonymous submodule,
but what if we want the resulting submodule to have an identifier?
We just need to wrap the turnstile with a named submodule::

  {-# LANGUAGE Submodules #-}

  module M where

  module S where A, B ⊢
    C
    D
    E

Desugars to::

  {-# LANGUAGE Submodules #-}

  module M where

  module S where
    module sealed where
      import A
      import B
      C
      D
      E

The turnstile has another important property. This::

  {-# LANGUAGE Submodules #-}

  module M where

  A ⊢ decl

is the same as this::

  {-# LANGUAGE Submodules #-}

  module M where

  ⊢
    import A
    decl

Examples
--------

There's a Haskell package for generating HTML called ``lucid``. It exposes
functions named after the HTML tag or attribute they represent, but they all have
an underscore suffix because some of the tag names clash with function names in Prelude.
One of these is ``head``::

  module M where

  import Lucid

  listHead = head

  htmlHead = head_

  ...

With submodules, we can say::

  {-# LANGUAGE Submodules, NoImplicitPrelude #-}
  
  module M where

  Prelude ⊢
    listHead = head
    ...

  Lucid ⊢
    htmlHead = head
    ...

Humans often use the same symbol or name to mean different things depending on the context.
For example, the ``+`` symbol can be used to represent addition of integers or vectors depending
on the context::

  {-# LANGUAGE Submodules, NoImplicitPrelude #-}

  module M where

  module TwoD where
    data Vec = Vec (Int, Int)

    (+) :: Vec -> Vec -> Vec
    (+) (Vec (a, b)) (Vec (c, d)) =
      let import Prelude in Vec (a + c, b + d)

  module ThreeD where
    data Vec = Vec (Int, Int, Int)

    (+) :: Vec -> Vec -> Vec
    (+) (Vec (a, b, c)) (Vec (d, e, f)) =
      let import Prelude in Vec (a + d, b + e, c + f)

  TwoD ⊢ result = Vec (1, 2) + Vec (3, 4)          -- Vec (4, 6)

  ThreeD ⊢ another = Vec (1, 2, 3) + Vec (4, 5, 6) -- Vec (5, 7, 9)

We can use submodules to disambiguate record fields::

  {-# LANGUAGE Submodules, OverloadedRecordDot #-}

  module M where

  module data Location = Mk
    { name :: String
    , lon :: Float
    , lat :: Float
    , visited :: [P.Person]
    }

  module data Person = Mk
    { name :: String
    , age :: Int
    , home :: L.Location
    , visited :: [L.Location]
    }

  import Person qualified as P
  import Location qualified as L

  visited :: P.Person -> L.Location -> (P.Person, L.Location)
  visited person location =
    ( person { P.visited = location : person.(P.visited) }
    , location { L.visited = person : location.(L.visited) }
    )

Or::

  {-# LANGUAGE Submodules, OverloadedRecordDot #-}

  module M where

  import Location qualified as L
  import Person qualified as P

  module data Location = Mk
    { name :: String
    , lon :: Float
    , lat :: Float
    , visited :: [P.Person]
    }

  module data Person = Mk
    { name :: String
    , age :: Int
    , home :: L.Location
    , visited :: [L.Location]
    }

  visited :: P.Person -> L.Location -> (P.Person, L.Location)
  visited person location =
    ( let import Person in person { visited = location : person.visited }
    , let import Location in location { visited = person : location.visited }
    )

These data declarations from the example in the ``LocalModules`` proposal::

  data Nat = Zero | Succ Nat

  data Fin :: Nat -> Type where
    Zero :: Fin (Succ n)
    Succ :: Fin n -> Fin (Succ n)

  data Elem :: a -> [a] -> Type where
    Zero :: Elem x (x : xs)
    Succ :: Elem x xs -> Elem x (y : xs)

can all be defined in the same module using contexts::

  module data Nat = Zero | Succ Nat

  module Fin where
    import Nat qualified

    data Fin :: Nat.Nat -> Type where
      Zero :: Fin (Nat.Succ n)
      Succ :: Fin n -> Fin (Nat.Succ n)

  module data Elem :: a -> [a] where
    Zero :: Elem x (x : xs)
    Succ :: Elem x xs -> Elem x (y : xs)

Effect and Interactions
-----------------------

The context of every declaration can be made more explicit.
Good synergy with ``-XRebindableSyntax``.

Costs and Drawbacks
-------------------

- Increased complexity in the Parser and Renamer
- Submodules can be abused to write code that is harder to understand

Backward Compatibility
----------------------

0. No breakage

Alternatives
------------

- `Duplicate record fields <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/duplicate_record_fields.html>`_
- `Local Modules <https://github.com/ghc-proposals/ghc-proposals/pull/283>`_
- `Namespaces <https://github.com/ghc-proposals/ghc-proposals/pull/564>`_

Unresolved Questions
--------------------

- How should Haddock work with submodules?
- Should foreign declarations be allowed in submodules?
- Should default declarations be allowed in submodules?
- How do submodules formally interact with record field labels?
- Should the turnstile also be allowed at the expression level?
  Would it be useful?
- Does the proposal have any interesting interactions with orphan instances?

Implementation Plan
-------------------

TBD

Acknowledgements
----------------

TBD
