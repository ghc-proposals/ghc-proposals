Contexts
========

.. author:: Rashad Gover
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/743>`_.
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
   depend on this module, or that are likely to be used with
   this module?

The 2 primary issues with modules, when looking at them as namespaces, are:

1. Importing a module brings its identifiers into the entire scope
   of the importing module. There is no way to contain the effect
   that imports have on the importing module's namespace.
2. Every module needs to be defined in its own file, so creating modules
   only for the sake of creating a fresh namespace isn't worth
   it. Creating a new namespace has a higher cost than we would like.

This proposal adds a new language construct besides modules called *contexts*
to give programmers a more lightweight, flexible alternative to modules
that addresses these issues. It also allows module import declarations in local
scopes like let statments and where clauses.

Proposed Change Specification
-----------------------------

All changes described in this specification only apply to modules that have the
``-XContexts`` language extension turned on.

Syntax
******

Contexts
~~~~~~~~

*Context declarations* are added as an extension to the ``topdecls`` production rule::

  topdecls -> ...
            | 'context' ['sealed'] [contextid] [exports] 'where' {topdecls}
            | 'context' wrappedtopdecl

  wrappedtopdecl -> 'type' simpletype = type
                  | 'data' [context =>] simpletype [= constrs] [deriving]
                  | 'newtype' [context =>] simpletype = newconstr [deriving]
                  | 'class' [scontext =>] tycls tyvar ['where' cdecls]

They start off with the ``context`` keyword, followed by optional *context modifiers*:
the ``sealed`` keyword, a *context identifier*, and an export list. After the ``context``
keyword and optional context modifiers, the ``where`` keyword is used and followed by a
block of declarations. Here's an example of a context declaration using all modifiers::

  context sealed C (type T, new, add) where
    data T = T { bar :: Int }

    new :: Int -> T
    new = T

    add :: T -> T -> T
    add (T x) (T y) = T (x + y)

Note that the production rule for context declarations is recursive,
so contexts can be nested within other contexts::

  context A where
    context B where
      context C where
        foo :: Int -> Int
        foo x = x * 3

Besides the normal context declaration syntax, there's
syntactic sugar for declaring contexts called *context wrapping*.
Context wrapping is done by prefixing one of the allowed ``topdecls``
with the ``context`` keyword. This context wrapping::

  context data T = T { foo :: Int, bar :: Float }

desugars to::

  context T where
    data T = T { foo :: Int, bar :: Float }

The identifier of the resulting context is the same as the identifier of the
wrapped declaration. Since context identifiers exist in a separate namespace from
type identifiers, class identifiers, etc., reusing the identifier for
the context doesn't cause any ambiguity.

Open Declarations
~~~~~~~~~~~~~~~~~

The ``topdecls`` and ``decls`` grammars are extended with open declarations.
Open declarations use the ``open`` keyword followed by a context identifier.
There is no import list equivalent for open declarations, so they have no control
over what identifiers a context exposes. Only where they're exposed.

Import Declarations
~~~~~~~~~~~~~~~~~~~

This proposal also extends the rules around import declarations::

  decl -> gendecl
        | (funlhs | pat) rhs
        | importdecl

This extension to the declaration production rule allows for import declarations
inside of let statements and where clauses.

Context Semantics
*****************

Context declarations have 3 orthogonal, binary traits:

1. Environment - *Unsealed* or *Sealed*. What environment does the context have?
2. Identity - *Anonymous* or *Named*. How are the context's declarations exposed?
3. Transparency - *Full* or *Partial*. What declarations does the context expose?

There are 2^3 = 8 possible forms of context declaration.

Environment
~~~~~~~~~~~

*Unsealed contexts* have standard lexical scoping behavior.

They *inherit* the environment of the scope they're declared in::

  {-# LANGUAGE Contexts #-}

  module M where
  
  import N (context T)

  y :: Int
  y = 32

  context S where
    open T -- exposes z

    f :: Int -> Int
    f x = x + y + z

Identifiers inside an unsealed context may *shadow* identifiers from the outer scope::

  {-# LANGUAGE Contexts #-}

  module M where

  import N (context T)

  y :: Int
  y = 32

  context S where
    open T

    y :: Int
    y = 10

    f :: Int -> Int
    f x = x + y + z

*Sealed contexts* do not inherit their outer context like unsealed contexts do::

  {-# LANGUAGE Contexts #-}

  module M where
  
  import N (context T)

  y :: Int
  y = 32

  context sealed S where
    import Prelude
    open T -- exposes z

    f :: Int -> Int
    f x = x + y + z -- Error! y is unknown

Since sealed contexts do not inherit their outer context,
declarations inside of sealed contexts cannot shadow outer ones::

  {-# LANGUAGE Contexts #-}

  module M where
  
  import N (context T)

  y :: Int
  y = 32

  context sealed S where
    import Prelude
    open T

    y :: Int -- This doesn't shadow the outer y.
    y = 10

    f :: Int -> Int
    f x = x + y + z

The only way to bring an identifier into a sealed context is through an explicit open or import declaration::

  {-# LANGUAGE Contexts #-}

  module M where

  context C where
    ...

  context sealed S where
    import O (context A, context B)
    open A
    open B
    open C
    ...

or by declaring a fresh identifier inside the sealed context.

In summary, sealing a context is useful if you need a blank slate
where internal declarations only depend on:

- Identifiers brought in scope via explicit opens and imports
- Identifiers declared directly in the context

Identity
~~~~~~~~

*Anonymous contexts* cannot be referenced since they don't have a identifier.

This means you cannot export or import an anonymous context::

  {-# LANGUAGE Contexts #-}

  module M where -- I can't export the anon context...What name?
  
  import N (context T)

  context (f) where
    open T
    f :: ...
    g :: ...

And you cannot explicitly open anonymous contexts::

  {-# LANGUAGE Contexts #-}

  module M where
  
  import N (context T)

  context (f) where -- I can't use open on this context...What name?
    open T
    f :: ...
    g :: ...

Instead, anonymous contexts are opened at the point of their declaration::

  {-# LANGUAGE Contexts #-}

  module M where
  
  import N (context T)

  context (f) where
    open T
    f :: ...
    g :: ...

  h x = f x -- f is exposed by the anonymous context

If a context does not have an identifier, the only logical thing to do is open it where it
is, since it cannot be referenced elsewhere.

Named contexts on the other hand can be exported, imported, and
opened explicitly::

  {-# LANGUAGE Contexts #-}

  module M (context S) where
  
  import N (context T)

  context S where
    open T
    ...

In short, naming a context allows it to be opened
somewhere other than where it was declared.

Not naming a context ensures that the context and its
declarations cannot be referenced anywhere else than
where it was declared.

Transparency
~~~~~~~~~~~~

*Fully transparent* contexts expose all identifiers inside of them when opened::

  {-# LANGUAGE Contexts #-}

  module M where

  context S where
    x = 5
    y = 10
    z = 20

  open S

  a = x + y + z

*Partially transparent* contexts have an export list, and only
expose identifiers listed in the export list::

  {-# LANGUAGE Contexts #-}

  module M where

  context S (x, z) where
    x = 5
    y = 10
    z = 20

  open S

  a = x + y + z -- Error! No y in scope.

Export lists can use ``-XExplicitNamespaces``::

  {-# LANGUAGE Contexts, ExplicitNamespaces #-}

  module M where

  context S (type T, type R, class C(..)) where
    type T = T

    type R = R { l :: Int }

    class C where
      m :: Int

Only identifiers that are declared in the context can be added to the context's export list.
Identifiers brought in via ``open`` or ``import`` cannot be added to the context's export list.

The transparency of a context determines what identifiers are exposed when the context
is opened.

Instances
~~~~~~~~~

Instance declarations are allowed inside contexts, but they are
still associated with the module they are declared in.
Instance declarations cannot be added to or excluded from the export list
of a context declaration. They are always exported and always imported (if the module is).
Instance declarations are the only declarations with this behavior.

The behavior of instances is unaffected by the existence of contexts.
The only difference is that instance declarations can now refer to locally scoped
type and class identifiers. As long as the unique identifiers of the
class and type referenced by the instance are in scope, the instance can be
resolved.

Context Algebra
***************

Context Cube
~~~~~~~~~~~~

Let the traits of a context be described by 3-dimensional, binary coordinates (x, y, z), where:

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
and represent the *origin context*, hence the name.
The origin context has the least "power".

The other 7 coordinates in this 3-D space can be derived from (0, 0, 0)
by moving in 1 unit increments along the x, y, and/or z axes. These coordinates
are the vertices of the unit cube, each representing a unique
context declaration form. This unit cube is called *the context cube*.

Origin Context
~~~~~~~~~~~~~~

The *origin context* is the context that is unsealed, anonymous, and has full transparency::

  {-# LANGUAGE Contexts #-}

  module M where

  y :: Int
  y = 5

  context where
    f :: Int -> Int
    f x = x + y

The origin context is:

- unsealed, so it inherits the environment it was declared in
- anonymous, so the declarations it exposes are added directly to the environment it was declared in
- fully transparent, so it exposes everything inside of it

Semantically, the example above is equivalent to::

  module M where

  y :: Int
  y = 5

  f :: Int -> Int
  f x = x + y

The origin context is important because all other forms can be
derived from this one.

The origin context does one thing: it allows you to use opens
and imports inside of it without polluting the outer context.

All context declaration forms contain the effects of opening a context or importing a module.
This is their primary power.

Top Context
~~~~~~~~~~~

Just as there is a context with the least "power", there is a context with the most.
This context is represented by the coordinates (1, 1, 1), and is called the *top context*.

Turnstile
*********

Another interesting form is 1 unit along the x-axis from the origin context.
It's the sealed, anonymous, fully transparent context form::

  {-# LANGUAGE Contexts, NoImplicitPrelude #-}

  module M (f, g) where

  context sealed where
    import Prelude

    x :: String
    x = "String"

    f :: Int -> String
    f _ = x

  y = f 5

  g = \x -> f x <> "!"

We get ``f`` and ``x`` at the top-level of the module, without exposing
the ``Prelude`` import. This works because this context declaration form

- is sealed, so it doesn't see any identifiers defined outside of it
  (besides module/context identifiers for importing/opening)
- is anonymous, so it's opened in the scope it's declared in
- is fully transparent, so it exposes all of its internal declarations

To make the syntax more convenient let's introduce the *turnstile*
operator, ``⊢``. The turnstile operator takes a series of module
or context identifiers on the LHS, and a series of one or more declarations
on the RHS::

  {-# LANGUAGE Contexts #-}

  module M (f, g) where

  Prelude ⊢
    x :: String
    x = "String"

    f :: Int -> String
    f _ = x

  y = f 5

  g = \x -> f x <> "!"

The turnstile operator desugars to the context form we defined
above and has the same properties.

The turnstile operator desugars to an anonymous context,
but what if the context needs to have a identifier?
We just need to wrap the turnstile with a named context
declaration::

  {-# LANGUAGE Contexts #-}

  module M where

  context S where A, B ⊢
    C
    D
    E

Desugars to::

  {-# LANGUAGE Contexts #-}

  module M where

  context S where
    context sealed where
      open A
      open B
      C
      D
      E

The turnstile has another important property. This::

  {-# LANGUAGE Contexts #-}

  module M where

  A ⊢ decl

is the same as this::

  {-# LANGUAGE Contexts #-}

  module M where

  ⊢
    open A
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

With contexts, we can say::

  {-# LANGUAGE Contexts, NoImplicitPrelude #-}
  
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

  {-# LANGUAGE Contexts, NoImplicitPrelude #-}

  module M where

  context TwoD where
    data Vec = Vec (Int, Int)

    (+) :: Vec -> Vec -> Vec
    (+) (Vec (a, b)) (Vec (c, d)) =
      let import Prelude in Vec (a + c, b + d)

  context ThreeD where
    data Vec = Vec (Int, Int, Int)

    (+) :: Vec -> Vec -> Vec
    (+) (Vec (a, b, c)) (Vec (d, e, f)) =
      let import Prelude in Vec (a + d, b + e, c + f)

  TwoD ⊢ result = Vec (1, 2) + Vec (3, 4)          -- Vec (4, 6)

  ThreeD ⊢ another = Vec (1, 2, 3) + Vec (4, 5, 6) -- Vec (5, 7, 9)

Effect and Interactions
-----------------------

The context of every declaration can be made much more explicit.
Good synergy with ``-XRebindableSyntax``.

Costs and Drawbacks
-------------------

- Increased complexity in the Parser and Renamer
- Contexts can be abused to write code that is harder to understand

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

- How should Haddock work with contexts?
- Should foreign declarations be allowed in contexts?
- Should default declarations be allowed in contexts?
- How do contexts formally interact with record field labels?
- Should the turnstile also be allowed at the expression level?
  Would it be useful?
- Should open declarations support ``qualifed`` and ``as``?
  If yes, what symbol should be used for context qualification?
- Does the proposal have any interesting interactions with orphan instances?

Implementation Plan
-------------------

TBD

Acknowledgements
----------------

TBD
