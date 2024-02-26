Fine-Grained Unused Warnings
============================

.. author:: Jakob Brünker
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/434>`_.
.. contents::

This proposal aims to make warnings about unused names more precise.

Motivation
----------

::

  a :: Int
  a = 4
    where bar = foo
          foo = 3

Compiling the above code with ``-Wall`` results in the
these warnings:

::

      Defined but not used: `bar'
      Defined but not used: `foo'

Sometimes, I look at a warning like this and think "I could have sworn I used
``foo`` somewhere" - but of course, what the warning is trying to say isn't
actually that ``foo`` is unused, but rather that all of ``foo``'s usage sites
are in unused bindings.

Furthermore, while it's useful to see such a warning for CI, to immediately
know all of the bindings that have to be removed to eliminate the warnings,
during development, these additional warnings take up a lot of valuable
screen real estate in ghcid.

Proposed Change Specification
-----------------------------

The proposed change aims to distinguish between genuinely (or directly) unused bindings and indirectly unused bindings. While the warnings for directly unused bindings remain unchanged, the warnings for indirectly unused bindings will now be controlled by a new flag, ``-Windirectly-unused-binds``, which is enabled by default.

1. **Relevant warning flag**. The **relevant warning flag** for a variable binding is defined as follows:

   - The relevant warning flag for top-level binds is ``-Wunused-top-binds``
   - The relevant warning flag for binds defined in ``where`` or ``let`` blocks is ``-Wunused-local-binds``
   - The relevant warning flag for variables bound by a pattern match is ``-Wunused-matches``
   - The relevant warning flag for type variables bound by ``forall`` is ``-Wunused-foralls``
   - The relevant warning flag for modules and names from modules that are being imported is ``-Wunused-imports``
   - The relevant warning flag for type variables bound in type patterns is ``-Wunused-type-patterns``
  
   Note: Related warning flags that are not affected by this proposal since they are not about binding names are

   - ``-Wunused-pattern-binds``
   - ``-Wunused-packages``
   - ``-Wunused-do-bind``

2. **Directly Unused Bindings:** A binding *B* is **directly unused** if it is referenced only in *B*'s own strongly-connected component, and the relevant warning flag is enabled.

   Viewing a set of definitions as a graph where each binding form a vertex, and each reference in the binding's body to another binding forms a directed edge, the strongly connected component of a vertex *B* is the largest possible set of vertices including *B* such there is a path from any vertex to any other vertex.

3. **Indirectly Unused Bindings:** A binding *B* is **indirectly unused** if it is directly unused, or referenced only in the body of an indirectly unused binding within the scope *B* is defined in, and the relevant warning flag is enabled.

   Limiting the second part of the definition to *B*'s scope let's us avoid generating a lot of unhelpful warnings in cases where the top-level binding which local bindings are defined in is unused:

   ::

     foo = b
       where b = bar

   Even if ``foo`` is unused, this will not throw a warning for ``b``, since ``foo`` is not defined within the scope ``b`` is defined in.

4. **Recursive and Mutual Recursive Bindings**:

   - From point 1. we can infer that if a binding is used only (mututally) recursively, it is directly unused.

   - For mutually recursive bindings, if none of the bindings in the group are used outside their mutual recursion, each binding in the group is directly unused. The warning for each binding will list the other bindings in the group it is directly involved with, e.g.

     ::
    
       Foo.hs:6:1: warning: [-Wunused-top-binds, -Windirectly-unused-binds]
           ‘b1’ is defined but used only in the following unused bindings: ‘b2’, ‘b4’

5. **Import and ``forall`` Bindings:** The proposal also extends to warnings about indirectly unused imports and ``forall`` binds. Both are considered to be unused if they are used only in definitions or type declarations of unused bindings, with the same direct vs. indirect distinction.

**Warning References and Messages:**

- A binding will produce a warning if

  - it is directly unused, or

  - it is indirectly unused and ``-Windirectly-unused-binds`` is enabled

- The warnings for indirectly unused bindings will reference all bindings they are used in. For example, if ``-Wunused-top-binds`` and ``-Wunused-local-binds`` are enabled,

  ::

    foo = bar
    baz = pureStrLn "Hi"
      where quux = bar
    bar = ...
    main = baz

  will produce three warnings:

  - ``foo`` is directly unused

  - ``quux`` is directly unused

  - ``bar`` is indirectly unused, and will produce a warning stating

    ::

      warning: [-Wunused-top-binds, -Windirectly-unused-binds]
          ‘bar' is defined but used only in the following unused bindings: ‘foo’, ‘quux’

- The warning for an indirectly unused binding B will reference the innermost (directly or indirectly) unused binding(s) whose right-hand sides mention B. For example, suppose ``bar`` is

  ::

    module M(f) where
    f = 22
    foo = 7
    wombat = 8
    bar = quux + 2
      where quux = foo * 2
            wux  = wombat + 1

  In this example,

  - ``quux`` is not unused (it is used in the right-hand side of ``bar``, which is not defined within the scope of ``quux``'s definition), while ``wux`` and ``bar`` are directly unused.
  - The binding ``wombat`` is indirectly unused; it's warning will mention ``wux`` (the innermost unused binding in which ``wombat`` is mentioned).
  - The binding for ``foo`` is also indirectly unused, but its warning will mention ``bar`` (not ``quux``) since ``bar`` is the innermost unused binding enclosing the refernce to ``foo``.

Examples
--------

::

  module Foo () where

  import Data.List as L

  foo = L.intercalate bar

  bar = baz
    where baz = undefined
          quux = wibble
          wibble = worble
          worble = quux
          wirble = quux
        
  far :: forall a (b :: a) c . c
  far = far

Currently, without this proposal, the file results in the following warnings, assuming ``-Wunused-imports``, ``-Wunused-top-binds``, ``-Wunused-local-binds``, and ``-Wunused-foralls`` are enabled:

::

  Foo.hs:5:1: warning: [-Wunused-top-binds]
      Defined but not used: ‘foo’

  Foo.hs:7:1: warning: [GHC-38417] [-Wmissing-signatures]
      Top-level binding with no type signature: bar :: a

  Foo.hs:7:1: warning: [-Wunused-top-binds]
      Defined but not used: ‘bar’

  Foo.hs:9:9: warning: [-Wunused-local-binds]
      Defined but not used: ‘quux’

  Foo.hs:10:9: warning: [-Wunused-local-binds]
      Defined but not used: ‘wibble’

  Foo.hs:11:9: warning: [-Wunused-local-binds]
      Defined but not used: ‘worble’

  Foo.hs:12:9: warning: [-Wunused-local-binds]
      Defined but not used: ‘wirble’

  Foo.hs:14:17: warning: [-Wunused-foralls]
      Unused quantified type variable ‘(b :: a)’
      In the type signature for ‘far’

  Foo.hs:15:1: warning: [-Wunused-top-binds]
      Defined but not used: ‘far’

With this proposal, these warnings would be produced instead, assuming ``-Windirectly-unused-binds`` is enabled:

::

  Foo.hs:3:1: warning: [-Wunused-imports, -Windirectly-unused-binds]:
      The import of ‘Data.List’ is used only by the following unused binding: ‘foo’
        except perhaps to import instances from ‘Data.List’
      To import instances alone, use: import Data.List()

  Foo.hs:5:1: warning: [-Wunused-top-binds]
      Defined but not used: ‘foo’

  Foo.hs:7:1: warning: [-Wunused-top-binds, -Windirectly-unused-binds]
      ‘bar' is defined but used only in the following unused binding: ‘foo’

  Foo.hs:9:9: warning: [-Wunused-local-binds, -Windirectly-unused-binds]
      ‘quux' is defined but used only in the following unused bindings: ‘worble’, ‘wirble’

  Foo.hs:10:9: warning: [-Wunused-local-binds, -Windirectly-unused-binds]
      ‘wibble' is defined but used only in the following unused binding: ‘worble’

  Foo.hs:11:9: warning: [-Wunused-local-binds, -Windirectly-unused-binds]
      ‘worble' is defined but used only in the following unused binding: ‘wibble’

  Foo.hs:12:9: warning: [-Wunused-local-binds]
      Defined but not used: ‘wirble’

  Foo.hs:13:15: warning: [-Wunused-foralls, -Windirectly-unused-binds]
      Quantified type variable ‘a’ is used only in the following unused variable: ‘(b :: a)’
      In the type signature for ‘far’

  Foo.hs:13:17: warning: [-Wunused-foralls, -Windirectly-unused-binds]
      Unused quantified type variable ‘(b :: a)’
      In the type signature for ‘far’

  Foo.hs:14:1: warning: [-Wunused-top-binds]
      Defined but not used: ‘far’


Effect and Interactions
-----------------------
For the most part, the effects of this proposal are minor. The main differences for existing
code-bases are that the warning message GHC prints for the indirect warnings
is different, although due to the more consistent treatment of warning flags, existing code bases can also sometimes
get more or fewer warnings in cases of indirectly unused bindings where two warning flags interact with one another. There can also be additional warnings about indirectly unused imports and `forall` binds.

Since the warnings don't have any special formats, existing tools should be able to handle them without issues.

Users that don't wish to see warnings about indirectly unused bindings can turn those warnings off.

Costs and Drawbacks
-------------------
The warning mechanism is somewhat more complicated and as a consequence might
have a somewhat higher maintenance cost, which might especially concern listing out all the other bindings that an indirectly unused binding is referenced by.

Alternatives
------------
* We could combine warnings of unused bindings and the indirect non-uses they induce. This could be similar to how error locations are combined in a single error message for duplicate declarations.

  * A possible advantage is that we could simplify the mechanism by removing the configurability of turning the warnings off.
    This would still give us the benefit of reducing the potential for confusion from these warnings, however users that wish to turn these warnings off could not do so.

  * A disadvantage is that most third-party tools dealing with error messages will likely have a harder time parsing the warning messages.

* A different name could be chosen for the new flag, ``-Windirectly-unused-binds``. For example:
  * ``-freport-indirect-uses``

* Instead of ``-Windirectly-unused-binds``, we could separate each warning flag (like ``-Wunused-imports``)
  into two (like ``-Windirectly-unused-imports`` and ``-Wdirectly-unused-imports``) and a warnings group like ``-Wno-indirect-uses`` to turn off all warnings about indirectly unused bindings at once.

  * This would offer more configurability if users want to see some warnings about indirectly unused bindings but not others.

  * It would require a higher number of warning flags.

Implementation Plan
-------------------

`@Jadefalke256 <https://github.com/Jadefalke256>`_ has `expressed interest <https://gitlab.haskell.org/ghc/ghc/-/issues/20190#note_505317>`_ in implementing this proposal.
