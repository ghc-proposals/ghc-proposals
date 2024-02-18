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

The proposed change aims to distinguish between genuinely (or directly) unused bindings and indirectly unused bindings. While the warnings for directly unused bindings remain unchanged, the warnings for indirectly unused bindings will now be controlled by a new flag, ``-freport-indirectly-unused-bindings``, which is enabled by default.

1. **Directly Unused Bindings:** A binding *B* is **directly unused** if it is referenced only in *B*'s own strongly-connected component.

   Viewing a set of definitions as a graph where each binding form a vertex, and each reference in the binding's body to another binding forms a directed edge, the strongly connected component of a vertex *B* is the largest possible set of vertices including *B* such there is a path from any vertex to any other vertex.

2. **Indirectly Unused Bindings:** A binding *B* is **indirectly unused** if it is referenced only in *B*'s own strongly-connected component, or the body of an indirectly unused binding in the scope *B* is defined in. The warning for these bindings will be reported only if ``-freport-indirectly-unused-bindings`` and the relevant existing warning flags (e.g., ``-Wunused-top-binds``, ``-Wunused-local-binds``) are enabled.

   Limiting the second part of the definition to *B*'s scope let's us avoid generating a lot of unhelpful warnings in cases where the top-level binding which local bindings are defined in is unused.

3. **Recursive and Mutual Recursive Bindings**:

   - From point 1. we can infer that if a binding is used only (mututally) recursively, it is directly unused.

   - For mutually recursive bindings, if none of the bindings in the group are used outside their mutual recursion, each binding in the group is directly unused. The warning for each binding will list the other bindings in the group it is directly involved with, e.g.

     ::
    
       Foo.hs:6:1: warning: [-Wunused-top-binds]
           ‘b1’ is defined but used only in the following unused bindings: ‘b2’, ‘b4’

4. **Import and `forall` Bindings:** The proposal also extends to warnings about indirectly unused imports and ``forall`` binds. Both are considered to be unused if they are used only in definitions or type declarations of unused bindings, with the same direct vs. indirect distinction.

**Warning References and Messages:**

- A binding will produce a warning if

  - the relevant warning flag (e.g. ``-Wunused-local-binds``) is enabled, and one of the following is true:

  - it is directly unused, or

  - it is indirectly unused *and* all of the bindings it *is* used in produce a warning about being unused, due to matching the criteria laid out in these bullet points (and ``-freport-indirectly-unused-bindings`` is on)

    - This means that e.g. if a top-level bind is used only in an unused local bind, both ``-Wunused-top-binds`` *and* ``-Wunused-local-binds`` must be enabled.

- The warnings for indirectly unused bindings will reference all bindings they are used in that throw a warning. For example, if ``-Wunused-top-binds`` and ``-Wunused-local-binds`` are enabled,

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

      warning: [-Wunused-top-binds]
          ‘bar' is defined but used only in the following unused bindings: ‘foo’, ‘quux’

- If there is a chain of indirectly unused bindings, e.g. ``a`` is used in ``b``, which is used in ``c``, which is used in ``d``, the question arises whether the warning about ``a`` should reference ``b``, ``c``, or ``d``. The answer is that it will reference the first binding in that chain that produces a warning (and ``a`` will produce no warning at all if none of them produce a warning). For example:

  ::
    
    bar = quux + 2
      where quux = foo * 2

  If ``foo`` is used only here, and ``bar`` is not used anywhere, the warning about ``foo`` will reference ``bar`` rather than ``quux``, since ``quux`` does not throw a warning, as according to the exception in the definition above, it is not considered "indirectly unused".
- The warning flags that are relevant are
    - ``-Wunused-top-binds``
    - ``-Wunused-local-binds``
    - ``-Wunused-pattern-binds``
    - ``-Wunused-binds``
    - ``-Wunused-foralls``
    - ``-Wunused-matches``
    - ``-Wunused-imports``
    - ``-Wunused-type-patterns``
- Related warning flags that are not affected by this proposal since they are not about binding names are
    - ``-Wunused-pattern-bindings``
    - ``-Wunused-packages``
    - ``-Wunused-do-bind``

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

With this proposal, these warnings would be produced instead, assuming ``-freport-indirectly-unused-bindings`` is enabled:

::

  Foo.hs:3:1: warning: [-Wunused-imports, -freport-indirectly-unused-bindings]:
      The import of ‘Data.List’ is used only by the following unused binding: ‘foo’
        except perhaps to import instances from ‘Data.List’
      To import instances alone, use: import Data.List()

  Foo.hs:5:1: warning: [-Wunused-top-binds]
      Defined but not used: ‘foo’

  Foo.hs:7:1: warning: [-Wunused-top-binds]
      ‘bar' is defined but used only in the following unused binding: ‘foo’

  Foo.hs:9:9: warning: [-Wunused-local-binds]
      ‘quux' is defined but used only in the following unused bindings: ‘worble’, ‘wirble’

  Foo.hs:10:9: warning: [-Wunused-local-binds]
      ‘wibble' is defined but used only in the following unused binding: ‘worble’

  Foo.hs:11:9: warning: [-Wunused-local-binds]
      ‘worble' is defined but used only in the following unused binding: ‘wibble’

  Foo.hs:12:9: warning: [-Wunused-local-binds]
      Defined but not used: ‘wirble’

  Foo.hs:13:15: warning: [-Wunused-foralls]
      Quantified type variable ‘a’ is used only in the following unused variable: ‘(b :: a)’
      In the type signature for ‘far’

  Foo.hs:13:17: warning: [-Wunused-foralls]
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

* A different name could be chosen for the new flag, ``-freport-indirectly-unused-bindings``. For example:
  * ``-freport-indirect-uses``

* Instead of ``-freport-indirectly-unused-bindings``, we could separate each warning flag (like ``-Wunused-imports``)
  into two (like ``-Windirectly-unused-imports`` and ``-Wdirectly-unused-imports``) and a warnings group like ``-Wno-indirect-uses`` to turn off all warnings about indirectly unused bindings at once.

  * This would offer more configurability if users want to see some warnings about indirectly unused bindings but not others.

  * It would require a higher number of warning flags.

Implementation Plan
-------------------

`@Jadefalke256 <https://github.com/Jadefalke256>`_ has `expressed interest <https://gitlab.haskell.org/ghc/ghc/-/issues/20190#note_505317>`_ in implementing this proposal.
