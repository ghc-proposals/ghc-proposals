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
The overarching idea is to separate warnings about names that are genuinely
unused, and warnings that are only "transitively unused" (or "indirectly unused"), i.e. only used by
other (transitively) unused bindings.

The warnings about transitively unused names will be reported if a new flag, `-freport-indirectly-unsed-bindings` is enabled (by default, it is) *and* the appropriate existing warning flags about unused bindings is enabled, i.e. one of

* ``-Wunused-top-binds``
* ``-Wunused-local-binds``
* ``-Wunused-pattern-binds``
* ``-Wunused-binds``
* ``-Wunused-foralls``
* ``-Wunused-matches``
* ``-Wunused-imports``
* ``-Wunused-type-patterns``

In each of these cases, if a name is not used by anything, the same warning will be produced that has been produced prior to this proposal.

For example, in the following code

::

  module Foo ()

  import Data.List as L

  foo = L.intercalate

``intercalate`` is transitively unused, since it is only used by ``foo``, which is unused. The warning that's produced will be

::

  Foo.hs:5:1: warning: [-Wunused-top-binds]
      Defined but not used: ‘foo’
  
  Foo.hs:3:1: warning: [-Wunused-imports, -freport-indirectly-used-bindings]:
      The import of `Data.List' is only used by the following unused binding: ‘foo’
        except perhaps to import instances from `Data.List'
      To import instances alone, use: import Data.List()

The warning about transitively unused bindings only appears if both the warning flag
for the unused binding (in this case ``-Wunused-top-binds``) and the the one for
the transitively unused binding itself (in this case ``-Wunused-imports``) are
enabled.

If ``foo`` is only used in a situation like

::

  bar = baz + 2
    where baz = foo * 2
	
and bar is unused, a question that arises is whether the warning about ``foo`` should reference ``bar``, or ``baz``.

In this particular case, since there would be no warning about ``baz`` being unused, it seem more helpful if the warning references ``bar``.

In general, a warning should only reference other bindings about which there is an "unused" warning.

In practice, that typically means warnings about transitively unused top-level bindings will tend to reference other top-level bindings, and warnings about transitively unused
local bindings will reference other local bindings. However, if ``baz`` were unused, it would be referenced in the warning instead, since using ``bar`` in that case would not get rid of the warning about ``foo``.

If a binding ``b`` is only used recursively, the warning message will say that it is only used in ``b``.

If a group of bindings ``b1``, ..., ``bn`` is mutually recursive, and none of
the bindings are otherwise used, each of the bindings will be treated as transitively unused.
In this case, the binding site will reference the bindings of this group in whose definitions it directly occurs, e.g.

::

  Foo.hs:6:1: warning: [-Wunused-top-binds]
      ‘b1’ is defined but only used in the following unused bindings: ‘b2’, ‘b4’

Related warnings that are not affected by this proposal are ``-Wunused-pattern-bindings``,
``-Wunused-packages``, and ``-Wunused-do-bind``, since these are not about binding names.

Examples
--------

NB: The only difference between these examples and how these warnings are currently presented is that the warnings about transitively unused bindings currently don't list the bindings in which they occur, and have the same phrasing as regular unused warnings - with the exception of the unused import warning and unused forall warnings, which don't currently occur if they are only transitively unused.

::

  module Foo () where

  import Data.List as L

  foo = L.intercalate bar

  bar = baz
    where baz = undefined
          quux = wibble
          wibble = worble
          worble = quux
        
  far :: forall a (b :: a) c . c
  far = far

::

  Foo.hs:3:1: warning: [-Wunused-imports, -freport-indirectly-used-bindings]:
      The import of ‘Data.List’ is only used by the following unused binding: ‘foo’
        except perhaps to import instances from ‘Data.List’
      To import instances alone, use: import Data.List()

  Foo.hs:5:1: warning: [-Wunused-top-binds]
      Defined but not used: ‘foo’

  Foo.hs:7:1: warning: [-Wunused-top-binds]
      ‘bar' is defined but only used in the following unused binding: `foo’

  Foo.hs:9:9: warning: [-Wunused-local-binds]
      ‘quux' is defined but only used in the following unused bindings: `worble', `wirble’

  Foo.hs:10:9: warning: [-Wunused-local-binds]
      ‘wibble' is defined but only used in the following unused binding: `worble’

  Foo.hs:11:9: warning: [-Wunused-local-binds]
      ‘worble' is defined but only used in the following unused binding: `wibble’

  Foo.hs:12:9: warning: [-Wunused-local-binds]
      Defined but not used: ‘wirble’

  Foo.hs:13:15: warning: [-Wunused-foralls]
      Foo quantified type variable ‘a' is only used in the following unused variable: `(b :: a)’
      In the type signature for ‘far’

  Foo.hs:13:17: warning: [-Wunused-foralls]
      Foo quantified type variable ‘(b :: a)’
      In the type signature for ‘far’

  Foo.hs:14:1: warning: [-Wunused-top-binds]
      Defined but not used: ‘far’

Effect and Interactions
-----------------------
For the most part, the effects of this proposal are minor. The only differences for existing
code-bases are that the warning message GHC prints for the transitive warnings
is different.

Since the warnings don't have any special formats, existing tools should be able to handle them without issues.

Users that don't wish to see warnings about transitively unused bindings can turn those warnings off.

Costs and Drawbacks
-------------------
The warning mechanism is somewhat more complicated and as a consequence might
have a somewhat higher maintenance cost.

Alternatives
------------
* We could combine warnings of unused bindings and the transitive non-uses they induce. This could be similar to how error locations are combined in a single error message for duplicate declarations.
  - A possible advantage is that we could simplify the mechanism by removing the configurability of turning the warnings off.
    This would still give us the benefit of reducing the potential for confusion from these warnings, however users that wish to turn these warnings off could not do so.

* A different name could be chosen for the new flag, ``-freport-indirectly-unsed-bindings``

Implementation Plan
-------------------

`@Jadefalke256 <https://github.com/Jadefalke256>`_ has `expressed interest <https://gitlab.haskell.org/ghc/ghc/-/issues/20190#note_505317>`_ in implementing this proposal.
