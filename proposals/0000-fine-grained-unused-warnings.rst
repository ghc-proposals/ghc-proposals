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
unused, and warnings that are only "transitively unused", i.e. only used by
other (transitively) unused bindings.

The warnings about transitively unused names will be controlled by the existing warning flags about unused bindings:

* ``-Wunused-top-binds``
* ``-Wunused-local-binds``
* ``-Wunused-pattern-binds``
* ``-Wunused-binds``
* ``-Wunused-foralls``
* ``-Wunused-matches``
* ``-Wunused-imports``
* ``-Wunused-type-patterns``

In each of these cases, if a name is not used by anything, the same warning will be produced that has been produced prior to this proposal.
However, now, if the definition associated with that name contains transitively unused names, the warning may additionally contain sub-warnings about those bindings.

For example, in the following code

::

  module Foo ()

  import Data.List as L

  foo = L.intercalate

``intercalate`` is transitively unused, since it is only used by ``foo``, which is unused. The warning that's produced will be

::

  Foo.hs:5:1: warning: [-Wunused-top-binds]
      Defined but not used: ‘foo’
      as a consequence the following imports are transitively unused [-Wunused-imports]:
        Data.List imported at foo.hs:3:1:

The warning about transitively unused bindings only appears if both the warning flag
for the unused binding (in this case ``-Wunused-top-binds``) and the the one for
the transitively unused binding itself (in this case ``-Wunused-imports``) are
enabled.

If a binding ``b`` is used in the definitions of more than one binding, but all
of those bindings are unused, a warning about ``b`` being transitively unused
will only be shown in exactly one of the unused bindings, assuming all of the
relevant warning flags are enabled as well. Which one is unspecified by this
proposal and may be chosen by the implementation.

If a binding ``b`` is only used recursively, it will be treated as being unused.

If a group of bindings ``b1``, ..., ``bn`` is mutually recursive, and none of
the bindings are otherwise used, the result is a warning of the form

::

  Foo.hs:6:1: warning: [-Wunused-top-binds]
      Defined but only used in a mutually recursive group:
        ‘b1’ defined at Foo.hs:6:1
        ‘b2’ defined at Foo.hs:7:1
        ...
        ‘bn’ defined at Foo.hs:18:1

The error location is the location of the first binding in the group.

This is similar to how multiple error locations are reported in errors about duplicate declarations.

Related warnings that are not affected by this proposal are ``-Wunused-pattern-bindings``,
``-Wunused-packages``, and ``-Wunused-do-bind``, since these are not about binding names.

Examples
--------

::

  module Foo ()

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

  Foo.hs:5:1: warning: [-Wunused-top-binds]
      Defined but not used: ‘foo’
      as a consequence the following imports are transitively unused [-Wunused-imports]:
        Data.List imported at foo.hs:3:1:
      and the following top-level binds are transitively unused [-Wunused-top-binds]:
        bar defined at foo.hs:7:1:
      and the following local binds are transitively unused [-Wunused-local-binds]:
        baz defined at foo.hs:8:9:
  
  Foo.hs:9:9: warning: [-Wunused-local-binds]
      Defined but only used in a mutually recursive group:
        ‘baz’ defined at Foo.hs:9:9
        ‘quux’ defined at Foo.hs:10:9
        ‘wibble’ defined at Foo.hs:11:9
        ‘worble’ defined at Foo.hs:12:9
  
  Foo.hs:14:17: warning: [-Wunused-foralls]
        Unused quantified type variable `(b :: a)'
        In the type signature for `far'
        as a consequence the following quantified type variables are transitively unused [-Wunused-foralls]:
          a defined at foo.hs:14:15
    
  Foo.hs:15:1: warning: [-Wunused-top-binds]
      Defined but not used: ‘far’

Effect and Interactions
-----------------------
For the most part, the effects of this proposal are minor. The only differences for existing
code-bases are that the warning message GHC prints for the transitive warnings
is different.

Costs and Drawbacks
-------------------
The warning mechanism is somewhat more complicated and as a consequence might
have a somewhat higher maintenance cost.

Alternatives
------------
* The warnings for transitively unused bindings need not be combined with the
  warnings for unused bindings

* There could be new warning flags specifically for controlling transitively
  unused warnings

Implementation Plan
-------------------
`@Jadefalke256 <https://github.com/Jadefalke256>`_ has
 `expressed interest
<https://gitlab.haskell.org/ghc/ghc/-/issues/20190#note_505317>`_ in
implementing this proposal.
