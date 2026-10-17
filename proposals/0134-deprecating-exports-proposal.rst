Deprecating Exports
===================

.. author:: Alanas Plascinskas
.. date-accepted:: 2018-07-14
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/4879
.. implemented:: 9.8
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/134>`_, with `an amendment discussed here <https://github.com/ghc-proposals/ghc-proposals/pull/595>`_.
.. contents::

This proposal introduces the ability to attach deprecation and warnings to
top-level items in module export lists.

Motivation
----------
To help transition between different versions of libraries, it can be useful to
deprecate re-exported identifiers, for example to signal that they will stop
being exported in a future version, and to recommend alternatives (e.g. to import
the identifiers from a different module, or to do something else entirely).

This feature was originally requested in ticket https://gitlab.haskell.org/ghc/ghc/issues/4879.

Proposed Change Specification
-----------------------------
The changes consist of the following aspects:

* A new syntax for attaching warning and deprecations to exports,

* The ability to decide when to emit deprecation warnings, which involves:

  - deciding which exported identifiers are deprecated, and
  - deciding when to emit warnings at use-sites of imported deprecated
    identifiers.

Syntax changes
~~~~~~~~~~~~~~
We introduce the following syntax in module export lists: ::

    module Data.List
    (  ...
        {-# DEPRECATED "Exported from Data.String instead" #-}
        lines,
        ...
    ) where
    ...

The deprecation pragma has an effect on the export item that immediately
follows it. Deprecation pragmas are only allowed on top-level exports items.
This means we would reject the following: ::

    module M ( T ( {-# DEPRECATED "msg" #-} fld ) ) where { .. }

Emitting warnings
~~~~~~~~~~~~~~~~~
A deprecated export indicates the intention for a module to stop exporting
a certain identifier in the future.

Suppose we have: ::

  module M ( {-# DEPRECATED "msg" #-} foo, ... ) where { .. }

  module N where { import M; ... }

The general principle (which will be refined in the subsequent sections) is
that GHC should emit a deprecation warning about ``foo`` in module ``N``
**if and only if** removing the export item ``foo`` from the
export list of ``M`` would break ``N``. That is, it should be the case that:

#. ``foo`` is only made available in ``M`` through that export item (i.e.
   removing the deprecated export item would genuinely cause breakage;
   ``foo`` is not made available through other export items).

#. Uses of ``foo`` in ``N`` should only come from the import of ``M``; ``foo``
   is not available through other imports.

We will now make these two requirements more precise.

Attaching warnings to exports
+++++++++++++++++++++++++++++
An identifier ``i`` exported by a module ``M`` is considered to be deprecated with
message "msg" if every export item of ``M`` that exports ``i`` has a deprecation
pragma with message "msg".

In practice:

* If all of the export items of ``M`` exporting ``i`` have a deprecation pragma
  with message "msg", mark ``i`` as a deprecated export of ``M`` with deprecation
  message "msg".

* If some of the export items of ``M`` exporting ``i`` are not deprecated,
  mark ``i`` as a **non**-deprecated export of ``M``. If, in addition, some
  export items exporting ``i`` are deprecated, emit a warning that we are
  discarding some deprecation messages.

* If several of the export items of ``M`` exporting ``i`` have deprecation
  pragmas, but the messages are not all the same, emit a
  "conflicting deprecation messages" error.

To illustrate, suppose that we have: ::

  module A where { data T = C | D }

The above specification leads us to decide which deprecation messages
to attach to various exported identifiers: ::

    module B1
      ( {-# DEPRECATED "don't use C" #-} T(C)
      , T(D)
      ) where { import A }

      -- Result: only the constructor C of T is deprecated, with message "don't use C".
      --         We emit a warning that T itself will not be deprecated,
      --         because it is explicitly exported twice, once with a deprecation
      --         and once without.


    module B2
      ( {-# DEPRECATED "msg" #-} T(C)
      , {-# DEPRECATED "msg" #-} T(D)
      ) where { import A }

    -- Result: T, C and D are all deprecated, with message "msg".


    module B3
      ( {-# DEPRECATED "msg1" #-} T(C)
      , {-# DEPRECATED "msg2" #-} T(D)
      ) where { import A }

    -- Result: error, because T has two conflicting deprecation messages,
    --         "msg1" and "msg2".


    module B4
      ( {-# DEPRECATED "msg" #-} T(C)
      , module A
      ) where { import A }

    -- Result: T and C are not deprecated: they are made available twice, once
    --         with a deprecation and once without. We emit a warning that we are
    --         discarding a DEPRECATED pragma.


    module B5
      ( {-# DEPRECATED "msg1" #-} T(C)
      , {-# DEPRECATED "msg2" #-} module A
      ) where { import A }

    -- Result: error because of conflicting deprecation messages


To re-export a whole module while deprecating a single identifier,
one might proceed as follows ::

  -- U.hs
  module U where { bad = "bad"; ... }

  -- V.hs
  module V ( module U, {-# DEPRECATED "msg" #-} U2.bad ) where
    import U hiding (bad)
    import qualified U as U2 (bad)

  -- W.hs
  module W where { import V (bad) }

This is in keeping with the general principle that we should get a warning in
``W`` precisely when removing the deprecated export of ``bad`` in ``V`` would
cause breakage. Contrast with example ``B4`` above, where removing the deprecated
export would not cause breakage downstream (in modules importing ``B4``):
``C`` is still exported by ``B4`` thanks to  the whole-module re-export of ``A``.

Warning at use-sites
++++++++++++++++++++
We emit deprecation warnings for an imported identifier with an export
deprecation in the following situations:

#. In an import declaration which explicitly imports the deprecated identifier,

#. In an occurrence (including a re-export) of an identifier which is in scope
   only through imports which attach deprecations to the identifier.
   In this case, all deprecation messages are emitted as warnings.

To illustrate, suppose we have: ::

  module A where { foo :: Int }
  module B ( {-# DEPRECATED "msg1" #-} foo ) where { import A }
  module C ( {-# DEPRECATED "msg2" #-} foo ) where { import A }

Then we get the following behaviour: ::

  -- Import lists
  module M1 where
    import B ( foo ) -- Warning "msg1": explicit import of a deprecated identifier

  module M2 where
    import B hiding ( foo ) -- no warning: we are explicitly hiding the deprecated "foo"


  -- Occurrences
  module M3 where
    import B  -- no warning here; "foo" is not explicitly mentioned
    bar = foo -- warning "msg1" here

  module M4 where
    import A
    import B
    bar = foo -- no warning: foo is in scope through both A and B,
              -- and A.foo is not deprecated

  module M5 where
    import A
    import B
    bar = B.foo -- warning "msg1": B.foo is in scope through the import of B,
                -- which deprecates "foo"

  module M6 where
    import B
    import C
    bar = foo -- two warnings, "msg1" and "msg2"


  -- Re-exports
  module M7
    ( foo ) -- warning "msg1": B deprecates foo
    where
      import B

  module M8
    ( module B ) -- warning "msg1" for the implicit re-export of foo
    where
      import B

  module M9
    ( module B, module C ) -- emit deprecation warnings with the two messages "msg1" and "msg2"
                           -- (NB: not an error for conflicting messages, as these are an occurrences)
    where
      import B
      import C

  module M10
    ( module A, module B ) -- no warning: foo is deprecated by B but not by A
    where
      import A
      import B


Effect and Interactions
-----------------------
If implemented correctly, this should not cause any side-effects. The only
observable change in behaviour is that GHC will emit additional warnings
arising from deprecations in export lists. All the other behaviour is expected
to remain the same.

Costs and Drawbacks
-------------------
The implementation cost is rather low: changes are mostly confined to:

* processing of export lists (``GHC.Tc.Gen.Export``),
* renaming of imports (``GHC.Rename.Names.filterImports``),
* emission of warnings when looking up identifiers (``GHC.Rename.Env.addUsedGREs``).

Alternatives
------------
As far as I know, there are no real alternatives to this feature.

One option is to mark deprecations in comments, but these would not be raised
by GHC during compilation.

Another option is to manually re-define identifiers and attach deprecation
information to them before re-exporting them, e.g. ::

  module U where { foo :: Int }
  module V ( foo ) where
    import qualified U

    {-# DEPRECATED "msg" #-}
    foo = U.foo

This is very impractical, especially as it can give rise to spurious name clash
errors.

Unresolved questions
--------------------
None at this time.

Implementation Plan
-------------------
This proposal is being implemented by Bartłomiej Cieślar in
https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10283.
