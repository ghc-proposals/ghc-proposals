DuplicateRecordFields without ambiguous field access
====================================================

.. author:: Adam Gundry
.. date-accepted:: 2020-11-16
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/366>`_.
.. contents::

This proposal addresses an unsatisfactory aspect of ``DuplicateRecordFields``, namely the unclear rules around when a field selector or update will be accepted, by entirely removing the type-directed name resolution aspect.  This proposal is
more restrictive than the `previous (dormant) simplification proposal 84 <https://github.com/ghc-proposals/ghc-proposals/pull/84>`_, allowing fewer programs, but correspondingly simpler.


Motivation
----------
The ``DuplicateRecordFields`` extension is useful to allow multiple datatypes to be defined with the same field name in the same module, for example::

  module M where

  data Person = MkPerson { name :: String, pets :: [Pet] }
  data Pet    = MkPet    { name :: Int }

It is now entirely unproblematic to use ``name`` in a record construction or pattern match, because the constructor name disambiguates which field is meant::

  simon = MkPerson { name = "Simon" }

  getPetName (MkPet {name = x }) = x

However a bare use of the selector is more troublesome.  Given the definition ``getName x = name x``, is ``x`` a ``Person`` or a ``Pet``?  Similar issues arise for record updates.

At present, potential ambiguities are sometimes resolved by the type-checker, using bidirectional type-checking.  However, this works only in rather limited circumstances, has a complicated implementation, and has proved confusing for users.  We therefore propose to simplify the extension by removing the possibility for the type-checker to resolve ambiguities.  This would mean that fewer programs are accepted, but would simplify both the specification and the implementation of ``DuplicateRecordFields``.


Proposed Change Specification
-----------------------------
A "field selector occurrence" is an in-scope identifier occurring in an expression, for which *all* the entities to which it could refer are record fields, e.g. ``name`` in ``getName x = name x`` with the definitions above.  A "field update occurrence" is an in-scope identifier occurring on the left-hand side of an equals sign in a record update expression, e.g. ``name`` in ``e { name = z }``.  A field selector or field update occurrence is "ambiguous" if it refers to more than one field in scope (i.e. there are multiple results when the identifier is looked up during name resolution).

When ``DuplicateRecordFields`` is disabled, ambiguous field selector/update occurrences are currently rejected during name resolution.  However, when it is enabled, they are currently disambiguated by the type-checker using the rules described below.  This proposal suggests removing most of these rules, and instead rejecting such definitions during name resolution.

Note that if an identifier refers to both non-fields and fields, it is always rejected by the renamer as excessively ambiguous, regardless of ``DuplicateRecordFields``.

This proposal does not affect field selector/update occurrences that are not ambiguous.


Existing rules for selectors
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The rules for resolving ambiguous field selectors occurrences (i.e. those for which multiple fields with the same label are in scope) are currently as follows:

1. If the selector is applied to an argument, and there is a type signature on the argument which determines a datatype, use that datatype.

2. During bidirectional type-checking, if the type being pushed in is a function whose domain determines a datatype, use that datatype.

(The meaning of "determines a datatype" is not clearly specified at present.  See  `proposal 84 <https://github.com/adamgundry/ghc-proposals/blob/duplicaterecordfields-simplification/proposals/0000-duplicaterecordfields-simplification.rst#when-does-a-type-determine-a-datatype>`_ for an attempt.  Since these rules are being removed, they are not part of the current proposal.)

For example, the following are currently accepted::

  data S = MkS { foo :: Int }
  data T = MkT { foo :: Int, bar :: Int }
  data U = MkU { bar :: Int, baz :: Int }

  d x = foo (x :: T)   -- by rule 1

  e = foo :: T -> Int  -- by rule 2

  f :: T -> Int
  f = foo              -- by rule 2

  g = k foo            -- by rule 2, assuming we already know k :: (T -> _) -> _

The following are currently rejected, and will remain so::

  x = foo

  y = foo (MkT 42)  -- argument does not have a type signature, so rule 1 does not apply

Note that a type signature is absolutely required for rule 1 to apply; no inference is performed, even if it is "obvious" what the type of the argument is.


Proposed new rules for selectors
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Rules 1 and 2 are removed.  There is no rule 3. Ambiguous field selector occurrences are rejected during name resolution.  In particular, examples ``d``, ``e``, ``f`` and ``g`` will now be rejected.


Existing rules for updates
^^^^^^^^^^^^^^^^^^^^^^^^^^
The rules for resolving ambiguous field update occurrences (i.e. those for which multiple fields with the same label are in scope) are currently as follows:

4. If there is only one datatype that has all the fields being updated, use that datatype.

5. If the expression being updated (i.e. the expression before the curly braces) has an explicit type signature determining a datatype, use that datatype.

6. During bidirectional type-checking, if the type being pushed in to the record update determines a datatype, use that datatype.

For example, the following are currently accepted by ``DuplicateRecordFields``::

  data S = MkS { foo :: Int }
  data T = MkT { foo :: Int, bar :: Int }
  data U = MkU { bar :: Int, baz :: Int }

  d x = x { foo = 3, bar = 2 } -- by rule 4, only T has both fields

  e x = (x :: T) { foo = 3 }   -- by rule 5

  f x = x { foo = 3 } :: T     -- by rule 6

  g :: T -> T
  g x = x { foo = 3 }          -- by rule 6

  h = k (x { foo = 3 })        -- by rule 6, assuming we already know k :: T -> _

The following are currently rejected, and will remain so::

  let x :: T
      x = blah
  in x { foo = 3 }

  \x -> [x { foo = 3 },  blah :: T ]

  \ (x :: T) -> x { foo = 3 }


Proposed new rules for updates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Rule 4 remains as above.  Rules 5 and 6 are removed.  Thus ambiguous field update occurrences are rejected during name resolution, except if there is a single datatype to which all the fields belong (which can be determined during name resolution rather than requiring information from type-checking).

In particular, under the revised specification of ``DuplicateRecordFields``, example ``d`` will continue to be accepted, but examples ``e``, ``f``, ``g`` and ``h`` will now be rejected.

Note that ``d`` is rejected when ``DuplicateRecordFields`` is disabled, because each field is required to be unambiguous in isolation, without considering the other fields in the update.


Transition period
^^^^^^^^^^^^^^^^^
Since this proposal will break existing code using ``DuplicateRecordFields``, we propose a transition period involving the following steps:

1. Introduce a new warning ``-Wambiguous-fields``, enabled by default.  This will make the compiler emit a warning for every ambiguous field selector/update occurrence it resolves under the rules described above.  The warning should explain that support for such occurrences will be removed in a future GHC release.

2. In a subsequent GHC release, remove support for ambiguous field selector/update occurrences entirely and remove the warning.  This step should not be taken until ``RecordDotSyntax`` or another generally-accepted mechanism for disambiguation is available, to provide users with a clear alternative.

This transition period will give time for users of ``DuplicateRecordFields`` to adapt their code (using ``RecordDotSyntax`` or otherwise), or raise concerns about the proposed changes and request a stay of execution.  Our expectation is that step 2 will be taken in the GHC release immediately following step 1, but this can be changed if feedback from users indicates that the removal of the feature is causing substantial pain.

The warning produced by ``-Wambiguous-fields`` should mention the specific selector and type that were determined by the disambiguation rules, rather than just complaining about the ambiguity.  This should make it easier for affected users to adapt their code.


Migration strategy
^^^^^^^^^^^^^^^^^^
Code that is broken by this proposal because it relies on ambiguous field occurrences can be fixed in one of the following ways:

1. Where the field is defined in a different module, use qualified imports, import hiding and/or aliases to remove the ambiguity.  For example, here is a `technique using import aliases <https://gist.github.com/chrisdone/d7a8f9e91e2ac111fac6ab72cc480f78>`_::

    {-# LANGUAGE DuplicateRecordFields #-}
    module M1 where
      data Person = MkPerson { name :: String, pets :: [Pet] }
      data Pet    = MkPet    { name :: Int }

    module N where
      import M1 as Person (Person(..))
      import M1 as Pet (Pet(..))

      getPersonName :: Person -> String
      getPersonName = Person.name

      setPersonName :: String -> Person -> Person
      setPersonName n p = p { Person.name = n }

   The new version of the code is completely backwards-compatible, and its meaning is clear. The downsides are that this approach cannot be used within a single module, barring enhancements to the module system, and it requires boilerplate imports.

2. Use ``RecordDotSyntax`` when it is available::

    {-# LANGUAGE DuplicateRecordFields, RecordDotSyntax #-}
    module M2 where
      data Person = MkPerson { name :: String, pets :: [Pet] }
      data Pet    = MkPet    { name :: Int }

      getPersonName :: Person -> String
      getPersonName p = p.name

      setPersonName :: String -> Person -> Person
      setPersonName n p = p { name = n }

   This works within a single module. However it requires a new as-yet-unreleased extension, and will not work for fields with higher-rank or unboxed types.

3. Use explicit pattern-matching and record construction, possibly in combination with ``NamedFieldPuns`` or ``RecordWildCards``::

    {-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}
    module M2 where
      data Person = MkPerson { name :: String, pets :: [Pet] }
      data Pet    = MkPet    { name :: Int }

      getPersonName :: Person -> String
      getPersonName MkPerson{name} = name

      setPersonName :: String -> Person -> Person
      setPersonName n MkPerson{pets} = MkPerson {name = n, pets }

   This works in a single module and does not require any new extensions, but it may require additional boilerplate, especially if a type has many constructors and/or fields.



Effect and Interactions
-----------------------
The new rules simplify the design and implementation of ``DuplicateRecordFields``, because the type-checker will no longer be involved in name resolution.  Name information (including knowledge of which fields belong to which datatypes) will be sufficient to determine which field is referred to by every occurrence of a record selection or update.

Under this proposal enabling ``DuplicateRecordFields`` for a module remains conservative, because any program that was accepted by the compiler without using the special selector disambiguation rules will still be accepted.  However, existing programs already using ``DuplicateRecordFields`` may cease to be accepted.

The ``RecordDotSyntax`` extension (`proposal 282 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0282-record-dot-syntax.rst>`_), and the ``HasField`` magic type class (`proposal 23 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0023-overloaded-record-fields.rst>`_), provide alternative mechanisms for field selection and update.  These do not apply in some rare circumstances (in particular, where fields have higher-rank or unboxed types), but in those cases users can use import hiding to limit the fields in scope and hence remove the ambiguity, or can write pattern-matching definitions instead of using record selectors.

The ``NoFieldSelectors`` extension (`proposal 160 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0160-no-toplevel-field-selectors.rst>`_) changes datatypes so that they do not bring field selectors into scope at all.  The current proposal complements ``NoFieldSelectors``, as it will make use of selectors under ``DuplicateRecordFields`` slightly less convenient.  However, ``NoFieldSelectors`` affects definition sites, while the current proposal affects use sites, so until ``NoFieldSelectors`` is universally adopted, the current proposal is relevant for addressing the question of how ambiguous field selector occurrences should be resolved.

The ``PatternSynonyms`` extension interacts awkwardly with the disambiguation rules in ``DuplicateRecordFields``, because record pattern synonyms may introduce new fields that work with existing types, so they do not work with type-directed name resolution.  This proposal will make a proper integration of ``PatternSynonyms`` and ``DuplicateRecordFields`` easier, because this problem will be removed.


Costs and Drawbacks
-------------------
This change may be disappointing for users who would prefer more use of type information to resolve ambiguous names.  Some users have already expressed this desire (e.g. see `issue #11343 <https://gitlab.haskell.org/ghc/ghc/-/issues/11343>`_).

The change is backwards-incompatible for code that makes use of the ``DuplicateRecordFields`` extension. Accordingly we propose a transition period with a compatibility warning. Even so, removing the feature may `cause user dissatisfaction <https://github.com/ghc-proposals/ghc-proposals/pull/366#issuecomment-702996205>`_.

The development cost of this change is relatively low (the new warning should be easy to implement, and the new specification mostly involves removing code).  It should reduce maintenance costs of GHC overall.  Moreover, since the specification of ``DuplicateRecordFields`` will be simpler, its behaviour will become easier to understand.


Alternatives
------------
Keeping the status quo is entirely feasible, even though the current design is not completely satisfactory.  This would allow us to wait until ``NoFieldSelectors`` and ``RecordDotSyntax`` have been tested in practice, before starting changes to ``DuplicateRecordFields``.

We could take the opposite approach, and increase the use of type inference to resolve ambiguous field occurrences, as requested by some users.  However, it is not clear how to do this in anything other than an essentially ad hoc manner, so the extension is likely to become even more complex to specify and implement.

We could extend or shorten the transition period. The current proposal strikes a balance between the desire to not break users' code without warning, and the desire to simplify the implementation.


Unresolved Questions
--------------------
None.


Implementation Plan
-------------------
If accepted, Adam Gundry will implement.  A `draft implementation of the warning <https://gitlab.haskell.org/ghc/ghc/-/commit/abf0fb3138bd05a94fe69fb887cf308e51d3a7d4>`_ exists already.  The implementation of the warning does not depend on the implementation of any other proposals, although the proposed transition period depends on the implementation of ``RecordDotSyntax``.
