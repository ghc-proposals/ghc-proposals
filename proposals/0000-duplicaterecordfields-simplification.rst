.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/84>`_.

.. contents::


DuplicateRecordFields Simplification
====================================

This proposal suggests a simplification of the ``DuplicateRecordFields`` extension introduced in GHC 8.0.  The proposed change would require more type signatures when using the extension, and limit its use with data families, but it would become easier to describe and implement.


Motivation
------------

The ``DuplicateRecordFields`` extension permits a single module to define multiple record fields with the same name, and when there are multiple fields in scope with the same name, permits these to be used in contexts where it is unambiguous which selector is meant.  For example, a module is permitted to define the following datatypes::

  data S = MkS { foo :: Int }
  data T = MkT { foo :: Int, bar :: Int }
  data U = MkU { bar :: Int, baz :: Int }

If ``foo`` is used in record construction or pattern-matching, the constructor name means it is unambiguous which field is meant.  However, record selection or update may be ambiguous.  For example, given a simple top-level definition ``f = foo`` it is not clear whether this should have type ``S -> Int`` or ``T -> Int``, so the definition is rejected.

At present, potential ambiguities are sometimes resolved by the type-checker, using bidirectional type-checking.  However, this works only in rather limited circumstances, has a complicated implementation, and has proved confusing for users.  It is therefore proposed to simplify the extension by removing the possibility for the type-checker to resolve ambiguities.  Instead, the datatype in question would be determined during name resolution.  This would mean that type annotations are required in more circumstances, but would significantly simplify both the specification and the implementation of ``DuplicateRecordFields``.


Proposed Change Specification
-----------------------------

The proposed changes are in three parts:

* Rules for resolving ambiguous record selectors
* Rules for resolving ambiguous record updates
* Requirements for type signatures to mention a datatype directly



Existing rules for selectors
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The rules for resolving ambiguous record selectors (i.e. those for which multiple fields with the same label are in scope) are currently as follows:

1. If the selector is applied to an argument, and there is a type signature on the argument determining a datatype, use that datatype.

2. During bidirectional type-checking, if the type being pushed in is a function whose domain determines a datatype, use that datatype.

For example, the following are currently accepted::

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

Rule 1 remains as above.  Rule 2 is replaced with the following:

3. If the selector is immediately enclosed by a function type signature whose domain determines a datatype, use that datatype.

Examples ``d`` and ``e`` above will continue to be accepted (by rules 1 and 3 respectively).  Examples ``f`` and ``g`` will now be rejected, even though rule 2 previously accepted them.


Existing rules for updates
^^^^^^^^^^^^^^^^^^^^^^^^^^

The rules for resolving ambiguous record updates (i.e. those for which multiple fields with the same label are in scope, for all the fields mentioned in the update) are currently as follows:

4. If there is only one datatype that has all the fields being updated, use that datatype.

5. If the expression being updated (i.e. the expression before the curly braces) has an explicit type signature determining a datatype, use that datatype.

6. During bidirectional type-checking, if the type being pushed in to the record update determines a datatype, use that datatype.

For example, the following are currently accepted::

  d x = x { foo = 3, bar = 2 } -- by rule 4

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

Rules 4 and 5 remain as above.  Rule 6 is replaced with the following:

7. If the entire update expression (i.e. outside the curly braces) has an explicit type signature determining a datatype, use that datatype.

Examples ``d``, ``e`` and ``f`` above will continue to be accepted (by rules 4, 5 and 7 respectively).  Examples ``g`` and ``h`` will now be rejected, even though rule 6 previously accepted them.



When does a type determine a datatype?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The rules above talk about types "determining a datatype".  It is currently slightly under-specified what this means.  In the current implementation:

* a datatype application determines the corresponding datatype;

* a data family application determines the particular data family instance, if there is one;

* if a type synonym expands to a type that determines a datatype, the type synonym determines that datatype.

In particular, note that type family applications are not reduced in order to determine datatypes.  For example::

  data U a = MkU { foo :: Int }

  data family V a
  data instance V Bool = MkVBool { foo :: Int }

  type W = U Bool

  type family X a
  type instance X Bool = U Bool

  foo :: U a -> Int    -- unambiguous (datatype)
  foo :: V a -> Int    -- ambiguous   (data family without a matching instance)
  foo :: V Bool -> Int -- unambiguous (data family with a matching instance)
  foo :: W -> Int      -- unambiguous (type synonym for a datatype)
  foo :: X Bool -> Int -- ambiguous   (type family not reduced)

The proposed change is that only a datatype application will determine the corresponding datatype.  That is, a type signature must give a datatype explicitly, not a data family, type synonym or type family.  In the above examples, ``foo :: U a -> Int`` will be accepted and all the rest will be rejected.  This is necessary so that the field can be disambiguated by the renamer, without access to information from type-checking (such as the definitions of type synonyms).

For name resolution purposes, fields of data families belong to the entire family, rather than an individual instance, which is why ``foo :: V Bool -> Int`` will be rejected under this proposal.  This is slightly unfortunate as, if a module defines multiple data family instances with the same field labels, there may be no way to write the selectors unambiguously.  However, it is a relatively obscure corner.


Effect and Interactions
-----------------------

The new rules simplify the design and implementation of ``DuplicateRecordFields``, because the type-checker will no longer be involved in name resolution.  Name information (including knowledge of which fields belong to which datatypes) will be sufficient to determine which field is referred to by every occurrence of a record selection or update.

In many cases, the ``HasField`` magic type class (see `the previous GHC proposal <https://github.com/ghc-proposals/ghc-proposals/pull/6>`_) makes it possible to write ``getField @"foo"`` in order to make use of a field ``foo`` determined by the type-checker.  This includes fields belonging to data families, but not fields with higher-rank types.


Costs and Drawbacks
-------------------

This change may be disappointing for users who would prefer more use of type information to resolve ambiguous names.  Some users have already expressed this desire (e.g. see `Trac #11343 <https://ghc.haskell.org/trac/ghc/ticket/11343>`_).

The change is backwards-incompatible, and may break code that makes use of the ``DuplicateRecordFields`` extension in GHC 8.0 and 8.2.  Such breakage can almost always be fixed by adding a type signature, except in corner cases involving data families.  The error message can be modified to explain the places in which a type signature can be added.  We believe that this extension has seen relatively limited use, and the cases that were accepted by the old rule but will not be accepted by the new rule are relatively uncommon.

The development cost of this change is relatively low, though not completely trivial, because some operations currently performed by the type-checker will have to be performed by the renamer instead.  This change should reduce maintenance costs of GHC overall.


Alternatives
------------

Keeping the status quo is entirely feasible, even though the current design is not completely satisfactory.

We could take the opposite approach, and increase the use of type inference to resolve ambiguous selector occurrences, as requested by some users.  However, it is not clear how to do this in anything other than an essentially ad hoc manner, so the extension is likely to become even more complex to specify and implement.


Unresolved questions
--------------------

The interaction between ``DuplicateRecordFields`` and ``PatternSynonyms`` has never been completely satisfactory (see `Trac #11228 <https://ghc.haskell.org/trac/ghc/ticket/11228>`_).  I hope that narrowing the scope of ``DuplicateRecordFields`` as proposed here may help with this, but I haven't yet thought this through.  Feedback on this point would be very welcome.


Implementation Plan
-------------------

If accepted, I will try to implement.
