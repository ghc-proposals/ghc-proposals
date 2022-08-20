Disambiguating record updates using type signatures
===================================================

.. author:: Adam Gundry
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/537>`_.
.. sectnum::
.. contents::

This proposal makes a small adjustment to name resolution for record updates
when ``DisambiguateRecordFields`` is enabled: where multiple types have
identically-named fields, expression type signatures can be used to disambiguate
which type is meant.


Motivation
----------

The ``DuplicateRecordFields`` extension makes it possible to define two records
with the same field name in a single module.  This raises the question of how to
interpret a record update such as ``r { f = v }`` if there are two fields ``f``
in scope.  The original design of ``DuplicateRecordFields`` used a limited form
of type-directed name resolution in this case, however support for this is
scheduled to be removed following `proposal #366
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0366-no-ambiguous-field-access.rst>`_.
GHC 9.2 and 9.4 include a warning when such updates are used.  For example, this module: ::

    {-# LANGUAGE DuplicateRecordFields #-}

    data S = MkS { f :: Char }
    data T = MkT { f :: Int }

    foo :: S -> S
    foo x = x { f = 'c' }

results in the following warning on GHC 9.2: ::

    M.hs:7:13: warning: [-Wambiguous-fields]
        The record update x {f = 'c'} with type S is ambiguous.
        This will not be supported by -XDuplicateRecordFields in future releases of GHC.
      |
    9 | foo x = x { f = 'c' }
      |             ^^^^^^^

The GHC developers would like to remove support for this (mis)feature and turn
the warning into a fatal error.  However, the question arises as to how such
updates should be written instead.

One possible answer is provided by the ``OverloadedRecordUpdate`` extension
(introduced in `proposal #282
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0282-record-dot-syntax.rst>`_
as amended by `proposal #405
<https://github.com/ghc-proposals/ghc-proposals/pull/405>`_).  This changes the
meaning of record update syntax to use typeclass polymorphism, based on the
``HasField`` typeclass.  Thus under ``OverloadedRecordUpdate`` the program above
would be accepted without warnings, and in fact the type of the update becomes
more general, because we have: ::

    foo :: HasField "f" s Char => s -> s
    foo x = x { f = 'c' }

However, ``OverloadedRecordUpdate`` is not yet fully implemented (pending a
redesign that is in progress but is not yet complete).  Moreover, there are
cases that cannot be handled by ``OverloadedRecordUpdate``, including:

* Type-changing update

* Updates to unlifted fields or data types

* Multiple updates, where several fields must be changed simultaneously for the
  update to be type-correct

* Updates to fields with higher-rank types

Some of these may be addressed by subsequent design changes to the ``HasField``
class, but others seem fundamentally difficult (in particular, multiple updates
and higher-rank types).  Thus there are cases where enabling
``OverloadedRecordUpdate`` is not a conservative extension, i.e. it will break
existing code.  (Changes to ``OverloadedRecordUpdate`` to improve this situation
are a topic for future proposals.)

All this suggests that we should have a mechanism for record updates where the
programmer explicitly specifies which type is being updated.  This gives a
migration path for existing code that relies on updates which were previously
accepted by ``DuplicateRecordFields`` but will be rejected following complete
implementation of `proposal #366
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0366-no-ambiguous-field-access.rst>`_.

There are various possible ways we could permit writing such updates:

* with an expression type signature on the record being updated, e.g. ``(r :: T
  a b) { f = v }`` (as in this proposal);

* with new syntax, for example ``r { T | f = v }``, ``r T.{ f = v }`` or ``T { f = v .. r }``,
  where ``T`` is a type constructor name without parameters (as in `proposal
  #310 <https://github.com/Ericson2314/ghc-proposals/blob/disambiguate-record-update/proposals/0000-disambiguate-record-update.rst>`_);

* by allowing a data type name to be used as if it were a module qualifier,
  e.g. ``r { T.f = v }`` (a small part of Local Modules `proposal #283
  <https://github.com/goldfirere/ghc-proposals/blob/local-modules/proposals/0000-local-modules.rst>`_).

This proposal currently assumes the first of these options, but either of the
alternatives are plausible, and are discussed further below.


Proposed Change Specification
-----------------------------

If:

1. ``DisambiguateRecordFields`` is enabled,

2. a record update has an expression type signature for the record being updated, and

3. the type is syntactically an application of a data type or newtype constructor ``T``;

then name resolution for the fields being updated will consider only:

a. fields of the type ``T`` (if it is a record type),
b. record pattern synonym fields that are unassociated, and
c. record pattern synonym fields that are associated with the type ``T``.

That is, when resolving the field names ``f1, ..., fN`` in the record update
``(r :: T a1 ... aM) { f1 = v1, ..., fN = vN }``, if ``T`` is a data type
constructor then fields associated with type constructors other than ``T`` will
be ignored.

If a type signature is given but the type is not syntactically a data type
(e.g. it is a type variable or type synonym), the normal name resolution rules
apply.



Examples
--------

Consider an expansion of the example from the motivation: ::

    {-# LANGUAGE DuplicateRecordFields #-}

    data S = MkS { f :: Char }
    data T = MkT { f :: Int }

    foo :: S -> S
    foo x = x { f = 'c' }          -- rejected: f is ambiguous

    foo2 x = (x :: S) { f = 'c' }  -- accepted: f refers to the field of type S

    foo3 x = x { f = 'c' } :: S    -- rejected: outer type signature ignored


    type T' = T

    foo4 x = (x :: T') { f = 'c' } -- rejected: type synonym not expanded


Pattern synonym fields
~~~~~~~~~~~~~~~~~~~~~~

Where there are unassociated pattern synonyms fields, even the presence of a type
signature may not be enough to disambiguate: ::

    {-# LANGUAGE DuplicateRecordFields, PatternSynonyms #-}

    data T = MkT { k :: Int }

    pattern MkP{k} = k
    pattern MkQ{k} = (k, ())

    foo x = (x :: T) { k = 3 }  -- rejected

In this example, despite the presence of the type signature, it is still
ambiguous whether ``k`` refers to the field of ``MkT`` or of ``MkP``.  It cannot
be ``MkQ`` but that is evident only after type-checking.  Accordingly, this
example will be rejected.

(This program currently triggers a bug in GHC, see `GHC issue #21898
<https://gitlab.haskell.org/ghc/ghc/-/issues/21898>`_.)


Associated pattern synonyms
~~~~~~~~~~~~~~~~~~~~~~~~~~~

When a pattern synonym field is exported, it may be "associated" with a data
type constructor, like this: ::

    {-# LANGUAGE DuplicateRecordFields, PatternSynonyms #-}
    module M (T(MkP, g)) where
      data T = MkT { f :: Int, x :: Bool }

      pattern MkP{g} = MkT g True

This means that importing ``T(..)`` will bring ``MkP`` and ``g`` into scope.
This proposal takes advantage of these associations to help with disambiguation
of pattern synonyms: ::

    {-# LANGUAGE DisambiguateRecordFields #-}
    module N where
       import M (T(..))

       data S = MkS { g :: Int }

       foo x = x { g = 0 }  -- rejected: g could refer to the field of S or MkP

       bar x = (x :: T) { g = 0 } -- accepted: must refer to MkP because S is ignored


Data families
~~~~~~~~~~~~~

An awkward corner remains with this proposal, namely data families where the
same field name is defined multiple times by different data instances, for
example: ::

    {-# LANGUAGE DuplicateRecordFields, TypeFamilies #-}

    data family T a
    data instance T Int  = MkTInt  { f :: Int  }
    data instance T Bool = MkTBool { f :: Bool }

    foo x = (x :: T Int) { f = 3 }  -- rejected

Notice that the type constructor ``T`` and field name ``f`` are not sufficient
to uniquely identify a field.  Even the record update ``(r :: T Int) { f = 3 }``
would be rejected under this proposal, because name resolution alone is not able
to distinguish ``T Int`` from ``T Bool``.

The heart of the issue is that data families allow the definition of multiple
*different* fields with the same label and belonging to the same type
constructor for name resolution purposes.  Internally, GHC generates separate
"representation types" for ``T Int`` and ``T Bool`` but these do not have names
available to user code.


Updates where only one constructor has all fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Where there is only one data constructor that has all the fields being updated,
GHC is able to use this fact to identify the type being updated (see `GHC issue
#21443 <https://gitlab.haskell.org/ghc/ghc/-/issues/21443>`_).  Following this
proposal, a type signature may additionally rule out certain types.  For
example, the following should be accepted: ::

    {-# LANGUAGE DuplicateRecordFields, TypeFamilies, PartialTypeSignatures #-}

    data S = MkS { f :: Int, g :: Int }

    data family T a
    data instance T Int  = MkTInt  { f :: Int, g :: Int  }
    data instance T Bool = MkTBool { f :: Bool }

    foo x = (x :: T _) { f = 3, g = 3 } -- accepted

Notice that ``S`` can be ruled out by the type signature, while ``T Bool`` can
be ruled out because it does not have all the fields being updated, and hence
the update must refer to ``T Int``.


Effect and Interactions
-----------------------

With this change, it is possible to write a type signature on a record
expression being updated, and have the type constructor from that signature used
for name resolution purposes.


Partial type signatures
~~~~~~~~~~~~~~~~~~~~~~~

If a type being updated has many parameters, or they have complicated values, it
might be annoying to be required to specify them, for (a contrived) example: ::

    data T a b c d e = MkT { f :: a, ... }

    wurble r = (r :: T (Int, Int, Int) Char Monad (,) [[[[[()]]]]]) { f = (1, 2, 3) }

This can be made somewhat more compact using ``PartialTypeSignatures``: ::

    wurble r = (r :: T _ _ _ _ _) { f = (1, 2, 3) }

At present, this will require the user to specify
``-Wno-partial-type-signatures`` to avoid many needless warnings, but
this could be addressed using the approach in
`proposal #491 <https://github.com/treeowl/ghc-proposals/blob/different-holes/proposals/0000-distinguish-partial-sigs.md>`_.


``DisambiguateRecordFields`` vs ``DuplicateRecordFields``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The new behaviour will apply when the ``DisambiguateRecordFields`` extension is
enabled.  ``DisambiguateRecordFields`` seems the natural home for this
behaviour, as it controls whether GHC uses contextual information about fields
during name resolution at use sites.  In particular, it has the effect of taking
the data constructor name into account when resolving field names in record
construction or pattern-matching, as well as ignoring non-fields when resolving
a field name in an update (see also `GHC issue #22160
<https://gitlab.haskell.org/ghc/ghc/-/issues/22160>`_).

The main purpose of ``DuplicateRecordFields`` itself is to enable the definition
(or re-export) of multiple types containing identically-named fields in a single
module.  ``DuplicateRecordFields`` implies ``DisambiguateRecordFields``, so
users enabling the former will automatically gain the latter behaviour.


``OverloadedRecordUpdate``
~~~~~~~~~~~~~~~~~~~~~~~~~~

This proposal changes the specification of name resolution for traditional
(non-overloaded) record updates.  When ``OverloadedRecordUpdate`` is in use, it
does not resolve the field names in the same way, so it is not affected by this
proposal.

If a subsequent change to ``OverloadedRecordUpdate`` provides a way to write
both non-overloaded and overloaded updates in a single file, then the
non-overloaded updates will benefit from the changes described here.


Costs and Drawbacks
-------------------

Since name resolution does not have access to type information, type signatures
must mention the record type constructor explicitly rather than using a type
synonym.  This means type synonyms are not entirely transparent.  Moreover,
there are subtle corner cases involving pattern synoynms and data families, as
demonstrated by the examples above.

The implementation cost for this change should be modest, and it unblocks
further simplifications to the implementation following `proposal #366
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0366-no-ambiguous-field-access.rst>`_.


Alternatives
------------

New syntax
~~~~~~~~~~

An obvious alternative to this proposal would be to add brand new syntax for
non-overloaded record updates, e.g. ``r { T | f = v }`` or ``r T.{ f = v }``,
where ``T`` is a (perhaps module-qualified) type constructor name.  This would
have the advantage that users would not need to write out the type parameters
(or ``_`` wildcards).

`Proposal #310 <https://github.com/Ericson2314/ghc-proposals/blob/disambiguate-record-update/proposals/0000-disambiguate-record-update.rst>`_
suggests introducing the syntax ``T { f = v .. r }`` for a non-overloaded record
update.  It essentially solves the same problem as this proposal, and is a
viable alternative to the approach described here.

However, introducing new syntax for a relatively obscure case seems
under-motivated (at least in the view of this proposal's author).  It would
introduce something new for tools to support and users to learn, whereas users
with a knowledge of standard Haskell will already be able to understand
expression type sigantures and record updates.

One might also imagine making bigger changes to record syntax, and thereby
making it more worth changing syntax at all (cf. `issue #328
<https://github.com/ghc-proposals/ghc-proposals/issues/328>`_).



Data type names as qualifiers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Another possible approach to this problem would be to allow data type names to
be used as if they were module qualifiers on field names.  This is a (small)
part of the `Local Modules proposal #283
<https://github.com/goldfirere/ghc-proposals/blob/local-modules/proposals/0000-local-modules.rst>`_.
For example: ::

    data S = MkS { f :: Int }
    data T = MkT { f :: Int }

    foo = S.f

    bar r = r { T.f = 3 }

This would make it easier to explicitly disambiguate both selectors and record
updates in the presence of otherwise-ambiguous fields.

The main downside of this approach, compared to the present proposal, is that it
is more complex to specify and implement.  Hierarchical module names in existing
Haskell treat the ``.`` separator as essentially just part of the name, so
``Data.List`` is a single unit rather than a module ``Data`` from which a module
``List`` is projected.  This would need to change under the Local Modules
proposal.  Even for the fragment of it considered here, the problem arises that
it is not clear (until after name resolution) whether a qualified name ``M.T.f``
refers to a module named ``M.T`` or a module named ``M`` containing a data type
``T``.

This approach would still struggle to disambiguate data family instances
defining the same field name multiple times in a single top-level module.
Resolving such cases appears to need the full power of local modules, so that
each data family could be defined in a separate local module.


Data constructor names rather than type constructors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If adding new syntax, we could imagine having the syntax mention a data
constructor, rather than a type constructor.  This would have the merit that
cases involving data families or unassociated pattern synonyms could be
disambiguated.  Apart from these rather obscure corner cases, however, it seems
strange.  In such cases, users can always fall back on writing an explicit
``case`` expression.


Retaining type-directed name resolution
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given that the difficulty arises from removing type-directed name resolution for
record updates following `proposal #366
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0366-no-ambiguous-field-access.rst>`_,
despite this feature being supported and working, why not simply abandon the
plan to remove it?  Apart from the complexity it introduces in the
implementation, however, there are more basic issues with the status quo.

Since "ambiguous" updates are not resolved until the type-checker, but Template
Haskell quotes run only the renamer, using an ambiguous update in a TH quote
must either fail utterly or defer name resolution to the splice site, which is
almost certainly not desirable.  More concretely, the following program is
currently rejected with an error "Ambiguous record updates not (yet) handled by
Template Haskell", but would be accepted under this proposal: ::

    {-# LANGUAGE DuplicateRecordFields, TemplateHaskellQuotes #-}
    module M( foo ) where
      data S = MkS { f :: Int }
      data T = MkT { f :: Int }
      foo x = [| ($x :: S) { f = 3 } |]

Moreover, the fact that name resolution alone cannot determine which field names
are being used causes problems with dependency analysis, and the existing
implementation does not properly support pattern synonyms (see `GHC issue #21898
<https://gitlab.haskell.org/ghc/ghc/-/issues/21898#note_446043>`_).


Removing type-directed name resolution without replacement
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is not completely clear that any of the proposed features are necessary; we
could simply remove all options for disambiguating updates, and require users to
disambiguate using the module system or write an explicit case expression.

Where the record type being updated is being imported from another module,
qualified imports can be used to disambiguate both selectors and updates without
much trouble: ::

    {-# LANGUAGE DuplicateRecordFields #-}
    module M where
      data S = MkS { f :: Int }
      data T = MkT { f :: Int }

    {-# LANGUAGE DisambiguateRecordFields #-}
    module N where
      import qualified M (S(..)) as S
      import qualified M (T(..)) as T

      foo = S.f
      bar r = r { T.f = 3 }

However, this can introduce quite a lot of boilerplate (especially if many
records need to be imported into many modules) and is not possible if the record
update is in the same module as the data type definition.

In principle, the latter point could be addressed by allowing modules to
"import" themselves (qualified, and with a restricted import list), like this: ::

    {-# LANGUAGE DuplicateRecordFields #-}
    module M where
      import qualified M (S(..)) as S
      import qualified M (T(..)) as T

      data S = MkS { f :: Int }
      data T = MkT { f :: Int }

      foo = S.f
      bar r = r { T.f = 3 }

Unfortunately this is not currently supported, although it *is* possible for use
sites to have a module qualifier for the current module.

Explicit case expressions are another option for disambiguating an update,
provided the data constructors of the type are in scope. This requires rewriting
the code significantly, however. ``RecordWildCards`` can be used to avoid
writing out the non-updated field names, but if the data type being updated is a
wide sum, the case expression must still mention all constructor names: ::

    data S = MkS  { f :: Int }
    data T = MkT1 { f :: Int, g :: Int, h :: Int }
           | MkT2 { f :: Int, g :: Int }
           | MkT3 { f :: Int, h :: Int }

    -- instead of foo r = r { f = 3 }
    foo r = case r of
              MkT1{..} -> MkT1{f = 3, .. }
              MkT2{..} -> MkT2{f = 3, .. }
              MkT3{..} -> MkT3{f = 3, .. }


Selectors
~~~~~~~~~

This proposal addresses the problem of disambiguating record updates, but not
selector functions, though some of the alternatives (e.g. data type names as
qualifiers) can be used for selectors as well.  We could imagine adding a
similar special name resolution rule for selector applications, e.g. those of
the form ``f (r :: T)``.

In practice, however, the ``OverloadedRecordDot`` extension can be used for
selectors in most cases (except for those involving higher-rank fields), without
encountering as many issues as for ``OverloadedRecordUpdate``.  Thus special
treatment of selectors seems less strongly motivated, and does not form part of
the present proposal.


Outer expression type signatures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Should the special treatment apply to expression type signatures on the record
being updated, the result of the update, or both?  The proposal as currently
drafted applies to ``(r :: T) { f = v }`` but not ``r { f = v } :: T``.  It
would be simple to support the latter as well, but then one could go even
further and consider cases such as ``(\ r -> r { f = v }) :: T -> T`` or other
expressions where type information could be propagated from further afield.
Ideally name resolution should be as simple as possible, however, making minimal
use of type information.  Thus, this proposal keeps the scope of the feature as
tightly constrained as possible while solving the problem in the Motivation.


Unresolved Questions
--------------------

None.


Implementation Plan
-------------------

The proposal author, Adam Gundry, is willing to implement the change.  This
should unblock the full implementation of `proposal #366
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0366-no-ambiguous-field-access.rst>`_
and allow the compiler implementation to be simplified, because it allows the
existing support for type-based update disambiguation to be removed, replacing
it with a feature that is strictly about name resolution.
