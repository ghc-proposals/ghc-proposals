Grand Unifying Namespaces Proposal
==================================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/214>`_.
.. sectnum::
.. contents::

GHC currently maintains several different namespaces. This proposal describes an easy
mechanism for giving the user control over which namespace an identifier is meant to
come from.

In brief: use ``type.`` to refer to the type-level namespace, ``data.`` to the term-level
namespace, and ``module.`` to refer to the module namespace. (Spoiler: this last idea
doesn't yet have a practical application, but perhaps it will someday.)

In the near future, ideas here can be used to clean up the current state
of ``-XExplicitNamespaces`` (where we use ``pattern`` to refer to a term-level definition)
and the proposed (accepted) changes to fixity declarations and ``WARNING`` / ``DEPRECATED``
pragmas (where we use ``value`` for the same purpose). (See the `proposal`_ for the details.)

.. _proposal: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0008-type-infix.rst

In the longer term, I hope that we will be able to freely refer to types in terms and
terms in types, meaning this namespace issue will worsen. The technique described here
scales to that use-case.

Motivation
----------
As the introduction states, we are currently in a sorry state of affairs.

* The pseudo-keyword ``pattern`` is used to select the term-level namespace in import/export
  lists, as documented in the `manual https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#explicit-namespaces-in-import-export>`_. This works both for pattern
  synonyms (its original application) and for data constructors (but not for term-level functions).

* Furthermore, the new pseudo-keyword ``value`` is used for the same purpose but in different
  contexts in a recently accepted `proposal`_.

.. _`visible dependent quantification`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0035-forall-arrow.rst
  
* The recently-implemented `visible dependent quantification`_ proposal allows types
  to take required, dependent variables::

    data ProxyVis :: forall k -> k -> Type where ...

  To use ``ProxyVis``, we must apply ``ProxyVis`` to a kind and then a type, thus:
  ``ProxyVis Bool True``.

  Today's theory of Haskell (that is, *without* dependent types) is strong enough to
  allow this syntax in terms, except that the namespaces are all wrong. For example,
  we might imagine ::

    data Proxy :: forall k. k -> Type where
      MkProxy :: forall a -> Proxy a

  Note the type of ``MkProxy`` takes a visible, dependent argument. With this, we
  could write ``MkProxy True`` or ``MkProxy Int`` instead of today's ``MkProxy :: Proxy True``
  and ``MkProxy :: Proxy Int`` or ``MkProxy @True`` and ``MkProxy @Int``. After all,
  what's the point of a proxy if we don't specify the choice of type?
  
* Looking further into the future, we will have a problem when Haskellers can use term-level
  definitions in types, and vice-versa. Indeed, this problem already exists: with data
  constructors. Consider ::

    data Nat = Zero | Succ Nat
    data Vec :: Type -> Nat -> Type where ...

  If we write ``Vec Char (Succ Zero)``, the ``Succ`` and ``Zero`` are really data constructors.
  As such, they are plucked from the data-level namespace, despite their occurrence in a type.
  If there are types named ``Succ`` and ``Zero``, we can select the data-level namespace by
  writing a ``'`` mark, thus: ``Vec Char ('Succ 'Zero)``. This is effective but potentially
  confusing. (It also suffers lexical complications. If I have a data constructor ``X'`` (that's
  a prime) and wish to use it in a type, then I have to write ``' X'``; note the space. Without
  that space, GHC lexes ``'X'`` as a character literal. I think this behavior should be
  continued, however, because it leaves open the possibility of characters in types.) The
  idea here--disambiguation--is the same as in other scenarios, yet the mechanism is different.

Background
----------
This section lays out the current arrangement of namespaces in GHC and then reviews areas
where namespace confusion can currently occur. This section may safely be skipped.

**Namespaces:**

Each namespace listing below raises the possibility of both alphabetic names (names that
start with a letter or underscore and then contain letters, underscores, numbers, and ``'``
marks) and symbolic names (names comprising only punctuation).

1. Data-level variables: These occur in terms.

   * Alphabetic: Must start with a lower-case letter.

   * Symbolic: Must start with something other than a ``:``.

2. Data constructors: These occur in terms and include pattern synonyms.

   * Alphabetic: Must start with an upper-case letter.

   * Symbolic: Must start with a ``:``.

3. Type-level variables: These occur in types.

   * Alphabetic: Must start with a lower-case letter.

   * Symbolic: Not possible.

4. Types and type constructors: These occur in types.

   * Alphabetic: Must start with an upper-case letter.

   * Symbolic: Any non-reserved string of punctuation.

5. Modules: These occur after the ``module`` keyword, in import lists, and before ``.`` in
   types and terms (as long as there is no space surrounding the ``.``).

   * Alphabetic: Must start with an upper-case letter.

   * Symbolic: Not possible.

**Contexts:**

Each context below describes how it looks up names in the various namespaces. Module
prefixes can occur in many contexts and will not be listed as exceptions below.

1. Terms:

   * Primary namespace: All identifiers and symbols are taken from the
     data-level variable namespace or the data constructor namespace, as
     appropriate.

   * Secondary namespace: None.
   
   * Disambiguation: There is no possibility of specifying a different namespace.

   * Example: ``f x y = case x of Nothing -> y; Just a -> a``. Types *can* appear
     in terms, but only after ``::`` or ``@``. Those two constructs introduce
     type contexts.

2. Types:

   * Primary namespace: All identifiers and symbols are taken from the
     type-level variable namespace or the type constant namespace, as
     appropriate.

   * Secondary namespace: For capitalized alphabetic identifiers or symbols
     that begin with a ``:``, if the lookup in the type-level namespace fails,
     GHC looks in the data constructor namespace.

   * Disambiguation: Users may prefix these names with a ``'`` to request a
     lookup in the data constructor namespace only.

   * Example: ``Vec Int ('Succ 'Zero)``

3. Import/export lists:

   * Primary namespace:

     * Uppercase alphabetic identifiers and ``:``\-symbols: Type constants namespace.

     * Lowercase alphabetic identifiers, including other symbols: Data-level variables namespace.

   * Secondary namespace: None. If the primary lookup fails, the program is rejected.

   * Disambiguation: Users may write ``type`` to choose the type constants
     namespace and ``pattern`` to choose the data constructor namespace. The use of
     ``type`` requires ``-XExplicitNamespaces`` and the use of ``pattern`` requires
     ``-XPatternSynonyms``.

   * Example::

       import My.Library ( Class(TypeFamily, DataFamily, method)
                         , DataType(DataConstructor, fieldLabel)
                         , DataFamily(InstanceDataConstructor, instanceFieldLabel)
                         , TypeSynonym
                         , pattern PatternSynonym
                         , (+++)       -- data-level operator
                         , type (+++)  -- type operator
                         , ordinaryFunction
                         )

     Note that the data constructor namespace becomes the primary namespace in the ``(...)``
     after a datatype, but not after a class. (The sub-import list after a class name has
     the same namespace behavior as a top-level import list.)
      

4. Fixities, ``WARNING``, and ``DEPRECATED`` (currently implemented):

   * Primary namespace: Both type constants and data constructors/variables are considered
     primary.

   * Secondary namespace: None.

   * Disambiguation: Not possible. If a name exists in both primary namespaces, the directive
     applies to the names in both namespaces, even if these names are unrelated.

   * Example: ``infixl +++ 5`` affects both the type-level and data-level ``+++`` operators,
     if both are in scope and defined locally.

5. Fixities, ``WARNING``, and ``DEPRECATED`` (as in `proposal`_):

   * Primary namespace: Data-level.

   * Secondary namespace: Type-level. Only when a name does not exist in a data-level
     namespace will a type-level namespace be consulted.

   * Disambiguation: Users can write ``type`` to choose the type-level namespace and
     ``value`` to choose the data-level namespace.

   * Example: Assume ``+++`` is in scope in both types and terms; both names have been
     defined locally. Then: ``infixl +++ 5`` affects only the data-level ``+++`` and
     ``infixl type +++ 5`` affects only the type-level ``+++``. Users may write
     ``infixl value +++ 5`` to make clear that they wish to affect the data-level ``+++``.

6. ``ANN`` pragmas:

   * Primary namespace: Data-level.

   * Secondary namespace: None.

   * Disambiguation: Users can write ``type`` to choose the type-level namespace.
     Users can also write ``module`` (and leave out the name) to choose to annotate
     the entire module. No extensions are required.

   * ``{-# ANN type Int "Something" #-}``; ``{-# ANN function "Something" #-}``;
     ``{-# ANN module "Something" #-}``

7. Module contexts:

   * Primary namespace: Modules.

   * Secondary namespace: None.

   * Disambiguation: If the module is used as a prefix (with ``.``) to some other
     name, spaces are prohibited around the ``.``.

8. Template Haskell name quotes:

   * Disambiguation: Users can write a single quote (``'abs``) to quote
     a data-level name and a double-quote (``''Int``) to quote a type-level name.
     
This is a mess!

Proposed Change Specification
-----------------------------

1. All features are controlled by the ``-XExplicitNamespaces`` extension.

2. Let keywords ``type``, ``data``, and ``module`` be *namespace specifiers*.

3. A namespace specifier may be used as the prefix of a ``modid`` from the `Haskell Report`_.
   Currently, we have these productions in the Report::

     qvarid -> [modid .] varid
     qconid -> [modid .] conid
     ...

   We see that ``modid`` is a module identifier. It is defined by ::

     modid -> {conid .} conid

   This proposal modifies this to be ::

     modid -> [namespace_specifier .] {conid .} conid
           |  namespace_specifier

   In addition, a ``namespace_specifier .`` prefix is allowed wherever an
   unqualified name occurrence can appear. (This effectively changes productions like
   that for ``tyvar`` to ``[namespace_specifier .] varid``.) Binding sites
   may not have a namespace specifier.

   Because qualified names are defined in Haskell's *lexical* syntax, there can
   be no spaces between the namespace specifier and the ``.``.

4. Namespace specifiers affect names in Template Haskell quotes.
   Note that the ``module`` namespace specifier makes sense here, too.

5. ``-Wcompat`` warns when:

   * ``pattern`` is used as a namespace specifier.
   * ``''`` is used to denote a type-level Template Haskell quote.
   * ``type`` is used without a ``.`` as a namespace specifier (in an import/export list).
   * ``'`` is used as a namespace specifier.
  
6. Two releases after this proposal is implemented, the first three bullets above
   (``pattern``, ``''`` for TH, and ``type`` without a ``.``) become errors.
   (In contrast,
   the ``'`` syntax will not have a planned phase-out.)

7. If this is accepted before the fixities `proposal`_ is implemented, then that
   proposal is to be amended to use ``data`` instead of ``value``.

8. It is an error to use a name in a context that does not expect that kind of name.
   For example, the use of a type name in a term will be an error, and the use
   of a module name anywhere they cannot already be used is an error.

9. ``ANN`` pragmas for modules may now mention the module name. Omitting the
   module name will become an error in two releases.

10. `Top-level kind signatures <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0036-kind-signatures.rst>`_ are amended. Instead of using the keyword ``type``
    to signal a top-level kind signature, we use the fact that the name to the left
    of the ``::`` is a (capitalized) type-level name. In effect, this replaces ``type T :: ...``
    with ``type.T :: ...``. Note the dot.


Examples
--------

Here are some examples with the proposed syntax::

  oneElement :: Vec Char (data.Succ data.Zero)) -- if Succ and Zero are unambiguous, the "data" is redundant

  someNames :: [Language.Haskell.TH.Name]
  someNames = [ 'True, 'data.False, 'type.Int, 'type.Bool, 'type.Either ]
    -- The "data" there is redundant

  num :: type.Int                 -- the "type" is redundant but harmless

  empty :: type.Vec type.Bool type.Zero   -- error: Zero is not in the type namespace

  import My.Library ( type.C, type.T, data.MkT )   -- the two "type"s are redundant but harmless

  module Foo ( type.C, data.T, module.X, module Y ) where ...
    -- NB: module.X is a synonym for module X.

  {-# ANN module.This.Module "Something #-}        -- The "This.Module" is new; no other module may be specified

  false = data.not (data.True)    -- "data"s redundant but harmless

  true = data.True :: data.Bool   -- error: no Bool in data namespace

Effect and Interactions
-----------------------

* Pseudo-keywords ``pattern`` (as used in import/export lists) and ``value`` (as written in
  the oft-referred `proposal`_) are replaced by ``data``.

* Disambiguation is now uniform: use ``type`` or ``data`` anywhere to disambiguate.

* In export lists, the use of ``module.X`` will now work, just like ``module X``. The ``module X``
  syntax will be retained in order to keep to the standard.

* Note that the ``'`` in ``'[True, False]`` is not exactly the same ``'`` as the
  one in ``'Succ``. The latter modifies a *name*; the former doesn't have a name
  to modify. Accordingly, the syntax ``'[True, False]`` remains. (But see an Alternative
  below for more discussion.)

* The varying defaults of different contexts are not changed, as doing so would be
  disastrous for backward compatibility. No current programs are newly rejected
  except those that use ``pattern`` or ``''`` (the TH quote) for namespace selection
  after two releases.

* The inclusion of ``module`` in this framework is because it fits so nicely. It is not
  yet useful, but it is my hope that this idea may spur on a proposal for first-class
  modules.

* Use of `visible dependent quantification`_ in types of terms may still require adding
  the type level as a secondary namespace in terms. Otherwise, every type mentioned in
  a term will have to have ``type`` nearby. This detail is left to the proposal for
  visible dependent quantification in types of terms, which is not covered directly by
  this proposal.

* The change to top-level kind signatures is in anticipation of a future where
  a function definition ``f x y = x + y`` is semantically similar to a datatype
  definition ``data T a = MkT Int a``. That is, both have optional type signatures
  ``f :: Int -> Int -> Int`` and ``type.T :: Type -> Type`` and both give a meaning
  to an identifier. Under this proposal, the type-level namespace and the data-level
  namespace remain separate -- meaning that type-level type signatures can be
  compiled differently from term-level type signatures -- but that restriction may
  be relaxed in the future. See `this comment <https://github.com/ghc-proposals/ghc-proposals/pull/214#issuecomment-478996527>`_ for some musings about some consequences of living
  in that future.

Costs and Drawbacks
-------------------
* There is a backward compatibility annoyance around the removal of ``pattern`` as a
  namespace specifier, but I do not think anyone will be too put out.

* ``type`` and ``data`` are certainly noisy, especially if we consider ``data`` as
  a replacement for ``'``.

* Calling a function, such as ``(+)`` a ``data`` is awkward. Yet it is simply too
  tempting to use ``data`` here, due to its status as a keyword.

* I do not expect this to be all that difficult to implement.

Alternatives
------------
* We could just drop the bit about ``module``.

* There is no concrete need to change ``type T ::`` to ``type.T ::``, but it does
  seem like it will create a more regular future.

* We could use ``value`` as the namespace specifier for data-level variables. However,
  we could not then use it in contexts like terms and types; it could never replace
  ``'``, for instance.

* A previous version of this proposal did not consider a namespace specifier like a module
  identifier, but more like a function applied to its argument. This old version is `available <https://github.com/goldfirere/ghc-proposals/blob/97b625aa85f7b77b282546003522ca71b8bb0d7b/proposals/0000-namespaces.rst>`_ in the history. I like this new version more. The old version allowed
  syntax like ``Vec Char (data (Succ Zero))`` which is quieter than the current proposal's
  equivalent, but I think the `future work`_ section below has an idea that's even better.

* We could also deprecate the current ``'`` syntax in types? There are two significant
  stumbling blocks here: promoted lists and tuples. If I say ``[Int]`` is that ``[] Int``
  or ``Int : <<nil>>`` (where ``<<nil>>`` unambiguously means the empty list)? Currently,
  we write ``'[Int]`` for the latter. If we drop ``'``, then it would have to be
  ``data [type Int]``, which is gross. If we don't allow namespace specifiers to work
  over more than one name at a time, then it would be ``data [Int]``, which isn't terrible.
  Similarly, do we have ``(Int, Bool) :: Type`` or ``(Int, Bool) :: (Type, Type)``. To get
  the latter meaning (the data constructor for tuples), we currently use ``'(Int, Bool)``.

  I think the use of lists and tuples is common enough that we can have special syntax
  for these cases. The quote-mark is well-established enough. However, it does conflict
  with ``'[]`` and ``'()`` as Template Haskell name quotes. There's no conflict when
  there are elements in the list/tuple, so we only have to worry about four names:
  the list type constructor, the nil data constructor, the unit type constructor, and
  the unit data constructor. These names could just be exported by, say,
  ``Language.Haskell.TH.Syntax``; we would advertise that these names cannot be quoted.

Unresolved Questions
--------------------

None at this time.

Future Work
-----------

*  It would be nice to be able to import names from one namespace to another wholesale.
   For example, ::

     import Prelude as type as data

   might be new syntax to import the Prelude, but making all names available in all
   namespaces. This could cause clashes. Should these be reported? Should clashes silently
   be resolved? We leave these details for another proposal. This one stands without it,
   but allowing these kinds of cross-namespace imports would help with the noisiness
   of using ``data.`` lots in types in code where many data constructors are used in types.
   The new syntax would also be useful to import names from one namespace into the other
   within one module.

* Introduce a new proposal for ``-XRequiredForAll`` that disables GHC's habit of
  implicit lexical generalization in types. This means that ``f :: a -> a`` would
  be rejected, in favor of ``f :: forall a. a -> a``. With ``-XRequiredForAll``,
  programmers gain the ability to reason locally about where type variables are
  brought into scope. This proposal would never suggest to turn on this extension
  by default, instead making it available for programmers who want this local
  reasoning ability.

Annex
=====

This is an annex to the
proposal. It is a vision for a more expressive future. Accepting this proposal
*does not* accept this annex. Accepting this proposal *does not* commit us to
accepting this annex in the future. Instead, it is included here so that we
can see how this proposal is considerate of the future. I expect to write
another proposal in the future (not anytime particularly soon) suggesting that
we accept this annex, but that would be a separate proposal with a separate
discussion and committee process.

1. Introduce a new extension ``-XDefaultNamespace``. It implies ``-XExplicitNamespaces``.

2. Let ``default`` be a *namespace specifier*.

3. Introduce a new namespace, called the *default* namespace. The default
   namespace is capable of holding both capitalized and lower-case identifiers
   and symbolic names of all (usual) spellings.

4. With ``-XDefaultNamespace`` in effect, all names bound in the module (except
   names that are currently put into the module namespace) are put into the
   default namespace.

5. With ``-XDefaultNamespace`` in effect, in all contexts (except where we
   currently look for modules, but including import/export lists),
   name resolution looks in the default
   namespace. If this lookup fails, look in both data-level and type-level
   namespaces. If exactly one of these secondary lookups succeeds, then name
   resolution has succeeded. If both secondary lookups succeed, name
   resolution fails with an error about ambiguity.

6. Uses of a name where a name of that sort is not expected is an error. In
   particular, automatic quantification of type variables happens only for type variables
   whose names are not in scope.

7. In a module without ``-XDefaultNamespace``, treat the default namespace
   as a secondary namespace in all contexts. If a context already has a secondary
   namespace, then name resolution looks in both the existing secondary namespace
   and the default namespace. If only one succeeds, then name resolution succeeds.
   If both secondary lookups succeed, name resolution fails with an ambiguity
   error.

8. As usual, the use of ``default`` as a namespace specifier is controlled by
   the ``-XExplicitNamespaces`` extension.

Alternative to annex
--------------------

See `@int-index's idea
https://github.com/ghc-proposals/ghc-proposals/pull/214#issuecomment-473196114`_
for an alternative approach.
