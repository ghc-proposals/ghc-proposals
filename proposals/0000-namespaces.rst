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

In brief: use ``type`` to refer to the type-level namespace, ``data`` to the term-level
namespace, and ``module`` to refer to the module namespace. (Spoiler: this last idea
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
  contexts in a recent accepted `proposal`_.

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

2. Types:

   * Primary namespace: All identifiers and symbols are taken from the
     type-level variable namespace or the type constant namespace, as
     appropriate.

   * Secondary namespace: For capitalized alphabetic identifiers or symbols
     that begin with a ``:``, if the lookup in the type-level namespace fails,
     GHC looks in the data constructor namespace.

   * Disambiguation: Users may prefix these names with a ``'`` to request a
     lookup in the data constructor namespace only.

3. Import/export lists:

   * Primary namespace:

     * Uppercase alphabetic identifiers and ``:``\-symbols: Type constants namespace.

     * Lowercase alphabetic identifiers, including other symbols: Data-level variables namespace.

   * Secondary namespace: None. If the primary lookup fails, the program is rejected.

   * Disambiguation: Users may write ``type`` to choose the type constants
     namespace and ``pattern`` to choose the data constructor namespace. The use of
     ``type`` requires ``-XExplicitNamespaces`` and the use of ``pattern`` requires
     ``-XPatternSynonyms``.

4. Fixities, ``WARNING``, and ``DEPRECATED`` (currently implemented):

   * Primary namespace: Both type constants and data constructors/variables are considered
     primary.

   * Secondary namespace: None.

   * Disambiguation: Not possible. If a name exists in both primary namespaces, the directive
     applies to the names in both namespaces, even if these names are unrelated.

5. Fixities, ``WARNING``, and ``DEPRECATED`` (as in `proposal`_):

   * Primary namespace: Data-level.

   * Secondary namespace: Type-level. Only when a name does not exist in a data-level
     namespace will a type-level namespace be consulted.

   * Disambiguation: Users can write ``type`` to choose the type-level namespace and
     ``value`` to choose the data-level namespace.

6. ``ANN`` pragmas:

   * Primary namespace: Data-level.

   * Secondary namespace: None.

   * Disambiguation: Users can write ``type`` to choose the type-level namespace.
     Users can also write ``module`` (and leave out the name) to choose to annotate
     the entire module. No extensions are required.

7. Module contexts:

   * Primary namespace: Modules.

   * Secondary namespace: None.

   * Disambiguation: If the module is used as a prefix (with ``.``) to some other
     name, spaces are prohibited around the ``.``.

This is a mess!

Proposed Change Specification (1)
---------------------------------

1. All features are controlled by the ``-XExplicitNamespaces`` extension.

2. Let keywords ``type``, ``data``, and ``module`` be *namespace specifiers*.

3. A namespace specifier may prefix a name any place a name can occur. It states
   what namespace the name belongs to. (Namespace specifiers may *not* be used
   where a name is bound. Occurrences only.)

4. In terms and types, namespace specifiers bind as tightly as function application.
   A namespace specifier describes the namespace for all names used in its scope.
   (This scope may contain multiple names if a ``(`` follows the namespace specifier.)

5. ``-Wcompat`` warns on uses of ``pattern`` and ``'`` as namespace specifiers.
  
6. Two releases after this proposal is implemented, it becomes an error to use
   ``pattern`` as a namespace specifier in import/export lists.
   (In contrast,
   the ``'`` syntax will not have a planned phase-out.)

7. If this is accepted before the fixities `proposal`_ is implemented, then that
   proposal is to be amended to use ``data`` instead of ``value``.

8. It is an error to use a name in a context that does not expect that kind of name.
   For example, the use of a type name in a term will be an error, and the use
   of a module name anywhere they cannot already be used is an error.

9. Namespace specifiers are not allowed as the first lexeme at top-level.

Proposed Change Specification (2)
---------------------------------

This second proposed change specification is, essentially, an annex to the
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

Effect and Interactions
-----------------------
This considers only the primary proposal, not the annex.

* Pseudo-keywords ``pattern`` (as used in import/export lists) and ``value`` (as written in
  the oft-referred `proposal`_) are replaced by ``data``.

* Namespace specifiers can now scope over a region of code, for convenience.

* Disambiguation is now uniform: use ``type`` or ``data`` anywhere to disambiguate.

* The varying defaults of different contexts are not changed, as doing so would be
  disastrous for backward compatibility.

* The inclusion of ``module`` in this framework is because it fits so nicely. It is not
  yet useful, but it is my hope that this idea may spur on a proposal for first-class
  modules.

* As is sometimes the case, the new syntax leaves open the possibility of ambiguity.
  Consider this::

    data X a = Y a

  Is this a datatype declaration for ``X``? Or perhaps it is a pattern-match, binding
  data-level variable ``a`` as the contents of data constructor ``X``, where we have
  disambiguated ``X`` by using the namespace specifier ``data``. To eliminate this
  ambiguity, I have said that namespace specifiers cannot be the first lexeme at top-level.

* Use of `visible dependent quantification`_ in types of terms may still require adding
  the type level as a secondary namespace in terms. Otherwise, every type mentioned in
  a term will have to have ``type`` nearby. This detail is left to the proposal for
  visible dependent quantification in types of terms, which is not covered directly by
  this proposal.

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

* We could use ``value`` as the namespace specifier for data-level variables. However,
  we could not then use it in contexts like terms and types; it could never replace
  ``'``, for instance.

* We could not allow namespace specifiers to work over more than one name at a time.

Unresolved Questions
--------------------
None at this time.

