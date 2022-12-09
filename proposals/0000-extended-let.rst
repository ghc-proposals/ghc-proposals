Let Extensions
==============

.. author:: Richard Eisenberg
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/523>`_.
.. sectnum::
.. contents::

.. _`#448`: https://github.com/ghc-proposals/ghc-proposals/pull/448
.. _`#378`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst

This proposal suggests several extensions to Haskell's ``let`` syntax,
allowing for local type synonyms (but not datatypes or classes) and ``let`` in
types. The two different features in this proposal
may be accepted independent of the other, but I think they synergize nicely.

This proposal was spun off from `#448`_, but you do not need to read that
proposal to understand this one.

``let``-binding types
---------------------

.. _type-let:

Motivation
~~~~~~~~~~

1. Users have, from time to time, requested the ability to make local type synonyms.
   GHC even has a little support for synonyms via equality constraints (e.g., writing
   ``f :: (a ~ Some Big Type With Lots Of Parts) => Maybe a -> a -> Maybe a``). Instead
   of encoding this idea via equality constraints, though, it would be nice to support
   it directly.

#. Type variables can stand for types, and so we can write code like ::

     f :: Maybe Bool -> Bool
     f (x :: Maybe b) = (True :: b)

   Note that the pattern signature binds ``b`` to ``Bool``. This is, essentially, a ``let``\ -bound
   type variable: in the scope of ``b``, ``b`` is synonymous with ``Bool``. Yet the only way
   to make such a ``b`` is via a pattern (or result, `#228`_) signature. Why force users
   to use matching instead of binding the variable directly?

#. Supporting ``let``\ -binding in types gets us a bit closer to unifying the syntax
   of types and terms, in sympathy with `#378`_.

Proposed Change Specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Create a new extension ``-XExtendedLet``.

#. With ``-XExtendedLet``, add two new productions for ``decl`` (from the `Haskell 2010 Report`_), ::

     decl → 'type' simpletype '=' type
          → 'type' tyvar '=' type

   and remove the (now redundant) production ``topdecl → 'type' simpletype '=' type`` from ``topdecl``.

   Note that the second form allows a local binding for a lower-case ``tyvar``; these
   synonyms may not be parameterized.

#. The form ``decl → 'type' tyvar '=' type`` is not allowed at top-level.

#. These new declaration forms introduce local type synonyms in terms, which scope over the same
   region of code that other declarations in the same ``let`` / ``where`` clause scope over.

   Semantically, these type synonyms are just shorthand for their right-hand sides. They can
   always be eagerly expanded. Accordingly, and like other type synonyms, local type synonyms
   may not be recursive.

#. Wildcards are allowed in the right-hand side of local synonyms. At usage sites of the
   synonym, the synonym is expanded. It is an error if that location does not allow wildcards.
   The wildcard is understood to stand for just one type shared among all the expansions,
   and the type it is unified with must be in scope at the location the synonym is defined.

Effects
~~~~~~~

1. We can now bind local type synonyms, avoiding the need to do so via pattern or result
   signatures.

#. Due to the way the grammar is written, binding types in this way
   applies equally in ``where`` clauses as it does in ``let`` clauses.

#. One challenge is how to present these local synonyms in error messages. It might be
   best to aggressively expand (unlike top-level type synonyms), especially because these
   local synonyms might refer to other local type variables that are in scope. As we gain
   experience with this new form, we can refine their appearance in error messages.

#. Note that this proposal does *not* allow for top-level lower-case type synonyms. There
   is nothing stopping us from doing so, but it would seem to violate expectations of Haskellers
   and would be the first instance of a lower-case type variable being in scope at the top level.

Example
~~~~~~~

The following program is rejected because the wildcard synonym requires being unified with
a type that is not yet in scope::

  id :: forall a. a -> a
  id = let type b = _ in \ @a x -> ((x :: b) :: a)

``let`` in types
----------------

This part of the proposal allows ``let`` to be used in types.

Motivation
~~~~~~~~~~

1. The careful reader will notes that the `section above <#type-let>`_ defining
   the ability to bind type synonyms in ``let`` expressions does not actually address
   a motivating example. This component of this proposal allows us to avoid repetition
   within a type signature.

Proposed Change Specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. With ``-XExtendedLet``, expand the grammar for types to include the following::

     type → 'let' tdecls 'in' type

     tdecls → '{' tdecl1 ';' ... ';' tdecln '}'
     tdecl → simpletype '=' type
           → tyvar '=' type

   Note that we do not include the ``type`` keyword in the grammar above, because
   we are already in type-syntax.

#. The type synonyms introduced in a ``let`` in types scope over the type after the
   ``in``.

#. As above, the synonyms may mention wildcards, and the definitions
   may not be recursive.

#. Add the possibility of a ``where`` clause to the end of the
   following syntax productions (all taken from the `Haskell 2010
   Report
   <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17500010>`_)
   and brackets denote optional elements (following the report)::

     topdecl → 'type' simpletype = type ['where' tdecls]    -- perhaps amended above to start with 'decl', not 'topdecl'
             | 'data' [context '=>'] simpletype ['=' constrs] [deriving] ['where' tdecls]
             | 'newtype' [context '=>'] simpletype '=' newconstr [deriving] ['where' tdecls]
     gendecl → vars '::' [context '=>'] type ['where' tdecls]

   In addition, a ``tdecl`` is allowed among the constructors in GADT syntax;
   it scopes over the entire ``where`` block, but not beyond it.

Examples
~~~~~~~~

1. Instead of ::

     f :: forall a b c. (c ~ Very Big Type a b) => c -> c -> c

   we can write ::

     f :: forall a b. let c = Very Big Type a b in c -> c -> c

   which more directly expresses what we mean.

Effects
~~~~~~~

1. This step further unifies term-level and type-level syntax, at low cost.

#. An initial version of this feature will likely want to expand the synonyms
   aggressively. We can think about ways to preserve synonyms as we gain experience
   with the feature.

#. Over time, we might decide to add ``where`` in more places.
   
Effect and Interactions
-----------------------

.. _`Explicit Binding Principle`: ../principles.rst#explicit-binding-principle
.. _`Syntactic Unification Principle`: ../principles.rst#syntactic-unification-principle

1. The `Syntactic Unification Principle`_ is supported. The new ``let`` syntax in types is a strict subset
   of its syntax in terms, and the semantics are compatible. Note that allowing ``let`` in types brings
   us closer to getting this principle.

#. The ``-XExtendedLet`` features work as a convenient replacement
   for pattern signature binds, without sacrificing the `Explicit Binding Principle`_. For example::

     f :: Maybe (Either Int (Char, Double, Bool)) -> Maybe (Either Int (Char, Double, Bool))
     f Nothing = Just (Left 5)
     f (x :: a) = (x :: a)

   could be rewritten to ::

     f :: Maybe (Either Int (Char, Double, Bool)) -> Maybe (Either Int (Char, Double, Bool))
     f = \cases
       Nothing -> Just (Left 5)
       x :: a -> x :: a
       where
         type a = _

   While this second version is more verbose, it makes the binding site of ``a`` explicit, the
   linchpin of the `Explicit Binding Principle`_.

#. The existing trick of using e.g. ``f :: forall a. (a ~ SomeBigType). ... a ... a ... a`` to
   bind a type variable can be retired (though it would still work just as well as it does today).

Costs and Drawbacks
-------------------

1. This introduces yet more syntax to read, understand, parse, have errors about, etc. This drawback
   alone may make this proposal not worth accepting.

Alternatives
------------

1. A `previous version of this proposal <https://github.com/goldfirere/ghc-proposals/blob/29abac166f44dc02b492462c2bcb942a8717354f/proposals/0000-extended-let.rst#id15>`_ included two possible syntaxes for a ``let`` amongst a list of patterns. Neither was very satisfactory, and
   discussion revealed that the syntax was really unnecessary. So I've removed this part of the proposal.


Unresolved Questions
--------------------

None at this time.

Implementation Plan
-------------------

The implementation here should be relatively easy, and should be done
in two phases (for the two parts of the proposal). Given the challenge
of finding time, I would love to work with a collaborator on the
implementation, where I do an initial implementation, but someone else
sees the work through. (This should be a fun opportunity to learn more
about GHC's internals.) I'm also happy for someone else to take over
from the beginning! Though I imagine my experience will be helpful in
putting together a design for the implementation that goes along with
the grain in GHC.
