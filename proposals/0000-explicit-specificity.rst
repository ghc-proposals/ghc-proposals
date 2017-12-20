.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/99>`_.

.. contents::

Explicit specificity in type variable binders
=============================================

`Visible type application
<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#visible-type-application>`_
brings with it the need to classify type variable arguments to functions as
either *specified* or *inferred*. The original `paper
<https://repository.brynmawr.edu/cgi/viewcontent.cgi?article=1001&context=compsci_pubs>`_
describes the motivation for this need, though the paper uses "generalized"
where this proposal uses "inferred". Briefly, it's impossible to know what ``f @Int`` would
mean if we don't have a user-written type signature for ``f``, so even if ``f`` is polymorphic
(but without a signature), we don't allow visible type application. None of this is new in this proposal.

GHC currently labels any user-written type or kind variable as *specified*, never *inferred*. This means
that the use of a type variable in a type signature *changes the user-visible typing behavior of a function*.
For example, consider the two following type signatures::

  typeRep1 :: Typeable a => TypeRep a
  typeRep2 :: Typeable (a :: k) => TypeRep (a :: k)

Because any user-written type or kind variable is *specified*, the type of ``typeRep2`` contains *two* specified
type variables, ``k`` and ``a``. This means that someone who wants the type represenatation for ``Int`` needs
to say ``typeRep2 @Type @Int`` or perhaps ``typeRep2 @_ @Int``. Contrast with ``typeRep1``, which is kind-polymorphic
but keeps its kind variable as inferred. Clients would get the representation for ``Int`` with ``typeRep1 @Int``.
None of this is new in this proposal.

This proposal includes a new form of type variable binder, ``{tyvar}`` or ``{tyvar :: kind}``, written in braces, that
introduces a new type variable but keeps that type variable's classification to be *inferred*, not *specified*.
Accordingly, one might write ::

  typeRep3 :: forall {k} (a :: k). Typeable a => TypeRep a

which would behave identically to ``typeRep1``, but a reader seeing the type signature would know that the
function is kind-polymorphic.

Motivation
------------

* One motivator appears in the introduction. It is nice to readers of
  kind-polymorphic code to annotate where kind-polymorphism happens, but doing
  so currently necessitates changes to the way clients use a function. With
  explicit specificity markers, such a downstream effect could be squashed.

* The way scoped type variables works also suggests a need for this feature.
  To bring a type variable into scope in a function definition, it is necessary
  to name the type variable in the type signature of the function. However, because
  all mentioned type variables become specified, choosing to bring a variable into
  scope in a *definition* affects the effective *interface* of a function. This
  is non-modular.

* GHC currently requires type variables to be listed in dependency order. However,
  what if a library wants to export a kind-polymorphic function where clients will
  more likely specialize the type, not the kind? Having explicit specificity gives
  us a workaround::

    typeRep4 :: forall {k} (a :: k) k'. (k ~ k', Typeable a) => TypeRep a

  With ``typeRep4``, a client can get the representation for ``Int`` with ``typeRep4 @Int``
  but can still specify the kind if they want: ``typeRep4 @Int @Type``.

Proposed Change Specification
-----------------------------

* Change GHC's ``tv_bndr`` production to include two new rules, making the full set of rules thus::

    tv_bndr ::= tyvar
              | '(' tyvar '::' kind ')'
	      | '{' tyvar '}'
	      | '{' tyvar '::' kind '}'

  The first two rules already exist; the last two are new.

* A type variable brought into scope with one of the two new productions will be treated as *inferred*
  and be unavailable for specialization via visible type application, following all the current rules
  for inferred type variables.

Effect and Interactions
-----------------------

GHC currently can *print* using the proposed syntax, if you turn ``-fprint-explicit-foralls`` on.
This proposal extends the parser to be able to understand this syntax.

This change is fully backward-compatible.

This change seems to be future-compatible as well: if we ever allow record syntax in types, that
will not conflict with this new feature, as the change proposed here affects only type variable
binder syntax, not the syntax of full-blooded types.

This syntax echoes the use in other languages where braces are used to denote invisible arguments.
In Haskell, however, type variables are invisible by default; the braces here serve to make the
argument "more invisible".


Costs and Drawbacks
-------------------
This is yet another feature to implement and describe. The difference between inferred and specified
is somewhat subtle, so this creates another corner for language learners to run into. The implementation
costs should be modest.


Alternatives
------------

* Do nothing.

* Invent new concrete syntax. But I think the braces work quite nicely.

Unresolved questions
--------------------
None at this time.

Implementation Plan
-------------------
I or a close collaborator volunteers to implement.
