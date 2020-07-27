Explicit specificity in type variable binders
=============================================

.. author:: Richard Eisenberg
.. date-accepted:: 2018-07-02
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/16393
.. implemented:: 9.0
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/99>`_.
.. contents::

This proposal introduces new syntax ``typeRep :: forall {k} (a :: k). ...`` (the
braces are new) to allow a user to quantify a variable without affecting
downstream users who might use visible type application.

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
type variables, ``k`` and ``a``. This means that someone who wants the type representation for ``Int`` needs
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

* Type variables brought into scope in braces are still available as scoped type variables. Example::

    foo :: forall {k} (a :: k). ...
    foo = ... both a and k are in scope here ...

  The braces do not affect this feature at all.

* The new form of type variable binder would be allowed only in the following places:

  + Type signatures of functions / variables / class methods
  + Expression type annotations
  + GADT-syntax constructor declarations
  + Haskell98-syntax existential variable quantification
  + Pattern synonym signatures (for both universal and existential variables)
  + Type synonym right-hand sides
  + Type signatures on variables bound in ``RULES``

  It is *not* allowed in the following places:

  + ``default`` type signatures for class methods
  + instance declaration heads
  + ``SPECIALISE`` pragmas
  + Type instance right-hand sides (indeed, all ``forall``\s are banned here)
  + Type declaration left-hand sides (for ``class``, ``data``, etc.)

  In most cases where the new form is allowed, we are declaring a new construct. The braces
  indicate which variables in the type of the new construct are to be *inferred*. In the case
  where braces are used in an expression type annotation, the braces indicate which type variables
  in the expression's type are *inferred*.

Effect and Interactions
-----------------------

Note that this proposal adds new syntax to the already-existent feature of inferred variables.
Effectively, there are two different ``forall``\s: one for specified variables and one for inferred
variables. This proposal changes nothing about that, but gives users access to quantifying over
inferred variables. Accordingly, ``forall {a} b. a -> b`` is convertible to, say, ``forall b a. a -> b``
via GHC's usual invisible-quantification-rearrangement rules.

Inferred variables (those brought into scope with braces) are not available for specialization
with visible type application, exactly like inferred type variables today. Visible type application
simply skips over these variables.

GHC currently can *print* using the proposed syntax, if you turn ``-fprint-explicit-foralls`` on.
This proposal extends the parser to be able to understand this syntax.

This change is fully backward-compatible.

This change seems to be future-compatible as well: if we ever allow record syntax in types, that
will not conflict with this new feature, as the change proposed here affects only type variable
binder syntax, not the syntax of full-blooded types. It is also compatible with
`visible type application in types <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0015-type-level-type-applications.rst>`_,
though we would need to use `top-level kind signatures <https://github.com/ghc-proposals/ghc-proposals/pull/54>`_
to indicate where we wanted inferred variables.

This syntax echoes the use in other languages where braces are used to denote invisible arguments.
In Haskell, however, type variables are invisible by default; the braces here serve to make the
argument "more invisible".

Costs and Drawbacks
-------------------
* This is yet another feature to implement and describe. The difference between inferred and specified
  is somewhat subtle, so this creates another corner for language learners to run into. The implementation
  costs should be modest.

* @Ericson2314 commented that this syntax is not compatible with a hypothetical future extension to allow
  type patterns in type variable binder positions. For example, we could imagine ::

    f :: forall (Just a). Proxy a -> ()

  to be an abbreviation for ::

    f :: forall ma a. (ma ~ Just a) => Proxy a -> ()

  in much the same way that we can abbreviate ::

    g x = case x of Nothing -> True
                    Just _  -> False

  to ::

    g Nothing  = True
    g (Just _) = False

  today. If we did this, then the full syntax of types *would* be available in type variable binder
  positions, making the braces conflict with record notation. If you think the ``=`` in records would
  disambiguate, that would no longer be true with record puns in play.

  I agree that this is a potential exposed root to trip over, but the root belongs to a tree of an
  as-yet-undiscovered species in a dark wood far away from any maintained paths. I don't think this
  concern is worth changing the syntax, though I'm grateful that the problem has been pointed out.

Alternatives
------------

* Do nothing.

* Invent new concrete syntax. But I think the braces work quite nicely.

* Allow functions to quantify type variables out of dependency order. The order that variables are
  quantified affects how a client must instantiate them with visible type application. This proposal
  describes a way to suppress variables from this list, when later variables are more useful to
  instantiate than earlier ones. However, another way to achieve this is simply to allow type
  variables to be introduced out of order. That is, make ``forall (a :: k) k. ...`` a valid
  type, where the type ``a`` comes first and its kind ``k`` comes second. (In this scheme, the
  type ``forall (a :: k). forall k. ...`` would be *invalid* because ``k`` would not be lexically
  in scope at its occurrence site.) This was suggested by @Bj0rnen in the pull request.

  I like
  the idea overall, but implementing this would be a significant burden. GHC currently uses the
  same types in Core as it does in Haskell. Types in Core need to be ordered with respect to
  dependency; that's how the theory works, and Core must be based closely on the theory. So, if
  Haskell wishes to relax the rule, then it would need to have its own types. It would all seem
  to require major engineering.

* Some commentary on this proposal has pointed out that there is an asymmetry between the ability
  to introduce inferred variables, but no way to instantiate them. One way to fix this would be
  to label variables with a *specificity level*. To instantiate an argument at specificity level
  *n*, use *n* ``@`` signs. When writing a ``forall``, use braces to increase the specificity
  number of an argument. So, *required* arguments are at specificty 0, requiring no ``@`` signs.
  Today's *specified* arguments are at specificity 1, requiring 1 ``@`` sign. If the user
  writes ``f :: forall {a}. ...``, ``a`` would have specificity 2, and a caller could instantiate
  ``a`` with ``f @@Int``. If the user writes ``g :: forall {{a}}. ...``, a call could instantiate
  ``a`` with ``g @@@Bool``. A variable that GHC infers would have infinity specificity.
  (Perhaps the label should be "inferredness", but "specificity" has the advantage of actually
  being an English word.)

  This resolves the asymmetry, but at the cost of making a corner of GHC's design yet more elaborate.
  I personally don't like this, but I am sympathetic to the concerns that inspired it.

Examples
--------

@yav has asked for clarification around these examples, which I include here:

* If we type ::

    data T1 a = C1 a

  we get ::

    type T1 :: Type -> Type
    C1 :: forall a. a -> T1 a

* If we type ::

    data T2 (a :: k) = C2 { f2 :: Proxy a }

  we get ::

    type T2 :: forall k. k -> Type
    C2 :: forall k (a :: k). Proxy a -> T2 a
    f2 :: forall k (a :: k). T2 a -> Proxy a

* If we type ::

    data T3 a where C3 :: forall k (a::k). Proxy a -> T3 a

  we get ::

    type T3 :: forall {k}. k -> Type
    C3 :: forall k (a :: k). Proxy a -> T3 a

* If we type ::

    data T4 a where C4 :: forall {k} (a::k). Proxy a -> T3 a

  we get ::

    type T4 :: forall {k}. k -> Type
    C4 :: forall {k} (a :: k). Proxy a -> T3 a

* If we type ::

    data T5 k (a :: k) where C5 :: forall k (a::k). Proxy a -> T5 k a

  we get ::

    type T5 :: forall k -> k -> Type
    C5 :: forall k (a :: k). Proxy a -> T5 k a

* If we type ::

    data T6 k a where C6 :: forall {k} (a::k). Proxy a -> T6 k a

  we get ::

    type T6 :: forall k -> k -> Type
    C6 :: forall {k} (a::k). Proxy a -> T6 k a

Unresolved questions
--------------------

.. _`#80`: https://github.com/treeowl/ghc-proposals/blob/type-level-type-app/proposals/0000-type-level-type-applications.rst

.. _`#54`: https://github.com/goldfirere/ghc-proposals/blob/kind-sigs/proposals/0000-kind-signatures.rst

How will this interact when we have visible type application in types
(proposal `#80`_)? For example, consider ::

  class C (a :: Proxy k) where ...

I want ``C`` to have only one required argument, ``a``. But I also want an explicit binding
site for ``k``, so I can choose ``k``\'s kind. A nice new piece of syntax would be ::

  class C @(k :: Maybe Bool) (a :: Proxy k) where ...

This was suggested by @Saagar-A in the commentary. What if the author wanted ``k`` to
be *inferred*? Then they would have to use a top-level kind signature, as proposed
in `#54`_. This last case should be rare enough that making it inconvenient should be OK.

One alternative I originally considered was ::

  class C {k :: Maybe Bool} (a :: Proxy k) where ...

where those braces mean that I don't want ``k`` to be a required argument of ``C``. However,
here the braces change ``k`` to be *specified* instead of *required*; in contrast, this
proposal suggests the brace syntax to change a variable from *specified* to *inferred*.
But this was too confusing when considered in the context of this larger proposal, and
so I wanted a better syntax. @Saagar-A came through with that better syntax.

Implementation Plan
-------------------
I or a close collaborator volunteers to implement.
