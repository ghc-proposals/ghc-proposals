More explicit ``forall``\s
==========================

.. author:: Richard Eisenberg
.. date-accepted:: 2017-06-11
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/2600, https://gitlab.haskell.org/ghc/ghc/issues/14268
.. implemented:: 8.8
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/55>`_.
.. contents::

On type signatures, users can write ``forall ...`` to explicitly bind type/kind variables.
This allows users to give the type variables explicit kinds. However, not all constructs that
bind type variables allow the user to write them explicitly. This proposal seeks to remedy this
by enhancing GHC's concrete syntax to permit ``forall ...`` expressions wherever type variables
may be bound.

Specifically, this proposal adds the ability to write a ``forall ...`` clause on the following
constructs:

* data/newtype instances (including associated types)
* type instances (including associated types)
* closed type family equations
* ``RULES``

In contrast, the following constructs already allow user-written kinds for type variables:

* type/data families (including associated ones): The type variables listed are type variable
  binders, not occurrences.
* type synonyms: The type variables listed are binders.
* data/newtypes: The type variables listed are binders, in both Haskell98 and GADT notation.
* classes: The type variables listed are binders.
* class instances: The user can write an optional ``forall ...`` after the word ``instance``.
* standalone-deriving instances: Ditto.
* values: The user can bring scoped type variables into scope via a type signature.
* type signatures: The user can write an optional ``forall ...``.
* pattern signatures: The user can write separate optional ``forall ...``\s for both universal
  and existential type variables.
* ``SPECIALIZE`` pragmas: The user can write an optional ``forall ...`` in the type.
* ``SPECIALIZE instance`` pragmas: The user can write an optional ``forall ...`` after the word ``instance``.

I believe the list above is a full accounting of places in the GHC concrete syntax where
type variables may appear. Do comment if I've missed anything.

Motivation
------------
Given the evolution in GHC's kind system (``TypeInType`` in particular),
it is no longer possible always to *infer* the kind
of a type variable. For example, a type variable ``a`` might be used only in a place where it is expected
to have kind ``F b`` for some non-injective type family ``F``. We cannot confidently choose ``a`` to
have kind ``F b`` in this circumstance. Similarly, GHC now supports *higher-rank kinds*, where a type
variable's kind might be a polytype. These, too, cannot be inferred.

Note that a use of a type variable in a kind signature ``(a :: F b)`` does not necessarily give
the declared kind for ``a``.

Due to the inability to always infer kinds, the user may need to specify a kind manually. This proposal
allows the user to do this in all syntactic scenarios.

Proposed Change Specification
-----------------------------

1. Permit ``forall ...`` to bind type variables after the word ``instance`` in a ``data``,
   ``newtype``, or ``type`` instance. For an associated type, the ``forall`` would appear after the word
   ``data``, ``newtype``, or ``type``.

2. Permit ``forall ...`` to bind type variables at the beginning of a closed type family equation.

3. Permit ``forall ...`` to bind type variables in equations in a ``RULES`` declaration. It would
   appear after the (optional) phase specifier and before the ``forall`` that introduces term-level
   variables. For an equation that introduces both type-level and term-level variables, there would
   appear two ``forall``\s. If only one ``forall`` appears in an equation, it binds *term-level* variables.
   In the case where a user wants type-level variables but no term-level variables, the second ``forall``
   would have to be written but would be empty.

In all cases, the new ``forall`` construct binds type variables with any given kinds. In all cases,
if the users has written a type-level ``forall``, that construct must bind *all* type variables used
in the construct, much like the all-or-nothing behavior of value-level type signatures.

These new extensions would be enabled with the old extension ``ExplicitForAll``, as they are backward-compatible
with that extension.

Examples
--------

1. ::

     data family F a
     data instance forall (x :: Bool). F (Proxy x) = MkF

     class C a where
       type F a b

     instance forall a. C [a] where
       type forall b. F [a] b = Int

2. ::

     type family G a b where
       forall x y. G [x] (Proxy y) = Double
       forall z.   G z   z         = Bool

3. ::

     {-# RULES
     "example"  forall a b. forall. map @a @b id = id
     "example2" forall a. forall (x :: a). id x = x
       #-}

Effect and Interactions
-----------------------
Class instances have permitted a ``forall`` for some time. This just extends the idea to other, similar
constructs.

With this change, a user can choose never to have a type variable be brought into scope implicitly.
A particularly defensive programmer may enjoy this level of control. Similarly, no kind inference is
ever necessary for type variables if the user wishes to avoid it.

Given that ``forall`` is a keyword in types with ``ExplicitForAll``, this change is fully backward-compatible.
Note that any new ``forall`` in a ``RULES`` equation would require two ``forall``\s, something not currently
permitted.

This proposal will fix long-standing ticket `#2600 <https://gitlab.haskell.org/ghc/ghc/issues/2600>`_.

Costs and Drawbacks
-------------------
This complicates the concrete and abstract syntax of Haskell, adding a maintenance burden. The new
syntax on ``RULES`` might be counter-intuitive, but it should be very easy to understand in other
places.


Alternatives
------------

I argue that maintaining the status quo is not a viable alternative, as the inability to specify
the kinds of variables in these places inhibits the use of ``TypeInType`` features.

There is no strict need for the all-or-nothing behavior of these new ``forall``\s; that requirement
can be dropped.

Resolved questions
------------------

1. **Why have two** ``forall``\ **s in a** ``RULES`` **declaration?** Because otherwise users would have a hard
   time telling type variables from term variables. A syntactic analysis could sort this out, but that
   seems more confusing than having two ``forall``\s.

2. **How will the two** ``forall``\ **s work with Dependent Haskell?** Dependent Haskell would need to generalize
   the syntax of ``RULES`` to allow an arbitrary number of uses of the ``forall`` keyword to be backward
   compatible with this proposal. This is in keeping with the use of ``forall`` in type signatures, where
   ``forall a b c. ...`` is an abbreviation for ``forall a. forall b. forall c. ...``. In short, I don't
   see problems here.


Unresolved questions
--------------------

None at this time.

Implementation Plan
-------------------

Implementation shouldn't be hard. I volunteer either myself or a close collaborator.
