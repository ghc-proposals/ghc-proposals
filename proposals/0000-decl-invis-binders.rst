Invisible binders in type declarations
======================================

.. author:: Vladislav Zavialov
.. co-author:: John Ericson, Simon Peyton Jones
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/425>`_.
.. contents::

We propose to allow invisible type variable binders (i.e. ``@k``) in type
declarations. Here are a few examples::

  class C @k (a :: k) where ...
         ^^^^

  data D @k @j (a :: k) (b :: j) = ...
        ^^^^^^^

  type family F @p @q (a :: p) (b :: q) where ...
               ^^^^^^^

We then propose to use these binders to simplify scoping rules and arity
inference.

Relation to `#155 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0155-type-lambda.rst>`_
---------------------------------------------------------------------------------------------------------------

This proposal is essentially the type-level counterpart to
`#155 "Binding type variables in lambda-expressions" <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0155-type-lambda.rst>`_.
While we do not have lambdas at the type level, we do have definition left-hand
sides. Compare::

  const :: a -> b -> a
  const @c x _ = (x :: c)        -- from #155

  type Const :: a -> b -> a
  type Const @c x _ = (x :: c)   -- this proposal

The subject of both proposals is the ``@c`` to the left of ``=``. The
difference is that #155 is about functions, while this proposal is about
``type``, ``data``, ``newtype``, ``class``, ``type family``, and ``data
family`` declarations.

#155 has been accepted, but it is not implemented at the time of writing.
Nevertheless, for the purposes of presentation, **the text of this proposal
assumes that term-level ``@t``-binders are already a part of the language.**

Motivation
----------

The goal of this proposal is to tidy up the language and to simplify certain
aspects of it related to name resolution, implicit quantification, and arity
inference.

We shall consider a number of various ways to arrive at the idea of
``@k``-binders. While each individual argument may seem weak, together they
form a compelling reason for the addition.

Argument 1: Symmetry between Terms and Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let us consider a kind-polymorphic class ``C``, such as the following::

  class C (a :: k)

In this declaration, ``a`` is a binding site for a type variable, whereas ``k``
is a usage site. You can easily verify this claim by trying to duplicate the
binder::

  class Cx (a :: k) (a :: k)    -- rejected, "a" is bound twice
  class Cy (a :: k) (b :: k)    -- ok

Where is ``k`` bound, then? That is where implicit quantification comes into
play. Compare with the following term-level definition::

  f (x :: a) (y :: a) = undefined

Here, ``x`` and ``y`` are variable binders, while ``a`` is a usage of an
implicitly quantified type variable. However, in terms there is a way to bind
``a`` explicitly::

  f @a (x :: a) (y :: a) = undefined
   ^^^^
   explicit binder

This proposal introduces the type-level equivalent of that feature::

  class C @k (a :: k)
         ^^^^
         explicit binder

As a consequence, the new syntax makes the language more uniform and
consistent.

Argument 2: Symmetry between ``class`` and ``instance``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let us once again consider the kind-polymorphic class introduced earlier::

  class C (a :: k)

The kind of ``C`` is ``forall k. k -> Constraint``. With this definition, all
of the following instances are permitted::

  instance C Int
  instance C Maybe
  instance C False

Note how ``Int``, ``Maybe``, and ``False`` are all of different kinds. That is
possible because we can instantiate ``k`` differently in each instance. If we
also enable ``TypeApplications``, we can make this clear::

  instance C @Type           Int
  instance C @(Type -> Type) Maybe
  instance C @Bool           False

This choice of syntax makes it apparent that ``C`` is in fact a multi-parameter
type class of kind ``forall k. k -> Constraint``. The first parameter of ``C``
is ``k``, the second parameter is ``a``, and both can be instantiated.

One might expect that if it is possible to instantiate ``k`` by writing ``C
@Type``, ``C @(Type -> Type)``, or ``C @Bool``, then the syntax to abstract
over ``k`` would be ``C @k``. Unfortunately, that is not the case::

  class C @k (a :: k)
         ^^^^
         rejected

This proposal lifts this restriction.

Argument 3: Deduplication with ``StandaloneKindSignatures``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now let us consider a slightly more complicated example::

  data P a b = MkP

  class C (a :: i -> i -> i) where
    p :: P a i

The point of interest here is the ``i`` parameter of ``C``. You will notice two things about it::

1. ``i`` is used as part of a lengthy, syntactically large type (``i -> i -> i``)
2. ``i`` is also used in the body of the class declaration (in the type of its method)

Now, assume we want for one reason or another to add a standalone kind
signature to ``C``::

  type C :: forall i. (i -> i -> i) -> Constraint
  class C (a :: i -> i -> i) where
    p :: P a i

This works, but now the ``i -> i -> i`` part is repeated twice, resulting in
undesirable code duplication. One might attempt to deduplicate by removing the
inline kind annotation on ``a``::

  type C :: forall i. (i -> i -> i) -> Constraint
  class C a where
    p :: P a i

Alas, now GHC will interpret this code very differently! The ``i`` in the
standalone kind signature for ``C`` and the ``i`` in the class method ``p`` are
no longer in any way related, and the type of ``p`` changes as follows::

  p :: forall i (a :: i -> i -> i). C a => P a i                -- before
  p :: forall i (a :: i -> i -> i) k (i1 :: k). C a => P a i1   -- after

This wouldn't happen if ``i`` from the standalone kind signature scoped over
the class body, as one might expect with ``ScopedTypeVariables`` enabled.
However, this is simply not the case: ``StandaloneKindSignatures`` and
``ScopedTypeVariables`` do not interact. One might argue that this is a reason
to make them interact in the expected way, but this proposal offers no
judgement in this regard. Instead, it offers an alternative that sidesteps the
issue entirely::

  type C :: forall i. (i -> i -> i) -> Constraint
  class C @i a where
    p :: P a i

By writing ``@i``, we bind the type variable, making it scope over the class
body; at the same time, we avoid repeating ``i -> i -> i``.

Argument 4: Quantification in Type Synonyms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider::

  type T1 = 'Nothing :: Maybe a
  type T2 = 'Just ('Nothing :: Maybe a)

``T1`` is currently legal, yielding ``T :: forall a. Maybe a``. The general
rule is that the free variables of a *top-level* kind annotation on the RHS are
brought into scope implicitly, and will be quantified in the final kind of the
type constructor.

In constrast, ``T2`` is currently illegal, because the kind annotation is not
at the top level.

We propose to drop this exotic form of implicit quantification from the
language. Both ``T1`` and ``T2`` would become illegal, but with ``@k``-binders
the programmer can rewrite them as follows::

  type T1 @a = 'Nothing :: Maybe a
  type T2 @a = 'Just ('Nothing :: Maybe a)

This way all the variables occurring on the RHS are bound on the LHS.
We exploit the new syntax to allow a nice, simple, uniform scoping rule.
To cite the User's Guide, "The reason for this exception [the strange, ad-hoc
rule about top-level kind annotations] is that there may be no other way to
bind k".

Argument 5: Arity Inference
~~~~~~~~~~~~~~~~~~~~~~~~~~~

*Arity* is a property of type synonyms and type families that determines how
many arguments are required at use sites (partial application is not allowed).
The notion of arity is described in more detail in `section 6.4.9.2.1. "Type family
declarations" <https://downloads.haskell.org/~ghc/9.2.1-alpha2/docs/html/users_guide/exts/type_families.html#type-family-declarations>`_
of the User's Guide.

Importantly, arity cannot be determined by looking at the kind of a type
constructor. Consider ``F`` declared as follows::

  type F :: Type -> forall k. Maybe k
  type family F x

The compiler can either assign it the arity of 1 or 2, and this choice will
determine whether the equations of the type family can pattern match on ``k``.
This will also determine whether a higher-kinded usage of ``F`` is possible.

Thus, both arities can make sense depending on intended usage. Currently, GHC
expects the programmer to employ an unsightly technique to specify the arity.
By default, arity inference tries to include as many forall-bound variables as
possible, to maximize the expressivity at definition site (at the cost of
higher-kinded usage). However, the user may opt out by duplicating the return
kind of the type family in its header::

  -- arity = 2
  type F :: Type -> forall k. Maybe k
  type family F x

  -- arity = 1
  type G :: Type -> forall k. Maybe k
  type family G x :: forall k. Maybe k

With ``@``-binders we can do the opposite. We propose that by default, arity
inference would include as few forall-bound variables as possible, to allow
higher-kinded usage. However, it shall also include all ``@``-bound variables::

  -- arity = 1
  type F :: Type -> forall k. Maybe k
  type family F x

  -- arity = 2
  type G :: Type -> forall k. Maybe k
  type family G x @k

This would simultaneously reduce code duplication and simplify the rules for
arity inference.

Argument 6: Explicit Binding Sites
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One might expect that for any implicitly quantified (type) variable, it would
be possible to bind it explicitly. For example, in ordinary type signatures we
can use ``ExplicitForAll`` to do it::

  f ::           a -> a     -- implicit quantification of "a"
  g :: forall a. a -> a     -- explicit quantification of "a"

That is currently not the case in type declarations. Let us once again consider
a kind-polymorphic type class::

  class C (a :: k) (b :: k) where ...

How would one bind ``k``? This proposal provides an answer::

  class C @k (a :: k) (b :: k) where ...
         ^^^^
         explicit binding site

This also increases expressivity in the presence of ambiguous variables::

  type family F a

  type C :: forall a. F a -> Constraint
  class C x where   -- no way to put a kind signature on "x"
    -- no way to bring "a" into scope here

In practice, it is easy to work around this problem by using ``forall a ->``
instead of ``forall a.``, and yet the need for the workaround reveals a rough
edge in the design of the language.

Motivation Summary
~~~~~~~~~~~~~~~~~~

Support for ``@k``-binders in type declarations will have the following
positive effects:

* Term- and type-level syntax become more consistent with one another
* ``class`` and ``instance`` declarations can be more symmetric
* Standalone kind signatures will no longer lead to duplication
* Implicit quantification rules in type synonyms can be simplified
* Arity inference becomes much more straightforward
* Every variable can have an explicit binding site

Now let us propose two additional changes that are only tangentially related to
``@k``-binders, but follow the spirit of "Argument 4" about using the LHS
exclusively to determine scope.

Addendum: Quantification in Type Family Instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider::

  type family F1 a :: k
  type instance F1 Int = Any :: j -> j

  type family F2 a :: k
  type instance F2 @(j -> j) Int = Any :: j -> j

  type family F3 a :: k
  type instance forall j. F3 Int = Any :: j -> j

The definitions and instances of ``F1``, and ``F2``, and ``F3`` are equivalent,
and all of them are already allowed today.

Notice that in ``F2``, the ``@(j -> j)`` is not a binding site for ``j``. If we
want to bind ``j`` explicitly in an instance (as opposed to a declaration), we
use an explicit ``forall j.``, as in ``F3``. That is why it is possible today
and does not require the ``@k``-binders introduced in this proposal.

In ``F1``, the ``j`` is only mentioned on the right-hand side, and yet is
implicitly quantified. This implicit quantification behavior is sometimes
counterintuitive, so we propose that all type variables must be bound on the
LHS. That is, ``F1`` would become illegal, while ``F2`` and ``F3`` would remain
accepted.

This is quite similar to the implicit quantification rules for type synonyms
that we presented in "Argument 4".

Addendum: Instantiation of Type Family Instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider::

  type family F a :: k

  type instance F Int = Char
  type instance F Int = Maybe

From the family declaration we see that ``F :: forall k. Type -> k``. The two
``type instance`` declarations appear to have an identical head, but by looking
at the RHS we can infer that the invisible kind argument of ``F`` is ``Type``
in the first instance, and ``Type -> Type`` in the second.  It would be much
clearer to write::

  type instance F @Type         Int = Char
  type instance F @(Type->Type) Int = Maybe

and indeed this is already legal.

We propose to require that the type instance be fully determined by the LHS, so
that the programmer sees two visibly distinct instance heads. For the purpose
of determining the LHS, the RHS is ignored.  So under this proposal the
instance::

   type instance F Int = Char

would mean::

   type instance F @k Int = Char

(where the LHS instantation is at an unconstrained kind ``k``).
Now the kind of the RHS if fixed to be ``k``, and the kind of ``Char`` does
not match that, so the declaration is rejected.

The principle is that it should be possible to see what instance the
programmer intended by looking only at the instance head (the LHS).
This property already holds for *data* family instances.  Suppose
``D`` is a data family of kind::

   D :: forall k. (k->Type) -> k -> Type

Now consider ::

   data instance D p q where
      MkD :: forall r. r Int -> T r Int

So what kind do ``p`` and ``q`` have?  No clues from the header, but from
the data constructor we can clearly see that ``r :: Type->Type``.  Does
that mean that the the *entire data instance* is instantiated at ``Type``
like this::

   data instance D @Type (p :: Type->Type) (q :: Type) where
      ...

Or does it mean that the GADT data constructor specialises that kind argument,
thus::

   data instance D @k (p :: k->Type) (q :: k) where
     MkD :: forall (r :: Type -> Type).
            r Int -> T @Type r Int

(It might be specialised differently in some other data constructor ``MkD2``).
GHC avoids this question by determining the instance header solely from the
header.  This proposal simply extends the same principle to type family instances.

Proposed Change Specification
-----------------------------

Background: SAKS Zipping
~~~~~~~~~~~~~~~~~~~~~~~~

**SAKS zipping** is the part of checking declarations against the corresponding
standalone kind signature that pairs quantifiers with zero-or-one binders.

Consider the following declaration::

  type T :: forall a. a -> forall b c. (b, c) -> forall d -> (a ~ b) => Type
  data T (x :: Type) y z = ...

Here we produce the following pairs::

    Quantifier  |   Binder
  --------------+------------
    forall a.   |
    a ->        | (x :: Type)
    forall b.   |
    forall c.   |
    (b, c) ->   | y
    forall d -> | z
    (a ~ b) =>  |

SAKS zipping works over two lists: quantifiers and binders. Let us define it in
pseudo-code::

  -- Base version (before this proposal)
  zipSAKS :: [Quantifier] -> [Binder] -> [(Quantifier, Maybe Binder)]
  zipSAKS (q:qs) (b:bs)
    | isInvisibleQuantifier q = (q, Nothing) : zipSAKS qs (b:bs)
    | otherwise               = (q, Just b)  : zipSAKS qs bs
  zipSAKS [] (b:bs) = error "Too many binders"
  zipSAKS _ [] = []

``isInvisibleQuantifier`` holds for ``forall a.``, ``forall {a}.``, and ``ctx
=>``, but does not hold for ``a ->`` or ``forall x ->``.

Background: Trailing Quantifiers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When SAKS zipping is done, some quantifiers may remain. Consider::

  type T :: forall a. Type -> forall b. Type
  type T x = ...

The produced pairs are::

    Quantifier  |   Binder
  --------------+------------
    forall a.   |
    Type ->     | x

Zipping stops when binders are exhausted, so the ``forall b.`` does not yield a
pair. Instead, it becomes a part of the return type. We call the remaining
quantifiers **trailing**.

During arity inference, it is determined how many of the trailing ``forall``
quantifiers are included in the arity of a type synonym or type family.

Background: Inferred vs Invisible Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In addition to visible (``forall x ->``) and invisible (``forall x.``)
quantification, GHC features inferred quantification ``forall {x}.``.

We leave it out of scope of this proposal and intentionally do not introduce
``@{k}``-binders.  See "Alternatives" for reasoning.

Primary Change: ``@k``-binders
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Relax the syntactic check of ``data``, ``newtype``, ``type``, ``class``,
   ``type family``, and ``data family`` declarations to allow ``@k``-binders in
   their headers::

     tv_bndr ::=
              | tyvar                         -- variable
              | '(' tyvar '::' kind ')'       -- variable with kind annotation
       (NEW)  | '@' tyvar                     -- invisible variable
       (NEW)  | '@' '(' tyvar '::' kind ')'   -- invisible variable with kind annotation
       (NEW)  | '@' '_'                       -- wildcard (to skip an invisible quantifier)

   The occurrences of ``@`` must be *prefix*, as defined by
   `#229 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0229-whitespace-bang-patterns.rst>`_.

2. When a declaration has no standalone kind signature, a ``@k``-binder gives
   rise to a ``forall k.`` quantifier in the inferred kind signature.
   The inferred ``forall k.`` does not float to the left; the order of
   quantifiers continues to match the order of binders in the header.

3. When a standalone kind signature is given, modify the process of zipping
   binders with quantifiers to match ``@k``-binders and ``forall k.``-quantifiers.
   To be precise, modify the pseudo-code of ``zipSAKS`` as follows::

    -- Updated version
    zipSAKS :: [Quantifier] -> [Binder] -> [(Quantifier, Maybe Binder)]
    zipSAKS (q:qs) (b:bs)
      | zippable q b  = (q, Just b)  : zipSAKS qs bs
      | skippable q   = (q, Nothing) : zipSAKS qs (b:bs)
      | otherwise     = error "Unzippable quantifier/binder pair"
    zipSAKS [] (b:bs) = error "Too many binders"
    zipSAKS _ [] = []

    skippable q = isInvisibleQuantifier q
    zippable q b =
      (isInvisibleForall q && isInvisibleBinder b) ||
      (isVisibleQuantifier q && isVisibleBinder b)

   ``isInvisibleForall`` holds for the ``forall x.`` quantifier only, but does
   not hold for ``forall {x}.`` or any other quantifier.

   ``isInvisibleBinder`` holds for ``@k``, ``@(k :: s)``, and ``@_``, but does
   not hold for ``k`` or ``(k :: s)``. ``isVisibleBinder`` is the opposite.

   ``isVisibleQuantifier`` holds for ``a ->`` and ``forall x ->`` only.
   ``isInvisibleQuantifier`` is the opposite.

   This implies that within a single declaration, some ``forall x.``
   quantifiers may be zipped with binders, while others are not.

Secondary Change: Arity Inference
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

4. During arity inference, do not include trailing ``forall x.`` quantifiers in
   the arity.

Secondary Change: Scoping Rules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

5. In type synonym declarations, require that every variable mentioned on the
   RHS must be bound on the LHS.

6. In type family and data family instances, require that every variable
   mentioned on the RHS must also occur on the LHS.

Secondary Change: Instantiation Rules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

7. In type family and data family instances, the instantiation is fully
   determined by the left hand side, without looking at the right hand side.

Examples
--------

Here's an example from the wild (thanks Jakob Bruenker)::

  data Relation n m = MkR

  type Trans :: forall a b c . Relation a b -> Relation b c -> Relation a c
  type family Trans pa pb where
    Trans rel MkR = rel -- this type checks but is a partial type family

In current GHC this typechecks, but the type family is not total. Why? Because
the fully-explicit version is::

  type family Trans pa pb where
    Trans @a @b @b rel MkR = rel

Notice the repeated ``b`` on the LHS. The author was entirely unaware that the
resulting type family was partial, because the equation he wrote looked total.
With change (7), the original program::

  type family Trans pa pb where
    Trans rel MkR = rel

would be rejected. Why? Because the LHS imposes no kind constraints, so we get::

  type family Trans pa pb where
    Trans @a @b @c (rel :: Relation a b) (MkR :: Relation b c) = ...

so the RHS must have kind ``Relation a c``. But it doesn't; ``rel`` has
kind ``Relation a b``. So the declaration is rejected, which would have
saved Jakob some time.

Effect and Interactions
-----------------------

* Changes (1), (2), and (3) provide the programmer with a more principled way
  of brining type variables into scope in certain corner cases.

* Changes (4), (5), and (6) simplify arity inference and scoping rules, but they are
  all breaking changes that rely on the new form of binders. We may want to introduce
  migratory warnings before pulling the trigger.

* Change (7) does not require a migration strategy.

Costs and Drawbacks
-------------------

The change to arity inference is breaking, but most users are
likely to be unaffected.

Alternatives
------------

* We could also introduce ``@{k}``-binders for ``forall {x}.``-quantifiers, but
  that is not actually symmetric with use sites, where ``f @{x}`` is not
  possible. In fact, it would be against the spirit of ``forall {x}.``, as the
  reason inferred variables are not subject to type applications is that we
  don't want their order to matter.

  Some thoughts on the topic can be found in GitHub comment `326/634791269
  <https://github.com/ghc-proposals/ghc-proposals/pull/326#issuecomment-634791269>`_.

Unresolved Questions
--------------------

None at the moment.

