Invisible binders in type declarations
======================================

.. author:: Vladislav Zavialov
.. co-author:: John Ericson, Simon Peyton Jones
.. date-accepted:: 2022-04-07
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/425>`_.
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

Proposed Change Specification: Declarations
-------------------------------------------

Syntax
~~~~~~

Relax the syntactic check of ``data``, ``newtype``, ``type``, ``class``,
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

Guarded behind a new flag, ``-XTypeAbstractions``.

Scope
~~~~~

In type synonym declarations, require that every variable mentioned on the
RHS must be bound on the LHS. For three releases before this change takes
place, include a new warning ``-Wimplicit-rhs-quantification`` in
``-Wcompat``, to inform users of affected definitions.

Kind Inference
~~~~~~~~~~~~~~

When a declaration has no standalone kind signature, a ``@k``-binder gives
rise to a ``forall k.`` quantifier in the inferred kind signature.
The inferred ``forall k.`` does not float to the left; the order of
quantifiers continues to match the order of binders in the header.

Kind Checking
~~~~~~~~~~~~~

To kind-check a declaration that has a standalone kind signature (SAKS), we
must associate the *quantifiers* of the kind signature with the *binders* of
the type declaration. We call this **SAKS zipping**. For example, consider the
following declaration::

  type T :: forall a. a -> forall b c. (b, c) -> forall d -> (a ~ b) => Type
  data T (x :: Type) @t y z = ...

Here we produce the following pairs::

      Quantifier  |   Binder
  ----------------+------------
  1.  forall a.   |
  2.  a ->        | (x :: Type)
  3.  forall b.   | @t
  4.  forall c.   |
  5.  (b, c) ->   | y
  6.  forall d -> | z
  7.  (a ~ b) =>  |

Notice that each quantifier is associated with either one binder or none.

This association plays two roles:

* It fixes the arity of the type constructor. The arity is the number of
  quantifiers up to and including the one paired with the last binder. In our
  example, the last binder is ``z``, which is paired with the sixth quantifier
  ``forall d ->``, so the arity is ``6`` (see also "Arity Inference" below).

* It associates the kinds in the kind signature with the variables in the
  declarations. For example, the binder ``y`` is associated with the quantifier
  ``(b,c) ->``, so ``y`` must have kind ``(b,c)``. Similarly the binder ``@t``
  is associated with the quantifier ``forall b.``, so ``t`` is simply a name
  for ``b``.

SAKS zipping works over two lists: quantifiers (from the signature) and binders
(from the declaration). Let us define it in pseudo-code::

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

Where the predicates are defined as follows (with ``⟦ ... ⟧`` denoting AST quotation)::

  isInvisibleForall :: Quantifier -> Bool
  isInvisibleForall q = case q of
    ⟦ forall x.        ⟧  ->  True
    ⟦ forall (x :: k). ⟧  ->  True
    _                     ->  False   -- incl. forall {x}.

  isInvisibleBinder :: Binder -> Bool
  isInvisibleBinder b = case b of
    ⟦  @k         ⟧   ->  True
    ⟦  @(k :: s)  ⟧   ->  True
    ⟦  @_         ⟧   ->  True
    _                 ->  False

  isVisibleBinder = not . isInvisibleBinder

  isVisibleQuantifier :: Quantifier -> Bool
  isVisibleQuantifier q = case q of
    ⟦  a ->                ⟧   ->  True
    ⟦  forall x ->         ⟧   ->  True
    ⟦  forall (x :: k) ->  ⟧   ->  True
    _                          ->  False

  isInvisibleQuantifier = not . isVisibleQuantifier

Arity Inference
~~~~~~~~~~~~~~~

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
quantifiers *trailing*.

In today's GHC, there is an additional step called *arity inference* to decide
which of the trailing quantifiers to include in the arity in addition to the
zipped ones.

We propose to remove this step entirely, so that the arity is fully determined
by SAKS zipping, as ``@k``-binders provide the same control over arity but in a
more principled way.

Inferred Variables
~~~~~~~~~~~~~~~~~~

In addition to visible (``forall x ->``) and invisible (``forall x.``)
quantification, GHC features inferred quantification ``forall {x}.``.

We leave it out of scope of this proposal and intentionally do not introduce
``@{k}``-binders.  See "Alternatives" for reasoning.

Proposed Change Specification: Instances
----------------------------------------

The changes to instances are not directly related to the main body of the
proposal, but they are close to it in spirit, so we include them here.

Scope
~~~~~

In type family and data family instances, require that every variable
mentioned on the RHS must also occur on the LHS.

Instantiation
~~~~~~~~~~~~~

In type family and data family instances, the instantiation is fully
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
With the proposed change to instantiation, the original program::

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

The proposed changes provide the programmer with a more principled way of
brining type variables into scope in certain corner cases, simplify arity
inference and scoping rules.

Costs and Drawbacks
-------------------

The proposed changes break existing code in a few ways.

Breakage 1: Scoping in Type Synonyms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The first source of breakage is a change in the scoping of type synonyms. Consider::

  type T1 = 'Nothing :: Maybe a

This is no longer accepted, as ``a`` is not bound on the LHS. Instead, the user must write::

  type T1 @a = 'Nothing :: Maybe a

See "Argument 4: Quantification in Type Synonyms" for the motivation.

There's no backwards-compatible way to rewrite this example, so we introduce
the ``-Wimplicit-rhs-quantification`` warning, wait for three releases (in
accordance with 3-Release-Policy), and only then make the change.

Breakage 2: Arity Inference
~~~~~~~~~~~~~~~~~~~~~~~~~~~

*Background*. Every type synonym and type family has an *arity*, which
specifies the number of arguments that must be supplied at every usage
of the type synonym or family. Note that arity is distinct from a kind.
For example::

  type F1 :: Type -> Type -> Type
  type family F1 a where
    F1 Int  = Maybe
    F1 Bool = Either Double

  type F2 :: Type -> Type -> Type
  type family F2 a b where
    F2 Int       Double    = Char
    F2 (Maybe a) (Maybe a) = a

Even though ``F1`` and ``F2`` have the same kind (``Type -> Type -> Type``),
they have a different arity. Thus, writing e.g. ``StateT Int (F1 b) a`` is allowed,
while ``StateT Int (F2 b) a`` would not be: the latter does not fully apply
``F2`` to all of its arguments, and GHC does not (yet!) support unsaturated
type families (or synonyms).

Importantly, arity applies both to *visible* arguments, like the ones above,
and *invisible* arguments, such as appear in e.g. ``type F3 :: forall k. k -> k``.

*Breakage*. The second source of breakage is the change to arity inference. Consider::

  type F :: Type -> forall k. Maybe k
  type family F x where
    F Int @Type = Just Bool
    F Int       = Just Either

This definition is currently assigned the arity of 2, but with the proposed
changes will be assigned arity of 1. The arity of 2 is important, because
the equations match on the kind variable. (The second equation does this, too, by
choosing ``k = Type -> Type -> Type``, according to the RHS. This confusing
situation is also barred by this proposal.) To keep the arity of 2, the user must rewrite it thus::

  type F :: Type -> forall k. Maybe k
  type family F x @k where ...

See "Argument 5: Arity Inference" for the motivation.

We assume that this is an obscure situation and the change will go
unnoticed by most users, because in order to encounter it, one must use
``StandaloneKindSignatures`` – a relatively recent addition in itself.

No migration strategy is provided: GHC must assign *some* arity to definition
with a trailing invisible variable, and having this behavior user-configurable
seems undesirable.

Breakage 3: Scoping in Instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The third source of breakage is the new requirement that variables mentioned on
the RHS must also occur on the LHS. Consider::

  type family F a :: k
  type instance F Int = Any :: j -> j

This is no longer accepted, as ``j`` is not mentioned on the LHS. The user must
rewrite it as follows::

  type family F a :: k
  type instance F @(j -> j) Int = Any :: j -> j

See "Addendum: Quantification in Type Family Instances" for the motivation.

The ``@``-binders in type instances are already legal, so there's no need for a
migration strategy: the fix is backwards-compatible.

Breakage 4: Instantiation
~~~~~~~~~~~~~~~~~~~~~~~~~

The fourth source of breakage is that instantiation of type/data family
instances is fully determined by the left hand side, without looking at the
right hand side. Consider::

  type family F a :: k
  type instance F Int = Char
  type instance F Int = Maybe

This is no longer accepted, as the LHS ``F Int`` does not fully determine if
the instance matches, as evidenced by the presence of two instances with
identical LHSs.

The user must rewrite this code as follows::

  type family F a :: k
  type instance F @Type         Int = Char
  type instance F @(Type->Type) Int = Maybe

See "Addendum: Instantiation of Type Family Instances" for the motivation.

The ``@``-binders in type instances are already legal, so there's no need for a
migration strategy: the fix is backwards-compatible.

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

