Class Method Headers
====================

.. author:: Vladislav Zavialov
.. date-accepted::
.. proposal-number::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/267>`_.
.. sectnum::
.. contents::

We propose to allow class variable binders on the left of ``::`` in class method signatures::

   class C a b where
      f @b @a :: (a, b) -> Bool

   -- f :: forall b a. C a b => (a, b) -> Bool

The ordering of these variables may differ from their ordering in the class declaration header.

The syntax mirrors that of associated type headers and
`"Type variable binders" <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0155-type-lambda.rst>`_.

Motivation
------------

At the moment, the signature of a class method is prefixed with class variables
and a class constraint::

  class C a b where
    f :: (a, b) -> Bool

  -- f :: forall a b. C a b => (a, b) -> Bool
  --      ^^^^^^^^^^^^^^^^^^^^
  --      compiler-generated prefix


The users have no way to guide generation of this prefix. In particular, the
ordering of the class variables is inherited from the class declaration header.
This matters at use sites with ``-XTypeApplications``::

  f @Int :: forall b. C Int b => (Int, b) -> Bool

What if we wanted a different ordering? ::

  f @Int :: forall a. C a Int => (a, Int) -> Bool

We cannot specify that we'd prefer  ``forall b a.`` instead of ``forall a b.``
for this particular method. There are two workarounds:

* Change the ordering of class variables in the class declaration header::

    class C b a where   -- instead of:  class C a b

  This has the downside that now the class constructor has a different ordering
  of variables, not just its method. Also, it does not give per-method control.

* Create a wrapper function::

    fWrapper :: forall b a. C a b => (a, b) -> Bool
    fWrapper = f @a @b

  This means that users have to define instances using original method names,
  but call the methods using the wrappers, which leads to more complicated
  APIs.

Variable ordering aside, consider visible dependent quantification that we have
in types today::

  data I   (a :: k)   --  I :: forall k.   (a :: k) -> Type
  data V k (a :: k)   --  V :: forall k -> (a :: k) -> Type

We distinguish invisible forall (``forall k.``) and visible forall (``forall k
->``). With the advent of dependent types, a similar feature will be added at
the term level, and then we will face the question of how to choose between
visible and invisible ``forall`` for class method variables.

This proposal provides an excellent forward compatibility story: we can simply
omit the ``@`` symbol in binders to indicate visibility::

  class C k (a :: k) where
    f @k a :: P a

  -- C :: forall k -> k -> Constraint
  -- f :: forall k. forall (a :: k) -> P a

That is, not only the ordering, but also the visibility of class variables can
differ between the class constructor and class methods.

Furthermore, class method headers are a limited version of the same feature of
associated types, and as such, it will be an indispensable asset in their
unification as described in the `Grand Class Unification
<https://github.com/ghc-proposals/ghc-proposals/pull/236>`_ meta-proposal.
Compare ``f`` and ``F``, which are now quite similar::

  class C a where
    type F a :: P a
    f a :: P a

    -- F :: forall a ->        P a
    -- f :: forall a -> C a => P a

To summarize, there are three reasons to make this change:

* Control of class variable ordering in class methods for use with ``-XTypeApplications``.
* Control of class variable visibility with advent of visible ``forall`` in terms.
* A step toward unification of associated types and class methods.

Proposed Change Specification
-----------------------------

Syntax
~~~~~~

Take the Haskell 2010 class method signature grammar as the starting point::

  gendecl -> vars :: [context =>] type
  vars    -> var_1 , ... , var_n         (n ≥ 1)

Instead of variable names for class method left-hand sides, we introduce the
notion of a signature header::

  sighdr  -> var (sigbndr_1 ... sigbndr_n)
  sigbndr -> tyvar
           | @tyvar

  gendecl -> sighdrs :: [context =>] type
  sighdrs -> sighdr_1 , ... , sighdr_n         (n ≥ 1)

A validity check ensures that the binders are only used in class method
signatures and are disallowed in function signatures.

The syntax of function bindings, including method definitions in instances, is
assumed to be extended by `"Type variable binders"
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0155-type-lambda.rst>`_
to allow type binders prefixed with ``@``::

  f @Int x = ...

Semantics
~~~~~~~~~

* When one or more binders are present in a class method signature, we consider
  it a class method header, and require the ``-XClassMethodHeaders`` extension.

* A class method header must bind every class variable mentioned in the class
  declaration header, and must bind it exactly once.

* No other variables can be bound in the class method header.

* In the compiler-generated top-level signature for the class method, variables
  bound as ``@a`` are quantified with ``forall a.``, and variables bound as
  ``a`` are quantified with ``forall a ->``.

* In the compiler-generated top-level signature for the class method, the ordering
  of quantifiers matches the ordering of binders in the class method header.

* The compiler-generated top-level signature for the class method is subject to
  validity checking, which should reject variables quantified out of dependency
  order and the (as of yet) unsupported visible ``forall``.

* Class method definitions in instance declarations may explicitly write out
  instantiations of invisible class variables::

    class C a where
      f @a :: a

    instance C Int where
      f @Int = 42

  This mirrors the syntax of associated type definitions (associated type
  family equations).

* The presence of a class method header is not observable outside the class
  declaration::

    class C a b where
      f       :: a -> b
      f @a @b :: a -> b  -- identical

  A consequence of this is that class method definitions may bind class
  variables regardless of the presence of a class method header in the class
  declaration::

    class C a where
      f :: a         -- note: no @a

    -- f :: forall a. C a => a

    instance C Int where
      f @Int = 42    -- still allowed

* Class variables bound in methods must be identical to their occurrences in
  the instance header, as is the case with associated types.

* Class variables bound in methods always precede other arguments.

Examples
--------

* Comma-separated class methods with different class variable ordering::

    class C a b where
      f @a @b, g @b @a :: a -> b

    -- f :: forall a b. C a b => a -> b
    -- g :: forall b a. C a b => a -> b

* Erroneous class method header that mentions non-class variable::

    class C a where
      f @b :: a -> b

  Rejected with the following message::

    • ‘b’ is not bound in the class declaration header ‘C a’
    • In the class method header: f @b

  The implementation may opt to provide a different error message in the same spirit.

* Erroneous class method header that mentions class variables out of dependency order::

    class C (a :: k) where
      f @a @k :: P a

    -- f :: forall a k. C (a :: k) => P a

  Rejected with the following message::

    • These kind and type variables: a k
      are out of dependency order. Perhaps try this ordering:
        k (a :: k)
    • In the compiler-generated class method signature:
        f :: forall a k. C (a :: k) => P a

  This is the same message as one would get if this signature was written by hand.
  The implementation may opt to provide a different error message in the same spirit.

* Erroneous class method header that uses (as of yet) unsupported visible ``forall`` in terms::

    class C (a :: k) where
      f @k a :: P a

    -- f :: forall k. forall a -> C (a :: k) => P a

  Rejected with the following message::

    • Illegal visible, dependent quantification in the type of a term:
        forall k. forall (a :: k) -> C a => P a
      (GHC does not yet support this)
    • In the compiler-generated class method signature:
        f :: forall k. forall a -> C (a :: k) => P a

  This is the same message as one would get if this signature was written by hand.
  The implementation may opt to provide a different error message in the same spirit.

* Instance declaration where the method definition binds class variables::

    class C a b where
      f :: a -> b

    -- f :: forall a b. C a b => a -> b

    instance C Int Bool where
      f @Int @Bool = even

* Instance declaration where the method definition binds some, but not all of class variables::

    class C a b where
      f @a @b :: a -> b

    -- f :: forall a b. C a b => a -> b

    instance C Int Bool where
      f @Int = even

* Instance declaration where the method definition binds class variables in a different order::

    class C a b where
      f @b @a :: a -> b

    -- f :: forall b a. C a b => a -> b

    instance C Int Bool where
      f @Bool @Int = even

* Erroneous instance declaration where the class variable bound in a method definiton does not match::

    class C a where
      f @a :: a

    instance C Int where
      f @Bool = ...

  Rejected with the following message::

    • Type indexes must match class instance head
      Expected: f @Int
        Actual: f @Bool
    • In the method definition for ‘f’
      In the instance declaration for ‘C Int’

  This is the same message as one would get for associated types.
  The implementation may opt to provide a different error message in the same spirit.

Effect and Interactions
-----------------------

The immediate pay-off of this change is that users get the ability to specify
the ordering of class variable quantification in class methods for use with
``-XTypeApplications``.

The long-term pay-off is that it offers syntax for visible quantification of
class variables and represents one of the steps in the `Grand Class Unification
<https://github.com/ghc-proposals/ghc-proposals/pull/236>`_  plan.


Visible ``forall`` Extended Specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When/if visible ``forall`` in terms is allowed, class method definitions in
instance declarations must explicitly write out visible class variable
instantiations::

  class C a where
    type F a :: a
    f a :: a

  -- f :: forall a -> C a => a

  instance C Bool where
    type F Bool = True
    f Bool = True

That is, we write ``f Bool = ...`` rather than ``f = ...``. These bindings
are a part of the left-hand side and cannot be lambda-bound::

  instance C Bool where
    f Bool = ...         -- correct
    f = \ @Bool -> ...   -- error: the visible class variable is not bound on the left-hand side

Limitations
-----------

There are some variable orderings that may be desirable by API authors but are
not allowed by the proposal::

  class C a b where
    m @c @a @b :: c -> a -> b

  -- intended outcome:   m :: forall c a b. C a b => c -> a -> b
  -- actual outcome:  error, 'c' is not a class variable

Note that here, a non-class variable ``c`` is supposed to go in front of class
variables ``a`` and ``b``. Why is this disallowed?

The proposal is deliberately conservative. There are a few things that headers
of associated types can do, but the proposed class method headers cannot:

1. Binding non-class variables::

    class C a where
      type F a b  :: blah -- ok
      f a b :: blah  -- error: 'b' is not a class variable

2. Not binding all of class variables::

    class C a b where
      type F a :: blah -- ok
      f a :: blah  -- error: 'b' is not bound

3. Adding inline kind annotations on binders::

    class C a where
      type F (a :: k) :: blah  -- ok
      f (a :: k) :: blah  -- syntax error

The proposal as written does not exclude the possibility of giving reasonable
semantics to these forms in the future. However, it is not clear if we should
follow the footsteps of associated types and allow all of these in terms, or if
we should deprecate these forms in types.

Lifting these restrictions would have consequences that were not fully evaluated:

1. Binding non-class variables would violate the principle that the part of the
   signature to the right of ``::`` defines the field type in the dictionary.
   Consider::

     class C a b where
       f :: fsig
       g :: gsig

   We can rewrite this as a record type::

     data CDict a b = MkCDict
       { f :: fsig,
         g :: gsig }

   Note that the field types ``fsig`` and ``gsig`` correspond exactly to the
   class method types. Reordering or changing the visibility of class
   variables maintains this property, while adding new variables does not.

2. Not binding all of class variables would be akin to a violation of the
   "forall-or-nothing" rule. We can say that unmentioned class variables are
   bound implicitly, but then we have to say where exactly: before or after the
   explicitly bound variables? ::

     class C a b where
       f @b :: blah

     -- is it   f :: forall a b. blah  ?
     --    or   f :: forall b a. blah  ?

   Or do we, perhaps, interleave these variables in a way that would make the
   signature well-scoped? ::

     class C (a :: k) (b :: j) where
       f @a @j :: blah

     -- f :: forall k (a :: k) j (b :: j) :: blah

   This is an intricate design question which we do not have to answer now (or, perhaps, at all).

3. Adding inline kind annotations on binders is plausible, but it would
   complicate the specification and the implementation with details about
   pattern-like meta-variables in these kind annotations. Basically, this is
   left as future work.

Costs and Drawbacks
-------------------

This is one more feature to implement and support.


Alternatives
------------

`Top-level signatures
<https://github.com/ghc-proposals/ghc-proposals/pull/148>`_ (not to be confused
with top-level kind signatures) is a different take on this issue.


Unresolved Questions
--------------------

None at the moment.

Implementation Plan
-------------------

I (Vladislav Zavialov) will implement.
