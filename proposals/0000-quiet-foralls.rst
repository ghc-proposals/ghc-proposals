**********************
Quiet forall binders
**********************

.. author:: Richard Eisenberg and Simon Peyton Jones
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/675>`_.
.. sectnum::
.. contents::

We propose to extend the syntax of forall-types to allow
a type-lambda to bind an "inferred" type variable, not just
just a "specified" one.

This proposal was developed during a three-day walk in the Italian
Alps, at 2500m.

Motivation and background
===========================

`GHC Proposal 448 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0448-type-variable-scoping.rst#type-arguments-in-lambda-patterns>`_ introduced type lambdas into Haskell's source syntax.
using extension ``-XTypeAbstractions``.  For example, given a function ::

  f1 :: (forall a. a -> a) -> Int

we can write the following call ::

  f1 (\ @b -> \(x::b) -> x)

Here the type-lambda ``\ @b ->`` binds the type variable ``b`` in the body of the lambda,
just as the term-lambda ``\(x::b) ->`` binds ``x`` in the body.  Proposal 425 makes a good
case for the usefulness of this feature.  Note in particular that the scoped
variables arising from type signatures (using ``-XExtendedForAllScope``) are irrelevant here.

Now supppose that the functions's type was ::

  f2 :: (forall {k}. forall (a::k). Proxy a -> Int) -> Int

Now, at a call site we can bind the *type* variable (using a type lambda) but not the *kind* variable.  That is, we can write ::

  f2 (\ @a -> blah)

but we *cannot* write ::

  f2 (\ @k -> \ @(a::k) -> blah)

Why not?  Because in the type signature ``forall {k}. forall (a::k). Proxy a -> Int``, the
``forall {k}`` binds an *inferred* variable ``k``.

Inferred forall-binders
------------------------

Inferred forall-binders have two properties:

* ``TypeApplications``  You can't use a type application for an inferred forall.  Thus if ::

     g :: forall {k}. forall (a::k). blah

  then, using ``-XTypeApplications``, you may write the call ``g @Int``.  But you may *not* write the call ``g @Type @Int``.
  The motivation is that it would be tiresome to be forced to write the kind argument
  (even with an underscore, thus ``g @_ @Int``) because it is
  implied by the type argument.

* ``TypeAbstractions`` You can't use an explicit type abstraction (aka, a type lambda) for an
  inferred forall.  Here the motivation is a bit less clear.  Why shouldn't you be able to write this?  ::

    f2 (\ @{k} -> \ @(a::k) -> blah)

  Here the curly braces in the ``\ @(k} ->`` signal that the lambda is for an inferred variable.
  This looks plausible but it is rejected in GHC, and for good reason.   Consider ::

    h :: forall a b. Proxy a -> Proxy b -> Int

  Then ``h``'s real (fully elaborated) type is ::

     h :: forall {k1} {k2}. forall (a::k1) (b::k2). Proxy @k1 a -> Proxy @k2 b -> int

  But does ``k1`` precede ``k2``, or vice versa?  The order would make a difference if we tried
  to define ``h`` thus ::

     h = \ @{k1} -> \ @{k2} -> \ @(a::k1) -> \ @(b::k2) -> blah

  Now the order of the k1/k2 foralls clearly matters.  But GHC, not the user, has chosen that order, and there is no simple algorithm for specifying what order it should choose.

  That is why the current design does not allow the ``\ @{k} ->`` as a type abstraction.

Thus we have the unsatisfactory situation that in a call to ``f2`` there is no convenient way
to bring the ``k`` binder into scope in the argument.  That is very unfortunate: all the examples in GHC Proposal 425 could be reframed using inferred variables (which the user is allowed to write; e.g. see the type of ``f2``).

Moreover, this matters in practice.  See GHC ticket `#20815 <https://gitlab.haskell.org/ghc/ghc/-/issues/20815>`_ and merge requests `!13190 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/13190>`_ and `!13187 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/13187>`_, where the ``deriving`` mechanism generates
a binding that GHC cannot typecheck because of the inability to bind kind variables.

Side note
----------
You might wonder if we could use a pattern signature, thus ::

  f2 (\ @(a :: k) -> blah)

thereby bringing both ``a`` and ``k`` into scope. But

* Pattern signatures contradict both the `Explicit Variable Principle <https://github.com/ghc-proposals/ghc-proposals/blob/master/principles.rst#212explicit-variable-principle-evp>`_  and the `Explicit Binding Principle <https://github.com/ghc-proposals/ghc-proposals/blob/master/principles.rst#222explicit-binding-principle-ebp>`_.
* Using a pattern signature to bring a variable into scope is very indirect, because we have to write out the (perhaps complicated) kind of `a` do so.
* It might be impossible to use a pattern signature if ``k`` was mentioned only in the result type, rather than in the kind of another forall-bound varaiable.
* All the variables might be inferred!  Consider ::

     f3 :: (forall {a}. Proxy a) -> Int

  Now we can't us a type-lambda to bring ``a`` into scope in the argument of a call.
  We could perhaps use a *term* lambda and a pattern signature, but if that is the
  recommended plan we should recommend it instead of type-lambdas too!  Plus it forces
  eta expansion which has other consequences.

Data type declaratations
---------------------------

Exactly the same discussion applies to data type declarations.
At the moment the following program is rejected ::

  type T :: forall {k}. k -> Type
  data T @{k} a = MkT (Proxy @k a)

But it makes perfect sense, it may in some cases be tiresome not to be able to bind ``k``
explicitly.

Conclusion
-----------

The frustrating thing about all this is that in the types of both ``f2``
and ``f3``, and the kind of ``T``, there is absolutely no doubt what order
any inferred forall's come in: they are explicilty specified by the
user.  There is nothing "inferred" about them!

This has come up before.  `GHC ticket #22648 <https://gitlab.haskell.org/ghc/ghc/-/issues/22648>`_
shows that the order of inferred variables is actually observable.   The ticket says "Interestingly enough, the type variables ``{a}`` and ``{b}`` are "inferred" only de jure; in the actual program, they are very much specified by an explicit forall. In other words, their order is determined by the source code, not by implementation details of the compiler."


Proposed change specification
=================================

We propose to separate the two aspects of inferred foralls described above,
by having *four* (instead of three) forms for forall-binders:

* ``Required``.  User-accessible,  written ``forall a -> type``. Can be bound and applied visibly, with no ``@`` decorations.
* ``Specified``.  User-accessible, written ``forall a. type``.  Can be bound and applied with an ``@`` decoration.
* ``Quiet``. User-accessible, written ``forall {k}. type``.  Can be bound and applied with an ``@{}`` decoration.
* ``Inferred``. Not user-accessible, printed ``forall {{k}}. type``.  Cannot be bound or applied.

The changes compared to today are:

* We add Quiet foralls.  A Quiet forall is very like a Specified one (it is fully under user control):

  * You can have a type lambda for a Quiet type variable, as well for a Required or Specified one.  (But, as now, not for an Inferred one.) For Quiet foralls, the type lambda has curly braces ``@{k}``.
  * Similarly in a type or class declaration, you can bind a Quiet variable, as well
    as a Specified or Required one.

  * In an application, you can provide a Quiet type argument (with ``-XTypeApplications``), using curly braces e.g. ``g3 @{Int}``.  So:

    * You *must* give a Required type argument e.g. ``g1 Int``
    * You *may* give a Specified type argument e.g. ``g2 @Int``
    * You *may* give a Quiet type argument e.g. ``g3 @{Int}``
    * You *must not* give an Inferred type argument.

  For example, if ``f :: forall {k}. forall (a::k). Proxy a -> Int`` we could call it in any of the following ways ::

      f Proxy
      f @Int Proxy
      f @{Type} @Int Proxy
      f @{Type} Proxy

* **Users cannot write Inferred foralls**.  GHC infers them (see function ``h`` above), but the user cannot write them.

* Inferred foralls should be avoided in error messages, as they are today.  The ``forall {{k}}. t`` syntax is to allow us to talk about them, and to allow GHC to print them when absolutely necessary.  (For example during compiler debugging.)

* The suface syntax of user-written types is unchanged. However, the syntax ``forall {k}. t`` now denotes a Quiet forall, rather than (in GHC today) an Inferred one.

* There are no changes at all to Required and Specified foralls.


Syntax changes
----------------

* Type-lambdas, and data type declarations, can have curly braces, thus

  * ``\ @{k} -> e``
  * ``data T @{k} a = ...``

* Type applications can have curly braces, e.g. ``f @{type}``.

(Vlad can you help us make that precise?.)


Extension flags and back-compat
---------------------------------

We do not propose to add a new extension flag; rather just to extend
what ``-XTypeAbstractions`` does.  Old programs witll continue to
compile.


Discusion, effects and interactions
==========================

There is something distressingly ad-hoc about this proposal.  But

* It fits into a framework we already have, by adding one more to our current
  list of Required, Specified, and Inferred variables. This list is *already* ad-hoc. It would
  be much simpler to just have Required foralls and nothing else -- but then all type arguments
  would be compulsory, and no one would want to use the language.  It's all about using
  perhaps-ad-hoc mechanisms to make programming convenient.  This proposal just fills out a
  missing corner of the design space.

* Perhaps a nicer story in type applications would be to have named type arguments, but that would be a much bigger change.

#22648 Order of inferred variables is observable
---------------------------------------------------

The proposal solves at least the first (term-level part of `GHC ticket #22648 <https://gitlab.haskell.org/ghc/ghc/-/issues/22648>`_.  The example there is ::

  const_inf1 :: () -> forall {a} {b}. a -> b -> a
  const_inf2 :: () -> forall {b} {a}. a -> b -> a

  const_inf1 _ x _ = x
  const_inf2 _ x _ = x

  const_spec :: () -> forall a b. a -> b -> a
  const_spec = const_inf1

If you replace ``const_inf1`` by ``const_inf2`` in the RHS of ``cons_spec``, the program
is rejected. The order of the inferred variables is observable.

Under this proposal, the above signatures use Quiet foralls, and there is no claim that their
order is irrelevant -- indeed it's a *goal* that their order is observable.  On the other hand
the user cannot write Inferred forall-binders (whose order is irrelevant); only GHC can.

The type-level part of the ticket is a bit better, but not solved ::

  data D a b
    -- GHC might infer
    --     D :: forall {{k1}} {{k2}}. k1 -> k2 -> Type
    -- or
    --     D :: forall {{k2}} {{k1}}. k1 -> k2 -> Type

  type family F :: forall k1 k2. k1 -> k2 -> Type
  type instance F = D

(Here we are using the new notation for Inferred variables.   The trouble here is that
the instance for ``F`` might be accepted with one GHC-chosen ordering, but rejected
with the other.  A solution to this part might to make ``forall {{k}}. ty`` un-equal
to ``forall k. ty``.   See `GHC Proposal #558 <https://github.com/ghc-proposals/ghc-proposals/issues/558>`_ and `this GHC commit <https://gitlab.haskell.org/ghc/ghc/-/commit/cf86f3ece835ecb389d73760c1d757622c084f0f>`_.

Backward Compatibility
----------------------

The proposal is fully backward compatible.

Alternatives
------------

Do nothing.

Unresolved Questions
--------------------


Implementation Plan
-------------------

The proposers believe that the proposal could be implemented in a day's work.

