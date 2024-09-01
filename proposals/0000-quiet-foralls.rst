**********************
Quiet forall binders
**********************

.. author:: Richard Eisenberg and Simon Peyton Jones
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
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
case for the usefulness of this feature.  Note in particular tha the scoped
variables arising from type signatures (using ``-XExetendedForAllScope``) are irrelevant here.

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

Thus we have the unsatisfactory situation that in a call to ``f2`` there is no convenient way
to bring the ``k`` binder into scope in the argument.  That is very unfortunate: all the examples in GHC Proposal 425 could be reframed using inferred variables (which the user is allowed to write; e.g. see the type of ``f2``).

Moreover, this matters in practice.  See GHC ticket `#20815 <https://gitlab.haskell.org/ghc/ghc/-/issues/20815>`_ and merge requests `!13190 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/13190>`_ and `!13187 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/13187>`_, where the ``deriving`` mechanism generates
a binding that GHC cannot typecheck because of the inability to bind kind variables.

Side note
----------
You might wonder if we could use a pattern signature, thus ::

  f2 (\ @(a :: k) -> blah)``

thereby bringing both ``a`` and ``k`` into scope. But

* The says that using a pattern signature is unnsatisfactory here.
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


Proposed change specification
=================================

We propose to separate the two aspects of inferred foralls described above,
by having *four* (instaed of three) forms for forall-binders:

* ``Required``, written ``forall a -> type``, exactly as Required forall-binders today.
* ``Specified``, written ``forall a. type``, exactly as Specified forall-binders today.
* ``Infrerred``, written ``forall {{k}. type``, exactly as Inferred forall-binders today.
* ``Quiet``, written ``forall [k}. type``, is new in this proposal.

A Quiet variable is very like a Specified one (fully under user control) but differs in exactly one way: it is omitted in type applications. More precisely:

* You can have a type lambda for Required, Specified, or Quiet type variables.

  * For Quiet foralls, the type lambda has curly braces ``@{k}``.

* Similarly in a type or class declaration, you can bind a Quiet variable, as well
  as a Specified or Required one.

* In an application,

  * You must give a Required type arugment e.g. ``g1 Int``
  * You may give a Specified type argument (with ``-XTypeApplications``). e.g ``g2 @Int``
  * You may not give a Quiet of Inferred type argument.

* Users cannot write Inferred foralls.  GHC infers them (see ``h`` above), but the user cannot write them.  So the suface yyntax of types is unchanged.


Syntax changes
----------------

Type-lambdas, and data type declarations, can have curly braces.  (Vlad can you help us
make that precise?.)

Extension flags and back-compat
---------------------------------

We do not propose to add a new extension flag; rather just to extend
what ``-XTypeAbstractions`` does.  Old programs witll continue to
compile.


Effects and interactions
==========================

There is something distressingly ad-hoc about this proposal.  But

* It fits into a framework we already have, by by adding one more to our current
  list of Required, Specified, and Inferred vaiables.
* Perhaps a nicer story in type applications would be to have named type arguments, but that would be a much bigger chaage.


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
The proposal is easy to implement.

