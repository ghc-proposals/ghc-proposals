Simplifying static forms
==============

.. author:: Simon Peyton Jones
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/732>`_.
.. sectnum::
.. contents::

This proposal simplifies the typing rules for ``static`` forms. The current design
is too elaborate and the implementation never worke.d


Motivation
----------

The ``static`` form in GHC is `described in GHC's user manual <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/static_pointers.html>`_. It allows you to define a "static pointer", of type ``StaticPtr a``.

In the form ``(static e)`` the intuitive idea is that ``e`` is "pure code", with no free variables;
that is, all ``e``'s free variables are bound at top level.  For example::

  y :: String
  y = "hello" ++ " Fred"

  f x = ....(static (reverse y))...

But GHC also allows this::

  f x = let { y = "hello" ++ " Fred" } in static (reverse y)

fHere `y` is bound nested-ly, but is morally top-level because it in turn has no
free variables. This flexibility is `described in the user manual <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/static_pointers.html#using-static-pointers>`_.

However, it turns out that implementing this apparently-simple extra expressiveness
is ridiculously hard to implement, leading to a series of bug reports (e.g. `#26545 <https://gitlab.haskell.org/ghc/ghc/-/issues/26545>`_ `#24464 <https://gitlab.haskell.org/ghc/ghc/-/issues/24464>`_ `#24773 <https://gitlab.haskell.org/ghc/ghc/-/issues/24773>`_).

**Complication 1**.  The expression ``e`` in ``static e``
might generate constraints.  We don't want to solve those from
locally-bound Givens, because they'll be out of scope when we promote to top level.  For example::

  f :: (Read a) => a -> StaticPtr a
  f x = static (read @a "hello")

Here we use the ``Read`` dictionary passed to ``f`` to parse the string. Not allowed!  The body of
the ``static`` is not just code: it mentions a dynamically bound (albeit invisible) parameter to ``f``.

Solution: wrap the constraints in an implication with ``SkolInfo`` of ``StaticFormSkol``; and
in the constraint solver zap all Givens when walking inside such an implication. That was
done in::

  commit 39d4a24beaa7874a69ffdc1528ca160818829169
  Author: Simon Peyton Jones <simon.peytonjones@gmail.com>
  Date:   Tue Sep 30 23:11:19 2025 +0100

    Build implication for constraints from (static e)

    This commit addresses #26466, by buiding an implication for the
    constraints arising from a (static e) form.  The implication has
    a special ic_info field of StaticFormSkol, which tells the constraint
    solver to use an empty set of Givens.

So that complication was unwelcome, but not too bad.

**Complication 2**.  What if we have::

  f x = let y = reverse "hello" in
        ...(static (y++y))...

The free variables of the static are just ``{y}``, and ``y`` is "morally-top-level": its right-hand side
has no free variables.
Sadly (as it turns out) GHC tries to accept this case.  When looking at the defn of y
(with no static in sight yet) the typechecker marks it at a "static binding", meaning that
it too can (and indeed must) be floated to top level.
So if the desugarer moves the static to the top level, it must move ``y`` too.  And that means it must mark the typechecked binding in some way, so the desugarer can identify it.

Not so hard, but there is quite a bit of new plumbing.

**Complication 3.**  But what if ``y``'s RHS generates constraints, which use Givens (or solved
dictionaries, which are very similar) from its context?  E.g.::

  f x = let p = x+1::Int; y = 2+3::Int in ...

Now there may be a ``d :: Num Int`` lying around from dealing with ``p``, and ``y`` may use it.
Oh no! Now that'll be out of scope if we move ``y`` to top level.

The same would happens as in Complication 1 if ``y``'s RHS used a dictionary bound by ``f``.

Plausible solution: use them same mechanism for static **bindings** as we did for ``static e``
**expressions**.  That is, build an implication constraint whose ``SkolInfo`` says "zap Givens".
This turned out to be considerably harder to implement than it was for Complication 1.
But I did it.

**Complication 4.**  What if ``y`` is not generalised, perhaps because of the Monomorphism
Restriction?  E.g.::

  f :: Num a => a -> blah
  f x = let y = 3+3 in (x+y, static( ..y.. ))

Now ``y`` is monomorphic and really does use the dictionary passed to ``f``.
So it really *cannot* appear in the static. Somehow ``y`` really isn't static after all,
despite its lack of free variables.

*We must reject this program*.

But that is far from obvious.  This slightly different program might be fine::

  f :: Num a => a -> blah
  f x = let y = 3+3 in ((3::Int)+y, static( ..y.. ))

Here we eventually figure out that ``y::Int`` so maybe it is static after all.
In implementation terms this would be harder still; indeed, I'm not sure how to do it,
given GHC's current architecture.

**Conclusion**.

* The apparently-simple idea of allowing nested bindings to appear in `static` if they
  are in turn static, turns out to be a major implementationn swamp (Complications 1,2,3).
  Lots of code and subtle explanations are required ... to achieve very little.

* It's not just the implementation. It is extremely difficult to explain to the programmer
  *precisely* which uses of static are OK and which are not.
  Any such explanation would required understanding of the Monomorphism Restriction, *and*
  predicting how the monomophism resolves (e.g. in Complication 4, if `y` ultimately turns
  out to have type ``Int``, the ``y`` can be static after all.

``static`` is not a heavily used feature.  It is the tail that is wagging the dog.
This proposal simplifies the specification (losing a little bit of expressiveness but
not much), allowing a much, much simpler implementation.

Proposed Change Specification
-----------------------------

I propose the following change, which returns to the original spec of `static e`:

* **In an expression `static e`, the free term variables of `e` must all be bound at top level.**

That is the complete specification.



Proposed Library Change Specification
-------------------------------------

No changes to libraries.

Examples
--------

Here are some examples::

  f1 x = static x         -- Not OK: x is not bound at top level

  f2 x = let y = x+1 in   -- Not OK: y is not bound at top level
         static (y+1)

  f3 x = let y = "hello" in   -- Not OK: y is not bound at top level
         static (y ++ y)

  z = "hello"
  f3 x = static (z ++ z)      -- OK: z is bound at top level

  f4 x = static (let z = "hello" in z ++ z)      -- OK: no free variables

The only real loss is that a top-level binding has a rather large scope.
But you can always put the binding *inside* the ``static`` as ``f4`` shows.

Effect and Interactions
-----------------------

I don't think there are any interactions.


Costs and Drawbacks
-------------------

From a design point of view this proposal is a clear win.



Backward Compatibility
----------------------

But there may be some back-compat issues. Perhaps existing libraries rely on using
nested let-bindings in ``static``.  I asked some key players:

* Laurent Rene de Cotret says *"I stand behind your proposal. As you mention, this will bring the behavior in line with the documented one. I'm happy to support Cloud Haskell users in transitioning when the time comes."*
* Mathieu Boespflug says *"This sounds reasonable to me. Simple is better."

Alternatives
------------
The status quo is not really an alternative: we have bugs that I don't know how to fix.
I suppose that with yet more effort we could do something that never crashed the compiler;
but was merely hard to explain to the user.  But I think a more likely outcome is that
we'll just have un-fixed bugs, and a hard-to-understand implementation, indefinitely.


Unresolved Questions
--------------------
None

Implementation Plan
-------------------
I will implement it.  Indeed I have mostly done so.


