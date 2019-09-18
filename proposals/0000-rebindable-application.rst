Rebindable Application
======================

.. author:: Mac Malone
.. date-accepted::
.. proposal-number::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/275>`_.
.. sectnum::
.. contents::

While most of GHC's syntax is in some way customizable
(ex. through overloaded type classes and/or ``RebindableSyntax``),
function application's juxtaposition syntax is not.
I propose to change that.

Motivation
----------

In Haskell, there are a lot of types that behave like functions.
These types thus have there own unique function application operations.
For example, the ``Functor`` class has ``<$>`` which, to quote the
documentation, is "function application lifted over a Functor".

As such, it would be nice if types other than the function arrow
(``->``) could be applied using the standard syntax.
For instance, if function application's juxtaposition syntax
worked on ``Applicative``, this would allow code like:

.. code-block:: haskell

  f = g <$> a <*> b <*> c

To be written like:

.. code-block:: haskell

  f = g a b c

And if it worked on ``Exp`` (from Template Haskell), code like this:

.. code-block:: haskell

  mapE f xs = ConE 'map `AppE` f `AppE` xs

Could be written like:

.. code-block:: haskell

  mapE f xs = ConE 'map f xs

Proposed Change Specification
-----------------------------

I propose that the juxtaposition syntax for application
(i.e. ``f a``) essentially becomes syntactic sugar for ``(f $# a)``
with the caveat that the applications in ``(f $# a)`` are not further
desugared, as that would cause an infinite loop.

By default, this uses a new ``$#`` operator found in ``GHC.Exts``.
However, if the ``RebindableApplication`` extension is turned on, it
uses the ``$#`` from the current scope.

The ``S#`` Operator
^^^^^^^^^^^^^^^^^^^

The ``S#`` operator is designed to represent standard function
application, thus it would ideally have the following signature:

.. code-block:: haskell

  infixr 0 $#
  ($#) :: forall r1 r2 (a :: TYPE r2) (b:: TYPE r2). (a -> b) -> a -> b

However, due to the restrictions of levity polymorphism, a function
cannot have this signature. As such, ``$#`` would have to be magic,
much like unboxed tuple constructors (ex. ``(#,,#)``).

The magic ``$#`` comes with two rules:

1. ``$#`` cannot be used unsaturated (i.e. one can not write ``($) = ($#)``).
   This would make ``$#`` a function, which it is not.
2. When ``$#`` is applied, the application is not further desugared.
   This means ``a $# b`` does not become ``($#) $# a $# b`` -- which would
   be invalid anyway due to rule 1.

Rebinding ``$#``
^^^^^^^^^^^^^^^^

Since ``$#`` is magic, rebindings of ``$#`` should be magic as well.
As such, when ``RebindableApplication`` is enabled, they are.
The binding is allowed to be fully levity polymorphic. The new ``$#``
is unable to be used unsaturated and its applications are not further
desugared.
This means that top-level declarations of ``$#`` can not be exported from
normal modules (as they can't be compiled into functions) and are thus
automatically excluded from module export lists.

Examples
--------

With this proposal, we could rebind ``$#`` like so:

.. code-block:: haskell

  import qualified GHC.Exts as GHC

  class Applicable a b r where
    apply :: a -> b -> r

  instance (b' ~ b) => Applicable (a -> b) a b' where
    apply = a GHC.$# b

  instance (b' ~ f b, Functor f) => Applicable (a -> b) (f a) b' where
    apply = (<$>)

  instance (b' ~ f b, Applicative f) => Applicable (f (a -> b)) (f a) b' where
    apply = (<*>)

  instance Applicable Exp Exp Exp where
    apply = AppE

  f $# a = apply f a

This would enable the simplified examples shown in the Motivation.


Effect and Interactions
-----------------------

This proposal allows different application functions to
all share the same syntax, which I would argue allows users to write
more concise (and, to a certain extent, clearer) code.

It also has the interesting effect of making application more
first-class syntactically (according to the definition Dijkstra outlined
`here <http://www.the-magus.in/Publications/ewd.pdf>`_).
It now has a explicit operator (i.e ``$#``) for which the juxtaposition
notation is merely syntactic sugar.

``TypeApplications``
^^^^^^^^^^^^^^^^^^^^

With the ``TypeApplications`` extension, functions can have types
applied to them. This proposal does not overloaded this kind of application.
However, there is still a question as to whether the proposed
desugaring could negatively impact ``TypeApplications``.

I propose that code like:

.. code-block:: haskell

  f @Int @Char a b

desugars to:

.. code-block:: haskell

  f @Int @Char $# a $# b

I think this should work as desired.


Costs and Drawbacks
-------------------

The proposed ``$#`` operator is very magical. Magic is generally not good.
However, this magic is not a desire of the proposal, but rather a necessary
consequence of current restrictions of levity polymorphism.
Were these restrictions ever lifted, ``$#`` could become significantly less
magic, with the only likely requirement being that applications of ``$#``
not be further desugared.

As such, I believe that the magic of ``$#`` should not work against the
proposal at conceptual level. It may however work against it at an
implementation level, depending on its difficulty in practice.

There is also some magic in the ``$#`` rebindings, as it is not particular
clear from a plain reading that the rebinding would effect the juxtaposition
syntax. This is helped somewhat by it being locked behind an extension, but
an alternative were the rebinding is more obvious may be preferred.

Alternatives
------------

There are a number of possible alternatives.

Do Nothing
^^^^^^^^^^

We can always do nothing.

This would require us to still use application operators like ``(<$>)`` to
perform application on types outside the function arrow ``(->)``.
It also leave us without an explicit function application operator.
While ``$`` generally suffices, it does not work in primitive code.
For example, the following is possible with ``$#`` but not with ``$``:

.. code-block:: haskell

  peekWord16LE# addr# = W16# $#
    uncheckedShiftL# (indexWord8OffAddr# addr# 1#) 8# `or#`
    indexWord8OffAddr# addr# 0#

Instead, to implement this in current Haskell, unsightly parentheses
are necessary.

No ``$#``
^^^^^^^^^

Instead of implementing ``$#``, we could conceivably have ``f a`` desugar to
``f $ a`` instead. As ``$`` is not (and can not be) fully levity polymorphic,
this would require the desugaring to only happen when ``RebindableApplication``
is enabled (as it otherwise would break code with primitive operations).
It would also mean that there is no way to recover the original behavior
in modules with ``RebindableApplication``.  As such, a more tightly scoped
approach would likely be desired.

Local Only
^^^^^^^^^^

Instead of allowing module wide function application rebindings,
rebindings could be restricted to some local scope (ex. to a
``let`` expression).

Idiom Brackets
^^^^^^^^^^^^^^

We could also use a bracket syntax to restrict the scoped of the rebinding.
For example:

.. code-block:: haskell

  (| ConE 'map f xs |)

The code within the brackets could then use whatever ``$#`` (or ``$``)
was in scope.


Unresolved Questions
--------------------

Does ``$#`` need more?
^^^^^^^^^^^^^^^^^^^^^^

I have suggested that the juxtaposition syntax always be desugared to ``$#``,
as the idea is for ``$#`` to have a one-to-one correspondence with the existing
function application syntax. I am not sure if the operator (as currently
proposed) actually has this correspondence.
If it does not, then it will need to be adjusted so it does.
If it cannot be so adjusted, then my proposal has a problem.
One quick fix is to desugar application in the proposed way only when
``RebindableApplication`` is enabled.
However, it would still be impossible to recover the original
behavior in the ``RebindableApplication`` module.
Thus, should this be the case, it might be a good reason to consider one of the
local alternatives described above.

``RankNTypes``
^^^^^^^^^^^^^^

I am not sure as to whether ``$#`` (as currently proposed) works
with functions of higher ranks. If it does, great. If it does not,
then it is my hope it can be modified to do so. If it can't be,
then the same problem as discussed above emerges.

Partially Saturated ``$#``
^^^^^^^^^^^^^^^^^^^^^^^^^^

I do not know enough about the details of levity polymorphism to know if
``$#`` can be used partially saturated. I believe that it can be, but if
not, it can be required that ``$#`` only be used fully saturated.

Naming
^^^^^^

All the names in the proposal (i.e. ``RebindableApplication`` and ``$#``)
could be changed if desired.


Implementation Plan
-------------------

**TBD**

Depending on the anticipated difficulty (hacking on GHC is rather new to me),
I could potentially volunteer to implement this. However, given some of the
magic involved, it might be wiser to have someone with more experience
handle it.
