Overloaded Application
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

While most of GHC's syntax is in some way customizable (ex. through overloaded
type classes or ``RebindableSyntax``), function application is not.
I propose to change that.

Motivation
----------

In Haskell, there are a lot of types that behave like functions and thus
have there own unique function application operation.
For example, the ``Functor`` class has ``<$>`` which, to quote the
documentation, is "function application lifted over a Functor".

As such, it would be nice if types other than the function arrow
(``->``) could be applied using the standard syntax.
For instance, if function application worked on ``Applicative``,
this would allow code like:

.. code-block:: haskell

  foo = m <$> a <*> b <*> c

To be written like:

.. code-block:: haskell

  foo = m a b c

And if function application work on ``Exp`` (from Template Haskell), code like
this:

.. code-block:: haskell

  mapE f xs = ConE 'map `AppE` f `AppE` xs

Could be written like:

.. code-block:: haskell

  mapE f xs = ConE 'map f xs

Proposed Change Specification
-----------------------------

When the ``OverloadedApplication`` extension is turned on, application
(i.e. ``f a``) would then essentially become syntactic sugar for ``apply f a``
with the caveat that ``apply f a`` can not be further desugared, as that would
cause an infinite loop.

If ``RebindableSyntax`` is enabled, the apply function currently in scope would
be used.

Otherwise, I propose ``apply`` comes from a new module
``GHC.OverloadedApplication`` where the following class is defined:

.. code-block:: haskell

  class Applicable (f :: TYPE r) (a :: TYPE r1) (b :: TYPE r2) where
    apply :: f -> a -> b

This class would come with the following instance:

.. code-block:: haskell

  instance Applicable ((a :: TYPE r1) -> (b :: TYPE r2)) a b where
    apply = ($#)

This instance requires a function application operation that is
levity polymorphic in both argument and result, which unfortunately the
currently existing ``($)`` is not.

As such, I propose introducing ``($#)``, a primitive function application
operation with the following signature:

.. code-block:: haskell

  infixr 0 $#
  ($#) :: forall r r1 r2. TYPE r1 -> TYPE r2 -> TYPE r

Examples
--------

The ``Applicable`` class could then be expanded by the user to add instances
like:

.. code-block:: haskell

  instance Functor f => Applicable (a -> b) (f a) (f b) where
    apply = (<$>)

  instance Applicative f => Applicable (f (a -> b)) (f a) (f b) where
    apply = (<*>)

  instance Applicable Exp Exp Exp where
    apply = AppE

which would enable the simplified examples shown in the Motivation.


Effect and Interactions
-----------------------

With this extension, function application can now be overloaded,
allowing users to write more concise (and, to certain extent, clearer)
code.

``TypeApplications``
^^^^^^^^^^^^^^^^^^^^

With the ``TypeApplications`` extension, functions can have types
applied to them.
This proposal does not overloaded this kind of application.
However, there is still a question as to whether the proposed
desugaring could negatively impact ``TypeApplications``.

I propose that code like:

.. code-block:: haskell

  foo @Int @Char a b

desugars to:

.. code-block:: haskell

  apply (apply (foo @Int @Char) b) c

which I believe will still work as desired.


Costs and Drawbacks
-------------------

Given that the changes proposed are hidden behind an extension that even when
enabled is a no-op without user overloads, I do not imagine it would negatively
effect new haskell users.
For users who are interested, I think working with it would come rather
naturally as it is simply syntactic sugar for existing approaches.

Alternatives
------------

Alternatively, we could do nothing and still have to use application operators
like ``(<$>)`` to perform application on types outside the function arrow
``(->)``.


Unresolved Questions
--------------------

Naming
^^^^^^

All the names in the proposal (i.e. for the extension, the class, the method,
and the primitive operation) could be changed if desired.
I personally think it would be nice if ``apply`` was instead an operator like
``($)``, but I could not think of a good symbol to use.

Provided Instances
^^^^^^^^^^^^^^^^^^

There is a question of what instances should be provided for ``Applicable``
by GHC.
I have proposed just an instance for the function arrow ``(->)``.
However, it could come with ``Functor`` and ``Applicative`` instances as well
(possibly along with others that I had not yet considered).

Implementation Plan
-------------------

TBD
