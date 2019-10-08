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
the function application syntax ``f a`` (i.e. the juxtaposition syntax)
is not. I propose to change that.

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

  mapE f xs = VarE 'map `AppE` f `AppE` xs

Could be written like:

.. code-block:: haskell

  mapE f xs = VarE 'map f xs

Proposed Change Specification
-----------------------------

I propose a new extension called ``RebindableApplication``. When this
extension is turned on, the juxtaposition syntax for function application
``f a`` becomes syntactic sugar for ``f $ a``, where ``$`` is whatever ``$``
is currently in scope. Operator application remains the  same (ex. ``f $ a``
is not further desugared). To clarify these changes, the table below lists
my proposed desugaring for each kind of application syntax.

+-----------------------+------------------+-----------------------+
| Application           | Current Syntax   |  Proposed Desugaring  |
+=======================+==================+=======================+
| Function Application  | ``f a``          | ``f $ a``             |
+-----------------------+------------------+-----------------------+
| Type Application      | ``f @t``         | ``f @t``              |
+-----------------------+------------------+-----------------------+
| Operator              | ``a <> b``       | ``a <> b``            |
+-----------------------+------------------+-----------------------+
| Left Section          | ``(e <>)``       | ``\x -> x <> e``      |
+-----------------------+------------------+-----------------------+
| Right Section         | ``(<> e)``       | ``\x -> e <> x``      |
+-----------------------+------------------+-----------------------+

The idea is that only application in the plain juxtaposition syntax is
rebindable, application found elsewhere remains the same.

To rebind function application, one sets the ``$`` currently
in scope. This can be done globally by declaring/importing a top-level
``$`` and locally by using ``let`` or ``where``.

Examples
--------

With ``RebindableApplication``, we can use a local rebind to
write the simplified examples shown in the motivation:

.. code-block:: haskell

  -- ``f = g <$> a <*> b <*> c`` can become
  f = let g' = pure g; ($) = (<*>) in g' a b c

  -- ``mapE = VarE 'map `AppE` f `AppE` xs`` can become
  mapE f xs = let map' = VarE 'map, ($) = AppE in map' f xs

Alternatively, we could use a type class and a global rebinding instead:

.. code-block:: haskell

  import qualified Prelude as P

  class Applicable a b r where
    ($) :: a -> b -> r

  instance (a' ~ a, b' ~ b) => Applicable (a -> b) a' b' where
    ($) = (P.$)

  instance (a ~ Exp, b ~ Exp) => Applicable Exp a b where
    ($) = AppE

  -- ``mapE = VarE 'map `AppE` f `AppE` xs`` can now become
  mapE :: Exp -> Exp -> Exp
  mapE f xs = VarE 'map f xs

Effect and Interactions
-----------------------

This proposal allows different application functions to
all share the same syntax, which I would argue allows users to write
more concise (and, to a certain extent, clearer) code.

It also has the interesting effect of making application more
first-class syntactically (according to the definition Dijkstra outlined
`here <http://www.the-magus.in/Publications/ewd.pdf>`_).
The juxtaposition notation is now merely syntactic sugar for an
operator (namely ``$``).

Costs and Drawbacks
-------------------

I imagine that their will be some maintenance costs associated with
the proposed extension -- though given that the proposal is purely
syntactic, I imagine such costs will be minor.

For learners, the desugaring may be initially confusing, but I would
argue that confusion will be mostly from long time Haskellers who are used
to function application being built into the syntax.
New users are just learning of the parallels between the
juxtaposition syntax and ``$`` and thus do not have such distinctions
ingrained.
Thus, I would argue that they will likely find the desugaring much more
straightforward (and possibly even expected).

The proposed desugaring does however come with a number of drawbacks due to
the limitations of the function ``$``.
Due to the restrictions of levity polymorphism, ``$`` can not be fully levity
polymorphic. Thus modules with ``RebindableApplication`` can not use the
juxtaposition syntax for primitive operations and constructors like ``I#``.
Similar problems occur with higher-rank functions defined with ``RankNTypes``.

I imagine these limitations will pose a major challenge to learning
the ins and outs of ``RebindableApplication``, and I consider them a major
weakness of the proposal overall.
Unfortunately, there is not much that can be done about this at the moment.
However, I intend to post a proposal soon that proposes an explicit application
operator ``$#`` (which was described in previous versions of this proposal).
If accepted, that operator could be used to mitigate these issues --
problematic applications could simply use it instead.

Alternatives
------------

There are a number of possible alternatives, two of which I will discuss here.

Do Nothing
^^^^^^^^^^

We can always do nothing. This would require us to still use application
operators like ``(<$>)`` to perform application on types outside the function
arrow ``(->)``.

Personally, I believe that this status quo is rather ugly and causes the
language to give unjustified primacy to functions represented by the function
arrow ``(->)`` as opposed to those presented other ways.
A similar critique was made by Dijkstra himself in the EWD note previously
referenced  (i.e. `this one <http://www.the-magus.in/Publications/ewd.pdf>`_).
As such, I do not believe it is correct to maintain the status quo.

Idiom Brackets
^^^^^^^^^^^^^^

If global rebindings of the juxtaposition syntax are considered too extreme,
we could use a bracketing syntax to limit the scope of the rebinding.
Instead of desugaring all occurrences of the juxtaposition syntax, we only
do so within the brackets. For example, using idiom brackets:

.. code-block:: haskell

  let map' = VarE 'map, ($) = AppE in (| map' f xs |)

could desugar to

.. code-block:: haskell

  let map' = VarE 'map, ($) = AppE in map' $ f $ xs

Unresolved Questions
--------------------

The name of the extension given in the proposal i.e. ``RebindableApplication``
could be changed if desired.


Implementation Plan
-------------------

**TBD**

Depending on the anticipated difficulty (hacking on GHC is rather new to me),
I could potentially volunteer to implement this.
