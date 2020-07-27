Meaning-preserving parsing rules for SCC annotations
====================================================

.. author:: Vladislav Zavialov
.. date-accepted:: 2019-04-17
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/15730
.. implemented:: 9.0
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/176>`_.
.. contents::

In today's Haskell adding an ``SCC`` annotation can change the semantics of an
expression::

  ghci> 1 / 2 / 2
  0.25
  ghci> 1 / {-# SCC ann #-} 2 / 2
  1.0

This is a side-effect of the parsing rules for ``SCC``, ``GENERATED``, and
``CORE`` annotations. We propose to change the parsing rules in a way that
guarantees that adding an annotation does not affect the meaning of a program.

Motivation
------------

When it comes to annotations in expressions, there a few expectations a
programmer might have:

1. Annotations have the lowest precedence of all syntactic constructs.
2. Annotations can be inserted anywhere inside an expression without
   parentheses.
3. Adding an annotation does not affect the structure or meaning of an
   expression in ways other than adding an annotation to a subexpression.

Lowest precedence
~~~~~~~~~~~~~~~~~

The first expectation that annotations have the lowest precedence can be
demonstrated with the following example::

  dfn =
      {-# SCC f #-}
      f a b

There are two sensible ways we could parse this::

  ({-# SCC f #-} f) a b -- Option A
  {-# SCC f #-} (f a b) -- Option B

In option A, the annotation applies only to ``f`` itself, binding tighter than
function application. In option B, the annotation applies to the entire
expression ``f a b``, binding looser than function application. As it is a more
common use case to annotate an entire subexpression rather than a small part of
it, GHC picks option B.

The same reasoning applies to expressions that involve operators::

  {-# SCC ann #-}  f a x + g b y + h c z  -- this expression...
  {-# SCC ann #-} (f a x + g b y + h c z) -- ... is parsed like this,
  {-# SCC ann #-} (f a x) + g b y + h c z -- ... not like this.

Arbitrary placement
~~~~~~~~~~~~~~~~~~~

The second expectation that annotations can be inserted anywhere in an
expression without parentheses can be demonstrated by the following example::

  x = 1 + f a b                -- without annotation
  x = 1 + {-# SCC f #-} f a b  -- with annotation

We want to be able to place the annotation even in the middle of the
expression, and it appears unnecessary to require parentheses in this case::

  x = 1 + ({-# SCC f #-} f a b)

Note that as of today, GHC does not fully fulfil this expectation and there are
places where annotations cannot be inserted::

  ghci> f {-# SCC ann #-} a b
  <interactive>:3:3: error: parse error on input ‘{-# SCC’

Structure preservation
~~~~~~~~~~~~~~~~~~~~~~

The third expectation that adding an annotation does not affect the structure
or meaning of an expression in ways other than adding an annotation to a
subexpression follows from they way they are used, from their name, and from
their syntax.

1. Annotations are often added and removed during development (for instance,
   ``SCC`` during profiling). The programmer is likely to expect that adding or
   removing an annotation will not change the meaning of their program.

2. The name *annotation* suggests that it is not a first-class syntactic
   construct that could change the structure of an expression in fundamental
   ways – instead, it must *annotate* a subexpression and leave the rest of the
   program intact.

3. The syntax of annotations mimics the syntax of comments, reusing ``{-`` and
   ``-}``, so it is natural to expect that they inherit syntactic properties of
   comments and have no effect on expression structure.

Unfortunately, with today's GHC, adding an annotation can have unexpected
effects on expressions::

  ghci> 1 / 2 / 2
  0.25
  ghci> 1 / {-# SCC ann #-} 2 / 2
  1.0

This is the result of current parsing rules::

  1 /                 2 / 2   ==   (1 / 2) / 2
  1 / {-# SCC ann #-} 2 / 2   ==   1 / (2 / 2)

Fundamental conflict
~~~~~~~~~~~~~~~~~~~~

Lowest precedence, arbitrary placement, structure preservation – pick two.

Let us consider the example where GHC fails to offer arbitrary placement::

  ghci> f {-# SCC ann #-} a b
  <interactive>:3:3: error: parse error on input ‘{-# SCC’

We can fix it in two ways:

1. By sacrificing lowest precedence, parse as ``f ({-# SCC ann #-} a) b``.
2. By sacrificing structure preservation, parse as ``f ({-# SCC ann #-} a b)``.

Now consider the second example where GHC fails to offer structure
preservation::

  ghci> 1 / 2 / 2
  0.25
  ghci> 1 / {-# SCC ann #-} 2 / 2
  1.0

We can fix it in two ways:

1. By sacrificing lowest precedence, parse as ``1 / ({-# SCC ann #-} 2) / 2``.
2. By sacrificing arbitrary placement, disallow the occurrence of the ``SCC``
   annotation in this position.

It is a tough choice which two properties to guarantee to the end-user (and
today we fully guarantee only one – lowest precedence):

1. Without lowest precedence, annotations may be unexpectedly added to smaller
   subexpressions.
2. Without arbitrary placement, adding an annotation may result in a
   compile-time error.
3. Without structure preservation, adding an annotation may affect the
   semantics of a program in unexpected ways.

In all three cases, the issue can be resolved by explicit parenthesization.

We argue that the best choice is to sacrifice arbitrary placement:

* From the user's standpoint, this is the most predictable option. Adding an
  annotation either results in an error or applies to an entire expression.

* From the implementor's standpoint, this is the easiest route. This statement
  comes from the experience of implementing the other option (Phabricator Diff
  `D5218 <https://phabricator.haskell.org/D5218>`_ sacrificies lowest
  precedence).

Therefore, we propose to disallow ``SCC``, ``GENERATED``, and ``CORE``
annotations in positions where they may affect the structure of an expression.

Proposed Change Specification
-----------------------------

``SCC``, ``GENERATED``, and ``CORE`` annotations have the same precedence as
lambda abstractions, let expressions, and conditionals, and also are right
associative. Additionally, none may appear in a position where it would
change the grouping of subexpressions.

Establish a policy that if any new annotations are added, they must adhere to
the same principles.

Effect and Interactions
-----------------------

Some existing programs with annotations may require adding parentheses to
compile.

Costs and Drawbacks
-------------------

This change will not have maintenance costs. In fact, it will result in a
simpler grammar, more amendable to other modifications, as there are less
positions where we have to support annotations.

Alternatives
------------

1. Continue to guarantee less than two properties.
2. Guarantee lowest precedence and arbitrary placement at the cost of structure
   preservation. This means we would allow ``f {-# SCC a #-} a b`` and parse
   it as ``f (a b)``.
3. Guarantee structure preservation and arbitrary placement at the cost of
   lowest precedence.

Unresolved Questions
--------------------

What option to pick? Arguably, structure preservation is the most important
property of the three, but it is much less clear whether lowest precedence or
arbitrary placement is more important.

Implementation Plan
-------------------

I (Vladislav Zavialov) will (attempt to) implement.
