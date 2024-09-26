Represent quotations in Template Haskell
==============

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

This proposal is about removing the nested bracket limitation in Template Haskell
so that more expression forms can be represented.

Motivation
------------

The quotation form turns an expression into it's representation. For example
quoting ``1 + 2`` (``[| 1 + 2 |]``) gives us the syntax tree for ``1 + 2`` as an
expression in the host language. However, not all expressions can be quoted, one
particular omission are quotations themselves. This proposal is about removing
this restriction and thus making quotation act in a more uniform manner.

For example, writing ``[| [| 5 |] |]`` should return the syntax for a quotation
which represent the number ``5``. ``[| [| [| 5 |] |] |]`` should evaluate to the
representation of a quotation containing a quotation of 5 and so on.

Removing this restriction makes Template Haskell into a proper multi-stage
language in the same vein as Metaocaml.


Proposed Change Specification
-----------------------------

There are two aspects which need to be modified to support quoting quotations.

1. Implement a representation of a quotation in the ``template-haskell`` library.

2. Generalise the cross-stage persistence machinery to work for ``n`` levels.

In order to implement the first point, we need to add a new constructor to
the ``Exp`` data type to represent quotations. This is main crux of the proposal,
what it means to represent a quotation. To answer this, we remember that
an ``Exp`` is a representation of a renamed expression.

We first might try to represent a bracket as just ``BrackE Exp`` which works
fine for simple quotations but doesn't account for splices. How can we represent
splices as well? One option is to add a new constructor for splices as well
but we remember that
after renaming all brackets are normalised so that all splices are removed.
Any splice which appears inside a quotation is turned into a "pending splice"
and inserted into an environment for the quotation. ::

   [| $(foo x) |] ==> [| x' |]_{x' = foo x}

This environment tells us that ``x'`` in the quotation refers to the result
of evaluating ``foo x``. So, the representation is an expression and an environment.::

   data Exp = ... | BrackE Exp [(Name, Exp)] | ...


Now turning to the second point, modifying cross-stage persistence to work
with nested brackets is straightforward.
In the two-level case, cross-stage persistence is the automatic lifting of a
variable from level 0 to level 1. ::

   foo x = [| x |]

This is implemented by desugaring the usage of ``x`` into a splice and lift. ::

   foo x = [| $(lift x) |]

The splice decreases the level by one so now ``x`` is used at the level it is bound.

With nested brackets, it is necessary to generalises this idea to work with
n levels. ::

   foo2 x = [| [| x |] |]

Now ``x`` is bound at level 0 and used at level 2. This means we have to decrease
the level of ``x`` twice by inserting two splices and two lifts.::

   foo2 x = [| [| $($(lift (lift x))) |] |]

We can also bind a variable in stage 1 and use it in stage 2 ::

   foo3 = [| \x -> [| x |] |]

The difference between the binding level and usage level is 1 so we insert
one splice and one lift. ::

   foo3 = [| \x -> [| $(lift x) |] |]

In general, we desugar a variable bound at level ``i`` and used at level ``j`` into
``j - i`` lifts follows by ``j - i`` splices.



Effect and Interactions
-----------------------

With these two proposed changes a user can nest quotations as deep as they
like and also seamlessly lift variables from earlier to later stages. As
Template Haskell already supports nested splices, it will now become a proper
multi-stage language like metaocaml which can be used for serious staging
applications.


Costs and Drawbacks
-------------------

The only drawback that I can see is that the serialised form of expressions
in my current implementation can get very big after a few levels. It
would be worth thinking about whethe it is possible to postpone an expression
by means other than lifting it multiple times.


Alternatives
------------


Unresolved Questions
--------------------

* No outstanding unresolved questions


Implementation Plan
-------------------

* I have already `implemented this proposal <https://gitlab.haskell.org/ghc/ghc/merge_requests/259>`_.
