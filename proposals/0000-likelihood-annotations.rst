Add Likelihood Annotations
==============

.. proposal-number:: _.
.. trac-ticket:: _.
.. implemented:: _.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/182>`_.
.. sectnum::
.. contents::

A lot of performance can be gained by optimizing for hot code paths.
However GHC currently does not allow users to actually give hints about which code
paths are hot code paths.

This proposal changes this.


Motivation
------------

Programmers often know a good deal more about program behaviour than we can currently
communicate to the compiler. This leads to missed optimization opportunities.

Consider types like ``data Either a b = Left a | Right b``

Left is often used to represent an exceptional case. Making Right the common case.

Based on this information and given code like ``either f g x`` we might want
to inline g but not f for example.

The same logic applies to many other cases. For example Lists are rarely empty.

A programmer has this information but no way to communicate this to the compiler.

Communicating this kind of information can allow optimization passes to produce
faster code. In particular, potential gains include:
 * Avoid inlining into unlikely case alternatives.
 * Produce better code layout.
 * Better register allocation.

This would also bring us closer to profile guided optimization (PGO).
A lot of the required backend work for this is also required for PGO.

Proposed Change Specification
-----------------------------

We propose a new Pragma: ``{-# LIKELY <NUM> #-}``

This will be useable in the following constructs:
 - Simple case expressions.
 - Simple function bindings.
 - If expressions.
 - Data type declarations for Sum Types.

Uses where the requirements are not satisfied will result in warnings similar to
the ones from bad UNPACK pragmas. Likelihood values must be >= 0.

Weight semantics
~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a simple branching expression with n branches ``[A1 .. An]``,
with likelihoods ``[L1 .. Ln]`` GHC will optimize code under the assumption that
the chance for the i-th branch to be taken is ``Li / sum [L1 .. Ln].``

In other words likelihood gives the relative frequency of branches.

While it's possible to give weights of zero the code covered by such weights
is not viewed as dead code. Instead the compiler will preserve the semantics of
this branch while trying to minimize impact of this branch on other branches.

Simple case expressions
~~~~~~~~~~~~~~~~~~~~~~~~~~

Simple case expressions are case expressions which:
 - Don't contain nested patterns.
 - Don't use guards.
 - Only match on ADTs or GADT.

We give a likelihood by <Pattern> -> <Pragma> <rhs>. See examples below.

If a case has no annotations, assumptions about likelihoods are up to the implementation.
If a case has alternatives with and without likelihood information then the compiler
will give a warning, and the unannotated alternatives are given an implementation dependent likelihood.
If a case doesn't match all possible constructors, then the unmatched constructors are assumed to have likelihood zero.

For a simple example take this code:

.. code:: haskell

 head xs = case xs of
    []    -> {-# LIKELY 0 #-} error "Empty list"
    (x:_) -> {-# LIKELY 1 #-} x

Here we tell the compiler that we assume the error case is never taken, and the second alternative is always taken.

If expressions
~~~~~~~~~~~~~~~~~~~~~~~~~~

If expressions can be annotated as shown below.

.. code:: haskell

 if cond then {-# LIKELY 2000 #-}
          e1
         else {-# LIKELY 1000 #-}
          e2

This is equivalent to the following case:

.. code:: haskell

 case cond of
  True -> {-# LIKELY 2000 #-} e1
  False -> {-# LIKELY 1000 #-} e2

Simple function bindings.
~~~~~~~~~~~~~~~~~~~~~~~~~~

These are functions matching on a single argument, analog to simple case expressions.

Their behaviour is identical to writing the function using a case instead of a binding pattern match.

Syntax example:

.. code:: haskell

 head []    = {-# LIKELY 0 #-} error "Empty list"
 head (x:_) = {-# LIKELY 1 #-} x




Default weights for Constructors
~~~~~~~~~~~~~~~~~~~~~~~~~~

Data type behaviour derives from the case behaviour.

Given by example, the syntax for a data declaration is as follows:

.. code:: haskell

 data Foo
   = {-# LIKELY <NUM> #-} Bar
   | {-# LIKELY <NUM> #-} Baz

 data Foo where
   {-# LIKELY <NUM> #-} Bar :: Foo
   {-# LIKELY <NUM> #-} Baz :: Foo

If likelihood information for data types is given, it must be given for all constructors.

When pattern matching on an expression of such a type using a simple case expression:
The default likelihoods are given by the information in the data declaration, unless
any explicit likelihoods are given in which case the information form the data declaration
won't be used.

When pattern matching on such an expression using other means the likelihood information
might be considered by the compiler but no guarantees are given.

Effect and Interactions
-----------------------

Intended Effects.
~~~~~~~~~~~~~~~~~~~~~~~~~~

This makes it possible to have GHC optimize better for hot code paths.

Currently high performance code tends to vary things like constructor order manually for maximal performance.
This will provide a more reliable alternative which will remain stable between versions.

A proof of concept implementation currently nets a ~3% speedup on nofib, along with a
2% increase in compile time, however only backend optimizations are implemented in this
prototype so the potential is greater still.

To give some examples:

Inlining
.................

.. code:: haskell

 f x = case x of
  Just v  -> {-# LIKELY 1 #-} e1
  Nothing -> {-# LIKELY 0 #-} e2

We can avoid inlining e2 knowing it is rarely called, reducing code size and
 making f itself a better inlining candidate.

Backend code
.................

For more low level optimization we always want control flow for the hot path to be
linear. This means given the code below:

.. code:: haskell

 f x = case x of
         C1 -> {-# LIKELY 1 #-} e1
         C2 -> {-# LIKELY 0 #-} e2

We want assembly (simplified to just the control flow) to look like this:

::

 f:
  <if x == C2> goto e2:
 e1:
  <e1_code>
 e2:
  <e2_code>

Currently the order of e1 and e2 is determined implicitly by the order of constructors
and the used GHC version.

With the pragma, GHC will try to generate this layout when beneficial.

Unsupported Interactions.
~~~~~~~~~~~~~~~~~~~~~~~~~~
There is no plan to add support for weights on other constructs than described above.
This includes, but is not limited to:

- Guards
- Complex Patterns (Nested Constructors, Multiple arguments)
- Rebindable Syntax
- Pattern Synonyms
- View Patterns

The issue with most of these is that there is no obvious way how weights should work with these.


Costs and Drawbacks
-------------------
This comes with an increase in compiler complexity and a small compile time overhead as one would expect.

I don't expect any negative impact on existing code.

Users not making explicit use of this feature could still gain performance benefits if libraries define
default weights.

Alternatives
------------
None I know of.

Unresolved Questions
--------------------

How weights should be given exactly, in particular:

 * Should weights be given only as integers, or should rationals be accepted.
   Currently favouring integers.

 * Should weights be given in the Int64 range and mapped to positive numbers.
   For example by a given weight w being interpreted as the value e^w.

   This would make it quite easy to express relative order or add weighted cases on both ends of the curve.
   However it makes it a lot harder to express exact relations between multiple branches.

Give feedback if you have ideas on how to improve this further.

Implementation Plan
-------------------
I would implement this.

Implementation is work in progress, the current state is available here: https://phabricator.haskell.org/D4327