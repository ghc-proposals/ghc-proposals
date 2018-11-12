Function Return Type Signatures
===============================

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


Motivation
------------

With the ScopedTypeVariables extension enabled, GHC already supports in-line type signatures during function definition, for example:

::

  f (x :: Int) = g x :: String

Many users coming from Scala, F#, OCaml, etc, prefer such inline signatures and would expect to be able to provide a return-type signature on the left-hand-side. For example:

::

  f (x :: Int) :: String = g x

Such a signature would also mean only needing to specify the type annotation once when there are multiple right-hand-side equations. For example:

::

  f (x :: Int) :: String
    | p x = g x
    | otherwise = msg

We propose an extension to allow the above syntactic form. It is a fairly small low-risk change, requiring no changes to the Happy grammar and parser. It provides an arguably more familiar and more succinct syntax for specifying type signatures.


Proposed Change Specification
-----------------------------

The Happy grammar for a top-level pattern or function bind, already specifies an optional signature:

::

  decl_no_th
    ...
    | infixexp_top opt_sig rhs

Currently a validation step fails if this optional signature is specified in addition to one or more pattern bindings. The proposal here is for us to not reject such a parse, but instead give it meaning.

The semantics are such that any return-type signature provided on the left-hand-side is semantically equivalent to a user provided expression signature applied to the right-hand-side body.

We propose that this change is enabled whenever ScopedTypeVariables is enabled; and is just a syntactic change.

Effect and Interactions
-----------------------

Because the extension is a simple syntactic addition which can be easily expressed using existing features, I do not expect any unintended effects or interactions.

Costs and Drawbacks
-------------------

The development/maintenance cost is small, but not insignificant as we will have to add an extra field into one of the front-end AST types.

Pattern signatures currently can only introduce rigid, fully-known type variables; and we are somewhat further encouraging their use with this extension (see https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#pattern-type-signatures). However, OCaml behaves in a similar fashion for such inline signatures.

Alternatives
------------

The alternative is to add an explicit expression signature on the right-hand-side of the function body, but this is counterintuitive to many beginners and new users from other languages. It also results in more boilerplate when using multiple right-hand-side expressions.

Implementation Plan
-------------------

The proposal authors will implement this change.
