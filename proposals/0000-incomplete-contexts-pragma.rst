.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

The INCOMPLETE_CONTEXTS pragma
==============================

The proposal is to add a pragma that allows a specified class to be left out of any explicit type declaration context.

Motivation
----------

The original inspiration came from the proposed implementation mechanism for preserving the backward compatibility with
the ``Eval`` class proposal. In the form presented here, though, the proposal would be more immediately useful during
development and debugging. So I'll start with this motivating example:
::
   
   low-level-function :: a -> b -> DataStructure a b -> DataStructure a b
   low-level-function a b = trace ("Working with " ++ show (a, b)) $ ...

You see, this ``low-level-function`` appears to contain a bug that's triggered by certain input values, and we added the
``trace`` call in order to discover the problematic inputs. Even though the function is polymorphic, we happen to use
it with a data type that is an instance of ``Show``.

The above example triggers the following GHC error report:
::
   
      No instance for (Show a) arising from a use of ‘show’
      Possible fix:
        add (Show a) to the context of
          the type signature for:
            low-level-function :: a -> b -> DataStructure a b -> DataStructure a b

If we listen to compiler's advice and try compiling again, we'll be rewarded by another error advising us to add ``(Show
b)`` to the function's context as well. Also, to add the same context to every single ``high-level-function`` that
depends on ``low-level-function``, and then to their callers *et cetera*. Sometimes it's enough to make one give up and
write a QuickCheck test suite instead. Or to live with the bug.

Proposed Change
---------------

Introduce pragma ``INCOMPLETE_CONTEXTS`` with a single parameter that has to be a single-argument class name. In this
example, the full pragma would be
::
   
   {-# INCOMPLETE_CONTEXTS Show #-}

It's obvious from the error message that GHC knows exactly how to fix the type signature. The pragma tells the compiler
to go ahead and apply its own recommendation throughout the module, transitively expanding all necessary contexts until
the module compiles.

It's not difficult to come up with examples of type classes other than ``Show`` that could be temporarily used for
debugging with this pragma: ``Monad``, ``MonadIO``, ``Typeable``, and ``Arbitrary`` immediately jump to mind. Apart from
debugging, the pragma could be used for rapid prototyping until we decide which class constraints are necessary: must we
demand a ``Monad`` or would an ``Applicative`` instance be enough?

Still, the pragma is meant to be strictly a debugging and development aid. It would only replace compiler errors by
compiler warnings that cannot be silenced. Once the bugs and the design issues are taken care of, the developer should
either remove the offending code or explicitly add the missing contexts. Hackage should refuse to accept any package
upload that still triggers the warnings.

As implied above, the effect of the ``INCOMPLETE_CONTEXTS`` pragma is per module. It should be placed before the
``module`` keyword, together with the ``LANGUAGE`` pragmas. The type signatures of exported bindings are affected the
same as everything else, but their users in importing modules are not: one may need to add the pragma to each importing
module.

Drawbacks
---------

We must be careful to prevent the use of ``INCOMPLETE_CONTEXTS`` from seeping into released packages. It would be a
grievous loss if incomplete signatures started appearing on Hackage.

Alternatives
------------

Instead of a whole new pragma, ``INCOMPLETE_CONTEXTS`` could become just another language extension. This would leave no
place to specify the class name, so presumably all context constraints would then become optional. I'd prefer some more
discipline, and I'm guessing the proposal would be easier to implement as presented.

If the need to add pragma to multiple modules turns out to be too inconvenient, we can add a command-line option in
addition to the pragma.

The specified proposal is made as simple as possible, so it provides no mechanism to cover any non-standard contexts
such as multiple-parameter type class constraints, type equality constraints, or type functions. Adding those would
require syntactic and semantic changes and presumably complicate the implementation.

Unresolved Questions
--------------------

Context ``HasCallStack`` is funny in that it it's not a class name and it already comes with an ad-hoc mechanism that
makes the transient context adjustments unnecessary. Per documentation:

    GHC solves HasCallStack constraints in three steps:

    1. If there is a ``CallStack`` in scope -- i.e. the enclosing function has a ``HasCallStack`` constraint -- GHC will
    append the new call-site to the existing ``CallStack``.

    2. If there is no ``CallStack`` in scope -- e.g. in the GHCi session above -- and the enclosing definition does not
    have an explicit type signature, GHC will infer a ``HasCallStack`` constraint for the enclosing definition (subject
    to the monomorphism restriction).

    3. If there is no ``CallStack`` in scope and the enclosing definition has an explicit type signature, GHC will solve
    the ``HasCallStack`` constraint for the singleton ``CallStack`` containing just the current call-site.

The present proposal does not cover it, but there is some appeal in allowing ``{-# INCOMPLETE_CONTEXTS HasCallStack
#-}`` with obvious semantics: the first point above would be additionally qualified with *or the ``INCOMPLETE_CONTEXTS
HasCallStack`` pragma is specified in the module*. This would provide an easy way to obtain deep call stacks while
debugging.
