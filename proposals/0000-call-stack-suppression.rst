Call stack suppression
======================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/117>`_.
.. sectnum::
.. contents::

Add a magical function to locally suppress call stack growth.


Motivation
------------
I was working on adding a library feature for adding messages to call stacks
(see `this Trac ticket <https://ghc.haskell.org/trac/ghc/ticket/14911>`_).
Most of what I need can be done at the library level, but there's one
spot where that is a mess. The basic idea is to add an extra constructor
to the ``CallStack`` type: ::

 data CallStack
  = EmptyCallStack
  | PushCallStack [Char] SrcLoc CallStack
  | FreezeCallStack CallStack
  | MessageCallStack [Char] CallStack

 messageCS :: forall a. HasCallStack
       => String
       -> (HasCallStack => a)
       -> a
 messageCS str val =
   let ?callStack = MessageCallStack str ?callStack
   in val

There are a couple little wibbles. The first is that we don't really want
the call stack to show ``messageCS``. That's no big deal; we can just pop
the call stack it's given. The bigger annoyance is that when GHC is solving
the ``HasCallStack`` constraint for ``val``, it will add in a frame for
the variable named ``val``, too late for us to remove it.

There are two ways to work around this problem, neither of which is very
nice. One option is to ``unsafeCoerce`` between ``->`` and ``=>`` to hide
the constraint from GHC. The other is to modify ``error`` to try to
recognize the ``val`` frames and ignore them.

Proposed Change Specification
-----------------------------
Add a magical function ::

 callWithStack :: (HasCallStack => a) -> CallStack -> a

This function, while magical, is extremely simple: it passes the given
``CallStack`` to its argument, without performing the usual call-stack-pushing
transformation to that argument. We can't quite implement this today, as best I
can tell.

Effect and Interactions
-----------------------
I don't foresee any interactions.

Costs and Drawbacks
-------------------
The only cost I know of is what should be a very small amount of extra
complexity in the type checker. The call stack internals are not intended
for Haskell learners, so it should have no immediate impact on them.

Alternatives
------------
We could put an extra ``Skip`` constructor on the stack when we add a message,
and make ``pushCallStack`` look a couple frames up to see if there's a
``Skip``, and if so remove the frame below it. This sounds way more complicated
than my poor brain can handle, and it seems like a good way to waste the
optimizer's time.

Unresolved questions
--------------------
I don't have any unresolved questions at the moment.

Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other ressources and prerequisites are required for implementation?
