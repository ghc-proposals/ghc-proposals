Replace the ``atomicModifyMutVar#`` primop
==========================================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/149>`_.
.. sectnum::
.. contents::

``atomicModifyIORef``, a thin wrapper around the ``atomicModifyMutVar#`` primop,
is very lazy. In some applications, this can lead to space leaks. As a result,
people have written various stricter versions. Unfortunately, these tend not
to be as efficient as they could be. I believe the solution is to replace
``atomicModifyMutVar#`` with a new primop.


Motivation
------------
``atomicModifyIORef`` is extremely lazy. In particular, ::

 atomicModifyIORef ref (const undefined)

will succeed, although both the new value in the ``IORef`` and the return
value from the operation will be undefined. There are several ways to
make this stricter. ``atomicModifyIORef'`` forces both the new value and
the return value: ::

 atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
 atomicModifyIORef' ref f = do
     !b <- atomicModifyIORef ref $ \a ->
             case f a of
                 v@(!a',_) -> v
     return b

One could also arrange to force the result of the given function without
forcing either of its components: ::

 atomicModifyIORefP :: IORef a -> (a -> (a,b)) -> IO b
 atomicModifyIORefP ref f = do
   Solo b <- atomicModifyIORef ref $ \a ->
               case f a of
                 (a', b) -> (a', Solo b)
   return b

Or to force the newly installed value without forcing the result: ::

 atomicModifyIORefN :: IORef a -> (a -> (a,b)) -> IO b
 atomicModifyIORefN ref f = do
   Solo b <- atomicModifyIORef ref $ \a ->
               case f a of
                 (!a', b) -> (a', Solo b)
   return b

But each of these solutions may lead to some extra work, and some of them
will lead to additional allocation. Moreover, the implementation of
``atomicModifyMutVar#`` has extra allocation *built in* to support
a lazy result value. When someone wants a result forced to WHNF, this
is wasteful.

Proposed Change Specification
-----------------------------
Replace ``atomicModifyMutVar#`` with ::

 atomicModifyMutVar_#
   :: MutVar# s a
   -> (a -> (a, b))
   -> State# s -> (# State# s, (a, b) #)

and add a user-facing wrapper ::

 atomicModifyIORef_
   :: IORef a
   -> (a -> (a, b))
   -> IO (a, b)

The new primop would return the full result of applying the passed function.
Like ``atomicModifyMutVar``, the new primop would be completely lazy. But
it serves as a much better base on which to build stricter operations.

We can define ::

 atomicModifyIORef ref f = snd <$> atomicModifyIORef_ ref f

 atomicModifyIORef' ref f = do
   (!_, !res) <- atomicModifyIORef_ ref f
   pure res

 atomicModifyIORefP ref f = do
   (_, res) <- atomicModifyIORef_ ref f
   pure res

 atomicWriteIORef ref x = do
   !_ <- atomicModifyIORef_ ref (\_ -> (x, ()))
   pure ()

Finally, ``atomicModifyIORef_`` is useful by itself if the user wants to use
the new ``IORef`` value for something else too!

For backwards compatibility, we can define ::

 atomicModifyMutVar#
   :: MutVar# s a
   -> (a -> (a, b))
   -> State# s -> (# State# s, b #)
 atomicModifyMutVar# mv f s =
   case atomicModifyMutVar_# mv f s of
     (# s', ~(_, b) #) -> (# s', b #)

which I expect to be just as efficient as the current ``atomicModifyMutVar#``
and sometimes more so.

All of these definitions strike me as much simpler and easier to reason about
than the ones required by ``atomicModifyMutVar#``.

Effect and Interactions
-----------------------
I don't foresee any significant interactions.

Costs and Drawbacks
-------------------
The development cost will be very low. I anticipate a low maintenance cost
as well. The new primop implementation is essentially the same as the current
one but with some parts removed: we just need to build two closures instead of
three. I can't think of any real drawbacks.

Alternatives
------------
We could change the primop without renaming it. I'd prefer not to break backwards
compatibility that way, however.

Unresolved questions
--------------------
What are the best names for these things?

Where should the compatibility wrapper live?

Should the compatibility wrapper have the bogus type ``atomicModifyMutVar#``
has now, or should it be restricted to pairs? I don't know if people are
currently taking advantage of the extra flexibility in the type.

Implementation Plan
-------------------
I have drafted `an implementation <https://phabricator.haskell.org/D4884>`_
which can be modified as needed.
