Replace the ``atomicModifyMutVar#`` primop
==========================================

.. author:: David Feuer
.. date-accepted:: 2018-07-11
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/15364
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/149>`_.
.. contents::

``atomicModifyIORef``, a thin wrapper around the ``atomicModifyMutVar#`` primop,
is very lazy. In some applications, this can lead to space leaks. As a result,
people have written various stricter versions. Unfortunately, these tend not
to be as efficient as they could be. I believe the solution is to replace
``atomicModifyMutVar#`` with a new primop.

Furthermore, there are situations where we want to modify an ``IORef``
but don't need to return any additional information. In these
cases we should be able to use a slightly lighter primop.

Finally, ``atomicWriteIORef`` is currently implemented in terms of
``atomicModifyIORef``, which is overkill to just get a write barrier. I believe
we should add a primop specifically to support it.

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
is wasteful. To top off the troubles, these are all exercises in
awkwardness and subtlety. We shouldn't have to go to so much trouble
to do such simple things.

Proposed Change Specification
-----------------------------

``atomicModifyMutVar#`` replacement
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Replace ``atomicModifyMutVar#`` with ::

 atomicModifyMutVar2#
   :: MutVar# s a
   -> (a -> (a, b))
   -> State# s -> (# State# s, a, (a, b) #)

and add a user-facing wrapper ::

 atomicModifyIORef2
   :: IORef a
   -> (a -> (a, b))
   -> IO (a, (a, b))
 atomicModifyMutVar2 (IORef (STRef ref)) f = IO $ \s ->
   case atomicModifyMutVar2# ref f s of
     (# s', old, res #) = res `seq` (# s', (old, res) #)

and a convenience function, ``atomicModifyIORefW``, detailed below. Note
that ``atomicModifyIORef2`` is *strict* in the (pair) result of the function.
Based on my experience reading code using atomic modification, I think
this is almost always what people actually want.

The new primop would return the previous value of the ``MutVar#`` as well as
the full result of applying the passed function.  Like ``atomicModifyMutVar``,
the new primop would be completely lazy. Semantically, ::

 atomicModifyMutVar2# mv f = unIO $
   atomicModifyMutVar (IORef (STRef mv)) $ \old ->
     let f_old = f old
     in (fst f_old, (old, f_old))

However, ``atomicModifyMutVar2#`` would serve as a much better base on which to
build stricter operations.

We can define ::

 atomicModifyIORef (IORef (STRef ref)) f = IO $ \s ->
   case atomicModifyMutVar2# ref f s of
     (# s', _, ~(_, res) #) -> (# s', res #)

 -- A version that ignores the previous value and forces the result
 -- of the function; the latter prevents space leaks in many cases.
 atomicModifyIORefW :: IORef a -> (a -> (a, b)) -> IO (a, b)
 atomicModifyIORefW ref f = do
   (_, p@(_,_)) <- atomicModifyIORef2 ref f
   return p

 atomicModifyIORef' ref f = do
   (!_, !res) <- atomicModifyIORefW ref f
   pure res

 atomicModifyIORefP ref f = do
   (_, res) <- atomicModifyIORefW ref f
   pure res

 -- Caveat: there's actually an altogether better way to implement this
 -- function; this is only an example.
 atomicWriteIORef ref x = do
   atomicModifyIORefW ref (\_ -> (x, ()))
   pure ()

All of these definitions strike me as much simpler and easier to reason about
than the ones required by ``atomicModifyMutVar#``.

Finally, ``atomicModifyIORef2`` is useful by itself if the user wants to use
the old and/or new ``IORef`` values for something else too.

For backwards compatibility, we can define ::

 atomicModifyMutVar#
   :: MutVar# s a
   -> (a -> (a, b))
   -> State# s -> (# State# s, b #)
 atomicModifyMutVar# mv f s =
   case atomicModifyMutVar2# mv f s of
     (# s', _, ~(_, b) #) -> (# s', b #)

which I expect to be at least as efficient as the current ``atomicModifyMutVar#``
and very often more so. In particular, it will be better when demand analysis
determines that ``b`` is used strictly or not used at all. In that case, the
selector thunk simply won't be created at all.

Extra power
###########

The type given above for ``atomicModifyMutVar2#`` is a little bit of a lie.
Because ``GHC.Prim`` doesn't have (boxed) tuple types, the type would actually
look like ::

 atomicModifyMutVar2#
   :: MutVar# s a
   -> (a -> c)
   -> State# s -> (# State# s, a, c #)

This type is of course rather dangerously wrong. But the *true* type lies
between them: the result must be a (possibly newtype-wrapped)
single-constructor datatype whose first field is lifted. We can get
express the real type using generics ::

 type family Leftmost (a :: Type -> Type) :: Type where
   Leftmost (M1 i ('MetaData _ _ _ 'True) f) = Leftmost' f
   Leftmost (M1 i ('MetaSel _ _ _ 'DecidedUnpack) f) = Leftmost' f
     -- It would also be reasonable to error out in the unpacked case.
   Leftmost (M1 i c f) = Leftmost f
   Leftmost (f :*: g) = Leftmost f
   Leftmost (K1 i c) = c

   Leftmost (f :+: g) = TypeError ('Text "Sum types cannot be used with atomicModifyIORefG")
   Leftmost U1 = TypeError ('Text "atomicModifyIORefG expects a record with at least one field")
   Leftmost V1 = TypeError ('Text "atomicModifyIORefG expects a record with at least one field")

 -- Dig through newtypes and unpacked things
 type family Leftmost' (a :: Type -> Type) :: Type where
   Leftmost' (M1 i c f) = Leftmost' f
   Leftmost' (K1 i c) = Leftmost (Rep c)

 atomicModifyIORefG :: a ~ Leftmost (Rep r) => IORef a -> (a -> r) -> IO (a, r)
 atomicModifyIORefG (IORef (STRef ref)) f = IO $ \s ->
   case atomicModifyMutVar2# ref f s of
     (# s', old, new #) -> (# s', (old, new) #)

This is safe as long as the ``Generic`` instances are derived or otherwise
legitimate.

Version with no extra return information
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

I think we should add a primop ::

 atomicModifyMutVar_#
  :: MutVar# s a
  -> (a -> a)
  -> State# s
  -> (# State# s, a, a #)

and a (result-strict) wrapper ::

 atomicModifyIORef_ :: IORef a -> (a -> a) -> IO (a, a)

This would be useful for (particularly strictly) modifying the contents of an
``IORef`` without producing additional information. It would
return only the old value and the new one.

Atomic swapping
^^^^^^^^^^^^^^^

Finally, I think we should add a primop ::

 atomicSwapMutVar#
   :: MutVar# s a
   -> a
   -> State# s
   -> (# State# s, a #)

and a wrapper ::

 atomicSwapIORef :: IORef a -> a -> IO a

This would just write a value to an ``IORef`` and return its old
value; it would be used to reimplement ``atomicWriteIORef``.

Effect and Interactions
-----------------------
I don't foresee any significant interactions.

Costs and Drawbacks
-------------------

Costs
^^^^^
The development cost will be very low. I anticipate a low maintenance cost
as well. The new primop implementation is essentially the same as the current
one but with some parts removed: we just need to build two closures instead of
three.

Potential drawbacks
^^^^^^^^^^^^^^^^^^^

1. If we actually use the result, but do so lazily, we'll perform two heap
   checks instead of one. I doubt this cost will ever be noticeable, whereas I
   imagine the reduced allocation in other situations may have a real impact for
   heavy users. Along with being very small, I predict that this cost will very
   rarely be realized in practice.

2. There is some history of the optimizer accidentally defeating the selector
   thunk optimization in the GC. I don't know if that could be a problem for the
   proposed reimplementation of ``atomicModifyIORef``, but if so it could
   theoretically lead to space leaks in unusual situations. The GHC test suite
   did not reveal any such problems, however; indeed, the only test deviation
   was a reduction in allocations in one test.

Alternatives
------------

0. We could add a new primop without removing the old one. This would give
   the best backwards compatibility, but I'm not sure it's really worth
   the trouble.

1. We could change the primop without renaming it. I'd prefer not to break
   backwards compatibility that way, however.

2. We could refrain from returning the previous ``MutVar#`` contents; indeed,
   the first draft of this proposal did so. But that is sometimes useful to
   have and the cost of providing it is minimal.

3. There is a large design space for library functions based around
   ``atomicModifyIORef2``. I don't have very strong opinions about which
   ones should be included; I'd even be okay with adding *only*
   ``atomicModifyIORef2`` and letting library developers figure out what
   else to add over time, if that would help move things along.

Unresolved questions
--------------------
1. What are the best names for the primop and wrappers?
   ``atomicModifyIORefW`` is an utterly terrible name, but I haven't
   been able to think of a good one.

2. Where should the compatibility wrapper live?

3. Should the compatibility wrapper have the bogus type ``atomicModifyMutVar#``
   has now, or should it be restricted to pairs? I don't know if people are
   currently taking advantage of the extra flexibility in the type. Someone
   could, for example, use a two-component record type instead of an actual
   tuple. If we want to support those uses of the wrapper, we'll need to
   stick an ``unsafeCoerce`` inside.

Implementation Plan
-------------------
I have drafted `an implementation <https://phabricator.haskell.org/D4884>`_
of ``atomicModifyMutVar2#`` which can be modified as needed.
