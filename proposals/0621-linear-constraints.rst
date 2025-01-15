Linear Constraints
==================

.. author:: Jack Hughes, Arnaud Spiwack
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/621>`_.
.. sectnum::
.. contents::

.. _paper: https://arxiv.org/abs/2103.06127
.. _linear_haskell_paper: https://arxiv.org/abs/1710.09756
.. _talk: https://www.youtube.com/watch?v=c8VZp-3eQU0
.. _`Existential Types proposal`: https://github.com/ghc-proposals/ghc-proposals/pull/473
.. _blog_freeze: https://www.tweag.io/blog/2023-01-26-linear-constraints-freeze/
.. _blog_scopes: https://www.tweag.io/blog/2023-03-23-linear-constraints-linearly/
.. _`Efficient resource management for linear logic proof search`: https://www.sciencedirect.com/science/article/pii/S0304397599001735?via%3Dihub
.. _`Linear Types proposal`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst
.. _mezzo_lang: http://protz.github.io/mezzo/

Since the introduction of linear types in GHC 9.0.1, programmers have
been able to write programs with safe manual memory management much
like Rust. However, using linear types in this way often requires
writing code with substantial boilerplate, which can turn writing such
code into a chore of carefully ensuring that linear resources are
threaded through the program correctly.

This proposal introduces *linear constraints*: constraints (in the
sense of “something left of a ``=>`` arrow”) which follow the linear
type discipline. Specifically, this proposal introduces a *linear fat
arrow* ``%1 =>``, which behaves as follows.

Consider

::

   useC :: C %1 => Int
   (+) :: Int %1 -> Int %1 -> Int


Then the following is accepted

::

   consuming :: C %1 => Int -> Int
   consuming n = useC + n

The constraint ``C`` is consumed exactly once (namely by the call to
``useC``).

On the other hand, the following program is rejected:

::

   neglecting :: C %1 => Int -> Int
   neglecting n = n + n

as ``C`` is never consumed.

Furthermore, as with linear arguments, we must *guarantee* that ``C``
will be consumed, regardless of the conditional branching. Thus, the
following would also be rejected:

::

   dithering :: C %1 => Bool -> Int
   dithering x = if x then useC else 10
as ``useC`` is only consumed when ``x == True``.

These examples show programs rejected by failing to guarantee that
``C`` will be used. However, as we are in a linear context, we must
also ensure that it isn't *overused*:

::

   overusing :: C %1 => (Int, Int)
   overusing = (useC, useC)

Since ``overusing`` consumes a linear ``C`` constraint twice, it is
rejected for violating linearity. We may amend the type scheme of
``overusing`` in the following way, however:

::

   stammering :: (C, C) %1 => (Int, Int)
   stammering = (useC, useC)

By providing an additional linear constraint, the resource usage
guarantees are satisfied and the program type checks.

The theoretical basis of this proposal, as well as further examples,
can be found in the `Linearly Qualified Types paper <paper_>`_ by
Spiwack et al. You may also want to watch the introductory
`talk at ICFP 2022 <talk_>`_ [17min].

Motivation
----------

The motivation of this proposal is to remove unnecessary boilerplate
that is often introduced when programming with linear types.

Threading
^^^^^^^^^

Consider the following program:

::

   read2AndDiscard :: MArray a %1 -> (Ur a, Ur a)
   read2AndDiscard arr0 =
     let (arr1, x) = read arr0 0
         (arr2, y) = read arr1 1
         () = free arr2
     in (x, y)

This is a function which takes an array as a linear argument, reads
the first two elements, and then de-allocates it before returning the
two elements. The fact that the array is a linear resource provides
some nice guarantees which ensure memory safety. For example, there is
no way we can go on to erroneously read from the array after
de-allocating it. Instead, when ``arr0`` is used as an argument to
``read``, ``read`` consumes it and returns the array under a new name
``arr1``. This process then repeats before finally ``free``
de-allocates the array.

While this code ensures that we use the ``MArray`` in a memory-safe
way, this re-naming process introduces boilerplate and becomes
cumbersome to both write and read. Compare to the doing the same with
the ``ST`` monad instead:

::

   read2AndDiscardST :: MArray s a -> ST s (a, a)
   read2andDiscardST arr = do
     x <- read arr 0
     y <- read arr 1
     return (x, y)

There is much less noise. To be honest, this is all a little silly:
the name management is completely mechanical; it should be handled
automatically by the compiler. This is what this proposal lets us do:
get pretty close to the latter style, without having to introduce a
monad.

A more interesting, though more complex, example involving freezing
nested mutable structure is elaborated in `this blog post <blog_freeze_>`_.

Typestate
^^^^^^^^^

In the `Linear Haskell paper <linear_haskell_paper_>`_, an API is
proposed for sockets to handle typestate (that is, reflecting in the
types that sockets change state over time). Assuming a linear IO
monad, it looks like this:

::

   socket :: IO (Socket Unbound)
   bind :: Socket Unbound %1 -> SocketAddress -> IO (Socket Bound)
   listen :: Socket Bound %1 -> IO (Socket Listening)
   ...

With it you can write programs such as:

::

  do
    s1 <- socket -- s1 :: Socket Unbound
    s2 <- bind s1 (SocketAddressInet6 S.inet6Any 8080 0 0) -- s2 :: Socket Bound
    s3 <- listen s2 -- s3 :: Socket Listening
    ...

The idea is that each call consumes its argument, so ``s1`` isn't
available when ``s2`` is, and we effectively track the typestate of
the socket.

But this isn't very idiomatic, as we have to return this new socket
every time (since the “do” notation supports shadowing, we could name
all of them ``s``, this would alleviate the burden a little). With
linear constraints, this can become

::

   data NewSocket where
     NewSocket :: Unbound s %1 => Socket s -> NewSocket

   socket :: IO NewSocket
   bind :: Unbound s %1 => Socket s -> SocketAddress -> IO (Bound s /\ ())
   listen :: Bound s %1 => Socket s -> IO (Listening /\ ())
   ...

   do
     NewSocket s <- socket -- Unbound s
     Box () <- bind s (SocketAddressInet6 S.inet6Any 8080 0 0) -- Bound s
     Box () <- listen s -- Listening s
     ...

This modifies the program much less. The source of the ``s`` variable
is more explicit. And with a little bit of ``QualifiedDo`` (or
``RebindableSyntax``) work, we can modify the program even less from
the idiomatic form (it would be even better with primitive existential
type, see the corresponding section below).

::

   Typestate.do
     NewSocket s <- socket
     bind s
     listen s
     ...


Creating linear values
^^^^^^^^^^^^^^^^^^^^^^

A well documented difficulty, when writing APIs for mutable data as
above, is that in order to guarantee that, say, an array is unique, it isn't
sufficient that ``read`` and ``write`` be linear functions. If I
create an array with

::

   new :: Int -> MArray -- or Int %1 -> MArray

Then ``new 57`` can be shared arbitrarily. This is a phenomenon known
as “promotion”: expressions without linear free variables are
unrestricted. The typical solution is for ``new`` to take a
continuation as an argument

::

   new :: Int -> (MArray %1 -> Ur a) %1 -> Ur a

This forces the array to be single-threaded (thanks to the ``Ur a``
return type, the ``MArray`` cannot escape the continuation's scope),
which we can use to guarantee uniqueness.

This is a little clumsy to program with. But more importantly, these
continuations aren't very composable as argued in `this blog post
<blog_scopes_>`_. See also the long discussion at
`tweag/linear-base#130
<https://github.com/tweag/linear-base/issues/130>`_. This proposal
will let us define ``new`` in direct style. Direct-style new requires
a little more than the simple linear constraint outlined so far,
namely *dupable classes*, specified in the eponymous section below.

Proposed Change Specification
-----------------------------

Main Syntax Changes
^^^^^^^^^^^^^^^^^^^

Currently, type class constraints in GHC do not support multiplicity
annotations.  GHC currently defines the syntax for type signatures as:

::

   ctype   ::= context '=>' ctype | type | ...

Essentially, type signatures can consist of (among other things which
we ignore here) zero or more qualified type arrows ``=>`` followed by
a type. Here ``context`` is a list of class constraints.

When ``-XLinearTypes`` is enabled, the following new syntax is
enabled:

::

   ctype ::= context '%' 1 '=>' ctype | context '=>' ctype | type | ...

Note that unlike multiplicities for function type arrows, linear
constraint arrows may only be instantiated with a ``1`` (linear)
multiplicity. Unlike linear function types, this proposal does not
introduce multiplicity polymorphism in constraint arrows, so there is
never a need for the multiplicity to be anything other than a ``1`` -
a ``Many`` multiplicity is already represented by omitting the
multiplicity entirely (i.e. using a standard constraint). Standard
non-linear constraints can then still be used in combination with
linear ones, with the order of linear and unrestricted constraints not
mattering.

Semantics/Desugaring
^^^^^^^^^^^^^^^^^^^

As typical, we define the semantics of linear constraints via
desugaring. The linear fat arrow ``%1 =>`` desugars to a linear arrow
``%1 ->``. Namely, writing ``⦇·⦈`` for the desugaring function:

- ``⦇C %1 => A⦈ = C %1 -> ⦇A⦈``
- ``⦇e :: C %1 => A⦈ = \(%1 $d :: C) -> ⦇e :: A⦈``

This desugaring means that changes to GHC Core itself are not
required: we only need the material introduced by linear types, which
is part of GHC since GHC 9.0.


Using type classes linearly
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Type class methods require an unrestricted class constraint:

::

  -- Given
  class Foo a where
    f :: F a
    g :: G a

  -- We have
  f :: Foo a => F a
  g :: Foo a => G a

This is unchanged. But we add one exception: in type classes with exactly
one method, the one method is linear in the class constraint
(otherwise there would never be inhabitants in the type ``C %1 => T``)

::

  -- Given
  class Bar a where
    h :: H a

  -- We have
  h :: Bar a %1 => H a

Typing inference
^^^^^^^^^^^^^^^^

In a way, there's no need to worry about type inference: if a function
``C %1 -> T``, with well-placed dictionaries, would be rejected, then
``C %1 => T`` will be rejected as well. So understanding linear types
is sufficient for the most part.

But when there is an accepted assignment of type ``C %1 -> T``, it
doesn't follow that the function of type ``C %1 => T`` will be
accepted. Because GHC's typechecker doesn't make guesses.

The one new rule introduced by this proposal is that when I want a
linear constraint ``C`` and I've been given both a linear and an
unrestricted ``C``, then this is considered ambiguous and raises a
type error. See Section 6.3 of the paper_ for more details.

To see why, consider this example

::

  class C
  giveC :: (C => Int) -> Int
  useC :: C %1 => Int

  bad :: C %1 => (Int, Int)
  bad = (giveC useC, useC)

  bad' :: C %1 => (Int, Int)
  bad' = (giveC useC, 0)

In ``bad``, if the leftmost ``useC`` uses the linear ``C`` from the
function signature, then ``bad`` would be rejected, it must used the
unrestricted ``C`` from ``giveC``. But in ``bad'`` it must use the
linear ``C`` instead. So this would force the leftmost ``useC`` to
make a guess. Instead we reject both ``bad`` and ``bad'``.

Equality constraints
~~~~~~~~~~~~~~~~~~~~

Given equality constraints are used for rewriting *only
if they are unrestricted* (correspondingly, equality constraints
generated by the type inference algorithm are unrestricted, as they
have always been).

The reason for this is that there is no clear semantics to make use of
a linear equality constraint ``a ~ b`` as part of the unification
algorithm (it's not that reasoning about linear equality is
meaningless, but a unification or congruence conversion for linear
equality isn't obvious to come up with, if someone has, we're not
aware). Nor does it feel like a true limitation as there is no example
where a linear equality would be useful. It's really not worth the
bother of trying to find a solution.

Dupable classes
^^^^^^^^^^^^^^^

A new module ``GHC.Constraint.Linear`` is introduced (inlined
alternative: bikeshed names, including the module name).

This module exposes the following:

::

  data LinearityToken :: ZeroBitType

  consumeLinearityToken :: LinearityToken %1 -> (# #)
  dup2LinearityToken :: LinearityToken %1 -> (# LinearityToken, LinearityToken #)

  data DupableClassModifier = Dupable

Class declaration can be annotated with the ``%Dupable``

::

   %Dupable class <ctx> => <head> where
    <methods>

Classes annotated with ``%Dupable`` must:

- Have a single method
- The method must be of type ``LinearityToken``

Such a dupable type class can be used multiple times (including 0
times) even if they are linear. *E.g.*::

  class Foo where
    foo' :: LinearityToken

  foo :: Foo %1 => Int -> Int

  dupes :: Foo %1 => (Int -> Int, Int -> Int)
  dupes = (foo, foo)

  consumes :: Foo %1 => Bool
  consumes = True

But, crucially, not passed to an unrestricted function::

  rejected :: Foo %1 => Ur (Int -> Int)
  rejected = Ur foo

There are a lot of alternatives for the design of this feature, so see
the *Alternatives* section for more thoughts.

Examples
--------

Threading
^^^^^^^^^

We refer back now to the first example from the motivation section,
which showed how writing a function which reads the first two elements
of an array became a tedious exercise of threading our linear resource
through the function. Using linear constraints, however, such a
function can be written as (this notation is very explicit, but it can be
improved, see the *“Do” notation* and *Strict lets* sections below):

::

   read2AndDiscard ::  (Read n, Write n) %1 => MArray a n -> (Ur a, Ur a)
   read2AndDiscard arr =
        read arr 0 & \cases (Box x) ->
        read arr 1 & \cases (Box y) ->
        free arr & \cases () ->
        (x, y)

The main way in which this differs from our previous function is that
our array is no longer a linear resource - it is
*unrestricted*. However, we maintain the guarantee that it is used in
a way which does not violate linearity through the ``Read n`` and
``Write n`` linear constraints. Here, ``n`` is a type-level tag used
to identify the array. Accordingly, our type constructor for
``MArray`` is parameterised by ``n``.

The type signatures for  ``read``, ``free``, and ``Box`` are:

::

   read  :: Read n %1 => MArray a n -> Int -> Read n /\ Ur a

   free :: (Read n, Write n) %1 => MArray a n -> ()

   data c /\ a where
     Box :: c %1 => a -> c /\ a

i.e. ``read`` is a function which consumes a linear ``Read n``
constraint, allowing us to read from the specified array index. It also
returns a new ``Read n`` constraint, allowing us to subsequently read
from the array again. Likewise, ``free`` consumes both a ``Read n``
and a ``Write n`` constraint and introduces none, ensuring that we
cannot read or write after freeing.

Thus we eliminate the need to manually thread the ownership of the
array through the function, whilst maintaining the guarantees of
unique ownership via the linear constraints.

For a more in-depth example along these lines, refer to section 4 of
the paper_.

“Do” notation
^^^^^^^^^^^^^

Using ``QualifiedDo`` (or ``RebindableSyntax``) we can turn the series
of ``\cases`` into something a little bit more visually appealing. The
``read2AndDiscard`` example can be recast as

::

   read2AndDiscard ::  (Read n, Write n) %1 => MArray a n -> (Ur a, Ur a)
   read2AndDiscard arr = DataFlow.do
        (Box x) <- read arr 0
        (Box y) <- read arr 1
        free arr
        (x, y)

We just need to define the following module

::

   module DataFlow where

   (>>=) :: a %1 -> (a %1 -> b) %1 -> b
   a >>= b = b a

   (>>) :: () %1 -> b %1 -> b
   () >> b = b

We'll write the rest of the examples in this style.

For monadic code, the normal do notation for linear monads already
does the right thing.

More “do” notation
^^^^^^^^^^^^^^^^^^

An option, with the “do” notation, is to go even further and handle
``Box`` in the notation. This could look like

::

   module QualifiedDataFow where

   (>>=) :: c /\ a %1 -> (c %1 => a -> b) %1 -> b
   (Box a) >>= b = b a

   (>>) :: c /\ ()  %1 -> (c %1 => b) %1 -> b
   (Box ()) >> b = b

   module QualifiedMonad where

   (>>=) :: Monad m => m (c /\ a) %1 -> (c %1 => a -> m b) %1 -> m b
   -- Using the do notation for linear monads
   ma >>= k = Linear.do { Box a <- ma; k a }

   (>>) :: m (c /\ ()) %1 -> (c %1 => m b) %1 -> m b
   mu >> mb = Linear.do { Box () <- mu; mb }

We'd have to change the APIs to always return a ``c /\ a`` even if
``c=()``. E.g.

::

   free :: (Read n, Write n) %1 => MArray a n -> () /\ ()

The code samples in this proposal use explicit ``Box``-s for clarity,
but it isn't meant to be prescriptive, we'll need more experience to
decide which style is best (it's worth pointing out that the
difference vanishes under the existential types proposal, see below).

The Linearly constraint
^^^^^^^^^^^^^^^^^^^^^^^

We can create a class, the paper_ calls it ``Linearly`` with the
following API:

::

  %Dupable class Linearly

  linearly :: (Linearly %1 => Ur a) %1 -> Ur a
  newLinearlyDict :: Linear.IO (Dict Linearly)

  data Dict c where
    Dict :: c %1 => Dict c

This ensures that it is not possible to ever build an unrestricted
evidence for ``Linearly``.

Equipped with this we can extend the API of the example above with
a way to create arrays::

  new :: Linearly %1 => Int -> NewMArray a

  data NewMArray a where
    NewMArray :: (Read n, Write n) %1 => MArray a n -> NewMArray a

Because there is no unrestricted evidence of ``Linearly``, the
linearity of the ``Linearly`` constraint will contaminate the returned
``NewMArray a`` value, ensuring in turn that the returned ``Read n``,
and ``Write n`` constraints *must* be used linearly, as required.

The difference with having ``new`` itself use a continuation is that
we can now have several calls to ``new`` in the same scope. Which
prevents the problems described in the *Motivation* section.

::

  linearly $ DataFlow.do
    (NewMArray arr1) <- new
    (NewMArray arr2) <- new
    … -- modify the array as suited
    Ur $ sum arr1 + sum arr2

Note how we introduce ``Linearly`` *once* with ``linearly``, but use
``Linearly`` twice (once per occurrence of ``new``), this uses the
fact that ``Linearly`` is dupable.

See also Sections 3.2 and 4 of the paper_.

Borrowing
^^^^^^^^^

Borrowing consists in zooming in on a part of a mutable data
structure. Our example will be taking a subarray:

::

   subarrayMeh :: (Read n, Write n) %1 => Int -> Int -> MArray a n -> NewMArray a

The problem with the type of ``subarrayMeh``, however, is that it
loses the right to write to the original array forever. This isn't
borrowing, this is theft.

But we can't keep the capabilities on the original array either:

::

   subarrayBad :: (Read n, Write n) %1 => Int -> Int -> MArray a n -> (Read n, Write n) /\ NewMArray a

Soundness or our mutable arrays depend on the absence of sharing. But
we very much intend ``arr`` and ``arr' = subarray 42 57 arr`` to share
mutable cells. So while ``arr'`` can be written to (or read from), we need to “pause”
the ability to write to (or read from!) ``arr``. One way to achieve
this is with quantified (linear) constraints

::

   data NewBorrowedArray n a where
     NewBorrowedArray
     :: (Read n', Write n', Write n, (Read n', Write n') %1 => Read n)
     %1 => MArray a n' -> NewBorrowedArray n a

   subarray :: (Read n, Write n) %1 => Int -> Int -> MArray a n -> NewBorrowedArray

The way this works is that as soon as you read or write from ``arr``,
you'll consume the ``Read n'`` and ``Write n'`` with the quantified
constraint, and it will render ``arr'`` unreadable.

This treatment of borrowing is inspired the `Mezzo programming
language <mezzo_lang_>`_. Though borrowing is more well-known these
days through the lens of Rust where borrowing is traditionally seen as
being bound to a scope (though in truth, non-lexical lifetimes makes
is no longer true). The scope-style is less primitive than the
quantified constraint style:

::

   subarrayScoped
     :: (Read n, Write n) %1 => Int -> Int -> MArray a n
     -> (forall n'. (Read n', Write n') %1 => (Read n', Write n') /\ r)
     %1 -> (Read n, Write n) /\ r
   subarrayScoped from len arr scope = DataFlow.do
     NewBorrowedArray arr' <- subArray from len arr
     Box r <- scope arr'
     Box r

Note that everything done with (linear or not) constraints can be done
by explicit argument passing. But this is a good example where the
minutia of the arguments would be quite painful to manage, while it's
mostly invisible with linear constraints.

Also note how borrowing is a sort of linear lens
``cap n %1 -> (cap n' * (cap n' %1 -> cap n))``. This isn't a
coincidence. A `recent paper
<https://hal.science/hal-04360462/document>`_ explores this connection
between borrowing and lenses. We don't know if this can be exploited
for API design.

PS: This example relies on the fact that ``Write n`` is useless
without ``Read n``.  A more satisfactory implementation would suspend
both ``Read n`` and ``Write n``. However, with linear constraints
``C1 %1 => (A, B)`` isn't equivalent to ``(C1 %1 => A, C1 %1 => B)``
so we'd need to write something like

::

   data NewBorrowedArray n a where
     NewBorrowedArray
     :: (Read n', Write n', (Read n', Write n') %1 => (Read n, Write n))
     %1 => MArray a n' -> NewBorrowedArray n a

This isn't permitted in GHC, and it probably shouldn't be, as it isn't
clear how to solve constraints when we are allowed such givens.

So it does seem that to implement borrowing in this fashion we need
some kind of “root” constraint which is necessary for all the
operations. So that we can suspend this constraint and this constraint
only to prevent all operations on ``arr``.

Effect and Interactions
-----------------------

The changes described in the above section equip GHC with a *linearly*
qualified type system, allowing us to write programs with linear
capabilities which are inferred to be correct implicitly. Primarily,
we can now write programs like the one given above, which no longer require
the manual threading of a linear resource to ensure that the resource
is used in a linear way - all the programmer has to do is ensure the
linear constraints are satisfied within the program.

Aside from introducing new syntax for linear constraint arrows, the
majority of changes to GHC are localised to GHC's constraint
generation and solving. Some care must therefore be taken with regard
to how linear constraints interact with existing features of GHC's
constraint solver: namely the interaction between linear constraints
with superclasses in type class constraints and with equality
constraints:

Superclasses
^^^^^^^^^^^^

Consider

::

   class Eq a => Ord a where ...

In terms of the constraint solver, this introduces an axiom ``Ord a => Eq a``.
This proposal doesn't change this axiom (that is the axiom
keeps using the unrestricted implication). It means that a linear
given ``Ord a`` cannot be used to derive an instance of ``Eq a``.

To see why, consider

::

   class Eq a => Ord a where
     compare :: a -> a -> Ordering

   -- This isn't globally consistent, for simplicity
   withOneOrd :: Eq a => (a -> a -> Ordering) %1 -> (Ord a %1 => r) -> r

If the superclass axiom was ``Ord a %1 => Eq a``, then we could write

::

   bad :: (Int -> Int -> Ordering) %1 -> True
   bad f = withOneOrd f (if 0 == 1 then True else False)

Notice how this doesn't use ``f`` at all, despite the guarantee that
``f`` be linear, which is unsound [#super-class-with]_.

The only way in which an ``Ord a %1 => Eq a`` would be sound is if
``Ord a`` had no method at all. In which case ``Ord`` could only be
used via its ``Eq a`` superclass. Even if we could make this work (see
below), this corner-case is hardly worth the bother.

Before we put the final nail in this coffin, let's briefly address
that the fact that the arrow in ``class Eq a => Ord a`` is the wrong
way around, suggests that the ``Eq`` is somewhat unrestricted here,
and maybe the intuitive axiom would be something like ``Ord a %1 => Ur (Eq a)``.
Such an axiom would break Lemma 5.5 of the paper_. Not only
is it outside of the fragment of linear logic that we know how to
solve, but it breaks the proof of soundness (so the resulting type
inference would presumably be unsound, although we don't know that, we
only know that we don't know how to prove it sound).

Finally, having an axiom ``Ord a %1 => Eq a`` for a superclass usually
breaks constraint solving anyway. To be precise, it breaks
*guess-free* constraint solving. The problem is that the axiom
overlaps with the instance axioms. In traditional Haskell, the way
this overlap is addressed is by using the superclass axiom in reverse:
instead of changing a wanted of type ``Eq a`` into a wanted of type
``Ord a``, givens of type ``Ord a`` let us add a given of type ``Eq a``.
But if the given is linear, that would hardly do: we'd have both
the original ``Ord a`` and the derived ``Eq a``, consuming both counts
as consuming the original ``Ord a`` twice, not once!

Final final nail: axioms of the form
``Traversable t %1 => (Functor t, Foldable t)``
aren't in the fragment that we know how to solve.

Instance contexts
^^^^^^^^^^^^^^^^^

We do not specify a way, in this proposal, for instance contexts to be
linear.

That is the syntax

::

  instance (Foo a, Bar a) %1 => Baz a where {…}

Is rejected. See alternatives for a potential specification.


Wanted with Other Multiplicities
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Givens, by virtue of the syntax, are always either linear or
unrestricted. However, wanteds can, in principle, have different
multiplicities.

Let

::

   p :: Multiplicity
   f :: A %p -> B
   useC :: C %1 => A

Then in

::

  f useC

we have wanted ``C`` with multiplicity ``p`` (``p`` is a rigid
variable). What do we do? We solve ``C`` as if it were an unrestricted
wanted.

Wanted with delayed multiplicities
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Consider

::

   f :: A %p -> (A %p -> B) -> B
   useC :: C %1 => A

Then in

::

   f useC

we have wanted ``C`` whose multiplicity is a unification variable, the
value of which will be determined by the context. What do we do? There
are two cases:

- There's an unrestricted given with head ``C`` *and no such linear given*, then
  the unrestricted given can solve the wanted.
- There is a linear given with head ``C``: we don't solve ``C`` until
  ``p`` has been determined.

Quantified Constraints
^^^^^^^^^^^^^^^^^^^^^^

Our syntax extension naturally extends constraints in types' context
to support linear implications ``C %1 => D`` when
``-XQuantifiedConstraint`` is on. While this is not described in the
paper_, our solving algorithm is based on `Efficient resource
management for linear logic proof search`_, where such higher-order
givens are handled. The extension is unproblematic, it would
presumably be more effort to prevent it than to support it.

Therefore, when ``-XLinearTypes`` is
on, contexts can contain implications of the form ``C %1 => D``.

Implicit parameters
^^^^^^^^^^^^^^^^^^^

Referencing an implicit parameter is linear in the implicit
parameters. This means that linear implicit parameters can effectively
be used in programs

::

   foo :: (?x :: A) %1 => A
   foo x = ?x

Note that, because implicit parameters are currently implemented as
single-method type classes, this comes for free in the implementation.

Existential types proposal
^^^^^^^^^^^^^^^^^^^^^^^^^^

The `Existential types proposal`_, if they ever materialise (🤞), will
make this proposal even better. Using linear constraints in APIs most
often require returning constraints too. For that we've been using
GADTs. This was our simple mutable array API:

::

  read  :: Read n %1 => MArray a n -> Int -> Read n /\ Ur a

  free :: (Read n, Write n) %1 => MArray a n -> ()

  data c /\ a where
    Box :: c %1 => a -> c /\ a

  new :: Linearly %1 => Int -> NewMArray a

  data NewMArray a where
    NewMArray :: (Read n, Write n) %1 => MArray a n -> NewMArray a

With this API we can write functions such as

::

  read2AndDiscard ::  (Read n, Write n) %1 => MArray a n -> (Ur a, Ur a)
  read2AndDiscard arr = DataFlow.do
       (Box x) <- read arr 0
       (Box y) <- read arr 1
       free arr
       in (x, y)

  linearly $ DataFlow.do
    (NewMArray arr1) <- new
    (NewMArray arr2) <- new
    … -- modify the array as suited
    Ur $ sum arr1 + sum arr2

There is still a little bit of noise there, what with the defining of
GADTs (while ``/\`` can be defined once and for all, types like
``NewMArray`` must be defined for most every type because we lack
type-level lambdas), and the constructors in the let-bindings.

With existential types, this would look something like this (the
existential types proposal defines, not coincidentally, a ``/\`` with
the same role as that above):

::

  read  :: Read n %1 => MArray a n -> Int -> Read n /\ Ur a
  free :: (Read n, Write n) %1 => MArray a n -> ()

  new :: Linearly %1 => Int -> exists n. (Read n, Write n) /\ MArray a n

  read2AndDiscard ::  (Read n, Write n) %1 => MArray a n -> (Ur a, Ur a)
  read2AndDiscard arr = DataFlow.do
       x  <- read arr 0
       y  <- read arr 1
       free arr
       (x, y)

  linearly $ DataFlow.do
    arr1 <- new
    arr2 <- new
    … -- modify the array as suited
    Ur $ sum arr1 + sum arr2

Quite a bit cleaner isn't it? Of course, though, since the existential
types proposal needs to modify Core, it's quite a bit more involved
that this one. And linear constraints are already pulling a lot of
weight without existential types.

Strict lets
^^^^^^^^^^^

In an earlier version of the proposal, threading with linear
constraint was done using let bindings in the examples. Like

::

  read2AndDiscard ::  (Read n, Write n) %1 => MArray a n -> (Ur a, Ur a)
  read2AndDiscard arr =
      let !(Box x)  = read arr 0
          !(Box y)  = read arr 1
          !()       = free arr
       in (x, y)

This was before the authors realised that strict let-bound gadt
patterns didn't actually expose their constraints to their body (so
this example wouldn't typecheck without further changes to GHC).

Let bindings being more flexible than the do notation, it may be
worth, in the future, addressing this limitation.

For the benefit of the reader, the discussion in the pull
request didn't turn up any design decision which led to strict let
patterns not exposing constraints. We can speculate that it's simply
because, for expendiency, or by oversight, a strict let pattern was
given the exact same typing rule as a lazy let pattern. Lazy patterns
cannot expose their constraints, it would be unsound. Consider

::

  data T a where { MkT :: Int -> T Int }
  f :: T a -> Int -> a
  f ~(MkT i) y = y

  veryBad :: Bool
  veryBad = f @Bool undefined 42 -- 42 is a boolean


Costs and Drawbacks
-------------------

The implementation is confined to the typechecker, and is expected to
be rather modest. In order to solve linear constraints, two changes
need to be made to the constraints:

- The multiplicity of constraints has to be tracked
- Wanted constraints can not only be paired with a multiplicative
  conjunction (when collecting constraints from both members of an
  application), but also with an additive conjunction (when collecting
  constraints from alternatives in a case- or if-expression).

For the former, we can simply pair constraints (given and wanted) with
a multiplicity (note that in the case of wanted the multiplicity can
be a variable which can be substituted later). For the latter, the
plan is to replace the type of the right-hand side of implication
constraints, currently a bag of constraints, to be a bag of bags of
constraints (read as an additive conjunction of multiplicative
conjunctions).

The constraint solver must count the linear givens that it uses. This
will add an extra state field in the solver to communicate that some
givens are not available anymore because they've been used to solve a
constraint before. Because we keep the algorithm guess free, this
extra state doesn't force us to backtrack and make different choices.

There may be changes to the desugarer. In particular, for classes
``C`` with superclasses to be supported as linear constraint, we'd
need the superclass dictionary to be held in an unrestricted field of
the dictionary of ``C``. But dictionaries are actually generated late
and we don't check linearity past the output of the desugarer. So this
bit should be free (not that it would be expensive if we had to
execute).

Dupable classes are a bit more work, mostly the solver needs to figure
out where to insert duplications of the dictionary when it's used
several times. Following the proof of the paper_ would make us add a
duplication at every application node, which is clearly
impractical. So some care is required here, the solution is not
immediately obvious.


Backward Compatibility
----------------------

This proposal doesn't affect the compilation of existing programs (with
or without ``-XLinearTypes``).


Alternatives
------------

Why not a monad instead?
^^^^^^^^^^^^^^^^^^^^^^^^

In this section we'll address various objections of the form “why
couldn't you use a monad gadget instead?”. Which, of course, would be
considerably cheaper, since it doesn't require changing GHC.

Monads instead of uniqueness
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Simon Peyton Jones asks

    You have

    ::

        read2AndDiscard arr =
             let !(Box x)  = read arr 0
                 !(Box y)  = read arr 1
                 !()       = free arr
             in (x, y)

    where the capability returned by each read is (somehow) fed into the next. Is this really better than the monadic version?

    ::

        do { x <- read arr 0
           ; y <- read arr 1
           ; free arr }

The answer is: it depends. The ``ST`` monad allows for
sharing/aliasing of mutable resources. Linear types' unique/unaliased
arrays is much simpler for reasoning, but cannot have sharing. This is
crucial for some algorithm and, though it comes with all the
difficulties we know, if you're writing such an algorithm, you don't
really have a choice but to ditch linear types and use the ``ST``
monad.

One consequence, which was a starting point of the `Linear Types
proposal`_ is that freezing a mutable array is unsafe with ``ST`` and
safe with linear types. With linear constraints, we can even
(conveniently) safely freeze nested mutable arrays (`see this blog
post <blog_freeze_>`_).

Furthermore, if you do actually want to do some manual memory
management, like we in this example, forgetting ``free`` in ``ST``
causes a hard-to-track memory leak. With linear types, it creates a
type error.

But, probably, the real motivation for using linear arrays rather than
``ST`` arrays is seen in the types. Compare

::

   read2AndDiscard :: (Read n, Write n) %1 => MArray n a -> (a, a)
   read2AndDiscard :: MArray r a -> ST r (a, a)

Plainly, and unsurprisingly, one has a monad, the other doesn't. It is
our claim (but it's presumably not a controversial claim at all) that
code without monad composes better than code with monads. The
counterargument, of course, is that the linear type ecosystem is still
limited, and you may have to pay a bigger novelty budget for linear
types than for monads. Which, hopefully, won't deter us from trying.

Typestate and indexed monad
~~~~~~~~~~~~~~~~~~~~~~~~~~~

One way to track typestate in monadic Haskell code, is to replace the
monad by an indexed monad

::

   class IndexedMonad m where
     return :: a -> m s s a
     (>>=) :: m s t a -> (a -> m t u b) -> m s u b

You get to track the typestate in the ``s``, ``t``, ``u`` type
variables; with ``QualifiedDo``, you can easily use the “do” notation
for this.

This requires no change to GHC. But nobody does this. Why? Either 1/
tracking typestate is useless, or 2/ using an indexed monad for
typestate is awful. While it's absolutely possible that (1) holds,
there's an argument for (2). In fact, that indexed monads are a bad
way to handle typestate was a big motivation for the original
`Linear Types proposal`_.

Let's first steelman this argument by designing an API for indexed
monads and sockets' typestate. The best approach is probably to take
`s`, `t`, … to be type-level lists, the name of a socket, instead of
being generated by an existential can be the position of that socket
in the list. It'd look something like

::

   socket :: IO s (s ++ [Unbound]) (Socket (Length s))
   bind :: HasAt n s Unbound => Socket n -> SocketAddress -> IO s (Replace n s Bound) ()
   listen :: HasAt n s Bound => Socket n -> IO n (Replace n s Listening) ()
   ...

Probably the hard technical bit here is to convince GHC that ``HasAt n
(Replace n s Bound) Bound`` holds. Maybe it can be done with a plugin?
Presumably with two sockets with type level names ``n`` and ``n'``,
we'd also need to be able to express that ``n`` and ``n'`` are
distinct. This looks like this API works fine when ``s`` is a concrete
list, but be quite tricky otherwise.

None of these are issues with linear constraints, because the solver
understand what it means to add a constraint (and, thanks to
linearity, to make a constraint unavailable for subsequent
computations). We don't even need to care that ``n`` and ``n'`` are
different. If we have ``(Bound n, Bound n')``, we can ``bind @n``,
then ``bind @n'`` (the API ensures that ``n`` and ``n'`` are distinct,
but it's irrelevant for the solver).

A monad as a source of uniqueness
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An existing way of using the duplication of a comonoid implicitly is
with a (linear) reader monad.

::

   newtype Reader e a = Reader (e %1 -> a)
   instance (Dupable e) => Monad (Reader e)

Could that replace the dupable constraint? Notably for the main
use-case which is to create new linear values.

::

   instance Dupable LinearityToken
   linearly :: Reader LinearityToken (Ur a) %1 -> Ur a
   new :: Int -> Reader LinearityToken (MArray a)

   linearly $ do
     arr1 <- new 42
     arr2 <- new 57
     ...

The short answer is “it doesn't”. This can be done today, and it's not
how people design APIs. Some APIs use linearity tokens like this to
create linear values. When passing comonoid around, in practice, it
seems that people prefer duplicating the elements manually, linear
base lets you get as many copies as you want in one line so it's
decently convenient:

::

   let !(a1, …, an) = elim (,…,) (dup a0)

Using a reader monad in this fashion is useful when your code is
already monadic. But if it is, you probably don't need this whole
linearity token business to begin with.

Multiplicity polymorphism
^^^^^^^^^^^^^^^^^^^^^^^^^

We chose to only allow only ``%1`` as the syntax for a multiplicity
in a linear constraint arrow. A possible alternative to this would be
to follow the approach for linear function type arrows and allow the
value of the multiplicity to be an ``atype``. This allows the user to
supply many different values e.g. variables, type applications, etc.

Mostly this would allow for multiplicity polymorphism on the
constraint arrow, like we have in the function arrow. But we don't
currently have a theory of constraint solving with givens that aren't
either linear or unrestricted.

Besides the fact that not knowing how to achieve this result
technically is good enough reason not go to there, there is not much
of a case for polymorphism on the constraint arrow. Where polymorphism
is needed is in higher-order functions, like
``map :: (a %p -> b) -> [a] %p -> [b]``. But there just aren't that many higher-order
functions with constraint arguments. And when there are, such as
``linearly :: (Linearly %1 => Ur a) -> Ur a``, we usually either
always want an unrestricted constraint or always want a linear
constraint.

Unrestricted annotation on constraint arrows
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Even if we don't allow arbitrary multiplicity annotation on the
constraint arrow, we could still choose to allow ``%Many =>`` for the
sake of symmetry (and occasional emphasis).

We have no particular reason to choose one rather than the other, so
we went for the laziest option.

A dedicated extension
^^^^^^^^^^^^^^^^^^^^^

We chose to modify the ``-XLinearTypes`` extension. Instead we could
create a new extension ``-XLinearConstraints`` without which it isn't
allowed to write ``%1 =>`` (``-XLinearConstraints`` would presumably
imply ``-XLinearTypes``).

We preferred modifying the existing extension, since this is a very
small change to require its own extension, linear constraints are
very strongly thematically related to linear types, the
``-XLinearTypes`` extension is still evolving anyway, and the change
is fully backward compatible.

Unrestricted modifier for constraints
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We could have an equivalent of ``Ur`` for constraint (let's write it
``UrC`` in this section). That is

::

   UrC C %1 => T  ≈ C => T

It doesn't seem quite useful at this point. Instead of

::

   (C, UrC D) %1 => T

We can write

::

   C %1 => D => T

It doesn't make a lot of difference. We may find out, with practice,
that having ``UrC`` would be preferable, but specifying it today seems
premature. Note that because of the limitations on the constraint
solver (specifically Lemma 5.5 from the paper_), ``UrC`` can't be
defined in user-land, it would need to be a specially understood
constructor.

In practice ``Ur`` is most useful when returning values, rather than
taking them as an argument. But in this case we are packaging
constraints in data types, and its easy to require them to be
unrestricted (if it ever shows up, since returning an unrestricted
constraint a rather niche concern):

::

   data AndUr a c where
     MkAndUr :: c => a -> AndUr a c

   f :: T %1 -> S `AndUr` C

Linear instance contexts
^^^^^^^^^^^^^^^^^^^^^^^^

This proposal doesn't specify a way for instance contexts to be
linear. The motivation is that there haven't been examples of instances
with linear context, so we doubt it's worth the implementation
cost. There's a relatively clear semantic that we can give to linear
instance contexts:

::

  instance Lin %1 => Unr => <head> where
    f -- The type class declares f :: F

For such an instance to be well-typed, it must define a single
method. The body of this method is typechecked against the type
signature

::

   Lin %1 => Unr => F

The paper_ handles such axioms. For their soundness, the only thing
that we need is to ensure that their desugaring is correct (which is
the case in this solution).

Additive product in quantified constraints
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As described, in the constraint solver, there are actually two kinds
of products: the multiplicative product, which aggregates constraint
uses of applications, and the additive product, which aggregates
constraint uses of case alternatives.

The additive product is only ever applied on wanteds, so it's largely
invisible to the programmer. However, the logic fragment from
`Efficient resource management for linear logic proof search`_, whose
constraint solving algorithm we use, has support for additive products
in givens (just like without ``-XQuantifiedConstraint``, constraint
implication can only be found in wanteds, but they are allowed in givens
when the extension is turned on).

So it would be natural that when ``-XLinearTypes`` and
``-XQuantifiedConstraint`` are both on, we'd allow additive product on
given constraints. This presumably would be a rather mild extension
(though some (possibly a lot of!) care would be required to make sure
that the resulting algorithm remains guess-free).

But this is a little bothersome, we'd have to settle on syntax too. So
before dedicating work to this sort of thing, we'd rather that a real
need has arisen.

More methods in linearly consumed type class
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We could loosen the requirement that type classes have exactly one
method for them to be used linearly. Instead we could require the
class to have a single *linear* method, and that all the other methods
be unrestricted (see *Additive dictionaries* below for even less
restrictions). In which case using the one linear method counts as
consuming the type class once.

We'd need a way to specify unrestricted methods, we could use the same
syntax as records in the `Linear Types proposal`_

::

   class C a where
     lin :: a -> T
     unr %Many :: a -> U

But for this relaxed condition to be useful we would need to be able
to call ``unr`` on a linear instance of ``C`` *without consuming it*.

In other words, we would need something like:

::

   lin :: C a %1 => a -> T
   unr :: C a %0 => a -> U

The calls to ``unr`` are free, they don't count toward the
exactly-once consumption of the instance. This ``0`` wouldn't mean
“erased at runtime” as has sometimes been proposed. Arnaud is pretty
convinced that this particular ``0`` is a desirable feature for Linear
Haskell, but there's little telling what kind of havoc it would wreak
on multiplicity inference in its current state. So we'd rather keep
this extension for a later time.

Additive dictionaries
^^^^^^^^^^^^^^^^^^^^^

We could go even further than *More methods in linearly consumed type
class* and interpret type class dictionaries as being additive
products.

To recapitulate, linear logic has two products: the multiplicative
product (⊗) and the additive product (&). In Linear
Haskell, all the algebraic datatypes are interpreted as being (sums
of) multiplicative products, additive product can be encoded.

In this proposal we've been assuming that type-class dictionaries have
multiplicative-product types. This is because today, in GHC
(specifically in Core), dictionaries have ordinary algebraic types.

But they don't have to. We could make a special type for dictionaries
which would be interpreted as additive (it doesn't really make a
difference at toplevel, so both interpretation are compatible with
current Haskell). In this case, calling any method of any type class
would be linear, which is much more theoretically satisfying.

We aren't proposing this because

- This is a much bigger change. For one thing it affects Core, where
  dictionaries are materialised. Core doesn't check linearity after
  optimisation (because it's too hard) but it does check linearity of
  the desugarer's output.
- Even if type classes were additive products, it's unclear we could use
  multiple-method type classes linearly. To have a linear instance, we
  need to build an instance which depends on a linear variable. The
  only way to do that is with type class reflection. There is no
  reflection mechanism today for type classes with more than one
  method (see, *e.g.*, `withDict
  <https://hackage.haskell.org/package/ghc-prim-0.13.0/docs/GHC-Magic-Dict.html#t:WithDict>`_). This
  would have to be designed, and it's quite the can of worm.

This proposal is, anyway, forward compatible with this
alternative. So we're proposing to avoid this complication.

Dupable classes
^^^^^^^^^^^^^^^

We specified dupable type classes in a way that make all dupable type
classes isomorphic. The reason for the design is that what we actually
have in mind in the long run is to be able to add unrestricted methods
to the type class as in the alternative above. This design has the
advantage that it's reasonably easy to implement, only wiring in a
type (``LinearityToken```) and two functions
(``consumeLinearityToken`` and ``dup2LinearityToken``). Nevertheless
there are a number of other ways to go about dupable classes.

Leave it out
~~~~~~~~~~~~

Let us point out that while the ability to form a ``Linearly``
constraint is both quite useful and absolutely at home in this
proposal, it's also perfectly consistent to make a linear constraints
proposal without any dupable type classes. If the design of this
feature proves too controversial, it's definitely an option to simply
excise the dupable class feature from the proposal.

Kind of LinearityToken
~~~~~~~~~~~~~~~~~~~~~~

The proposal deliberately specifies that ``LinearityToken`` be 0
width, and the ``dup2`` and ``consume`` functions to correspondingly
return unboxed tuples. This is meant to emphasise that this is all for
low level manipulation and making sure that there is no cost in
storing ``LinearityToken``. These are all meant to be used while
defining a dupable type class and its API, but it's not intended for
them to be apparent in said API.

An somewhat middle-ground option is to expose

::

  data LinearityToken# :: ZeroBitType

  data LinearityToken = MkLinearityToken LinearlyToken#

  -- Both functions below can be defined as easily inlineable thin
  -- wrapper so that in most cases no allocation is needed
  consumeLinearityToken :: LinearityToken -> ()
  dup2LinearityToken :: LinearityToken -> (LinearityToken, LinearityToken)

Just Linearly
~~~~~~~~~~~~~

The most useful dupable type class in the proximate future (in fact
the only known example yet; a dupable ``Read`` capability for mutable
data structures could be another example, with the idea that it'd be
use akin to Rust's immutable borrowing, but it's far from certain that
it'd work as intended), is ``Linearly``. So another option, to avoid
introducing any ad hoc syntax is to simply expose the (abstract)
``Linearly`` constraint from the ``GHC.Constraint.Linear`` module *and
nothing else*. So that ``GHC.Constraint.Linear`` would be

::

  module GHC.Constraint.Linear where

  -- Magically dupable
  class Linearly

  linearly'sToken :: Linearly %1 => LinearityToken

  data LinearityToken :: ZeroBitType

  consumeLinearityToken :: LinearityToken -> (# #)
  dup2LinearityToken :: LinearityToken -> (# LinearityToken, LinearityToken #)


This is quite economical from a language extension perspective, but
the authors of this proposal are somewhat worried of the difficulties
of wiring in a type class.

That being said having just the linearly type class is forward
compatible with pretty much any further plan, since the ``Linearly``
type class is abstract and can be later implemented in terms of a more
general feature.

Linearly in GHC's library
~~~~~~~~~~~~~~~~~~~~~~~~~

Even if ``Linearly`` isn't primitive, considering that it's so useful,
and so prototypical of the use of dupable constraints, it may be worth
exporting ``Linearly`` from ``GHC.Constraint.Linear`` in addition to
the primitives.

We're trying to be minimal in what we add to GHC, and would rather
define ``Linearly`` in the linear-base library.

Magic methods (1)
~~~~~~~~~~~~~~~~~

Instead of fixing the type ``LinearityToken``, we can let the one
method be of any type, but provide the duping functions to the type
class.

In this type, a dupable type class declaration could look like

::

   %Dupable class Foo where
     consm %Consume %Many :: T -> ()
     dupl %Dup2 %Many :: T -> (T, T)
     foo :: T

(the names of the modifiers would be part of the API, but the name of
the methods, themselves, are free). As described here this design
requires unrestricted fields in classes (see above). But see next
section.

Magic methods (2)
~~~~~~~~~~~~~~~~~

Having to define duplication functions for each instance is not
particularly desirable: these methods are properties of the class, not
the instance. So we could specify the corresponding functions when
creating the class.

::

  %Dupable class Foo where
    consm %Consume = … -- Required to be of type T -> ()
    dupl %Dup2 = … -- Required to be of type T -> (T, T)
    foo :: T

But this sort of static method doesn't exist in GHC, this sounds like
a rather large departure from the status quo.

Type class requirement
~~~~~~~~~~~~~~~~~~~~~~

Another possible interface for dupable type classes could be to define
a type class

::

  class Dupable a where
    consume :: a -> ()
    dup2 : a -> (a, a)

Then, dupable class must still have a single method, say of type
``T``, and defining a dupable class requires ``Dupable T``.

This has a few implications: we need to design the precise methods of
the ``Dupable`` type class (the one above are fine, but maybe there
are other options that mesh better with efficient implementation,
linear-base, for instance, defines ``Dupable`` `differently
<https://hackage.haskell.org/package/linear-base-0.4.0/docs/Data-Unrestricted-Linear.html#t:Dupable>`_,
we may also prefer unboxed tuples.), we need to wire-in the
``Dupable`` type class, we need to access the ``Dupable T`` dictionary
when emiting evidence for dupable classes (this may not be easy to
implement).

This is a backward compatible extension to fixing the
``LinearityToken`` type (as long as we make sure that ``Dupable
LinearityToken`` is well-kinded).

Unresolved Questions
--------------------

N/A


Implementation Plan
-------------------

The initial prototype implementation, by Csongor Kiss, is available
`here
<https://archive.softwareheritage.org/browse/revision/f6fc5ba23770b42d1d6020e177787757b16a9ea0/?origin_url=https://github.com/kcsongor/ghc&snapshot=aa61d803eaec9eb4425e3eb8ed2b0fbbd60633cc>`_. The
implementation of this proposal will build upon this foundation and
will be carried out by Arnaud Spiwack. Rebasing the prototype has
proved quite difficult, so the current plan is a reimplementation,
using the prototype as a reference. The work in progress can
be followed `here
<https://github.com/tweag/ghc/tree/linear-constraints>`_.

Endorsements
-------------

.. rubric:: Footnotes

.. [#super-class-with] This proposal interprets type class
                       dictionaries as multiplicative products, if we
                       went with the *Additive dictionaries*
                       alternative, it would be possible to have
                       linear super-class axioms but ``withOneOrd``
                       would have to consume its argument both in the
                       ``compare`` method and the ``Eq a`` dictionary,
                       rather than only the ``compare`` method as in
                       the example.
