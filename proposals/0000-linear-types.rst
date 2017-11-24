.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. sectnum::

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/91>`_.

.. contents::

Linear Types
============

This proposal introduces a notion of *linear function* to GHC. Linear
functions are regular functions that guarantee that they will use
their argument exactly once. Whether a function ``f`` is linear or not
is called the *multiplicity* of ``f``. We propose a new language
extension, ``-XLinearTypes``, to allow users to annotate functions
with their multiplicities.

When turned on, the user can enforce a given multiplicity for ``f``
using a type annotation. By constraining the multiplicity of
functions, users can create library API's that enforce invariants not
otherwise enforceable with current Haskell.

The theory behind this proposal has been fully developed in a peer
reviewed conference publication that will be presented at POPL'18. See
the `extended version of the paper <https://arxiv.org/abs/1710.09756>`_.

Motivation
----------

Type safety enforces that *well-typed programs do not go wrong*.
Programs will sometimes crash, or fail to terminate, but they do not
segfault. Through well-chosen abstractions, types can be used to
enforce further properties, such as trees being well-balanced. One
such further property is *resource safety*, namely that,

1. system resources only change state through legal transitions from
   one state to another,
2. state transitions happen in a timely manner.

For example, a file handle transitions from open to closed, but never
from closed to open. We want to enable users to program file I/O API's
that statically enforce that all I/O happens only on open handles,
never on closed handles (*no use-after-free*). Moreover, we want such
API's to enable early closing of handles by the user (*prompt
deallocation*). Use-after-free and prompt deallocation are hard to
impossible to enforce with current Haskell.

This proposal hits another goal as a side benefit. In Haskell, impure
computations are typically structured as a sequence of steps, be it in
the ``IO`` monad or in ``ST``. The latter in particular serves to
precisely control which effects are possible and the scope within
which they are visible. But using monads to write "locally impure"
computations that still look pure from the outside has an unfortunate
consequence: computations are over-sequentialized, making it hard for
the compiler to recover lost opportunities for parallelism.

Linear types enable better solutions to both problems:

1. using types to guarantee resource safety, and
2. using types to control the scope of effects without forcing an
   unnatural sequencing of mutually independent effects.

In the `companion paper <https://arxiv.org/abs/1710.09756>`_ to this
proposal, we have worked out in detail several use cases for linear
types. We argue that linear types have far ranging consequences for
the language. Salient use cases from the paper include:

- Safe mutable arrays with a safe *non-copying* `freeze` operation.
- Off-heap memory that enables allocating, reading, writing and
  freeing memory safely, without use-after-free or double-free errors.
  This is an important use case for latency sensitive systems
  programming, where moving objects off-heap, out of the purview of
  the GC, is beneficial for avoiding long GC pauses and achieving
  predictable latencies. A prototype is implemented in the
  `linear-base library
  <https://github.com/tweag/linear-base/blob/master/src/Foreign/Marshal/Pure.hs>`_.
- Safe zero-copy data (de)serialization, a notoriously difficult
  endeavour that is in fact so error prone without linear types that
  most production systems today typically avoid it.
- Safe and prompt handling of system resources like files, sockets,
  database handles etc. A `blog post
  <http://www.tweag.io/posts/2017-08-03-linear-typestates.html>`_
  demonstrates this use case in more detail, including tracking the
  state of sockets in types.
- Statically enforced communication protocols between distributed
  processes communicating via RPC.

The keyword in the above examples is **safety**. This proposal is not
about improving the performance of the compiler's generated code. It
is not about new runtime support. It is about enabling programmers to
build safer API's that enforce stronger properties, thereby bringing
*possible* but otherwise high-risk optimization techniques, like
managing memory manually, into the realm of the *feasible*.

Resource-safety or any other property are *not* an inherent property
of linear types. They are properties of API's making careful use of
linear types.

The use cases put forth above are diverse and pervasive. Yet they are
but a few examples of the safety properties that can be conveniently
captured with linear types. Here are a few more:

- @gelisam designed `a linear API
  <https://github.com/gelisam/linear-examples>`_ for `3d-printable
  models
  <https://www.spiria.com/en/blog/desktop-software/making-non-manifold-models-unrepresentable>`_.
- @facundominguez `shows how linear types
  <http://www.tweag.io/posts/2017-11-29-linear-jvm.html>`_ make it
  possible to safely manage two GC heaps managed by two separate GC's,
  but shared between two language runtimes.

Proposed Change Specification
-----------------------------

.. _Specification:

We introduce a new language extension. Types with a linearity
specification are syntactically legal anywhere in a module if and only
if ``-XLinearTypes`` is turned on.

This proposal only introduces a new type for functions. It does not
affect the runtime system, optimization passes or code generation.

Definition
~~~~~~~~~~

We say that a function ``f`` is *linear* when ``f u`` is consumed
exactly once implies that ``u`` is *consumed exactly once* (defined
as follows).

- Consuming a value of a data type exactly once means evaluating it to
  head normal form exactly once, then consuming its fields exactly
  once
- Consuming a function exactly once means applying it and consuming
  its result exactly once

The type of linear functions from ``A`` to ``B`` is written ``A ->.
B`` (see Syntax_).

Linearity is a strengthening of the contract of the regular function
type ``A -> B``, which will be called the type of *unrestricted*
functions.

Remark: linear function ``f`` can diverge (*i.e.* either not terminate
or throw an exception) or be called on diverging data. In this case,
``f`` will not necessarily consume its argument. This is fine: we can
still build safe programming interfaces, as explained in the
Exceptions_ section below).

Polymorphism
~~~~~~~~~~~~

In order for linear functions and unrestricted functions not to live
in completely distinct worlds, to avoid code duplication, we
introduce a notion of polymorphism, dubbed *multiplicity polymorphism*,
over whether a function is linear.

A linear function is said to have multiplicity ``1`` while an
unrestricted function is said to have multiplicity ``ω``. Multiplicity
polymorphic functions may have variable multiplicity (see also Syntax_), *e.g.*

::

  map :: (a :p-> b) -> [a] :p-> [b]

Without polymorphism, we would need two implementations of `map` with
the exact same code: one for ``p=1`` and one for ``p=ω``. Function
composition is even worse: it takes two multiplicity parameters,
hence, would require four identical implementations:

::

  (.) :: (b :p-> c) -> (a :q-> b) -> a :(p ':* q)-> c

Syntax
~~~~~~

.. _Syntax:

*This section is only a proposal for a syntax for linear types. For
other possibilities to consider, see the `Alternatives`_ section.*

This proposal adds only one new syntactical construct:

- The multiplicity annotated arrow, for polymorphism, is written
  ``a :p-> b`` (where ``a`` and ``b`` are types and ``p`` is a
  multiplicity). To avoid introducing a new notion of "mixfix"
  operators, we introduce a familly of (infix) type constructors:
  ``(:p->)`` for each multiplicity ``p``. This technically steals
  syntax as ``(:)`` is a valid type operator under the discouraged
  ``-XDataKinds`` syntax. But this should not be a problem in
  practice.

  - In ``(:p->)``, ``p`` can be any type expression of kind
    ``Multiplicity`` (see below). So that the following is legal
    (though see Alternatives_):

    ::

      type familly F (a :: *) :: Multiplicity
      f ::  forall (a :: *). Int  :(F a)-> a -> a

In the fashion of levity polymorphism, the proposal introduces a data
type ``Multiplicity`` which is treated specially by the type checker,
to represent the multiplicities:

- ::

    data Multiplicity
      = One    -- represents 1
      | Omega  -- represents ω

- Accompanied by two specially recognised type families:

  ::

    type family (:+) :: Multiplicity -> Multiplicity -> Multiplicity
    type family (:*) :: Multiplicity -> Multiplicity -> Multiplicity

  Note: unification of
  multiplicities will be performed up to the semiring laws for
  ``(:+)`` and ``(:*)`` (see Specification_).

The linear and unrestricted arrows are aliases:

- ``(->)`` is an alias for ``(:'Omega ->)``
- ``(->.)`` (ASCII syntax) and ``(⊸)`` (Unicode syntax) are aliases
  for ``(:'One ->)``

Constructors & pattern-matching
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Constructors of data types defined with the Haskell'98 syntax

::

  data Foo
    = Bar A B
    | Baz C

have linear function types, that is ``Bar :: A ->. B ->. Foo``. This
is true in every module, including those without ``-XLinearTypes``
turned on. This implies that most types in ``base`` (``Maybe``,
``[]``, etc…) have linear constructors. We also make the constructor
of primitive tuples ``(,)`` linear in their arguments.

With the GADT syntax, multiplicity of the arrows is honoured:

::

  data Foo2 where
    Bar2 :: A ->. B -> C

means that ``Bar2 :: A ->. B -> C``. This means that *data types
written in GADT syntax with the ``(->)`` arrow are not the same as if
they were defined with Haskell'98 syntax*.

The definition of consuming a value in a data type exactly once must
be refined to take the multiplicities of fields into account:

- Consuming a value in a datatype exactly once means evaluating it to
  head normal form and consuming its *linear* fields exactly once.

When pattern matching a linear argument, linear fields are introduced
as linear variables, and unrestricted fields as unrestricted
variables:

::

  f :: Foo2 ->. A
  f (Bar2 x y) = x  -- y is unrestricted, hence does not need to be consumed

An exception to this rule is ``newtype`` declarations in GADT syntax:
``newtype``-s' argument must be linear (see Interactions_
below).

Base
~~~~

Because linear functions only strengthen the contract of unrestricted
functions, a number of functions of ``base`` can get a more precise
type. However, for pedagogical reasons, to prevent linear types from
interfering with newcomers' understanding of the ``Prelude``, this
proposal does not modify ``base``. Instead, we expect that users will
publish new libraries on Hackage including more precisely typed
``base`` functions. One such library has already started `here
<https://github.com/tweag/linear-base>`_.

Any linear variant of ``base`` need not redefine any of the data types
defined in ``base``. This is because like for all other data types,
constructors of (non-GADT) data types in ``base`` are linear under
this proposal. Since we get to reuse data types, libraries
implementing linear variants of ``base`` functions remain compatible
with ``base`` (e.g. there need not be two ``Maybe`` types, two list
types etc).

The only function which will need to change is ``($)`` because its
typing rule is built in the type checker. Ignoring the details about
levity and higher-rank polymorphism in the typing rule, the type
``($)`` will be:

::

  ($) :: (a :p-> b) ⊸ a :p-> b

Defining a linear variant of ``base`` is out of scope of this
proposal. Possible future standardisation of the library content is
the competence of the Core Libraries Committee (CLC). For expository
purposes of the next sections, however, we assume that such a library
will at least define the following data type:

::

   data Unrestricted a where
     Unrestricted :: a -> Unrestricted a

See the paper for intutions about the ``Unrestricted`` data type.

Multiplicities
~~~~~~~~~~~~~~

.. _Multiplicities:

So far, we have considered only two multiplicities: ``1`` and ``ω``.
But the metatheory works with any so-called sup-semi-lattice-ordered
semi-ring (without a 0) of multiplicities. That is: there is a 1,
a sum and a product with the usual distributivity laws, a (computable)
order compatible with the sum and product, such that each pair of
multiplicities has a (computable) join. Even if there is only two
multiplicities in this proposal, the proposal is structured to allow
future extensions.

Here is the definition of sum, product and order for this proposal's
multiplicities (in Haskell pseudo-syntax):

::

   _ + _ = ω

   1 * x = x
   x * 1 = 1
   ω * ω = ω

   _ ⩽ ω = True
   x ⩽ y = x == y

Every variable in the environment is annotated with its multiplicity,
which constrains how it can be used. A variable *usage* is said to be
of multiplicity ``p``, or ``0``, in a term ``u`` if:

- ``p=0`` and ``x`` is not free in ``u``
- ``p=1`` and ``u = x``
- ``p=p1+q*p2`` and ``u = u1 u2`` with ``u1 :: a :q-> b`` and the
  usage of ``x`` in ``u1`` is ``p1``, and in ``u2`` is ``p2``
- ``u = λy. v`` and the usage of ``x`` in ``v`` is ``p``.

A variable's usage is correct if it is smaller than or equal to the
multiplicity annotation of the variable. Incorrect usage results in
a type error. This definition is close to the intended implementation
of multiplicities. The `paper <https://arxiv.org/abs/1710.09756>`_ has
a more declarative definition.

The multiplicity of a variable introduced by a λ-abstraction is taken
from the surrounding typing information (typically a type annotation
on an equation). For instance

::

  foo :: A :p-> B
  foo x = …  -- x has multiplicity p

The above takes care of the pure λ-calculus part of Haskell. We also
need to consider ``let`` and ``case``.

A ``let`` binding is considered to have an implicit multiplicity
annotation (the annotation is inferred). The variables introduced by a
``let`` bindings with annotation ``p`` all have multiplicity
``p``. And the usage of ``x`` in ``let_p {y1 = u1; … ;yn = un} in v``
(where the ``yi`` are variables) is ``p*q1 + … + p*qn + q`` where the
usage of ``x`` in ``ui`` is ``qi`` and in ``v`` is ``q``.

If a let has recursive binders, then ``p`` must be ``ω``.

A ``case`` expression has an implicit multiplicity annotation, like
``let`` binding. It if often inferred from the type annotation of an
equation. The usage of ``x`` in ``case_p u of { … }``, where the usage
of ``x`` in ``u`` is ``q`` is ``p*q`` plus the *join* of the usage of
``x`` in each branch.  Note that, in usages, ``0 ≰ 1`` as arguments
with multiplicity ``1`` are consumed exactly once, which doesn't
include not being consumed at all.

The multiplicity annotation of variables introduced by a pattern depend
on the constructor and on the implicit annotation of the
``case``. Specifically in ``case_p u of {…; C x1 … xn -> …; …}`` Where ``C :: a1 :q1-> … an :qn-> A``,
Then ``xi`` has multiplicity annotation ``p*qi``. For instance

::

  bar :: (a,b) :p-> c
  bar (x,y) = … -- Since (,) :: a ->. b ->. (a,b), x and y have
                -- multiplicity p

.. _Patterns

Deep patterns & multiple equations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Type-checking deep patterns naturally extends the simple patterns
above. For instance in

::

  f :: Maybe (a, b) ->. …
  f (Just (x,y)) = …

since the type annotation on the first argument is linear, the outer
pattern is type-checked as a ``case_1``:

::

  f mxy = case_1 mxy of
    Just xy -> …

Therefore, the generated intermediate variable ``xy`` has multiplicity
1, therefore, the inner pattern is elaborated as a ``case_1`` (that is
the same multiplicity as the intermediate variable).

::

  f mxy = case_1 mxy of
    Just xy -> case_1 xy of
      (x, y) -> …

Unresolved questions
++++++++++++++++++++

It is not currently clear whether we can accept wildcard patterns in
linear patterns.

::

  m :: (a->a) -> Maybe a ->. Maybe a
  m f (Just x) = Just (f x)
  m f y        = y

It would be convenient for the programmer, but with the current typing
rules for Core, the generated Core would not be well-typed as the
second argument would be seen as non-linear (see the `The Core Corner`_ and
`Unresolved questions`_ section below for more details).

Wildcard patterns are especially useful in multiple-equations

::

  mix :: [a] ->. [a] ->. [a]
  mix [] ys = ys
  mix xs [] = xs
  mix (x:xs) (y:ys) = x:y:(mix xs ys)

Let us illustrate the issue, with a variant of the previous example.

::

  data Colour
    = Red
    | Green
    | Blue

  f :: Colour ->. Colour ->. Colour
  f Red q = q
  f p Green = p
  f Blue q = q

The function ``f`` is desugared into the following Core expression

::

  f = \ p q -> case_1 p of p2
     { Red -> q
     ; WILDCARD ->
         case_1 q of q2
         { Green -> p2
         ; WILDCARD ->
             case_1 p2 of p3
             { Blue -> q2
             ; WILDCARD -> <error> }}}

However the usage of ``p2`` and ``q2`` is different in different
branches of the ``case``, so they are not used linearly, at least not
using our typing rules. To accept code such as the above, we would
need a typing rule, in Core, which accepts the desugaring of ``f``.

If this sort of code is allowed (that is, the generated code is
accepted in Core), then multiple-equations and deep pattern pose no
difficulty. Otherwise the surface language will have to include a
criterion which makes the above set of equations unsound, while
accepting the code below:

::

  mix :: [a] ->. [a] ->. [a]
  mix [] ys = ys
  mix (x:xs) [] = x:xs
  mix (x:xs) (y:ys) = x:y:(mix xs ys)

It is not clear how to define such a criterion.

Unboxed data types
~~~~~~~~~~~~~~~~~~

GHC supports unboxed data types such as ``(#,#)`` (unboxed pair) and
``(#|#)`` (binary unboxed sum). The proposal treats them as their boxed
equivalent (``(,)`` and ``Either``, respectively, for these two
examples): the constructors are linear (and case can have various
multiplicities).

.. _Subtyping

Subtyping
~~~~~~~~~

The type ``A->.B`` is a strengthening of ``A->B``, but the type
checker doesn't do subtyping. It relies on polymorphism
instead. However, following the definition above, note that

::

  f :: A ->. B

  g :: A -> B
  g = f  -- should not be well-typed
  g x = f x  -- is well-typed

It would be unfortunate if this rule was actually enforced: for instance a linear function in a
library could not be used with ``map`` from base. Which means that
everybody would have to start caring about linearity. Worse: every use
of ``map Just`` would now be untyped. Fortunately, this sort of
opportunity is easily detected and the former definition of ``g`` is
understood as the latter, well-typed, one. It means that is not a
breaking change to strengthen a *first-order* regular arrow ``->``
into a linear ``->.`` in an interface.

Records and projections
~~~~~~~~~~~~~~~~~~~~~~~

Records constructors are linear. That is. in

::

   data R = R {f1 :: A1, … fn :: An}

we have ``R :: A1 ->. … ->. An ->. R``. Projections
take an *unrestricted* record as argument: ``f1 :: R -> A1`` (because
otherwise the other fields would not be consumed). There is an
exception to this rule: if all the other fields are unrestricted (in
the current proposal, it means that ``f1`` is the *only* field, but
see `Binders with multiplicity`_), then ``f1`` is made linear:
``f1 :: R ->. A1``. This non-uniformity is justified by the standard
``newtype`` idiom:

::

  newtype Foo = Foo { unFoo :: A }

which becomes much less useful in linear code if ``unFoo :: Foo ->
A``. Our practice of linear Haskell code indicates that this feature,
while a mere convenience, is desirable (see *e.g.* `here
<https://github.com/tweag/linear-base/blob/e72d996b5d0600b2d5f2483b95b064d524c83e46/src/System/IO/Resource.hs#L59-L61>`_).

Inference
~~~~~~~~~

.. _Inference:

Because of backwards compatibility, we initially chose the following
strategy: when the type of a function is not constrained by given
constraints, we conservatively assume it to have multiplicity ω.

Experience shows that this sometimes yield very confusing error messages
where perfectly valid code is rejected:

::

  type family L x
  type instance L Int = A ->. A

  f :: L x -> x

  u :: Int
  u = f (\x -> x)

While the identity function is indeed linear, because the resolution
of the type family (``L Int ~ Int``) is delayed in GHC, ``\x -> x`` is
considered to have no given type, and is inferred to have a non-linear
type, and is refused by the type-checker.

We therefore need a more refined strategy, to avoid surprising
behaviour like the above. We do not expect it to be too hard to
implement a better strategy, but we don't have a specification yet.

A more profound difficulty exists for inference: for explicit ``let``
bindings and ``case`` expressions (*i.e.* which are not generated from
the desugaring of an equation but are written as ``let``, ``where``,
or ``case`` in the surface syntax), we want to infer the multiplicity
annotation. The process for this is not yet defined (see `Unresolved
questions`_ below for a more precise description of this issue).

.. _Exceptions

Non-termination, exceptions & catch
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the presence of non-termination or exceptions, linear functions may
fail to fully consume their argument. We can think of it as: the
consumption of the result of the function was never complete, so the
consumption of the argument need not be either. However, because
exceptions can be caught, a program can observe a state where a value
``v`` has been passed to a linear function ``f`` but the call ``f v``
has exited (with an exception) without consuming ``v``. So while, the
guarantee provided by linear functions holds for converging
computations, we must weaken it in case of divergence:

- Attempting to consume exactly once ``f v``, when ``f`` is a linear
  function, will consume ``v`` exactly once if the consumption of ``f
  v`` converges, and *at most once* if it diverges.

Where "consuming at most once" is defined by induction, like
"consuming exactly once", but every sub-consumption is optional.

In the paper, we gave a simplified specification of a linear ``IO``
monad (called ``IOL``) which ignored the issue of exception for the
sake of simplicity. Can we, still, write a resource-safe ``RIO`` monad
with linear types despite the added difficulty of exceptions? Yes, as
this section will show.

Concretely, how do we ensure that the sockets from the example API are
always closed, even in presence of exceptions? This boils down to how
the ``RIO`` monad is implemented. Below is a sketch of one possible
implementation of ``RIO`` (see `here
<https://github.com/tweag/linear-base/blob/master/src/System/IO/Resource.hs>`_
for a detailed implementation).

First, note that since Haskell programs are of type ``IO ()``, we need a
way to run ``RIO`` in an ``IO`` computation, this is provided by the
function

::

  runRIO :: RIO (Unrestricted a) -> IO a

In order to achieve resource safety in presence of exception, ``runRIO``
is tasked with releasing any live resource in case of exception.

To implement this, ``RIO`` keeps a table of release actions, to be used
in case of exceptions. Each resource implemented in the ``RIO``
abstraction registers a release action in the release action table
when they are acquired.

If no exception occurs, then all resources have been released by the
program. In case an exception occurs, the program jumps to the handler
installed by ``runRIO``, which releases the leftover resources.

An alternative strategy would be to add terminators on every resources
acquired in ``RIO``. Release in the non-exceptional case would still
be performed by the program, and the GC would be responsible for
releasing resources in case of exception. The release in case of
exception would be, however, less timely.

Can ``RIO`` have a ``catch``?
-----------------------------

It is possible to catch exceptions inside of ``RIO``. But in order to
ensure resource safety, the type cannot be linear:

::

  catchL :: Exception e
         => RIO (Unrestricted a)
	 -> (e -> RIO (Unrestricted a))
	 -> RIO (Unrestricted a)

That is: no linear resource previously allocated (in particular linear
variables which are not ``RIO`` resources) can be referenced in the
body or the handler, and no resource allocated in the body or handler
can be returned. In effect, ``catchL`` delimits a new scope, in which
linear resources are isolated. To implement ``catchL``, we simply give
it its own release action table, so that in case of exceptions all the
local resources are released by ``catchL``, as ``runRIO`` does, before
the handler is called. The original release action table is then
reinstated.

With this implementation, it is clear that capturing linear resources
from the outside scope would compromise timely release, and returning
locally acquired resources would leak resources if an exception
occurs.

The latter restriction can be lifted as follows: instead of
reinstating the original release action table in the non-exceptional
case, instate the *union* of the original table and the local one. In
this case the type of ``catchL`` would be the following:

::

  catchL :: Exception e
         => RIO a -> (e -> RIO a) -> RIO a

Even with this type, however, exception handling remains clumsy, and
it may prove more convenient to use a more explicit exception-management
mechanism for linear resources, such as the ``EitherT`` monad.

The choice between these two types (and corresponding implementation)
for ``catch``, or the absence of ``catch`` altogether, is a design
question for the library that implements a monad such as ``RIO``.

Can I throw linear exceptions?
------------------------------

In the type of ``catchL`` above, the type of the handler is ``e -> RIO
a``. Correspondingly, the type of the exception-throwing primitives are:

::

  throwRIO :: Exception e => e -> RIO a
  throw :: Exception e => e -> a

That is, exceptions don't have a linear payload.

While there does not seem to be any conceptual difficulty in throwing
exception with linear payload, we have noticed that, in practice, many
(linearly typed) abstractions which we have come up with rely on
values not escaping a given scope. Barring a mechanism to delimit the
scope of exceptions with linear payload, such linear exceptions may
compromise such abstractions.

To be conservative, and avoid potential such issue, we currently
consider exceptions as only carrying unrestricted payloads in our
library.

Effect and Interactions
-----------------------

.. _Interactions:

A staple of this proposal is:

*it does not modify Haskell for those who don't want to use it, or
don't know about linear types.*

Even if an API exports linear types, they are easy to ignore: just
pretend that the dotted arrows are regular arrows. It is always safe
in all contexts to substitute something with a linear arrow where
a regular arrow was expected.

Linear data types are just regular Haskell types, which means it is
cheap to interact with existing libraries.

Rebindable Syntax
~~~~~~~~~~~~~~~~~

There is an unpleasant interaction with ``-XRebindableSyntax``: ``if
u then t else e`` is interpreted as ``ifThenElse u t e``.
Unfortunately, these two constructs have different typing rules when
``t`` and ``e`` have free linear variables. Therefore well-typed
linearly typed programs might not type check with
``-XRebindableSyntax`` enabled.

Unrestricted newtypes
~~~~~~~~~~~~~~~~~~~~~

The meta-theory of linear types in a lazy language fails if we allow
unrestricted ``newtype``-s:

::

  newtype Unrestricted' a where
    Unrestricted' :: a -> Unrestricted' a

Intuitively, this is because forcing a value ``v :: Unrestricted a``
has the consequence of consuming all the resources in the closure of
``v`` making it safe to use the value many times or not at all. But
newtypes convert ``case`` into a cast, hence the closure is never
consumed. So ``newtype`` must not accept non-linear arrow with
``-XLinearTypes``: the above produces an error (see also `Without
-XLinearTypes`_ below).

These are interpreted as linear ``newtype``-s and a
warning is emitted (see Specification_ above).

Pattern-matching
~~~~~~~~~~~~~~~~

Lazy pattern-matching is only allowed for unrestricted (multiplicity
``ω``) patterns: lazy patterns are defined in terms of projections
which only exist in the unrestricted case. For instance

::

  swap' :: (a,b) ->. (b,a)
  swap' ~(x,y) = (y,x)

Means

::

  swap' :: (a,b) ->. (b,a)
  swap' xy = (snd xy, fst xy)

Which is not well-typed in particular since fst is not.

::

  fst :: (a,b) -> a -- resp. snd
  fst (a,_) = a

So ``swap'`` must be given the type ``(a,b) -> (b,a)``.

Unresolved questions:

- It is unknown at this point whether view patterns can be linear
- It is unknown at this point whether ``@`` pattern of the form
  ``x@C _ _`` can be considered linear (it is as much a practical
  question of whether there is a reasonable way to implement such a
  check as a theoretical question of whether we can justify it).
- There is no account yet of linear pattern synonyms.

Without -XLinearTypes
~~~~~~~~~~~~~~~~~~~~~

.. _`Without -XLinearTypes`:

When using ``-XLinearTypes``, the GADT-syntax equivalent of a
Haskell'98 type declaration uses the linear arrow rather than the
unrestricted arrows, as is customary in Haskell. Worse: GADT-syntax
``newtypes``-s are *rejected* if they use unrestricted arrows.

Since this proposal is completely backwards compatible, GADT-syntax
``newtype``-s must behave differently without
``-XLinearTypes``. GADT-syntax ``data`` definitions need not, but it
is the expectation of the programmer that the following two are
equivalent definitions (which they are not with ``-XLinearTypes``):

::

  data Maybe a
    = Just a
    | Nothing

  data Maybe a where
    Just :: a -> Maybe a
    Nothing :: Maybe a

To follow the principle of least surprise (which we take to mean that
only programmers aware of ``-XLinearTypes`` would be surprised), we
interpret GADT-syntax type declaration (both ``data`` and ``newtype``)
in code without ``-XLinearTypes`` to be *linear*, despite the
ostensible use of an unrestricted arrow.

Costs and Drawbacks
-------------------

Learnability
~~~~~~~~~~~~

This proposal tries hard to make the changes unintrusive to newcomers,
or indeed to the existing language ecosystem as a whole. However, if
many users start adopting it, inevitably, linear arrows may start
appearing in so many libraries that it becomes hard to be oblivious to
their existence. They can be safely ignored, but teachers of Haskell
might still consider them distracting for their students.

Development and maintenance
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The arrow type constructor is constructed and destructed a lot in
GHC's internals. So there are many places in the type checker where
the GHC implementation will have to handle multiplicities. It is most
often straightforward as it consists in getting a multiplicity
variable and pass it to a function. Nevertheless, it is possible to
get it wrong. And type checker developers will have to be aware of
multiplicities to modify most aspects of type checking.

Linear types also affect Core: Core must handle linear types in order
to ensure that core-to-core passes do not break the linearity
guarantees. The flip side is that all core-to-core passes must make
sure that they do not break linearity. It is possible that some of the
pre-linear-type passes actually do break linearity in some cases
(note: there has been no evidence of this so far).

Unification of multiplicity expressions (as for instance in the type
of ``(.)`` above) requires some flavour of unification module
associativity and commutativity (AC). Unification modulo AC is
well-understood an relatively easy to implement. But would still be
a non-trivial addition to the type-checker. We may decide that
a simplified fragment is better suited for our use-case that the full
generality of AC.

Alternatives
------------

.. _Alternatives:

This section describes variants that could be considered for inclusion
in the proposal.

Syntax of the multiplicity-parametric arrow
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The proposed mixfix ``a :p-> b`` syntax for the
multiplicity-parametric arrow makes a potentially non-trivial addition
to the parser. So does the proposed type constructor indexed by a
multiplicity ``(:p->)``.

A way to simplify the changes to the parser would be to have the type
constructor be

::

  ARROW :: Multiplicity -> * -> * -- ignoring levity

It would be very inconvenient to use a prefix notation for
multiplicity parametric arrows: we wouldn't want the type of ``map``
to read

::

  map :: ARROW 'Omega (ARROW p a b) (ARROW p [a] [b])

So we introduce a binary type construction ``WithMult`` (or some
operator syntax). It is a syntax error to use ``WithMult`` anywhere
except to the left of an arrow. And ``WithMult a p -> b`` means
``ARROW p a b``. So that the type of ``map`` becomes:

::

  map :: (a `WithMult` p -> b) -> [a] `WithMult` p -> [b]

Syntax of multiplicity expression
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We proposed that, in ``a :p-> b``, ``p`` could be any expression, as
long as it is of kind ``Multiplicity``. This is simpler in terms of
modifying the parser, but the error messages may be confusing for very
little benefit: in practice we would expect to have polynomial
expressions of multiplicity variables. Plus, any expression beyond
this form is unlikely to be resolved by the type checker
satisfactorily.

So we could decide to restrict ``p`` to the following grammar:

.. code:: bnf

  MULT ::= 'One
         | 'Omega
         | VARIABLE
         | MULT :+ MULT
         | MULT :* MULT

Lexical tokens of the multiplicity-parametric arrow
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Assuming that the multiplicity polymorphic arrow deserves a full-blown
mixfix syntax, here are other notations which have been floated:

- ``(-p->)``
- ``(->_p)`` (using the ``_`` to represent the subscript from the
  paper as in Latex)
- ``(->:p)``. We've used this one a little, and found that it was
  confusing, seeming to attach the multiplicity to the result, where
  it ought to be thought as affecting the argument. The same probably
  apply to ``(->_p)``.

Lexical token of the linear arrow
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We propose ``(->.)`` as a notation for the linear arrow. An
alternative, based on the resemblance with the Unicode notation
``(⊸)`` would be ``(-o)``.

We chose ``(->.)`` because it does not change the lexer (``-o`` is not
a token in current GHC, and ``a-o`` is currently interpreted as ``(-)
a o``). ``-o`` does not convey the intuition that ``->.`` is just
``->`` for most intents and purposes (except for those advanced users
who do care about the distinction).

Other strategies when -XLinearTypes is turned off
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The proposal holds that in absence of ``-XLinearTypes``, GADT-syntax
type declarations are interpreted as linear declarations. This
achieves two purposes:

- For ``data`` declarations: it honours the expectation of the
  programmer unaware of or unfamiliar with ``-XLinearTypes`` that
  Haskell'98 syntax can always be replaced by the appropriate GADT
  syntax without affecting the semantics.
- For ``newtype`` declarations: it makes sure that the existing
  GADT-syntax ``newtype``-s are valid, while must be rejected when
  ``-XLinearTypes`` is turned on.

This choice is aimed at making the life of programmers which don't use
``-XLinearTypes`` as unaffected by the existence of linear types as
possible. On the other hand, one may point out that it will make it so
that turning ``-XLinearTypes`` will change the semantics of
GADT-syntax type declarations. While we believe it to be a lesser
problem, let us outline an alternative plan.

- ``data`` declaration honour the unrestricted arrow annotation even
  with ``-XLinearTypes`` turned off. This means that they are *not*
  equivalent to the corresponding Haskell'98 declaration anymore. This
  would likely mean that users of ``-XLinearTypes`` will want to
  discourage the use of GADT syntax where Haskell'98 syntax even in
  codebases which don't use ``-XLinearTypes``.
- ``newtype`` declarations are always linear. Even if we use
  unrestricted arrows in their definitions. Even with
  ``-XLinearTypes`` turned on. When ``-XLinearTypes`` is on, a warning
  is emitted.

Binders with multiplicity
~~~~~~~~~~~~~~~~~~~~~~~~~

.. _`Binders with multiplicity`:

In the paper, we wrote ``λ x :₁ A. u`` for (unannotated) linear
functions. We don't currently provide a corresponding syntax, for lack
of good syntax.

If a syntax is provided, we could also use this syntax to have records
with different multiplicities.

::

  data R = R { unrestrictedField ::(Omega) A, linearField ::(One) B }


Affine types instead of linear types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. _`Affine types`:

In the presence of exceptions, it may seem that linear functions do
not necessarily consume their arguments. For instance, an ``RIO a``
may abort before closing its file handles. And because of ``catch`` we
are able to be observe this effect. Could affine types agree better
with this reality?

A function is called *affine* if it guarantees that if its returned
value is consumed at most once, then its argument is consumed at most
once.

There are three possible systems we can consider:

1. a system with linear functions (as we are proposing),
2. a system with affine functions,
3. a system with both linear and affine functions.

All three system are consistent and can be easily accommodated in our
formalism. In fact the formalism has been designed with extensibility
in mind, and the proposed implementation is easy to change in order to
cope with affine functions. Therefore the choice between these three
systems is not a fundamental issue of this proposal. We are arguing
for system (1), but it can easily be changed.

We argue against system (2) for the following reasons, expanded upon
below:

* Many API properties crucially rely on linearity.
* Affine types and linear types are *not* equi-expressive (see next
  section).
* Some API properties (not all) can be achieved using linear types in
  direct style, or with affine types in continuation passing style
  (CPS). As is well-established in the literature, programming in
  direct style is easier, less verbose and less error prone than CPS.
  So abandoning the stronger guarantee of linear types would come at
  a cost for API designers.
* While affine types are sufficiently strong to achieve many desirable
  properties, linear types can express them just as well at minimal
  implementation and API design cost.

An example of a direct style API that crucially relies on linearity is
@gelisam's `3D-printable models
<https://www.spiria.com/en/blog/desktop-software/making-non-manifold-models-unrepresentable>`_).
Exceptions can only be caught in the ``IO`` monad, yet this API is
pure. So exceptions are not a concern in the design of this API. The
properties this API wants to enforce hold even with linear types and
even in the face of exceptions being thrown (in a pure or impure
context) and caught (in an impure context). No linear types means this
API would need to use CPS, if that works at all to enforce the same
properties.

Another example is `language interop
<http://www.tweag.io/posts/2017-11-29-linear-jvm.html>`_ by
@facundominguez and @mboes. In this example, Haskell users create GC
roots for every object in the JVM's heap that they want to reference
directly. These GC roots must be released as soon as the reference is
no longer useful. Introducing a ``bracket``-like ``withJvmScope``
action is one way to ensure all roots do get deleted eventually (at
scope exit), but in practice, in complex dual-language projects,
introducing neither too fine-grained or too coarse-grained scopes has
proven very difficult. Furthermore, ``bracket``-like constructs break
tail-recursion. Linear types enable working with a single global
resource scope, while still guaranteeing eventual deletion of roots,
in any order. Affine types do not. At any rate, not in direct-style.

Now, in this latter example, exceptions do impose both an
implementation cost and a design cost. The implementation cost arises
because we want a stronger guarantee: we want to know that all GC
roots are always freed exactly once, so we must register each GC root
to free them if an exception is thrown. A free-at-most-once guarantee
wouldn't require this, but is also not realistic. In the above use
case, we *do* want references to be freed eventually, so we have to
bother with registration either way, whether with affine or linear
types. The design cost is that ``catch`` requires a weaker type than
desirable, as discussed above, limiting its power.

It should be noted that affine types are *sufficient* for many use
cases. Examples: in-place mutation of garbage-collected structures
like mutable arrays. Affine types also make it possible to ascribe
a more precise type to ``catch`` (writing ``'A`` for the affine
multiplicity):

::

  catch :: Exception e => RIO a :'A-> (e -> RIO a) :'A-> RIO a

So affine mutable arrays could be free variables in the body of
a ``catch``. It's not clear yet that this finer type for ``catch``
would actually be useful: the same affine free variable could not
appear both in the body and the handler. The only instance of such
a pattern which we've found documented so far, is in the Alms
programming language, where the ``catch`` is used to perform clean-up,
*i.e.* close a resource, (see `Jesse Tov's thesis p67
<http://users.eecs.northwestern.edu/~jesse/pubs/dissertation/tov-dissertation-screen.pdf#figure.4.7>`_).
We invite the community to come up with more use cases for affine
types and where linear types would impose a high implementation and/or
API design cost.

Finally, while it is easy to implement system (3), we have not
included it in the proposal. We propose to reserve it for a later
proposal (see also `More multiplicities`_ below), while thriving in
this proposal to focus first on the minimal system that adequately
addresses the motivations.

Remarks on the relation between affine and linear types
+++++++++++++++++++++++++++++++++++++++++++++++++++++++

As noted by @rleshchinskiy, we can recover, in a limited case, the
guarantees of linear types in system (2) via an encoding. The idea is
to introduce a type-level name for each resource that we want
linearity guarantees for (this requires to introduce the resource in
continuation-passing). Here is what it would look like for the socket
example:

::

  data Socket (n :: *) (s :: State)
  data Closed (n :: *)

  newSocket :: RIO (forall n. Socket n 'Unbound :'A-> RIO (Unrestricted a, Closed s)) :'A -> RIO (Unrestricted a)
  […]
  close :: Socket n s -> RIO (Closed s)

This, however, requires to release resources in some sort of a
stack-like discipline: if resources are released in an unbounded
out-of-order manner, we can't retain the relation between the resource
names and the type of the expression. Therefore we cannot have, say, a
priority queue of sockets with the above affine API. Whereas linearly
typed priority queues are perfectly fine.

Conversely, affine types can be encoded in linear types (folklore in
the literature):

::

  type Affine a = forall k. Either (a ->. k) k ->. k

  drop :: Affine a ->. ()
  drop x = x $ Right ()

Unfortunately, with this encoding, it is still not easy to give the following
type to ``catch``:

::

  catch :: Exception e => Affine (RIO a) ->. Affine (e -> RIO a) -> RIO a

Therefore, despite the tantalising proximity, system (1) and (2) are
different in practice.

Subtyping instead of polymorphism
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Since ``A ->. B`` is a strengthening of ``A -> B``, it is tempting to
make ``A ->. B`` a subtype of ``A -> B``. But subtyping and polymorphism
don't mesh very well, and would yield a significantly more complex
solution.

In general, subtyping and polymorphism are not comparable, and some
examples will work better with one or the other. Therefore it makes
sense to go for the simplest one.

In this proposal

::

  f :: A ->. B

  g :: A -> B
  g = f

is, in theory, ill-typed. But it would be a problem to reject this
program (especially with all the constructors which have been
converted to linear types). So the type inference mechanism elaborates
this program to the well-typed η-expansion

::

  f :: A ->. B

  g :: A -> B
  g x = f x

This also work at higher arity, including mixed of linear and
non-linear arguments:

::

  f' :: A ->. B -> C ->. D

  g :: A -> B -> C -> D
  g = f
  -- is interpreted as:
  -- g x y z = f x y z

Zero as a multiplicity
~~~~~~~~~~~~~~~~~~~~~~

The implementation, and the usage-based definition of linearity in the
Multiplicities_ section, use a ``0``. It is currently kept out of the
actual multiplicities because we have no use case for this. But it
would not be hard to provide. Additionally, ``0`` has been used by
`Conor McBride
<https://link.springer.com/chapter/10.1007/978-3-319-30936-1_12>`_ to
handle dependent types, which may matter for Dependent Haskell.

An alternative which we may consider, or which we may take into account
when Dependent Haskell progresses, would be to have the multiplicity
``0`` as an additional multiplicity.

The definitions of sum, product and order would have to be modified as
follows:

::

   0 + x = x
   x + 0 = x
   _ + _ = ω

   0 * _ = 0
   _ * 0 = 0
   1 * x = x
   x * 1 = 1
   ω * ω = ω

   _ ⩽ ω = True
   x ⩽ y = x == y

Note in particular that ``0 ≰ 1``.

An important point to note, however, is that ``case_0`` is
meaningless: it makes it possible to create values dependending on a
value which may not exist at runtime. For instance the length of a
list argument with multiplicity ``0``.

::

  -- Wrong!
  badLength :: [a] :'0-> Int
  badLength [] = 0
  badLength (_:l) = 1 + badLength l

  -- Not linear! But well-typed if the above is accepted
  f :: [a] ->. (Int, [a])
  f l = (badLength l, l)

Because we want to allow ``case_p`` for a variable ``p``, this
creates a small complication. Which can be solved in a number of way:

- Make it so that multiplicity variables are never instantiated by
  ``0``, in particular type-application of multiplicity variables must
  prohibit ``0``.
- Instead of restricting variables and type applications so that
  ``case_p`` is allowed for a variable ``p``, we can allow arbitrary
  variables and disallow, in particular, ``case_p``.

  In this case, we would have:

  ::

     map :: (a :(p:+'One)-> b) -> [a] :(p:+'One)-> [b]
     map f [] = []
     map f (a:l) = f a : (map f l)

  In practice, under this situation, the type of ``map`` is probably better
  written as

  ::

     map :: forall p a b q. (p ~ q :+ 'One) => (a :'One-> b) -> [a] :p-> [b]

  In order to play more nicely, for instance, with explicit type
  applications.

  A benefit is that higher-order functions with no ``case`` such as
  ``(.)`` are now capable of taking functions with multiplicity ``0`` as
  argument.
- A variation on the same idea is to introduce a constraint

  ::

    CaseCompatible :: Multiplicity -> Constraint

  which is discharged automatically by the compiler. Variables
  implementing this are acceptable in ``case``. So ``map`` would be of
  type.

  ::

    map :: (CaseCompatible p) => (a :p-> b) -> [a] :p-> [b]

  This is harder to implement than just reusing ``p~q:+'One`` as a
  constraint, but is more resistant to having more multiplicities than
  just 0, 1, and ω, as is currently proposed.
- Another option is to have a type of multiplicities *excluding* ``0``
  and have another type of extended mulitplicities for multiplicities
  with ``0``. Note that a different ``(:+)`` and ``(:*)`` would have to
  act on extended multiplicities.

.. _`No annotation on case`

No annotation on case
~~~~~~~~~~~~~~~~~~~~~

Instead of having ``case_p`` (see Multiplicities_) we could just have the
regular ``case`` (which would correspond to ``case_1`` in this
proposal's formalism). This would simplify the addition of ``0``.

On the other hand, doing this loses the principle that linear data
types and unrestricted data types are one and the same. And sacrifices
much code reuse.

Uniqueness instead of linearity
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Languages like Clean and Rust have a variant of linear types called
uniqueness, or ownership, typing. This is a dual notion: instead of
functions guaranteeing that they use their argument exactly once, and
no restriction being imposed on the caller, with uniqueness type, the
caller must guarantee that it has a non-aliased reference to a value,
and the function has no restriction.

Where uniqueness really shines, is for in-place mutation: the ``write``
function can take a regular ``Array`` as an argument, it just needs to
require that it is unique. Freezing is really easy: just drop the
constraint that the ``Array`` is unique, it will never be writable
again.

With linear types, we need to have two types ``MArray`` (guaranteed
unique) and ``Array``, just like in Haskell today. This is fine when
we are freezing one array: just call ``freeze``. But what if we are
freezing a list of arrays? Do we need to ``map freeze``? This is
unfortunate (the problem is even more complicated if we start
considering ``MArray (MArray a)``). It has a feel of ``Coercible``,
but it does feel harder.

On the other hand, other examples work better with linear types, such
as fork-join parallelism. This is why Rust has a notion of so-called
mutable borrowed reference, on which constraints are more akin to
linear types (or rather, affine types, technically).

Overall, uniqueness type system are significantly more complex to
specify and implement than linear types systems such as this
proposal's.

Linearity-in-kinds
~~~~~~~~~~~~~~~~~~

Instead of adding a type for linear function, we could classify types
in two kinds: one of unrestricted types and one of linear
types. A value of a linear type must be used in a linear fashion.

This would get rid of the continuation of ``newMArray`` in the
motivating ``MArray`` interface.

The most natural way to do this, in Haskell, is to add a second
parameter to ``TYPE`` (the first one is for levity polymorphism). So,
ignoring the levity polymorphism, we would have ``TYPE 'One`` for linear
types and ``TYPE 'Omega`` for unrestricted type. We get polymorphism by
abstracting over the multiplicity.

As interesting as it is, there is quite some complication associated
to it. First, because of laziness, you can't have a function of type
``(A :: TYPE 'One) -> (B :: TYPE 'Omega)`` (because you don't need to
consume the result, hence you may not consume an argument that you
have to consume). So what would be the type of the arrow? Something
like ``forall (p :: Multiplicity) (q ⩽ p). p -> q -> q``. So we're
introducing some kind of bounded polymorphism in our story. This is
quite a bit harder than our proposal.

Most types will live in both kinds, but that would have to be
explicit:

::

  data List (p :: Multiplicity) (a :: TYPE p) :: TYPE p where
    [] :: List p a
    (:) :: a -> List p a -> List p a

Mixing non-linear and linear lists (*e.g.* with ``(++)``) would
require either some subtyping from ``List 'Omega a`` to ``List 'One a`` (but
as discussed above, subptyping in presence of polymorphism quickly
becomes hairy) or some conversion function.

It it worth taking into account that the issues with ``MArray`` and
``Array`` (which may be ``Array 'One`` and ``Array 'Omega`` in this case)
above are not solved by such a situation. Unless there is a subptyping
relation from ``Array 'Omega`` from ``Array 'One``, which cannot be performed
by an explicit function since this would be equivalent to the
proposal's situation.

On the other hand, the CPS interface to ``newMArray`` delimits a scope
in which the array lives. This gives a perfect opportunity to put
clean-up code to react to exceptions. So it may not be such a bad thing
after all.

So linearity in kind seem to add a lot of complication for very little
gain.

On the matter of dependent Haskell, to the best our knowledge, the only
presentations of dependent types with linearity-in-kinds disallow
linear types as arguments of dependent functions.

Additive conjunction
~~~~~~~~~~~~~~~~~~~~

There is a connective of linear logic which is not included in this
proposal: the additive conjunction, typically written ``A&B``. It
differs from the multiplicative conjunction (written ``A⊗B`` in linear
logic, and ``(A, B)`` in Linear Haskell) in that it has two *linear*
projections ``π₁ :: A&B ->. A`` and ``π₂ :: A&B ->. B`` but, contrary
to the multiplicative conjunction, only one of the two conjuncts of a
linear ``A&B`` will be consumed (that is: consuming a value ``u`` of
type ``A&B`` exactly once, means consuming ``π₁ u`` exactly once, or,
*exclusively*, consuming ``π₂ u`` exactly once).

It is not part of the proposal because it can be encoded:

::

  type a & b = forall k. Either (a ->. k) (b ->. k) ->. k

What could be a benefit of having a primitive support for ``A & B``?
Values of type ``A&B`` could be implemented as a lazy thunk rather
than a function. But this only really matters for unrestricted values,
but in this case, the role of lazy pair is already played by
``Unrestricted (A, B)`` (due to our treatment of ``case``, see `No
annotation on case`_).

On the other hand we believe additive pairs of effectful computations
to be more useful in effectful context. In which case we would use:

::

  type a & b = Either (a ->. ⊥) (b ->. ⊥) ->. ⊥

For some effect type ``⊥`` (it could be ``type ⊥ = RIO ()`` for
instance).

So on balance, we didn't consider additive pairs to be useful enough
to justify a dedicated implementation and syntax.

Future extensions (not part of this proposal)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Toplevel-linear binders
+++++++++++++++++++++++

Something that hasn't been touched up by this proposal is the idea of
declaring toplevel linear binders

::

  module Foo where
  token ::('One) A  -- made up syntax

Here ``token`` would have be consumed exactly once by the program,
this property is a link-time property. This generalised the
``RealWorld`` token which is currently magically inserted in the
``main`` function (the existence of which is checked at link time).

This would allow libraries to abstract on ``main`` or to provide their
own linearly-threaded token.

.. _`More multiplicities`

More multiplicities
+++++++++++++++++++

One central aspect of the proposed system is that it is very easy to
extend with new multiplicities: add a multiplicity to the
``Multiplicity`` data-type, extend the sum, product, ordering, and
join functions.

As discussed in the `Affine types`_ section, one such extra
multiplicity is the multiplicity of affine functions (which is
the join of ``0`` and ``1``). The `paper
<https://arxiv.org/abs/1710.09756>`_ also suggests a "borrowing"
multiplicity which would allow for arbitrary usage, but be strictly
smaller than ``ω``.

It is not clear what the eventual list of multiplicity should be. The
literature teaches us that multiplicities classify co-effects, of
which there are many.

Instead of trying to come up with a definite list of multiplicities
which ought to be built in, we hope to be able to propose a solution
to make it possible for libraries to define new multiplicities.


The Core corner
---------------

.. _`The Core corner`:

*This section is an appendix to the proposal describing the changes
to GHC's Core intermediate language in order to accommodate the new
feature of this proposal*

The bulk of the modifications to Core is described in §3 of the `paper
<https://arxiv.org/abs/1710.09756>`_.

Changes to the type system
~~~~~~~~~~~~~~~~~~~~~~~~~~

Here is a summary of the changes included in the paper:

- All variables have an attached multiplicity (just like they have an
  attached type)
- Type variables can be of kind ``Multiplicity``
- The arrow type constructor has an additional argument (the
  multiplicity ``p`` in ``(:p->)``)
- Data constructors have mulitplicities attached to their fields
- It seems that, because of the worker/wrapper split in the strictness
  analysis, Core will need unboxed tuples with multiplicity-annotated
  fields. Even if there is no surface syntax for these in the
  proposal.

Here are the changes and interactions which were omitted in the paper:

- In the paper the only polymorphism described is polymorphism in
  multiplicities, there is no added difficulty due to general type
  polymorphism.
- The paper does not have existentially quantified type
  variables. They do not cause any additional difficulty.
- The paper uses a traditional construction for ``case``, but Core's
  is a bit more complex: in Core, ``case`` is of the form ``case u as
  x of { <alternatives> }`` where ``x`` represents the head normal
  form of ``u``. Moreover one of the alternative can be ``WILDCARD ->
  <rhs>`` (where ``WILDCARD`` is Core's representation of ``_``). The
  simplest way to type check this extra binder, is to type ``case u as
  x of { <alternatives> }`` as ``let x = u in case x of {
  <alternatives> }`` (using, in the latter form, the simplified
  ``case`` from the paper). Without any modification, however, this
  typing rule may prove to simplistic. It is not entirely clear how to
  make the surface language desugar to well-typed terms with this rule
  (see Patterns_). It also has consequences on `Core
  transformations`_. See also `Unresolved questions`_

There is no change to the term syntax, only the types and the linter
are affected.

Note: the constraint arrow ``=>`` is interpreted as an unrestricted
arrow (*i.e.* of multiplicity ω).

Effect on transformations and passes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. _`Core transformations`:

An indication that the effects of linear types on Core transformations
should be small is that GHC must already preserve linearity: in the
case of ``ST`` and ``IO``, a token is passed around which must be used
linearly. At the surface level, linearity is enforced by the abstract
interface, but it is manifest in Core, so core must preserver their
linearity. Therefore, any interaction between linearity and Core
transformations are due either to new patterns which couldn't be
previously expressed or limitation of the type-checking.

Below are the transformations which we have analysed so far:

η-reduction
  Because the η-expansion of a linear function can be an unrestricted
  function, it is not, in general, safe, to η-reduce functions
  (η-expasions are even added to the compiler: see Subtyping_). GHC
  already does not perform η-reduction carelessly, so we need to add
  an extra condition for η-reduction to be successful.

Inlining
  Suppose we have

  ::

    let_1 x = u in if b then … x … else … x …

  GHC may try to line ``x`` at the some (but not necessarily all) of
  the use sites. For instance, GHC may try to reduce to

  ::

    let_1 x = u in if b then … u … else … x …

  But this is not recognised as linear under the current typing rules
  (because, among other things ``u`` counts as having been used twice,
  once in the right-hand-side of the let, and once in the ``then``
  branch).

  So, under the current typing rules, linear lets could be inlined at
  *every* site (this is a form of β-reduction) or none at all. But, of
  course, this inlining transformation does not change the meaning of
  the program, so it is still valid. If th

Common Subexpression Elimination (CSE)
  When encountering an expression of the form

  ::

    let x = u in e

  the rewrite rule ``u --> x`` is added to the environment when
  analysing ``e``.

  This can't safely be done in general for linear lets:

  ::

    let_1 x = u in e

  There are several potential strategies:

  - Ignore linear lets for the purpose of CSE. After all, we are
    unlikely to find many occurrences of ``u`` if ``u`` is used in a
    ``let_1``.
  - Try and see if we can replace the ``let_1`` by a ``let_ω`` (that
    is, if ``u`` only has unrestricted type variables). And continue
    with ``u --> x`` if the ``let_1`` was successfully promoted to
    ``let_ω``.
  - Do not change the ``let_1`` immediately, but when an occurrence of
    ``u`` is encountered, lazily promote the ``let_1`` to a ``let_ω``
    if needed (if we have resolved the issue with inlining, we may not
    always need to promote the ``let_1``). It is not completely clear
    how to pursue this option.

Case-binder optimisations:
  GHC will try to transform

  ::

     case_1 x of y {
       (p:ps) -> (case_1 x of …) (case_1 x of …)}

   into

   ::

     case_1 x of y {
       (p:ps) -> let x_?? = y in (case x of …) (case x of …)}

   This transformation, similar to CSE, is valid only because we are
   calling for a ``case_1`` of some unrestricted variable. This is
   difficult for several reasons:

   - Under the naive typing rule for case-binders proposed above, it
     is not even correct to use ``y`` inside an alternative: it has
     been consumed by being the scrutinee.
   - Even if we have a more flexible typing rule for ``let`` (see
     below), it remains that ``y`` has multiplicity ``1`` and that for
     the right-hand side of the alternative to type-check, we actually
     need ``let_ω x = y in …``, which is not well-typed.

   Like for CSE, we can either prevent this optimisation for linear
   cases. Or we can try to promote the ``case_1`` to a ``case_ω``, and
   perform the transformation only if it's successful.

Float-in & float-out
  These transformation move let-bindings inside or outside
  λ-abstractions. They are safe in presence of linear types.

Note that the issues and interactions were illustrated with examples
of multiplicity 1, but the same arguments works for any multiplicity
which are not ω (in particular multiplicity variables).

Advanced typing rule for ``let``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There is no known account of a type-system which would account for the
inlining transformation. Let alone of one which would not require too
much engineering. But the idea is, conceptually, simple enough: from
the point of view of usage, ``x`` and ``u`` must be considered the
same (since ``u`` may contain several variables with their own
multiplicity, it requires more than a union-find structure).

Provided we can give a precise description of such a system, it can be
used to make general inlining well-typed, and it would resolve the
rigidity of the case-binder typing rule discussed above.

However, it may be worth noticing a potentially surprising behaviour:
we may use, as an optimisation, the fact that a ``let`` is linear to
avoid saving its thunk upon evaluation as we are not going to force it
again. But the case-binder does not have this property:
computationally does not quite behave like a linear ``let``.

Unresolved questions
--------------------

.. _`Unresolved questions`:

This section summarises the questions that have yet to be resolved.

Inference
~~~~~~~~~

- There is no systematic account of type inference. Can it be made
  predictable when a type annotation is required? For compatibility
  reasons, we want to infer unrestricted arrows conservatively, but
  experience shows that it can result in very surprising type
  errors. See Inference_ for more details.

- In the formalism, case expressions are indexed by a multiplicity:
  ``case_p`` (and similarly ``let_p``). In the surface language, we
  can deduce the multiplicity in equations when their is a type
  annotation.

  ::

    fst :: (a,b) -> a
    fst (a,_) = a    -- this is inferred as a case_ω

    swap :: (a,b) ->. (b,a)
    swap (a,b) = (b,a)   -- this is inferred as a case_1

  But what of explicit ``case`` and ``let`` in the surface language? We
  can annotate them with a multiplicity, but it is generally clear from
  the context which multiplicity is meant. So the multiplicity
  annotation really ought to be inferred. The general idea is: if
  their is any linear variable in the scrutinee, then the case must be
  linear, and if there are only unrestricted variables, it can be
  unrestricted. Is it sound to always pick the highest possible value ?
  What if there are multiplicities with variable multiplicity ?

Patterns
~~~~~~~~

It is not clear yet how the following should be handled:

- View patterns: linear view patterns should not be a problem as long
  as there is only one view and that the patterns are grouped into a
  single call to the view (otherwise the patterns would translate, in
  Core, to several calls using the same linear variable, which is not
  allowed). It is not clear yet that we can have a predictable
  criterion which would allow programmers to use linear view
  patterns without generating faulty Core. On the other hand, it would
  be unfortunate not to have linear view patterns at all, as views
  matter more in linear types as there are usually no projections.
- ``@``-patterns: The pattern ``x@(Just _) -> …`` could be seen as
  linear. After all, it is equivalent to ``Just y -> let x = Just y in
  …``. It is not clear that we can make the linearity checking in Core
  accept this sort of patterns (see also the `The Core corner`_ section above).
- Pattern synonym: linear pattern synonyms have not been studied
  yet. In particular, how they ought to be type checked, when they are
  defined. It is still unknown whether this problem is hard or easy.

Syntax
~~~~~~

Linear monads, like ``RIO`` in the socket motivating example will
require the ``do`` notation to feel native and be comfortable to
use. There is a facility to do this ``-XRebindableSyntax`` but,
besides the problem with ``itThenElse`` mentionned above, this has a
much too coarse grain behaviour: realistically, the same file will
want to mention regular monads and linear monads (there is also
another useful type of monads where multiplicity can change), but
``-XRebindableSyntax`` changes the meaning of ``do`` globally. A
solution would be to have a locally-rebindable ``do`` syntax such as
is attempted in `this proposal
<https://github.com/ghc-proposals/ghc-proposals/pull/78>`_.

Core
~~~~

In Core, ``case`` is of the form ``case u as x of { <alternatives> }``
where ``x`` represents the head normal form of ``u``. It is used by
the compiler in some Core to Core passes. It is also how default
alternatives of a case are implemented:

::

  fmap' :: (a -> a) -> Maybe a -> Maybe a
  fmap' (Just x) = Just (f x)
  fmap' y = y

is elaborated into

::

  \f o -> case_ω o as y of { Just x -> Just (f x) ; WILDCARD -> y }

But it is not obvious what to do for linear cases. The following is a
linearity violation as ``y`` in a sense contains ``x`` (basically, you
could define a function ``a ->. (a,a)`` generically with this).

::

  case_1 o as y of { Just x -> Just (x,y) }

So we need a simple story (Core needs to stay fairly simple) for the
case-binder of linear cases.

The easiest thing to do would be to type ``case_p u as y of { … }`` as
``let_p y = u in case y of { … }``. But this may not be a good idea:
it would prevent default cases, or legitimate patterns such as
``x@(Just _)`` from being considered linear. It may also make some
compiler passes harder than they ought to.

After all, there is a transformation for ``x@(Just _) -> u`` which
makes is a linear program: ``Just y -> let_1 x = Just y in u``. And
the latter program has precisely the same behaviour.

It is not known at this point whether the simpler typing rule would be
an obstacle or whether it is worth it to have a more fine-grained
typing.

Solving this will have user-facing implications, in particular regarding
which view patterns and ``@``-patterns are available in linear
functions.

See also the sections Patterns_ and `The Core corner`_.

Implementation Plan
-------------------

- @aspiwack will implement the proposal
- @aspiwack will implement and release a library exporting standard
  functions and types for linearly typed programs.
