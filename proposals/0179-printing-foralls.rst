Clean up printing of foralls
============================

.. author:: Richard Eisenberg
.. date-accepted:: 2019-04-17
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/16320
.. implemented:: change (1) in 9.0, changes (2) and (3) in 9.2
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/179>`_.
.. contents::

GHC has two infelicities around the way it prints ``forall`` types around visible type application.
This proposes a fix for both. The two
fixes are entirely separable, but it seems sensible to debate them together.

Motivation
----------
With visible type application, we have a distinction between *specified* variables and *inferred* ones.
In brief, a specified variable is one that has been written by the user, whereas an inferred variable
is invented by GHC. Here is an example::

  id :: a -> a
  id x = x

  id2 x = x

The type of ``id`` contains a specified variable, ``a``, written by the user. Though ``id2`` will be
inferred to be polymorphic, its type variable is inferred, not specified.

Visible type application works only with specified variables. This is because we can rely on the ordering
and existence of specified variables only; inferred variables might be reordered between different minor
versions of GHC. Thus, ``id @Int 5`` is accepted, while ``id2 @Int 5`` is rejected. None of this is new.

There are two problems with the status quo:

1. GHC prints braces around inferred variables, but only with ``-fprint-explicit-foralls`` on. This is
   sometimes confusing. For example::

     GHCi, version 8.7.20181028: http://www.haskell.org/ghc/  :? for help
     Prelude> :set -XPolyKinds
     Prelude> import Type.Reflection
     Prelude Type.Reflection> :t +v typeRep
     typeRep :: forall k (a :: k). Typeable a => TypeRep a

     Prelude Type.Reflection> :set -fprint-explicit-foralls
     Prelude Type.Reflection> :t +v typeRep
     typeRep :: forall {k} (a :: k). Typeable a => TypeRep a

   Even without ``-fprint-explicit-foralls``, we get an explicit ``forall`` because the type is quantified
   over a poly-kinded type variable. (This is described in the manual. We had to turn on ``-XPolyKinds`` to
   prevent GHCi from defaulting the kind variable to ``Type``.) But, lo and behold, the ``k`` is actually
   *inferred*, not *specified*, only witnessed after ``-fprint-explicit-foralls`` is enabled.

2. The type printed by ``:type <expr>`` is the type that would be inferred for ``it`` if we said
   ``let it = expr``. This means that ``<expr>``\'s type is instantiated and regeneralized. This
   instantiating/regeneralizing allows us to solve some constraints. For example::

     Prelude> :set -XTypeApplications
     Prelude> :t (+) @Int
     (+) @Int :: Int -> Int -> Int

   Without instantiating, we would still see the ``Num Int =>`` constraint in the printed type.

   However, this is awfully confusing with visible type application::

     Prelude> :t map
     map :: forall {a} {b}. (a -> b) -> [a] -> [b]

   Both of ``map``\s type variables are *specified*, yet GHC does not print it accordingly. This is
   because the variables are instantiated and then regeneralized. In the process, GHC declares them
   to be *inferred*, because they were regeneralized. To override this behavior, we must use ``:t +v``::

     Prelude Type.Reflection> :t +v map
     map :: forall a b. (a -> b) -> [a] -> [b]

Proposed Change Specification
-----------------------------
1. Whenever printing variables quantified in a ``forall``, print inferred variables with braces.

2. Maximally instantiate any *inferred* or dictionary arguments (class constraints) to expressions
   passed to ``:type``.

3. Remove ``:type +v``.

Effect and Interactions
-----------------------
* Proposed change (1) fixes motivation (1) handily.

* Proposed change (2) means to instantiate any *inferred* type variables and try to solve
  any class constraints in the type of an expression passed to ``:type``, as long as there
  are no intervening visible or *specified* arguments. Here are some examples to illustrate::

    foo :: forall a. (a ~ Int) => a -> a
    bar :: forall a b. (a ~ Int) => a -> b -> a

    > :type foo
    foo :: (a ~ Int) => a -> a
    > :type foo @Int
    foo @Int :: Int -> Int
    > :type foo @Bool
    **TYPE ERROR**
    > :type bar
    bar :: (a ~ Int) => a -> b -> a
    > :type bar @Int
    bar @Int :: (Int ~ Int) => Int -> b -> Int
    > :set -fprint-explicit-foralls
    > :type bar @Int
    bar @Int :: forall b. (Int ~ Int) => Int -> b -> Int
    > :type bar @Int @Bool
    bar @Int @Bool :: Int -> Bool -> Int
    > :type (+) @Int
    (+) @Int :: Int -> Int -> Int

  As we can see here, the new behavior for ``:type`` combines the advantages of the old
  ``:type`` (it does some intantiating and constraint-solving) and the old ``:type +v``
  (it doesn't fiddle with specified variables). The new ``:type`` isn't perfect, though:
  it still reports ``Int ~ Int`` in the type of ``bar @Int``; it does this because
  there is an intervening specified variable, ``b``.

* Now that ``:type`` doesn't fiddle with specified variables, ``:type +v`` seems redundant.
  Note that it is not *entirely* redundant, as suggested to me by @int-index. For example,
  suppose we have ::

    quux :: Arbitrary T => T -> T

  for some concrete type ``T``. This is allowed with suitable extensions, and is useful
  when the ``Arbitrary T`` instance is defined in a testing module as an orphan. Yet,
  any use of ``:type quux`` will yield a type error. Of course, users can use ``:info quux``
  in this case and get the result they want.

* Note that this proposal is all about GHCi and printing. It does *not* change the language
  that GHC compiles.

Costs and Drawbacks
-------------------
* The drawback to change (1) is that it means GHC is printing more fancy widgets in types. Without
  ``-XTypeApplications``, users do not care about the inferred/specified distinction and may be
  unfamiliar with the new notation.

* The drawback of change (2) is that users might see more unsolved constraints with ``:type``,
  but these should appear only with ``-XTypeApplications``.

* The drawback of change (3) is that users might be surprised to see ``:type +v`` dropped. It would
  be easy to have GHCi produce an error stating that the feature has been removed because ``:type``
  has been improved for a few releases.

Alternatives
------------
* These are free design decisions, and the sky is the limit.

* Previously, this proposal suggested special-casing ``:type`` to behave like ``:type +v`` when
  the expression is just a single name. However, like all special cases, this could lead
  to unexpected behavior. This new formulation seems better.

Unresolved Questions
--------------------
None at this time.


Implementation Plan
-------------------

(Note mainly for self.)
We think that (2) could be implemented easily by setting ``ir_inst`` to ``False`` when processing
a ``:type`` invocation, and then doing ``topInstantiateInferred`` at the top of ``tcArgs``. While
in town, have ``topInstantiateInferred`` be a bit faster when ``inst_all`` is ``False``, a common
case.
