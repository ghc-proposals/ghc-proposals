List instances for a type in GHCi
==============

.. author:: Xavier Denis
.. date-accepted:: 2018-11-05
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/issues/15610
.. implemented:: 8.10
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/166>`_
.. contents::


There is currently no mechanism to get the instances for a specific type in GHCi, `:info` command allow specifying more than a top-level constructor. This proposal suggests a new GHCi command that would allow programmers to query GHC for valid instances matching a type.

Motivation
------------

Typeclass resolution and deriviation are integral parts of modern Haskell. Unfortunately, the compiler doesn't volunteer a lot of information on them during development. Figuring out whether a type has a specific instance usually requires you to work backwards: starting from the typeclass and going through every instance in Haddock, recursively, to figure out if your type satisfies it. This experience isn't optimal and can get annoying for large or nested types and classes.

This is exactly the kind of task that the compiler could help us with though. Since it already has all the visible instances for a given piece of code loaded, we could just ask it to list all instances that a type matches! This proposal suggests adding a new command and related machinery to GHCi to acomplish this. An example of this command's possible execution is shown below:

.. code-block:: none

  >> import Data.Functor.Sum (Sum)
  >>
  >> :instances Sum [] []
  Eq   a => Eq   (Sum [] [] a)
  Ord  a => Ord  (Sum [] [] a)
  Show a => Show (Sum [] [] a)
  Read a => Read (Sum [] [] a)
  Functor  (Sum [] [])
  Foldable (Sum [] [])
  Eq1      (Sum [] [])
  Ord1     (Sum [] [])
  Show1    (Sum [] [])
  Read1    (Sum [] [])
  FunctorWithIndex     (Either Int Int) (Sum [] [])
  FoldableWithIndex    (Either Int Int) (Sum [] [])
  TraversableWithIndex (Either Int Int) (Sum [] [])

It would provide a new tool for Haskell developers, both new and experienced that helps during during development and debugging. It allows a new way of talking with our beloved compiler and to better understand how it thinks.

Proposed Change Specification
-----------------------------

This proposal adds a new command to GHCi called ``:instances`` which provides a listing of all valid instances for a given type.

Syntax
~~~~~~

Valid queries to ``instances`` are types, if anything other than a type is provided as an argument, the command will return an error explaining the intended usage. The command interprets everything after ``instances`` as a single type, this means there is no way to specify multiple types to lookup instances of MPTCs. Holes can be used to represent free variables in the argument type, for example: ``Either _ _``. To express relations between multiple holes, named holes can be used, for example:: ``Either _a _a``.

Execution
~~~~~~~~~

Provided with a valid type, ``instances`` will attempt to match it against the heads of all visible class instances and reduce constraints until only those mentioning holes in the type remain. The output will consist of a formatted listing of all matching instances that satisfy the stated condition on constraints. Each instance should be simplified as much as possible, meaning that if an instance: ``(c ~ Bool) => C c`` were found it would be presented as ``C Bool``.


Effect and Interactions
-----------------------

This new command gives programmers the ability to see every possible instance for a type that the compiler sees. This is useful both for beggining and  `advanced <https://github.com/Iceland_jack>`_ haskellers who are exploring types and what they can do with them.

When combined with recent extensions like ``GeneralizedNewtypeDeriving`` and ``DerivingVia``, a type can 'steal' many instances from other structurally identical ones but finding those instances is currently a tedious manual process.

This also provides a way for beginners to see what's possible with the types given to them. Instead of having to browse Haddocks and stumble on instances for their type, GHC can simply provide all that information on the spot!

Since this is simply a GHCi command, there will be no interaction with the language itself.

Costs and Drawbacks
-------------------

The primary costs for this feature are implementation time. Because it is a GHCi command, it won't have any backwards compatibility or performance issues. It should actually help make Haskell a little more accessible to beginners as well, giving a better view on what GHC sees as possible instances for their types.

Alternatives
------------

Currently GHCi has an ``:info`` command which offers some information on instances. However, ``:info`` only works on single words, which means that ``:info Either String Int`` will return results for ``Either``, ``String``, and ``Int`` separately. This makes it a partial replacement at best for the ``:instances`` command.

Examples
--------


.. code-block: none

  >> :instances Sum f f
  Functor f => Functor (Sum f f)
  Foldable f => Foldable (Sum f f) ..

Basic instance lookup:

.. code-block: none
  >> :instances [_]

  Show _ => Show [_]
  Eq _ => Eq [_]
  ....

Often when dealing with monad stacks, it can be useful to figure out exactly which instances are available for a given type:

.. code-block: none
  >> :instances StateT _s (ReaderT _r IO)
  MonadIO (StateT _s (ReaderT _r IO))
  Functor (StateT _s (ReaderT _r IO))
  MonadState (StateT _s (ReaderT _r IO))
  MonadReader (StateT _s (ReaderT _r IO))
  ...


Unresolved Questions
--------------------


Future Work
-----------

The original `ticket <https://gitlab.haskell.org/ghc/ghc/issues/15610>`_ also suggests some further work. There are several improvements that could be proposed. These are meant as ideas that could be fleshed out in a future proposal.

Negative Results
~~~~~~~~~~~~~~~~

A second command ``:noinstance`` would output a list of the unsatisfied constraints that prevent an instance from being found for a specific type.

Multi-Parameter Type Classes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Adding support for searching for multi-parameter type classes that include several specified types could be useful as well. It's unclear how to actually dilineate the multiple types that need to be provided.

Trace Info
~~~~~~~~~~

We could annotate the specific location that each instance was provided from.

.. code-block:: none
  >> :instances Sum [] []
  ..
  Functor (Sum [] [])    -- (Functor f, Functor g) => Functor (Sum f g) -- Defined in ‘Data.Functor.Sum’
  ..

Implementation Plan
-------------------

If accepted, I, `@xldenis <https://github.com/xldenis>`_ intend to implement this feature.
