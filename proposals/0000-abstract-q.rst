Abstract Q
==============

.. author:: Teo Camarasu
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/700>`_.
.. sectnum::
.. contents::

Template Haskell is GHC's metaprogramming facility. It allows users to write Haskell programs that can manipulate on Haskell syntax trees.
These programs are run at compile-time, and their results are spliced into the syntax tree.
They can be effectful and run ``IO`` actions or introspect into the compiler state in limited ways.
These effects are exposed to users through the ``Quote`` and ``Quasi`` typeclasses and the ``Q`` monad.

These typeclasses are both external interfaces used by users and internal interfaces of GHC.
This exposes implementation details to users, which makes it difficult or impossible to alter
the internal interface while keeping the external interface the same.

We propose adding a new ``MetaHandlers`` record type of actions as a purely internal interface, which is not exposed to users.
``Q`` is to be changed into an abstract newtype over ``MetaHandlers -> IO a``. ``Quasi`` and ``Quote`` are to be implemented in terms of it.
``runQ :: Quasi m => Q a -> m a`` would become a method of ``Quasi``.

Motivation
----------

Let's motivate this change with an example.

Suppose I wanted to add new effect that could be used in Template Haskell splices:
``reifyCore :: Name -> Q Core``, which given the ``Name`` of a term would produce the Core representation of it.

I would currently implement this by adding this as a method to ``Quasi`` and adding an implementation in GHC.
This forces a breaking change on ``template-haskell``, since it exports ``Quasi``.
We cannot hide this method for the sake of compatibility, since end users depend on the ability to write custom instances of ``Quasi`` for their own monads
(eg, `th-orphans <https://hackage.haskell.org/package/th-orphans-0.13.16/docs/Language-Haskell-TH-Instances.html>` provides instances for certain monad transformers).

We would run into the same issue if we wanted to remove a method from ``Quasi``  or change its type.
The change in GHC would have to be reflected in the exposed interface of ``template-haskell``, forcing end-users to upgrade to a new major release if they want to use the new version of GHC.

By separating out the internal interface, we can avoid this tight coupling.
``reifyCore`` can be added as a field to ``MetaHandlers``. ``Quasi`` would then live purely in ``template-haskell``.
Old versions of ``template-haskell`` would still be compatible with the new version of GHC, as they can simply ignore the new field.
A new version of ``template-haskell`` can add the corresponding method to ``Quasi`` and use the field from ``MetaHandlers`` to implement it.

Proposed Change Specification
-----------------------------

No changes are being proposed to the language.

Proposed Library Change Specification
-------------------------------------

This proposal requires making changes to the interface of ``template-haskell``.

The definition of ``Q`` would change from ::

 newtype Q a = Q { unQ :: forall m. Quasi m => m a }

to::

 newtype Q a = Q { unQ :: MetaHandlers -> IO a }

``unQ`` and the ``Q`` constructor would no longer be exported from ``template-haskell``.
This is a breaking change.

The ``runQ :: Quasi m => Q a -> m a`` function would be moved from the top-level and become
a method of ``Quasi``. This is a breaking change.
Note that ``runQ/Quasi`` is to ``Q`` as ``liftIO/MonadIO`` is to ``IO``.

The rest of the changes are internal to GHC and ``ghc-internal``.

Effect and Interactions
-----------------------

* The `Pure Template Haskell proposal <https://github.com/ghc-proposals/ghc-proposals/pull/655>`_ aims to
  empower users to ban use of ``IO`` in Template Haskell splices.
  This proposal opens up a lightweight implementation path for something along these lines.
  One could implement a ``dropIO :: Q a -> Q a`` function that removes the ``runIO`` effect from the ``MetaHandlers`` record,
  replacing it with an error call. This function could only be implemented by accessing ``ghc-internal``.


Costs and Drawbacks
-------------------

The main cost of this proposal is that it entails a breaking change to the ``template-haskell`` interface.
The implementation should be relatively simple and if anything it should simplify things as an existential is being replaced with a common-or-garden record.


Backward Compatibility
----------------------
TODO: impact assessment but it's likely to be minor


Implementation Plan
-------------------
Teo Camarasu will implement this.

We would create a new ``MetaHandlers`` record in ``ghc-internal``.
This record would have a field for each method of ``Quasi``.

We would have to refactor the code for running splices in the compiler to use use this new interface.

We would update ``template-haskell`` with the changes proposed here.

We would submit patches to any packages on Hackage broken by these changes.

Endorsements
-------------
