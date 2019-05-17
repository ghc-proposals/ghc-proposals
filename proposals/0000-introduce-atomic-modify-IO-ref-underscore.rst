.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/230>`_.
.. sectnum::
.. contents::

We introduce two new functions in `Data.IORef`, `atomicModifyIORef_, atomicModifyIORef'_ :: IORef a -> (a -> a) -> IO`.
The latter is strict in the value stored in `IORef`.

These are straightforward convenience functions mirroring the `modifyIORef` API.

Motivation
------------

It promotes a common pattern of providing `_` versions of functions in the standard libarary
which enable one to ignore return values from some monadic context.

Being able to mutate a value `IORef` atomically without wanting to return anything is a
natural operation to want to perform. A data structure that wishes to modify some data
in a race-free manner would enjoy using this primitive.

Proposed Change Specification
-----------------------------

We extend `Data.IORef` with two new functions that are helpers around existing functions.
It might be the case that more efficient implmentations are possible. 

.. code-block:: haskell

 import Control.Arrow ((&&&))

 -- | Variant of 'atomicModifyIORef' which ignores the return value
 atomicModifyIORef_ :: IO a -> (a -> a) -> IO ()
 atomicModifyIORef_ r f = atomicModifyIORef r $ f &&& const ()

 -- | Variant of 'atomicModifyIORef'' which ignores the return value
 atomicModifyIORef'_ :: IO a -> (a -> a) -> IO ()
 atomicModifyIORef'_ r f = atomicModifyIORef' r $ f &&& const ()


Effect and Interactions
-----------------------

None forseen

Costs and Drawbacks
-------------------

- Maintaince cost is low.
- Might be seen as a utility function that is better suited for being in a library
- Already exists 

Alternatives
------------

Use an existing package which implements the combinato, such as 
`yjtools <https://hackage.haskell.org/package/yjtools-0.9.18/docs/Data-IORef-Tools.html>`_

Unresolved questions
--------------------

None?

Implementation Plan
-------------------

The implementation is listed in the Proposed Change Specification section.
