.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

Bundle pattern synonyms with a type synonym
===========================================

GHC allows bundling pattern synonyms with a datatype, cf.
`Section 9.7.3 <https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/glasgow_exts.html#import-and-export-of-pattern-synonyms>`_
of the ghc manual
(the design is discussed on the ghc wiki, see `AssociatingSynonyms <https://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms/AssociatingSynonyms>`_).
Bundling enables pattern synonyms to be imported along with a datatype just like data constructors, using the ``Type(..)`` syntax.
Pattern synonyms can also be used to offer a simplified view on a type,
in which case it would be more natural to bundle them with a type synonym
for the corresponding simplified type,
e.g., ``State`` instead of ``StateT`` for the state monad.
Therefore, support for bundling pattern synonyms with type aliases seems desirable.

Motivation
----------

Consider the following motivating example, which is a stripped-down version of
`mtl <https://hackage.haskell.org/package/mtl>`_'s ``Identity`` and ``State`` monads,
with a pattern synonym for the latter. ::

    {-# LANGUAGE ViewPatterns #-}
    {-# LANGUAGE PatternSynonyms #-}
    {-# LANGUAGE ScopedTypeVariables #-}

    module State (
        Identity(..),
        State, pattern State, runState
    ) where

    import Control.Monad

    newtype Identity a = Identity { runIdentity :: a }

    newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }

    type State s a = StateT s Identity a

    pattern State :: (s -> (s, a)) -> State s a
    pattern State { runState } <- ((runIdentity .) . runStateT -> runState)
        where State runState_ = StateT (Identity . runState_)

This code works, but any user code that wants to explicitly import the state monad,
its constructor (namely, the pattern ``State``) and selector (``runState``)
will have to import the three exported entities separately, which is a bit annoying.
More importantly, it is impossible to change a data type (like ``State``) to a type synonym
without breaking backward compatibility for imports.

Proposed Change
---------------

The change affects import and export lists and their semantics.

Syntax
^^^^^^

There is no change to the formal syntax of
import declarations (`HR:5.3 <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1010005.3>`_) and
export lists (`HR:5.2 <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1000005.2>`_).
However, the constraint that a type synonym can only be referred to by its namein export lists is relaxed.

More precisely, in `HR:5.2 <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1000005.2>`_,
the restriction ::

    A type synonym T declared by a type declaration may be named by the form T , where T is in scope.

is replaced by ::

    A type synonym T declared by a type declaration may be named by the form T, where T is in scope,
    or in the form T(c1,...,cn), where c1...cn name pattern synonyms, data constructors,
    or record field selectors.

In addition, the associated pattern synonyms should not be *visibly incompatible* (using terminology from `AssociatingSynonyms#Typing <https://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms/AssociatingSynonyms#Typing>`_): ::

    If unfolding type synonyms in a saturated application of T results in the shape
        T t1 ... tn = S t1' ... tn'
    where S is a data/newtype constructor, then c1,...,cn should be constructors of S, functions with
    type of shape S t1' ... tn' -> t, or pattern synonyms P that are not visibly incompatible with S,
    that is, if the type of P after unfolding type synonyms has shape ... -> S' t1' ... tn' for a
    data/newtype constructor S', then S = S'.

Note that `HR:5.3 <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1010005.3>`_
does not spell out any restriction on the imported entities.

Semantics
^^^^^^^^^

An export of the form ``T(c1,...,cn)``, where ``T`` is a type synonym,
will export the type synonym ``T``, and the pattern synonyms and selectors ``c1`` to ``cn``.

An import of the form ``T(d1,...,dm)``, where ``T`` is a type synonym exported in the form ``T(c1,...,cn)`` will import ``T`` and ``d1`` to ``dm``, provided that each ``dj`` equals some ``ci``.

An import of the form ``T(..)``, where ``T`` is a type synonym exported in the form ``T(c1,...,cn)`` will import ``T`` and ``c1`` to ``cn``.


*Is there a better way of checking whether ``c1`` to ``cn`` are actually
associated with the type synonym ``T``?*

None have been proposed.

The specification covers the simple case where the type synonym unfolds
directly into a data type or newtype, based on
`AssociatingSynonyms#Typing <https://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms/AssociatingSynonyms#Typing>`_.

Discussion
^^^^^^^^^^

Revisiting the motivating example,
the export list of the `State` module could be changed to ::

 module State (
     Identity(..),
     State(State, runState)
 ) where

Then, importing ``State(..)`` from the ``State`` module would import the pattern and selector into another module.

Should this be tied to some language extension?
^^^^^^^^^^

Bundling pattern synonyms isn't guarded by a language extension so it seems sensible that this isn't either.

Drawbacks
---------

As specified, one can bundle any pattern synonym, data consrtructor, or record selector with a type synonym.
This may be abused to cause confusion.

Alternatives
------------

None so far.

Unresolved Questions
--------------------

None at the moment.

Remarks
-------

* There is a Trac ticket (`#12857 <https://ghc.haskell.org/trac/ghc/ticket/12857>`_) that predates the prosal
* For another motivating example, see https://github.com/int-e/haskell-src-exts-simple/issues/2
* It may make sense to give arbitrary functions the benefit of being associated with a type synonym, or possibly a type class or data type.
  But this should be a separate proposal.
