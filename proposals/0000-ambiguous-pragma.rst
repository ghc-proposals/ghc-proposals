Replace AllowAmbiguousTypes with AMBIGUOUS pragma
============================================

.. author:: James Koppel
.. date-accepted:: 
.. ticket-url::
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/338>`_.
.. contents::



Motivation
----------

``TypeApplications`` have been a great boon, allowing substantial shortening to any code that formerly needed ``Proxy``'s.  For example, a function ``getTypeName :: (HasName a) => String`` could be invoked ``getTypeName @Int``, rather than ``getTypeName (Proxy :: Proxy Int)``.
 

Such applications require ``AllowAmbiguousTypes``. However, ``AllowAmbiguousTypes`` reduces the strength of checking in the rest of the file. For example, if there is a declaration in the file like ``foo :: (Functor g) => f a -> f a``, where ``Functor g`` is a typo for ``Functor f``, then the defect will not be caught until use.

The goal of this proposal is to allow such type applications without removing the ability of the ambiguity check to catch errors, much as the ``OVERlAPS`` pragma and friends did for ``OverlappingInstances``.


Proposed Change Specification
-----------------------------

* Deprecate {-# AllowAmbiguousTypes #-}
* Add a new pragma {-# AMBIGUOUS name_of_function #-}

GHC will disable the ambiguity check for functions marked ambiguous.

Examples
--------

::

 class HasName a where
   {-# AMBIGUOUS getTypeName #-}
   getTypeName :: String

 instance HasName Int where getTypeName = "Int"

 x = getTypeName @Int -- "Int"
 y = getTypeName      -- Error: Could not resolve (HasName a1)


 -- Not marked ambiguous; gives type error
 void :: (Functor g) => f a -> f ()
 void = fmap (const ())

  


Effect and Interactions
-----------------------

Should be none.


Costs and Drawbacks
-------------------

Short-term, all files with ``AllowAmbiguousTypes`` would need to be refactored to add this pragma where needed.

This change makes files with ambiguous types more readable in the same way that the ``{-# OVERLAP #-}`` pragma did, by marking which types are ambiguous and hence only callable with type applications, and restoring the ambiguity check for the rest of the file. Users will need to learn the association that functions intending to be called with type applications should have this pragma, and then functions with this pragma are intended to be called with type applications.


Alternatives
------------

The chief alternative is to keep the status quo of ``AllowAmbiguousTypes``.


Unresolved Questions
--------------------

The main unresolved question is the syntax of the pragma. ``{-# AMBIGUOUS name_of_function #-} foo :: (Cxt f) => a``, vs. something closer to what is done for overlapping instances, e.g.: ``foo {-# AMBIGUOUS #-} :: (Cxt f) => a``.

Another unresolved question is whether and how this applies to pattern synonyms and GADT constructors.


Implementation Plan
-------------------


Endorsements
-------------
