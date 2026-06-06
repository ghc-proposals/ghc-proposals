======================
Class Scoped Functions
======================

.. author:: Viktor WW
.. date-accepted::
.. ticket-url:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/590>`_.
.. sectnum::
.. contents::

GHC has a lack of support for backward compatible refactoring of class methods if the class is already public (in libraries).

This proposal gives an intermediate way how to do this painlessly.

Motivation
----------

GHC has a lot of Proposals (including approved ones), which just declare backward compatible stability of Language.

This Proposal is an effective tool for most conservative part for changing: backward compatible refactoring of class methods in libraries.

Background
~~~~~~~~~~

GHC has a lack of support for *backward compatible refactoring* of class methods if the class is already public.

Here it is a **real** example of the ``Monoid a`` class in the ``base``, where method ``mappend`` is impossible to extract from the class without breaking backward compatibility of the rest code: 
::

  class Semigroup a => Monoid a where

      -- __NOTE__: This method is redundant and has the default
      -- implementation @'mappend' = ('<>')@ since /base-4.11.0.0/.
      -- Should it be implemented manually, since 'mappend' is a synonym for
      -- ('<>'), it is expected that the two functions are defined the same
      -- way. In a future GHC release 'mappend' will be removed from 'Monoid'.
      mappend :: a -> a -> a
      mappend = (<>)

      mempty :: a
      mempty = mconcat []

      mconcat :: [a] -> a
      mconcat = foldr mappend mempty
      
      {-# MINIMAL mempty | mconcat #-}


Flexibility on change of class-functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Once a class becomes public, renaming or even removing methods becomes practically impossible without breaking backward compatibility.

Let's say we have ::

    -- Bar.hs (library1):
    class Bar a where
        foo :: a -> a

    -- ----------------------------------
    -- LibT1Bar.hs (library2):
     import Bar

     instance Bar T1 where
        foo = somefunc

    -- ----------------------------------
    -- Main.hs:
    import Bar
    import LibT1Bar

    instance Bar T2 where
        foo = somefunc

    myFunc :: T -> ....
    myFunc = someDefinition using foo


And we decided to rename ``foo`` into ``bar`` (or we decided to write completely alternative class-functions). 

Is it possible do not break backward compatibility? 
::

    class Bar a where
        bar :: a -> a -> a

    foo :: Bar a => a -> a
    foo = someTransform bar

But what to do with all instances? These changes fully broke backward compatibility.


Proposed Change Specification
-----------------------------

We propose, that backward compatible refactoring of class methods could be done not in 1 Stage, but in 2 Stages (or more). 

- **Intermediate Migration Stage**: we transform deprecated function into intermediate Class scoped functions (CSFs). This allows to reuse old code and old libraries with old, but already deprecated, definitions. And same time this allows to write code in a new way. To make sure, that in the new code is written differently, we deprecate by pragma to write old way.

- **Second Stage**: in some distant future, when the old code is no longer used anywhere, we get rid of CSFs.


Main benefit - is a possibility to declare instances old way and new way same time. This allows to reuse old libraries and same time write more effective new way instances.

Syntax
~~~~~~

In all examples here a keyword ``let`` is used to mark function as Class scoped function. 

It looks like ``let`` block without ``in`` inside ``do`` notation 
::

    -- Bar.hs:
    class Bar a where

        {-# DEPRECATED #-}
        let foo :: a -> a
            foo = bar

        bar :: a -> a
        bar = foo

        {-# MINIMAL foo | bar #-}

    -- this "foo" is not deprecated
    foo :: Bar a => a -> a
    foo = bar

    -- --------------------------------------------
    -- LibT1Bar.hs:
    import Bar

    -- instance using old way with redeclaration "foo" method
    instance Bar T1 where 
        foo = somefunc1

    -- --------------------------------------------
    -- Main.hs:
    import Bar
    import LibT1Bar

    -- instance using new way with redeclaration "bar" method
    instance Bar T2 where 
        bar = somefunc2

Semantics
~~~~~~~~~

Class scoped functions (CSF) have simple rules:

* CSFs can be marked as CSF's by a keyword ( ``let`` ) in class definition only
* CSF visibility/scope is inside ``where`` clause of classes and instances where they could be defined or used
* CSFs always shadow outside functions with the same name
* CSFs is best suits together with ``{-# DEPRECATED #-}``, but this is not a mandatory

Now we can rewrite the ``Monoid`` class as follows::

    class Semigroup a => Monoid a where

        {-# DEPRECATED #-}
        let mappend :: a -> a -> a
            mappend = (<>)

        mempty :: a
        mempty = mconcat []

        mconcat :: [a] -> a
        mconcat = foldr mappend mempty

        {-# MINIMAL mempty | mconcat #-}

    -- this "mappend" is not deprecated
    mappend :: Monoid a => a -> a -> a
    mappend = (<>)

And this is enough with full import for backward compatible refactoring.


Examples
--------

We could use Class Scoped Functions for different backward compatible refactoring migration strategies.

Removing a method
~~~~~~~~~~~~~~~~~

It is an example here of backward compatible removing ``mappend`` of ``Monoid a``.

And here is a fresh **real** example with long discussion to remain or not ``second`` in ``Bifunctor a`` and remain backward compatible.

Main question: how much this affects Hackage libs.

With this Proposal it is possible to have both ::

  class (forall a. Functor (p a)) => Bifunctor p where
      -- {-# MINIMAL bimap | first, second #-}
      {-# MINIMAL bimap | first #-}

      bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
      bimap f g = first f . second g

      first :: (a -> b) -> p a c -> p b c
      first f = bimap f id

      let second :: (b -> c) -> p a b -> p a c
          -- second = bimap id
          second = fmap

  -- this outside "second" is defined differently then inner one
  second :: forall a b. Functor (p a) => Bifunctor p => (b -> c) -> p a b -> p a c
  second = bimap id

Renaming
~~~~~~~~

Example of backward compatible renaming a class-method ::

  class Foo a where

     {-# DEPRECATED #-}
     let foo_old :: a -> a
         foo_old = foo_new

     foo_new :: a -> a
     foo_new = foo_old

   -- this outside of class function is not deprecated
   foo_old :: Foo a => a -> a
   foo_old = foo_new

Swap the order of arguments
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Example of backward compatible swapping the order of arguments in a class-method ::

  class Bar a where
     type Collect a

     {-# DEPRECATED #-}
     let elem_old :: a -> Collect a -> Bool
         elem_old = flip elem_new

     elem_new :: Collect a -> a -> Bool
     elem_new = flip elem_old

  -- this outside of class function is not deprecated
  elem_old :: Bar a => a -> Collect a -> Bool
  elem_old = flip elem_new

Change amount of arguments
~~~~~~~~~~~~~~~~~~~~~~~~~~

Example of backward compatible changing amount of arguments in a class-method ::

  class Bar a where
     type Collect a

     {-# DEPRECATED #-}
     let nextN_old :: Collect a -> Int -> (Collect a, Maybe a)
         nextN_old c m = go (c, Nothing) m
           where
             go r n = case n of
               | n <= 0    => r
               | otherwise => go (next_new $ fst r) (n - 1)

     next_new :: Collect a -> (Collect a, Maybe a)
     next_new c = nextN_old c 1

  -- this outside of class function is not deprecated
  nextN_old :: Collect a -> Int -> (Collect a, Maybe a)
  nextN_old c m = go (c, Nothing) m
    where
      go r n = case n of
         | n <= 0    => r
         | otherwise => go (next_new $ fst r) (n - 1)


Effect and Interactions
-----------------------

If CSF is deprecated by ``{-# DEPRECATED #-}`` , this means it is deprecated to be used in custom (non-auto) instances only.

Any other Effect and Interactions are unknown.

Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs for this feature to be intermediate.

Backward Compatibility
----------------------

This proposal is backward compatibility driven, so we expected it is fully backward compatible. And more: this proposal is fully future compatible.

Alternatives
------------

Main alternative is status-quo, to remain as is: painful backward incompatible refactoring of class methods.

Alternative Syntax
~~~~~~~~~~~~~~~~~~

We could add a new keyword or use already existed one, which is important in class declaration only.

It denotes a class scoped function, so it could be named:

- ``let`` (it's already a keyword in Haskell) and it is used in all examples 
- ``inner`` new keyword by meaning
- ``hidden`` new keyword
- ``private`` new keyword
- ``function`` new keyword (PHP/JS/Lua-like)
- ``func`` (Go-like)
- ``fun`` (OCaml/F#/SML-like)
- ``fn`` (Rust-like)
- ``defun`` (Lisp-like)
- ``define`` (Scheme-like)
- ``def`` (Python/Ruby-like)
- ``lambda`` (Lisp/Python-like)
- ``sub`` (Perl-like) 

Unresolved Questions
--------------------

Unfortunately, this Proposal do not cover for **detailed** import for backward compatibility.
::

    module Main where
       -- import (B) case
       import Bar (Bar(foo)) -- explicit detailed import
       import Bar (Bar(..))  -- implicit detailed import

       myFunc :: T -> ....
       myFunc = someDefinition using foo


Implementation Plan
-------------------

It is unclear.
