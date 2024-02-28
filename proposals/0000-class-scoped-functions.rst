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

GHC has a lack of support for backward compatible refactoring of renaming class methods if the class is already public.
This proposal gives a way how to do this painlessly.

Motivation
----------

Background
~~~~~~~~~~

GHC has a lack of support for backward compatible refactoring of renaming class methods if the class is already public.

Here it is a real example of the ``Monoid a`` class in the ``base``, where method ``mappend`` is impossible to extract from the class without breaking backward compatibility of the rest code: ::

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

      -- Bar.hs:
      module Bar where
        
            class Bar a where
                  foo :: a -> a


      -- Main.hs:
      module Main where
            -- (B) case
            import Bar (Bar(foo)) -- explicit detailed import
            import Bar (Bar(..))  -- implicit detailed import

            -- (A) case
            import Bar  -- implicit full import
            instance Bar T where
                  foo = somefunc

            myFunc :: T -> ....
            myFunc = someDefinition using foo


And we decided to rename ``foo`` into ``bar`` (or we decided to write completely alternative class-functions). Is it possible do not break backward compatibility? ::

      class Bar a where
            bar :: a -> a -> a

      foo :: Bar a => a -> a
      foo = someTransform bar

But what to do with all instances (A) and detailed imports (B)? These changes fully broke backward compatibility.


Proposed Change Specification
-----------------------------

We propose, that backward compatible refactoring of renaming class methods could be done in 2 Stages. 

**First Stage**: we transform deprecated function into Class scoped functions (CSFs). This allows to reuse old code and old libraries with old, but already deprecated, definitions. And same time this allows to write code in a new way. To make sure, that in the new code is written differently, we deprecate by pragma to write old way.

**Second Stage**: in some distant future, when the old code is no longer used anywhere, we get rid of CSFs.


Syntax
~~~~~~

We could add a keyword (or use already existed one), which is important in class declaration only.

It denotes a class scoped function, so it could be named ``let`` (it's already a keyword in Haskell) or ``inner`` or ``hidden`` or ``private`` (by meaning), 
``function`` (PHP/JS/Lua-like), ``func`` (Go-like), ``fun`` (OCaml/F#/SML-like), ``fn`` (Rust-like), ``defun`` (Lisp-like), ``define`` (Scheme-like), 
``def`` (Python/Ruby-like), ``lambda`` (Lisp/Python-like),  ``sub`` (Perl-like) 

In all examples here a keyword ``let`` is used. ::

    class Bar a where

        let foo

        {-# DEPRECATED #-}
        foo :: a -> a
        foo = bar

        bar :: a -> a
        bar = foo

        {-# MINIMAL foo | bar #-}

    -- this "foo" is not deprecated
    foo :: Bar a => a -> a
    foo = bar

    -- Main.hs:
    -- (B) case
    import Bar (Bar(foo)) -- explicit detailed import
    import Bar (Bar(..))  -- implicit detailed import

    -- (A) case
    import Bar  -- implicit full import
    instance Bar T where
        foo = somefunc


Semantics
~~~~~~~~~

Class scoped functions (CSF for (A) case) have simple rules:

* CSFs can only be defined in classes and instances
* CSFs are only in scope in class and instance definitions
* CSFs always shadow outside functions with the same name
* CSFs is best suits together with ``{-# DEPRECATED #-}``, but this is not a mandatory

Now we can rewrite the ``Monoid`` class as follows::

    class Semigroup a => Monoid a where

        let mappend
        
        {-# DEPRECATED #-}
        mappend :: a -> a -> a
        mappend = (<>)

        mempty :: a
        mempty = mconcat []

        mconcat :: [a] -> a
        mconcat = foldr mappend mempty

        {-# MINIMAL mempty | mconcat #-}

    -- this "mappend" is not deprecated
    mappend :: Monoid a => a -> a -> a
    mappend = (<>)


Unfortunately, these changes require also changes for detailed import ((B) case) for backward compatibility.

So we need to have additional explicit extension "``NoImportClassScopedFunction``" for disable import functions with names equal to Class Scoped Function names, and otherwise it is enabled. 


Examples
--------

We could use Class Scoped Functions for different backward compatible refactoring strategies.

Renaming
~~~~~~~~

Example of backward compatible renaming a class-method ::

  class Foo a where

     let foo_old

     {-# DEPRECATED #-}
     foo_old :: a -> a
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

     let elem_old

     {-# DEPRECATED #-}
     elem_old :: a -> Collect a -> Bool
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

     let nextN_old

     {-# DEPRECATED #-}
     nextN_old :: Collect a -> Int -> (Collect a, Maybe a)
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

Removing a method
~~~~~~~~~~~~~~~~~

It is an example here of backward compatible removing ``mappend`` of ``Monoid a``.

and here is a fresh example with discussion to remain or not ``second`` in ``Bifunctor a`` and remain backward compatible ::

  class (forall a. Functor (p a)) => Bifunctor p where
      -- {-# MINIMAL bimap | first, second #-}
      {-# MINIMAL bimap | first #-}

      bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
      bimap f g = first f . second g

      first :: (a -> b) -> p a c -> p b c
      first f = bimap f id

      let second

      second :: (b -> c) -> p a b -> p a c
      -- second = bimap id
      second = fmap

  -- this outside "second" is defined differently then inner one
  second :: forall a b. Functor (p a) => Bifunctor p => (b -> c) -> p a b -> p a c
  second = bimap id


Effect and Interactions
-----------------------

Any Effect and Interactions are unknown.

Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs for this feature to be minimal.

Backward Compatibility
----------------------

This proposal is backward compatibility driven, so we expected it is fully backward compatible. And more: this proposal is fully future compatible.

Alternatives
------------

An alternative is status-quo, to remain as is.

Implementation Plan
-------------------

It is unclear.
