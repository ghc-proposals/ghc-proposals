Class Scoped Functions
====================

.. author:: Viktor WW
.. date-accepted::
.. ticket-url:: 
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/590>`_.
.. sectnum::
.. contents::

GHC has a lack of support for renaming class methods if the class is already public.
This proposal gives a way how to do this painlessly.

Background
----------

GHC has a lack of support for renaming class methods if the class is already public.

Here is a real example of the ``Monoid a`` class in ``base``::

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

Motivation
----------

Once a class becomes public, renaming or even removing methods becomes practically impossible 
without breaking backward compatibility.

Let's say we have::

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


And we decided to rename ``foo`` into ``bar`` (or we decided to write completly alternative class-functions). Is it possible? ::

      class Bar a where
            bar :: a -> a -> a

      foo :: Bar a => a -> a
      foo = someTransform bar

But what to do with all instances (A) and detailed imports (B)? These changes fully broke backward compatibility.


Proposed Change Specification
-----------------------------

We propose, that renaming class methods could be done in 2 Stages. 

First Stage: we transform deprecated function into Class scoped functions (CSFs). This allows to reuse old code and old libraries with old, but already deprecated, definitions. And same time this allows to write code in a new way.

Second Stage: we get rid of CSFs, when the old code is no longer used anywhere.


Syntax
~~~~~~

We could add a keyword (or use already existed one), which is important in class declaration only.

It denotes a class scoped function, so it could be named ``let`` (it's already a keyword), ``function`` (PHP/JS/Lua-like), ``func`` (Go-like), 
``fun`` (OCaml/F#/SML-like), ``fn`` (Rust-like), ``defun`` (Lisp-like), ``define`` (Scheme-like), ``def`` (Python/Ruby-like),
``lambda`` (Lisp/Python-like),  ``sub`` (Perl-like) ::

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
* CSFs is best suits together with ``{-# DEPRECATED #-}``

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


Unfortunately, these changes require changes for detailed import ((B) case).

So we need to have additional explicit extension "``NoImportClassScopedFunction``" for disable import functions with names equal to Class Scoped Function names, and otherwise it is enabled. 


Effect and Interactions
-----------------------

Any Effect and Interactions are unknown.

Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs for this feature to be minimal.

Backward Compatibility
----------------------

This proposal is backward compatibility driven, so we expected it is fully backward compatibile. And more: this proposal is fully future compatibile.

Alternatives
------------

An alternative is status-quo, to remain as is.

Implementation Plan
-------------------

It is unclear.
