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

GHC has lack of support for renaming of class methods if class is already public.
This proposal gives a way how to do this painlessly.

Background
----------

GHC has lack of support for renaming of class methods if class is already public.

Here is an real example of a ``Monoid a`` class in "base" module::

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

Once class become public and renaming or even removing methods becomes practically impossible 
without breaking backward compatibility.

Let we have::

      -- Bar.hs:
      module Bar where
        
            class Bar a where
                  foo :: a -> a


      -- Main.hs:
      module Main where
            -- (B) case
            import Bar (Bar(foo)) -- explicit
            import Bar (Bar(..))  -- implicit

            -- (A) case
            instance Bar T where
                  foo = somefunc


And we decided to rename ``foo`` into ``bar``. Is it possible? ::

      class Bar a where
            bar :: a -> a

      foo :: Bar a => a -> a
      foo = bar

But what to do with all instances (A) and imports (B)? These changes fully broke backward compatibility.


Proposed Change Specification
-----------------------------

Syntax
~~~~~~

We add a keyword, which is important in class declaration only.

It is a Class Scoped function, so it could be named as ``function`` (PHP/JS/Lua-like), ``func`` (Go-like), 
``fun`` (OCaml/F#/SML-like), ``fn`` (Rust-like), ``defun`` (Lisp-like), ``define`` (Scheme-like), ``def`` (Phyton/Ruby-like),
``lambda`` (Lisp/Python-like),  ``sub`` (Perl-like) ::

    class Bar a where

        function foo
        
        foo :: a -> a
        foo = bar

        bar :: a -> a
        bar = foo

        {-# MINIMAL foo | bar #-}

    foo :: Bar a => a -> a
    foo = bar

    -- Main.hs:
    import Bar (Bar(foo)) -- explicit
    import Bar (Bar(..))  -- implicit

    -- (A) case
    instance Bar T where
        foo = somefunc


Semantics
~~~~~~~~~

Class Scoped function (CSF for (A) case) has simple rules:

* CSF could be defined in where-part of classes and instances only
* CSF has a scope of an instance(where it is defined),  a class (where it is defined) and it's instances
* CSF has no scope outside of class and instances 
* CSF is always shadows outside function with same name

Now we can rewrite ``Monoid a`` class as follows::

    class Semigroup a => Monoid a where

        function mappend
        
        mappend :: a -> a -> a
        mappend = (<>)

        mempty :: a
        mempty = mconcat []

        mconcat :: [a] -> a
        mconcat = foldr mappend mempty

        {-# MINIMAL mempty | mconcat #-}

    mappend :: Monoid a => a -> a -> a
    mappend = (<>)


Unfortunately, these changes require changes for detailed import ((B) case).

So we need to have implicit extension "``ImportFromClassFunction``" for omitting these changes in import and we 
need to explicit switch it off by "``NoImportFromClassFunction``" language pragma. 

Alternatively we could control specific imports with "``{-# NOIMPORTFN Mod #-}``" / "``{-# IMPORTFN Mod #-}``" pragma.


Effect and Interactions
-----------------------

Any Effect and Interactions are unknown.

Costs and Drawbacks
-------------------

We expect the implementation and maintenance costs for this feature to be minimal.

Backward Compatibility
----------------------

This proposal is backward compatibility driven, so we expected it is fully backward compatibile.

Alternatives
------------

An alternative is status-quo, to remain as is.

Implementation Plan
-------------------

It is unclear.
