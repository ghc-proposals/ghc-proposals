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

It denotes a class scoped function, so it could be named ``function`` (PHP/JS/Lua-like), ``func`` (Go-like), 
``fun`` (OCaml/F#/SML-like), ``fn`` (Rust-like), ``defun`` (Lisp-like), ``define`` (Scheme-like), ``def`` (Python/Ruby-like),
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

Class scoped functions (CSF for (A) case) have simple rules:

* CSFs can only be defined in classes and instances
* CSFs are only in scope in class and instance definitions
* CSFs always shadow outside functions with the same name

Now we can rewrite the ``Monoid`` class as follows::

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

So we need to have the implicit extension "``ImportFromClassFunction``" for omitting these changes in import and we 
need to explicit switch it off by a "``NoImportFromClassFunction``" language pragma. 

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
