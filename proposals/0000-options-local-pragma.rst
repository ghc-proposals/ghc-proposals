Options Local Pragma
====================

.. proposal-number:: 
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/234>`_.
.. sectnum::
.. contents::

We propose to add functionality for switching GHC-options locally. For instance, in this code
::
 {-# OPTIONS_GHC -Wno-name-shadowing #-}

 f a =
   {-# OPTIONS_LOCAL -Wname-shadowing #-}
   \a -> a + a

 g a = \a -> a + a

there is ``-Wname-shadowing`` warning in funcion ``f`` but not in function ``g``

This proposal rises some ticket problems:
 1. https://gitlab.haskell.org/ghc/ghc/issues/602
 2. https://gitlab.haskell.org/ghc/ghc/issues/10150

Motivation
------------

Warnings and errors by compiler help to write nice code. Using ``OPTIONS_LOCAL`` pragma you can more flexible configure compiler options.

Local control of warnings
~~~~~~~~~~~~~~~~~~~~~~~~~
 
1. Consider `suppress orphan instance warning per instance <https://gitlab.haskell.org/ghc/ghc/issues/10150>`_. We disable ``-Worphans`` warning for ``instance ApplyFunc Box`` but warning for `instance ApplyFunc Bottle` works.
   ::
    module Foo (
      ApplyFunc(..)
    ) where

    class ApplyFunc f where
      func :: (a -> b) -> f a -> f b

   
    module Bar (
      Box(..)
    , Bottle(..)
    ) where

    data Box a = Empty
               | Content a 

    data Bottle a = Water
                  | Milk a 

   
    {-# OPTIONS -Worphans #-}
    module Baz where

    import Foo
    import Bar

    instance {-# OPTIONS_LOCAL -fno-warn-orphans #-} ApplyFunc Box where
      func f Empty       = Empty
      func f (Content a) = Content $ f a

    instance ApplyFunc Bottle where
      func f Water    = Water
      func f (Milk a) = Milk $ f a

2. Consider `suppress particular kinds of warnings for parts of a source file <https://gitlab.haskell.org/ghc/ghc/issues/602>`_. In this example we don't get ``-Wunused-do-bind`` warning for ``f`` but get it for ``g``.
   ::
    {-# OPTIONS_GHC -Wunused-do-bind #-}

    f :: IO ()
    f = {-# OPTIONS_LOCAL -Wno-unused-do-bind #-} do
      getLine
      return ()

    g :: IO ()
    g = do
      getLine
      return ()
      

Using language extensions locally
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`Comment from the ticket 602 <https://gitlab.haskell.org/ghc/ghc/issues/602#note_108677>`_: "It might be reasonable to consider adding arbitrary option-changes locally. (For example, I'd love to be able to turn on LANGUAGE pragmas only for part of a file"

1. Let's enable ``-XPartialTypeSignatures`` in ``x``. Such code doesn't compile because partial type signature wasn't allow in ``y``.
   ::
    x = {-# OPTIONS_LOCAL -XPartialTypeSignatures #-}
      let a :: _
          a = ()
       in ()
       
    y =
      let a :: _
          a = ()
       in ()

Proposed Change Specification
-----------------------------

GHC already support the ``OPTIONS_GHC`` pragma for configuring options for the file as a whole. We propose to create a similar pragma ``OPTIONS_LOCAL`` which will do the same things but locally. You can see what it looks like in the *Motivation section*.

Places for ``OPTIONS_LOCAL`` pragma:
 - expression
 - declaration
 - types

The pragma uses `meaning-preserving parsing rules for SCC annotationssuppress <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0046-scc-parsing.rst>`_ for expressions and types. As for declarations - it applies to the following declaration.

Costs and Drawbacks
-------------------

1) **Estimate on development and maintenance costs**

Every compiler flag can require individual way to collaborate with ``OPTIONS_LOCAL`` pragma.

2) **Influence to learnability of the language**

``OPTIONS_LOCAL`` pragma is optional pragma and is non-essential for basic users of the language. The area of using intersects with `OPTIONS_GHC` pragma and as a result it does not require any more learning after the OPTIONS_GHC pragma. There is only one distinction - you need to learn where and how to place it inside the file (somewhat like the SCC pragma).

3) **Remaining drawbacks**

None.


Alternatives
------------

None.

Unresolved Questions
--------------------

There is `a question on Stackoverflow <https://stackoverflow.com/questions/12717909/stop-ghc-from-warning-me-about-one-particular-missing-pattern/>`_. It says to rid of the warning in case of incomplete patterns in a code you can use only ``error``. Whether is fine to add such feature using pragma ``OPTIONS_LOCAL``?


Implementation Plan
-------------------

There is `the proof of concept implementation <https://gitlab.haskell.org/ghc/ghc/merge_requests/1029>`_.
