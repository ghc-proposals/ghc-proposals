Local Warning Pragmas
=====================

.. proposal-number:: 
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/234>`_.
.. sectnum::
.. contents::

We propose to add functionality for control warnings locally, in particular, add pragmas ``WARN``, ``IGNORE`` and ``ERROR``. Consider this code:
::
 {-# OPTIONS_GHC -Wname-shadowing #-}

 main :: IO ()
 main = {-# IGNORE name-shadowing #-} do 
   let x = "some data"
   let x = "updated version of that data"
   let x = "further changed version of data"
   print x
    
Here in each case, you are effectively throwing away the previous version of the data without any warning but if you remove ``IGNORE`` you get ``-Wname-shadowing``. You can rewrite some lines to rid of it:
::
 let x1 = "some data"
 let x2 = "updated version of that data"
 let x3 = "further changed version of data"

then you have all of ``x1``, ``x2``, and ``x3`` in scope and you may make an error by picking an older numbered x, when conceptually you really only want one x in scope.

This proposal rises some ticket problems:
 1. https://gitlab.haskell.org/ghc/ghc/issues/602
 2. https://gitlab.haskell.org/ghc/ghc/issues/10150

Motivation
------------

All examples have been created or updated pursuant to `the remarks by @scott-fleischman <https://github.com/ghc-proposals/ghc-proposals/pull/234#issuecomment-496661390>`_. 

Warnings and errors by compiler help to write nice code. Using ``WARN``, ``IGNORE`` and ``ERROR`` pragmas you can configure warnings which are indicate bugs with more flexibility.

Code examples
~~~~~~~~~~~~~
      
1. `Suppress the warning in case of incomplete patterns <https://stackoverflow.com/questions/12717909/stop-ghc-from-warning-me-about-one-particular-missing-pattern/>`_. 

We might choose one place where we reluctantly decide that we don't want to match all patterns (due to other people's code, or maybe your own poor choice in the past but not able/willing to fix right now), but we certainly want to check for complete patterns everywhere else in the module. For example, there are deprecated cases in a sum type, where we didn't want to pattern match on them due to being deprecated, which also would generate a deprecated warning.

Pragma ``IGNORE`` fixes it:
::
 {-# OPTIONS_GHC -Wincomplete-patterns #-}

 {-# INGNORE incomplete-patterns #-}
 f :: (Show a) => Maybe a -> String
 f (Just a) = show a

2. `Suppress orphan instance warning per instance <https://gitlab.haskell.org/ghc/ghc/issues/10150>`_. 

We need to define an orphan instance for some type in an external library (``Bar``). It serves a nice documentation-like purpose to keep those instances local to avoid allowing any orphan in an entire module. Later we can search for the local instance declarations and revisit the decision to use them.

We disable ``-Worphans`` warning for ``instance ApplyFunc Box`` but warning for ``instance ApplyFunc Bottle`` works.
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


 {-# OPTIONS_GHC -Worphans #-}
 module Baz where

 import Foo
 import Bar

 instance {-# IGNORE orphans #-} ApplyFunc Box where
   func f Empty       = Empty
   func f (Content a) = Content $ f a

 instance ApplyFunc Bottle where
   func f Water    = Water
   func f (Milk a) = Milk $ f a

3. **Local suppress warnings -Wmissing-signature**.

Suppose you want to use temporary value or function for debug and you don't want define any signature for it. At the same time you want to track missing signature in the remaining part.

In this example you get warning ``-Wmissing-signatures`` for ``x`` but not for ``y``.
::
 {-# OPTIONS_GHC -Wmissing-signatures #-}

 x2 :: Int -> Int
 x2 = (* 2)

 x3 :: Int -> Int
 x3 = (* 3)

 x4 :: Int -> Int
 x4 = (* 4)

 x = 12

 {-# IGNORE missing-signatures #-}    
 y = 13

Another motivation
~~~~~~~~~~~~~~~~~~

4. **Other people's code**. With a large codebase that uses lots of libraries and limited developer resources we need to respond to changes in libraries as we update to more recent versions. We may not agree with decisions of various libraries, but we do have to respond to them, and we may not be able to make the fully correct response immediately.

5. **Allowing local exceptions to warnings**. It allows us to turn on warnings globally but allow local exceptions that we can document where they came from and why we are not able or willing to change them in the short term. This could be because it's not technically possible or because we are not willing to invest the time and effort to make the changes now. (We can file a ticket to improve it later.)

6. **More easily quarantine deprecations**. We turn on the warning for use of deprecated code, but often libraries make choices that make it hard to immediately remove the deprecated code. Suppose a library deprecated a record field that is still even used internally by the library. The library disabled the deprecation warning in the entire module in their own code, and we are forced to also disable deprecations in our modules that use the field, or to quarantine our use of that field to a separate smaller module that only has code using the deprecated field. It would have been nicer to indicate which deprecated field that we are intentionally using to avoid allowing any other deprecated code to be used in the module.

7. **Documentation**. A local declaration provides documentation about which warnings we are disabling and why. In particular if the syntax for local pragmas is unique enough, it makes common search/replace an easy way to gauge how large a task it would be to update all of it at a future time. For example, a redundant constraint can be useful to express the intention of the code for purposes of clarity even when not strictly necessary.

Proposed Change Specification
-----------------------------

GHC already support the ``OPTIONS_GHC`` pragma for configuring options for the file as a whole (in particular, configure warnings). **We propose to create new pragmas**:

1. ``WARN`` - enables a warning locally
2. ``IGNORE`` - disables a warning locally
3. ``ERROR`` - makes a specific warning into a fatal error localy

This pragmas use an idea of (``-W``, ``-Wno-``, ``-Werror-``) batch switching of flags `proposed @nomeata <https://github.com/ghc-proposals/ghc-proposals/pull/234#issuecomment-495977461>`_.

**Places for pragmas**:
 - expression
 - declaration
 - types

These pragmas use `meaning-preserving parsing rules <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0046-scc-parsing.rst>`_ for expressions and types. As for declarations - they apply to the following declaration.

Local work makes sense not for every warning. In case of misuse compiler gives some error.

Here some warnings which are proposed to be added in ...

**blacklist**
 - all "batch enabling" flags like ``-W``, ``-Wall``, etc.
 - all ``-f*``
 - all ``-Wdefer*``
 - all deprecated warnings: ``-Wamp``, ``-Wduplicate-constraints``, ``-Wmissing-*-sigs``, ``-Wstar-is-type``
 - all ``-Wnoncanonical*``, ``-Wmissing-monadfail-instances``, ``-Wsemigroup``, ``-Wmissing-methods``
 - all ``-Wdodgy*``
 - ``-Wunrecognised-warning-flags``,  ``-Wunrecognised-pragmas``
 - ``-Wduplicate-exports``, ``-Wmissing-export-lists``
 - ``-Whi-shadowing``, ``-Wmissing-home-modules``
 - ``-Wimplicit-prelude``
 - ``-Wmissing-import-lists``, ``-Wunused-imports``
 - ``-Wtabs``
 - ``-Wunsupported-llvm-version``
 - ``-Wcpp-undef``

**whitelist**
 - ``-Wtyped-holes``
 - ``-Wpartial-type-signatures``
 - ``-Wmissed-specialisations``, ``-Wall-missed-specialisations``
 - ``-Wwarnings-deprecations``, ``-Wdeprecations``, ``-Wdeprecated-flags``
 - ``-Wunsupported-calling-conventions``
 - ``-Woverflowed-literals``
 - ``-Wempty-enumerations``
 - ``-Wsimplifiable-class-constraints``
 - ``-Widentities``
 - ``-Wimplicit-kind-vars``
 - ``-Wincomplete-patterns``, ``-Wincomplete-uni-patterns``
 - ``-Wincomplete-record-updates``
 - ``-Wmissing-fields``
 - ``-Wmissing-signatures``, ``-Wmissing-exported-signatures``, ``-Wmissing-local-signatures``, ``-Wmissing-pattern-synonym-signatures``
 - ``-Woverlapping-patterns``, ``-Winline-rule-shadowing``
 - ``-Worphans``
 - ``-Winaccessible-code``
 - ``-Wstar-binder``
 - ``-Wtype-defaults``
 - ``-Wmonomorphism-restriction``
 - ``-Wunticked-promoted-constructors``
 - ``-Wunused-binds``, ``-Wunused-top-binds``, ``-Wunused-local-binds``, ``-Wunused-pattern-binds``
 - ``-Wunused-do-bind``
 - ``-Wunused-foralls``
 - ``-Wunbanged-strict-patterns``
 
**there is some syntax for local suppression** (but we can add it in whitelist):
 - ``-Wredundant-constraints``
 - ``-Wname-shadowing``
 - ``-Wunused-matches``
 - ``-Wwrong-do-bind``
 - ``-Wunused-type-patterns``
 - ``-Wpartial-fields``
 
Costs and Drawbacks
-------------------

1) **Estimate on development and maintenance costs**

Some warnings can require individual way to collaborate with local using.

2) **Influence to learnability of the language**

These pragmas are optional pragmas and are non-essential for basic users of the language. The area of using intersects with ``OPTIONS_GHC`` pragma and as a result it does not require any more learning after the ``OPTIONS_GHC`` pragma. There is only one distinction - you need to learn where and how to place it inside the file (somewhat like the ``SCC`` pragma).

3) **Remaining drawbacks**

None.

Alternatives
------------

We proposed to create one pragma ``OPTIONS_LOCAL`` which works like ``OPTIONS_GHC`` and provides a local control warnings and language extensions. This idea was rejected because:

- every local language extension requires individual way of implementation and can sense what is different from the global sense
- using one name ``OPTIONS_LOCAL`` for warning is not so convenient

Unresolved Questions
--------------------

Would it be useful to add local language extensions? There are three ways to local work with language extensions:

1. Tweak the ``LANGUAGE`` pragma to be acceptable in other places, not only at the top.
2. Create a new pragma ``LANGUAGE_LOCAL``
3. Create individual local pragmas for every extension when it makes sense
4. Forget this idea

Implementation Plan
-------------------

There is `the proof of concept implementation <https://gitlab.haskell.org/ghc/ghc/merge_requests/1029>`_.
It doesn't use the proposed pragmas but demonstrates the basic idea of working with local warnings using a single pragma ``OPTIONS_LOCAL``.
