.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: 12389

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/87>`_.

.. contents::

Trailing and leading commas for subexport lists
==============

The `Haskell 2010 Report <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1000005.2>`_ permits a trailing comma in the main export list.
The subexport list, for data constructors of a type, does not permit a trailing or leading comma.
This proposal extends GHC's syntax for subexport lists to allow leading and trailing commas.

Motivation
------------

This proposal is initially motivated by `this Trac issue <https://ghc.haskell.org/trac/ghc/ticket/12389>`_, where extraneous warnings are generated for duplicate/redundant exports in the presence of CPP macros.
The issue reported that in order to avoid warnings, every permutation of exports had to be defined:

::
 module Foo (
 #ifdef TESTING
 #ifdef USE_PATTERN_SYNONYMS
   Foo (Foo, Pat1, Pat2)
 #else
   Foo (Foo)
 #endif
 #elif USE_PATTERN_SYNONYMS
   Foo (Pat1, Pat2)
 #else
   Foo
 #endif

Trailing and leading commas would allow the code to reduce duplication and noise.

More generally, permitting leading and trailing commas is helpful for providing cleaner diffs and more consistent syntax in multiline lists.

When languages support trailing commas, they tend to show up as required or suggested in style guides:

1. `Thoughtbot JavaScript style guide <https://github.com/thoughtbot/guides/tree/master/style/javascript>`_
#. `Airbnb Ruby style guide <https://github.com/airbnb/ruby#multiline-hashes>`_
#. `PEP8 Python style guide <https://www.python.org/dev/peps/pep-0008/#when-to-use-trailing-commas>`_
#. `PSR-2 Extension to PHP coding style <https://github.com/php-fig-rectified/fig-rectified-standards/blob/master/PSR-2-R-coding-style-guide-additions.md>`_
#. `This C# Style guide <https://github.com/dvdsgl/csharp-in-style#enums>`_

As the export list already supports trailing commas, it makes sense to allow subexport lists to also support trailing commas.

For the most part, leading commas have the same arguments as trailing commas.
There is much less discussion of this online, as Haskell appears to be one of the only languages that generally prefers leading commas for lists of any sort.

Proposed Change Specification
-----------------------------
The grammar for export items is currenty:

::
 export -> qvar
         | qtycon[(..)|(cname_1, ..., cname_n)]  (n >= 0)
         | qtycls[(..)|(var_1, ..., var_n)]      (n >= 0)
         | module modid

This proposal will change the sublists in the ``qtycon`` and ``qtycls`` to have this form:

::
 ([,]id_1, ..., id_n [,]) (n >= 0)

Effect and Interactions
-----------------------
This proposal provides a solution for the initial issue as described in the motivation.
The problem code is repeated:

::
 module Foo (
 #ifdef TESTING
 #ifdef USE_PATTERN_SYNONYMS
   Foo (Foo, Pat1, Pat2)
 #else
   Foo (Foo)
 #endif
 #elif USE_PATTERN_SYNONYMS
   Foo (Pat1, Pat2)
 #else
   Foo
 #endif

Given trailing and leading commas, one could instead write:

::
 module Foo (
   Foo(
 #ifdef TESTING
     , Foo
 #endif
 #if USE_PATTERN_SYNONYMS
     , Pat1
     , Pat2
 #endif
 )

Costs and Drawbacks
-------------------
Implementing this change is a small modification to the Haskell grammar and parser.
Many languages in common use support trailing commas in certain contexts, so this is unlikely to be confusing.

However, people might wonder why they can use a trailing/leading comma in an export list, but not in a Haskell list or tuple, or a Haskell record declaration.
Currently, trailing commas are permitted in the export list and `in import lists (but not import sub-lists) <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1010005.3>`_.

Alternatives
------------

1. Only allow a trailing comma in the sub-export list.
   This is consistent with the main export list, but will mean that the original issue will need to use somewhat un-idiomatic trailing commas in the sub-export list.
#. Extend this change to import sub-lists, for consistency.
#. Extend this change to record declarations as well.
#. Extend this change to value-level lists and tuples (this seems like it would be much more invasive, especially considering ``TupleSections``).

Unresolved questions
--------------------
Simon Peyton Jones posed the following questions:

1. It should be consistent with exports lists themselves. 
   Do they allow leading commas? If not, it'd make sense to add them. 
   Thus ``module M( , f, g, ) where ...``
#. Do we allow multiple leading or trailing commas?
   What about repeated commas in the middle of a list?
#. What about import lists? Should they not be consistent?
#. Should we require a language extension flag?

Implementation Plan
-------------------
A patch to the parser has been made in `this Phabricator diff <https://phabricator.haskell.org/D4134>`_ to implement trailing and leading commas in the subexport list.
