.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/87>`_.

.. contents::

ExtraCommas
==============

The `Haskell 2010 Report <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1000005.2>`_ permits a trailing comma in the main export list.
The subexport list, for data constructors of a type, does not permit a trailing or leading comma.
This proposal extends GHC's syntax to allow leading and trailing commas in delimited enumerations, including export lists, import lists, contraints, lists, etc.

Motivation
------------

This proposal was initially motivated by `this Trac issue <https://ghc.haskell.org/trac/ghc/ticket/12389>`_, where extraneous warnings are generated for duplicate/redundant exports in the presence of CPP macros.
The issue reported that in order to avoid warnings, every permutation of exports had to be defined::

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

When languages support trailing commas, style guides tend to require or suggest them in enumerations spanning multiple lines:

1. `Thoughtbot JavaScript style guide <https://github.com/thoughtbot/guides/tree/master/style/javascript>`_
#. `Airbnb Ruby style guide <https://github.com/airbnb/ruby#multiline-hashes>`_
#. `PEP8 Python style guide <https://www.python.org/dev/peps/pep-0008/#when-to-use-trailing-commas>`_
#. `PSR-2 Extension to PHP coding style <https://github.com/php-fig-rectified/fig-rectified-standards/blob/master/PSR-2-R-coding-style-guide-additions.md>`_
#. `This C# Style guide <https://github.com/dvdsgl/csharp-in-style#enums>`_

As the export list already supports trailing commas, it makes sense to allow subexport lists to also support trailing commas.

For the most part, leading commas have the same arguments as trailing commas.
There is much less discussion of this online, as Haskell appears to be one of the only languages that generally prefers leading commas for lists of any sort.

Extending these changes to other uses of comma delimited lists confers the same benefits to these uses.

An exception is made for tuple values.
The syntax `(a, b,)` in a tuple would conflict with the `TupleSections` language extension.

Proposed Change Specification
-----------------------------

For each of these:

* record-like occurences (declarations, patterns, constructions, etc)
* module export lists
* module import lists
* lists literals (i.e. [], both type-level & term-level)
* fixity declarations
* comma-separated enumerations in type-signatures (e.g. (Monad m, Monoid m) => ...)
* (maybe) pattern guards

Modify the grammar to accept a trailing and leading comma.

As an example, the grammar for export items is currently::

    export -> qvar
            | qtycon[(..)|(cname_1, ..., cname_n)]  (n >= 0)
            | qtycls[(..)|(var_1, ..., var_n)]      (n >= 0)
            | module modid

This proposal will change the sublists in the ``qtycon`` and ``qtycls`` to have this form::

    subList -> (, commaList)
             | (commaList [,])
                 
    commaList -> id_1, ..., id_n (n >= 0)

Other syntaxes will follow the same model.

Effect and Interactions
-----------------------

This proposal provides a solution for the initial issue as described in the motivation.
The problem code is repeated::

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

Given trailing and leading commas, one could instead write::

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

There is a potential interaction with ``TupleSections``, if this change were allowed for tuple values.
``TupleSections`` will interpret ``(a, b, )`` as ``(a, b, ) :: c -> (a, b, c)``, while ``ExtraCommas`` would interpret it as ``(a, b, ) :: (a, b)``.


Costs and Drawbacks
-------------------

Implementing this requires a change to the parser and the creation of a new language extension.
Many languages in common use support trailing commas in certain contexts, so this is unlikely to be confusing for people coming from other language.

Alternatives
------------

1. Extend this change to value-level tuples (this seems like it would be much more invasive, especially considering ``TupleSections``).

Unresolved questions
--------------------

Implementation Plan
-------------------

A patch to the parser has been made in `this Phabricator diff <https://phabricator.haskell.org/D4134>`_ to implement trailing and leading commas in the subexport list.
That work can be extended to include the other cases.
