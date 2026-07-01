Ghc CPP
=======

.. author:: Alan Zimmerman
.. date-accepted:: 
.. ticket-url::
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/616>`_.
.. sectnum::
.. contents::

Haskell code uses the ``CPP`` extension extensively. I'm proposing to
introduce a new ``GhcCPP`` extension to perform a subset of CPP processing as
an alternative, as part of the normal GHC compilation process.

This subset will be just enough to control conditional compilation,
and will only be active in lines introduced with a ``#`` and
preprocessor keyword.

It will be tooling friendly, in that parts of the code disabled by the
processor will be saved as comments, if comments are being processed.


Motivation
----------

Haskell code uses the ``CPP`` extension to manage version changes, when the
compiler or a library changes.

CPP gets the job done, but it is not Haskell-native, and so has a
side-effect of not being tooling-friendly. It runs as a separate pass
over the source code, emitting updated source for compilation which
can actually change pretty much anything, if a person decides to use
the full power of CPP to redefine existing Haskell, or introduce
macros to generate boilerplate code.

The primary motivation for this proposal is to make it possible to
safely perform source-code transformations on code containing
preprocessor directives.

Currently in ``ghc-exactprint`` when a file uses CPP, the original
file and preprocessor output are diffed at the token level and the
"missing" parts inserted as comments. This works in some cases, but
for many it fails, for reasons such as

- A comparison to a numeric constant:

.. code:: haskell

   #if __GLASGOW_HASKELL__ < 707

Shows up as a stray ``906`` if compiled with GHC 9.6.x

.. code:: haskell

   #if __GLASGOW_HASKELL__906 < 707

- Preprocessor directives inside haskell comments are also processed.
  This makes the diff fail too.

- Trailing backslashes are eliminated, and the lines joined up. This
  breaks some ASCII art comments, as well as multi-line strings.

By implementing a subset of CPP processing inside GHC, we can avoid
these drawbacks, and provide conditional compilation facilities that
cover most of the "normal" current needs.

The processing will be explicitly limited to lines introduced by a
standard CPP directive, such as ``#define FOO``, and operate at the
GHC Lexer level. This means that it will not touch directives
appearing inside comments, nor will CPP macros be applies anywhere but
in other CPP directives.


Proposed Change Specification
-----------------------------

When enabled, the lexer has the additional rules:

.. code:: haskell

   cpp -> '#' cppkeyword [ anysymbol {any} ] newline
         | '#' cppkeyword [ anysymbol {any} ] '\' newline cppcont
   cppcont -> [ anysymbol {any} ] '\' newline cppcont
         |  [ anysymbol {any} ] newline
   cppkeyword -> 'define' | 'include' | 'undef' | 'error' | 'ifdef'
                 | 'ifndef' | 'if' | 'elif' | 'else' | 'endif'

   ccomment = '/*'  [ anysymbol {any} ] '*/'
   whitestuff -> whitechar | comment | ncomment | ccomment

Note: as per
https://gcc.gnu.org/onlinedocs/cpp/Initial-processing.html the
CPP-style comments do not nest. They are needed because they are valid
CPP syntax, and are emitted in files such as ``cabal_macros.h``.

The only tokens emitted are ``cpp`` and ``cppcont``. The additional
comment type is stored with the others if they are being kept.

These will be passed to the actual preprocessor for processing. This will

- Accumulate continued ``cpp`` and ``cppcont`` tokens until a full CPP
  directive line is constructed.
- Apply the standard CPP processing to this line. (Based on
  https://gcc.gnu.org/onlinedocs/cpp/Overview.html)
- Any preprocessor state will be inserted into an (opaque) field in
  the parser state, made available to it by get and set operators.
- Any include file processing will temporarily switch the lexer input,
  then continue.  See https://gcc.gnu.org/onlinedocs/cpp/Header-Files.html


The result of this processing is to put the preprocessor into one of two states

- Normal
- Ignoring

In **Normal** mode it passes all non-cpp tokens on to the parser as
normal.

In **Ignoring** mode it converts all non-cpp tokens into comments,
which are stored or not as per normal comment processing.

In either case, the ``cpp`` and ``cppcont`` tokens are converted to
comments, as above, and not passed on to the parser.

Example:

.. code:: haskell

   {-# LANGUAGE GhcCPP #-}
   #define FOO
   #ifndef FOO
   x = 1
   #else
   x = 5
   #endif

Results in the following token stream (showing comments as they are lexed):

.. code:: haskell

   Comment ({-# LANGUAGE GhcCPP #-})
   Comment (ITcpp "#define FOO")
   Comment (ITcpp "#ifndef FOO")
   Comment (ITvarid "x")
   Comment (ITequal)
   Comment (ITinteger 1)
   Comment (ITcpp "#else")
   ITvarid "x"
   ITequal
   ITinteger 5
   Comment (ITcpp "#endif")
   ITeof

Note that the commented out region is shown as being individual
tokens. In the GHC Lexer they are pushed into the parser state and
attached to the appropriate ``ParsedSource`` during parsing, if
``Opt_KeepRawTokenStream`` is set.

They can be recombined during lexing, or afterwards in anything using
the tokens.


Proposed Library Change Specification
-------------------------------------

N/A

Examples
--------

Effect and Interactions
-----------------------

There may be potential interactions between this extension and CPP.
It would probably be wise to emit a warning if both are enabled at the same time.

We will have to ensure that the appropriate file search paths for any
``#include "filename"`` directives match what would happen in the CPP
case.

Also, to do include file processing, the preprocessor leg at least
will have to be in the IO monad, or have some protocol to request the
source for an include.


Costs and Drawbacks
-------------------
The main cost is some additional complexity in the lexer, to emit the
preprocessor tokens when the pragma is enabled. This should be a fixed
change though, and the actual preprocessor processing happens in
another layer. So it should not adversely affect ongoing language
evolution in the GHC lexer and parser.


Backward Compatibility
----------------------

This proposal introduces a new extension, ``GhcCPP``, as an alternative
to the existing ``CPP`` extension.

It explicitly does not set out to be a drop-in replacement for
``CPP``, as it has a reduced and changed functionality.

As it is a pragma, it is up to potential users of the feature to turn
it on in a case-by-case basis as part of their development process.
They can always fall back to ``CPP`` if the effects are not what they
expect.

Will your proposed change cause any existing programs to change behaviour or
stop working? Assess the expected impact on existing code on the following scale:

0. No breakage



Alternatives
------------
List alternative designs to your proposed change. Both existing
workarounds, or alternative choices for the changes. Explain
the reasons for choosing the proposed change over these alternative:
*e.g.* they can be cheaper but insufficient, or better but too
expensive. Or something else.

The PR discussion often raises other potential designs, and they should be
added to this section. Similarly, if the proposed change
specification changes significantly, the old one should be listed in
this section.

Unresolved Questions
--------------------
TBD

Implementation Plan
-------------------

If accepted, the author (@alanz) will implement it.
There is already some preliminary work on
https://gitlab.haskell.org/ghc/ghc/-/tree/wip/az/ghc-cpp

Endorsements
-------------
Yes please
