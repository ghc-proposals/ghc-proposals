Enable BangPatterns by default
===============================

.. author:: Andreas Klebinger
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/343>`_.
.. sectnum::
.. contents::

BangPatterns are a useful, popular and mostly harmless extensions
that should be enabled by default.

In the current eco system not doing so carries a higher cost than changing the default.

Motivation
----------

The BangPatterns extension being disabled by default carries a real cost
as it's often used by users, but it's hardly ever beneficial to disable it.

BangPatterns are incredible useful and enabling them comes at a very low cost
of giving up the ability to define the ``!`` operator in a certain way.

They give a convenient syntax to specify strict arguments which
as far as I can tell has been well received and is in wide use.

It seems to be the custom that code is in general written without
the extension enabled at first. With users only enabling as needed.
Common tools like hlint even encourage users to delete the pragma if
not currently used. The result is usually one of the following:

1. The extension is always enabled by the build tool.
2. The extension is added/removed in the source as needed.
3. The pragma is used independent of it's use.

All three options have drawbacks which are discussed further down.

I argue that the real world cost of BangPatterns being disabled
by default is higher than the cost of enabling the extension by default
would be.

For this reason I consider it the pragmatic thing to enable the extension by default
and propose as much.

Proposed Change Specification
-----------------------------

* When invoked GHC should behave as if -XBangPatterns has been passed,
* unless a language standard is requested. (Haskell98 or Haskell2010)
* GHC provides a new (unstandardized) language level -XGHC2020 which is
  based on Haskell2010 but includes the proposed change in behaviour.

Examples
--------

These invocations would enable the BangPatterns extensions:

::

 ghc WithBangs.hs
 ghc WithBangs.hs -XBangPatterns
 ghc WithBangs.hs -XGHC2020


These would not:

::

 ghc NoBangs.hs -XNoBangPatterns
 ghc NoBangs.hs -XHaskell98
 ghc NoBangs.hs -XHaskell2010

Effect and Interactions
-----------------------

This would allow use of BangPatterns without having to explicitly enable the extension.
I'm not aware of any significant interactions with other GHC extensions.

Build tools currently passing -XHaskell2010 would need to be updated to support the new
language level.

Costs and Drawbacks
-------------------

Code defining the ``!`` operator infix will break if this proposal is accepted.
This is easy to work around for actively maintained projects, but it is a cost.

Implementation and maintenance is hardly an issue as the extension is already implemented
and maintained.

This proposal would signify a change between from GHC towards the haskell standard.

Alternatives
------------

Defy -XHaskell2010
~~~~~~~~~~~~~~~~~~~~~

As an alternative to respecting the Haskell2010 standard we could change the default
behaviour even when -XHaskell2010 is requested.

While easier for build tools. It does not seem worthwhile overall.

Enabling of BangPatterns by users via build tools
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This can cause issues as the interpretation of source files becomes dependent
on external configurations. While it reduces the risk of files being compiled
without BangPatterns enabled it does not eliminate it.

Enabling of BangPatterns users in the source as needed
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

I generally use this approach (and it seems common). However I regularly find myself
hitting build errors as I forgot to enable the extension for a particular file.
It has also happened that I pushed changes to CI only to discover far later
that CI failed since the file in question did not have BangPatterns enabled.

In a similar fashion refactoring code can sometimes remove all bang patterns in a file.
As a consequence the now unused BangPatterns extension should be removed from the file.

Until eventually it's used again. At which point the build might fail again if I forgot
to re-add the extension.

Enabling of BangPatterns by the user independent of actual use
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

While this seems like a reasonable solution it still allows users to
forget about adding the extension pragma to new source files.

Users might also still get warnings from toolings about unused pragmas
and many consider unused pragmas bad practice.

Wait for a new Haskell standard
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

I do not expect a new standard to be released in the next few years.

Unresolved Questions
--------------------

None

Implementation Plan
-------------------
I would implement this change.