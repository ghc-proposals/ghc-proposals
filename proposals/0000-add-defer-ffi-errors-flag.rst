.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_. **After creating the pull request, edit this file again, update the number in the link, and delete this bold sentence.**

.. contents::

Add defer-ffi-errors flag
=========================

Add defer-ffi-errors flag to GHC that converts compile time ffi errors into runtime errors.
This would allow GHC to compile and typecheck code designed for other compilers such as GHCJS.


Motivation
------------

Currently if you want to use the FFI to call into javascript for GHCJS development, but still want GHC specific tooling, you have to do:

::

    #ifdef __GHCJS__
    foreign import javascript unsafe
        "foo()"
        foo :: IO ()
    #else
    foo :: IO ()
    foo = error "GHCJS required to use foo"
    #endif

This is verbose and ugly, and it also doesn't help in situations where you are importing functions from a library that does not do this.
Also if a compiler besides GHCJS gets JS support, none of the existing code that does the above is going to work, since ``__GHCJS__`` won't be defined.

Currently many GHCJS libraries such as react-hs have a lot of lines of code devoted to the above pattern.
We also currently have libraries like ``ghcjs-base-stub`` that you must `if impl(ghcjs)` into your cabal file in order to import from ``ghcjs-base`` without breaking ghc.
This also leads to it often being necessary to send PR's whenever a desired ``ghcjs-base`` feature is needed that is not yet in ``ghcjs-base-stub``.

Proposed Change Specification
-----------------------------

Add defer-ffi-errors flag that converts any compile time errors that occur due to ffi declerations into equivalent runtime errors that occur when using the imported value.

By default emit a warning when such a compile time error is converted, but add ``-Wno-deferred-ffi-errors`` which disables such warnings.

Effect and Interactions
-----------------------

This would make the `#ifdef` approach unnecessary, and you can instead enable this flag whenever compiling with GHC or using GHC powered tooling.

This means we can now just write:

::

    foreign import javascript unsafe
        "foo()"
        foo :: IO ()

Which is much nicer and more concise.

Should not have significant / contentious interactions with existing features.

Costs and Drawbacks
-------------------

Minimal development and maintenance costs.
Could potentially lead to a runtime error occuring that would have been caught at compile time if flag is accidentally turned on and warning accidentally ignored.

Alternatives
------------

Continue to use `#ifdef` as explained above, which has the significant drawbacks mentioned above.

Add true JavaScript support to GHC, which would be better than this proposal, but also much harder to implement and maintain, and this change would not inhibit such a possibility.


Unresolved questions
--------------------

None

Implementation Plan
-------------------

I would be open to implementing the change. No significant resources or prerequisites needed.
