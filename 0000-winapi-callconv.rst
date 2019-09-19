.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

Add winapi pseudo calling convention
====================================

On Windows when calling Windows API functions the calling conventions differ
based on the architecture. The correct calling convention is often gotten wrong
and GHC's suggestion is somewhat misleading. A new pseudo calling convention
would solve these issues.

Motivation
----------

On `x86` the default calling convention for Windows API calls is `stdcall`,
but on `x86_64` it is a variant of `__fastcall`.

Due to historical reasons people tend to think `stdcall` is the actual calling
convention for both. On `x86_64` GHC would then generate the following warning

::

    * the 'stdcall' calling convention is unsupported on this platform,
      treating as ccall
    * When checking declaration:
        foreign import stdcall unsafe "static shlobj.h ShellExecuteA" c_ShellExecuteA


Which is confusing as it's technically incorrect, but also users seem to replace the
`stdcall` with `cdecl` in the source, which ends up breaking `x86`.

The solution to this ends up needing to use `CPP` to correctly choose the right
calling convention. This `CPP` has to be repeated over and over and over again.

Only looking at base we see this workaround is used:

::

    > git grep WINDOWS_CCONV | wc -l
    87

and in `Win32`

::

    > git grep WINDOWS_CCONV | wc -l
    525

Proposed Change
---------------

The Haskell 2010 FFI specs allow for the flexibility to implement custom calling
conventions. In fact we already do this with the `CApi` calling convention.

It would simplify code, ease confusion and remove the need for `CPP` if we have
a new pseudo calling convention `winapi` that is to be used specifically for calls
to Windows API calls.

Concretely I propose having the following:

::

   foreign import winapi "foo" c_foo :: IO ()

Where the compiler will determine the right calling convention to use for the
target platform.

I do not propose to change the warning, as for general code, `cdecl` is the right
suggestion as native compilers will correctly reinterpret this to the x86_64 ABI
calling convention.

The maintenance should be fairly low as the ABI is set and won't change.

I propose this to be added without needing a language extension, as it does not
modify the language at all.  It just makes use of clauses already in the existing
specification.

Drawbacks
---------

foreign exports become slightly confusing. The calling convention should really
only be used for `imports` and `wrappers` but the FFI specification does not
allow the flexibily of having calling conventions only one way.

Alternatives
------------

The standard pattern used to work around this is usually

::

    #if defined(i386_HOST_ARCH)
    # define WINDOWS_CCONV stdcall
    #elif defined(x86_64_HOST_ARCH)
    # define WINDOWS_CCONV ccall
    #else
    # error Unknown mingw32 arch
    #endif

An alternative implementation would be to extend the list of standard CPP defines
for GHC to include `WINDOWS_CCONV`. This however means you still need to have
`{-# LANGUAGE CPP #-}` in order to use.

Unresolved Questions
--------------------

What should happen with `foreign export` code.

I propose to let

.. code-block::

   foreign export winapi "foo" c_foo :: IO ()


have the same semantics as import for determining
the calling convention but also generate a warning such as

::

    * the 'winapi' calling convention is not supported for exports,
      treating as ccall
