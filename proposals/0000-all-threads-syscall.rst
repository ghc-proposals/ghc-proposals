All Threads Syscall
===================

.. author:: Nicolas Trangez
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/533>`_.
.. sectnum::
.. contents::

Several Linux kernel security features exposed as syscalls are applied to the
thread invoking such syscall only, then inherited by newly created threads.
Since the GHC threaded RTS creates multiple threads before Haskell code is
executed, we need a way to invoke some syscall in all running threads.


Motivation
----------
In Linux, many attributes of the context in which code runs is kept per thread
instead of process-global, as often assumed. As an example, the ``setuid``
syscall will only change the UID of the thread invoking the syscall, not of
all threads running in the process, as required by POSIX.

This is often not an issue because the ``libc`` implementation of such library
functions jump through hoops to ensure the syscalls are executed in every
running thread before returning, effectively guaranteeing POSIX compliance.
This is known as the ``setxid`` functionality, which is internal to the libc
library.

As such, when calling the (C library) ``setuid`` function from within Haskell
code will result in the desired result: all code in the process is now running
using the new UID.

Several Linux syscalls don't have ``libc`` wrappers (yet), or aren't applied
using ``setxid``-like mechanisms. In C codebases, a developer would ensure
all attributes are set by code running in the main thread before any new
threads are spawned. However, in software using some managed runtime system,
said runtime may spawn threads before application code is launched, which is
the case when using the GHC threaded RTS.

There are work-arounds available (see below), though these are brittle and rely
on RTS implementation details. Hence, other platforms (e.g., Go) introduced
the ability to invoke a syscall in all (managed) threads.

Proposed Change Specification
-----------------------------
On Linux, the GHC threaded RTS will be extended with a new function::

    long rts_allThreadsSyscall(long num,
                               long arg1, long arg2, long arg3,
                               long arg4, long arg5, long arg6)

This function will promptly invoke the given syscall ``num`` with the given
args in all (POSIX) threads managed by the RTS.

The semantics of this function are the same as those of |glibc|_
|nptl_setxid|_ or Go_'s |syscall_runtime_doAllThreadsSyscall|_:

- First, the syscall is invoked in the calling thread. Keep track of the
  result.

- Then, set up machinery to invoke the syscall in all (other) running threads,
  and wait for them to accomplish this task, blocking the calling thread.

- If during the invocation of the syscall in any of the other threads the result
  is *not* equal to the first invocation, abort the process.

- Return the result.

This function should be exposed to C code in Haskell packages as other
functions of the RTS are (TODO how is that?), and could optionally be exposed
in a Haskell module (as it is exposed in the Go standard library).

.. |glibc| replace:: ``glibc``
.. _glibc: https://www.gnu.org/software/libc/
.. |nptl_setxid| replace:: ``__nptl_setxid``
.. _nptl_setxid: https://elixir.bootlin.com/glibc/glibc-2.36.9000/source/nptl/nptl_setxid.c#L175
.. _Go: https://go.dev/
.. |syscall_runtime_doAllThreadsSyscall| replace:: ``syscall_runtime_doAllThreadsSyscall``
.. _syscall_runtime_doAllThreadsSyscall: https://github.com/golang/go/blob/f983a9340d5660a9655b63a371966b5df69be8c5/src/runtime/os_linux.go#L708

Examples
--------
Ensure no privileged (think SUID) binaries can be spawned from the current
process::

    int rc = rts_allThreadsSyscall(__NR_prctl, PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0, 0);
    assert(rc == 0);

After initial setup, use Landlock to restrict access of the running process::

    int rc = rts_allThreadsSyscall(__NR_landlock_restrict_self, ruleset_fd, 0, 0, 0, 0, 0);

Effect and Interactions
-----------------------
Depending on the implementation details, this proposal could affect code that
relies on specific (POSIX) signals if one such signal is repurposed.

Costs and Drawbacks
-------------------
TODO

Alternatives
------------
As an alternative, instead of providing this functionality as part of the RTS,
a C library developed as part of |libcap|_ can be used: |libpsx|_. This
works, functionally, but comes with several drawbacks:

- ``libpsx`` itself needs to be hooked in the involved process by wrapping
  ``pthread_create`` for the library to track all running threads. This
  requires linker options.

- ``libpsx`` uses a signal (``SIGSYS``) to invoke its syscall calling code in
  all threads. If a thread masks this signal (which, e.g., the GHC RTS *ticker*
  thread does), this results in a deadlock. As a work-around, ``sigfillset``
  can be wrapped to clear the ``SIGSYS`` bit, but this is obviously a hack.
  (Note how the ``glibc`` ``sigfillset`` implementation actually clears the
  bit of the signal it uses internally to implement ``setxid`` in the given
  set!)

This is implemented in the |psx|_ package (as used by |landlock|_) and works.
However, it breaks if the RTS ever adds threads which do mask ``SIGSYS`` using
other means, or if threads are ever created without going through
``pthread_create``.

.. |libcap| replace:: ``libcap``
.. _libcap: https://git.kernel.org/pub/scm/libs/libcap/libcap.git/
.. |libpsx| replace:: ``libpsx``
.. _libpsx: https://git.kernel.org/pub/scm/libs/libcap/libcap.git/tree/psx
.. |psx| replace:: ``psx``
.. _psx: https://hackage.haskell.org/package/psx
.. |landlock| replace:: ``landlock``
.. _landlock: https://hackage.haskell.org/package/landlock

Unresolved Questions
--------------------

Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other resources
and prerequisites are required for implementation?

Endorsements
-------------
