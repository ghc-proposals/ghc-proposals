.. proposal-number:: 
.. trac-ticket::
.. implemented:: 
.. highlight:: haskell

Use `%rsp` for the SP on x86_64
===============================

Motivation
----------

We currently use `%rbp` for the stack pointer on x86_64.  It might be a good idea to switch to using `%rsp`; this proposal is to collect the evidence and rationale and come to a conclusion.

Why we might want this:

* We could potentially use the `ret` instruction for returning.  This is one byte shorter than `jmp *(%rbp)`, and might benefit from better CPU optimisations.  Using `call` is not possible, because the return address has an info table preceding it.
* Some tools (e.g. perf) look at `%rsp` to collect information about the stack.  This might enable smoother integration with external tooling, especially in conjunction with DWARF.  (TODO: flesh this out).

Proposed Change
---------------

Swap the purposes of `%rbp` and `%rsp`; make `%rbp` point to the spill area on the C stack, and make `%rsp` point to the Haskell stack.

Drawbacks
---------

* Unfortunately the instruction encoding for instructions using `%rsp` is often longer than for `%rbp`.  e.g. `mov  %rax,-0x8(%rsp)` is 5 bytes compared with 4 bytes for the corresponding instruction using `%rbp`.  So despite the 1-byte saving for a `ret` instruction, code would likely get larger if we used `%rsp` for the stack.

* We have to worry about signals messing up the stack, but we have a 128-byte red zone on x86_64.  (what fallback mechanisms would be required if we needed more than this?)

Alternatives
------------

Don't do this :)

Unresolved Questions
--------------------

See TODOs above.
