Add integer-openssl as BSD-licensed alternative for integer-gmp
==============

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

Proposal to add a fast, BSD-licensed alternative for GHC's ``Integer`` type,
next to currently available ``integer-gmp`` (default, fast and LPGL3-licensed)
and ``integer-simple`` (slow, but pure and BSD3-licensed) implementations.
Concretely, this would use OpenSSLs "BIGNUM" arbitrary size integer library
contained in ``libcrypto``.
 
Motivation
------------

By default, GHC uses ``integer-gmp`` as implementation for the ``GHC.Integer``
interface. The GNU MP Bignum Library is distributed dual licensed under `LGPLv3
<https://www.gnu.org/licenses/lgpl.html>` and `GPLv2
<https://www.gnu.org/licenses/gpl-2.0.html>`. Although being fast and available
on many platforms, these licenses are sometimes undesirable:

* when statically linking (e.g. on Windows)
* when "Convey the object code in, or embodied in, a physical product" (LGPLv3 -> GPLv3)

The only available alternative is ``integer-simple``, which is a pure haskell
implementation of the ``GHC.Integer`` interface and is `BSD3
<https://opensource.org/licenses/BSD-3-Clause>` licensed. However, it is quite
slow and infeasible for contexts where big integer multiplication / division are
in fact required, e.g. public key cryptography.

So, the goal is to have a fast (enough), BSD3-licensed alternative for the
``Integer`` data type available in GHC.

Proposed Change Specification
-----------------------------

A new integer library ``integer-openssl`` is added to GHC source tree as
submodule and selectable via both (Makefile and hadrian) build systems.
``integer-gmp`` should remain the default integer library, but instructions and
documentation is updated to allow GHC users to build the compiler using
``integer-openssl``.

``integer-openssl`` will implement the portable interface of ``GHC.Integer``
(and ``GHC.Integer.Logarithms`` and ``GHC.Integer.Logarithms.Internals`` if
required - see open questions).

Libraries depending on GMP internals will be updated to work with
``integer-openssl`` like they do with work currently with ``integer-simple``.

Effect and Interactions
-----------------------

By having an alternative integer library, users of GHC can switch the
``Integer`` backend to ``integer-openssl`` and get a ``ghc`` which is not linked
against ``libgmp`` but instead ``libcrypto``.

Haskell packages using ``integer-gmp`` internals, and thus depending on
``integer-gmp``, usually have flags to switch between ``integer-gmp`` and
``integer-simple``, where the latter typically conforms to the "portable"
interface. Corresponding flags need to be set by users of those packages when
they use a GHC compiled against ``integer-openssl``.

In summary, ``integer-openssl`` would be completely opt-in and if packages were
properly switching between GMP-specific and the standard ``GHC.Integer``
interface before, they will be able to do as well with ``integer-openssl``.


Costs and Drawbacks
-------------------

Cost for maintaining another integer library is of course existing, as it is now
with ``integer-simple``. It depends on how stable the ``GHC.Integer`` interface
is (feedback on any planned changes welcome!).

Alternatives
------------

Similar efforts for having a permissive licensed integer library existed in the
past and were discussed or prototypically implemented so far.

* a faster than ``integer-simple``, pure haskell implementation:
  `https://github.com/erikd/haskell-big-integer-experiment`
* Wiki page about replacing GMP
  `https://ghc.haskell.org/trac/ghc/wiki/ReplacingGMPNotes`


Unresolved questions
--------------------

What's the "portable" interface including? Right now there is code in ``base``
which does not only include ``GHC.Integer`` but also ``GHC.Integer.Logarithms``
and even ``GHC.Integer.Logarithms.Internals``.

Implementation Plan
-------------------
(Optional) If accepted who will implement the change? Which other ressources and prerequisites are required for implementation?

1) Implement the "portable" ``GHC.Integer`` interface for 32bit and 64bit in a
   library, where implementation is tested and benchmarked against the builtin
   one.

   Currently, about half of the interface are implemented in
   `https://github.com/ch1bo/integer-openssl`.

2) Make ``integer-openssl`` a build option for both, Makefile and Hadrian based
   build of GHC.

   This involves small modifications in ``ghc``, ``base``, ``bytestring`` and
   ``text`` (mostly ``.cabal`` files in libraries).

   A working in progress is available on `https://github.com/ch1bo/ghc`
