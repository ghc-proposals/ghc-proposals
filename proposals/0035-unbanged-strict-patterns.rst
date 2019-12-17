Make unboxed tuple patterns lazy / warn on unbanged strict patterns
===================================================================

.. author:: Richard Eisenberg
.. date-accepted:: 2018-09-12
.. ticket-url:: https://phabricator.haskell.org/D2842
.. implemented::
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/35>`_.
.. highlight:: haskell
.. contents::

(Throughout this proposal, unboxed sums are treated identically to unboxed tuples. I refer only to unboxed tuples for simplicity.)

Despite the fact that unboxed tuples are unlifted, the `manual <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unboxed-tuples>`_ says that unboxed tuple patterns can actually be *lazy*. This is not working
in GHC 8.0; unboxed tuple patterns are strict, in violation of the manual. However, it is trivial in the implementation to
make unboxed tuple patterns that do not bind any variables of unlifted types to be lazy. (Recall that unboxed tuples can
easily accommodate storing lifted types.) This proposal moves to make the implementation agree with the manual by making
unboxed tuple patterns that do not bind any variables of unlifted types to be lazy. Call this change (A).

Separately, any pattern that binds a variable of an unlifted type must be strict, because unlifted variables can never refer to thunks.
For some time now (many years), such patterns must also be banged, to make it obvious to users that the pattern is strict.
(Otherwise, the user might not realize that there is an unlifted type in the mix.) However, if we now include unboxed tuples
in this scheme (that is, requiring bangs on unboxed tuple patterns that bind a variable of an unlifted type), many programs will
break. Thus this proposal also moves to make the check for a bang on a pattern that binds a variable of an unlifted type into
a warning instead of an error. Call this change (B).

Because bang-patterns are backward compatible, change (B)'s new warning does not violate the three-release-policy, as the fix is
fully backward-compatible.

Motivation
------------

1. The manual is effectively GHC's specification. GHC 8.0 is in violation of the specification in this regard. Change (A)
brings us into compliance.

2. Change (A) allows *more* programs to terminate and makes the language more expressive.

3. Change (A) simplifies the specification of when a pattern is strict, dropping the current special case for unboxed tuples.

4. Change (B) turns an error that is really about coding style into a warning, as it should be (in my opinion). GHC has no
trouble compiling strict patterns without a bang -- it's all about user expectations.

5. Change (B) is necessary to allow change (A) not to break code in the wild.

By way of history: I did not set out to change this directly, but I hit upon the
inconsistency between the implementation and the specification in other work. The code
was quite tangled, and this is a cleaning-up of this whole area. But the cleaning-up
has user-facing effects, leading to this proposal.

Proposed Change Specification
-----------------------------

Define an "unlifted-var pattern" to be any pattern that binds a variable with an unlifted type. Note that ::

    x :: Char
    (# True, x #) = blah

is *not* an unlifted-var pattern.

Change (A) says that unlifted-var patterns, and only unlifted-var patterns, are strict by default.

Before Change (A), unboxed tuple patterns were considered as unlifted-var patterns, regardless of whether
or not a variable of unlifted type was bound.

Change (B) says that unlifted-var patterns must have a bang. Otherwise a warning (``-Wunbanged-strict-patterns``) is issued.

 * Bare variables do not need a bang.
 * The bang may occur outside of or within an as-pattern.
 * Bare wildcards do not need a bang.

Before Change (B), unboxed tuples patterns did not need a bang.

Examples
--------

1.

    ::

        z = ()
          where x :: Bool
                (# x #) = undefined

    Evaluating ``z`` throws an error in GHC 8.0, but will result in ``()`` under this proposal. This change in semantics will make strictly more programs terminate, but it could introduce hitherto-unexpected laziness.

2.

    ::

        z = ()
          where x :: Int#
                !(# x #) = undefined

    Evaluating ``z`` throws an exception, both before and after this proposal. This proposal requires the presence of the bang.

3.

    ::

        z = ()
          where x :: Bool
                (# 3#, x #) = (# 4#, undefined #)

    Evaluating ``z`` results in ``()``. This is another example of the semantics change.

4.

    ::

        z = ()
          where 3# = 4#

    Evaluating ``z`` results in ``()``. The ``3#`` pattern is not an unlifted-var pattern, according to the rules above. This is a change in the implementation compared to GHC 8.0, but the behavior described here seems more in keeping with the specification of lazy bindings in Haskell.

5.

    ::

        z = ()
          where I# x = 4

    This code is rejected by GHC 8.0 with an error. Change (B) makes this error into a warning. The binding is strict.

Effect and Interactions
-----------------------

See the examples above for some brief discussion of the change in semantics.

In general, this proposal is a *simplification* of our rules around unlifted bindings.

Although not harped on in this proposal, unlifted-var patterns are subject to two further restrictions other than
strictness: they must not be recursive, and they must not bind any variables that have a polymorphic type.
Previously, this restriction applied also to unboxed tuple patterns, but change (A) removes this behavior.
So, the following is rejected in GHC 8.0 but accepted under this proposal::

    z = (f 'x', f True)
      where (# f #) = (# id #)

Costs and Drawbacks
-------------------

The drawback is the change in semantics. As a simplification in previous behavior, this proposal has a net benefit into perpetuity.


Alternatives
------------
We do not have to do (A). It is easy enough to retain the existing behavior. But it is a special case, both in the code and in the manual.

There is also a middle ground for (A) around unboxed tuples: we could pretend they always have a bang on them. That means that ::

    z = ()
      where (# x #) = undefined

would diverge because of the implicit bang on the unboxed-tuple pattern. This implicit bang could be surpressed with an explicit
``~``::

    z = ()
      where ~(# x #) = undefined

would still evaluate to ``()``. This is still a change from existing behavior, where lazy unboxed tuple bindings are impossible to write, and unboxed tuples are subject to the other restrictions above. (In this "middle ground" proposal, an unboxed tuple binding would still be allowed to be recursive, say.)

For (B), we could keep the error as is, which would mean (in concert with (A)) breaking code.


Unresolved questions
--------------------

Where in the spectrum of warning flags should ``-Wunbanged-strict-patterns`` sit? You can see the current state of play
in the `manual <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html#warnings-and-sanity-checking>`_.
The choices are:

* On by default
* In ``-W``
* In ``-Wall``


Implementation Plan
-------------------
This is already implemented, but it is easy enough to tweak the design.
