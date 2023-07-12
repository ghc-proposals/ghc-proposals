``WARNING`` pragmas with categories
===================================

.. author:: Adam Gundry
.. date-accepted:: 2022-12-21
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/17209
.. implemented:: 9.8
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/541>`_.
.. sectnum::
.. contents::

This proposal extends the syntax of ``WARNING`` pragmas to allow them to specify
a "warning category".  Users can then enable or disable specific
warning categories using command-line flags.  This will make it more reasonable
to attach ``WARNING`` pragmas in cases where it is desirable to give users the
option to disable the warning individually.


Motivation
----------

The recent `core-libraries proposal #87
<https://github.com/haskell/core-libraries-committee/issues/87>`_, "Add {-#
WARNING #-} to Data.List.{head,tail}", has lead to a great deal of debate.  That
proposal seeks to discourage users from using certain partial ``Prelude``
functions through the use of ``WARNING`` pragmas, which cause warnings to be
emitted when the functions are used.  It has been controversial because, while
there is broad consensus that partial functions should *usually* be avoided,
there is less consensus that they should *never* be used.

A key problem with ``WARNING`` pragmas as they currently exist is that there is
no way to disable them selectively. It is possible to disable them entirely,
with ``-Wno-warnings-deprecations``, but this will suppress all warnings from
``WARNING`` pragmas, regardless of source.  Thus if the Core Libraries Committee
decide to add a ``WARNING`` to partial functions, users who wish to continue
using them will either need to put up with warnings that they consider
unhelpful, suppress potentially-helpful warnings, or define their own
non-standard versions of the functions.  This makes the change more
controversial than it should be, as it potentially inconveniences existing
users.

Instead, this ghc-proposal extends ``WARNING`` pragmas so they can be annotated
with a "warning category".  Users can then enable or disable these
categories individually.  For example, it would allow the core-libraries
proposal to introduce::

    {-# WARNING in "x-partial" head "This is a partial function, it throws an error on empty lists." #-}
    {-# WARNING in "x-partial" tail "This is a partial function, it throws an error on empty lists." #-}

Such warnings are enabled by default, so the appropriate warning would be
displayed at any use of ``head`` or ``tail``.  However, users may control this
behaviour to their liking, prefixing the warning category name with an ``x-`` to
distinguish it from the built-in warning flags:

* Users who prefer to use the functions unimpeded may specify
  ``-Wno-x-partial`` to suppress all warnings annotated with the
  ``partial`` warning category, while still seeing other warnings.

* Users who do not wish to use the functions at all may specify
  ``-Werror=x-partial`` so that any use of the functions will be reported
  as an error, while other warnings are unaffected.


Proposed Change Specification
-----------------------------

A warning *category* is a string consisting of valid identifier characters or dashes.

#. A `WARNING pragma
   <https://downloads.haskell.org/ghc/9.4.1/docs/users_guide/exts/pragmas.html#warning-deprecated-pragma>`_
   may be immediately followed by the ``in`` keyword and a single warning category in double quotes.
   See below for details of the grammatical changes.

#. Individual warning categories may be enabled or disabled using new
   ``-W<category>`` or ``-Wno-<category>`` options, and their priority may be
   controlled using the ``-Werror=<category>`` or ``-Wwarn=<category>`` options.

#. A ``WARNING`` pragma without a category, or a ``DEPRECATED`` pragma, is
   interpreted as if it was a ``WARNING`` pragma with the single category
   ``deprecations`` specified.

#. The existing ``-Wwarnings-deprecations`` warning flag is interpreted as a
   synonym for ``-Wdeprecations`` (and similarly for
   ``-Wno-warnings-deprecations``, ``-Werror=warnings-deprecations`` and so on).

#. A new warning flag ``-Wextended-warnings`` switches on warnings from
   ``WARNING`` pragmas regardless of category; ``-Wno-extended-warnings``
   switches them all off.

#. A category is *recognised* if it either starts with ``x-`` or is
   ``deprecations``.  GHC will emit an error if a ``WARNING`` pragma uses a
   category that is not recognised.

There is no change to the existing rules for when ``WARNING`` pragmas give rise
to warnings; these changes merely affect whether the warnings are displayed.
The command-line flags are processed from left to right, with later
flags overriding previous ones, as at present.

Points 3 and 4 make the proposal backwards compatible: existing ``WARNING`` or
``DEPRECATED`` pragmas will still be controlled via the ``-Wdeprecations`` or
``-Wwarnings-deprecations`` options.  However if a library introduces a category
for a previously uncategorised warning, the warning will no longer be suppressed
by ``-Wno-deprecations``.  (Prior to this proposal, all ``WARNING`` and
``DEPRECATED`` pragmas were treated uniformly and could be disabled either by
``-Wno-deprecations`` or by ``-Wno-warnings-deprecations``.)

Point 5 is not strictly necessary, but ``-Wno-extended-warnings`` will allow
users to suppress all ``WARNING`` messages much like
``-Wno-warnings-deprecations`` does at present.  The naming is chosen to allow
for other sources of "extended warnings" in the future.

Point 6 makes sure that the command line options ``-Wx-partial`` and ``-Wno-x-partial``
can readily be distinguished from the existing large number of built-in warning
categories, such as ``-Wtabs``, ``-Wdodgy-import``, ``-Winaccessible-code`` etc. (See the
`user manual section <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-warnings.html?highlight=warning#warnings-and-sanity-checking>`_ for a complete list).
This way GHC can still
report unrecognised warning flags, rather than silently accepting them.


Grammar of warning declarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The grammar of declarations is extended as follows:

============  =  =====================================================================
*decl*        →  ``{-# WARNING`` [``in`` ``"`` *category* ``"``] [*things*] *strings* ``#-}``

*category*    →  { *small* | *large* | *digit* | ``'`` | ``-`` }

*things*      →  *thing1*, ..., *thingN*
*thing*       →  *varid* | *conid*
*strings*     →  *string* | ``[`` *string1*, ..., *stringN* ``]``
============  =  =====================================================================

The category can be omitted entirely, so this subsumes the existing
syntax for ``WARNING`` pragmas. If present,
the ``in`` keyword is followed by a single category as a double-quoted string.
Similarly the list of *things* is optional as it may be omitted (for a ``WARNING`` on a module header).

The *strings* may be a single string or a list (with the latter giving a
multi-line warning message).  Since a module header may have a pragma with no
*things*, e.g. ``{-# WARNING "message" #-}`` or ``{-# WARNING ["message1", "message2"] #-}``,
we use the ``in`` keyword to indicate the presence of a warning category.

The *category* non-terminal subsumes both *varid* and *conid*, so it is
possible to use the name of the thing to which a warning is being attached as
the category, provided it is not an operator.  The dash character (``-``) is permitted as a character
in addition to identifier characters, since dashes are frequently used in
warning names.

(The original version of this proposal used square brackets surrounding the
warning category, and did not put the category in quotes, but then it was not
obvious whether ``{-# WARNING [`` should be followed by a category or a string,
and it was difficult to lex category names containing dashes.)


Examples
--------

Suppose the definitions of ``head`` and ``nub`` are annotated with::

    {-# WARNING in "x-partial" head "This is a partial function, it throws an error on empty lists." #-}
    {-# WARNING in "x-quadratic" nub "The nub function has quadratic run-time complexity. If possible, use nubBy or nubOn." #-}

and the user program contains occurrences of both ``head`` and ``nub``::

    module M where
      foo = head
      bar = nub

This will result in the following warnings::

    M.hs:2:7: warning: [-Wx-partial]
        In the use of ‘head’ (imported from Prelude):
        "This is a partial function, it throws an error on empty lists."
      |
    2 | foo = head
      |       ^^^^

    M.hs:3:7: warning: [-Wx-quadratic]
        In the use of ‘nub’ (imported from Prelude):
        "The nub function has quadratic run-time complexity. If possible, use nubBy or nubOn."
      |
    3 | bar = nub
      |       ^^^

Notice that the message lists the warning category that applies.  In current
versions of GHC, this displays ``-Wdeprecations``.

The following examples show the effect of various combinations of warning
flags:

===============================  ===============================================
Warning flags                    Result
===============================  ===============================================
None                             Warnings displayed by default
``-Wno-x-partial``               Warning for ``nub`` but not ``head``
``-Wno-extended-warnings``       No warnings
``-Wno-warnings-deprecations``   Warnings displayed (category is not ``deprecations``)
===============================  ===============================================

Warning severity levels may be overridden by subsequent arguments on the
command-line.  For example, ``-Wno-extended-warnings -Werror=x-partial``
will result in errors instead of warnings with the category ``partial``,
but no other warnings from ``WARNING`` or ``DEPRECATED`` pragmas.  On the other
hand, ``-Werror=x-partial -Wno-extended-warnings`` will result in no
warnings because the second option overrides the first.


Effect and Interactions
-----------------------

This proposal should help resolve the controversy over whether ``head`` and
``tail`` should be annotated with ``WARNING`` pragmas.  By annotating them with
categorised warnings, users will be warned about their use by default, but may
choose to override the warnings as they wish.

This approach also provides an alternative to `proposal #528
<https://github.com/ghc-proposals/ghc-proposals/pull/528>`_, which is about
discouraging users from importing from "internal" modules, without completely
prohibiting their import.  For example, a ``WARNING in "x-ghc-prim-internals"``
pragma could be attached to all modules in ``ghc-prim``.  Users would then be
advised that such imports are discouraged, but could silence the warning with
``-Wno-x-ghc-prim-internals``.

These pragmas may be useful for libraries outside ``base`` as well, in
particular where library authors wish to selectively discourage use of certain
parts of their API.



Costs and Drawbacks
-------------------

This is yet one more feature to implement, although the implementation cost
should be fairly modest.

Overall this should make the language more accessible to newcomers, as library
authors will be able to use ``WARNING`` pragmas to discourage certain features
even if those warnings can be reasonably be disabled in some contexts.

This proposal does not provide a way to disable warnings at specific use sites,
only at the module level.  In some cases, it would be nice to be able to mark
individual uses as having been approved and the warning suppressed for that use
alone, rather than for all uses in the module.

It might be helpful to establish conventions around which categories exist, such
as ``x-partial`` for warnings about partial functions.  These issues are currently
left to individual library authors.

This proposal does not provide a mechanism for organising or namespacing warning
categories, as they are simply bare identifiers.  Thus if libraries use the
names of their functions as categories, the names cannot be qualified to
distinguish definitions from separate modules.


Alternatives
------------

Custom type warnings
~~~~~~~~~~~~~~~~~~~~

This proposal may be contrasted with `proposal #454
<https://github.com/ghc-proposals/ghc-proposals/pull/454>`_, which introduces a
built-in constraint ``Warning`` that can be used for custom warnings along
similar lines to the existing support for custom type errors.  That proposal
allows categorisation of warnings in a similar way, and moreover allows
type-level programming to control the presence and content of warnings, and the
suppression of warnings at individual use sites.

However, this proposal is simpler, and by keeping the ``WARNING`` annotations as
separate pragmas rather than requiring them to be part of the types, avoids the
risk that introducing ``Warning`` constraints may have unexpected effects on
program semantics.  Assuming this proposal is accepted, it would be fairly
simple to change the ``Warning`` class to be controlled using the same flags.


Multiple categories
~~~~~~~~~~~~~~~~~~~

The current version of this proposal (like `proposal #454
<https://github.com/ghc-proposals/ghc-proposals/pull/454>`_) does not allow
multiple categories to be attached to a single warning.  It could be useful to
include this feature, because it allows for multiple categories at different
levels of granularity (potentially including a different category for every
identifier).

Support for multiple categories it is omitted for now in the interests of
simplicity. It can lead to confusing effects, e.g. if a warning on ``head`` was
given both the ``head`` and ``partial`` categories, a user might specify
``-Werror=head -Wno-partial``.

It would be easy to change this later and allow a comma-separated list of
warning categories.


Control over severity
~~~~~~~~~~~~~~~~~~~~~

Under this proposal, all warnings from ``WARNING`` pragmas are treated as
belonging to ``-Wdefault``.  One might imagine libraries wanting to customise
this, e.g. showing them only with ``-Wall`` or ``-Wcompat``, or treating them as
errors with ``-Werror``.  This introduces more complexity, however.  Ideally,
severity should be a property of the entire category, but there is no up-front
definition of categories.

A plausible alternative would be to indicate that
classification in the prefix (``xw-``, ``xe-``, ``xi-``), so that categories
starting with `xe-` are errors by default. This does not
currently seem worth the additional complexity.


Glob patterns
~~~~~~~~~~~~~

Users may wish to disable multiple related warning categories in one go. One way
to achieve this would be to support glob-style command-line flags such as
``-Wno-x-partial*``, which would disable all of the warning categories
``x-partial``, ``x-partial-foo``, ``x-partial-bar`` and so on.

However, glob support is not part of the current proposal, in the interests of
simplicity. If in the future categorised warnings become sufficiently widely
used that glob support becomes necessary, this question can be revisited.


Unresolved Questions
--------------------

None.


Implementation Plan
-------------------

Support with the implementation of this proposal would be welcome.
