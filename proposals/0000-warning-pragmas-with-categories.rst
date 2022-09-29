``WARNING`` pragmas with categories
===================================

.. author:: Adam Gundry
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/541>`_.
.. sectnum::
.. contents::

This proposal extends the syntax of ``WARNING`` pragmas to allow them to specify
one or more "warning categories".  Users can then enable or disable specific
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
with one or more "warning categories".  Users can then enable or disable these
categories individually.  For example, it would allow the core-libraries
proposal to introduce::

    {-# WARNING [head,partial] head "This is a partial function, it throws an error on empty lists." #-}
    {-# WARNING [tail,partial] tail "This is a partial function, it throws an error on empty lists." #-}

By default, ``-Wwarnings-deprecations`` is enabled, so the appropriate warning
would be displayed at any use of ``head`` or ``tail``.  However, users may
control this behaviour to their liking:

* Users who prefer to use the functions unimpeded may specify
  ``-Wno-warnings:partial`` to suppress all warnings annotated with the
  ``partial`` warning category, while still seeing other warnings.

* Users who do not wish to use the functions at all may specify
  ``-Werror=warnings:partial`` so that any use of the functions will be reported
  as an error, while other warnings are unaffected.

* Since multiple warning categories can be applied to an identifier, users may
  distinguish more finely if needed, for example ``-Werror -Wwarn=warnings:head
  -Wno-warnings:tail`` could be used in a codebase where most warnings are
  considered errors, except uses of ``head`` are merely warned about, and uses
  of ``tail`` are not mentioned.


Proposed Change Specification
-----------------------------

The `WARNING and DEPRECATED pragmas
<https://downloads.haskell.org/ghc/9.4.1/docs/users_guide/exts/pragmas.html#warning-deprecated-pragma>`_
may be immediately followed by a comma-separated list of warning categories in
square brackets. Each warning category is an identifier beginning with a letter
and followed by valid identifier characters or dashes.

More formally, the grammar of declarations is extended as follows:

============  =  =====================================================================
*decl*        →  ``{-# WARNING`` [*categories*] *things* *strings* ``#-}``
*decl*        →  ``{-# DEPRECATED`` [*categories*] *things* *strings* ``#-}``

*categories*  →  ``[`` *category1*, ..., *categoryN* ``]``
*category*    →  (*small* | *large*) { *small* | *large* | *digit* | ``'`` | ``-`` }

*things*      →  *thing1*, ..., *thingN*
*thing*       →  *varid* | *conid*
*strings*     →  *string* | ``[`` *string1*, ..., *stringN* ``]``
============  =  =====================================================================

The list of categories can be omitted entirely, so this subsumes the existing
syntax for ``WARNING`` and ``DEPRECATED`` pragmas.  That is, [*categories*]
means that the presence of the *categories* non-terminal is optional; if present
it is a list in square brackets (hence the use of typewriter font ``[``
... ``]`` in the definition of the *categories* production).

The *category* non-terminal subsumes both *varid* and *conid*, so it is always
possible to use the name of the thing to which a warning is being attached as
one of the categories.  The dash character (``-``) is permitted as a character
in addition to identifier characters, since dashes are frequently used in
warning names.

There is no change to the existing rules for when ``WARNING`` or ``DEPRECATED``
pragmas give rise to warnings, except that individual warning categories may be
enabled or disabled using new ``-Wwarnings:<category>`` or
``-Wno-warnings:<category>`` options, and their priority may be controlled using
the ``-Werror=warnings:<category>`` or ``-Wwarn=warnings:<category>`` options.
Here ``<category>`` represents the name of a *category* according to the grammar
above.  The ``-Wwarnings-deprecations`` warning flag (and its synonym
``-Wdeprecations``) controls the display of all warnings from ``WARNING`` or
``DEPRECATED`` pragmas, regardless of category.  The command-line flags are
processed from left to right, with later flags overriding previous ones, as at
present.


Examples
--------

Suppose the definitions of ``head`` and ``tail`` are annotated with::

    {-# WARNING [head,partial] head "This is a partial function, it throws an error on empty lists." #-}
    {-# WARNING [tail,partial] tail "This is a partial function, it throws an error on empty lists." #-}

and the user program contains occurrences of both ``head`` and ``tail``::

    module M where
      foo = head
      bar = tail

This will result in the following warnings::

    M.hs:2:7: warning: [-Wwarnings:head, -Wwarnings:partial]
        In the use of ‘head’ (imported from Prelude):
        "This is a partial function, it throws an error on empty lists."
      |
    2 | foo = head
      |       ^^^^

    M.hs:3:7: warning: [-Wwarnings:tail, -Wwarnings:partial]
        In the use of ‘tail’ (imported from Prelude):
        "This is a partial function, it throws an error on empty lists."
      |
    3 | bar = tail
      |       ^^^^

Notice that the message lists all the warning categories that apply.  In current
version of GHC, this displays ``-Wdeprecations`` alone.

The following examples show the effect of various combinations of warning
flags:

===============================  ===============================================
Warning flags                    Result
===============================  ===============================================
None                             Warnings displayed
``-Wno-warnings-deprecations``   No warnings
``-Wno-warnings:partial``        No warnings
``-Wno-warnings:tail``           Warning displayed for ``head`` but not ``tail``
===============================  ===============================================

Warning severity levels may be overridden by subsequent arguments on the
command-line.  For example, ``-Wno-warnings-deprecations
-Wwarnings:head -Werror=warnings:tail`` will result in warnings about ``head``
and errors about ``tail``, but no other ``-Wwarnings-deprecations`` messages.
On the other hand, ``-Werror=warnings:partial -Wno-warnings-deprecations`` will
result in no warnings because the second option overrides the first.


Effect and Interactions
-----------------------

This proposal should help resolve the controversy over whether ``head`` and
``tail`` should be annotated with ``WARNING`` pragmas.  By annotating them with
categorised warnings, users will be warned about their use by default, but may
choose to override the warnings as they wish.

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
as ``partial`` for warnings about partial functions.  Moreover it is unclear
whether every identifier with an attached warning should have a category
corresponding to the name of the identifier, as with the ``-Wwarnings:head`` and
``-Wwarnings:tail`` examples.  These issues are currently left to individual
library authors.

This proposal does not provide a mechanism for organising or namespacing warning
categories, as they are simply bare identifiers.  Thus if libraries use the
names of their functions as categories, the names cannot be qualified to
distinguish definitions from separate modules.


Alternatives
------------

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

`Proposal #454 <https://github.com/ghc-proposals/ghc-proposals/pull/454>`_ does
not allow multiple categories to be attached to a single warning, but it seems
useful to include this feature, because it allows for multiple categories at
different levels of granularity (potentially including a different category for
every identifier).


Unresolved Questions
--------------------

GHC currently supports both ``WARNING`` and ``DEPRECATED`` pragmas, which are
synonymous.  Similarly the flags ``-Wwarnings-deprecations`` and
``-Wdeprecations`` are synonymous, and there is no ``-Wwarnings`` flag.  This
seems somewhat inconsistent.  Should we reduce the redundancy by considering
deprecations one particular category of warnings?

Could the use of a colon in the command-line flag name cause issues on some
platforms?  Is there another syntax that would be preferable?


Implementation Plan
-------------------
Support with the implementation of this proposal would be welcome.
