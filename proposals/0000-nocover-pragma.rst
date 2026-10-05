NOCOVER expression pragma
=========================

.. author:: Florian Ragwitz
.. date-accepted::
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/work_items/27870
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

When measuring test coverage, not all code can always be covered. Some code may
be intentionally unreachable, defensive, or otherwise impractical to exercise.
Such code creates expected gaps in coverage reports, making it more difficult
for developers to identify unintended gaps.

This proposal allows authors to annotate expressions with a ``NOCOVER`` pragma
to exclude them from coverage reporting.

Motivation
----------

Consider this code from GHC:

::

  parse_mod (Qual (ModuleName mod) _) = upd $ \d -> d { mainModuleNameIs = mkModuleNameFS mod }
  -- we appended ".m" and any parse error was caught. We are Qual or something went very wrong
  parse_mod _ = error "unreachable"

The author acknowledges that some code is unreachable, and explains the reason
for that to the reader. However, when generating a coverage report for that
code, the ``error`` expression will always be marked as uncovered despite that
being expected.

While that's technically true, it's also unhelpful for a developer studying the
coverage report in order to identify and fix real coverage issues, as those can
be easy to miss amongst cases like the above.

This proposal introduces a new expression pragma ``NOCOVER`` to explicitly
exclude cases like the above from coverage reporting. With that pragma, the
above code can instead be expressed like this:

::

  parse_mod (Qual (ModuleName mod) _) = upd $ \d -> d { mainModuleNameIs = mkModuleNameFS mod }
  parse_mod _ =
    {-# NOCOVER "we appended \".m\" and any parse error was caught. We are Qual or something went very wrong" #-}
    error "unreachable"

This preserves the information provided by the original comment for readers of
the source code, but also informs coverage reporting tools about the fact that
some uncovered code is expected, allowing it to generate more practically
useful reports.

Proposed Change Specification
-----------------------------

Locations which currently allow expression pragmas (such as ``SCC``) are
extended to also allow a new ``NOCOVER`` pragma of the following form:

::

  '{-# NOCOVER' STRING '#-}'

An expression annotated with that pragma, including all its sub-expressions,
will be excluded from HPC coverage information. This applies to expression,
alternative, and boolean boxes. If the expression is directly enclosed in
parentheses, such as in ``f = maybe ({-# NOCOVER "unreachable" #-} 0) (Just
1)``, that enclosing parenthetical expression is excluded from coverage as
well. All other enclosing expressions are treated normally for coverage
purposes, such as ``1 +`` in ``1 + {-# NOCOVER "..." #-} foo``.

The required ``STRING`` provides an opportunity for the author to describe the
reason why coverage should be disabled for the given expression. This
description is intentionally required to make uses of ``NOCOVER``
self-documenting for the benefit of future readers, and to gently discourage
use of the pragma to paper over legitimate coverage gaps.

Adding a ``NOCOVER`` annotation will never change the behaviour of a program
other than how coverage is recorded by HPC.

Similar to the ``SCC`` expression pragma, ``NOCOVER`` is not valid at arbitrary
points within infix expressions, such as ``1 / {-# NOCOVER "foo" #-} 2 / 2``,
because allowing expression pragmas in such positions can affect how the
expression is parsed. See `GHC Proposal #176
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0176-scc-parsing.rst>`_.
Parentheses can be used for disambiguation: ``1 / ({-# NOCOVER "foo" #-} 2) /
2``.

``NOCOVER`` is solely an expression pragma. A declaration-level version could
be useful for top-level bindings, class instances, and perhaps others, but is
out of scope for this proposal.

Proposed Library Change Specification
-------------------------------------

This proposal does not affect ``base`` or ``ghc-experimental``.

Examples
--------

::

  -- rejected: expression pragmas are not valid at this position
  f = 1 / {-# NOCOVER "foo" #-} 2 / 2

  -- rejected: no description
  f = {-# NOCOVER #-} 42

  -- rejected: non-expression
  {-# NOCOVER "foo" #-}
  f = 42

  -- no coverage reporting in if expression, including alternative and boolean coverage
  f x =
    1 +
    {-# NOCOVER "foo" #-}
      if x
        then 0
        else 1

  -- no coverage reporting for the error expression, but if expression still receives coverage
  f x =
    if p x
      then 42
      else {-# NOCOVER "p is always true" #-} error "unreachable"

  -- no coverage reporting for the RHS, but top-level box is still created
  f = {-# NOCOVER "foo" #-} 42

Effect and Interactions
-----------------------

``NOCOVER`` only affects HPC coverage information. Other mechanisms using
ticks, such as source notes and breakpoints, are unaffected.

Costs and Drawbacks
-------------------

The existing implementation for GHC is straightforward and should come at a
very small maintenance cost over time.

Programs directly working with the Haskell AST may need small updates to
handle the new pragma.

``NOCOVER`` can be used to conceal code which should be tested. Misuse can't be
prevented, but the required description attempts to discourage it.

Backward Compatibility
----------------------

This proposal has no effect on the behaviour of existing programs.

Programs using the new ``NOCOVER`` pragma will work without changes on older
GHC versions, but emit an ``Unrecognised pragma: NOCOVER`` warning under
``-Wunrecognised-pragmas``.

Alternatives
------------

HPC provides an overlay mechanism which can also be used to exclude certain
expressions from coverage reporting. Using overlays is more brittle than this
proposal, as it relies on specific source span locations. Overlays also don't
have the benefit of keeping coverage exclusions local to the code in question,
and are less convenient to use with existing tooling such as ``cabal test
--enable-coverage``.

Implementation Plan
-------------------

This proposal is implemented `here
<https://gitlab.haskell.org/ghc/ghc/-/merge_requests/16750>`_. Some additional
work is required to add test cases for the new feature.
