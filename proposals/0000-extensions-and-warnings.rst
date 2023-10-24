Integrate language extensions and warnings
===========================================

.. author:: Richard Eisenberg and Simon Peyton Jones
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header::
.. sectnum::
.. contents::



This proposal unifies language extensions and warnings into one mechanism, thus simplifying GHC's user interface while offering more expressiveness, including allowing the language edition to specify a default set of warnings.

Motivation
-------------
This proposal discusses both language extensions and warning flags. These features may seem distinct, but in fact (see this discussion):
many extensions could have been implemented as warnings instead (such as -XMultiParamTypeClasses, -XDeriveFunctor or -XAllowAmbiguousTypes)
many warnings could have been implemented as extensions (such as -Wname-shadowing)
It is something of a historical accident that one thing has ended up in "warnings" while another has ended up in "language extensions".  However although language extensions and warning flags overlap in functionality, they also have some distinctively different functionality:
Language extensions can change the behaviour of a program; warnings cannot.
Warnings can warn.  Language extensions cannot: they can only error.

This proposal seeks to unify the features of language extensions and warnings, and then present a simplified user interface over the unified feature.

An important direction of travel is that we want a **language edition**, such as `-XGHC20201`, to fix a particular language specification.
In particular, our aspirational goal is that if a program compiles with `ghc-9.8 -XGHC2021` then it should also compile with `ghc-9.10 -XGHC2021`.

*"Good" and "bad"*.  In general, language extensions often enable a good new behaviour, while disabling a warning enables a bad new behaviour. But "good" and "bad" are clearly subjective, and we've already gotten this wrong a few times. (For example, ``-XDatatypeContexts`` allows a bad behaviour and ``-Wno-unticked-promoted-constructor`` allows a good one, at least in Richard's opinion.)  So we propose the following principle:

  **Principle of neutrality.**  GHC itself should not have an opinion about "good" and "bad", for example by categorising one as a language extension and the other as a warning flag; rather language editions should express that choice.

Not in scope: There are flags that control GHC's language that we are not (yet) including, such as ``-fdefer-type-errors`` and ``-fpedantic-bottoms`` that control GHC's behaviour. While they fit within the overall framework here, there is no great need to consider them now and will only serve to complicate the debate.



Proposed Change Specification
-----------------------------

1. **Extensions can warn**. For any given language extension, say GADTs:

   * ``-XGADTs`` allows ``GADTs``
   * ``-XNoGADTs`` errors on a use of ``GADTs``
   * ``-XWarnGADTs`` warns on a use of ``GADTs``


2. **Warnings are just extensions**. Almost all current warnings, such as -Wname-shadowing, become a language extension -XWarnNameShadowing, with the obvious algorithmic name conversion.

   Back-compat: all existing warning-flag syntax remains (perhaps indefinitely); but almost all are re-interpreted as a synonym for language extension flags.   For example ``-Wname-shadowing`` is a synonym for ``-XWarnNameShadowing``.  ("Almost all" because a few warnings are extra-linguistic, such as ``-Winconsistent-flags``.)

3. **Implications**.  A language extension may imply others.  This is true today; for example ``-XTypeFamilyDependencies`` implies ``-XTypeFamilies``.    The warning form has a similar dependency: -``XWarnTypeFamilyDependencies`` implies ``-XWarnTypeFamilies``.

4. **Conservative and non-conservative extensions**.   A conservative extension adds a feature to the language, without affecting the meaning of any existing program; a non-conservative extension changes the meaning of a program.

5. **Non-warnable extensions**.  Some language extensions are non-warnable, so you are not allowed to say ``-XWarnAlternativeLayoutRule`` for example.

   The vast majority of extensions are warnable; in particular, all conservative extensions are warnable.  Most non-conservative extensions could usefully be made warnable, although it might take extra work to do so.  Examples:

   * ``-XWarnMonomorphismRestriction``: we already have a warning when this "bites", and it did indeed take extra work.
   *  ``-XWarnRebindableSyntax``: this would be new, but we would warn on every use of a rebindable construct that does not refer to the appropriate name from base.
   * ``-XWarnDeepSubsumption``: would warn when deep subsumption was actually used, and simple subsumption would not have sufficed.

6. **Non-negatable extensions**. Some language extensions are non-negatable; for example, you cannot say ``-XNoSafe``.  (This is the case today, because someone might want to ensure that all files are compiled Safely, and an individual module should not be able to opt out.)

7. **Incompatible extensions**.  Two language extensions can be mutually incompatible.  For example ``-XSafe`` and ``-XUnsafe``.   It is an error to specify both at "warn" level or above.

8. **Language editions**.  A language edition, like ``-XGHC2024``, simply implies a bunch of other extensions, just as today.  Each language edition is incompatible with other language editions, so you can specify at most one language edition.

   Any particular version of GHC comes with its own "default language edition". For example, GHC 9.8 has default language edition GHC2021.   What that means is that the language extensions implied by GHC2021 are switched on; but GHC2021 itself is not, so that the user can say ``ghc -XGHC2024`` without an incompatible-extension warning.

Extensions are processed in order, as today.  (Richard has a separate proposal in preparation, to make extensions order-independent.)

The meaning of `-W` and `-Wall` would continue to be "enable all recommended warnings" and "enable all reasonable warnings", just as in GHC today.
These lists may therefore vary with GHC version; so a later GHC version may warn about things that an earlier GHC version does not.

Effect and Interactions
-----------------------

This design has the following happy consequences.

* The tension between warnings and language extensions disappears.  For example, at the top of a module we can write::

	{-# LANGUAGE GADTs, NoIncompletePatterns #-}

  rather than::

	{-# LANGUAGE GADTs #-}
	{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

* A language edition fixes a set of warnings, unlike the situation today.  For example, ``-XGHC2024`` could include warnings about incomplete patterns.


* A language edition could choose to error on what is today a warning, such as ``-XNoMissingMethods``.   (Today you can say ``-Werror=missing-methods``, but you can't do that in a language edition.)   An opt-in change of this nature is the purpose of GHC Proposal 571


* A language edition could choose to allow, but warn, about a language extension, e.g ``-XDeriveFunctor``.  That is not possible today.

* We could add a non-warnable non-negatable language extension ``-XStable`` that is defined to be incompatible with all Experimental extensions, but otherwise does nothing at all.   Thus, adding -XStable will ensure that no experimental extensions can be used, which is (close to) the goal of GHC Proposal 617. 

* A language edition could, if we wanted, choose to be incompatible with some experimental extension (e.g. ``-XLinearTypes``), or even with all experimental extensions (via ``-XStable``).

* ``-Wcompat`` turns on warnings that will be enabled by default in the future, but remain off in normal compilations for the time being.  It can continue to do so.  But under this proposal, warnings "enabled by default in the future" will simply be part of the default language edition. 

Unresolved questions
----------------------

* How does this play with the new user defined warning categories?

* A new language extension for each warning, and a new warning for each language extension.  Two long lists (extensions and warnings) combined into one even longer list.  Could feel intimidating.

* Will we end up supporting something for longer?   Eg ``-Wmonad-fail``.  It lived only for a few releases, it warned you if you didn't write your code in a forward compatible way.
  * Policy idea: Support the past three language editions, but drop support for earlier ones.

* Currently dropping warnings is seen as no-fuss-required; but if warnings were language extensions, we'd need to treat them much more carefully.






Proposed Change Specification
-----------------------------

A **partial field** is a field that does not belong to every constructor of the
corresponding datatype.

A **partial selector occurrence** is a use of a record selector for a partial
field, either as a selector function in an expression, or as the solution to a
``HasField`` constraint.

The new warning flag ``-Wincomplete-record-selectors`` will emit a warning for
each partial selector occurrence for which the pattern match checker cannot
statically determine that the selector is applied to a constructor that
includes the field.

The exact capabilities of the pattern match checker are out of scope for this
proposal.  It is acceptable if all partial selector occurrences emit a warning,
but the implementation may choose to suppress the warning in particular cases
where it can determine that any argument to the partial selector will contain
the field.

This warning is implied by ``-Wall``, just like ``-Wincomplete-record-updates``
following `proposal #71 <https://github.com/ghc-proposals/ghc-proposals/pull/71>`_.


Examples
--------

Recall the datatype from the `Motivation`_::

  data T = T1 { x :: Int, y :: Bool }
         | T2 { y :: Bool }

Here ``x`` is a partial field and ``y`` is a total field.

When ``-Wincomplete-record-selectors`` is enabled:

1. An occurrence of ``x`` as a selector (in an expression) causes a warning. It
   is irrelevant whether or not it is applied. Thus ``f1 r = x r`` and ``g1 =
   x`` both warn, but ``h1 r = y r1`` does not.

2. A constraint ``HasField "x" T Int`` being solved automatically causes a
   warning.

   - In particular this arises with ``f2 = getField @"x" @T``, but also with
     ``OverloadedRecordDot`` in cases such as ``g2 (r :: T) = r.x``.

   - On the other hand ``h2 r = getField @"x" r`` and ``k2 r = r.x`` do not warn
     because their types are polymorphic in the record type, subject to a
     ``HasField`` constraint.

   - A later call to ``h2`` or ``k2`` at type ``T`` does trigger a warning,
     because this leads to the constraint ``HasField "x" T Int`` being solved.

3. Uses of the field ``x`` in record construction or pattern-matching do not
   lead to a warning, so these are fine::

    h3 = T1 { x = 3, y = True }

    k3 T1{x=x'} = x'
    k3 T2{} = 0


Long range information
~~~~~~~~~~~~~~~~~~~~~~

Expressions such as the following will obviously never cause a pattern match
failure at runtime, because ``x`` is applied to an argument that will
necessarily use the ``T1`` constructor::

    x (T1 { x = 0, y = True })

    case r of { T2 _ -> 0 ; _ -> x r }

    let t1 = T1 { x = 0, y = True } in t1.x

Thus the implementation may be able to suppress the warning, depending on the
capabilities of the pattern match coverage checker.


GADTs
~~~~~

Consider the following GADT::

    data G a where
      MkG1 :: { x :: Int    } -> G Bool
      MkG2 :: { y :: Double } -> G Char

Any use of ``x`` or ``getField @"x"`` applied to a term of type ``G a`` will
result in a warning.  However if the argument type is ``G Bool`` then the
warning may optionally be suppressed, for example, this definition need not emit
a warning::

    f :: G Bool -> Int
    f r = getField @"x" r


Effect and Interactions
-----------------------
The ``NoFieldSelectors`` extension allows users to suppress field selector
functions, thereby avoiding the risk of calling a partial selector function in
an expression.  This does not prevent use of ``OverloadedRecordDot`` for the field,
however, so the proposed warning is still useful.

This proposal assumes that ``HasField`` constraints always represent selectors,
not updates.  This is true in currently implemented GHC versions, but would no
longer be true if `proposal #158
<https://github.com/ghc-proposals/ghc-proposals/pull/158>`_ was to be
implemented as currently specified.  I intend to bring forward a separate
proposal to split updates into a separate class, thereby avoiding this issue
(see also `proposal #286
<https://github.com/ghc-proposals/ghc-proposals/pull/286>`_).

This proposal makes no changes to ``-Wpartial-fields``, so that users may choose
to receive warnings at definition sites or at use sites.  Both may be useful in
different contexts:

- a library author may wish to enable ``-Wpartial-fields`` to avoid ever
  defining a partial field in their library, since they have no guarantee that
  downstream users will enable the use-site warnings;

- an application author may be using an existing library that defines partial
  fields, but may wish to avoid using them by enabling
  ``-Wincomplete-record-selectors -Wincomplete-record-updates``.


Costs and Drawbacks
-------------------
The implementation cost of this warning should be low, as GHC can easily
determine which fields are partial, and record this information for later use.

Users who set ``-Wall -Werror`` may see build failures if they use partial
fields as selectors, but if this is not desired they can set
``-Wno-incomplete-record-selectors``.


Alternatives
------------
For ``HasField``, it would be possible to change its definition so that it would
not be solved at all for partial fields, or provide an alternative
implementation (either manually or automatically) returning a ``Maybe`` value.
This would avoid partiality when using ``OverloadedRecordDot``, without a need
for warnings.  It seems simplest to keep ``HasField`` consistent with existing
selector functions, however.

This does not make it possible for a library author to define a datatype with
partial fields such that their users *cannot* use partial operations.  Instead,
downstream modules will need to enable ``-Werror=incomplete-record-selectors``
in order to rule out such cases.  We could imagine somehow annotating datatypes
to impose restrictions such as preventing selection or update, but this is not
part of the current proposal.


Naming
~~~~~~

The new flag is named ``-Wincomplete-record-selectors`` for consistency with the
existing ``-Wincomplete-record-updates`` (and similarly-named warnings such as
``-Wincomplete-patterns``).  These all share the property of warning about code
that necessarily performs an incomplete pattern match.

The naming of ``-Wpartial-fields`` at first seems inconsistent with this, and we
might imagine changing it to something like ``-Wincomplete-record-definitions``.
However, it is somewhat different to the others, because it is possible to
define a partial field but use it only through total mechanisms (e.g. pattern
matching).  If we were to define a warning group ``-Wincomplete`` to collect
together incompleteness warnings (as suggested in discussion on `proposal 351
<https://github.com/ghc-proposals/ghc-proposals/pull/351>`_) it would make sense
to include ``-Wincomplete-record-selectors`` and ``-Wincomplete-record-updates``
but not ``-Wpartial-fields``.  Thus this proposal does not change the name of
``-Wpartial-fields``.


Unresolved Questions
--------------------
None.
