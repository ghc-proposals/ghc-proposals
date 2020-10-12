Syntax for Modifiers: a generalization of linear-types syntax
==============

.. author:: Your name
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. contents::

This proposal introduces a new form of syntax ``%blah`` that defines a *modifier*.
Modifiers somehow change the meaning of the next token. The ``blah`` must have
a type which is a member of the new class ``Modifier``. For now, all modifiers
will be built in, but we might imagine making an extensible feature later.

As of the writing of this proposal, there will be precisely one modifier: the
``%1`` or ``%Many`` of linear types. However, `#242`_ proposes *matchability*
essentially as a postfix modifier; under this proposal, the syntax for
matchability and multiplicity would be unified.

The main goal of this proposal is to set out a future direction
for the syntax of modifiers.

Motivation
----------

The latest form of linear types allows us to write ``Int %1 -> Bool`` to write
a function that must consume its input ``Int`` precisely once. The fact that
the function is linear is indicated by the ``%1`` before the ``->``.

A separate proposal `#242`_ describes how the matchability of a function will
be written postfix, with a ``@``. (The precise meaning of "matchability" is
irrelevant here.) So, if someone wanted to specify both the multiplicity and
the matchability of a function, they would write e.g. ``Int %Many -> @Unmatchable Bool``.
The problme is that we can easily imagine yet more decorations one would want
to put on an arrow. With multiplicity coming before and matchability afterwards,
there is now no more room for other modifiers (such as injectivity, or visibility,
or higher-order role, or others we haven't imagined yet).

In addition, GHC *already* supports some modifier-like constructs, but uses
a different syntax. Specifically, the term-level operators ``inline`` and
``oneShot`` are given types like the identity function, but they are really
modifiers on the variable that comes afterwards. We might imagine making
their special status more explicit by writing them ``%inline`` or ``%oneShot``.

Proposed Change Specification
-----------------------------

* Introduce a new extension ``-XModifiers``.

* With ``-XModifiers``, introduce modifier syntax on arrows as follows (cf.
  the Haskell 2010 Report for all BNF syntax, and recall that we use braces
  to denote "zero or more")::

    type     ::= btype [ {modifier} -> type ]
    prefix%  ::= '%'    -- only in prefix position
    modifier ::= prefix% atype

* With ``-XModifiers``, introduce modifier syntax on types as follows::

    btype    ::= {modifier} atype | btype atype

* With ``-XModifiers``, introduce modifier syntax on the term level as follows::

    fexp     ::= {modifier} aexp | fexp aexp

* Reserve the use of ``%`` in a prefix occurrence to be used only for modifiers;
  though this proposal does not do so, we can imagine extending the modifier syntax
  to apply to further syntactic situations (e.g. term-level operators, declarations,
  import lists, etc.). The one exception is the syntax ``%1`` for a linear function,
  which continues to be allowed.

* The use of a modifier on anything but a type-level arrow is an error.

* Introduce a new type-level constant ``Modifier :: Type -> Constraint``, exported
  from ``GHC.Exts``.

* Let the constraint ``Modifier Multiplicity`` be satisfiable; let no other
  ``Modifier`` constraint be satisfiable.

* During constraint generation, let an occurrence ``%(ty)``, where ``ty :: ki``,
  emit a constraint ``Modifier ki``.

* A modifier of type ``Multiplicity`` changes the multiplicity of the following arrow.
  Multiple modifiers of type ``Multiplicity`` on the same arrow are not allowed.

* ``-XLinearTypes`` implies ``-XModifiers``.
  
Examples
--------
Here are some examples that will be accepted or rejected with this proposal::

  f1 :: Int %1 -> Bool    -- unaffected, actually: that "%1" is one lexeme, and
                          -- is not a modifier. See more on this below.
  f2 :: Int %Many -> Bool -- accepted: Many :: Multiplicity, and Modifier Multiplicity holds
  f3 :: Int %m -> Bool    -- rejected: the kind of m is ambiguous
  f4 :: Int %(m :: Multiplicity) -> Bool   -- accepted
  f5 :: Int %One %Many -> Bool   -- rejected (although it will parse)
  f6 :: Int %Many %Many -> Bool  -- rejected

Effect and Interactions
-----------------------
* It is expected that the matchability of `#242`_ will have a kind ``Matchability``,
  and that ``Modifier Matchability`` will be satisfiable. Then, users will be able
  to write ``Int %Many %Matchable -> Bool`` or ``Int %Matchable %Many -> Bool``.
  The details are left to `#242`_ (assuming this proposal is accepted first).
  The author of `#242`_, Csongor Kiss, was involved in the conceptualization of
  this proposal.

* Future modifiers will also seamlessly work with existing ones, where order
  is not expected to matter (though that would be up to other proposals to
  spell out).

* Let's assume we have overloaded numbers at the type level, and then consider
  ``%1``. Under this proposal, we would have ``1 :: a`` where ``Num a`` and
  ``Modifier a`` must hold. If we have ``Multiplicity`` specified at the end of
  the ambient ``default``\ing list, then ``Multiplicity`` will be the first
  (and only) member of that list that satisfies both ``Num`` and ``Modifier``.
  Accordingly, GHC will default ``a`` to be ``Multiplicity``, and all will be
  well. (We may want this case to avoid activating ``-Wtype-defaults``, but
  that's a conversation for later.)

* This proposal means that ``Int %m -> Bool``, acceptable today as a
  multiplicity-polymorphic function, would be rejected. The user would need
  to add a kind annotation to tell us that ``m`` is a multiplicity (and not,
  say, a matchability, which is also expected to support polymorphism). See
  an Alternative below for a trick to mitigate this problem.

* This proposal paves the way for future proposals introducing new modifiers.
  Possible candidates:

  * matchability
  * injectivity on arrows
  * ``oneShot``
  * ``inline``
  * a replacement for the ``{-# OVERLAPPING #-}`` pragmas. These pragmas
    have, in my opinion, never really fit in: they change the semantics
    of the declaration. Pragmas are meant to be hints or instructions
    to the compiler, not something that changes the meaning of a program
    and its typing rules.
  * a mechanism for suppressing warnings over one region of a program,
    instead of at the module level: ``%(suppress "uni-complete-patterns") (case x of ...)``.
    This could also be done with a pragma.

* Some other features that have had tortuous and torturous syntax
  discussions may have enjoyed having the modifier option. For example,
  this might have been used instead of ``type role`` for role annotations:
  ``data Tagged (%nominal t) a = Tagged a``. Or it might have been an
  alternative for ``-XDerivingStrategies``.

* Though not proposed here, we can imagine extensions allowing abstractions
  over ``Modifiers``. This might allow being able to solve ``Modifier (a,b)``
  when ``Modifier a`` and ``Modifier b`` holds, thus allowing something
  like ``type ManyMatch = '(Many, Matchable); foo :: Int %ManyMatch -> Bool``.

* Though not proposed here, we can imagine a large extension to this
  mechanism allowing for *user-written* ``Modifier``\s. Perhaps a
  ``Modifier`` type supports some function call to the GHC API that
  transforms the meaning of bit of syntax. The possibilities are
  tantalizing.
  
* These modifiers recall Java's `Annotations <https://en.wikipedia.org/wiki/Java_annotation>`_
  mechanism, which were a direct inspiration.
  
Costs and Drawbacks
-------------------
* The loss of the inferred kind of ``m`` in multiplicity polymorphism is a
  drawback. However, a user seeing ``Int %m -> Bool`` is hard-pressed to
  understand what is going on. On the other hand, ``Int %(m :: Multiplicity) -> Bool``
  is much more perspicuous.

* Any feature has a maintenance burden, but this one should be fairly small.
  In particular, the ``Modifier`` scheme dovetails perfectly with the existing
  class-based overloading machinery within GHC.

* Having yet another special symbol in a special position is a drawback.
  Yet ``%`` is *already* such a symbol (due to ``-XLinearTypes``), and the
  existence of an extensible modifiers mechanism makes it possible to
  avoid adding new symbols to this set.

Alternatives
------------
* If we label ``Modifier`` an "interactive class", we can use
  ``-XExtendedDefaultRules`` to allow GHC to default the type of ``m``
  in ``Int %m -> Bool`` to be ``Multiplicity``. See `the documentation <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/ghci.html#type-defaulting-in-ghci>`_ for
  more details. This will work well, but I actually prefer not doing this,
  and being explicit about multiplicity polymorphism.

* There does not seem to be much point in introducing modifier
  syntax beyond the ``->`` syntax, but it seemed helpful to do so here.
  We can drop that.

Unresolved Questions
--------------------
* Is it too soon? That is, this proposal solves a problem we do not yet have:
  the combination of multiplicity and matchability. Yet, it seems much easier
  to consider this idea separate from the quite considerable complexity of `#242`_,
  and so I have made it a separate proposal.

