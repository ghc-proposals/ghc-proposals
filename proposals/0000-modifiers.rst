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
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/370>`_.
.. contents::

This proposal introduces a new form of syntax ``%blah`` that defines a *modifier*.
Modifiers somehow change the meaning of the next token. The ``blah`` is parsed
and renamed as a type. For now, all modifiers
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

1. Introduce a new extension ``-XModifiers``.

2. With ``-XModifiers``, introduce modifier syntax on arrows as follows (cf.
   the Haskell 2010 Report for all BNF syntax, and recall that we use braces
   to denote "zero or more")::

     type     ::= btype [ {modifier} -> type ]
     prefix%  ::= '%'    -- only in prefix position
     modifier ::= prefix% atype

3. With ``-XModifiers``, introduce modifier syntax on types as follows::

     btype    ::= {modifier} atype | btype atype

4. With ``-XModifiers``, introduce modifier syntax on the term level as follows::

     fexp     ::= {modifier} aexp | fexp aexp

5. With ``-XModifiers``, introduce postfix modifier syntax on patterns as follows::

     lpat     ::= apat {modifier}

6. With ``-XModifiers``, introduce postfix modifier syntax on record field declarations as follows::
     
     fielddecl ::= vars '::' (type | '!' atype) {modifier}
     
7. Reserve the use of ``%`` in a prefix occurrence to be used only for modifiers;
   though this proposal does not do so, we can imagine extending the modifier syntax
   to apply to further syntactic situations (e.g. term-level operators, declarations,
   import lists, etc.).

8. The type of a modifier is determined only by synthesis, never by checking.
   That is, in the bidirectional type-checking scheme used by GHC, we find the
   type of the modifier by running the synthesis judgment. Effectively, this
   means that if we consider a modifier to be some head (constructor or
   variable) applied to a sequence of arguments (possibly none), the head must
   have a known type: constructors always have a known type, and variables
   have a known type if declared with a type signature. Alternatively, the
   modifier may have a top-level type signature.

9. A modifier of type ``Multiplicity`` changes the multiplicity of the following arrow,
   preceding pattern-bound variable (but only on the top level of a lambda pattern),
   or preceding record field.
   Multiple modifiers of type ``Multiplicity`` on the same arrow are not allowed.
   Any other use of a modifier is an error.

10. ``-XLinearTypes`` implies ``-XModifiers``.
  
Examples
--------
Here are some examples that will be accepted or rejected with this proposal::

  f1 :: Int %1 -> Bool    -- unaffected, actually: that "%1" is one lexeme, and
                          -- is not a modifier. See more on this below.
  f2 :: Int %Many -> Bool -- accepted: Many :: Multiplicity
  f3 :: Int %m -> Bool    -- rejected: the kind of m is undeclared
  f4 :: Int %(m :: Multiplicity) -> Bool   -- accepted with a type signature
  f5 :: Int %One %Many -> Bool   -- rejected (although it will parse)
  f6 :: Int %Many %Many -> Bool  -- rejected
  f7 :: Int %(m :: Multiplicity) -> Int %m -> Int
    -- rejected: the second use of '%m' has an unknown type

  map :: forall (m :: Multiplicity). (a %m -> b) -> [a] %m -> [b]
    -- accepted: m has a known type

The syntax (and semantics) for modifiers on patterns and record fields is exactly
as described in the `linear types proposal`_.

.. _`linear types proposal`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst#syntax
  
Effect and Interactions
-----------------------
* It is expected that the matchability of `#242`_ will have a kind ``Matchability``.
  Then, users will be able
  to write ``Int %Many %Matchable -> Bool`` or ``Int %Matchable %Many -> Bool``.
  The details are left to `#242`_ (assuming this proposal is accepted first).
  The author of `#242`_, Csongor Kiss, was involved in the conceptualization of
  this proposal.

* Future modifiers will also seamlessly work with existing ones, where order
  is not expected to matter (though that would be up to other proposals to
  spell out).

* The ``%1`` will remain a single lexeme and does not participate with this
  proposal. We may want more exceptions to the general scheme in the future.

* The key action of this proposal is to carve out a new syntax space, anchored
  by a prefix occurrence of ``%``. Ideally, there would be few exceptions to
  the general scheme (but ``%1`` is one such exception). It is possible that
  future extensions to this idea will be disambiguated before the type checker
  gets a chance to do its work.
  
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
  ``data Tagged (%Nominal t) a = Tagged a``. Or it might have been an
  alternative for ``-XDerivingStrategies``.

* Though not proposed here, we can imagine a large extension to this
  mechanism allowing for *user-written* modifiers, giving meanings
  via a plugin. Perhaps some modifier supports some function call to the GHC API that
  transforms the meaning of bit of syntax. The possibilities are
  tantalizing.
  
* These modifiers recall Java's `Annotations <https://en.wikipedia.org/wiki/Java_annotation>`_
  mechanism, which were a direct inspiration.
  
Costs and Drawbacks
-------------------
* The loss of the inferred kind of ``m`` in multiplicity polymorphism is a
  drawback. However, a user seeing ``Int %m -> Bool`` is hard-pressed to
  understand what is going on. On the other hand, labeling ``m :: Multiplicity``
  explicitly (either in the binding for ``m`` or in a usage site)
  is much more perspicuous.

* Any feature has a maintenance burden, but this one should be fairly small.

* Having yet another special symbol in a special position is a drawback.
  Yet ``%`` is *already* such a symbol (due to ``-XLinearTypes``), and the
  existence of an extensible modifiers mechanism makes it possible to
  avoid adding new symbols to this set.

Alternatives
------------
* A previous version of this proposal described that modifiers would work
  via a ``Modifier`` class-like constraint. However, type inference seemed,
  well, challenging. So this simplifies the proposal to be more syntactic.

* There does not seem to be much point in introducing modifier
  syntax beyond the linear-types syntax, but it seemed helpful to do so here.
  We can drop that.

Unresolved Questions
--------------------
* Is it too soon? That is, this proposal solves a problem we do not yet have:
  the combination of multiplicity and matchability. Yet, it seems much easier
  to consider this idea separate from the quite considerable complexity of `#242`_,
  and so I have made it a separate proposal.

.. _`#242`: https://github.com/ghc-proposals/ghc-proposals/pull/242
