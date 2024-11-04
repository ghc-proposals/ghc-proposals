Syntax for Modifiers: a generalization of linear-types syntax
==============

.. author:: Richard Eisenberg
.. date-accepted:: 2020-12-18
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/22624
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at pull request #370 <https://github.com/ghc-proposals/ghc-proposals/pull/370>`_ and amended by `pull request #392 <https://github.com/ghc-proposals/ghc-proposals/pull/392>`_.
.. contents::

This proposal introduces a new form of syntax ``%blah`` that defines a *modifier*.
Modifiers somehow change the meaning of the next token or of the construct they
appear in. The ``blah`` is parsed
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

     type          ::= btype [ {modifier} -> type ]
     prefix%       ::= '%'    -- only in prefix position
     modifier      ::= prefix% atype
     modifiers     ::= {modifier}
     declModifiers ::= [ modifier [ ';' ] [ declModifiers ] ]

3. With ``-XModifiers``, introduce modifier syntax on types as follows::

     btype    ::= modifiers atype | btype atype

4. With ``-XModifiers``, introduce modifier syntax on the term level as follows::

     fexp     ::= modifiers aexp | fexp aexp

5. With ``-XModifiers``, introduce modifier syntax in patterns as follows::

     lpat     ::= modifiers lpat | ...

6. With ``-XModifiers``, introduce modifier syntax on record field declarations as follows::

     fielddecl ::= vars modifiers '::' (type | '!' atype)

7. With ``-XModifiers``, introduce modifier syntax on top-level declarations as
   follows::

     topdecl ::= declModifiers 'type' simpletype '=' type
             |   declModifiers 'data' [context '=>'] simpletype ['=' constrs] [deriving]
             |   declModifiers 'newtype' [context '=>'] simpletype = newconstr [deriving]
             |   declModifiers 'type' 'data' ...
             |   declModifiers 'class' [scontext '=>'] tycls tyvar ['where' cdecls]
             |   declModifiers 'instance' [scontext '=>'] qtycls inst ['where' idecls]
             |   declModifiers 'default' '(' type1 ',' ... ',' typen ')'
             |   declModifiers 'foreign' fdecl
             |   declModifiers ';' decl

   Recall that the Haskell 2010 Report uses brackets to denote an optional bit
   of syntax. The optional semicolons allow modifiers to appear on a line
   previous from the declaration affected. The semicolon is mandatory on
   ``decl`` because ``decl``\ s do not start with keywords (except for fixity
   declarations) and may have modifiers of their own. The semicolon makes
   clear that the modifier is meant to affect the entire declaration.

8. With ``-XModifiers``, introduce modifier syntax on data constructor
   declarations as follows::

     -- H98-style constructor
     constr ::= modifiers con ['!'] atype1 ... ['!'] atypek
              | modifiers (btype | '!' atype) conop (btype | '!' atype)
              | modifiers con '{' fielddecl1 ',' ... ',' fielddecln '}'

     -- GADT-style constructor
     gadt_constrs ::= modifiers con_list '::' sigtype

   Modifiers in ``gadt_constrs`` apply to each constructor in ``con_list``.

9. Reserve the use of ``%`` in a prefix occurrence to be used only for modifiers;
   though this proposal does not do so, we can imagine extending the modifier syntax
   to apply to further syntactic situations (e.g. term-level operators, declarations,
   import lists, etc.).

10. Modifiers are parsed, renamed, and type-checked as *types*.

11. With ``-XModifiers``, the type of a modifier is determined only by
    synthesis, not by checking.
    That is, in the bidirectional type-checking scheme used by GHC, we find the
    type of the modifier by running the synthesis judgment. Effectively, this
    means that if we consider a modifier to be some head (constructor or
    variable) applied to a sequence of arguments (possibly none), the head must
    have a known type: constructors always have a known type, and variables
    have a known type if declared with a type signature. Alternatively, the
    modifier may have a top-level type signature.

12. Future modifiers will be put *before* the element they modify. Alternatively,
    a modifier can be put directly before a syntactic closer or separator, such
    as ``;`` or ``where`` or ``)``.

13. Modifiers of unknown kind produce an error.

14. Modifiers of known kind but with an unknown meaning produce a warning,
    controlled by ``-Wunknown-modifiers``. They are otherwise ignored. (However,
    in order to know that a modifier is unknown, it still must be parsed,
    renamed, and type-checked.)

LinearTypes
-----------
With ``-XLinearTypes``:

* A modifier of type ``Multiplicity`` changes the multiplicity of the following
  arrow, or following pattern-bound variable of a lambda, or following let or
  where binding, or preceding record field. Multiple modifiers of type
  ``Multiplicity`` on the same arrow are not allowed. Any other use of a
  modifier still has no meaning.

* The ``%1`` modifier is handled as a special case. It's renamed (and so
  typechecked) the same as ``%One``, even if it appears somewhere that linear
  modifiers aren't expected. If a user does want the modifier ``1 :: Nat``, they
  can write it as ``%01``. (Requires ``-XDataKinds``.)

* The linear arrow ``a ⊸ b`` has the same meaning as ```a %1 -> b``. Other
  modifiers are accepted: ``a %Matchable ⊸ b`` has the same meaning as
  ``a %Matchable %1 -> b``.

With ``-XNoLinearTypes``, the ``%1`` modifier is not special. It refers to the
type ``1 :: Nat`` and requires ``-XDataKinds``.

``-XLinearTypes`` implies ``-XModifiers``. But the latter can be explicitly
disabled with ``-XLinearTypes -XNoModifiers``. This introduces backwards
compatible behavior:

* Only ``Multiplicity`` modifiers are permitted, and only in the places they're
  recognized. Any use of a modifier is an error.

* The kind of a modifier is determined by checking for ``Multiplicity``, not
  through synthesis. So ``Int %m -> Bool`` is forbidden with ``-XLinearTypes
  -XModifiers``, because ``m`` has unknown kind. But it's permitted with
  ``-XLinearTypes -XNoModifiers``, equivalently to ``Int %(m :: Multiplicity) ->
  Bool``.

This behavior may be deprecated in future.

Renaming and typechecking
-------------------------
Initially, the only recognized modifiers will be the multiplicity modfifiers of
linear types. These take effect during typechecking. Other modifiers are
expected to take effect during renaming, such as the ``NoFieldSelectors``
modifier of `proposal 512`_.

.. _`proposal 512`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0512-nofieldselectors-per-datatype.md

This poses a problem. The following is accepted::

  type family F a where
    F 1 = Many
    F 2 = NoFieldSelectors

  f :: () %(F 1) -> ()

Ideally, the following would be accepted as well::

  %(F 2) data G = G { g :: () }

But type family resolution happens in the typechecker.

Define an **RN modifier** as one that takes effect during renaming. This is
context-dependent: ``%NoFieldSelectors`` will be an RN modifier before a
``data`` declaration, but not before an arrow.

One possible solution is to simply forbid such things for RN modifiers. If the
type can't be resolved during renaming, it has no effect during renaming. During
typechecking, if it gets resolved to something that would have had effect during
renaming, we throw an error.

This isn't ideal because it's not obvious to users which modifiers are RN
modifiers.

A more complicated solution involves invoking the typechecker during renaming.
To do this we need a "stage restriction": a modifier obeys the stage restriction
if every identifier and every type family instance in the modifier is imported,
not defined in the current module.

When renaming a modifier, if it violates the stage restriction, it takes no
effect during renaming. We then typecheck the modifier, during typechecking. If
it resolves to an RN modifier, we throw an error.

If it doesn't violate the stage restriction, then we can typecheck it during
renaming. If it resolves to an RN modifier, it takes effect. Subsequently, we
typecheck it again during typechecking. We throw an error if either

* It resolved to an RN modifier during renaming, and resolves to a different
  modifier during typechecking.

* It didn't resolve to an RN modifier during renaming, but does during
  typechecking.

It might resolve to a different modifier if there are overlapping instances
defined in this module.

This solution would probably not be implemented in the initial release of
modifiers.

Examples
--------
Here are some examples that will be accepted or rejected with this proposal::

  f1 :: Int %1 -> Bool    -- accepted: %1 is a special case, see below.
  f2 :: Int %Many -> Bool -- accepted: Many :: Multiplicity
  f3 :: Int %() -> Bool   -- accepted: () :: ()
  f4 :: Int %m -> Bool    -- rejected: the kind of m is undeclared
  f5 :: Int %(m :: Multiplicity) -> Bool   -- accepted with a type signature
  f6 :: Int %One %Many -> Bool
    -- rejected (although it will parse) with -XLinearTypes; accepted otherwise
  f7 :: Int %Many %Many -> Bool
    -- rejected with -XLinearTypes; accepted otherwise
  f8 :: Int %(m :: Multiplicity) -> Int %m -> Int
    -- rejected: the second use of '%m' has an unknown king

  map :: forall (m :: Multiplicity). (a %m -> b) -> [a] %m -> [b]
    -- accepted: m has a known type

With ``-XLinearTypes -XNoModifiers``, ``f4`` and ``f8`` are accepted, and ``f3``
is rejected.

The syntax (and semantics) for modifiers on patterns and record fields is exactly
as described in the `linear types proposal`_.

.. _`linear types proposal`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst#syntax

Further examples:

* Types: ``%Mod1 T (%Mod2 a) (%Mod3 (S b))``; ``Mod1`` applies to ``T``, ``Mod2`` applies to ``a``, and ``Mod3`` applies to ``S b``.
  Note that this proposal does not introduce any valid modifiers for types.

* Terms: Same as the example above.

* Lambda expressions: ``\ %Many x -> ...``, ``\ %One x %Many y -> ...``.

* Field declaration: ``data T = MkT { field %Many :: Int }``.

* Class declaration: ``%Mod class C a where ...``. Other declaration forms are similar. This proposal
  does not introduce any valid modifiers for types, but `#390 <https://github.com/ghc-proposals/ghc-proposals/pull/390>`_ does.

Effect and Interactions
-----------------------
* It is expected that the matchability of `#242`_ will have a kind ``Matchability``.
  Then, users will be able
  to write ``Int %Many %Matchable -> Bool`` or ``Int %Matchable %Many -> Bool``.
  The details are left to `#242`_ (assuming this proposal is accepted first).
  The author of `#242`_, Csongor Kiss, was involved in the conceptualization of
  this proposal.

* Proposals `#390`_ and `#512`_ also anticipate using modifier syntax.

.. _`#390`: https://github.com/ghc-proposals/ghc-proposals/pull/390
.. _`#512`: https://github.com/ghc-proposals/ghc-proposals/pull/512

* Future modifiers will also seamlessly work with existing ones, where order
  is not expected to matter (though that would be up to other proposals to
  spell out).

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

* A key design principle here is that modifiers affect the next item in the AST (if
  one exists). By keeping with this principle, we avoid the possibility of ambiguity:
  if some modifiers affected a previous element and some affected the next, then we
  could find ourselves in trouble.

* The ``-Wunknown-modifiers`` warning is meant to enable future compatibility. For
  example, suppose we want to label ambiguous types with ``%Ambiguous``. It would
  be very annoying to use, say, CPP to remove the modifier for GHCs that do not
  support it. Instead, this proposal allows the modifier to be accepted and
  ignored. This would only work if ``Ambiguous`` is in scope in the type namespace.
  Additionally, a given GHC must know how to parse modifiers at the
  location where they are written. Perhaps a more complete design would modify
  the entire Haskell grammar putting modifiers wherever they could potentially
  make sense (and thus be more future compatible), but this proposal covers
  only types and terms (and not, say, class declarations).

* Because modifiers are treated as types, they will typically begin with
  a capital letter. (Note that a polymorphic multiplicity is a type variable,
  and this is fine.)

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

* We could avoid ambiguity using extra punctuation (e.g. ``class ( %Mod1, %Mod2 ) C a b => D a b c where ...``),
  but "modifiers come before what they modify" is simple and uniform.

* We could require semicolons between modifiers and opening keyword
  for all declarations, but it seems easy enough and harmless enough not to.

Unresolved Questions
--------------------
* Is it too soon? That is, this proposal solves a problem we do not yet have:
  the combination of multiplicity and matchability. Yet, it seems much easier
  to consider this idea separate from the quite considerable complexity of `#242`_,
  and so I have made it a separate proposal.

.. _`#242`: https://github.com/ghc-proposals/ghc-proposals/pull/242

* This proposal floats the idea of ``%oneShot`` and ``%inline``, but these
  might fit better as pragmas than modifiers. In any case, they are not
  proposed concretely here and would be subject to a future proposal.

* How does this interact with Template Haskell?

* What warning groups imply ``-Wunknown-modifiers``?
