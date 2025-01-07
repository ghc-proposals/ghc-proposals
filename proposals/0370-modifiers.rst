Syntax for Modifiers: a generalization of linear-types syntax
==============

.. author:: Richard Eisenberg
.. date-accepted:: 2020-12-18
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/22624
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at pull request #370 <https://github.com/ghc-proposals/ghc-proposals/pull/370>`_ and amended by `pull request #392 <https://github.com/ghc-proposals/ghc-proposals/pull/392>`_.
.. sectnum::
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

     type          ::= btype [ modifiers -> type ]
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

13. Modifiers of unknown or polymorphic kind produce an error.

14. Modifiers of known kind but with an unknown meaning produce a warning,
    controlled by ``-Wunknown-modifiers``. They are otherwise ignored. (However,
    in order to know that a modifier is unknown, it still must be parsed,
    renamed, and type-checked.)

15. With ``-XLinearTypes``:

    * A modifier of type ``Multiplicity`` changes the multiplicity of the
      following arrow, or following pattern-bound variable of a lambda, or
      following let or where binding, or preceding record field. Multiple
      modifiers of type ``Multiplicity`` on the same arrow are not allowed. Any
      other use of a modifier still has no meaning.

    * The ``%1`` modifier is handled as a special case. It's renamed (and so
      typechecked) the same as ``%One`` (using the ``One`` from ``base``), even
      if it appears somewhere that linear modifiers aren't expected. If a user
      does want the modifier ``1 :: Nat``, they can write it as ``%01``.
      (Requires ``-XDataKinds``.)

    * The linear arrow ``a ⊸ b`` has the same meaning as ``a %1 -> b``. Other
      modifiers are accepted: ``a %Matchable ⊸ b`` has the same meaning as ``a
      %Matchable %1 -> b``.

16. With ``-XModifiers -XNoLinearTypes``, the ``%1`` modifier is not special. It
    refers to the type ``1 :: Nat`` and requires ``-XDataKinds``. The warning
    generated by ``-Wunknown-modifiers`` hints that the user probably wants to
    enable ``-XLinearTypes``.

17. With ``-XLinearTypes -XNoModifiers``, backwards compatible behavior is
    introduced:

    * Only ``Multiplicity`` modifiers are permitted, and only in the places
      they're recognized. Any other use of a modifier is an error.

    * The kind of a modifier is determined by checking for ``Multiplicity``, not
      through synthesis. So ``Int %m -> Bool`` is forbidden with
      ``-XLinearTypes -XModifiers``, because ``m`` has unknown kind. But it's
      permitted with ``-XLinearTypes -XNoModifiers``, equivalently to
      ``Int %(m :: Multiplicity) -> Bool``.

    This may be deprecated in future.

18. ``-XLinearTypes`` implies ``-XModifiers``. But the latter can be explicitly
    disabled with ``-XLinearTypes -XNoModifiers``.

Renaming and typechecking
-------------------------
*This section is descriptive, not normative.*

When a proposal introduces a modifier, that proposal must specify how it
behaves. Initially, the only recognized modifiers will be the multiplicity
modifiers of linear types, and their behavior is specified above. But we expect
there to be two general stories.

Some modifiers will take effect during type checking, such as multiplicity
modifiers. Call these **TC modifiers**. Others will take effect during renaming,
such as the ``NoFieldSelectors`` modifier of `proposal 512`_. Call these **RN
modifiers**.

.. _`proposal 512`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0512-nofieldselectors-per-datatype.md

For multiplicity modifiers, the following is accepted::

  type family F a where
    F 1 = Many
  f :: () %(F 1) -> ()

And we expect that for other TC modifiers, this would work too. A TC modifier
can be replaced by an equal-in-the-type-system type without changing program
behavior.

For RN modifiers, this wouldn't work. Only the exact type constructor would take
effect. For example, the modifier in the following would be unrecognized::

  type NFS = NoFieldSelectors
  %NFS data G = G { g :: () }

All modifiers must be type checked. So it would be possible to detect such
situations. If a modifier isn't recognized during renaming, but resolves to a
recognized RN modifier during type checking, a warning or error could be
emitted.

But it would also be possible to support type families and synonyms. This would
be a more complicated solution, involving invoking the typechecker during
renaming. To do this we need a "stage restriction": a modifier obeys the stage
restriction if every identifier and every type family instance in the modifier
is imported, not defined in the current module.

When renaming a modifier, if it violates the stage restriction, it takes no
effect during renaming. We then typecheck the modifier, during typechecking. If
it resolves to an RN modifier, we throw a warning or error.

If it doesn't violate the stage restriction, then we can typecheck it during
renaming. If it resolves to an RN modifier, it takes effect. Subsequently, we
typecheck it again during typechecking. We throw a warning or error if either

* It resolved to an RN modifier during renaming, and resolves to a different
  modifier during typechecking.

* It didn't resolve to an RN modifier during renaming, but does during
  typechecking.

It might resolve to a different modifier if there are overlapping instances
defined in this module.

We expect this more complicated behavior would only be adopted if there's user
demand for it after RN modifiers are first released.

Examples
--------
Here are some examples that will be accepted or rejected with this proposal::

  f1 :: Int %1 -> Bool      -- accepted: %1 is a special case, see below.
  f2 :: Int %Many -> Bool   -- accepted: Many :: Multiplicity
  f3 :: Int %() -> Bool     -- accepted: () :: Type
  f4 :: Int %m -> Bool      -- rejected: the kind of m is undeclared
  f5 :: Int %(m :: Multiplicity) -> Bool  -- accepted with a type signature
  f6 :: Int %One %Many -> Bool
    -- rejected (although it will parse) with -XLinearTypes; accepted otherwise
  f7 :: Int %Many %Many -> Bool
    -- rejected with -XLinearTypes; accepted otherwise
  f8 :: Int %(m :: Multiplicity) -> Int %m -> Int
    -- rejected: the second use of '%m' has an unknown kind
  f9 :: Int %Maybe -> Bool  -- accepted: Maybe :: Type -> Type
  f10 :: Int %Nothing -> Bool
    -- rejected: `Nothing :: Maybe a` has polymorphic kind

  map :: forall (m :: Multiplicity). (a %m -> b) -> [a] %m -> [b]
    -- accepted: m has a known type

  -- these are all accepted:
  data D = %() Int :* Bool   -- the constructor declaration is modified
  data D = %() (:*) Int Bool -- the same
  data D = (%() Int) :* Bool -- the type of the first argument is modified

  x :: %Maybe Int -- x is of type (Int, modified with type Maybe)
  x :: %(%Maybe Maybe) Int
    -- x is of type (Int, modified with type (Maybe, modified with type Maybe))

With ``-XLinearTypes -XNoModifiers``, ``f4`` and ``f8`` are accepted, and
``f3``, ``f9``, ``f10``, and all the modifiers not attached to arrows are
rejected.

The syntax (and semantics) for modifiers on patterns and record fields is exactly
as described in the `linear types proposal`_.

.. _`linear types proposal`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst#syntax

Further examples:

* Types: ``%Mod1 T (%Mod2 a) (%Mod3 (S b))``; ``Mod1`` applies to ``T``, ``Mod2`` applies to ``a``, and ``Mod3`` applies to ``S b``.
  Note that this proposal does not introduce any valid modifiers for types.

* Terms: Same as the example above.

* Lambda expressions: ``\ (%Many x) -> ...``,
  ``\ (%One x :: Int) (%Many y) -> ...``.

* Field declaration: ``data T = MkT { field %Many :: Int }``.

* Class declaration: ``%Mod class C a where ...``. Other declaration forms are similar. This proposal
  does not introduce any valid modifiers for classes, but `#390 <https://github.com/ghc-proposals/ghc-proposals/pull/390>`_ does.

Effect and Interactions
-----------------------
* It is expected that the matchability of `#242`_ will have a kind ``Matchability``.
  Then, users will be able
  to write ``Int %Many %Matchable -> Bool`` or ``Int %Matchable %Many -> Bool``.
  The details are left to `#242`_ (assuming this proposal is accepted first).
  The author of `#242`_, Csongor Kiss, was involved in the conceptualization of
  this proposal.

* Proposals `#390`_ and `#512`_ also anticipate using modifier syntax. Proposal
  `#232`_ predates this proposal, but hasn't yet been implemented, and the
  author thinks it's mostly a good fit for modifier syntax.

.. _`#232`: https://github.com/ghc-proposals/ghc-proposals/pull/232
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
  say, a matchability, which is also expected to support polymorphism).

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

* What scope are modifiers looked up in? For example::

    %a data F a
    %G data G

  If these are accepted, they'd be meaningless under the current proposal. But
  should they be rejected, or accepted with an unknow-modifiers warning, or
  what?
