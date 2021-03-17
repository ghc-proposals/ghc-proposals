Stable GADT constructor syntax
==============================

.. author:: Vladislav Zavialov
.. date-accepted::
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/402>`_.
.. contents::

The syntax of GADT constructor declarations is `documented in the User's Guide
<https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/exts/gadt_syntax.html#formal-syntax-for-gadts>`_
as "subject to change". We propose to fix a couple of issues associated with it
and to advertise the resulting syntax as stable (that is, a breaking change to
it would require another proposal).


Motivation
----------

First and foremost, let us do away with the misconception that the GADT
constructor signatures are type signatures.

1. **Constructor-specific syntax**

   There are syntactic forms that are not valid in type signatures but are
   currently allowed in GADT constructor signatures::

     data T a where
       S :: !a -> T a
       R :: { fld :: a } -> T a

   In ``S``, the field is marked as strict using a prefix ``!``. In ``R``, the
   field is given a name using record syntax ``{ ... }``. The signatures of these
   constructors are perfectly fine, but they are rejected if used as function type
   signatures::

     s :: !a -> T a
     s = S

     r :: { fld :: a } -> T a
     r = R

   ``s`` and ``r`` are rejected with the following error messages::

     GadtConSyn.hs:7:6: error:
         Unexpected strictness annotation: !a
         strictness annotation cannot appear nested inside a type

     GadtConSyn.hs:10:6: error:
         Record syntax is illegal here: {fld :: a}

2. **Constructor-specific limitations**

   There are also type signatures that are not valid GADT constructor
   signatures::

     k :: a -> Eq a => F

   This is accepted (assuming a ``data F`` declaration elsewhere), but an
   analogous constructor signature is not::

     data F where
       K :: a -> Eq a => F

   ``K`` is rejected with the following error message::

     GadtConSyn.hs:16:3: error:
         GADT constructor type signature cannot contain
         nested ‘forall’s or contexts

So, if GADT constructor signatures are syntactically not type signatures, then
what are they?  The User's Guide provides a BNF-style grammar for GADT
constructor declarations, see `6.4.7.1. Formal syntax for GADTs
<https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/exts/gadt_syntax.html#formal-syntax-for-gadts>`_::

  gadt_con ::= conids '::' opt_forall opt_ctxt gadt_body

  conids ::= conid
          |  conid ',' conids

  opt_forall ::= <empty>
              |  'forall' tv_bndrs '.'

  tv_bndrs ::= <empty>
            |  tv_bndr tv_bndrs

  tv_bndr ::= tyvar
           |  '(' tyvar '::' ctype ')'

  opt_ctxt ::= <empty>
            |  btype '=>'
            |  '(' ctxt ')' '=>'

  ctxt ::= ctype
        |  ctype ',' ctxt

  gadt_body ::= prefix_gadt_body
             |  record_gadt_body

  prefix_gadt_body ::= '(' prefix_gadt_body ')'
                    |  return_type
                    |  opt_unpack btype '->' prefix_gadt_body

  record_gadt_body ::= '{' fieldtypes '}' '->' return_type

  fieldtypes ::= <empty>
              |  fieldnames '::' opt_unpack ctype
              |  fieldnames '::' opt_unpack ctype ',' fieldtypes

  fieldnames ::= fieldname
              |  fieldname ',' fieldnames

  opt_unpack ::= opt_bang
              :  {-# UNPACK #-} opt_bang
              |  {-# NOUNPACK #-} opt_bang

  opt_bang ::= <empty>
            |  '!'
            |  '~'

Unfortunately, there are a couple of issues associated with it:

* Right before this grammar is given, the User's Guide states "Note that this
  grammar is subject to change in the future", so it is of limited use when
  writing forward-compatible code.

* It does not actually match the implementation, as it mistakenly states that a
  strictness annotation is followed by a ``btype`` (it actually must be
  followed by an ``atype``), and it does not mention linear type syntax at all.

* It is too permissive with regards to parentheses, causing implementation
  issues (see `#19192
  <https://gitlab.haskell.org/ghc/ghc/-/issues/19192#note_329172>`_).

In this proposal we aim to give an alternative specification that would fix the
aforementioned issues.

Let us first point out that GADT constructor syntax pursues two contradictory
goals:

1. On the one hand, GADT constructor syntax tries its best to mimic type
   syntax::

     data T x where
       MkT :: forall a. Eq a => Maybe a -> T 0

     ghci> :t +v MkT
     MkT :: forall a. Eq a => Maybe a -> T 0

   Notice how the type signature reported by ``:t`` is identical to the one in
   the declaration.

2. On the other hand, GADT constructor syntax allows the user to specify
   additional information about the fields, such as their names (using record
   syntax ``{ fld :: a }``) and strictness (``{-# UNPACK #-}``, ``!a``,
   ``~a``). This information is not reflected in the constructor's type.

It is impossible to define a constructor by its type signature and at the same
time include information that is not part of its type signature. This inherent
contradiction means that any solution will be a compromise.

Perhaps if we were to drop one of these goals, we could come up with something
glorious, but in practice we must take backwards-compatibility and ease of
implementation into account. With that in mind, let us establish the following
principles:

1. A GADT constructor signature is either in *prefix style* or *record style*.
   The two styles are completely distinct and we do not seek to unify their syntax.

2. A record-style GADT constructor signature has the following parts in a
   fixed order:

   #. (optional) ``forall tvs.`` to bind type variables;
   #. (optional) ``ctx =>`` to introduce constraints;
   #. ``{ fld1 :: a, fld2 :: b, ... }`` to describe constructor fields, using
      the same syntax as classic Haskell98-style record type declarations, such
      as ``data R = MkR { fld1 :: a, fld2 :: b, ... }``;
   #. ``->`` as a separator, which is special syntax rather than
      the ``(->)`` type constructor;
   #. the result type ``T``;

3. A prefix-style GADT constructor signature has zero or more of the following
   parts in a free order:

   #. type variable telescope ``forall tvs.`` or ``forall tvs ->``;
   #. constraint context ``ctx =>``;
   #. parameter ``a ->``, ``a %p ->``, or ``a ⊸``, possibly with a strictness
      annotation;

   It then ends with the result type ``T``.


Proposed Change Specification
-----------------------------

Changes from status quo
~~~~~~~~~~~~~~~~~~~~~~~

1. Drop support for parentheses around the tail of a prefix-style constructor
   signature (fix `#19192
   <https://gitlab.haskell.org/ghc/ghc/-/issues/19192>`_).
2. Permit nested foralls and contexts (a free order of quantifiers) in
   prefix-style constructor signatures (fix `#18389
   <https://gitlab.haskell.org/ghc/ghc/-/issues/18389>`_).

Grammar specification
~~~~~~~~~~~~~~~~~~~~~

Existing non-terminals
^^^^^^^^^^^^^^^^^^^^^^

In this section we describe the non-terminals, the definition of which is out
of scope of this proposal (they are already present in the GHC grammar)

1. ``atype`` - syntactically atomic types, e.g. ``T``, ``42``, ``x``, ``( ... )``, ``[ ... ]``
2. ``ftype`` - application chain consisting of ``atype``, e.g. ``T @a b``
3. ``btype`` - infix operator chain consisting of ``ftype``, e.g. ``T @a b + Q 42``
4. ``forall_telescope`` - either ``forall tvs.`` or ``forall tvs ->``
5. ``sig_vars`` - comma-separated field names, e.g. ``fld_1, fld_2, fld_n``, n >= 1
6. ``mult`` - multiplicity annotation, e.g. ``%1``

Proposed non-terminals
^^^^^^^^^^^^^^^^^^^^^^

Constructor declarations::

  gadt_con ::= conids '::' gadt_prefix_sig
            |  conids '::' gadt_record_sig

  conids ::= conid
          |  conid ',' conids


Prefix form::

  gadt_prefix_sig ::= btype
                   |  quantifier gadt_prefix_sig

  quantifier ::= forall_telescope
              |  btype '=>'
              |  fieldtype '->'
              |  fieldtype '⊸'
              |  fieldtype mult '->'

Record syntax::

  gadt_record_sig ::= opt_forall opt_ctx '{' fielddecls '}' '->' btype

  opt_forall ::= <empty>
              |  forall_telescope

  opt_ctx ::= <empty>
           |  btype '=>'

  fielddecls ::= fielddecl
              |  fielddecl ',' fielddecl

  fielddecl ::= sig_vars '::' fieldtype
             |  sig_vars mult '::' fieldtype

Field types::

  fieldtype ::= opt_unpack btype
              | opt_unpack strictness_sigil atype

  opt_unpack ::= <empty>
              |  '{-# UNPACK #-}'
              |  '{-# NOUNPACK #-}'

  strictness_sigil ::= '!'
                    |  '~'

Clarifications and side conditions:

1. In ``gadt_record_sig``, the ``opt_forall`` must be of the ``forall tvs.`` form, not ``forall tvs ->``.
2. In ``strictness_sigil``, the ``!`` and ``~`` are assumed to be prefix occurrences.
3. In ``strictness_sigil``, the ``~`` is guarded behind ``-XStrictData``.
4. Under ``UnicodeSyntax``, the ``::``, ``->``, and ``=>`` terminals can be
   written as ``∷``, ``→``, and ``⇒`` respectively.
5. The ``btype`` production in ``gadt_prefix_sig`` and ``gadt_record_sig`` is
   taken as the result type of the GADT constructor; it must be a substitution
   instance (allowing expansion of type synonyms, but not type families)
   of the type being declared. This implies that ``data T where T1 ::
   (Int -> T)`` is rejected. The error message should not only say that ``(Int
   -> T)`` is not an instance of ``T`` but also helpfully explain where
   parentheses are and are not allowed in constructor signatures.
   (Note that the treatment of type synonyms vs. type families matches the
   status quo.)

Effect and Interactions
-----------------------

Since we drop support for parentheses, the following programs will be all
rejected by the new grammar::

  data T where
    T1 :: (Int -> T)
    T2 :: (forall a. a -> T)
    T3 :: forall a. (a -> T)
    T4 :: forall a. (a -> (T))

This makes it possible to avoid conflicts between ``btype`` and
``gadt_prefix_sig`` in the LALR grammar.

On the other hand, we now allow nested ``forall`` and contexts::

  data T a where
    MkT :: Int -> forall a. Eq a => T a

Implementing this proposal as described will fix
`#19192 <https://gitlab.haskell.org/ghc/ghc/-/issues/19192>`_ and
`#18389 <https://gitlab.haskell.org/ghc/ghc/-/issues/18389>`_, and unblock
`#18782 <https://gitlab.haskell.org/ghc/ghc/-/issues/18782>`_.

Costs and Drawbacks
-------------------

* The proposed grammar allows parentheses in fewer places than the current
  implementation (e.g. ``MkT :: Int -> (Int -> T)`` will be rejected) and users
  will need to migrate.

* The free order of quantifiers in prefix-style constructors will have major
  effect on the implementation.

* The fixed order of quantifiers in record-style constructors makes it more
  limited than prefix-style constructors. We defer this concern to future work,
  as it is not obvious which syntax would be best and there is no pressing need
  to address this.

Future Work
-----------

* Generalize the record syntax to allow flexible quantification.
* Fix pattern synonym signatures in a similar way.

Alternatives
------------

* Complete redesign of GADT syntax to avoid contradictory goals.

Unresolved Questions
--------------------

None at the moment.
