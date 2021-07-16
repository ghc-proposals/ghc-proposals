Record Dot Syntax
=================

.. author:: Neil Mitchell and Shayne Fletcher
.. date-accepted:: 2020-05-03, amended 2021-03-09
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/18599
.. implemented:: 9.2
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/282>`_ and  `amended by this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/405>`_.
.. contents::


Records in Haskell are `widely recognised
<https://www.yesodweb.com/blog/2011/09/limitations-of-haskell>`__ as
being under-powered, with duplicate field names being particularly
troublesome. We propose new language extensions
``OverloadedRecordDot`` and ``OverloadedRecordUpdate`` that provide
syntactic sugar to make the features introduced in `the HasField
proposal
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst>`__
more accessible, improving the user experience.

1. Motivation
-------------

In almost every programming language we write ``a.b`` to mean the ``b``
field of the ``a`` record expression. In Haskell that becomes ``b a``,
and even then, only works if there is only one ``b`` in scope. Haskell
programmers have struggled with this weakness, variously putting each
record in a separate module and using qualified imports, or prefixing
record fields with the type name. We propose bringing ``a.b`` to
Haskell, which works regardless of how many ``b`` fields are in scope.
Here’s a simple example of what is on offer:

.. code:: haskell

   {-# LANGUAGE OverloadedRecordDot, OverloadedRecordUpdate #-}

   data Company = Company {name :: String, owner :: Person}
   data Person = Person {name :: String, age :: Int}

   display :: Company -> String
   display c = c.name ++ " is run by " ++ c.owner.name

   nameAfterOwner :: Company -> Company
   nameAfterOwner c = c{name = c.owner.name ++ "'s Company"}

We declare two records both having ``name`` as a field label. The user
may then write ``c.name`` and ``c.owner.name`` to access those fields.
We can also write ``c{name = x}`` as a record update, which works even
though ``name`` is no longer unique. Under the hood, we make use of
``getField`` and ``setField`` from the `HasField proposal <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst>`__.

An implementation of this proposal has been battle tested and hardened
over two years in the enterprise environment as part of `Digital
Asset <https://digitalasset.com/>`__\ ’s `DAML <https://daml.com/>`__
smart contract language (a Haskell derivative utilizing GHC in its
implementation), and also in a `Haskell preprocessor and a GHC
plugin <https://github.com/ndmitchell/record-dot-preprocessor/>`__. When
initially considering Haskell as a basis for DAML, the inadequacy of
records was considered the most severe problem, and without devising the
scheme presented here, wouldn’t be using Haskell. The feature enjoys
universal popularity with users.

2. Proposed Change Specification
--------------------------------

For the specification we focus on the changes to the parsing rules, and
the desugaring, with the belief the type checking and renamer changes
required are an unambiguous consequences of those.

2.1 Language extensions
~~~~~~~~~~~~~~~~~~~~~~~

This change adds new language extensions ``OverloadedRecordDot`` and
``OverloadedRecordUpdate``.

If ``OverloadedRecordDot`` is on:

- The expression ``.lbl`` means ``getField @"lbl"``;
- The expression ``e.lbl`` means ``getField @"lbl" e``.

If ``OverloadedRecordDot`` is not on, these expressions are parsed as
uses of the function ``(.)``.

If ``OverloadedRecordUpdate`` is on:

- The expression ``e{lbl = val}`` means ``setField @"lbl" val``;
- Update expressions with qualified labels like ``r{M.x = val}`` are disallowed.

If ``OverloadedRecordUpdate`` is not on, ``e{lbl = val}`` means just
what it does in Haskell98.

If ``OverloadedRecordDot`` and ``OverloadedRecordUpdate`` are both on
the expression ``e{lbl₁.lbl₂ = val}`` means ``e{lbl₁ = (e.lbl₁){lbl2 =
val}}`` otherwise the expression ``e{lbl₁.lbl₂ = val}`` is illegal.

2.1.1 Syntax
^^^^^^^^^^^^

In the event the language extensions ``OverloadedRecordDot`` and
``OverloadedRecordUpdate`` are enabled:

======================= ==================================
Expression              Equivalent
======================= ==================================
``(.lbl)``              ``(\e -> e.lbl)``
``(.lbl₁.lbl₂)``        ``(\e -> e.lbl₁.lbl₂)``
``e.lbl``               ``getField @"lbl" e``
``e.lbl₁.lbl₂``         ``(e.lbl₁).lbl₂``
``e{lbl = val}``        ``setField @"lbl" e val``
``e{lbl₁.lbl₂ = val}``  ``e{lbl₁ = (e.lbl₁){lbl₂ = val}}``
``e.lbl₁{lbl₂ = val}``  ``(e.lbl₁){lbl₂ = val}``
``e{lbl₁ = val₁}.val₂`` ``(e{lbl₁ = val₁}).val₂``
``e{lbl₁}``             ``e{lbl₁ = lbl₁}`` [Note: requires ``NamedFieldUpdates``]
``e{lbl₁.lbl₂}``        ``e{lbl₁.lbl₂ = lbl₂}`` [Note: requires ``NamedFieldUpdates``]
======================= ==================================

- **Updating nested fields.** ``e{lbl = val}`` is the syntax of a standard H98 record update. It’s the nested form introduced by this proposal that is new : ``e{lbl1.lbl2 = val}``. However, in the event ``OverloadedRecordUpdate`` is in effect, note that ``e{lbl = val}`` desugars to ``setField @"lbl" e val``].
- **Punning.** With ``NamedFieldPuns``, the form ``e { x, y }`` means ``e { x=x, y=y }``. With ``OverloadedRecordUpdate`` this behaviour is extended to nested updates: ``e { a.b.c, x.y }`` means ``e { a.b.c=c, x.y=y }``. Note the variable that is referred to implicitly (here ``c`` and ``y``) is the last chunk of the field to update. So ``c`` is the last chunk of ``a.b.c``, and ``y`` is the last chunk of ``x.y``.

2.1.2 Precedence
^^^^^^^^^^^^^^^^

``M.N.x`` looks ambiguous. It could mean:

- ``(M.N).x`` that is, select the ``x`` field from the (presumably nullary) data constructor ``M.N``, or
- The qualifed name ``M.N.x``, meaning the ``x`` imported from ``M.N``.

The ambiguity is resolved in favor of ``M.N.x`` as a qualified name.
If the other interpretation is desired you can still write ``(M.N).x``

We propose that ``.`` “bind more tightly” than function application
thus, ``f r.a.b`` parses as ``f (r.a.b)``.

============== ===================
Expression     Interpretation
============== ===================
``f r.x``      means ``f (r.x)``
``f r .x``     is illegal
``f (g r).x``  ``f ((g r).x)``
``f (g r) .x`` is illegal
``f M.n.x``    means ``f (M.n.x)`` (that is, ``f (getField @"x" M.n)``)
``f M.N.x``    means ``f (M.N.x)`` (``M.N.x`` is a qualified name, not a record field selection)
============== ===================

2.1.3 Fields whose names are operator symbols
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We propose that dot notation isn’t available for fields whose names are
operator symbols (for example, ``+``, ``.+.`` and so on).

[Note : For fields whose names are operator symbols, one can still
write ``getField`` expressions (e.g. ``getField @".+." r``)].

2.2 Definitions
~~~~~~~~~~~~~~~

For what follows, we use these informal definitions:

* A **field selector** is an expression like ``.a`` or ``.a.b``;
* A **field selection** is an expression like ``r.a`` or ``(f x).a.b``;
* A **field update** is an expression like ``r{a = 12}`` or ``r{a.b = "foo"}``;
* A **punned field update** is an expression like ``r{a}`` or ``r{a.b}`` (here it is understood that ``b`` is a variable bound in the environment of the expression and only valid syntax if the ``NamedFieldPuns`` language extension is in effect).

2.3 Lexing and Parsing
~~~~~~~~~~~~~~~~~~~~~~

The prototype implements the parsing scheme presented here. More
information about the prototype is available in `this
section <#91-prototype>`__.

2.3.1 Lexer
^^^^^^^^^^^

A new token case ``ITproj Bool`` is introduced. When the
``OverloadedRecordDot`` extension is enabled occurences of operator
``.`` not as part of a qualified name are classified using the
whitespace sensitive operator mechanism from `this (accepted) GHC
proposal <https://github.com/ghc-proposals/ghc-proposals/pull/229>`__.
The rules are:

=========== ================ ==================== =========
Occurence   Token            Means                Example
=========== ================ ==================== =========
prefix      ``ITproj True``  field selector       ``.x``
tight infix ``ITproj False`` field selection      ``r.x``
suffix      ``ITdot``        function composition ``f. g``
loose infix ``ITdot``        function composition ``f . g``
=========== ================ ==================== =========

No ``ITproj`` tokens will ever be issued if ``OverloadedRecordDot`` is
not enabled.

2.3.2 Parsing
^^^^^^^^^^^^^

The Haskell grammar is extended with the following productions. We use
these notations:

====== ===========
Symbol Occurence
====== ===========
*.ᴾ*   prefix
*.ᵀ*   tight-infix
====== ===========

2.3.2.1

.. role:: raw-html(raw)
    :format: html

[Field]
:raw-html:`<br />`
     *field*   →   *varid*

.. _section-1:

2.3.2.2


[Field to update]
:raw-html:`<br />`
     *fieldToUpdate*   →   *fieldToUpdate* *.ᵀ* *field*   |   *field*

.. _section-2:

2.3.2.3


[Field selectors]
:raw-html:`<br />`
     *aexp*   →   *( projection )*
:raw-html:`<br />`
     *projection*   →   *.ᴾ* *field*   |   *projection* *.ᵀ* *field*

.. _section-3:

2.3.2.4


[Field selection]
:raw-html:`<br />`
     *fexp*   →   *fexp* *.ᵀ* *field*

.. _section-4:

2.3.2.5


[Field update]
:raw-html:`<br />`
     *fbind*   →    *field* *.ᵀ* *fieldToUpdate* *=* *exp*
:raw-html:`<br />`
     *fbind*   →   *field* *.ᵀ* *fieldToUpdate*

3. Examples
-----------

This is a record type with functions describing a study ``Class`` (*Oh!
Pascal, 2nd ed. Cooper & Clancy, 1985*).

.. code:: haskell

   data Grade = A | B | C | D | E | F
   data Quarter = Fall | Winter | Spring
   data Status = Passed | Failed | Incomplete | Withdrawn

   data Taken =
     Taken { year :: Int
           , term :: Quarter
           }

   data Class =
     Class { hours :: Int
           , units :: Int
           , grade :: Grade
           , result :: Status
           , taken :: Taken
           }

   getResult :: Class -> Status
   getResult c = c.result -- get

   setResult :: Class -> Status -> Class
   setResult c r = c{result = r} -- update

   setYearTaken :: Class -> Int -> Class
   setYearTaken c y = c{taken.year = y} -- nested update

   getResults :: [Class] -> [Status]
   getResults = map (.result) -- selector

   getTerms :: [Class]  -> [Quarter]
   getTerms = map (.taken.term) -- nested selector

Further examples `accompany the
prototype <https://gitlab.haskell.org/shayne-fletcher-da/ghc/-/blob/f74bb04d850c53e4b35eeba53052dd4b407fd60b/record-dot-syntax-tests/Test.hs>`__
and yet more (as tests) are available in the examples directory of `this
repository <https://github.com/ndmitchell/record-dot-preprocessor>`__.
Those tests include infix applications, polymorphic data types,
interoperation with other extensions and more.

4. Effect and Interactions
--------------------------

**Polymorphic updates:** When enabled, this extension takes the
``a{b=c}`` syntax and uses it to mean ``setField``. The biggest
difference a user is likely to experience is that the resulting type of
``a{b=c}`` is the same as the type ``a`` - you *cannot* change the type
of the record by updating its fields. The removal of polymorphism is
considered essential to preserve decent type inference, and is the only
option supported by `the HasField proposal <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst>`__.
Anyone wishing to use polymorphic updates can write
``let Foo{..} = a in Foo{polyField=[], ..}`` instead.

**Higher-rank fields:** It is impossible to express ``HasField``
instances for data types such as
``data T = MkT { foo :: forall a . a -> a}``, which means they can’t
have this syntax available. Users can still write their own selector
functions using record puns if required. There is a possibility that
with future types of impredicativity such ``getField`` expressions could
be solved specially by the compiler.

**Lenses and a.b syntax:** The ``a.b`` syntax is commonly used in
conjunction with the ``lens`` library, e.g. \ ``expr^.field1.field2``.
Treating ``a.b`` without spaces as a record projection would break such
code. The alternatives would be to use a library with a different lens
composition operator (e.g. ``optics``), introduce an alias in ``lens``
for ``.`` (perhaps ``%``), write such expressions with spaces, or not
enable this extension when also using lenses. While unfortunate, we
consider that people who are heavy users of lens don’t feel the problems
of inadequate records as strongly, so the problems are lessened. In
addition, it has been discussed
(e.g. `here <https://github.com/ghc-proposals/ghc-proposals/pull/282#issuecomment-546159561>`__),
that this proposal is complimentary to lens and can actually benefit
lens users (as with ``NoFieldSelectors`` one can use the same field
names for everything: dot notation, lens-y getting, lens-y modification,
record updates, ``Show/Generic``).

**Rebindable syntax:** When ``RebindableSyntax`` is enabled the
``getField`` and ``setField`` functions are those in scope, rather than
those in ``GHC.Records``. The ``.`` function (as used in the ``a.b.c``
desugaring) remains the ``Prelude`` version (we see the ``.`` as a
syntactic shortcut for an explicit lambda, and believe that whether the
implementation uses literal ``.`` or a lambda is an internal detail).

**Enabled extensions:** The extensions do not imply enabling/disabling
any other extensions. It is often likely to be used in conjunction
with either the ``NoFieldSelectors`` extension or\
``DuplicateRecordFields``.

5. Costs and Drawbacks
----------------------

The implementation of this proposal adds code to the compiler, but not a
huge amount. Our `prototype <#91-prototype>`__ shows the essence of the
parsing changes, which is the most complex part.

If this proposal becomes widely used then it is likely that all Haskell
users would have to learn that ``a.b`` is a record field selection.
Fortunately, given how popular this syntax is elsewhere, that is
unlikely to surprise new users.

This proposal advocates a different style of writing Haskell records,
which is distinct from the existing style. As such, it may lead to the
bifurcation of Haskell styles, with some people preferring the lens
approach, and some people preferring the syntax presented here. That is
no doubt unfortunate, but hard to avoid - ``a.b`` really is ubiquitous
in programming languages. We consider that any solution to the records
problem *must* cause some level of divergence, but note that this
mechanism (as distinct from some proposals) localises that divergence in
the implementation of a module - users of the module will not know
whether its internals used this extension or not.

The use of ``a.b`` with no spaces on either side can make it harder to
write expressions that span multiple lines. To split over two lines it
is possible to use the ``&`` function from ``Base`` or do either of:

::

   (myexpression.field1.field2.field3
       ).field4.field5

   let temp = myexpression.field1.field2.field3
   in temp.field4.field5

6. Alternatives to this proposal
--------------------------------

Instead of this proposal, we could do any of the following:

- Using the `lens library
  <https://hackage.haskell.org/package/lens>`__. While lenses help
  both with accessors and overloaded names (e.g. ``makeFields``), one
  still needs to use one of the techniques mentioned below (or
  similar) to work around the problem of duplicate name selectors. In
  addition, lens-based syntax is more verbose, e.g. \ ``f $ record
  ^. field`` instead of possible ``f record.field``. More importantly,
  while the concept of lenses is very powerful, that power can be
  `complex to use
  <https://twitter.com/fylwind/status/549342595940237312?lang=en>`__,
  and for many projects that complexity is undesirable. In many ways
  lenses let you abstract over record fields, but Haskell has
  neglected the “unabstracted” case of concrete fields. Moreover, as
  it has been `previously mentioned <#Effect-and-Interactions>`__,
  this proposal is orthogonal to lens and can actually benefit lens
  users.
-  The `DuplicateRecordFields
   extension <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#duplicate-record-fields>`__
   is designed to solve similar problems. We evaluated this extension as
   the basis for DAML, but found it lacking. The rules about what types
   must be inferred by what point are cumbersome and tricky to work
   with, requiring a clear understanding of at what stage a type is
   inferred by the compiler.
-  Some style guidelines mandate that each record should be in a
   separate module. That works, but then requires qualified modules to
   access fields - e.g. \ ``Person.name (Company.owner c)``. Forcing the
   structure of the module system to follow the records also makes
   circular dependencies vastly more likely, leading to complications
   such as boot files that are ideally avoided.
-  Some style guidelines suggest prefixing each record field with the
   type name, e.g. \ ``personName (companyOwner c)``. While it works, it
   isn’t pleasant, and many libraries then abbreviate the types to lead
   to code such as ``prsnName (coOwner c)``, which can increase
   confusion.
-  There is a `GHC plugin and
   preprocessor <https://github.com/ndmitchell/record-dot-preprocessor>`__
   that both implement much of this proposal. While both have seen light
   use, their ergonomics are not ideal. The preprocessor struggles to
   give good location information given the necessary expansion of
   substrings. The plugin cannot support the full proposal and leads to
   error messages mentioning ``getField``. Suggesting either a
   preprocessor or plugin to beginners is not an adequate answer. One of
   the huge benefits to the ``a.b`` style in other languages is support
   for completion in IDE’s, which is quite hard to give for something
   not actually in the language.
-  Continue to
   `vent <https://www.reddit.com/r/haskell/comments/vdg55/haskells_record_system_is_a_cruel_joke/>`__
   `about <https://web.archive.org/web/20210504193320/https://bitcheese.net/haskell-sucks>`__
   `records <https://medium.com/@snoyjerk/least-favorite-thing-about-haskal-ef8f80f30733>`__
   `on <https://www.quora.com/What-are-the-worst-parts-about-using-Haskell>`__
   `social <http://www.stephendiehl.com/posts/production.html>`__
   `media <https://www.drmaciver.com/2008/02/tell-us-why-your-language-sucks/>`__.

All these approaches are currently used, and represent the “status quo”,
where Haskell records are considered not fit for purpose.

7. Alternatives within this proposal
------------------------------------

7.1 Should the extensions imply ``NoFieldSelectors`` or another extension?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Typically the extensions will be used in conjunction with
``NoFieldSelectors``, but ``DuplicateRecordFields`` would work too. Of
those two, ``DuplicateRecordFields`` complicates GHC, while
``NoFieldSelectors`` conceptually simplifies it, so we prefer to bias
the eventual outcome. However, there are lots of balls in the air, and
enabling the extensions should ideally not break normal code, so
we leave everything distinct (after `being convinced
<https://github.com/ghc-proposals/ghc-proposals/pull/282#issuecomment-547641588>`__).

7.2 Should a syntax be provided for modification?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Earlier versions of this proposal contained a modify field syntax of the
form ``a{field * 2}``. While appealing, there is a lot of syntactic
debate, with variously ``a{field <- (*2)}``, ``a{field * = 2}`` and
others being proposed. None of these syntax variations are immediately
clear to someone not familiar with this proposal. To be conservative, we
leave this feature out.

7.3 Should there be update sections?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are no update sections. Should ``({a=})``, ``({a=b})`` or
``(.lbl=)`` be an update section? While nice, we leave this feature out.

7.4 Should pattern matching be extended?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We do not extend pattern matching, although it would be possible for
``P{foo.bar=Just x}`` to be defined.

7.5 Will whitespace sensitivity become worse?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We’re not aware of qualified modules giving any problems, but it’s
adding whitespace sensitivity in one more place.

7.6 Should a new update syntax be added?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One suggestion is that record updates remain as normal, but
``a { .foo = 1 }`` be used to indicate the new forms of updates. While
possible, we believe that option leads to a confusing result, with two
forms of update both of which fail in different corner cases. Instead,
we recommend use of ``C{foo}`` as a pattern (with ``-XNamedFieldPuns``)
to extract fields if necessary.

7.7 Why two extensions and not just one?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Things we could have done instead:

1. Add two extensions, as proposed here.

- **Pro**: flexibility for people who want type-changing update, but would still like dot-notation. Breaking back on type-changing update, like ``OverloadedRecordUpdate`` does, has proved to be controversial, and we don’t want it to hold back the integration of this proposal in GHC.
- **Pro**: orthogonal things are controlled by separate flags.
- **Con**: each has to be documented separately: two flags with one paragraph each, instead of one flag with two paragraphs. (The implementation cost is zero: it's only a question of which flag to test.)
2. Add a single extension (``OverloadedRecordFields``, say) to do what ``OverloadedRecordDot`` and ``OverloadedRecordUpdate`` do in this proposal.

- **Pro**: only one extension.
- **Con**: some users might want dot-notation, but not want to give up type-changing update.
3. Make this modification a no-op, doing nothing. Instead adopt precisely the previous proposal. Use ``RecordDotSyntax`` as the extension, covering both record dot and update.  However, we should then be prepared to change what ``RecordDotSyntax`` means later.  In particular, it is very likely that we’ll want ``RecordDotSyntax`` to imply ``NoFieldSelectors``.

- **Pro**: only one extension
- **Con**:  changing the meaning of an extension will break programs.
4. Use ``RecordDotSyntax``, just as in the original proposal, but add ``NoFieldSelectors`` immediately

- **Con**: it’s too early to standardize this, we’re not really sure that it’s what we want (e.g. we may want ``DuplicatRecordFields`` instead).

NB: the difference between (2) and (3) is tiny: only whether we have ``OverloadedRecordFields`` now and ``RecordDotSyntax`` later; or ``RecordDotSyntax`` now and <something else> later.



7.8 Why not make ``RecordDotSyntax`` part of this proposal?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We think ``RecordDotSyntax`` will enable these extensions plus some
extension that allows multiple field names, e.g. ``NoFieldSelectors``.
Which final extension that is has not yet been determined.

8. Unresolved issues
--------------------

None.

9. Implementation Plan
----------------------

9.1 Prototype
~~~~~~~~~~~~~

To gain confidence these changes integrate as expected `a
prototype <https://gitlab.haskell.org/shayne-fletcher-da/ghc/-/tree/record-dot-syntax-4.1>`__
was produced that parses and desugars forms directly in the parser. For
confirmation, we *do not* view desugaring in the parser as the correct
implementation choice, but it provides a simple mechanism to pin down
the changes without going as far as adding additional AST nodes or type
checker rules. The prototype was rich enough to “do the right thing”. Update
July 2021: More tests are now available in the GHC tree, e.g.
`RecordDotSyntax1.hs
<https://gitlab.haskell.org/ghc/ghc/-/blob/master/testsuite/tests/parser/should_run/RecordDotSyntax1.hs>`__.

9.2 Who will provide an implementation?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If accepted, the proposal authors would be delighted to provide an
implementation. Implementation depends on the implementation of `the
HasField proposal
<https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst>`__.
