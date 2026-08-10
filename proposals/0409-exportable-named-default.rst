Generalized, named, and exportable ``default`` declarations
===========================================================

.. author:: Mario Blažević
.. date-accepted:: 2021-09-30
.. ticket-url:: https://gitlab.haskell.org/ghc/ghc/-/issues/24305
.. implemented:: 9.12
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/409>`_ and `amended by this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/672>`_.
.. sectnum::
.. contents::

The ``default`` declaration as specified in Haskell 2010 report is very limited. This proposal aims to make it useful
while keeping backward compatibility. Another goal of the proposal is to leave the ``default`` declaration in the deep
language background, in a manner of speaking, so that expert users can apply it to help beginners who can remain
blissfully unaware of it.

Motivation
----------

Section `4.3.4 <https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-790004.3.4>`_ of the Haskell 2010
language report specifies the behaviour of the ``default`` declaration. The specification sharply limits the
declaration, as it applies

- only within the current module,

- only to the class ``Num``.

The first limitation means that every user is forced to repeat the same declaration in every module if they should try
to use a non-standard numeric data type in an ambiguous way.

The reason for the latter limitation is that the numeric literals are the only literals with ambiguous types in the
language report. Since then, however, the ``OverloadedStrings`` and ``OverloadedLists`` language extensions have made
more syntactic constructs ambiguous. The former in particular is commonly used for more convenient coding of ``Text``
literals. Another potential source of ambiguity are the Prelude and the core libraries which are slowly evolving to be
more generalized.

Proposed Change Specification
-----------------------------

The present proposal is basically an expanded version of the earlier `name the class
<https://prime.haskell.org/wiki/Defaulting#Proposal1-nametheclass>`_ Haskell Prime proposal. It is also a slightly
amended version of `a proposal <https://github.com/haskell/rfcs/pull/18>`_ for the ill-fated *Haskell 2020*
language stadardization attempt.

The Haskell 2010 language report specifies the following syntax for the ``default`` declaration:

|    \ *topdecl* → ``default`` (*qtycon*\ `1`:subscript: , … , *qtycon*\ `n`:subscript:) (n ≥ 0)

where each type *qtycon*\ `i`:subscript: must be an instance of class ``Num``.

Naming the class
~~~~~~~~~~~~~~~~

In the current language standard, the ``default`` declaration implicitly applies to class ``Num`` only. The proposal is
to make this class explicit, so the syntax becomes

|    \ *topdecl* → ``default`` *qtycls*? (*qtycon*\ `1`:subscript: , … , *qtycon*\ `n`:subscript:) (n ≥ 0)

where each type *qtycon*\ `i`:subscript: must be an instance of the specified class *qtycls*. The types may belong to
any kind, but the class must have a single parameter.

If no class is specified, the earlier default of ``Num`` is assumed. In other words, the Haskell '98 syntax of

::

   default (Int, Float)

would mean exactly the same as

::

   default Num (Int, Float)

This syntactic extension would be enabled by a new ``{-# LANGUAGE NamedDefaults #-}`` pragma.

Exporting the defaults
~~~~~~~~~~~~~~~~~~~~~~

Another thing the current report specifies is that the declaration applies only within the current module. This
proposal does not modify that behaviour: a ``default`` declaration by itself does not apply outside its module. That
is the purpose of another extension to the module export list. To the existing syntax


|   \ *export* → *qvar*
|              | *qtycon* [(..) | ( *cname*\ `1`:subscript: , … , *cname*\ `n`:subscript: )]  (n ≥ 0)
|              | *qtycls* [(..) | ( *var*\ `1`:subscript: , … , *var*\ `n`:subscript: )] 	  (n ≥ 0)
|              | ``module`` *modid*
|
| would be added another alternative
|
|  \ 
|              | ``default`` *qtycls*

The effect of the new alternative would be to export the default declaration that is in effect in the module for the
named class *qtycls*. This can mean either that it's declared in the same module or that it's imported from another
module.

When exporting a ``default Num`` declaration, the class ``Num`` has to be explicitly named like any other class.

An ``import`` of a module always imports all the ``default`` declarations listed in the module's export list. There is
no way to exclude any of them. This is the default option for this proposal, but there are `alternatives`_.

A module can export its ``default`` only by specifying them explicitly in its export list using the above syntax
extension. In particular, module with no explicit export list (as in ``module M where {...}``) does *not* export any
``default`` declarations, and neither does the re-export of a whole module (as in ``module M (module N) where{...}``,
regardless of whether *N* and *M* are the same or different modules).

The syntactic extension to exports would be enabled by the same ``{-# LANGUAGE NamedDefaults #-}`` pragma. The new
semantics of imports would be enabled by default with no ``LANGUAGE`` extension required.

`WARNING` pragma on export
++++++++++++++++++++++++++

As with regular export items, the user can attach a ``WARNING`` pragma to an export of a default: ::

  {-# LANGUAGE NamedDefaults #-}
  module M ({-# WARNING "This default is deprecated, use explicit type applications" #-} default MyClass)

The warning would be triggered only if an importer actually uses the default to disambiguate a type. In other words,
the pragma would replace a generic compiler warning about type defaults, enabled by ``-Wtype-defaults``, with a
specific warning. The category of the warning is ``-Wdeprecations`` by default, but the pragma may also specify a
user-defined warning category, as in::

  {-# WARNING in "x-ambiguous-types" "Your code depends on defaults for disambiguation" #-}

As usual, the ``WARNING`` pragma with no explicit class can be replaced with a ``DEPRECATED`` pragma that has the same
effect.

Subsumption
~~~~~~~~~~~

Definition: given two ``default`` declarations for the same class
   
   |      ``default`` *C*  (*Type*\ `1`:subscript:\ `a`:superscript: , … , *Type*\ `m`:subscript:\ `a`:superscript:)
   |      ``default`` *C*  (*Type*\ `1`:subscript:\ `b`:superscript: , … , *Type*\ `n`:subscript:\ `b`:superscript:)

if *m* ≤ *n* and the first type sequence *Type*\ `1`:subscript:\ `a`:superscript: , … , *Type*\ `m`:subscript:\
`a`:superscript: is a sub-sequence of the second sequence *Type*\ `1`:subscript:\ `b`:superscript: , … , *Type*\
`n`:subscript:\ `b`:superscript: (*i.e.*, the former can be obtained by removing a number of *Type*\ `i`:subscript:\
`b`:superscript: items from the latter), we say that the second declaration *subsumes* the first one.


Rules for disambiguation of multiple declarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Only a single ``default`` declaration can be in effect in any single module for any particular class. If there is more
than one ``default`` declaration in scope, the conflict is resolved using the following rules:

1. Two declarations for two different classes are not considered to be in conflict; they can, however, clash at a
   particular use site as we'll see in the following section.
2. Two declarations for the same class explicitly declared in the same module are considered a static error.
3. A ``default`` declaration in a module takes precedence over any imported ``default`` declarations for the same
   class. However the compiler may warn the user if an imported declaration is not subsumed by the local declaration.
4. For any two imported ``default`` declarations for the same class where one subsumes the other, we ignore the
   subsumed declaration.
5. If a class has neither a local ``default`` declaration nor an imported ``default`` declaration that subsumes all
   other imported ``default`` declarations for the class, the conflict between the imports is unresolvable. The effect
   is to ignore all ``default`` declarations for the class, so that no declaration is in effect in the module. The
   compiler may choose to emit a warning in this case, but no error would be triggered about the imports. Of course an
   error may be triggered in the body of the module if it contains an actual ambiguous type for the class with the
   conflicting imported defaults, as per the following subsection.

Any warnings issued in the situations listed above would be in the ``-Wtype-defaults`` category.

As a result, in any module each class has either one default declaration in scope (a locally-declared one, or an
imported one that subsumes all other imported ones), or none. This single default is used to resolve ambiguity, as
described in the next subsection.

Note that a ``default`` declaration that repeats a type name more than once is perfectly valid, and sometimes may
be necessary to resolve coflicts. For example, a module that imports two conflicting defaults

::

   default C (Int, Bool)

and
   
::

   default C (Bool, Int)

may use a local declaration

::

   default C (Int, Bool, Int)

to override the imports. Because this declaration subsumes both imported defaults it will not trigger any compiler
warning. When used to resolve ambiguity (next section) it behaves exactly like ``default C( Int, Bool)``; that is, the
repeats can be discarded.

   
Rules for disambiguation at the use site
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The disambiguation rules are a conservative extension of the existing rules in Haskell 2010, which state that
ambiguous type variable *v* is defaultable if:

    - *v* appears only in constraints of the form *C* *v*, where *C* is a class, and

    - at least one of these classes is a numeric class, (that is, ``Num`` or a subclass of ``Num``), and

    - all of these classes are defined in the Prelude or a standard library.

    Each defaultable variable is replaced by the first type in the default list that is an instance of all the
    ambiguous variable’s classes. It is a static error if no such type is found.

The new rules instead require only that 

- *v* appears in at least one constraint of the form *C* *v*, where *C* is a single-parameter class.

Informally speaking, the type selected for defaulting is the first type from the ``default`` list for class *C* that
satisfies all constraints on type variable *v*. If there are multiple *C*\ `i`:subscript: *v* constraints with
competing ``default`` declarations, they have to resolve to the same type.

To make the design more explicit, the following algorithm *can* be used for default resolution, but any other method
that achieves the same effect can be substitued:

Let *S* be the complete set of unsolved constraints, and initialize *S*\ `x`:subscript: to an empty set of constraints.
For every *v* that is free in *S*:

1. Define *C*\ `v`:subscript: = { *C*\ `i`:subscript: v | *C*\ `i`:subscript: v ∈ *S* }, the subset of
   *S* consisting of all constraints in *S* of form (*C*\ `i`:subscript: v), where *C*\ `i`:subscript: is a
   single-parameter type class.
2. Define *D*\ `v`:subscript:, by extending *C*\ `v`:subscript: with the superclasses of every *C*\ `i`:subscript: in
   *C*\ `v`:subscript:
3. Define *E*\ `v`:subscript:, by filtering *D*\ `v`:subscript: to contain only classes with a default declaration.
4. For each *C*\ `i`:subscript: in *E*\ `v`:subscript:, find the first type *T* in the default list for
   *C*\ `i`:subscript: for which, for every (*C*\ `i`:subscript: v) in *C*\ `v`:subscript:, the constraint
   (*C*\ `i`:subscript: *T*) is soluble.
5. If there is precisely one type *T* in the resulting type set, resolve the ambiguity by adding a ``v
   ~ T``\ `i`:subscript: constraint to a set *S*\ `x`:subscript:; otherwise report a static error.

Examples
--------

The main motivation for expanding the ``default`` rules is the widespread use of the ``OverloadedStrings`` language
extension, usually for the purpose of using the ``Text`` data type instead of ``String``.

With this proposal in effect, and some form of ``FlexibleInstances``, the Haskell Prelude could export the declarations

::

   default IsString (String)
   default IsList ([])

Then a user module could activate the ``OverloadedStrings`` or ``OverloadedLists`` extension without triggering any
ambiguous type errors, still using the ``String`` and list type from the Prelude.

The authors of the alternative string implementations like ``Text`` would export the following declaration instead::

   default IsString (Text, String)

Any user module that activates the ``OverloadedStrings`` extension and imports ``Data.Text`` would thus obtain the
default declaration suitable for working with ``Text`` without any extra effort. Since the Prelude declaration's list
of types is a sub-sequence of the latter declarations, it would be subsumed by it.

A user module could, by chance or by design, import two independently-developed modules that export competing defaults
for the same class, for example the previous ``Text`` module and the ``Foundation.String`` module with its own
exported declaration ::

   default IsString (Foundation.String, String)

In this case the importing module would discard both contradictory declarations. If the developers desire a particular
default, they just have to declare it in the importing module. Furthermore, if they export this ``default``
declaration, every importer of the module will have the conflicts resolved for them::

   module ProjectImports (Text.Text, Foundation.String,
                          default IsString)

   import qualified Data.Text         as Text
   import qualified Foundation.String as Foundation

   default IsString (Text.Text, Foundation.String, String)

An equivalent story can be told for the ``OverloadedLists``, by replacing ``Text`` and ``Foundation.String`` by
``Vector`` and ``Foundation.String`` by ``Foundation.Array``.

Effect and Interactions
-----------------------

GHC already supports two extensions that modify the defaulting mechanism:
`ExtendedDefaultRules <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/ghci.html#type-defaulting-in-ghci>`_ and
`OverloadedStrings <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html?highlight=overloadedstrings#overloaded-string-literals>`_.

ExtendedDefaultRules
~~~~~~~~~~~~~~~~~~~~

The former is fully devoted to defaulting. Its effect is to extend the defaulting rules so that they apply not only to
the class ``Num`` as specified by the language standard, but also to any class in the following list: ``Show``,
``Eq``, ``Ord``, ``Foldable``, ``Traversable``, or any numeric class. This list is hard-coded and not
user-extensible. Furthermore, the extension adds ``()`` and ``[]`` to the list of default types to try. If the present
proposal is accepted, ``ExtendedDefaultRules`` could be reformulated as a set of actual ``default`` declarations
brought into the scope::

  default Show ((), Integer, Double)
  default Eq ((), Integer, Double)
  default Ord ((), Integer, Double)
  default Foldable ([])
  default Traversable ([])
  default Num ((), Integer, Double)

OverloadedStrings
~~~~~~~~~~~~~~~~~

The ``OverloadedStrings`` extension by itself causes many new ambiguities, much like the ambiguites caused by the
overloaded numeric literals which were the original reason for ``default`` declarations in the first place. To rectify
this problem, the extension tweaks the defaulting mechanism. To quote from the GHC manual:

- Each type in a ``default`` declaration must be an instance of ``Num`` or of ``IsString``.

- If no ``default`` declaration is given, then it is just as if the module contained the declaration ``default
  (Integer, Double, String)``.

- The standard defaulting rule is extended thus: defaulting applies when all the unresolved constraints involve
  standard classes or ``IsString``; and at least one is a numeric class or ``IsString``.

Once again, if the present proposal were adopted, the above rules could be expressed as an actual ``default``
declaration::

   default IsString (Integer, Double, String)

OverloadedLists
~~~~~~~~~~~~~~~

The ``OverloadedLists`` extension does not currently bring any defaulting rules into scope. There is no need to change
that. Once this proposal is adopted, a library like ``Vector`` could export a rule::

  default IsList ([], Vector)

ParallelListComp, TransformListComp, and MonadComprehensions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The same consideration could be extended to the ``ParallelListComp``, ``TransformListComp``, and
``MonadComprehensions`` extensions. None of them bring any special defaulting rules. The desugaring of the first two
extensions on their own seems to be hard-wired to list-specific functions like ``zip``. This means that their use
effectively neutralizes ``OverloadedLists``. When combined with the ``MonadComprehensions`` extension, the
``ParallelListComp`` extension is generalized to target any ``MonadZip`` instance, but ``TransformListComp`` is
not. To target a type other then ``[]``, GHC Users Guide instead suggests the combination of three extensions::

  {-# LANGUAGE TransformListComp, MonadComprehensions, RebindableSyntax #-}

There is some opportunity here for the expanded use of the present proposal, but the backward compatibility is
sufficiently messy for me to refrain from making any suggestions. The extensions are also fairly old and not
particularly popular, so they may be best left alone.

Costs and Drawbacks
-------------------

Use-site conflicts
~~~~~~~~~~~~~~~~~~

The earlier `Haskell Prime proposal <https://prime.haskell.org/wiki/Defaulting>`_ notes several ways in which defaults
for different classes can contradict each other::
   
   default A (Int,String,())
   default B (String,(),Int)
   (A t, B t) => t

   default C (Int,Double,String,())
   default D (Double,String,Int,())
   (C t, D t) => t

The solution to this problem depends on where the conflicting defaults come from.

- If they are declared in the same module: just don't do that; or

- if the defaults are imported, declare one or more overriding defaults to resolve the conflict.

Alternatives
------------

Explicit ``ImportedDefaults``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Originally this proposal came with a separate ``ImportedDefaults`` extension to enable the imports of ``default``
declarations.

The proposal in its present form does not preserve full backward compatibility at the module level: it may change the
semantics of a previously valid module that was relying on the implicit ``default (Integer, Double)`` rule. It is much
more likely, however, for this extension to resolve a type ambiguity that was preventing the module to compile, so the
committee decided to just enable it by default.

Declaration imports
~~~~~~~~~~~~~~~~~~~

Most features of the present proposal are completely determined by the constraints of backward compatibility and ease of
use, but in case of declaration imports the choice was more arbitrary.

As stated above, the default option is to automatically import all ``default`` declarations the module exports, with
no choice offered to the importer. If a default is unwanted, it can easily be modified or turned off by another
``default`` declaration.

This choice has been made because it seems to be easiest on the beginners: they don't need to know anything about
defaults, especially if they work with a prepared set of imports that take care to resolve the potential ``default``
conflicts for them.

An alternative approach would be to treat default exports the same way normal named exports are treated: if an
``import`` declaration explicitly lists the names it wants to import, it has to also explicitly list ``default`` and
the class name for each desired default declaration. While this solution would probably leave the language more
consistent, it would also make its infamous learning curve even steeper for beginners.

An optional extension compatible with either of these alternatives would be to allow the ``hiding`` clause to list the
``default`` declarations that should not be brought into the scope. This is not a part of the present proposal simply
because it's unnecessary.

Module re-exports
~~~~~~~~~~~~~~~~~

As proposed in the `Exporting the defaults`_ section, a re-export of a whole module would not export the ``default``
declarations imported from that module. The reasoning behind this constraint was to prevent a module from exporting a
conflicting set of declarations without also exporting a local subsuming declaration, as in this example::

   module M( f, g, module A, module B ) where
     import A   -- Say A exports default X( P, R )
     import B   -- Say B exports default X( Q, R )
     default X( P, Q, R )

The alternative would be to simplify the semantics and have ``module A, module B`` re-export export everything
including the conflicting ``default`` declarations. The compiler could warn the author that the lack of an export of a
subsuming declaration makes life harder for the module's importers.

Global coherence
~~~~~~~~~~~~~~~~

A proposal was put forward to treat ``default`` declarations the same way as ``instance`` declarations, *i.e.*, to
always export and import them and to insist on their global coherence. In some ways this is easier in case of
``default`` declarations, because coherence can always be recovered by adding a new ``default`` declaration that
subsumes all conflicting declarations for the same class. For example if any two modules contain two conflicting
declarations from above::

   default C (Int,Double,String,())
   default D (Double,String,Int,())

any third (presumably higher-level) module can recover the coherence and resolve the conflict in favour of the first
module by declaring::

   default C (Int,Double,String,(),Int,())

Both old declarations are subsumed by the new one. However there would be no way to simply turn off a ``default``
declaration within a module. Besides, ``default`` coherence wouldn't bring any benefits it does to ``instance``
declarations.


Multi-parameter type classes and other constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This proposal does not cover MPTCs nor type equality constraints, but this section will speculate how it could be
extended to cover them in future.

First, let us generalize the single-parameter type class defaults by expanding the class name and each type name to
full constraints. The above example

::
   
   default IsString (Text, String)

would then be written as

::
   
   default IsString t => (t ~ Text, t ~ String)

The former notation would be syntactic sugar for the latter. Since comma is already used as a constraint combinator,
we'd actually prefer to replace it by something else. The logical choice would be semicolon, which always appears
inside braces in the rest of the language::
  
   default IsString t => {t ~ Text; t ~ String}

So now we have a general enough notation to accommodate MPTCs. We could, for example, say

::
  
   default HasKey m k => {m ~ IntMap v, k ~ Int;
                          m ~ Map k v;
                          m ~ [(k, v)];
                          m ~ Map k v, k ~ String}

The defaulting algorithm would replace the constraint on the left hand side consecutively by each semicolon-separated
constraint group on the right-hand side until it finds one that completely resolves the ambiguity.

Again, this extension is not a part of the proposal because it would depend on type equality at least, and because its
utility is unproven. Still, it's good to know that the proposal does not close off this potentially important
development direction.
