Generalized, named, and exportable ``default`` declarations
===========================================================

.. author:: Mario Blažević
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/409>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
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
amended version of `a proposal<https://github.com/haskell/rfcs/pull/18>`_ for the ill-fated *Haskell 2020*
language stadardization attempt.

The Haskell 2010 language report specifies the following syntax for the ``default`` declaration:

|    \ *topdecl* → ``default`` (*qtycon*\ `1`:subscript: , … , *qtycon*\ `n`:subscript:) (n ≥ 0)

where each type *qtycon*\ `i`:subscript: must be an instance of class ``Num``.

Naming the class
~~~~~~~~~~~~~~~~

In the current language standard, the ``default`` declaration implicitly applies to class ``Num`` only. The proposal is
to make this class explicit, so the syntax becomes

|    \ *topdecl* → ``default`` *qtycls*? (*qtycon*\ `1`:subscript: , … , *qtycon*\ `n`:subscript:) (n ≥ 0)

where each type *qtycon*\ `i`:subscript: must be an instance of the specified class *qtycls*. If no class is
specified, the earlier default of ``Num`` is assumed.

The types may belong to any kind, but the class must have a single parameter.

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

The syntactic extension to exports would be enabled by a new ``{-# LANGUAGE ExportedDefaults #-}`` pragma, which would
imply the aforementioned ``NamedDefaults`` pragma. The new semantics of imports would be enabled by a new ``{-#
LANGUAGE ImportedDefaults #-}`` pragma.

Rules for disambiguation of multiple declarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Only a single ``default`` declaration can be in effect in any single module for any particular class. If there is more
than one ``default`` declaration in scope, the conflict is resolved using the following rules:

1. Two declarations for two different classes are not considered to be in conflict; they can, however, clash at a
   particular use site as we'll see in the following section.
2. Two declarations for the same class explicitly declared in the same module are considered a static error.
3. A ``default`` declaration in a module takes precedence over any imported ``default`` declarations for the same
   class.
4. For any two imported ``default`` declarations for the same class
   
   |      ``default`` *C*  (*Type*\ `1`:subscript:\ `a`:superscript: , … , *Type*\ `m`:subscript:\ `a`:superscript:)
   |      ``default`` *C*  (*Type*\ `1`:subscript:\ `b`:superscript: , … , *Type*\ `n`:subscript:\ `b`:superscript:)

   if *m* ≤ *n* and the first type sequence *Type*\ `1`:subscript:\ `a`:superscript: , … , *Type*\ `m`:subscript:\
   `a`:superscript: is a sub-sequence of the second sequence *Type*\ `1`:subscript:\ `b`:superscript: , … , *Type*\
   `n`:subscript:\ `b`:superscript: (*i.e.*, the former can be obtained by removing a number of *Type*\
   `i`:subscript:\ `b`:superscript: items from the latter), we say that the second declaration *subsumes* the first
   one. The effect is to ignore the subsumed first declaration.
5. If a class has neither a local ``default`` declaration nor an imported ``default`` declaration that subsumes all
   other imported ``default`` declarations for the class, the conflict between the imports is unresolvable. The effect
   is to ignore all ``default`` declarations for the class, so that no declaration is in effect in the module. The
   compiler may choose to emit a warning in this case, but no error would be triggered unless there is an actual
   ambiguous type in the module.

Rules for disambiguation at the use site
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The disambiguation rules are a conservative extension of the existing rules in Haskell 2010, which state that
ambiguous type variable *v* is defaultable if:

    - *v* appears only in constraints of the form *C* *v*, where *C* is a class, and

    - at least one of these classes is a numeric class, (that is, ``Num`` or a subclass of ``Num``), and

    - all of these classes are defined in the Prelude or a standard library.

    Each defaultable variable is replaced by the first type in the default list that is an instance of all the
    ambiguous variable’s classes. It is a static error if no such type is found.

The new rules require instead that 

- *v* appears only in constraints of the form *C* *v*, where *C* is a class, and

- there is a ``default`` declaration in effect for at least one of these classes or their super-classes.

The type selection process remains the same for any given class *C*. If there are multiple *C*\ `i`:subscript: *v*
constraints with competing ``default`` declarations, they have to resolve to the same type. In other words, the type
selected for defaulting has to be the first type that satisfies all the class constraints, in every ``default``
declaration in effect. It is a static error for different ``default`` declarations to resolve to different types, or
for any of them to not resolve to any type.

To make the design more explicit, the following algorithm *can* be used for default resolution:

0. Assuming that the type inference produces the constraint set {*C*\ `1`:subscript: *v*, … , *C*\ `n`:subscript: *v*}
   for a type variable *v*,
1. add all super-classes of every *C*\ `i`:subscript: to the constraint set,
2. filter the constraint set to contain only the classes with a ``default`` declaration in effect,
3. map every found ``default`` *C*\ `i`:subscript: declaration to the first type *T*\ `i`:subscript: in its type list
   that satisfies *all* required constraints from step 0 for the ambiguous type variable *v*, and finally,
4. if there is more than one distinct type *T*\ `i`:subscript: in the resulting type set, report a static error.

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
`ExtendedDefaultRules<https://ghc.gitlab.haskell.org/ghc/doc/users_guide/ghci.html#type-defaulting-in-ghci>`_ and
`OverloadedStrings<https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html?highlight=overloadedstrings#overloaded-string-literals>`_.

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

Furthermore, as the whole purpose of this extension is to enable string literals to enable types other than
``String``, its presence should probably imply ``ImportedDefaults``.

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

Implicit ``ImportedDefaults``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The only reason this proposal adds the ``ImportedDefaults`` extension is to preserve full backward compatibility at
the module level: it may change the semantics of a previously valid module that was relying on the implicit ``default
(Integer, Double)`` rule. It is much more likely, however, for this extension to resolve a type ambiguity that was
preventing the module to compile, so one may be inclined to just enable it by default. If the proposal is adopted,
this extension would become a prime candidate for addition to the ``GHC202X`` package.

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
