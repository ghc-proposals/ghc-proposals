

Ambiguous Type per-signature pragma
===================================

.. date-accepted:: 2019-08-15
.. author:: AntC2
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/232>`_.
.. contents::


Provide a per-signature ``{-# AMBIGUOUS #-}`` pragma, to precision-control which signatures are allowed/intended to be ambiguous. This to be instead of the module-wide ``LANGUAGE AllowAmbiguousTypes`` setting. Because ambiguous signatures (at definition sites) for functions/methods are more likely to be an error than be intended -- even for code written to combine with ``-XTypeApplications`` at usage sites. Currently the ``-XAllowAmbiguousTypes`` lifts the ambiguity check indiscriminately for all signatures in a module.

The likelihood of error is made worse by GHC's current error reporting that suggests a blanket ``To defer the ambiguity check to use sites, enable AllowAmbiguousTypes``. Naive (and not-so-naive) users tend to grab for this suggestion which a) will only move the ambiguity to the usage site where it is more difficult to diagnose; and b) is liable to allow through other ambiguous signatures that were not intended. So this proposal also tightens error reporting and wording.

This proposal is a residue from discussions around 'Top-level signatures' #148. Thank you to @goldfirere and @int-index.

Motivation
------------


Ambiguous types/signatures at definition sites combine powerfully with ``TypeApplications`` at usage sites for fancy type trickery. This is an 'advanced' feature that should be opt-in. Currently ``LANGUAGE AllowAmbiguousTypes`` is a module-wide setting that abandons checking for ambiguity, whether or not the user intends to use some function/method with ``TypeApplications``.

Consider this code (a classic of its time)
::

    class Collects c e  where
      insert :: e -> c -> c
      empty :: c

GHC correctly rejects this::

    * Could not deduce (Collects c e0)
      from the context: Collects c e
        bound by the type signature for:
                   empty :: forall c e. Collects c e => c
      The type variable `e0' is ambiguous
    * In the ambiguity check for `empty'

So far so good. There are several possible approaches to avoiding this ambiguity

* Use a (Associated) Type Family to derive ``e`` from ``c``, so ``e`` can be removed as a type parameter (or constrained by an ``~`` to the TF.)
* Use a Functional Dependency.
* Use method ``singleton :: e -> c`` instead of ``empty`` with its failure to ground ``e``.

Any of those are better than what GHC suggests next::

    To defer the ambiguity check to use sites, enable AllowAmbiguousTypes

Novice users will switch on that option and get class ``Collects`` to compile. Great, thank you GHC. Except they are now in for a world of pain and confusion. At usage sites, they'll get a whole series of ``The type variable `e0' is ambiguous`` and ``Could not deduce (Collects [e1] e0)`` messages. If they persevere they might reach::

    instance Collects [e] e  where
      insert = (:)
      empty = []

    *> insert 'c' (empty :: [Char])

    * Ambiguous type variable `e0' arising from a use of `empty'
      prevents the constraint `(Collects [Char] e0)' from being solved.
      Probable fix: use a type annotation to specify what `e0' should be.

Grr. There *is* a type annotation on that ``empty``; and the ``insert 'c' ...`` is telling the type of ``e0``. Where/how on earth to specify ``e0`` otherwise? The answer (which they'll get on StackOverflow) is there's only one way: use a ``TypeApplications``. Why didn't GHC say that long ago? Why did GHC lead down the rabbit-hole of ``AllowAmbiguousTypes``? The User Guide is no help: for ``AllowAmbiguousTypes``, there's a series of examples where types at the usage site can be resolved by an annotation or a literal of specific type. There's brief mention of ``TypeApplications``. The patient helpers at StackOverflow are adept at winding users back to the definition site, and advising they probably didn't ever need to go down the rabbit-hole.

Oh, and thanks to ``AllowAmbiguousTypes`` being a module-wide setting, there's probably several other definitions in the module that didn't get ambiguity-checked. So at usage sites there's piles of these ambiguities to be unwound.

In an ideal world, that error message should probably say something long-winded to the effect: do you know what you're doing with ``TypeApplications``/is that what you intend at usage sites? Then you forgot to switch on ``AllowAmbiguousTypes`` in the definition module. (Oh, and watch out that any/all of your definitions might be ambiguous, I won't be checking them.) It should go on to say: if you don't understand what I'm talking about, probably there's a better way to avoid that ambiguity.

Then this proposal firstly aims to avoid users blundering blindly into ambiguous signatures, by improving error messages and warnings; secondly avoids the dangers of module-wide abandoning ambiguity checking.

Proposed Change Specification
-----------------------------

1. There is to be a pragma ``{-# AMBIGUOUS #-}``, to appear immediately after the ``::`` (so before the type), in principle wherever ``::`` can appear. That is a function or method definition's signature; term type annotations beginning ``::``; pattern signatures (and pattern synonyms). The "in principle" would also include ``::`` introducing kind signatures (type of types); and possibly places ready for not-yet-developed usages of type ``@`` applications. Examples::

        f :: {-# AMBIGUOUS #-} C a => Int

        class Sized a  where
          sizeOf :: {-# AMBIGUOUS #-} Integer
      
        apply (x :: {-# AMBIGUOUS #-} forall a. (Read a, Show a) => String -> String) = x @Int "01"
    
        norm :: {-# AMBIGUOUS #-} forall a. (Read a, Show a) => String -> String
        norm = show @a . read                                     -- needs type-lambda #155
    
        data T :: {-# AMBIGUOUS #-} F a -> Type                   -- ambiguous kind signature
        
   Also ``{-# AMBIGUOUS #-}`` can appear after ``::`` in a ``data`` or ``newtype`` declaration -- either for fields or GADT-style constructors; or in H2010-style immediately ~after~ before the constructor (that is, after ``=`` or ``|``, because no ``::``)::
   
        data D a where                                            -- GADT style
          MkD :: {-# AMBIGUOUS #-} (forall b. C a b => a)

        data D2 a = {-# AMBIGUOUS #-} MkD2  (forall b. C a b => a) (Show a => a) 
                                               -- H2010 style; note no ::; note the second arg is not ambiguous
                                               
        newtype N a = {-# AMBIGUOUS #-} MkN { foo :: {-# AMBIGUOUS #-} (forall b. C a b => a) } 
                                               -- AMBIGUOUS not ambiguous, but one is redundant

   
   See Appendix giving a diff to the BNF for the language (wrt the Report, and affected extensions).

2. Signatures marked ``AMBIGUOUS`` are to be validated as if ``-XAllowAmbiguousTypes`` is set, for that signature only. (If that is already set module-wide, the pragma has the effect of suppressing the ``-Wambiguous-type`` warning, see 5.)

3. This does not change the validation for ambiguous types/type variables at usage sites.

4. The error reporting from the ambiguity check that currently suggests ``To defer the ambiguity check to use sites, enable AllowAmbiguousTypes`` must make clear this is likely to entail using ``TypeApplications`` at usage sites, and that there are several possible approaches to avoid ambiguous types. (Prefer not mentioning ``AllowAmbiguousTypes`` at all.) ::
 
     If you intend to resolve the ambiguity at each use site (typically via TypeApplications), prefix this signature with the AMBIGUOUS pragma.
 
5. There is to be a flag ``-Wambiguous-types`` controlling whether a warning is raised for ambiguous types -- as allowed by ``-XAllowAmbiguousTypes``. That is:

   - If ``-XAllowAmbiguousTypes`` is not set, reject ambiguous signatures/don't also warn.
   - If ``-XAllowAmbiguousTypes`` is set and the signature is also marked ``{-# AMBIGUOUS #-}``, then don't issue the warning (see "migration path" below).
   - If a signature marked ``{-# AMBIGUOUS #-}`` is not in fact ambiguous, ignore.

   ``-Wambiguous-types`` is to be in bin-of-warnings ``-W`` "normal warnings", on grounds an ambiguous signature is outside Haskell 2010. (If some time in future, ``-XAllowAmbiguousTypes`` is to be deprecated in favour of per-signature pragmas, move ``-Wambiguous-types`` into the ``-Wcompat`` bin.)

6. The pragma can only appear with an explicit ``::`` signature; not for terms where the inferred signature is ambiguous such as toplevel functions or instances::

    data Option a
    class C a
    instance C (F b) => C (Option a)
    
   For those cases, the user must contrive an explicit signature (with ``-XInstanceSigs`` if necessary).

7. ``-XTypeApplications`` is to be changed, so that when enabled, ``f @t`` is valid regardless of whether ``t`` is ambiguous. That is, Type applications are to be allowed ambiguous signatures, no warning or error, without needing ``{-# AMBIGUOUS #-}`` (nor needing ``LANGUAGE AllowAmbiguousTypes``). For example, this is currently rejected without ``AllowAmbiguousTypes``::

        x = foo @(forall b. C a b => a) bar


Effect and Interactions
-----------------------

By lifting the ambiguity check only for signatures deliberately flagged, this ensures ambiguity checking does apply for the bulk of the signatures in the program *at the definition site*. Then ambiguity is less likely to manifest at *usage* sites, where it is more difficult to diagnose -- particularly if that is in a separate module.

The proposed behaviour affects only validation and error/warning messages, not type checking rules or type inference.

Existing code using ``AllowAmbiguousTypes`` is not affected. That is, ambiguities are not checked. The migration path away from the module-wide setting for modules with ambiguous signatures is:

* Switch on ``-Wambiguous-types``; compile the module to examine signatures that are currently ambiguous.

* If their ambiguity is expected and understood; mark as ``{-# AMBIGUOUS #-}`` (and that will suppress the warning). Otherwise diagnose and correct.

* Remove the ``LANGUAGE AllowAmbiguousTypes`` setting and recompile.

After this proposal is in place, with experience of how onerous or intrusive is the per-signature ``{-# AMBIGUOUS #-}``, a possible future migration path away from module-wide ``AllowAmbiguousTypes`` for GHC is

* In due course (not within scope of this proposal), deprecate the ``AllowAmbiguousTypes`` extension in GHC, and eventually remove it. (Same idea as introducing the ``OVERLAPPABLE`` and friends pragmas; then deprecating ``OverlappingInstances``/``IncoherentInstances``.)

Also note

* Discussion on this proposal is going on in parallel with #234 'Local Warning Pragmas'. Because ``{-# AMBIGUOUS #-}`` is a language extension (an alternative to ``-XAllowAmbiguousTypes``), not merely controlling warnings, I see this as outside the scope that #234 has evolved to. (The ``-Wambiguous-types`` point 5. warning might fall within the 'Local Warning' in the sense of #234, but note that ``{-# AMBIGUOUS #-}`` in effect is a local suppression of that warning.)


Costs and Drawbacks
-------------------

The proposal is for superficial tweaks to error reporting/warnings. There is no deep impact on type checking or inference.

For code intending to make heavy use of ``TypeApplications`` at usage sites, there may be many ambiguous signatures, needing many pragmas at definition sites that might be onerous to code. Against that, the per-signature pragma means that other definitions in the module do get properly checked against ambiguity.

GHC's suggestion ``To defer the ambiguity check to use sites, enable AllowAmbiguousTypes`` is currently costing a great deal of perplexity and frustration for novice and not-so-novice users. Evidence: StackOverflow questions anon. Switching on the option in the definition module is not likely to help anything compile, unless the user is consciously intending to use ``TypeApplications`` at the usage site/module. That may not be the best approach for the coding requirements (see Motivation section), but GHC's message does not suggest other options. Novice and not-so-novice users are likely to attach too much weight to that suggestion.


Alternatives
------------
Do nothing. That is, continue with the module-wide ``AllowAmbiguousTypes`` setting.

    These definitions do not compromise type safety or class coherence. If you don't use ``-XTypeApplications``, then they're just useless definitions. [@goldfirere commenting in #148]
    
I would disagree with that "useless". I see the confusion they cause as harmful. Especially because that follows from the error message's misleading ``enable AllowAmbiguousTypes``.

Re Type Applications [Section 2 Point 7.] possibly changing to silently accept ambiguous signatures might be considered too radical of a change. (Although this is the opposite of a breaking change: it will accept more programs, without needing ``LANGUAGE AllowAmbiguousTypes``.) Then Point 7. could require an ``{-# AMBIGUOUS #-}`` pragma after the ``@``, and syntactically would need the signature and pragma enclosed in parens, for example::

    x = foo @({-# AMBIGUOUS #-} forall b. C a b => a) bar

Unresolved questions
--------------------

No contrary feedback received for these questions, so left here as visible for Committee discussion.

* [No objection to:] Precise wording proposed for the rejection message that currently suggests enabling ``AllowAmbiguousTypes``.

* [Implementor's judgment:] Re pragmas that change semantics (such as the ``{-# OVERLAPPABLE #-}`` series), there has been comment they're difficult for source tooling utilities to observe. As well as the ``AMBIGUOUS`` pragma per signature, should there be a module-wide ``LANGUAGE`` setting? ``-XAmbiguousTypesPragma``.

* [From discussion, decide against this:] For modules containing more ambiguous types than not, so with ``AllowAmbiguousTypes`` switched on, should there be a per-signature pragma ``{-# NO[T_]AMBIGUOUS #-}`` that *does* apply the ambiguity check? That would prevent in future deprecating ``AllowAmbiguousTypes``.

* [Implementor's judgment:] If a signature marked ``{-# AMBIGUOUS #-}`` is not in fact ambiguous ...? A comment suggested warning of the non-ambiguity. 


Implementation Plan
-------------------

I am not accredited to interfere in GHC's type checking. Hopefully this is a narrowly targetted mod that merely suppresses the rejection message, if the pragma is present in the AST for the signature.

Appendix: BNF diff for appearences of ``{-# AMBIGUOUS #-}``
---------------------------------------------------------

(The optional ``[ {-# AMBIGUOUS #-} ]`` is the change.)
::

    exp     → infixexp :: [ {-# AMBIGUOUS #-} ] [context =>] type      (expression type signature)
            | infixexp
            
    fexp    → [fexp [@([ {-# AMBIGUOUS #-} ] type)] ] aexp	       (function application with type applicn, see Alternatives)

    gendecl → vars :: [ {-# AMBIGUOUS #-} ] [context =>] type          (type signature)
            | fixity [integer] ops                                     (fixity declaration)
            |                                                          (empty declaration)

    constr  → con [ {-# AMBIGUOUS #-} ] [!] atype1 … [!] atypek	       (arity con  =  k, k ≥ 0)
            | [ {-# AMBIGUOUS #-} ] (btype | ! atype) conop (btype | ! atype)	    (infix conop)
            | con { fielddecl1 , … , fielddecln }	               (n ≥ 0)

    newconstr → con [ {-# AMBIGUOUS #-} ] atype
            | con { var :: [ {-# AMBIGUOUS #-} ] type }
            
    topdecl → ...
            | data simpletype where [gconstrs] [deriving]              (GADT style data or newtype)
            | newtype simpletype where [newgconstrs] [deriving]
            | ...
            
    gconstrs → gconstr1 | … | gconstrn	                               (n ≥ 1)
    gconstr → con :: [ {-# AMBIGUOUS #-} ] [{ fielddecl1 , … , fielddecln }] [context =>] type  
                                                   	               (n ≥ 0, GADT style constructor)
                                                                       (GADT style newtype constr likewise)

    fielddecl → vars :: [ {-# AMBIGUOUS #-} ] (type | ! atype)

And optional ``[ {-# AMBIGUOUS #-} ]`` following ``::`` in

* a pattern signature;
* a pattern synonym signature;
* a kind signature (in class/instance heads).
