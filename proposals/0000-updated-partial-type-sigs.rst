Update design of partial type signatures
========================================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/194>`_.
.. sectnum::
.. contents::

There are a number of points of confusion and lack of user control around the partial type signatures feature.
These are explained in more detail in the Motivation_ section. This proposal describes an update to this feature.
It is *not* backward-compatible, but the expectation is that partial type signatures are a tool to use during
development, not something that should remain in a codebase for releases (though this proposal makes it easier to
do so, if you wish).

Motivation
------------
There are a number of infelicities around partial type signatures, described here in no particular order (but
numbered for back-reference).

1. Underscores may mean many different things.

   a. In patterns, a ``_`` means a part of a pattern that matches anything, but is otherwise immaterial. Examples::

        const x _ = x
        type instance IsJust (Just _) = True   -- happens in types, too!

      I will call these underscores "ignoreds". (Please suggest a better name.)

   b. In expressions, a ``_`` means a part of an expression for which you want GHC to print its type. Example::

        tuple :: (Int, Double, Bool, Char, Float, ShowS)
        tuple = (5, 3.14, False, _, _, _)   -- oops: I lost count, and then I forgot what ShowS meant

      Note that this can be useful in types, too::

        foo :: SomeMajorTypeFamily Nat Bool (_ 5) -> Bool  -- I forget: should I use 'Just or 'Left there??

      There is no way to get this behavior in types today.

      I will call these underscores "holes".

   c. In type signature declarations, a ``_`` means a part of a type for which you want GHC to infer its value using
      the definition of the variable being declared. Example::

        addOne :: _ -> Int
        addOne x = x + 1

      Here, we want GHC to use the *definition* of ``addOne`` to infer that the ``_`` stands for ``Int``. This works
      also for constraints::

        addOnePoly :: _ => a -> a
        addOnePoly x = x + 1

      GHC will infer that the ``_`` stands for ``Num a``. These underscores are called wildcards.

      In all cases, GHC reports to the user what it figured out. GHC normally rejects such programs, but it will
      accept them with ``-XPartialTypeSignatures`` enabled. GHC still reports its deductions for the wildcards
      (these underscores are called "wildcards"), unless ``-Wno-partial-type-signatures`` is specified.

      Wildcards naturally are disallowed in places where there is no expression nearby, such as in class method types
      or instance heads.

      The existence of a wildcard in a type signature drastically changes the algorithm GHC
      uses during type-checking. As one example of this, if a declaration has a wildcard in its type, it may
      not use polymorphic recursion, which requires a complete type signature and an utter absence of type inference.
      Another example is that GHC defers kind-generalization until *after* checking the expression; this can
      affect whether or not a program is accepted. (More on this point below.)

   d. In visible type applications, a ``_`` means a part of a type for which you want GHC to infer its value using
      context. Example::

        x = id @_ True

      GHC will infer that the ``_`` stands for ``Bool``. This form is most useful if you have multiple type
      parameters and want to skip some. For example, we have ``const :: forall a b. a -> b -> a`` and we
      might say ``const @_ @Bool 'x'`` to get a constant function of type ``Bool -> Char``. Note that the
      choice for ``a`` is easily inferrable to be ``Char`` here.

      I will call these underscores "elisions". These occur in visible type applications. They never
      cause GHC to reject a program or print out further information.

.. _`visible kind application`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0015-type-level-type-applications.rst

   These four meanings of underscores are distinct; we should allow programmers direct control over
   which behavior they want. Holes are very much like elisions, though: the different is that GHC does
   not try to fill in a hole.

2. Visible kind applications don't fit well with partial type signatures. As recently merged into HEAD,
   the `visible kind application`_ implementation treats underscores as a combination of wildcard and elision:
   the existence of an underscore in a visible kind application has GHC treat a type signature as partial,
   even though GHC does not stop compilation or emit warnings for such underscores. Furthermore, the value
   of an underscore in a visible kind application (e.g., in ``Proxy @_ True``) can often be inferred from
   context, not from an expression. This design was chosen because it's close to the treatment for visible
   type applications, but it's an unhappy compromise.

   Visible kind application (even an innocuous usage like the example in the previous paragraph) with underscores is not allowed
   where wildcards are not allowed, such as in data constructor declarations and in instance heads.

   This bit is really just an outright bug, but it's unclear how to fix this bug without a proposal such
   as this one.

3. Named wildcards act like wildcards but are named. This allows two niceties: the user can specify that
   the same wildcard is used twice, and output is clarified by giving a name to the wildcard (instead of
   just ``_``). Here is an example::

     foo :: _w -> _w -> _w
     foo x 'z' = x

   GHC will infer that ``foo :: Char -> Char -> Char`` (and that ``_w`` stands
   for ``Char``), knowing that the second argument must be a ``Char`` and that
   both arguments and the return type must be the same. This feature is
   enabled with ``-XNamedWildCards``.

   The feature is undiscoverable. If I write the code above in a module without ``-XNamedWildCards``, I get a type
   error. This is because ``_w`` is a legal type variable name in standard Haskell. No error message in GHC suggests enabling this
   extension.

   Naming is useful for all form of underscore except for ignoreds, as knowing that several different
   underscores mean the same thing can aid inference.

4. The current design of partial type signatures treats type generalization and constraint generalization differently.
   Consider these examples::

     ex1 :: _ -> _
     ex1 x = x

     ex2 :: _ -> _
     ex2 x = x + 1

     ex3 :: _ -> _
     ex3 x = not x

   With ``-XPartialTypeSignatures`` enabled, ``ex1`` and ``ex3`` are accepted, but ``ex2`` is rejected. (Actually, ``ex2``
   is accepted because of type defaulting. Say ``default ()`` to disable type defaulting, and you will observe that it
   is rejected. Avoiding this twist in the narrative would complicate the example unnecessarily.) In ``ex1``, GHC discovers
   that ``_`` stands for an unconstrained type variable ``t``, generalizes, and gets ``ex1 :: t -> t``. In ``ex3``, GHC
   discovers that ``_`` standards for ``Bool``. In ``ex2``, GHC discovers that ``_`` stands for ``t`` where ``Num t`` must
   hold; however, it rejects the declaration because no context was specified. In order to accept ``ex2``, we need to
   write ::

     ex2 :: _ => _ -> _

   With the possibility of a constraint, then ``ex2`` is accepted, with type ``Num t => t -> t``.

   What's awkward here is that the examples are given in order of increasing specificity; each example's type is more
   specific than the previous. Yet GHC's behavior wibbles and wobbles between them.

5. Sometimes, a type signature is meant to stand on its own; sometimes, it is meant to be understood in relation to
   a nearby expression. For example, these type signatures stand on their own::

     const :: a -> b -> a
     prox :: Proxy a -> Proxy a
     x :: Int

   In all cases, we know everything there is to know about the type of these variables without looking further.
   In the case of ``prox``, "knowing everything about the type" means that we can observe that ``a``\'s kind is
   unconstrained. The full signature will be ``forall {k :: Type} (a :: k). Proxy @k a -> Proxy @k a``.

   These type signatures do not stand on their own::

     wurble :: _ -> Bool
     wobble :: Either _ Bool -> Char

   In order to know the types of ``wurble`` and ``wobble``, we must read their definitions.

   Currently, we can distinguish between these cases by looking for the presence of an underscore (or, with
   ``-XNamedWildCards``, a type variable spelled with an initial underscore) in the type signature. However,
   sometimes we want a type signature with an underscore to stand alone (that is, be *complete*), and sometimes
   we want a signature without an underscore not to (that is, be *partial*).

   If we assume ``type Const :: forall a b. a -> b -> a``, then this type signature can be complete::

     blurb :: Proxy (Const @_ @Bool 5) -> ()

   We can infer that the first kind argument to ``Const`` should be ``Nat``. No definition for ``blurb``
   is needed to know its type. Yet, our rule above that we distinguish complete from partial signatures
   by the presence of an underscore works against us here.

   Conversely, assuming ``f :: forall (a :: Bool). Proxy a -> ()``, this type signature can be partial::

     hiccup :: Proxy a -> ()
     hiccup = f

   By looking at the *definition* for ``hiccup``, we learn that the kind of ``a`` should be ``Bool``. But
   we cannot know that if the type signature is *complete*; indeed, GHC rejects ``hiccup`` today, believing
   its type signature to be complete. The ``singletons`` package runs up against this: in the code it
   generates, it needs the expression in order to infer the kind of a type variable. Yet, short of inserting
   an underscore (and then asking our dear users to enable ``-XPartialTypeSignatures -Wno-partial-type-signatures``),
   there is no way of getting the behavior we want.

6. Control over partial type signatures vs complete type signatures is based on the presence or absence of
   a wildcard. This means that GHC sometimes makes the wrong decision: it is conceivable to want a partial
   type signature without a wildcard, and to write a complete signature with one.

   Here is a desired-complete signature that has a missing piece::

     foo :: Proxy @_ True -> ()

   We can infer the value of the elided kind argument to ``Proxy`` from the kind of ``True``.

   For an example of a desired-partial signature without a wildcard, see the examples below_.
   
**Summary**

Missing bits of programs vary along quite a few different axes.

Axis 1: how to fill the missing bit in?

A. Fresh variable; these are "ignored" underscores.

B. Figure it out by local context (``const @_ @Bool 'x'``); these are "elisions".

C. Figure it out by looking at an expression nearby (``foo :: _ -> Int``); these are "wildcards".

Axis 2: do we report information to the user?

A. No

B. Yes, as a warning

C. Yes, as an error

Axis 3: if we are reporting, what do we report?

A. The inferred value of the missing bit (``foo :: _ -> Int``)

B. The inferred type of the missing bit (``foo z = _``)

C. (not applicable -- for completeness with respect to 2A)
   
Axis 4: do we want to name the missing bit?

A. No

B. Yes, in order to reuse the same missing bit in multiple places

Axis 5: what context does the missing bit appear in?

A. In a pattern (including patterns of a type family equation)

B. In a term

C. In a type signature with a nearby expression (``foo :: _ -> Int; foo z = z + 1``);
   these may be partial (though changing them to partial changes their behavior)

D. In a type signature without a nearby expression (``data T where MkT :: _ -> T``);
   these must be complete

I claim that (with the exception of controlling the severity of a diagnostic when we are not
printing diagnostics) many locations in this matrix make sense and would be useful. However,
we currently inhabit this matrix only sparsely. (This proposal does not affect patterns, 5A.)

* 1A/2A/3C/4A/5A: **Allowed**: ``foo _ = True``, ``type instance Foo _ = True``

* 1A/2A/3C/4A/5B: **Not allowed**: ``figureItOutLater :: Int -> Int; figureItOutLater = _``; allowed with this
  proposal.
  
* 1A/2A/3C/4A/5C: **Not allowed**: ``foo :: _ -> Int; foo x = 5`` (we could just use a type variable, but the same
  argument could apply to ``_`` patterns, and yet we really like those); allowed with this proposal.

* 1A/2A/3C/4A/5D: **Not allowed**: ``data Existential where MkEx :: _ -> Existential`` (ditto comment); allowed
  with this proposal.

* 1A/2A/3C/4B/5A: **Allowed**: ``type family Equals a b where Equals _x _x = True`` (but
  removing the underscores emits no warning; perhaps that's just a bug)

* 1A/2A/3C/4B/5B: **Appears unuseful**, until we have very snazzy term inference

* 1A/2A/3C/4B/5C: **Appears unuseful**; just use an ordinary type variable

* 1A/2A/3C/4B/5D: **Appears unuseful**; just use an ordinary type variable

* 1A/2BC/...: **Appears unuseful**; if we don't care about the missing bit, we don't want a diagnostic; exception follows:

* 1A/2B/3A/4A/5B: **Not allowed**: ``figureItOutLater :: Int -> Int; figureItOutLater = _``, but now we get a warning
  each time we do this. Allowed with this proposal.

* 1B/2A/3C/4AB/5A: I can't tell the difference between this and the 1A case.
  
* 1B/2A/3C/4A/5B: **Not allowed**: ``id :: a -> a; id = _``, ``foo :: Sing True; foo = _`` (we can infer the exact value of these expressions from
  their types); compatible with this proposal.

* 1B/2A/3C/4A/5C: **Allowed**: ``foo :: Proxy @_ True -> ()`` with
  ``-XPartialTypeSignatures -Wno-partial-type-signatures``; the type signature
  becomes partial, even if we don't want this behavior. This last bit is fixed by this
  proposal.

* 1B/2A/3C/4A/5D: **Not allowed**: ``data T where MkT :: Proxy @_ True -> T``; allowed by this proposal.

* 1B/2A/3C/4B/5B: I think this might be useful, but I can't come up with an example. It would mean
  that we can infer the value of a term only by analyzing two (or more) of the term's occurrences.
  Compatible with this proposal.

* 1B/2A/3C/4B/5C: **Allowed**: ``foo :: Proxy @_k True -> Proxy @_k a`` but with same caveats as
  1B/2A/3C/4A/5C. This will print a diagnostic with this proposal (1B/2BC/3AB/4B/5C).

* 1B/2A/3C/4B/5D: **Not allowed**: ``data T where MkT :: Proxy @_k True -> Proxy @_k a -> T``. This
  will print a diagnostic with this proposal (1B/2BC/3AB/4B/5D).

* 1B/2BC/3A/4AB/5A: **Appears unuseful**: I don't know what this means.

* 1B/2BC/3A/4A/5B: **Not allowed**: See 1B/2A/3C/4A/5B, but with printing. Compatible with this proposal.

* 1B/2BC/3A/4A/5C: **Allowed**, but only with visible dependent quantification: ``type VDQ :: forall k -> k -> Type; foo :: VDQ _ Int -> ()``.
  Reporting is always suppressed in a visible kind application. This proposal requires giving a name (4B) to the missing bit
  in order to get a diagnostic.

* 1B/2BC/3A/4A/5D: **Not allowed**: Like 1B/2A/3C/4A/5D. This proposal requires giving a name (4B) to the missing bit
  in order to get a diagnostic.

* 1B/2BC/3A/4B/5B: Like 1B/2A/3C/4B/5B. Named missing bits always print a diagnostic with this proposal.

* 1B/2BC/3A/4B/5C: **Allowed**: Like 1B/2A/3C/4B/5C, but with other flags. This proposal removes dependency on flags
  and does not force a signature to be partial.

* 1B/2BC/3A/4B/5D: **Not allowed**: Like 1B/2A/3C/4B/5D. Allowed by this proposal.

* 1B/2BC/3B/4A/5A: **Not allowed**: ``foo :: Maybe Bool -> (); foo (Just _) = ()``, but I want the type of the underscore.
  
* 1B/2B/3B/4A/5B: **Not allowed**: This is like 1B/2A/3C/4A/5B, but with printing. Compatible with this proposal.

* 1B/2C/3B/4A/5B: **Allowed**: ``foo :: Int -> Bool; foo 5 = _``. Retained with this proposal.

* 1B/2BC/3B/4A/5C: **Not allowed**: ``foo :: Either _ Bool -> Int``, but I want to know the type of the ``_``. Use
  a name (4B) to get the diagnostic with this proposal.
  
* 1B/2BC/3B/4A/5D: **Not allowed**: ``data T where MkT :: Either _ Bool -> T``, but I want to know the type of the ``_``.
  Use a name (4B) to get the diagnostic.

* 1B/2BC/3B/4B/5A: **Not allowed**: ``type instance Foo _x (Left _x) = ...``, where I want the kind of ``_x``.
  
* 1B/2BC/3B/4B/5B: **Not allowed**: ``foo a = (_x True, not (_x a))`` (we can infer ``_x :: Bool -> Bool``, but only
  by looking at *both* occurrences). Allowed by this proposal.

* 1B/2BC/3B/4B/5C: **Not allowed**: Like 1B/2BC/3B/4A/5C. Allowed by this proposal.

* 1B/2BC/3B/4B/5D: **Not allowed**: Like 1B/2BC/3B/4A/5D. Allowed by this proposal.

* 1C/.../5A: Not applicable.

* 1C/.../5B: Not applicable.

* 1C/.../5D: Not applicable.

* 1C/2A/3C/4A/5C: **Allowed**: ``foo :: _ -> Int; foo True = 5``, with ``-XPartialTypeSignatures -Wno-partial-type-signatures``. Retained
  with this proposal.

* 1C/2A/3C/4B/5C: **Allowed**: ``foo :: _t -> _t; foo _ = True``, with ``-XPartialTypeSignatures -XNamedWildCards -Wno-partial-type-signatures``.
  Named missing bits always print a diagnostic with this proposal.

* 1C/2BC/3A/4A/5C: **Allowed**: Like 1C/2A/3C/4A/5C, but with different flags. Use a name (4B) to get a diagnostic with this
  proposal.

* 1C/2BC/3A/4B/5C: **Allowed**: Like 1C/2A/3C/4B/5C, but with different flags. This proposal removes the
  dependency on flags.
  
* 1C/2BC/3B/4A/5C: **Not allowed**: ``foo :: Proxy _ -> (); foo (Proxy :: Proxy @Bool a) = ()``, but I want the know that type of the ``_`` is ``Bool``.
  Use a name (4B) to get a diagnostic with this propopsal.

* 1C/2BC/3B/4B/5C: **Not allowed**: Similar to 1C/2BC/3B/4A/5C. Allowed by this proposal.

There are several problems not really reflected by this taxonomy:

a. We might usefully want to combine several possibilities in close proximity. Changing between modes with module-wide flags is unpleasant.

b. As discussed in motivation point (5), we won't want the choice between complete and partial type signatures to be controlled by
   the presence of underscores.
  
Proposed Change Specification
-----------------------------

1. Outside of patterns, treat ``_`` and ``__`` as elisions everywhere. This means that ``_`` and ``__`` mean "I don't care".

   - In types (with ``-XElidedTypes``), a ``_`` is treated as a fresh unification variable. This means that ``foo :: _ -> _`` is the same as
     ``foo :: a -> b``, while ``Proxy @_ True`` is the same as ``Proxy @Bool True``. You're instructing GHC that
     you want it to fill in the ``_`` with what is necessary for the type to kind-check. Once GHC is finished processing
     the type, however, any opportunity to solve for ``_`` has been taken; at that point, if it is still unconstrained,
     GHC generalizes over it, like it would a fresh normal type variable.

     Elisions can appear anywhere a type can be written, but they cannot
     appear as a constraint in a complete type signature. They do not cause
     diagnostics to be printed. The existence of an elision does *not* cause a
     signature to be treated as partial.

     Without ``-XElidedTypes``, an underscore in a type is an error; the error message would suggest
     either using a named wildcard to get a diagnostic or enabling ``-XElidedTypes`` to accept the
     elision.

     Each elision spelled ``__`` will cause GHC to print out a diagnostic warning explaining how
     the elision was filled. Writing an elision as ``_`` suppresses the diagnostic, as does
     ``-Wno-elided-types``.

   - In expressions, a ``_`` is a part of the expression the author did not care to write. Currently, this means
     that ``_`` will be replaced with ``error "elision at <line>:<col>"``. In this case, an error will be printed,
     stating the inferred type of the ``_`` and suggesting to enable ``-XElidedExpressions`` if the user
     wants to keep the ``error``\ing behavior. With ``-XElidedExpressions``, GHC will still warn, controlled
     by ``-Werroring-elided-expressions``. (This case is an exception to the general rule that ``_`` prints
     no diagnostic, as it seems willfully cruel not to print one here. Once GHC can infer proper expressions,
     this might be changed.)

     In the future, GHC may support the possibility of inferring expressions. An elided expression may then
     be filled in, not with a call to ``error``, but a correct expression. For example, we might imagine
     that ::

       id :: a -> a
       id x = _

     has its elision filled in with the only possible non-bottom value, ``x``. Any such behavior will
     have to be specified separately and would likely be guarded by an extension flag and possible diagnostic
     output.

2. Outside of patterns, treat an unbound identifier beginning with an underscore as a named wildcard. A named wildcard
   induces GHC to print an error with the wildcard's type and a suggested value. This behavior is
   controlled by the ``-XNamedWildCards`` extension.

   - In a type signature, a named wildcard behaves much as one does today, though its kind will be printed
     in the diagnostic along with the other information. Just like today, a suggestion will be included
     to enable ``-XPartialTypeSignatures``.

   - In a type outside a type signature (e.g., in an instance declaration or a data constructor type),
     a named wildcard will induce a diagnostic including the wildcard's kind and any information GHC
     can figure out about its value.

   - In an expression, a named wildcard will induce a diagnostic including the
     wildcard's type and any information GHC can figure out about its value,
     including suggested replacements. In this way, an expression named
     wildcard will behave like holes have.

   In all cases, the diagnostic is suppressed by prefixing the identifier with only one underscore;
   two or more underscores will induce the diagnostic.

3. In the absence of ``-XNamedWildCards``, the use of a type variable that begins with an underscore will induce a warning
   that it looks like an attempt to write a named wildcard. This warning will be
   controlled by ``-Wpossible-named-wildcards``, on by default.
     
4. Enabling ``-XPartialTypeSignatures`` is necessary in order for GHC to accept a program with
   named wildcards in type signatures. These signatures must also be written using the new
   modifier ``%Partial`` instead of the typical ``::``. That is, we would now write ::

     quux %Partial :: _w -> Bool
     quux x = not x

   The modifier would be a loud indication that the signature is *partial*. It induces GHC
   to use its partial-type-signature algorithm instead of its typical type-checking algorithm.

   Partial type signatures would work with elisions, too, allowing ::

     wurble %Partial :: _ -> _
     wurble x = not x

   Partial type signatures would be kind-generalized *after* checking the function body. This would
   allow something like the following to be accepted::

     silly %Partial :: Proxy a -> ()
     silly (_ :: Proxy @Bool _) = ()

   Note that the expression would be more specific than its type signature, if we kind-generalized
   the signature *before* processing the expression.

   This example is further explained below_.

   Partial type signatures forbid polymorphic recursion, as they do today.

5. Partial type signatures would generalize fresh variables only when an *extra-variables* wildcard
   is in the type. That is, the last item in the list of variables after the word ``forall`` can
   now be an elision ``_`` or a named wildcard ``_w`` (but only in a partial type signature).
   In either case, this means that GHC can
   generalize over more variables than have been written in the type signature. As usual, an elision
   produces no diagnostic, while a named wildcard does. Here are two examples::

     ex4 %Partial :: _w -> _w
     ex4 x = x

     ex5 %Partial :: forall _. _w -> _w
     ex5 x = x

   Here, ``ex4`` is rejected, because we do not know what type ``x`` should have and we cannot
   generalize. On the other hand ``ex5`` is accepted. The extension ``-XPartialTypeSignatures``
   would have to be enabled; no diagnostic would be printed.

   The use of an extra-variables wildcard anywhere other than a top-level ``forall`` in a
   partial type signature is disallowed, much like the extra-constraints wildcard previously.

   Note that any ordinary type variables mentioned in a type are generalized as usual. Thus, ::

     ex6 %Partial :: _w -> a
     ex6 x = x

   is accepted, as the ``a`` is already a quantified type variable. On the other hand, ::

     ex7 %Partial :: _w -> a -> a
     ex7 _ x = x

   is rejected, as we have no type for the first argument of ``ex7``.

6. The ``Partial`` modifier will be exported from ``GHC.Exts``.

Here is a summary:

+----------------------------+------------------------+------------------------------+
|                            |elision                 |named wildcard (assume        |
|                            |                        |``-XNamedWildCards``)         |
+----------------------------+------------------------+------------------------------+
|in complete type signature  |GHC uses unification to |GHC treats the signature as   |
|                            |fill in elision. If     |if it were partial, if        |
|                            |unification does not    |possible. GHC uses the        |
|                            |find a value for the    |definition of the identifier  |
|                            |elision, generalize.    |to solve for the wildcard.    |
|                            |Needs ``-XElidedTypes``.|The diagnostic prints both    |
|                            |                        |the value GHC has discovered  |
|                            |Covers 1AB/2A/3C/4A/5CD,|for the wildcard and its      |
|                            |without forcing a       |kind. Compilation is aborted  |
|                            |partial type signature  |always.                       |
|                            |                        |                              |
|                            |                        |Covers 1BC/2C/3AB/4B/5CD      |
|                            |                        |                              |
+----------------------------+------------------------+------------------------------+
|in partial type signature   |same as above, but      |same as above, but            |
|(assume                     |information from the    |compilation is not aborted    |
|``-XPartialTypeSignatures``)|definition can be taken |(i.e., any diagnostic is a    |
|                            |into account            |warning)                      |
|                            |                        |                              |
|                            |Covers 1ABC/2A/3C/4A/5C |Covers 1C/2B/3AB/4B/5C        |
+----------------------------+------------------------+------------------------------+
|in a visible type           |same as above, but no   |same behavior as an elision,  |
|application                 |generalization. If GHC  |but requiring                 |
|                            |is unable to figure out |``-XPartialTypeSignatures``   |
|                            |what an elision should  |(and allowing sharing)        |
|                            |be, error.              |                              |
|                            |                        |Covers 1B/2BC/3AB/4B/5CD      |
|                            |Covers 1B/2A/3C/4A/5CD  |                              |
|                            |                        |                              |
|                            |                        |                              |
|                            |                        |                              |
+----------------------------+------------------------+------------------------------+
|in another type (e.g., data |same as in a complete   |Not allowed; issue an error   |
|constructor signature,      |type signature          |with the kind of the          |
|instance head, etc.)        |                        |wildcard and any information  |
|                            |Covers 1AB/2A/3C/4A/5D  |GHC can figure out about the  |
|                            |                        |content of the wildcard.      |
|                            |                        |                              |
|                            |                        |Covers 1B/2C/3B/4B/5D         |
+----------------------------+------------------------+------------------------------+
|in an expression            |GHC replaces the        |Same behavior as today's      |
|                            |underscore with ``error |holes: a diagnostic is        |
|                            |"elision at             |printed with the hole's type  |
|                            |<line>:<col>"`` with    |and suggestions for what it   |
|                            |``-XElidedExpressions``.|might be filled in with.      |
|                            |In the future, perhaps  |This behavior does **not**    |
|                            |GHC can be cleverer (for|require an extension.         |
|                            |example: ``f :: a -> a; |                              |
|                            |f = _``.                |                              |
|                            |                        |                              |
|                            |Covers 1AB/2A/3C/4A/5B  |Covers 1AB/2C/3AB/4B/5B       |
|                            |                        |                              |
|                            |                        |                              |
+----------------------------+------------------------+------------------------------+
						  

Effect and Interactions
-----------------------
* All positions marked **Not allowed** (outside of patterns 5A)
  in the Motivation are now allowed.

* The new design allows the user to control whether they want an elision or a named wildcard, using
  a convenient naming convention.

* The new design gives users fine control over generalization, through the use of ``%Partial`` to
  suppress kind generalization and the use of ``forall a b c _.`` to explicitly enable type
  generalization.

* Visible kind application now fits in nicely. Users can control whether they want elisions
  or wildcards.

* Partial type signatures have become louder, through the addition of ``%Partial``. This makes it
  more sensible to keep partial type signatures in released code. The new syntax also
  allows users to write elisions in type signatures without causing the signature to
  become partial.

* Wildcards might look like definitions in lens-heavy code; that is, a misspelled ``_field``
  would now be a wildcard. I don't think the ensuing error message would pose a challenge
  to a programmer in figuring out what happened. We can continue to suggest possible misspellings
  in such error messages.

* The specification above describes behavior outside of patterns. No change is made to
  the way patterns behave. Note that type signatures in patterns are not patterns.
  
Costs and Drawbacks
-------------------
* Partial type signatures have proved hard to implement and with many corner cases. The new
  design seems no simpler nor more complicated than the current, but it will take a fair amount
  of work to re-engineer.

* The new design does not adequately treat patterns. It is conceivable that a user would want
  a wildcard (with diagnostic information) in a pattern, and this is no more achievable with this
  proposal than it was previously.

* This proposal is not backward compatible, as it requires the ``%Partial`` modifier for partial
  type signatures.

* This proposal warns on the Haskell98 program ::

    id :: _w -> _w
    id x = x

  Thus, this standards-conforming program would now cause GHC to bleat (but still accept,
  with its original meaning).
  
* Elisions in types (``const :: a -> _ -> a``) seem less useful than other aspects of this
  proposal, and yet they occupy prime syntactic real estate. Is there a better design around
  this issue?
  
Alternatives
------------
* Though specification parts (1), (2), and (3) are tightly linked, the others are not, and could be
  usefully removed from this proposal while not losing other parts.

* Though there is no burning fire here (and thus "do nothing" isn't
  unreasonable), the design of visible kind application is really quite
  awkward. If we choose to walk away from this more comprehensive proposal, it
  would be great to have a concrete design for underscores in visible kind
  application, at least.

* In order to separate the choice of naming from the appearance of diagnostics, we could
  imagine a ``{-# PRINT #-}`` pragma (or similar) telling GHC what to print. However, I dislike
  this because a user who justs wants to make a quick query won't want to write ``{-# PRINT #-}``
  to get it.

* This proposal uses one underscore to mean "don't print" and two (or more) to mean "print".
  This decision could be reversed.

Resolved Questions
------------------
* Q: "Aha! So what you really mean is that ``_`` is universal in complete type signatures
  and existential in partial ones."

  A: Not quite. If we have ``data Prox k (a :: k)``, then ``f :: Prox _ True`` is perfectly
  fine; GHC would discover that ``_`` stands for ``Bool``. This is *not* universal quantification.
  The elision is just a spot about which we don't care, and want GHC to do its best.
  In ``g ::? forall _. _ -> _; g x = x``, we actually generalize over the elision, so it's not
  exactly existential quantification. (Without the ``forall _``, it would be.)

Unresolved Questions
--------------------
* Is this really the best syntax? 

* Is this design too elaborate? I have a tendency to build elaborate but expressive edifices. Perhaps
  there is a sweet spot closer to the ground here.
  
* I would welcome new syntax dealing with patterns in this framework.

Further examples
----------------

.. _below:
  

* Let's dive deeper into this example::

    silly %Partial :: Proxy a -> ()
    silly (_ :: Proxy @Bool _) = ()

  Actually, let's first consider something very closely related::

    sillier :: Proxy a -> ()
    sillier (_ :: Proxy @Bool _) = ()

  This definition of ``sillier`` is rejected. That's because GHC processes its type signature in isolation.
  GHC sees that we wish to quantify over ``a``. After kind-checking, the kind of ``a`` is utterly unconstrained.
  Thus, GHC infers ``sillier :: forall {k} (a :: k). Proxy @k a -> ()``. All of this has happened *without*
  looking at the definition of ``sillier``. When GHC does look at that definition, it is rejected, as
  the argument to ``sillier`` has type ``Proxy @k a``, not ``Proxy @Bool a``. This is the kind-level
  equivalent of something like ::

    silliest :: a -> a
    silliest True = False

  In both ``sillier`` and ``silliest``, the type signature is *more general* than the definition. It's
  just that ``sillier`` is harder to see that.

  Returning to ``silly``, with the partial type signature marker ``%Partial``, GHC will *not* kind-generalize
  the type. It will effectively infer ``silly :: forall (a :: _k). Proxy @_k a -> ()``, where ``_k``
  behaves like a named wildcard. (I say "behaves like" because named wildcards become unification variables
  internally; in this case, the kind of ``a`` really would just be a unification variable.) Now, when
  checking the definition of ``silly``, GHC is free to discover that ``_k`` should be ``Bool``, and all is
  well.

  Note that I did *not* mean to write ``silly %Partial :: Proxy _a -> ()``. I want ``a`` to be a skolem here. It's
  ``a``\s *kind* that I want not to be a skolem.

  Thus, ``silly`` is silly only because it is contrived, not because it is wrong.

* Here are some examples around the use of elisions:

  1. ::

       f :: _ -> _

     This would mean the same as ``f :: a -> b``.

  2. ::

       f :: forall a. a -> _

     This would mean the same as ``f :: forall a. forall b. a -> b`` (the second ``forall`` prevents
     ``b`` from being in scope in the defined term). I'm sure you now ask: "Why the unrequested
     generalization?" Because this is a complete type signature, not a partial one. The new
     generalization behavior affects only *partial* type signatures. So, ::

       f ::? forall a. a -> _

     would use the definition for ``f`` to determine the result type, but that type could not
     mention a type variable other than ``a`` (assuming we're at top-level).

  3. ::

       f :: forall a _. a -> _

     This is rejected, as it contains an extra-variables wildcard in a non-partial type signature.
