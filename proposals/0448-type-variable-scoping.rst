Modern Scoped Type Variables
============================

.. sectnum::
.. author:: Richard Eisenberg
.. date-accepted:: 2022-07-25
.. ticket-url::
.. implemented::
.. highlight:: haskell
.. header:: This proposal was `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/448>`_ and `amended by #604 <hhttps://github.com/ghc-proposals/ghc-proposals/pull/604`_.
.. contents::

This proposal updates the treatment of scoped type variables in GHC,
tying together many existing proposals:

.. _`#99`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0099-explicit-specificity.rst
.. _`#119`: https://github.com/ghc-proposals/ghc-proposals/pull/119
.. _`#126`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0126-type-applications-in-patterns.rst
.. _`#128`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0128-scoped-type-variables-types.rst
.. _`#155`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0155-type-lambda.rst
.. _`#228`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0228-function-result-sigs.rst
.. _`#238`: https://github.com/ghc-proposals/ghc-proposals/pull/238
.. _`#270`: https://github.com/ghc-proposals/ghc-proposals/pull/270
.. _`#281`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst
.. _`#285`: https://github.com/ghc-proposals/ghc-proposals/pull/285
.. _`#291`: https://github.com/ghc-proposals/ghc-proposals/pull/291
.. _`#378`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst
.. _`#402`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0402-gadt-syntax.rst
.. _`#420`: https://github.com/ghc-proposals/ghc-proposals/pull/420
.. _`#425`: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0425-decl-invis-binders.rst
.. _`#523`: https://github.com/ghc-proposals/ghc-proposals/pull/523
.. _Type Variables in Patterns: https://richarde.dev/papers/2018/pat-tyvars/pat-tyvars.pdf
.. _Kind Inference for Datatypes: https://richarde.dev/papers/2020/kind-inference/kind-inference.pdf
.. _`Haskell 2010 Report`: https://www.haskell.org/onlinereport/haskell2010/haskellch10.html
.. _`Visible Type Applications`: https://richarde.dev/papers/2016/type-app/visible-type-app.pdf
.. _`principles`: ../principles.rst
.. _`Contiguous Scoping Principle`: ../principles.rst#contiguous-scoping-principle
.. _`Explicit Binding Principle`: ../principles.rst#explicit-binding-principle
.. _`Explicit Variable Principle`: ../principles.rst#explicit-variable-principle
.. _`Visibility Orthogonality Principle`: ../principles.rst#visibility-orthogonality-principle
.. _`Syntactic Unification Principle`: ../principles.rst#syntactic-unification-principle
.. _`Lexical Scoping Principle`: ../principles.rst#lexical-scoping-principle

* `#126`_:
  Accepted, implemented proposal on accepting type arguments to constructor patterns,
  allowing constructions like
  ``f (Just @Int x) = x + 5``
  and
  ``g (Dynamic @a x) = show (typeOf (x :: a))``.
* `#155`_:
  Accepted, not implemented proposal on accepting type arguments to lambdas,
  allowing constructions like
  ``\ @a (x :: a) -> x``.
* `#238`_:
  Not yet accepted proposal that updates and extends `#155`_ to include a reshuffling of extensions around scoped type variables,
  as well as describing clearer rules mediating between the old ``-XScopedTypeVariables`` behavior and the new proposed behavior.
* `#285`_:
  Accepted, not implemented proposal introducing ``-XNoImplicitForAll`` and ``-XNoPatternSignatureBinds``,
  restricting where type variables may implicitly be brought into scope.
* `#291`_:
  Not yet accepted proposal that is an amendment to `#126`_ to change the way type variables are brought into scope to be more like term variables, and less like pattern signatures.
* `#420`_:
  Not yet accepted proposal that is an amendment to `#285`_,
  with a very tiny, technical tweak to a definition.

This proposal supersedes all of the proposals above, including the accepted ones.
The goal of writing this is to provide a unified framework,
so that we can design our language with cohesive treatment of scoped type variables,
instead of simply an agglomeration of features.

Motivation
----------

With GHC's powerful type-level programming features,
we need powerful abilities to bring type variables into scope.
The proposal defers to the individual proposals linked above for motivation for why we generally want these type-level features.
Individual aspects of this unifying proposal are motivated near where they are introduced.

How to read this proposal
-------------------------

This is a large proposal, with a number of moving parts.
The essential reason all these moving parts are glued together in just one proposal is so that they can be unified by their desire to uphold the principles added to our `principles`_ document.
Individual components of this proposal can be designed, debated, and implemented separately,
yet are presented in one document as they are meant to dovetail together nicely.

As currently written, this proposal is not self-contained, in that motivation for some individual pieces was not copied from their source proposals.
In all cases, when this proposal refers to others as inspiration, seeking more information there will likely be helpful.

If this proposal is accepted, it may be a good idea to incorporate that motivation, etc., right in this proposal here, to make it self-contained.
I am happy to do this at the direction of the committee.

Extension shuffling
-------------------

Right now, ``-XScopedTypeVariables`` does a lot of heavy lifting.
This proposal breaks up ``-XScopedTypeVariables`` into its components.
This enables finer-grained control,
and the ability for e.g. the ``a`` in ``f :: forall a. a -> a`` not to scope over the definition of ``f``.

The new meaning of ``-XScopedTypeVariables`` is the same as the old one.
The only backward-incompatible part of this is that, today, ``-XPatternSignatures`` is a deprecated synonym of ``-XScopedTypeVariables``.
Under this change, that would no longer be true.

This component of this proposal is taken from the not-yet-accepted proposal `#238`_,
changing the name of what I now call ``-XExtendedForAllScope``,
and simplifying the binding story around pattern signatures (getting rid of ``-XPatternSignatureBinds``).
This part of the proposal also refines ``-XPatternSignatures`` as a new way of handling the pattern-signature-binding part of `#285`_.

``-XImplicitBinds`` is the combination of ``-XImplicitBindsForAll`` and ``-XPatternSignatureBinds`` from accepted
proposal `#285`_, and differ from that proposal in that:

  - The extensions are combined for fewer knobs when the motivations are the same.

  - ``RULES`` was forgotten as an example.

  - Examples of implicit binds falsely categorized as pattern signature binds are now properly included under the proposed change specification.

    "Pattern signatures" has a narrow meaning but @Ericson2314 misunderstood it to include other negative-position type signatures.
    Now that the extensions are combined we side-step the phrase "pattern signature" more easily.

Motivation for any extension shuffling
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The main goal of this extension shuffling is to introduce ``-XExtendedForAllScope`` as an extension separate from ``-XScopedTypeVariables``.
This separation is motivated by two reasons:

* Some people [citation needed] dislike the behavior captured in ``-XExtendedForAllScope``
  (where the ``a`` in ``f :: forall a. a -> a`` is in scope in ``f``\ 's definition).
  Separating out the extension allows us to avoid this behavior.

* The behavior of ``-XExtendedForAllScope`` is at odds with the behavior of ``-XTypeAbstractions`` for binding `type variables in lambda patterns <#type-vars-in-lambda>`_;
  see `this specification point <#fraught-relationship>`_.
  It thus seems necessary to separate out the problematic ``-XExtendedForAllScope`` from the other components of ``-XScopedTypeVariables``.

A secondary goal is to clean up some issues with proposal `#285`_ while simplifying things:

* ``-XImplicitForAll`` and ``-XPatternSignatureBinds`` have the exact same
  motivation, and it is unclear why one would ever want one without the other.

* ``-XImplicitForAll`` and ``-XPatternSignatureBinds`` *missed* some of the cases in the examples, which clearly are implicit binding forms meant to be turned off per the overall motivation, but nonetheless slipped through the cracks of the drafting process.

Having separated out ``-XExtendedForAllScope``, it seemed strange to have a ``-XRumpEndOfOldScopedTypeVariables``
extension, and so I've introduced separate ``-XMethodTypeVariables`` and ``-XPatternSignatures``.

Motivation for ``-XPatternSignatures``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is taken from `#119`_.
"I" and "me" here is Joachim Breitner, aka @nomeata.

Originally, ``PatternSignatures`` was a an extension on its own,
but at some point it started to imply ``ScopedTypeVariables`` and eventually was deprecated in favor of the latter.
This has always bothered me and I often find myself in situations where I need to use a pattern signature without having any need for scoped type variables.
This need has increased with more polymorphic functions in ``base`` (e.g. post FTP).

I too often thoughts “I should have raised this point when it was time, but it is too late now”.
But maybe it is not too late… hence this proposal.

The concrete motivation is to be able to write something like this::

   {-# LANGUAGE OverloadedStrings #-}
   foo :: Monad m => m Int
   foo = do
     list <- return ""
     return $ length list

Currently, this fails with (much shortened)::

    Test.hs:4:18: error:
        • Could not deduce (Data.String.IsString (t0 a0))
            arising from the literal '""'
    Test.hs:5:12: error:
        • Could not deduce (Foldable t0) arising from a use of 'length'

Ah, the FTP strikes again.
So to fix this, I have to specify ``list``\ 's type.
In Haskell98 I can add a type signature to the use of ``list``, but that is ugly:
Types should be declared where stuff is brought into scope!
So I want to write::

   {-# LANGUAGE OverloadedStrings #-}
   foo :: Monad m => m Int
   foo = do
     list :: String <- return ""
     return $ length list

but I get::

    Test.hs:4:3: error:
        Illegal type signature: 'String'
          Type signatures are only allowed in patterns with ScopedTypeVariables

Ok, that works, but why am I bothered with ``ScopedTypeVariables``?
Furthermore, ``ScopedTypeVariables`` is not conservative;
it may actually break my program somewhere!

What I really want in this case is a pattern signature,
and it would be nice if I could just state that ``PatternSignatures``.

Motivation for not doing binding
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``-XPatternSignatures`` as proposed here is more narrow than its prior incarnation circa GHC 6.
This is because it just allows signatures using already-bound variables, and doesn't include any implicit binding mechanism for variables in the signature that aren't yet bound.
(That instead is left for ``-XImplicitBinds``.)

We here discuss the motivation for this decision.

Principles
""""""""""

This is necessary in order to uphold the `Lexical Scoping Principle`_, part (a).

Avoiding Redundancy
"""""""""""""""""""

A `comment <https://github.com/ghc-proposals/ghc-proposals/pull/523#issuecomment-1346449731>`_ SPJ left in now-closed proposal `#523`_ states the argument well:

  Currently pattern signatures are funny: you can only tell whether ``(\(x::a) -> blah)`` brings ``a`` into scope if you know whether or not ``a`` is already in scope.
  Not a beautiful thing.

  [...]

  An alternative would be to abolish pattern signatures --- or at least abolish the rule that allows a pattern signature to bring a variable into scope.
  _That rule was only present to allow us to give a name to existential type variables._ E.g.

  ::

    data T = forall a. MkT [a] (a -> Int)

    f :: T -> [Int]
    f (MkT (xs :: [a]) f) = let mf :: [a] -> [Int]
                                mf = map f
                            in mf xs

  Here the pattern signature on ``xs`` brings ``a`` into scope, so that it can be mentioned in the type signature for `mf`.
  In the past there was no other way to do this.
  But now we can say

  ::

    f :: T -> [Int]
    f (MkT @a xs f) = let mf :: [a] -> [Int]
                          mf = map f
                      in mf xs

  So we could, if we chose, deprecate and ultimately abolish the ability for pattern signatures to bring a new type variable into scope.
  Instead of *adding* complexity to the language, let's *remove* it.

It would be hard to change ``-XScopedTypeVariables``, so we don't propose that.
But right now, and *only* right now, it is easy to adjust ``-XPatternSignatures`` before it is reintroduced.
This is our best shot to steer people away from pattern signature binds and towards ``@`` instead!

Unified Namespace
^^^^^^^^^^^^^^^^^

`#281`_ introduces ``-XRequiredTypeArguments`` which is *almost* backwards compatible, except for conflicting with implicit binding.
The general method of ``-XRequiredTypeArguments`` w.r.t namespacing is to simulate a single namespace by having variable usages check the "other" namespace" when what they are looking for is not found in "proper" namespace for the location of the identifier.
For example,

::

  x = Int
  y = 1 :: x -- OK renaming, x is found in the term namespace.

::

  type X = Int
  y :: Type = X -- OK renaming, X is found in the type namespace

(There are errors after renaming in the above examples, but lets ignore them for now.
The goal is to make those errors go away long term, so we should not rely on them giving us "syntax to steal".
More complicated examples *will* work completely with `#281`_ without further generalizations that rely on the same cross-namespace variable lookup in both directions.)

This is an extension of the same method of namespace used for ``-XDataKinds``, as is backwards-compatible for the same reason.

The issues arise with implicit binding (pattern signature bindings and implicit ``forall`` bindings alike).
Consider this program::

  t = Int
  foo (x :: t) = 0

With ``-XScopedTypeVariables`` today, ``t`` is considered unbound, and so ``t`` is implicitly bound.
But this breaks the single-namespace illusion --- ``t`` *would* have been found in the other namespace, if it weren't for the implicit binding.
``-XRequiredTypeArguments`` is thus forced to choose between being a monotonic extension (allowing more programs, changing the meaning of no existing program) or faithfully simulating a unified single namespace;
it chooses the latter at the expense of the former.
It does so by changing the implicit binding rules to consult both namespaces first: ``t`` above is is a use not a bind.

The goal of this proposal, `#448`_ is to move away away from ``-XScopeTypeVariables``, and adopt designs that are compatible with ``-XRequiredTypeArguments`` without requiring it.
``-XPatternSignatures`` *without* implicit bindings is just that:

- Adding just implicit bindings is a monotonic extension
- Adding just cross-namespece variable resolution is a monotonic extension

It therefore serves as a "least common ancestor" of these other extensions.
It is useful to materialize these points in the design space with language extensions:
both to isolate the points of agreement from the points of controversy in the design space,
and allow people to write less restricted code that they are nonetheless confident they can copy between between different modules with different versions of the language without issue.

Consistency
"""""""""""

This more narrow formulation of ``-XPatternSignatures`` matches ``-XKindSignatures``.
``KindSignatures`` doesn't allow implicit binds for a rather roundabout reason: implicit binds would imply implicit kind-level foralls, which would require ``-XPolyKinds``::

  ghci> :set -XKindSignatures
  ghci> :set -XNoPolyKinds
  ghci> data Foo (a :: b)

  <interactive>:3:16: error:
      Unexpected kind variable ‘b’
      Perhaps you intended to use PolyKinds
      In the data type declaration for ‘Foo’

Given the other extensions being proposed here, we can retroactively reinterpret this as a simple syntactic rule: ``-XKindSignatures`` alone doesn't do implicit binding::

  ghci> :set -XKindSignatures
  ghci> :set -XNoImplicitBinds
  ghci> data Foo (a :: b)

  <interactive>:3:16: error: Not in scope: type variable ‘b’

The error message is completely different, but the effect with respect to merely whether the program was rejected is the same.

Now, both extensions (``-XPatternSignatures`` and ``-XKindSignatures``) just allow, respectively, term-level and type-level signatures, with no other functionality like implicit binding mechanisms also thrown in.

Conservativism for standardization
""""""""""""""""""""""""""""""""""

With both of these extensions being very minimal, I think they would be easy uncontroversial candidates for a new language report.
Conversely, all implicit binding constructs are very fraught with a complicated mix of upsides and downsides, we and should only standardize them with great care.

Motivation for ``-XNoImplicitBinds``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is mostly taken  from `#285`_, but modified now that @Ericson2314 realizes both extensions share the same motivations not one having more than the other.

Education
^^^^^^^^^

Some people think that implicit binding is bad for people learning Haskell.
All other variables are explicitly bound, and the inconsistency means more to learn.
Also, implicit syntax in general allows the beginner to not realize what they are doing.
What are tedious tasks for the expert may be helpful learning steps to them.

Further, most beginning students may be taught with both ``-XImplicitBinds``, ``-XNoExplicitForAll``, and ``-XNoPolyKinds``.
This means it's impossible to write forall types by any means.
Combine with ``-Wmissing-signatures`` and ``-Wmissing-local-signatures``, so inferred polymorphic types of bindings are also prohibited, and a monomorphic custom prelude, and forall types are all but expunged entirely.

@Ericson2314 doesn't wish to argue whether these choices do or don't actually help learning, but just state that some people have opinions that they do and there is no technical reason GHC cannot accommodate them.

Consistency
^^^^^^^^^^^

Notice how today that out-of-scope variables in negative position signatures are implicitly bound in *different* ways depending on whether they are type variables (in pattern signatures) or kind variables (in negative position kind signatures).
By banning implicit binding, we side-step that difference.

After all, given::

  data Foo (a :: k)

desugars to::

  data Foo @k (a :: k)

a new Haskeller might conceivably think::

  \(Foo (a :: k) -> ..

desugars to::

  \(Foo @k (a :: k) ->

or::

  \ @k (Foo (a :: k) ->

which happen to be true in some simple common cases, but are in fact incorrect in general.

That it takes a complicated example to show why these false desugarings aren't true in general make this is a huge educational stumbling block!

Unified Namespace
^^^^^^^^^^^^^^^^^

See the discussion above for ``-XPatternSignatures``.
The same exact principles apply.
Problemantic programs with implicit binding look something like these::

  t = Int
  x :: t -- sugar for 'forall t. t', not a use of 't' resolving to 'Int'
  x = 0

  t = Int
  foo (x :: t) = 0 -- sugar for 'foo = let t = _ in \(x :: t) -> 0'

(That is a new example for implicit ``forall``, and the same example for implicit pattern signature binds.)

Should the ``t`` in each ``x :: t`` cause implicit ``forall t.`` and ``let t = _ in`` to be synthesized or not?

Without ``-XImplicitBinds`` we have no choice but do the implicit desugaring that violates the unified namespace abstraction.
Concretely, in both ``x :: t`` above, the ``t`` would have to not refer to the top-level ``t = Int`` but to a fresh implicit binding, as has historically been the case.
Otherwise we would be changing the meaning of valid programs based on the presence of mere warnings (``-Wpuns`` and ``-Wpattern-binds``), which is not allowed.
This works, but isn't very satisfactory to users who, never having thought of "type versus term namespaces", are suddenly confronted with this distinction when the try to use ``t``.
``-Wpattern-binds`` should at least cache this so it is not a silent "gotcha", but it is still surprising.

With ``-XNoImplicitBinds``, however, we know no implicit bindings will be synthesized, and thus can refer to the ``t`` defined above (with the semantics of this usage given in `281#_`).
There is no gotcha, and the pun-free users can stay blissfully ignorant of type vs term variable namespacing.

``-XRequiredTypeArguments`` chooses to break with ``-XScopedTypeVariables`` to make ``t`` above refer to ``Int`` and not a freshly-quantified type variable in the second example.
We cover both interactions with ``-XImplicitForAll``, so now it is clear that ``-XRequiredTypeArguments`` and ``-XImplicitForAll`` are the extensions at all.
With ``-XNoImplicitBinds``, ``-XRequiredTypeArguments`` is a montonic extension.

Proposed Change Specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Points below up to and including the new (backward-compatible) definition of
``-XScopedTypeVariables`` come from not-yet-accepted proposal `#238`_.
``-XImplicitBinds`` is a fixed and simplified (via combining extensions) version of accepted proposal `#285`_.

1. Re-purpose deprecated extension ``-XPatternSignatures``.
   With ``-XPatternSignatures``, we allow type signatures in patterns.
   These signatures can mention in-scope type variables as variable occurrences, but can not bind type variables without the separate ``-XImplicitBinds`` extension.
   Do note that extension is on by default, however.

   The current ``-XPatternSignatures`` is just a synonym for ``-XScopedTypeVariables``.
   This change is thus not backward-compatible, but given that the existing extension is deprecated, I think this change is acceptable.

#. Introduce ``-XMethodTypeVariables``.
   With ``-XMethodTypeVariables``, type variables introduced in an instance head would scope over the bodies of method implementations.
   Additionally, type variables introduced in a class head would scope over the bodies of method defaults.

#. Introduce ``-XExtendedForAllScope``.
   With ``-XExtendedForAllScope``, any type variables mentioned in an explicit ``forall`` scopes over an expression.
   This applies to the following constructs:

   * Function bindings
   * Pattern synonym bindings (including in any ``where`` clause)
   * Expression type signatures

   Separating out ``-XExtendedForAllScope`` gets us closer to the `Contiguous Scoping Principle`_.

#. The extension ``-XScopedTypeVariables`` would imply all of the above extensions:
   ``-XPatternSignatures``, ``-XMethodTypeVariables``, and ``-XExtendedForAllScope``;
   this way, ``-XScopedTypeVariables`` does not change from its current meaning.

#. Introduce ``-XImplicitBinds``.
   With ``-XImplicitBinds``, a few sorts of implicit bindings are enabled:

   #. Implicit forall in positive position type signatures.

      With this extension, out-of-scope type variables are implicitly quantified over the following constructs.
      With ``-XNoImplicitBinds``, this implicit scoping does not happen, and the use of the variable is an error.

      Constructs affected:

      #. Type signatures for variable declarations, methods, and foreign imports & exports.
         Example:
         ``let f :: a -> a; f = ... in ...``
         becomes
         ``let f :: forall a. a -> a; f = ... in ...``.

      #. Kind signatures.
         Example:
         ``type T :: k -> Type``
         becomes
         ``type T :: forall k. k -> Type``.

      #. GADT constructor declarations.
         Example:
         ``MkG :: a -> Maybe b -> G (Either Int b)``
         becomes
         ``MkG :: forall a b. a -> Maybe b -> G (Either Int b)``.

      #. Pattern synonym signatures.
         Example:
         ``pattern P :: a -> Maybe a``
         becomes
         ``pattern P :: forall a. a -> Maybe a``.
         Implicit quantification in pattern synonyms always produces *universal* variables, never existential ones.

      #. Type annotations in expressions and ``SPECIALISE`` pragmas.
         Example:
         ``Right True :: Either a Bool``
         becomes
         ``Right True :: forall a. Either a Bool``.

      #. Types in a ``deriving`` clause.
         Example:
         ``data T deriving (C a)``
         becomes
         ``data T deriving (forall a. C a)``.

      #. Instance heads, including standalone-deriving instances.
         Example:
         ``instance Show a => Show (Maybe a)``
         becomes
         ``instance forall a. Show a => Show (Maybe a)``.

      #. Type and data family instances, as well as closed type family equations.
         Example:
         ``type instance F (Maybe a) = Int``
         becomes
         ``type instance forall a. F (Maybe a) = Int``.

      #. ``RULES`` pragmas.
         Example:
         ``{-# RULES "name" forall (x :: Maybe a). foo x = 5 #-}``
         becomes
         ``{-# RULES "name" forall a. forall (x :: Maybe a). foo x = 5 #-}``.
         (The double-\ ``forall`` syntax separates type variables like ``a`` from term variables like ``x``.)

      This is the former ``-XImplicitForAll`` from accepted but unimplemented proposal `#285`_;
      the only change is including ``RULES`` pragmas, which @Ericson2314 simply forgot to include in `#285`_ (his own admission).

   #. Implicit binds in pattern signatures:

      Out-of-scope type variables written in a pattern signature would be bound there and would remain in scope over the same region of code that term-level variables introduced in a pattern scope over.

      Example:
      ``id (x :: a) = a``
      becomes (using not-yet-approved syntax from `#523`_ to make the wildcard explicit):
      ``id = let type a = _ in \(x :: a) -> a``.

      This is the former ``-XPatternSignatureBinds`` from accepted, unimplemented proposal `#285`_.

   #. Implicit binds in kind signatures:

      Out-of-scope type variables written in a negative position kind signature (positive ones are implicit foralls) are bound as implicit capital lambdas to the left of the parameter they occur in.

      Example:
      ``data Foo (b :: a)``
      becomes
      ``data Foo @a (b :: a)``.

      This was intended to be included in the former ``-XPatternSignatureBinds`` from accepted, unimplemented proposal `#285`_, but mistakenly wasn't as these are not "pattern signatures" in the current terminology.

   This extension is on by default for backwards compatibility.

Effects
~~~~~~~

1. We could now advocate for avoiding ``-XExtendedForAllScope``, in favor of ``-XTypeAbstractions`` (introduced below).
   The other parts of the old ``-XScopedTypeVariables`` (namely, ``-XPatternSignatures`` and ``-XMethodTypeVariables``) could be considered for inclusion in a future language standard.

Alternatives
~~~~~~~~~~~~

1. We could further break down ``-XImplicitBinds``, like before.

   But fixing the drafting error would require a *third* extension, ``-XNegativeSignatureBinds``, in addition to the original two.
   This would allow more conservative defaults --- we must have Haskell98 implicit foralls but not the others which are all guarded behind language extensions today.

   However, @Ericson2314 sensed there is a weariness with too many extensions coming from this, and so didn't do it.

Costs and Drawbacks
~~~~~~~~~~~~~~~~~~~

Unified Namespacing and extension monotonicity
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Unified namespacing was touted as a beneficiary of ``-XNoImplicitBinds`` above.
But on the other hand, `270#`_ and `281#_`, the latter of which is accepted and partially implemented, adopt a model where variables in types resolving to variables defined in the term namespace as a fallback unconditionally.
This is indeed backwards compatible, however it breaks the property of ``-XImplicitBinds`` being strictly *non-forklike* in allowing only more programs, not changing the meaning of existing programs.

To wit, if ::

  t = Int
  x :: t -- out of scope, no type variable `t` in scope.
  x = 0

is an invalid program, we can *either* make it valid by saying the second ``t`` is a use or implicit bind, but we cannot do *both*.
Assuming either interpretation, switching the other is a reinterpretation of an already invalid program.

One way to reconcile this is to say ``-Wpuns`` must in fact be an extension ``-XNoPuns``, and that ``-XPuns`` and ``-XImplicitBinds`` are mutually exclusive.
This removes the "both extensions" case from the extension configuration partial order, and restore monotonicity.

But I don't think this is a good idea.
Punning is rather more controversial than expected, and it was very polite of the anti-punning / Dependent Haskell caucus to restrict themselves to a mere warning.
There is precedent for extensions like ``-XScopedTypeVariables`` changing the meaning meaning of previously-valid programs,
and ``-XImplicitBinds`` could just do so in much the same.
The "type variable usage resolving to term variable binding" use-case is very new so no existing programs would be impacted.

`281#_` also contains ``-Wterm-variable-capture``, which is the subset of ``-Wpun-bindings`` that just refers to *implicit* binding, and we could imagine turning it on more default (e.g. with ``-Wcompat`` as stepping stone).
That would prepare us for a world where implicit binding only happens when a variable is unbound in both namespaces, and in that world ``-XImplicitBinds`` is one again monotonic.

Type arguments in constructor patterns
--------------------------------------

.. _pattern-type-args:

This is an update to accepted, implemented proposal `#126`_,
incorporating the logic of not-yet-accepted amendment `#291`_.

The original proposal `#126`_ is indeed implemented and released,
but the implementation is not faithful to the specification around type variables that are already in scope.
The original proposal says that, if ``a`` is already in scope, then ``f (Just @a x) = ...`` is an *occurrence* of the in-scope ``a``.
By contrast, the implementation errors in this case.

Not-yet-accepted amendment `#291`_ says that type variables scope just like term variables: they can be shadowed.
Accordingly, ``f (Just @a x) = ...`` would always, unconditionally bind a new type variable ``a``, possibly shadowing any in-scope type variable ``a``.
This design supports the `Visibility Orthogonality Principle`_,
which states that the presence of an ``@`` should affect only whether a thing is visible or not, not other characteristics (like its shadowing and scoping behavior).
Additionally, this choice edges us closer to the `Lexical Scoping Principle`_,
because we no longer have to check whether ``a`` is in scope before identifying the ``a`` in ``f (Just @a x) = ...`` is a binding site or an occurrence.

The other change in this restatement is the use of new extension ``-XTypeAbstractions`` instead of the current status of piggy-backing on the combination of ``-XTypeApplications`` and ``-XScopedTypeVariables`` (*both* need to be enabled today).
This proposal suggests that initially ``-XScopedTypeVariables`` and ``-XScopedTypeVariables`` should jointly enable type applications in constructor patterns; but that this combination doing so should be deprecated, and at some later point removed.
We have conflicting principles at play:

- New experimental functionality should not be gated under older established extensions

- Breaking changes under established extensions --- even if it only affects experimental functionality that should have not been there in the first place --- should be avoided.

Given these too things, a small deprecation cycle / migration path to ``-XTypeAbstractions`` seems the best we can do.

Motivation
~~~~~~~~~~

This is taken directly from `#126`_.

``TypeApplications`` are a convenient and natural way to specifying types of polymorphic functions.
Consider::

  data Foo a where MkFoo :: forall a. a -> Foo a

With ``TypeApplications``, I can replace the somewhat clumsy ``MkFoo (x :: ty)`` with ``MkFoo @ty x``.
Seen this way, explicit type applications are merely an alternative syntax for type signatures.

At the moment, this only works in terms, but not in patterns:
We can use type signatures in patterns (if ``PatternSignatures`` or ``ScopedTypeVariables`` are enabled), but not type applications.
Given the strong relation between these syntactic forms, this is odd – why can I write::

    foo (MkFoo (x :: ty)) = …

but not::

    foo (MkFoo @ty x) = …

This proposal fills this gap:
It allows type applications in patterns, and specifies them to behave “just like type signatures”.

The intention of the following specification is that the following holds:
For a constructor with type like ``C :: forall a. a -> …`` the meaning of ``C @ty x`` should coincide with the existing form ``C (x :: ty)``.

Proposed Change Specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Introduce a new extension ``-XTypeAbstractions``
   (This extension is further extended in the next part of this proposal.)

#. When ``-XTypeAbstractions`` is enabled, allow type application syntax in constructor patterns.

   Concretely, the grammar goes from ::

     pat → gcon apat1 … apatk
         …

   to ::

       pat → gcon tyapp_or_pat1 … tyapp_or_patk
           …

       tyapp_or_pat → '@' atype    -- '@' is in prefix position
                    → apat

#. For backward compatiblity, *also* accept type application syntax in constructor patterns if both ``-XScopedTypeVariables`` and ``-XTypeApplications`` are enabled, but ``-XTypeAbstractions`` is not.
   In that case, emit a warning, stating that type applications in constructor patterns should be enabled with ``-XTypeAbstractions``, and that the temporary expedient of enabling it by the combination of ``-XScopedTypeVariables`` and ``-XTypeApplications`` will be removed.

   After 2 releases remove clause (b); ``-XTypeAbstractions`` will be the only way to enable this feature.

#. Type applications in constructor patterns do *not* affect whether the pattern-match is successful.

#. Type applications in constructor patterns must correspond to ``forall … .`` quantifications in the declared constructor or pattern synonym type.
   (Right now, pattern synonyms require all such quantifications to occur before any term arguments,
   but accepted proposal `#402`_ allows these quantifications to occur in any order in data constructors.)

#. In accordance with the `Visibility Orthogonality Principle`_,
   the rules that determine whether a variable occurrence is a binding site or a use site are not affected by the presence of a ``@``.
   At the time of writing, the rules for patterns and pattern signatures are as follows:

   * Outside pattern signatures,
     variable occurrences are considered binding sites,
     shadowing any other in-scope variables.
     It is an error to bring the same type variable into scope in two (or more) places within the same match group.

   * Inside pattern signatures (i.e. on the right-hand side of ``pat :: sig``),
     occurrences of in-scope type variables are usages,
     whereas occurrences of out-of-scope type variables create implicit bindings.
     It is allowed to mention the same out-of-scope variable more than once.

   Generalize these rules to apply not only to pattern signatures but also to
   kind signatures in type applications in constructor patterns
   (and to kind signatures in type variable patterns defined in the "Type arguments in lambda patterns" section).

#. Typing follows the rules in `Type Variables in Patterns`_.
   In particular, see Figure 7, which we modify here in two ways:

   1. Ignore the ``isInternalTypeVar`` premise, which was done away with by accepted proposal `#128`_.

   #. Change the ``cs = ftv(τ's) \ dom(Γ)`` premise to be ``cs = ftv(τ's)`` and ``cs # dom(Γ)``.
      That is, instead of making the new type variables ``cs`` be only those that are not already in scope,
      require all the type variables to be fresh (shadowing is possible, but left implicit here).

#. A wildcard ``_`` as a type argument says simply to skip that argument;
   it does not trigger any behavior associated with partial type signatures.
   In particular, ``-XPartialTypeSignatures`` is not necessary, and no diagnostic is produced.

Examples
~~~~~~~~

There are examples of pattern signatures using type variables which are already in scope::

    foo :: forall b. Maybe b -> ()
    foo @a (_ :: Maybe a) = ()

    bar :: forall b. Maybe b -> ()
    bar (Just @a (_ :: a)) = ()

    baz :: forall b. b ~ () -> ()
    baz @b () = ()
      where
        () :: b = ()

These examples are all accepted with ``-XPatternSignatures``.

This is an example of a pattern signture binding a type variable::

    id (x :: a) = x :: a

This example is allowed with ``-XScopedTypeVariables`` as today, but disallowed with just ``-XPatternSignatures``.

Here is an example (taken from `#15050 <https://gitlab.haskell.org/ghc/ghc/issues/15050#note_152286>`_)::

    type family F a where
      F Bool = Int
    data T a where
      MkT :: forall b a. b ~ F a => b -> T a

    foo :: T Bool -> ()
    foo (MkT @Int _) = ()

This should type-check, because the following code does::

    foo :: T Bool -> ()
    foo (MkT (_ :: Int)) = ()

Note that the data constructor expects up-to two type arguments (``forall b a.…``), but we are passing only one type argument, which then corresponds to the *first* type argument of of the data constructor.

A more complex example is this (also inspired by `#15050 <https://gitlab.haskell.org/ghc/ghc/issues/15050>`_)::

    data T a where
      MkT1 :: forall a.              T a
      MkT2 :: forall a.              T (a,a)
      MkT3 :: forall a b.            T a
      MkT4 :: forall a b. b ~ Int => T a
      MkT5 :: forall a b c. b ~ c => T a

    foo :: T (Int, Int) -> ()
    foo (MkT1 @(Int,Int))  = ()
    foo (MkT2 @x)          = (() :: x ~ Int => ())
    foo (MkT3 @_ @x)       = (() :: x ~ x => ())
    foo (MkT4 @_ @x)       = (() :: x ~ Int => ())
    foo (MkT4 @_ @Int)     = ()
    foo (MkT5 @_ @x @x)    = (() :: x ~ x => ())    -- not accepted

All (save the last) of these equations type-check
(just like they would if added value arguments of type ``a``, ``b``,... to the constructors and turned the type applications into type signatures).
The last is rejected because it tries to bind ``x`` twice in the same pattern, in just the same way as a pattern binding the same term variable twice is rejected.

Note that the ``@_`` are not treated like partial type signatures:
they do not create any diagnostics;
they are merely placeholders for type variables not bound.

Note that it is usually a type error to supply a non-tyvar type, or an in-scope tyvar, in an existential position (e.g. ``MkT3 @_ @Int`` is wrong),
unless the data constructor has constraints that equate the existential type variable to some type (as in the equations involving ``MkT4`` and ``MkT5`` above).

::

  {-# LANGUAGE ExtendedForAllScope #-}
  data Ex = forall a. MkEx a
  f2 :: forall b. b -> Ex -> Int
  f2 y (MkEx @b z) = ...

This is rejected under `#126`_, as it appears to insist that the existential type packed in ``MkEx`` is the same as the type argument passed to ``f2``.
On the other hand, this is accepted by the current proposal, allowing the existential ``b`` to shadow the ``b`` brought into scope by the ``forall``.

This shadowing behavior mimics what happens with term variables in patterns.

::

  f :: Maybe Int -> Int
  f (Nothing @a) = (4 :: a)
  f (Just @a _)  = (5 :: a)

This is accepted.
The type variable ``a`` is bound to ``Int``, by pattern-matching.

Here is an example of pattern signatures within a type abstraction in a pattern::

   {-# LANGUAGE ScopedTypeVariables #-} -- for pattern signature bindings
   data Proxy a = P
   g2 :: Proxy (Nothing @(a, a)) -> ()
   g2 (P @(Nothing :: Maybe (t, t))) = ()

Note multiple occurrences of ``t`` in the pattern. Normally, we would disallow
multiple bindings of a single variable::

   f1 (P x) (P x) = x               -- Rejected (multiple bindings of ‘x’)
   f2 (P @a x) (P @a y) = x         -- Rejected (multiple bindings of ‘a’)

Pattern and kind signatures, however, are not subject to this restriction,
since variable occurrences in pattern signatures are considered usages (not bindings)::

   {-# LANGUAGE ScopedTypeVariables #-} -- for pattern signature bindings

   g1 (P x :: Proxy (a,a)) = x               -- Accepted (multiple occurrences of ‘a’ notwithstanding)

   g2 :: Proxy (Nothing @(a, a)) -> ()
   g2 (P @(Nothing :: Maybe (t, t))) = ()    -- Accepted (multiple occurrences of ‘t’ notwithstanding)

``-XNoImplicitBinds``
^^^^^^^^^^^^^^^^^^^^^

Many of these examples also use ``-XTypeAbstractions`` from here and Proposal `#425`_.

Basic examples
""""""""""""""

#. ::

     f :: t -> ... -- error: `t` is not bound
     f x = ...

   This could be rewritten as::

     f :: forall t. t -> ...
     f x = ...

#. ::

     f (x :: t) = ... -- error: `t` is not bound

   This could be rewritten as::

     f :: forall t0. ...
     f @t (x :: t) = ... -- OK

#. ::

     data Some where
       MkSome :: forall t. t -> Some

     f (MkSome (x :: t)) = ... -- error: `t` is not bound

   This could be rewritten as::

     data Some where
       MkSome :: forall t. t -> Some

     f (MkSome @t x) = ... -- OK

Not just term definitions
"""""""""""""""""""""""""

Besides top level term bindings, we currently have signatures with implicit forall quantification for expressions, data declerations, family declarations, and instances [#class-forall]_.
This proposal applies to all alike:

#. ::

     ... (id :: t -> t) -- error: `t` is not bound

   This could be rewritten as::

     ... (id :: forall t. t -> t) -- OK

#. ::

    data D :: k -> Type where -- error: `k` is not bound

   This could be rewritten as::

    data D :: forall k. k -> Type where -- OK

#. ::

    type family F :: k -> Type where -- error: `k` is not bound

   This could be rewritten as::

    type family F :: forall k. k -> Type where -- OK

#. ::

    instance Eq t => C t where -- error: `t` is not bound

   This could be rewritten as::

    instance forall t. Eq t => C t where -- OK

When ``-XStandaloneKindSignatures`` is on, these new standalone signatures are affected as well.

#. ::

     type F :: k -> Type -- error: `k` is not bound
     data F _ = ...

   This could be rewritten as::

     type F :: forall k. k -> Type -- OK
     data F _ = ...

#. ::

     type F :: k -> k -- error: `k` is not bound
     type family F where

   This could be rewritten as::

     type F :: forall k. k -> k -- OK
     type family F where

#. ::

     type C :: (k -> Type) -> Constraint -- error: `k` is not bound
     class C f where

   This could be rewritten as::

     type C :: forall k. (k -> Type) -> Constraint -- OK
     class C f where

#. ::

     type D :: k -> Type -- error: `k` is not bound
     data D where

   This could be rewritten as::

     type D :: forall k. k -> Type -- OK
     data D where

Pattern signatures in GADT declarations, family declarations, and class declarations are also restricted.
I'll first use a hypothetical yet-unproposed ``@``-abstraction syntax to "fix" these examples to demonstrate the analogy to the previous examples.
Then I'll put the inline signature or top-level signature workaround that exists today.

#. ::

     data D (y :: x) (z :: y) where -- error: `x` is not bound, `y` and `z` are fine

   Could be be rewritten as::

     data D @x (y :: x) (z :: y) where -- OK

#. ::

     type family F (y :: x) (z :: y) where -- error: `x` is not bound, `y` and `z` are fine

   Could be be rewritten as::

     type family F @x (y :: x) (z :: y) where -- OK

#. ::

     class Eq a => C (y :: x) (z :: y) where -- error: `x` is not bound, `y` and `z` are fine

   Could be be rewritten as::

     class Eq a => C @x (y :: x) (z :: y) where -- OK

   Note that since there is no ``class F :: ...`` syntax analogous to ``data F :: ...``,
   so ``-XStandaloneKindSignatures`` are the only way to write explicitly kind-polymorphic classes.

Note that the variables to the left of the ``::`` are deemed explicit bindings analogous to ``f (y :: x) (z :: z) = ...`` and permitted.
However ``x`` to the right of the ``::`` is a use, not otherwise bound, and thus implicit binding today.
It is not permitted as-is, and must be explicitly bound or discarded as done in the working alternatives.

Effects
~~~~~~~

1. The ability to bind existential variables via a construct such as this is necessary to support the `Explicit Variable Principle`_.

#. The previous proposal `#126`_ followed the paper more closely, bringing into scope only those variables that are not already in scope.
   However, given that this behavior is triggered only by a ``@``, doing this is in violation of the `Visibility Orthogonality Principle`_.
   This newer version instead labels all variables as binding sites.

#. Having type variables have the same behavior as term variables with respect to shadowing (and repeated binding) upholds the `Visibility Orthogonality Principle`_.
   In addition, the fact that type variables are unconditionally brought into scope upholds the `Lexical Scoping Principle`_, part (a).

#. It may be useful to write a variable occurrence to instantiate a universal argument.
   This proposal prevents this possibility.
   We expect a future proposal to remedy this problem, with either a modifier or some symbol.
   For example, perhaps we would say e.g. ``f (Just @(*a) x) = ...`` to denote an occurrence of already-in-scope type variable ``a``.

#. Backward-compatibility with the current implementation,
   which merely requires both ``-XScopedTypeVariables`` and ``-XTypeApplications`` to be in effect and not any extension dedicated to this feature,
   is preserved.
   But whenever the old way of enabling this feature is used, a deprecation warning will be issued.

#. After 2 releases of deprecation with the warning, the above implication is removed.
   That cleans up new experimental functionality from leaking under established extensions.
   This *is* a breaking change, but with the advanced notice given via the warning, the costs are reduced to the point that the benefits are deemed to outweigh them.

Type arguments in lambda patterns
---------------------------------

.. _type-vars-in-lambda:

This is a restatement of accepted, unimplemented proposal `#155`_, as amended by not-yet-accepted `#238`_.
It introduces the ability to bind type variables by a lambda, controlled by the ``-XTypeAbstractions`` extension.

Motivation
~~~~~~~~~~

This is adapted from `#238`_.

There are several motivating factors for this addition:

1. There are cases where a ``Proxy`` is necessary in order for a higher-rank function argument to access a type variable,
   such as::

     type family F a

     higherRankF :: (forall a. F a -> F a) -> ...

     usage = higherRankF (\ (x :: F a) -> ...)

   The ``(x :: F a)`` pattern signature does not work, because ``F`` is not injective.
   There is no way to be sure that the ``a`` in ``usage`` is meant to match the ``a`` in ``higherRankF``.
   Currently, there is simply no way for ``usage`` to get access to the type variable written in the signature for ``higherRankF``.
   This code would have to be rewritten to use ``Proxy``.
   Under this proposal, however, ``usage`` could be simply ::

     usage = higherRankF (\ @a x -> ...)

   Ah.
   That's better.

2. With `#126`_, we can bind type variables in constructor patterns, allowing us to easily capture existentials.
   The only other place a type variable can enter scope is in a function definition, and so it's only logical to extend `#126`_ to do so.
   Furthermore, doing so is necessary to uphold the `Explicit Variable Principle`_.

3. ``-XExtendedForAllScope``\'s mechanism for binding type variables using a ``forall`` in a signature has never sat well with some.
   (I'm in the some, but I'm not the only one.)
   A type signature can appear arbitrarily far away from a function definition, and (to me) the use of ``forall`` to induce scoping over the function definition is far from intuitive.
   Using this new syntax, all the action happens in the function definition.
   This allows for the possibility of usefully disabling ``-XExtendedForAllScope`` while still binding type variables, helping to support the `Contiguous Scoping Principle`_.

4. See crowd-sourced example `here <https://github.com/ghc-proposals/ghc-proposals/pull/155#issuecomment-459430140>`_.

Proposed Change Specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. With ``-XTypeAbstractions``,
   introduce a new form of pattern (cf. The `Haskell 2010 Report`_)::

     apat → … | '@' tyvar | '@' '(' tyvar '::' kind ')' | '@' '_'   -- '@' is a prefix occurrence

   Conveniently, ``apat``\ s are used both in function left-hand sides and in lambda-expressions, so this change covers both use-cases.

   (Note that this does not subsume the new grammar for constructor patterns, which allow *types*, not just variables.)

#. In accordance with the `Visibility Orthogonality Principle`_,
   the rules that determine whether a variable occurrence is a binding site or a use site are not affected by the presence of a ``@``.
   That is, name resolution in kind signatures in type variable patterns follows the rules for pattern signatures.
   (The rules for pattern signatures are given in the "Type arguments in constructor patterns" section).

#. A type variable pattern is not allowed in the following contexts:

   1. To the right of an as-pattern
   #. As the top node in a lazy (``~``) pattern
   #. As the top node in a ``lpat`` (that is, to the left of an infix constructor,
      directly inside a parenthesis, as a component of a tuple,
      as a component of a list, or directly after an ``=`` in a record pattern)

#. Typing rules for the new construct are as in a `recent paper <https://richarde.dev/papers/2021/stability/stability.pdf>`_:
   see ETm-InfTyAbs, ETm-CheckTyAbs, Pat-InfTyVar, and Pat-CheckTyVar, all in Figure 7.
   While the typeset versions remain the official typing rules, I will summarise the different rules below.

   **Background**.
   GHC implements *bidirectional* type-checking, where we sometimes know what type to expect an expression to have.
   When we know such a type (for example, because we have a type signature, or an expression is an argument to a function with a known type), we say we are in *checking* mode.
   When we do not know such a type
   (for example, when we are inferring the type of a ``let``\ -binding or the type of a function applied to arguments),
   we say we are in *synthesis* mode.
   The `Practical Type Inference <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/putting.pdf>`_ paper gives a nice, Haskell-oriented introduction.

   1. In synthesis mode, when examining ``\ @a -> expr``, we simply put ``a`` in scope as a fresh skolem variable (that is, not equal to any other type) and then check ``expr``.
      (Presumably, ``expr`` uses ``a`` in a type signature.)
      When we infer that ``expr`` has type ``ty``, the expression ``\ @a -> expr`` has type ``forall a. ty``.
      Example: ``\ @a (x :: a) -> x`` infers the type ``forall a. a -> a``.
      (For this example, we note that ``\ @a (x :: a) -> x`` is a short-hand for ``\ @a -> \ (x :: a) -> x``.)

   #. In checking mode,
      when examining ``\ @a -> expr`` against type ``ty``,
      we require that ``ty`` has the shape ``forall a. ty'``,
      where ``a`` is a *specified* variable (possibly after skolemising any *inferred* variables in ``ty``),
      renaming the bound variable as necessary to match the name used in the expression.
      We then check ``expr`` against type ``ty'``.

   #. In synthesis mode,
      when examining a function argument ``@a`` to a function ``f``,
      we bring ``a`` into scope as a fresh skolem variable and check the remainder of the arguments and the right-hand side.
      In the type of ``f``, we include a ``forall a.`` in the spot corresponding to the type variable argument.

      If there are multiple equations, each equation is required to bind type variables in the same locations.
      (If this is burdensome, write a type signature.)
      (We could probably do better,
      by inferring the maximum count of bound type variables between each required argument and then treating each set of bound type variables as a prefix against this maximum,
      but there is little incentive.
      Just write a type signature!)

   #. In checking mode,
      when examining a function argument ``@a`` to a function ``f`` with type signature ``ty``,
      we require the corresponding spot in the type signature to have a ``forall a`` (possibly renaming the bound variable).
      The type variable ``a`` is then brought into scope and we continue checking arguments and the right-hand side.

      Multiple equations can bind type variables in different places, as we have a type signature to guide us.
      *Exception:*
      The number of type variables bound after all term patterns must be the same for all equations;
      discussion below.

#. Typing rules for pattern synonym bindings are complicated, as usual.

   1. A visible type abstraction in a pattern synonym binding that lacks a type signature is rejected.
      (While we could, at some cost, work out what should happen here, please just use a type signature.)

   #. (Background information; no new specification here.)
      Pattern synonym type signatures have a restricted form that looks like this::

         pattern P :: forall universal_tvs.   required_context =>
                      forall existential_tvs. provided_context =>
                      arg1 -> arg2 -> ... ->
                      result

      `The GHC manual <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/pattern_synonyms.html#typing-of-pattern-synonyms>`_ has the details for how parts of this signature can be left out;
      I will not repeat these rules here.
      The key observation is that all quantified type variables occur *before* any required term-level arguments.

      Furthermore, pattern synonym bindings may be specified in two parts, for explicit bidirectional pattern synonyms::

         pattern P <- pat
           where P = expr

      Call the top line the *pattern synonym pattern binding*,
      while the second line is the *pattern synonym expression binding*.

      In an implicitly bidirection pattern synonym binding,
      the pattern synonym pattern binding and pattern synonym expression binding are written with one bit of syntax.
      For the purposes of this proposal, though, we consider type-checking this bit of syntax *twice*,
      once as a pattern synonym pattern binding, and once as a pattern synonym expression binding.

   #. With ``-XTypeAbstractions``, a pattern synonym pattern binding may include any number of type abstractions (such as ``@a`` or ``@_``) directly after the pattern synonym name.
      (Such a binding must be written in prefix notation, not infix.)
      These bindings correspond to a prefix of the *specified* *universal* type variables in the pattern synonym's type.
      It is an error to write more type abstractions than there are specified universal variables.

      Each type abstraction binds a local name to the corresponding universal type variable.
      These names are available in the right-hand side (after the ``<-`` or ``=``).

      (Existentials are excluded here because an existential type variable is bound by the pattern in the right-hand side.
      There appears to be no motivation for being able to name these on the left.)

      The rules for the usage of such variables on the right-hand side are unchanged from the way scoped type variables work in pattern synonyms today.

   #. With ``-XTypeAbstractions``,
      a pattern synonym expression binding may include any number of type abstractions (such as ``@a`` or ``@_``) directly after the pattern synonym name.
      (Such a binding must be written in prefix notation, not infix.)
      These correspond to a prefix of the concatentation of the specified universal and specified existential type variables written in the pattern synonym type signature.
      It is an error to write more type abstractions than there are specified universal and specified existential type variables.

      Each type abstraction binds a local name to the corresponding universal or existential type variable.
      These names are available in the right-hand side (after the ``=``).

      (Existentials are included here because a pattern synonym used as an expression takes existentials as arguments from call sites,
      and it is sensible to bind these on the left.)

      The rules for the usage of such variables on the right-hand side are just as they exist for ordinary function bindings.

   .. _fraught-relationship:

#. ``-XTypeAbstractions`` and ``-XExtendedForAllScope`` have a fraught relationship,
   as both are trying to accomplish the same goal via different means.
   Here are the rules keeping this sibling rivalry at bay:

   1. ``-XExtendedForAllScope`` does not apply in expression type signatures.
      Instead, if users want a type variable brought into scope, they are encouraged to use ``-XTypeAbstractions``.
      (It would not be hard to introduce a helpful error message instructing users to do this.)

   #. If ``-XExtendedForAllScope`` is enabled,
      in an equation for a function definition for a function ``f``
      (and similar for pattern synonym pattern bindings and pattern synonym expression bindings):

      * If ``f`` is written with no arguments or its first argument is not a type argument
        (that is, the next token after ``f`` is not a prefix ``@``),
        then ``-XExtendedForAllScope`` is in effect and brings type variables into scope.

      * Otherwise, if ``f``\'s first argument is a type argument, then ``-XExtendedForAllScope`` has no effect.
        No additional type variables are brought into scope.

Examples of new behavior of scoped type variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   f :: forall a. a -> a
   f @b x = (x :: a)   -- rejected, because -XExtendedForAllScope is disabled here

   g :: forall a. a -> a
   g @a x = (x :: a)   -- accepted with -XTypeAbstractions

   h = ((\x -> (x :: a)) :: forall a. a -> a)
     -- accepted with previous -XScopedTypeVariables, but rejected
     -- now

   i = ((\ @a x -> (x :: a)) :: forall a. a -> a)
     -- accepted with -XTypeAbstractions

Note that turning off ``-XExtendedForAllScope`` with ``-XTypeAbstractions`` is necessary if we think about where type variables are brought into scope.
Are they brought into scope by the ``forall``? Or by the ``@a``?
It can't be both, as there is no sensible desugaring into System F.
Specifically, if we have ``expr :: forall a. ty``, that gets desugared into ``/\ a -> expr``.
If we have ``(\ @a -> expr) :: forall b. ty``, what does it get desugared into?
It would have to be ``/\ b -> /\ a -> expr``, but then ``b`` and ``a`` are different.

Here might be another way of thinking about it.
Suppose we're checking ``expr`` against the pushed-down (known) type ``forall a. ty``.
If we bring ``a`` into scope, what type do we check ``expr`` against?
Is it ``forall a. ty`` again?
That's very awkward if ``a`` is *already* in scope.
If we check ``expr`` against ``ty`` and ``expr`` looks like ``\ @b -> expr'``,
then we check ``\ @b -> expr'`` against ``ty`` -- not against ``forall a. ty``.

Further examples
~~~~~~~~~~~~~~~~

Here are two real-world examples of how this will help, courtesy of @int-index:

1. It would be useful to eliminate ``Proxy`` in this style of proof::

     class WithSpine xs where
       onSpine ::
         forall r.
         Proxy xs ->
         ((xs ~ '[]) => r) ->
         (forall y ys.
           (xs ~ (y : ys)) =>
           WithSpine ys =>
           Proxy y ->
           Proxy ys ->
           r) ->
         r

   Code taken `from here <https://github.com/int-index/caps/blob/2f46fc6d5480bdef0a17f64359ad6eb29510dba4/src/Monad/Capabilities.hs#L273>`_.

   Compare:

   a. ``@``\-style: ``withSpine @xs (onNil ...) (\ @y @ys -> onCons ...)``
   b. ``Proxy``\-style: ``withSpine (Proxy :: Proxy xs) (onNil ...) (\(Proxy :: Proxy y) (Proxy :: Proxy ys) -> onCons ...)``

2. From `reflection <https://hackage.haskell.org/package/reflection-2.1.4/docs/Data-Reflection.html#v:reify>`_::

     reify :: forall a r. a -> (forall (s :: *). Reifies s a => Proxy s -> r) -> r

   Compare:

   a. ``@``\-style: ``reify (\ @s -> ...)``
   b. ``Proxy``\-style: ``reify (\(Proxy :: Proxy s) -> ...)``

Examples of varying type variables among equations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. _varying-type-lambda-examples:

::

     f1 @a (x :: a) = x    -- accepted

     f2 @a True  x (y :: a) = x
     f2 @_ False x y        = y   -- accepted

     f3 @a True  x (y :: a) = x
     f3    False x y        = y   -- rejected: too confusing to have different type variable bindings

     f4 :: Bool -> a -> a -> a
     f4 @a True  x (y :: a) = x
     f4    False x y        = y   -- accepted: the type signature allows us to do this

     f5 :: Bool -> forall a. a -> a -> a
     f5 True @a x (y :: a) = x
     f5 False   x y        = y    -- accepted

     f6 :: Bool -> forall a. a -> a -> a
     f6 True  @a = const @a @a
     f6 False @_ = flip const     -- accepted: the type variables after term variables line up

     f7 :: Bool -> forall a. a -> a -> a
     f7 True  @a = const @a @a
     f7 False    = flip const     -- rejected: variable tail of type variables

Effects
~~~~~~~

1. An astute reader will note that I put spaces after all my lambdas.
   That is because ``\@`` is a valid name for a user-defined operator.
   This proposal does not change that.
   If you want to bind a type variable in a lambda, you must separate the ``\`` from the ``@``.

#. This proposal makes abstracting over type variables the dual of applying types with visible type application.

#. Accepted proposal `#99`_ introduces the possibility of user-written specificity annotations (``forall {k} ...``).
   An *inferred* variable,
   including one written by the programmer using this new notation,
   is not available for use with any form of visible type application, including the one proposed here.
   If you have a function ``f :: forall {k} (a :: k). ...``,
   you will have to rely on the behavior of ``-XExtendedForAllScope`` to bring ``k`` into scope in ``f``\'s definition,
   or you will have to use a pattern signature.
   This is regrettable but seems an inevitable consequence of the ``{k}`` notation.

#. This delivers the `Explicit Variable Principle`_, meaning we can rid of ``Proxy``.

#. The `last set of examples <#varying-type-lambda-examples>`_ above show how we deal
   with functions with multiple equations with varying type variable bindings.

   No variation is allowed when there is no type signature, as doing so seems challenging (though possible),
   and we can just encourage a type signature.

   With a type signature, variation is allowed (example ``f4``, with one exception:
   the tail of arguments must be consistent.
   The reason for this restriction can be understood in thinking about ``f7``:
   in the right-hand side of the second equation,
   is the expected type ``forall a. a -> a -> a`` or ``a -> a -> a``, with ``a`` already bound?
   This choice matters: perhaps the right-hand side is ``\ @a -> flip (const @a @a)``.
   Or, if we have a type like ``Bool -> forall a b. ...``, are both ``a`` and ``b`` bound to the left of the ``=``?
   We could, for example, look at all equations and bind a number of variables equal to the maximum number of type variables across all equations.
   But re-consider ``f7`` again:
   if we just wrote the second equation without the first, that would have a different
   meaning than writing the equation along with the first.
   That is, we might imagine this being accepted::

     f7' :: Bool -> forall a. a -> a -> a
     f7' False = \ @a -> flip (const @a @a)

   but this being rejected as ill-typed::

     f7'' :: Bool -> forall a. a -> a -> a
     f7'' False   = \ @a -> flip (const @a @a)
     f7'' True @a = const @a @a

   This is strange, where the addition of a new equation violates the typing of a previous one (that was otherwise fine).
   To avoid this strangeness, we simply forbid varying the number of bound variables in the tail.

   Note that we do not want to forbid binding variables in the tail generally, because someone might want ::

     myId :: forall a. a -> a
     myId @a = id @a

   which binds a variable in the tail.
   Happily, definitions like this will have only one equation.

#. (technical) The `Visible Type Applications`_ (VTA) paper defines the behavior about what to do when checking against a polytype: it says to deeply skolemize.
   However, eager deep skolemization will spell trouble for this extension, as we need the lambdas to see the ``forall``\s.
   The end of the Section 6.1 in the `extended VTA <https://richarde.dev/papers/2016/type-app/visible-type-app-extended.pdf>`_ paper discusses why we do eager deep skolemization:
   essentially, the alternative would be to do type generalization at inflection points between checking and inference mode, right before doing the subsumption check.
   Type generalization is hard in GHC, though, and so the paper avoided it.
   In order to implement this proposal, we'll have to work out how to do this.

Costs and Drawbacks
~~~~~~~~~~~~~~~~~~~

1. This part of the proposal is *not* backward-compatible with today's ``-XScopedTypeVariables``,
   because it rejects expressions like ::

     ((\x -> (x :: a)) :: forall a. a -> a)

   which are accepted today.
   No migration period is proposed, because it is very hard to imagine how ``-XTypeAbstractions`` and ``-XExtendedForAllScope`` should co-exist peacefully here.
   Instead, we can issue a specific error message telling users how to migrate their code in this case.

   My hope is that constructs such as this one are rare and would not impact many users.

   If necessary, we could imagine taking the expression ``expr :: forall ... . ty`` and looking proactively to see whether ``expr`` ever uses a type variable pattern from this proposal.
   If not, ``-XExtendedForAllScope`` could trigger (and we issue a warning with ``-Wcompat``).
   But, if a type argument appears anywhere in ``expr``, then ``-XExtendedForAllScope`` is disabled.
   This would be backward-compatible, but unfortunately non-local and annoying.
   I prefer just to skip this migration step.

Alternatives
~~~~~~~~~~~~

1. We could add the following specification item if we like:

   **Specification**

   If ``-XTypeAbstractions`` is in effect, then a function binding may use ``@(..)`` on its left-hand side.
   Here is the BNF (cf. the `Haskell 2010 Report`_, Section 4.4.3), recalling that braces mean "0 or more"::

     funlhs  →  var apat { apat }
             |  pat varop pat
             |  '(' funlhs ')' apat { apat }
             |  funlhs '@' '(' '..' ')'

   The last line is new, and we assume the ``@`` is in prefix form.
   This construct is available only when the function being defined has a type signature.
   The new construct brings into scope all type variables brought into scope at that point in the signature.
   Note that implicitly quantified type variables are brought into scope at the top of a signature, and so ::

     f :: a -> b -> a
     f @(..) = -- RHS

   would have ``a`` and ``b`` in scope in the ``RHS``.

   The ``@(..)`` construct works for both *specified* and *inferred* variables,
   and is additionally available in pattern synonym pattern bindings
   (where it brings into scope only universals) and pattern synonym expression bindings (where it brings into scope both universals and existentials).
   (In an implicitly bidirectional pattern synonym, the ``@(..)`` brings into scope only universals.)

   **Discussion**

   This new notation seems like a convenient middle ground,
   allowing for an easy transition from the old-style ``-XScopedTypeVariables`` to the newer ``-XTypeAbstractions``.
   It brings the *inferred* variables (from `#99`_) into scope, quite conveniently.
   This new notation also allows type variables to be brought into scope without the ``forall`` keyword in the type,
   in case the user does not want to trigger ``forall``\ -or-nothing behavior.

   Note that this notation is forward compatible with visible dependent quantification in terms (`#281`_)::

     f :: foreach (count :: Int) (label :: String) (is_paid_for :: Bool) -> Invoice
     f (..) = -- here, count, label, and is_pair_for are all in scope

   This style allows for more perspicuous types while avoiding redundancy.
   The particular example here uses ``foreach`` to denote arguments that are available at runtime,
   but nothing about ``foreach`` is required to make this all work (as far as scoping is concerned).

   Accepting the ``@(..)`` syntax does *not* entail accepting this new, separate ``(..)`` syntax, though it is good to know that the idea is forward compatible.

   A ``@(..)`` argument counts as a type argument when asking whether ``-XExtendedForAllScope`` affects a function equation.

   The new ``@(..)`` notation does *not* work with expression type signatures, lambda-expressions, or anywhere other than a function binding with a type signature.
   This is because doing so would require propagating type information into scoping, which is problematic.

   Some have argued on GitHub that it may be best to hold off the ``@(..)`` until we gain more experience here:
   adding new features is easier than removing them.
   While I agree that this could be done,
   the ``@(..)`` construct makes for a very easy migration from today's ``-XScopedTypeVariables`` and is thus tempting to be around from the start.
   I don't feel strongly but would personally vote for inclusion.

#. We could simply make ``-XExtendedForAllScope`` and ``-XTypeAbstractions`` incompatible.
   If the user specifies both, reject the program.

   I find this approach less convenient, as it prevents an easy migration from the status quo
   (with ``-XScopedTypeVariables`` enabled often, including in ``-XGHC2021``)
   to a future relying more on ``-XTypeAbstractions``.
   The approach described in this proposal means that enabling ``-XTypeAbstractions`` affects nothing about ``-XExtendedForAllScope``,
   until a user tries to actually use a type abstraction.
   That's a nice property.

Effects and Interactions
------------------------

The effects of this proposal are written out in the individual sections.
Here, I summarize the effects on the principles_.

#. The `Explicit Variable Principle`_ is made to hold, by allowing explicit binders for type variables for existentials and the variables bound by an inner ``forall`` in a higher-rank
   type.

#. The `Lexical Scoping Principle`_ outside of Template Haskell is made to hold with ``-XNoImplicitBinds``

   Indeed, the purpose of ``-XNoImplicitBinds`` is to be the single extension which is both necessary and sufficient to do this.
   (The Template Haskell issue is something would be solved by more rigorous notations of hygiene. That has little to do with ``-XNoImplicitBinds`` as currently specified, and is much more work of a very different nature.)

   The `Lexical Scoping Principle`_, part (a), is upheld.
   Binders occur in patterns, after ``forall``, in
   ``let`` declarations, and a few other discrete places in the AST -- and
   nowhere else. In particular, binders do not occur in pattern signatures.

   The `Lexical Scoping Principle`_ part (b) is made to hold,
   by describing pattern-signature binds as occurrences and making type applications in patterns unconditionally bring new variables into scope.

   This would not be the case with the treatment of in-scope variables as originally written in `#126`_,
   where the choice between a binding site and an occurrence depends on whether a type variable is in scope.

#. The `Syntactic Unification Principle`_ is bolstered by ``-XNoImplicitBinds``

   As discussed in the "Consistency" section of the motivation for that extension, the different forms of implicit binding we have today work quite differently.
   In many case, those different forms are chiefly distinguished by being confined to one of the type- or term- level.
   For example, "regular patterns" in expressions and the right hand sides of type synonyms ought to be basically the same in a unified-namespace world, but the implicit binding mechanisms they each support today are unrelated.
   Avoiding this inconsistency is therefore part of the type and term syntax unification espoused by this principle.

#. The `Explicit Binding Principle`_ is made to hold under ``-XNoImplicitBinds`` and ``-XPatternSignatures`` by side-stepping the need for new explicit syntax.

   Making ``-XPatternSignatures`` not imply implicit bindings keeps that extension in accordance with the `Explicit Binding Principle`_.
   That principle says implicit binding constructions should have explicit counterparts they desugar to.

   In general, solutions to `Lexical Scoping Principle`_ are also solutions to `Explicit Binding Principle`_.
   It is just for implicit forms which one wishes to leave enabled that explicit syntax is needed, and explicit syntax forms an additional solution to the `Explicit Binding Principle`_ (and not the `Lexical Scoping Principle`_).

   ``let type name = _ in``, proposed in Proposal `#523`_, would be such explicit syntax.
   If that proposal is accepted, then we can say ``-XNoImplicitBinds`` goes back to just being a solution for the `Lexical Scoping Principle`_, and ``let type name = _ in`` is just the solution for the `Explicit Binding Principle`_.
   This is conceptually simpler and we hope to get there, but in the meantime we don't want to deny ``-XNoImplicitBinds`` its extra benefit!

#. The `Visibility Orthogonality Principle`_ is made to hold,
   by ensuring that types and terms are treated identically in patterns.
   This was not the case with the old version of `#126`_ for constructor patterns,
   which treated variables after ``@`` different to those without a ``@``.

Costs and Drawbacks
-------------------

1. The poor interplay between ``-XExtendedForAllScope`` and ``-XTypeAbstractions`` is regrettable,
   but I see no way to improve this.

#. The extension shuffling introduces some complexity.
   Is the gain worth the complexity?

Alternatives
------------

Unresolved Questions
--------------------

None at this time.

Implementation Plan
-------------------

I am very keen to get this implemented and would be happy to support others taking on this work or to do it myself.
