.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

Top-level kind signatures
=========================

This proposal adds *top-level kind signatures* allowing users to declare the kind of
type-level declarations introduced with ``type``, ``data``, ``newtype``, or ``class``.

For example::

  type MonoTagged :: Type ~> Type ~> Type
  data MonoTagged t x = MonoTagged x

  type Id :: forall k. k -> k
  type family Id x where
    Id x = x

  type C :: (k ~> Type) ~> k ~> Constraint
  class C a b where
    f :: a b

Declarations that have a top-level kind signature (with no wildcards)
can use polymorphic recursion; declarations
without such signatures are understood to have inferred kinds, and polymorphic
recursion is not available.

This proposal replaces GHC's current notion of syntactic
CUSKs_.

.. _CUSKs: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#complete-user-supplied-kind-signatures-and-polymorphic-recursion

    
Motivation
------------
This is a simplification over the current story around CUSKs, which are fiddly and
unpredictable. For example, here_ are_ some_ tickets_ borne of confusion around CUSKs.

.. _here: https://ghc.haskell.org/trac/ghc/ticket/12928
.. _are: https://ghc.haskell.org/trac/ghc/ticket/10141
.. _some: https://ghc.haskell.org/trac/ghc/ticket/13109
.. _tickets: https://ghc.haskell.org/trac/ghc/ticket/13761

This new proposal makes the choice of whether or not to infer a kind much simpler.
Even better, this proposal makes type-level polymorphic recursion have the same rules
as term-level polymorphic recursion: you *must* have a top-level signature (with no
wildcards).

The examples above include an unusual choice of syntax, using ``~>`` where one might
expect ``->``. This is to support `partially applied type families <https://github.com/ghc-proposals/ghc-proposals/pull/52>`_, where a *matchable* arrow (which I'm spelling ``~>``) is
distinct from the unmatchable arrow ``->``. Briefly, a matchable function is one that
is generative and injective, so that pattern matching it makes good sense. Thus, data
and type constructors are matchable, while ordinary functions (and, therefore, type
families) are not. Currently, ``->`` is used in kinds, but its use there is always
for *matchable* "functions", like type constructors or promoted data constructors.
As part of enabling partially applied type families, it is necessary to introduce this
second arrow. In keeping with the vastly more common use of ``->`` to denote ordinary
term-level functions, I propose ``~>`` as the matchable arrow.

Proposed Change Specification
-----------------------------

1. Introduce a new top-level declaration form ::

     type <name> :: <kind> 

   It is distinguishable from a type synonym by the lack of an ``=`` on the line. A
   type-level declaration with a top-level kind signature may use polymorphic recursion;
   one without is considered to have an inferred kind and may not use polymorphic recursion.

   The kind given is checked against the declaration of the type. All kind generalization
   is done before ever examining the full declaration, just like how GHC treats type
   signatures.

   Associated types may be given kind signatures within their classes.
   
   Top-level kind signatures are enabled with the extension ``-XTopLevelKinds``.

2. Introduce a new kind former ``~>``, with parsing identical to ``->``, that denotes
   a matchable type. This new type former has very limited use (for now): it may appear
   in

   * top-level kind signatures
   * the result kind of GADT-style datatype declarations, as well as type and data families
   * data constructor types

   Any other use is considered an error. (With partially applied type families, this type
   will become more useful.)

   The meaning of ``~>`` will be identical to that of ``->`` in all contexts other than
   top-level kind signatures.

   In a top-level kind signature, the use of ``->`` will imply that the declaration is
   for a type family. Any arguments to the left of ``->``s must be arguments to the
   type family that must be saturated at every occurrence. All other top-level kind
   signatures will use only ``~>``.

3. Introduce a new extension ``-XCUSKs``, on by default, that detects CUSKs as they
   currently exist. A CUSK will be treated identically to a top-level kind signature.

   When ``-XNoCUSKs`` is specified, only a top-level kind signature enables
   polymorphic recursion.

4. Plan to turn ``-XCUSKs`` off by default in GHC 8.8 and to remove it sometime thereafter.


Effect and Interactions
-----------------------
This is largely a simplification over the status quo, eventually eliminating the need for
the fiddly definition and detection of CUSKs. It allows users to control whether they want
inference or specification in a more conspicuous way than CUSKs do.

I don't foresee intricate interactions with other features.

Template Haskell will need to be updated accordingly.

Costs and Drawbacks
-------------------
Implementation should be rather straightforward, as this is a new syntactic construct.

Parsing may be slightly complicated by the similarity to a type synonym, but I doubt this
will pose more than an hour's delay in implementation.

Checking and generalizing the kind can be done by already-written code (in TcHsType).
More checks will have to go in TcValidity to check for poor uses of ``~>``.

The hardest part will be complicating the code in TcTyClsDecls, which is already somewhat
involved; however, I don't think this change will be invasive, as it will just affec the
code that currently checks for CUSKs.

Alternatives
------------

* Don't do anything. I find the current situation to be confusing, though, generating
  several confused users yearly.

* Don't add ``~>``, but otherwise keep this proposal. This choice is reasonable, but
  it's not forward compatible and will cause pain down the road if we ever implement
  partially applied type families.

* We don't need the ``type`` keyword to introduce non-symbolic kind signatures, as the
  capital letter can tip GHC off. Perhaps omit.
  
* The syntax for closed type families with a top-level signature is redundant. Perhaps
  this could be simplified.
  
* I'm not particularly pleased with ``-XTopLevelKinds``. ``-XKindSignatures`` is the
  Right Answer, but that's taken. (That should really be ``-XKindAscriptions``, but
  that's another story.)

* Other transition plans are welcome. We could just abandon CUSKs entirely, asking the
  few users who play in this dark corner to use some CPP.


Unresolved questions
--------------------
These are essentially considered in the "Alternatives" section.


Implementation Plan
-------------------
I (or a close collaborator) will implement.
