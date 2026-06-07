Import Exposing
===============

.. author:: Akhra Mellivora Gannon
.. date-accepted:: 
.. ticket-url:: 
.. implemented:: 
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/758>`_.
.. sectnum::
.. contents::

An extension allowing import declarations to selectively alias identifiers under multiple qualifiers (including unqualified).


Motivation
----------
Variants on this feature have been proposed a few times, going back to `before proposals moved to GitHub <https://gitlab.haskell.org/ghc/ghc/-/issues/10478>`_. Failure of prior proposals appears to be mostly due to them attempting to make too many changes at once, creating too much room for debate/bikeshedding, leading to loss of momentum/interest. This proposal hopes to avoid the morass by introducing a single keyworded feature which does not conflict with any existing syntax, and which further proposals can build around.

More directly, this proposal grew out the `Default Qualified Imports <https://github.com/ghc-proposals/ghc-proposals/pull/760>`_ proposal (henceforth "#760"). It seemed this proposal should stand on its own both to reduce #760's footprint, and because this proposal's features are useful with or without #760. (Furthermore, even "competing" proposals should benefit from having this part nailed down!)


Proposed Change Specification
-----------------------------
Adds the ``ImportExposing`` extension, which revises `Haskell 2010 Report Section 5.3 <https://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1010005.3>`_ as follows.

In 5.3.0, *impdecl* is extended with an optional ``exposing`` clause:

  | *impdecl* 	→ 	``import`` [``qualified``] *modid* [``as`` *modid*] [*impspec*] [``exposing`` [``as`` *modid*] *eimpspec* …]
  |
  | *eimpspec* 	→ 	( *import*:subscript:`1` , … , *import*:subscript:`n` [ , ] ) 	    *(n ≥ 0)*

The list of *varid* symbols in the final paragraph is updated to include ``exposing``.

In 5.3.1, the following is added as list item 4:

  | 4. Additional entities can be specified via ``exposing`` (*import*:subscript:`1` , … , *import*:subscript:`n`). These will always be added as top-level names, regardless of how the import declaration is otherwise qualified.
  |
  | The form ``exposing`` ``as`` *modid* (*import*:subscript:`1` , … , *import*:subscript:`n`) adds the specified entities qualified with the *modid* local alias, without extending the top-level environment. Multiple ``exposing`` clauses are legal, provided each uses a different ``as`` qualifier (or lack thereof).
  |
  | If an entity in an ``exposing`` group will already be imported with the same qualifier, a warning is issued under ``-Wunused-imports``.
  |
  | It is an error for an ``exposing`` clause to import a name which has been excluded by a ``hiding`` clause with the same qualifier.


Examples
--------
Haskell 2010:
::
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Data.Set (Set)
  import qualified Data.Set as Set
  import Data.Vector (Vector, (!), (!?), (//))
  import qualified Data.Vector as Vec

Proposed:
::
  {-# LANGUAGE ImportExposing #-}
  import qualified Data.Map as Map exposing (Map)
  import qualified Data.Set as Set exposing (Set)
  import qualified Data.Vector as Vec exposing (Vector, (!), (!?), (//))

Haskell 2010:
::
  module A
  ( module X
  , renamed
  , madeTotal
  ) where

  import B (B)
  import qualified B as C
  import qualified B as X (reexport1, reexport2)
  import qualified D
  import qualified D as X hiding (partial)

  renamed :: a -> B
  renamed = C.conflictingName

  madeTotal :: Foldable t => t a -> Maybe b
  madeTotal xs = if null xs then Nothing else Just $ D.partial xs

Proposed:
::
  {-# LANGUAGE ImportExposing #-}
  ...
  import qualified B as C exposing (B) exposing as X (reexport1, reexport2)
  import qualified D as X hiding (partial) exposing as D (partial)
  ...

Effect and Interactions
-----------------------
Taken on its own, this proposal eliminates most cases where multiple imports of the same module are necessary, or simply attractive for ergonomics. It introduces one explicit error state, specifically to enforce Haskell 2010 syntax as taking precedence over the new mechanism; and reuses an existing warning in a way that mirrors current behavior (redundant imports from separate declarations trigger ``-Wunused-imports``).

Beyond that, it lays groundwork for better ergonomics if imports are made qualified by default (e.g. as in #760).


Costs and Drawbacks
-------------------
Regarding development and maintenance: uncertain, feedback requested!

Adding more syntax necessarily impacts learnability, but the keywords and positioning were chosen to hopefully be self-explanatory. Also, while the examples above are one-liners, real-world imports are often split over several lines, and the clause structure works well with that practice.

One missing capability stands out, especially when considering potential default-qualified behavior: while this proposal allows multiple *inclusion* lists with different qualification, any *exclusion* must occur in the regular ``hiding`` clause, meaning you can't start with a whole-module import and selectively pare it down. But while this might sometimes be desirable (e.g. exposing *most* of a module ``as`` an alias, with *all* of it available via the base import; the final example could be shorter with this!) it seems unavoidable that the added complexity would heavily impact not only learnability, but readability even by experts. An option to address this is offered in Alternatives.


Backward Compatibility
----------------------
``ImportExposing`` admits all existing programs with no change in behavior.


Alternatives
------------

No ``as`` Clauses
^^^^^^^^^^^^^^^^^
The initial idea for this proposal, informed by #760, was strictly a mechanism for adding identifiers to the top-level namespace from an otherwise-qualified import declaration. ``as`` clauses emerged during the write-up and seem to provide significant utility (even for unqualified imports) with reasonably low added complexity. If this opinion proves contentious, the initial motivation is still satisfied without them.

Bikeshedding
^^^^^^^^^^^^
Another option considered for the keyword was ``opening``. This makes sense for top-level imports, less so with the addition of ``as`` clauses. However, if they are removed it may be worth considering.

(N.b. in #760 an ``unqualified`` keyword is introduced, with ``open`` offered as a shorter alternative; thus, a matching alternative here.)

``hiding as`` Clauses
^^^^^^^^^^^^^^^^^^^^^
The noted inability to specify *subtractive* subsets of the base import could be addressed with a ``hiding as`` form of the ``exposing`` syntax. While such behavior could be made unambiguous, it introduces potentially non-obvious complications, not least of which being that unlike ``exposing`` there is already syntax for ``hiding`` *without* the ``as``, which belongs in a non-adjacent part of the import declaration! This could be obviated by using a different keyword, but that seems *more* confusing. The proposal felt stronger without it.

With this addition, the final example above could use:
::
  import qualified D hiding as X (partial)

Notably this would be *fully* equivalent to the Haskell 2010 comparison, rather than merely satisfactory to the example. However, it also highlights a potential parsing complication: in this case the absence of several optional clauses means we must reach ``as`` (and not raise an error on the assumption it's a misplaced 5.3.3 local alias) before knowing what kind of ``hiding`` this is!


Unresolved Questions
--------------------
not sure I got the BNF right to specify that ``exposing`` ``[as]`` is repeatable? (nothing comparable in the existing import spec)


Implementation Plan
-------------------
Tentatively and with a likely need for guidance, the author can attempt implementation if nobody else finds it an exciting project.


Endorsements
-------------
(Optional) This section provides an opportunity for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.

Acknowledgments
---------------
(Optional) This section provides an opportunity to say thanks
to third parties for their contributions to the proposal.
