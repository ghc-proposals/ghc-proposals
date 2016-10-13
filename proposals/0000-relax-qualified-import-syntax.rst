.. proposal-number::

.. trac-ticket:: #12693

.. implemented::

.. highlight:: haskell

Relax qualified import syntax
=============================

Allow to write ``qualified`` keyword after module name.

Motivation
----------

There are some problem with current qualified import syntax.

Case 1. ::

    import Data.HashMap (HashMap)
    import qualified Data.HashMap as HashMap
    import Data.Map (Map)
    import qualified Data.Map as Map
    import Data.Set (Set)
    import qualified Data.Set as Set
    import Data.Text (Text)
    import qualified Data.Text as Text

Module names start in different columns, so the user cannot search it without help of tools, so the code is unreadable.

Case 2. ::

    import           Data.HashMap (HashMap)
    import qualified Data.HashMap as HashMap
    import           Data.Map (Map)
    import qualified Data.Map as Map
    import           Data.Set (Set)
    import qualified Data.Set as Set
    import           Data.Text (Text)
    import qualified Data.Text as Text

Too many space used, too less space left for imported items list, too hard to maintain without special tools.

Proposed Change
---------------

Allow to write ``qualified`` keyword (exactly) after module name.

Current grammar (from ``compiler/parser/Parser.y``): ::

    importdecl
        : 'import' maybe_src maybe_safe
          optqualified maybe_pkg modid
          maybeas maybeimpspec

Proposed grammar: ::

    importdecl
        : 'import' maybe_src maybe_safe
          (optqualified maybe_pkg modid | maybe_pkg modid optqualified)
          maybeas maybeimpspec

No semantics changes proposed.

Profit: ::

    import Data.HashMap (HashMap)
    import Data.HashMap qualified as HashMap
    import Data.Map (Map)
    import Data.Map qualified as Map
    import Data.Set (Set)
    import Data.Set qualified as Set
    import Data.Text (Text)
    import Data.Text qualified as Text

1. Module names aligned vertically for free (except "safe" and "source", see below).
2. No extra spaces => more space for import spec list => less lines used.
3. It is easy to understand.
4. It looks more natural from the point of view of English language.

Moreover,

1. The proposal doesn't introduce new syntax constructions looking like existing but doing other things.
2. It is easy to implement.

Package import: package name is semantically a part of a module name, and therefore should stay before module name.

Drawbacks
---------

No actual drawbacks. Third-party syntax parsers need to be updated, but the change in easy.

Alternatives
------------

This proposal looks a little like `ShorterImportSyntax <https://ghc.haskell.org/trac/ghc/wiki/ShorterImportSyntax>`_ and `#10478 <https://ghc.haskell.org/trac/ghc/ticket/10478>`_, but differs from them in such ways:
1. It doesn't actually make imports shorter in visible symbols, only in lines used (in case of very long imported items list), a little.
2. It doesn't introduce new syntax constructions looking like existing but doing other things.

Unresolved Questions
--------------------

"Safe" and "source" import: very special case, may be discussed.
