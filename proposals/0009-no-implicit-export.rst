.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/94>`_.

.. contents::

Proposal NoImplicitExports
==============

Much of the community has settled around the idea that implicit exports are a misfeature. Disabling implicit exports is a cheap method of amending this situation. This feature is intended to be used in ``default-extensions`` and applied project wide.


Motivation
------------

Explicit exports allow GHC and other external tooling to identify dead code, ease optimization, and reduce linking surface. As well implicit exports tend to enable pollution of the global name-space causing authors to create more byzantine naming schemes. By removing implicit exports we open up a wide surface of improvement at the cost of programmer book keeping.


Proposed Change Specification
-----------------------------
This proposal introduces a new language pragma ``ImplicitExports``. This pragma is enabled by default in ``DynFlags.hs``. When disabled, open module exports are reinterpreted as empty.

The following statement:

::
 module Foo where

becomes:

::
 modules Foo () where

This pragma is semantically treated as a syntactic extension and only effects modules where it has been applied.

Files that do not explicitly export may produce a compilation warning:

::
 Warning: module "Foo" does not produce any exports.

 module Foo where
        ^^^


Effect and Interactions
-----------------------
Limiting the effect of this pragma to applied modules should cause little concern for side effect, other than programmer surprise. This can be mitigated by a compiler warning.


Costs and Drawbacks
-------------------
This pragma presents very little cost for maintenance. It will shift the burden of export maintenance to the user.


Alternatives
------------

1. External tooling like ``hlint`` could cover this case, but linting is often opt in, trails behind GHC releases when new syntax is added and produces more complexity with CI.
2. `Purescript <https://github.com/purescript/purescript/issues/2012>` disabled implicit imports in version 0.9. This has some similar intent, but enables different improvements. Explicit imports reduce breakage from namespace pollution, but they do not enable the same optimization and dead code analysis that explicit exports do.


Unresolved questions
--------------------
n/a


Implementation Plan
-------------------
There are two basic implementations with different properties.

The first is to implement this as part of ``Parser.y``. We must replace the empty case of ``maybeexports``.

::
 maybeexports :: { (Maybe (Located [LIE GhcPs])) }
       :  '(' exportlist ')'       {% ams (sLL $1 $> ()) [mop $1,mcp $3] >>
                                      return (Just (sLL $1 $> (fromOL $2))) }
       |  {- empty -}              {% maybeImplicitExports }

 maybeImplicitExports :: P (Maybe (Located [LIE GhcPs]))
 maybeImplicitExports = do
   enabled <- (LangExt.ImplicitExports `extopt`) . options <$> getPState
   if enabled
     then pure Nothing
     else pure . Just $ sL0 []

This implementation nips implicit exports at the bud, but does not provide opportunity for warning.

The other is re-interpreting ``Nothing`` in ``TcRnExports``. This involves adjusting the ``Nothing`` case of  ``exports_from_avail``.

::
 exports_from_avail Nothing rdr_env _imports _this_mod
 = do
   implicit_exports <- (LangExt.ImplicitExports `extopt`) . options <$> getPState
   let avails =
         -- likely incorrect :)
         if implicit_exports
           then map fix_faminst . gresToAvailInfo
             . filter isLocalGRE . globalRdrEnvElts $ rdr_env
           else []
   return (Nothing, avails)

In this implementation it is possible to throw a warning such as ``Opt_WarnDodgyExports``.
