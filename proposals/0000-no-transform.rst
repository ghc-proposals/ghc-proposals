Deprecate and remove ``-XTransformListComp``
==========================================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

Almost no one uses ``-XTransformListComp``, yet it takes quite a lot of support to keep it
running. This proposal suggests to rip it out.

Motivation
------------
The extension ``-XTransformListComp`` (described `here <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TransformListComp>`_) is barely used on
Hackage. Specifically, it is used in only 7 packages. Five of these (``llvm-pretty``, ``marxup``, ``manatee-core``, ``nettle-frp``, ``monadiccp``) do not compile on GHC 8.4. The other two
are ``haddock`` and ``haddock-api``. Each use the extension for precisely one construct.

Yet, there is quite a bit of infrastructure around this extension. Here is its 20-line declaration
within GHC::

    | TransStmt {
      trS_ext   :: XTransStmt idL idR body, -- Post typecheck,
                                            -- R in (>>=) :: Q -> (R -> S) -> T
      trS_form  :: TransForm,
      trS_stmts :: [ExprLStmt idL],   -- Stmts to the *left* of the 'group'
                                      -- which generates the tuples to be grouped

      trS_bndrs :: [(IdP idR, IdP idR)], -- See Note [TransStmt binder map]

      trS_using :: LHsExpr idR,
      trS_by :: Maybe (LHsExpr idR),  -- "by e" (optional)
        -- Invariant: if trS_form = GroupBy, then grp_by = Just e

      trS_ret :: SyntaxExpr idR,      -- The monomorphic 'return' function for
                                      -- the inner monad comprehensions
      trS_bind :: SyntaxExpr idR,     -- The '(>>=)' operator
      trS_fmap :: HsExpr idR          -- The polymorphic 'fmap' function for desugaring
                                      -- Only for 'group' forms
                                      -- Just a simple HsExpr, because it's
                                      -- too polymorphic for tcSyntaxOp
    }                                 -- See Note [Monad Comprehensions]

As you might imagine, parsing, renaming, and type checking this beast is quite non-trivial.
It also complicates the lexer and the parser, introducing three new conditional keywords
(``group``, ``by``, and ``using``).

Beyond GHC, there is roughly an order of magnitude more packages that manipulate code
involving ``-XTransformListComp`` constructs than there packages that use it.

If we consider that only the Haddock packages make useful use of this feature (because the
other packages do not appear to be actively maintained), then I would wager that the Haskell
ecosystem spends roughly 3 orders of magnitude more effort (measured in lines of code) at
supporting this construct than we do taking advantage of it. Note that this construct appeared
in GHC 6.10, so there has been plenty of time for adoption.

I propose to stop doing so.

Proposed Change Specification
-----------------------------
In the next release of GHC, deprecate ``-XTransformListComp``.

Two releases (1 year) later, remove support for ``-XTransformListComp``.

Effect and Interactions
-----------------------
Many lines of code deleted.

Costs and Drawbacks
-------------------
The `original paper <https://www.microsoft.com/en-us/research/wp-content/uploads/2007/09/list-comp.pdf>`_ was written by two prominent members of our community, and it would be sad if they felt
discouraged from contributing in the future.

Alternatives
------------
- Continue to maintain this extension.

- Remove the extension without a deprecation period.

Unresolved questions
--------------------
None at this time.

