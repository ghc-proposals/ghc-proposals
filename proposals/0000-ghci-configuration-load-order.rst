Change the load order of .ghci files so that local settings override global ones
================================================================================

.. proposal-number::
.. trac-ticket::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/123>`_.
.. sectnum::
.. contents::

Here you should write a short abstract motivating and briefly summarizing the proposed change.

With the way that ghci currently loads `.ghci` files a local/project setting will always be overriden by a setting from a `.ghci` file from a more global scope - such as a users home directory. I propose that it would be more intuitive for local settings to be given precedence over global settings and would enable a user to have sensible defaults that can the be overriden for any given particular project.

Motivation
------------
Currently ghci gives precedence to global .ghci conguration
settings by applying them after any local/project specific .ghci files:

`relevant code <https://github.com/ghc/ghc/blob/314bc31489f1f4cd69e913c3b1e33236b2bdf553/ghc/GHCi/UI.hs#L561>`_

.. code-block:: haskell

    dot_cfgs <- if ignore_dot_ghci then return [] else do
        dot_files <- catMaybes <$> sequence [ current_dir, app_user_dir, home_dir ]
        liftIO $ filterM checkFileAndDirPerms dot_files
    mdot_cfgs <- liftIO $ mapM canonicalizePath' dot_cfgs

    let arg_cfgs = reverse $ ghciScripts dflags
        -- -ghci-script are collected in reverse order
        -- We don't require that a script explicitly added by -ghci-script
        -- is owned by the current user. (#6017)
    mapM_ sourceConfigFile $ nub $ (catMaybes mdot_cfgs) ++ arg_cfgs
        -- nub, because we don't want to read .ghci twice if the CWD is $HOME.


This means that one cannot override global settings on a per project basis with settings that might be convenient to using ghci in that project (importing certain librarues, different settings etc). This is, I would argue, quite counter-intuitive based on how most tools work and is not something that is especially clear to the end user or something that can currently be worked around.

On the other hand there are a couple of open issues where people dont like the idea of
local ghci evaluation for security reasons:

`Reading ./.ghci files raises security issues <https://ghc.haskell.org/trac/ghc/ticket/6017>`_

`GHCi by default opens .ghci files in local directories <https://ghc.haskell.org/trac/ghc/ticket/14250>`_


Proposed Change Specification
-----------------------------

As a way of enabling users to choose between the current behaviour and behaviour where local settings take precedence I suggest the following resolution:

- Change the order or evaluation from global first to local
- include a new ghci setting/flag that controls the order of precedence of local/project specific .ghci files.
- This flag could then be set in the global/home .ghci file or the command line and read/evaluated in a first pass (might make sense if the flag


Effect and Interactions
-----------------------

I believe this would both address the counterintuitive loading strategy
implied currently (i was convinced local .ghci files didnt work because
when i changed the prompt it was always overwritten by my global config)
and the security question.

The only question then would be if evaluation of project/local .ghci files
would be on or off by default. I would favour the former but that is just
personal preference.

Costs and Drawbacks
-------------------

The Costs:
- a change in previous behaviour for users who preferred that for reasons given above.
- some extra load time

The Benefits:
- this behaviour would, in my opinion at least, be more intuitive for most people based on experiences with similar configuration schemes.

Alternatives
------------

The alternative might be to:

- leave things the way they are but explain (in a message displayed as ghci is loading) why things arent going to change when a prject level `.ghci` file is encountered. This would at least inform the user why a local `.ghci` file was seemingly ignored.
- change precedence but dont have an option to change back

Unresolved questions
--------------------

If the precedence is made configurable then we should ensure that a local package cannot override the precedence setting. In other words, when it comes to the precedence setting itself, we should always give priority to the "most global setting" (In fact a local setting should probably always be ignored).

Implementation Plan
-------------------
