.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
                     
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/85>`_.

.. contents::

Stable Language Extensions
==========================

Language extensions serve two purposes: As a test bed and playground for language experimentation and development, but also as an opt-in mechanism to proven and stable language variants. This proposal describes a procedure to distinguish these two, as a service to our confused users and a guideline to our own work.


Motivation
------------

We have a clear rule that GHC’s interpretation of plain Haskell (without language extensions) shall not change. But what about Haskell with language extensions? Some of the proposals that the GHC committee receives aspire to change the meaning of an extension, say ``Foo``. At this point, we have to conflicting desires: We want ``Foo`` to be better. But we also want to cater for users who use ``{-# LANGUAGE Foo #-}`` and might expect their code to work in all versions of GHC supporting ``Foo``.

Proposed Change Specification
-----------------------------

Every major release of GHC shall categorize its supported language extensions into two groups: **Experimental** and **Stable**. The user manual will clearly state the status and, for stable extensions, since when it is considered stable.

The rules for changes to a language extensions are then as follows:

Changing an **experimental** language extension
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

An **experimental** language extension can be improved with little obstruction. In particular:

* Extending it to allow more programs (e.g. expanding the scope of ``ExplicitNamespaces``, `#65 <https://github.com/ghc-proposals/ghc-proposals/pull/65>`_) is no problem.
* A change that makes GHC not accept some programs any more shall be carefully considered, but is ok.
* A change that silently changes the runtime behaviour of a program shall be avoided, if possible, but is not completely out of the question (e.g. improvements to derived instances).

Users who want stability should consider an experimental language extension, as implemented by different GHC versions, as separate extensions and not expect compatibility in either direction.

Changing a **stable** language extension
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A **stable** language extension shall not be changed, if that change can be reasonably avoided. If a useful change is proposed, then it should be implemented under a new language extension. Changes that are unquestionably bugfixes (whether the bug was in the language extension specification or the implementation) are still ok.

Users can rely that the _stable_ version of a langage extension is, well, stable, and any version of GHC supporting the language extension as stable should accept the same programs.

For users who want a safeguard agains accidentially using an unstable language extension (e.g. when using an earlier compiler), a ``-Wexperimental-extension`` warning is provided.

Changing a language extension status
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Language extensions can be promoted from **experimental** to **stable**, using the existing GHC proposal process. The proposal document is expected to state

* The language extensions.
* Since which version of GHC the language extension has not changed.
* A justification why this language extension is likely to not undergo further changes.

If the proposal is accepted, the documentation is updated to reflect the new status, as well as the ``-Wexperimental-extension`` flag.

In case released major GHC versions happen to implement the language extension in its stable form _and_ a point update is released for these major versions, then the change to the documentation and ``-Wexperimental-extension`` is backported.

Costs and Drawbacks
-------------------
Besides a bit bookkeeping in GHC’s code, it is mostly an organizational cost: More proposals to manage.

There are two failure modes:

* Too many stable extensions: We will slow down development and/or cause a proliferation of langauge extensions variants that are confusing to our users and hard to implement properly.
* Too few stable extensions: Users will be forced to use experimental extensions even if they would rather not to.


Alternatives
------------
* Status Quo: We just apply common sense.
* Versioned language extensions (which is somewhat equivalent to this proposal if we too eargerly mark proposals as stable).


Unresolved questions
--------------------
* Is the ``-Wexperimental-extension`` flag useful, even if it can “err” on the conservative side in older releases that happen to implement the stable semantics of a language extension before we decided it's stable?

Implementation Plan
-------------------
* Joachim will update the ghc-proposals procedural README to encompass this new variant of proposals.
* Someone will have to include the description of what a stable extension is in the uses’s guide.
* Someone will have to extend the user’s guide special mark-up for langauge extensions with new meta-data fields (status, and stable when).
* Someone will have to implement ``-Wexperimental-extension``.
