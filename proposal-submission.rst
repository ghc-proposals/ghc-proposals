The GHC Proposals Process
=========================

GHC proposals move through the following phases,

- *Development*: Develop a proposal document laying out the motivation for your
  proposed change, specifying the change, and assessing its benefits
  and drawbacks. The discussion phase is started by opening a pull request
  against the ``ghc-proposals`` repository.

- *Discussion*: The community will debate the pros and cons of your proposal
  as described by the document. You should revise your proposal document as
  discussion progresses. This discussion phase will end four weeks after
  the last edit.

- *Committee decision*: The GHC Steering Committee will review your proposal
  document and its ensuing debate and decide whether to accept or reject
  your proposal. If rejected you may amend and resubmit your proposal document
  at any time.

- *Implementation*: Accepted proposals may be implemented and submitted
  for code review. In most cases, proposal authors will implement the
  change themselves, but there is no obligation to do so.


Content of the Proposal Document
--------------------------------

Each proposal document must follow the following outline. A template is provided in `proposal template <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0000-template.rst>`_.

a. *Motivation*: Give a strong reason for why the community needs this change. Describe the use case as clearly as possible and give at least one concrete example. Explain how the status quo is insufficient or not ideal.

b. *Proposed Change Specification*: Specify the change in precise and comprehensive yet concise language. Your specification may include,

   - grammar and semantics of any new syntactic constructs
   - the types and semantics of any new library interfaces
   - how the proposed change addresses the original problem (perhaps referring back to the example)

c. *Effect and Interactions*. Detail how the proposed change addresses the original problem raised in the motivation. Detail how the proposed change interacts with existing language or compiler features. Think about what surprising or problematic interactions may occur.

d. *Costs and Drawbacks*. What are the drawbacks and costs to the community should this change be implemented? For example, does this make Haskell harder to learn for novice users?  Does it make Haskell code harder to read or reason about? Will the implementation be complex or invasive?

e. *Alternatives*: List alternatives to your proposed change and discuss why they are insufficient.

f. *Unresolved questions*: Explicitly list any remaining issues that remain in the conceptual design and specification. Be upfront and trust that the community will help. Please do not list *implementation* issues.

g. *Implementation Plan* (Optional): If accepted who will implement the change? It's quite okay to say "unknown" if you don't feel willing or able to carry out the implementation yourself.


Note that proposals are written in `ReStructuredText
<http://www.sphinx-doc.org/en/stable/rest.html>`_ rather than Markdown for its
expressiveness and ease of integration into other GHC infrastructure. See the
`GHC Users Guide
<http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/editing-guide.html>`_
for a brief introduction to ReStructuredText.


Submitting your Proposal for Discussion
---------------------------------------

If you are unfamiliar with GitHub, see detailed instructions below.

1. Fork the ``ghc-proposals`` repository
2. Setup your fork,

   a. Create a branch for your proposal
   b. Start your proposal by copying ``proposals/0000-template.rst`` to a new file in the ``proposals/`` directory.

3. Write the proposal document, commit, and push to your ``ghc-proposals`` fork
4. Submit your proposal for discussion by opening a pull request for your branch against the ``master`` branch of ``ghc-proposals/ghc-proposals``. Label it with the ``Under discussion`` label.

The pull request summary should include a brief description of your
proposal, along with a link to the rendered view of proposal document
in your branch. For instance,

.. code-block::

    This is a proposal augmenting our existing `Typeable` mechanism with a
    variant, `Type.Reflection`, which provides a more strongly typed variant as
    originally described in [A Reflection on
    Types](http://research.microsoft.com/en-us/um/people/simonpj/papers/haskell-dynamic/index.htm)
    (Peyton Jones, _et al._ 2016).

    [Rendered](https://github.com/bgamari/ghc-proposals/blob/typeable/proposals/0000-type-indexed-typeable.rst)


After you open your pull request, the community is invited to comment and
debate. Feel free to improve your document to reflect the discussion. The goal
is to make the strongest proposal possible and demonstrate that all alternatives
have been considered. It is very likely that multiple iterations are necessary
before the proposal is ready for consideration by the GHC Steering Committee.

When you feel that your proposal is ready,

1. review the discussion thread and ensure that the proposal text accounts for
   all salient points.
2. write a pull request comment briefly summarizing the major points raised
   during the discussion period and stating your belief that the proposal is
   ready for review..
3. replace the ``Under discussion`` label with ``Pending committee review``.

The committee will review the proposal, your summary, and the surrounding discussion
within two weeks and leave a comment with its conclusion.


Detailed instructions
---------------------

The instructions below are intended for users with little experience using
GitHub and git.

Forking repositories
^^^^^^^^^^^^^^^^^^^^

1. Point your browser to the ``ghc-proposals/ghc-proposals`` repository
2. Click on "Fork" (in the upper right corner)

See GitHub's `documentation <https://help.github.com/articles/fork-a-repo/>`_ if
you are unfamiliar with GitHub's notion of "forking".


Setting up your proposal fork
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

1. Clone the repository to your computer.

   a. You find the clone command after clicking on "Clone or Download" button.
   b. Open a terminal in the cloned directory.

2. Create a new branch for your proposal (say, $myproposal). ``git branch $myproposal; git checkout $myproposal``
3. Copy the template to a new file ``cd proposals;  cp 0000-template.rst 0000-$myproposal.rst``
4. Add the new file to the repository ``git add 000-$myproposal.rst``
5. Commit the file to your github ``git commit -a``
6. Push your new branch to your github repository ``git push --set-upstream origin $myproposal``

You only need to do this once. For all future edits to your proposal you only
need to commit and push. e.g. ``git commit -a; git push``.


Moving your proposal to the Discussion phase
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In order to submit your proposal to the discussion phase,

1. Navigate to the main page of your repository in your browser
2. Select the branch containing your proposal in the "Branch:" drop-down
3. Click on "Compare & pull request" button
4. You will be brought to the "Open a pull request" page. Ensure that ``ghc-proposals/ghc-proposals`` is specifed as the "base fork" and that ``master`` is specified as the "base branch".
5. Verify that the diff shown only contains your proposal document.
6. Select the label ``discussion`` on the right hand side.
7. In the "comment" box at the top of the page write a brief description of your proposal along with a link to the proposal document (link to the ``0000-$myproposal.rst`` file in your GitHub fork)

  .. code-block::

    This is a proposal augmenting our existing `Typeable` mechanism with a
    variant, `Type.Reflection`, which provides a more strongly typed variant as
    originally described in [A Reflection on
    Types](http://research.microsoft.com/en-us/um/people/simonpj/papers/haskell-dynamic/index.htm)
    (Peyton Jones, _et al._ 2016).

    [Rendered](https://github.com/bgamari/ghc-proposals/blob/typeable/proposals/0000-type-indexed-typeable.rst)
