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

2. Create a new branch for your proposal (say, ``$myproposal``). ::

       git branch $myproposal
       git checkout $myproposal

3. Copy the template to a new file ::

       cd proposals
       cp 0000-template.rst 0000-$myproposal.rst

4. Add the new file to the repository ::

       git add 0000-$myproposal.rst

5. Commit the file with an informative commit message ::

       git commit -a

6. Push your new branch to your GitHub repository ::

       git push --set-upstream=origin $myproposal

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
6. In the "comment" box at the top of the page write a brief description of your proposal along with a link to the proposal document (link to the ``0000-$myproposal.rst`` file in your GitHub fork)

  .. code-block::

    This is a proposal augmenting our existing `Typeable` mechanism with a
    variant, `Type.Reflection`, which provides a more strongly typed variant as
    originally described in [A Reflection on
    Types](http://research.microsoft.com/en-us/um/people/simonpj/papers/haskell-dynamic/index.htm)
    (Peyton Jones, _et al._ 2016).

    [Rendered](https://github.com/bgamari/ghc-proposals/blob/typeable/proposals/0000-type-indexed-typeable.rst)
