.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

**Note:** There are several alternatives presented in this proposal, with the goal
of having discussion guide us to which alternative we wish to adopt. Paragraphs/sections that
apply only to one alternative are prefixed with a number.

Collaborative Commentary
========================

The current `ghc wiki <https://ghc.haskell.org/trac/ghc/wiki>`_ contains lots of useful
documentation for new contributers to ghc.  However finding, updating and editing the
wiki can be intimidating at first.  Especially the search part can be quite hard.

(1) Now that we have the new ghc-proposal process, I'd like to make the case
for moving the Commentary and Documentation parts out of the wiki and into a version
controlled repository.

(2) Now that we have the new ghc-proposal process, I'd like to open the ability
to backport previously-proposed and implemented features to the new proposal format.

Motivation
----------

Make it easier for everyone to participate in finding the relevant bits, updating the
commentary and documentation.

Proposed Change
---------------

The proposed change consists of setting up a [(1) repository | (2) folder in the existing
ghc-proposals repo] that will hold the documentation
files in reStructuredText format;
(1) compiling a list of pages to be migrated from the
wiki to the repository; and (1) finding a group of volounteers to move these over.

The repository will be periodically built using sphinx (as deployed for cabal and GHC), and produce an easy-to-read experience on the web.

I suggest that everyone who has a pull request merged into the repository is
given commit rights. The entry barrier to actually add new documentary or comments to
the repository is therefore an initial pull request.

Drawbacks
---------

* (2) This fragments our documentation. Some will remain on the wiki, and some features
  will be migrated to the GitHub repo. However, this fragmentation would happen naturally
  with the ghc-proposals process, as any new features after summer 2016 would end up in
  GitHub and not on the wiki anyway. This backports the problem. In some years, however,
  I would expect most features to be on GitHub and this drawback to diminish.

* Loss of Trac integration with comments; it's now harder to link to Trac tickets.

Alternatives
------------

* Don't do this. Keep the status quo.

* Choice (1).

* Choice (2).

Unresolved Questions
--------------------

* (1) How many volunteers are willing to review and rewrite documentation from the wiki?
