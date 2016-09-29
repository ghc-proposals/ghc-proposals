.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

Collaborative Commentary
========================

The current `ghc wiki <https://ghc.haskell.org/trac/ghc/wiki>`_ contains lots of useful
documentation for new contributers to ghc.  However finding, updating and editing the
wiki can be intimidating at first.  Especially the search part can be quite hard.

Now that we have the new ghc-proposal process, I'd like to make the case
for moving the Commentary and Documentation parts out of the wiki and into a version
controlled repository.

Motivation
----------

Make it easier for everyone to participate in finding the relevant bits, updating the
commentary and documentation.

Proposed Change
---------------

The proposed change consists of setting up a repository that will hold the documentation
files in reStructuredText format; compiling a list of pages to be migrated from the
wiki to the repository; and finding a group of volounteers to move these over.

The repository will be periodically built using sphinx (as deployed for cabal and GHC),
and produce an easy-to-read experience on the web.

I suggest that everyone who has a pull request merged into the repository is
given commit rights. The entry barrier to actually add new documentary or comments to
the repository is therefore an initial pull request.

Drawbacks
---------

No known drawbacks so far.

Alternatives
------------

There are no alternatives ;-)

Unresolved Questions
--------------------

None yet.
