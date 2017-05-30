.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

Using Maniphest instead of Trac
================================

We use phabricator for code review, and have moved the review process to github,
this proposal argues for the use of maniphest for issues, such that issue tracking,
and code review is done in phabricator.

Motivation
----------

Right now we need to ensure we have interop between phabricators code review and
tracs issues tracking, when moving the issue tracking into phabricator's maniphest,
this would not be necessariy any more.  It would also unify the tooling.


Proposed Change
---------------

- Move all of the issue tracking from trac to maniphest
- Removing the custom interop tooling.

Drawbacks
---------

The UI and usage of maniphase compared to trac is different and hence might result
in a different experience for those working with and triaging the issues.
Trac issue numbers would need to be migrated somehow.

Alternatives
------------


Unresolved Questions
--------------------

Can we find consensus on doing this?
