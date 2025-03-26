.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

GHC Performance Regression Collection
=======================================

GHC's performance has audibly regressed over the last few versions and the
ghc team is actively working on improving the situation.  By providing
more performance regression examples from the community we might be able
to do a better job in understanding and finding these regression.

Motivation
----------

If ghc's compilation performance is slow this can become a really costly
issue if everyone is waiting for ghc to compile. It is therfore in the
interest of everyone to have a really fast haskell compiler.  However
only the community can collectively come up with all the test cases and
regressions they see in their daily lives.  Artificially creating
performance regression test cases that cover the whole spectrum, is a
non trivial task.  However if community members have these regressions
in their real world code, and they can possibly provide these to build
a larger database of code samples, it could aid the overall performance
analysis and improvement of ghc.

Proposed Change
---------------

The proposed change is to have a github repository say `ghc/regressions`,
which accepts pull requests with regression examples.  These will then be
reviewed by a trusted team. And merged into the `ghc/regressions` repository
if deemed safe. The repository will be built periodically and the
performance metrics for each example will be collected in a standardized
fashion.

Ideally we'd have some infrastructure in place that would go back and build
the code with older versions of ghc as well (if supported by that ghc version)
and collect those metrics as well to see if the code did actually regress
or not.

Drawbacks
---------

This is probably quite expensive infrastructure wise.
