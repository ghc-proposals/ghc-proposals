.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

Improved GHC development
========================

Developing GHC can be burdonsome, and the whole process has received lots of
critique. We propose some (potential fundamental) changes to address these
shortcomings. We believe the current tools supporting GHC development are more
than adequate but also believe there is room for improvements. These proposals
all enhance the existing tools instead of replacing them outright.

Motivation
----------

Make GHC development fun again! What I’d really like to see improved is the “multiple (web)services, multiple accounts” issue solved. As it is right now I need to have a github account (ghc-proposals, soon tiny PR against ghc), a trac account (for the wiki and issue tracker; which never seems to remember me), a phabricator account, likely a stack overflow account for questions and answers. On top of that I also need to be on a large set of mailing lists if I want to not miss anything, and an irc, reddit, and twitter client to tap into the most recent discussions. And there’s another wiki on haskell.org for which I would need to sign up and get an approved account.

The often proposed solution is to move everything to GitHub. Some of us believe GitHub is not up to the task of managing a project such as GHC. But we also believe with minor changes we can greater leverage the user base of GitHub while also providing valuable information back to the non GitHub users.

Proposed Change
---------------
- Link Phabricator and GitHub

  Phabricator seems to have built in support for OAuth. One easy way so lower the barrier is to support OAuth logins to GitHub. This way a user doesn't have to create a new account. They can simply login using their GitHub credentials. *(This seems to already be enabled, maybe we should advertise it more.)*

  Bonus points if the GitHub API allows us to extract the user's public key from GitHub. This would also mean less setup for the user to be able to push a diff.

  Phabricator documentation for this https://secure.phabricator.com/book/phabricator/article/configuring_accounts_and_registration/
  
- Link Trac to GitHub

  We already have a read-only mirror for code that is incredibly useful. I propose extending this read-only mode further. 

   * Support OAuth on Trac for logins against GitHub as well.
   * Synchronize Trac issues in a read-only way to GitHub. The usefulness of this feature comes In the ability for users to reference the issues from their own GitHub projects. This gives us some valuable information such as who these issues affect. This information would get synched back to Trac. This also allows users to be notified when something gets fixed. Since the issue would be closed on GitHub as well. Finally it gives some information on the importance of issues.

  With the ability to login directly on Trac using your GitHub account, I believe it's easier to take issues on one place: Trac. So read-only would be enough.

  As a whole, I find Trac MUCH better for ticket triaging than Github or Phab,
  both of which seem to be quite bare and simple in what they provide. I am not
  in favor of ditching it. Whatever we gain we lose in the ability to generate custom views on issues. Which is a powerful filtering mechanism for prioritization.

  Also we have and continue to accept patches just uploaded
  to Trac as a diff. We tend to ask people to upload it to phab for better reviews
  and so it's attributed to them when we commit. Some don't (and we then do it ourselves),
  most due. If they don't need another login then I suspect almost all would.

  There's a (seemingly) actively maintained project that does all the above, could we leverage it?
  https://github.com/trac-hacks/trac-github

- Allow doc changes on Trac

  I am not sure about the feasibility of this one, but there is a Trac plugin that adds a new section to trac under /doc that is able to render .rst files and more from a source repository. Essentially making a wiki from them.

  https://github.com/trac-hacks/tracdocs

  We could have harbormaster build the docs and provide them to trac. I am not sure how reviews would go. Styling would be another issue. 

  Lastly this plugin seems to only support SVN. but that's not a huge barrier.

- Helping Newcomers

   * Create a better landing page for newcomers (see proposal below)
   * Show newcomers tickets more prominently
   * Clean up build instructions. The pages have become way too complex over the years. 
   * Create native setup script particular to an environment.

  As I am a windows maintainer, I will offer a proposal: Use scripts similar to my own setup scripts. my setup script for a 100% unattended build env setup for Windows are here: https://github.com/Mistuke/GhcDevelChoco/releases

  These are entirely self contained environments that can be removed by a simple rm -rf /.
  You can have as many as you want on the same machine without them interfering with eachother or with whatever else you might have done to your GHC already installed.
   
  It's not 100% production ready but it works and does so well.

  This script will setup an environment containing (if you want):
   * A full msys setup
   * gdb/gcc etc for debugging
   * check out Haskell sources
   * do a local install of php and arc
   * configure arc
   * setup Hadrian
   * setup SSH (for those who want to interface with it this way)
 
  The script is set up to provide you with a working environment with little interaction needed. Setup is just one call. 

  For Windows, such a script is a much better solution than Docker, simply because Docker does not work very well for Windows targets.

- Phab reviewers list

  Reviewers are currently assigned based on a set of Phabricator rules. As a maintainer on a less popular platform I know the pain of getting people to review your code.
  
  So my suggestion is to also assign reviewers based on the git history of the files you're changing. 

  The reason for this is that currently it's always the same people reviewing patches.
  Their time is spread thin. Particularly on less popular platforms it basically comes down to 4 people.
  
- Update trac linters

  The git linters and Arc linters aren't currently checking the same things.

  Particularly is the fact that the pre-commit hooks check
  the summary text but arc doesn't. So commits can get rejected at push time requiring more fork for maintainers.

  Also I want to say I love the summary document you have to fill in.
  It ensures useful information is there later when I have to find out why a change was made. So whatever we do, don't remove this.

- Phab signup screen

  It's recently been made a requirement to require a public key to push to phab. The error you get when you don't do this and try to push a patch is very very cryptic and unintuitive. Could we make a plugin that asks the user to upload a public key on trac if they haven't done so? Like a banner at the top?

- Add some automation to Trac (Or whatever bug tracker we end up using)

  - Particular on new tickets post a friendly reminder that if they want they can give it a hand in fixing it themselves.
  - Parse information added, in particular check if reproduction steps are there etc.
  - If stack is used, kindly ask if a repo without can be used. The amount of bug reports with stack is increasing and regardless of my own opinion on the tool, these reports are not very useful as is.
  - Maybe automatically CC people from a pool based on the information in the ticket? I tend to miss tickets because my filters are quite strict. Generally if the ticket doesn't mention my name, is directed at me or has "Windows" in the body somewhere it will skip my inbox. I review filtered tickets only once a week.
  - If a newcomer assigns a ticket to themselves, have trac automatically post links to useful pages:
  
      - how to setup build environment.
      - how to get help.
      - assign a mentor?
      - after x amount of time with no progress, remind them again that help is available

- Unify the issue tracker and code review into phabricator

- Move the Wiki someplace else. Is the wiki in phabricator any good, maybe we should use it.
  However, finding something in the ghc wiki has been more hit and miss for me than anything else. The
  built in search almost never reveals what I’m looking for, and resorting to a search engine only sometimes
  helps me find what I’m looking for. (This might totally be my fault).

- Reduce the number of Mailing lists. Some people are supposedly not on the haskell ml; so they missed emails such as
  https://mail.haskell.org/pipermail/haskell/2016-September/024995.html, and only saw it due to
  https://twitter.com/ezyang/status/780134457101516800.
  Are our MLs really that high volume that we need so many?

- Allow trivial pull requests to be accepted on GH as well. It shouldn't be as a big hurdle
  to ask someone to please be so kind and submit the patch through phabricator if it’s more involved with a
  link to https://www.haskell.org/contributing (see above).

- Add a dedicated contribution page to haskell.org

  As @mpickering pointed out, there are many projects to contribute to. Hence we suggest:
  https://www.haskell.org/contributing to provide a list of haskell project to contribute to.
  https://www.haskell.org/contributing/ghc to list the following content
  
  
    GHC Development is facilitated through phabricator
    
    Please go to https://phabricator.haskell.org and
    create an account.
    
    If you want to report a bug, please file a bug report through
    the “Maniphest” module, you can find on the left.
    
    If you are looking for something to contribute, and browse the
    Open Tasks at https://phabricator.haskell.org/maniphest/query/open/
    
    Clone the GHC tree and build it
    [ clone and build instructions here; note about stage2 pinning to reduce compile times and other
    build system features ]
    
    Hack to your hearts content on GHC (you might get some quick responses
    regarding ghc’s internals at irc://irc.freenode.net/ghc, as well as in
    the ghc commentary at ..., or the ghc-dev mailing list for which you
    can sign up at ...)
    
    Validate your build [ plus instructions how to do so, and how to run
    performance measurements on the changed ghc; if one is interested in
    that as well ] (MP: This should perhaps suggest just submitting a diff and let phab validate).
    
    Upload your patch to Phabricator using the arc command line tool. You will have
    to upload your public key during the first use, just follow the instructions.
    
    To upload your patch, commit your local changes; and run
    `$ arc diff origin/master`
    
    arc will run a few linters against your diff, and provide you with a form
    to fill in all the details regarding your patch. This form is usually
    prepopulated with the commit messages from your local commits.
    
    During the review process you might have to update your diff. To do so
    you can rebase your changes against the most recent master and/or add
    additional commits to it; once you are done updating your patch, run
    `$ arc diff --update`
    
    Once your diff has been accepted, someone with commit rights with “land” your
    diff into the official ghc tree.

On this note. I'd like to see haskell.org/contributing to match the style of
haskell.org, as well as being built thorugh a static page generater off of a
git repository (e.g. like GH Pages with Jekyll). That way contributing to those
would provide trivial history and allow easy collaboration.


Drawbacks
---------

These changes have potentially higher maintenance overhead. But they should be additional to what we have. If they fall out it shouldn't severely inconvenience us. 

Alternatives
------------

There are no alternatives ;-)

Unresolved Questions
--------------------

- Who is in charge of the specific webservices.
- These proposals rely on several open source projects. Are we willing to take on the liability of relying on their maintenance?
