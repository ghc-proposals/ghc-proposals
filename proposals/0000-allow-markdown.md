---
proposal-number: ""
ticket-url: ""
implemented: ""
---

# Allow Proposals to be Written in Markdown

The current proposal process requires proposals to be written in ReST. This is a
frustrating barrier to entry, and doesn't seem to pay its rent in real-life
proposals.

This will not deprecate writing ReST, should a proposal author wish to do so.


## Motivation

Markdown is a widely adopted document format that we can expect users to know;
it's supported on Github, reddit, Slack, and to some degree, Facebook Messenger.
Furthermore, it's is the default format for most non-WYSIWYG blogging software.
Markdown makes the common cases easy.

ReStructuredText, on the other hand, doesn't have nearly the market-share. The
proposal template implicitly acknowledges this, in that it starts with a section
users are expected to delete, explaining how to do basic things in ReST.

A quick Github code search agrees with this. There are [180M Markdown
documents](https://github.com/search?l=&q=filename%3A%2A.md+language%3AMarkdown&type=Code)
on Github, compared with only [14M for
ReST](https://github.com/search?l=&q=filename%3A%2A.rst+language%3AreStructuredText&type=Code).

Furthermore, ReST is significantly more verbose than Markdown.  For a comparison
of everyday constructs in both languages, see the appendix at the end of this
document.

The ghc-proposals README offers this in the way of explanation:

> Note that proposals are written in ReStructuredText rather than Markdown for
> its expressiveness and ease of integration into other GHC infrastructure.  See
> the GHC Users Guide for a brief introduction to ReStructuredText.


### Nobody actually uses ReSt's expressiveness

To see if anyone actually uses this added expressiveness, I forked the
`ghc-proposals` repository and grepped around in it for the constructs mentioned
in the GHC Users Guide. The results are not favorable for ReST.

We're told we can reference other documents in the Users Guide (a feature which
obviously can't work when the proposal is *designed to be viewed on Github*.)
This comes in the form `:ref:`, as well as package haddocks in the form
`:<package>-ref:`.

```shell
$ grep -r ':\([A-Za-z-]*\)ref:' *
index.rst:* :ref:`genindex`
index.rst:* :ref:`modindex`
index.rst:* :ref:`search`
```

We're told we can do math via `:math:`:

```shell
$ grep -r ':math:' * | wc -l
0
```

Even if math were used, it isn't rendered in either the [GHC Users
Guide](http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/editing-guide.html#math),
nor on Github.

The other features described in the Users Guide have a similarly absent usage:

```shell
# index entries
$ grep -r '^\.\. \+index::' * | wc -l
0

# citations
$ grep -r ']_' * | wc -l
0

# admonitions
$ grep -r '^\.\. \+\(attention\|error\|note\|caution\|hint\|tip\|danger\|important\|warning\)::' * | wc -l
0

# command-line options
$ grep -r '^\.\. \+\(rts-flag\|ghc-flag\)::' * | wc -l
0

# ghci options
$ grep -r '^\.\. \+\(rts-flag\|ghc-flag\)::' * | wc -l
0
```

*Literally nobody* is using the extra expressiveness of ReST when writing
proposals.


### Tying into other GHC workflows

As best I can tell, there is no integration of the proposal process with any
existing GHC infrastructure. Even if there were, I doubt that the utility gained
from it is more than the very real frustration in writing ReST.

One potential argument to this proposal is that "since we're writing GHC's
documentation in ReST, we should use it for the proposal process too." I don't
think this holds water. If a proposal is passed, the proposal text is never
going to be sufficient for direct inclusion into the Users Manual.

Furthermore, there's a lazy-evaluation solution here. Rather than force everyone
to write ReST *just in case*, let's only require someone to write ReST as part
of a documentation MR on GHC itself. There's no reason to leak GHC
implementation details into the proposal process.



## Proposed Change Specification

We will copy `0000-template.rst` to `0000-template.md`, and replace the ReST
formatting with Markdown formatting.

We will then remove the

> Note that proposals are written in ReStructuredText rather than Markdown for
> its expressiveness and ease of integration into other GHC infrastructure.  See
> the GHC Users Guide for a brief introduction to ReStructuredText.

stanza from the README.



## Costs and Drawbacks

We will need to keep the `rst` and `md` templates in sync. This is not very
costly, the template has changed only 11 times in 3 years.


## Unresolved Questions

Are there any actual integrations between `ghc-proposals` and the GHC
infrastructure? If so, will this change break anything?


## Implementation Plan

If accepted, I will implement it.


## Appendix: ReST verbosity

```diff
@@ headings
- This is a big long title
- ========================
+ # This is a big long title
```

This one is especially egregious, since the ReST parser requires the exact same
number of `=`s as characters in the line above it.

```diff
@@ links
- `hello world <https://haskell.org>`_
+ [hello world](https://haskell.org)
```

```diff
@@ inline code
- ``foo :: Int``
+ `foo :: Int`
```

```diff
@@ block code
- ::
-
-     foo :: Int
-     foo = 5
-
+ ```haskell
+ foo :: Int
+ foo = 5
+ ``
```

Note that markdown doesn't require indentation of the code block, nor does it
require blank lines before and after it.

```diff
@@ inline code
- ``foo :: Int``
+ `foo :: Int`
```

