---
author: Baldur Blöndal
date-accepted: ""
proposal-number: "325"
ticket-url: "https://github.com/ghc-proposals/ghc-proposals/pull/325"
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/325   ).

# Give error messages unique reference IDs

This is an idea I've thought about and assumed there is a good reason software doesn't do this.

Mark error messages with some kind of ID

```haskell
> class a

<interactive>:5:7: error:
    [GHCERR_b356c55] Malformed head of type or class declaration: a
```

that way if we completely rewrite the error message we can still
search for "`GHCERR_b356c55`" and will get results for *both* versions. 

```haskell
> class a

<interactive>:5:7: error:
    [GHCERR_b356c55] Hey you really can't write such things: a
```


## Motivation

This is basically for better googling, it would clutter the error messages but I am curious about the discussion.
