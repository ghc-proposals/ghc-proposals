---
author: Cate Roxl
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/382).


# Make learning and playing with Haskell through GHCi easier through adding `:clear [variable identifier]`.  

Adding :clear [variable identifier] that only applies to variable identifiers or functions defined in GHCi.

## Motivation

Haskell, as purely functional language, has been catching the attention of newcomers and experienced programmers who are wanting to learn Functional Programming/Thinking.

While I acknowledge immutability being the central concept of functional programming—perhaps it may be the reason why `:clear` doesn't exist, the proposed GHCi command may just make interacting with GHCi more convenient; and learning Haskell more fun.


## Proposed Change Specification

Let's say that we're taking a Haskell course, and we're asked to write a function to reverse a list. So we start up GHCi, then we go:

```
Prelude> rev x = reverse x
Prelude> rev "Haskell"
"lleksaH"
```

Now there are other ways to write such function, which we can play and study with. But because of immutability, once we declare `rev = reverse x` or any variable identifier to a variable: it stays that way. 

Unless. . . We quit GHCi and start it up again. 

It may seem to be such a small inconvenience, but it can actually disrupt the flow of some people because they can't just play with it straightaway.

Adding the `:clear [variable identifier/name]` may put an end to such inconvenience:

```
Prelude> rev x = reverse x
Prelude> rev "Haskell"
"lleksaH"
Prelude> :clear rev -- Instead of quitting, they can do this instead.
Prelude> rev [] = []; rev xs = last xs : rev (init xs)
Prelude> rev "haskell"
"lleksaH"
```

But `:clear [variable indentifier/name]` only applies to `variable identifiers` that has been defined in GHCi, and not in .hs files; so that people can still remember the concept of immutability.

```
Prelude> :load myfunction.hs
[1 of 1] Compiling Main             ( myfunction.hs, interpreted )
Ok, one module loaded.
*Main> rev 
"lleksaH"
*Main> :clear rev

<interactive>: (error message)
```
Such GHCi command can also be very useful to people who make mistakes in the definition of their functions.

## Examples


## Effect and Interactions


## Costs and Drawbacks


## Alternatives

Restarting GHCi, which I have explained above.

## Unresolved Questions


## Implementation Plan


## Endorsements

[Deleting a function defined in GHCi](https://stackoverflow.com/questions/53552152/deleting-a-function-defined-in-ghci)

