---
author: Ryan Hendrickson
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/676>).

# An empty-list shortcut for list comprehensions

As currently implemented, every generator in a list comprehension is always
fully traversed. In cases when a generator or guard does not depend on any
variables bound by the previous generator, an empty list or a false guard means
that one or more of the previous generators don't have to be fully traversed—if
there is no dependency, the ‘failing’ expression cannot evaluate to a different
result for the remaining elements of the earlier generator(s). This proposal
introduces alternate strategies for desugaring such list comprehensions that
improve (with a caveat) performance on some inputs, and in cases involving
infinite inputs allow some list comprehensions to terminate that currently do
not.

(The caveat: list fusion. Much more on this later.)

To forestall any potential confusion, this proposal is *not* about a user-facing
syntax or API for breaking out of loops early. This is only about an
optimization GHC can apply to list comprehensions as they are written today.

## Motivation

Consider the list comprehension `[ (x, y) | x <- [1..3], y <- f x ]`. Deeply
evaluating this expression requires all of `f 1`, `f 2`, and `f 3` to be
evaluated, and there's no way around this. Contrast with
`[ (x, y) | x <- [1..3], y <- f 0 ]`. Now the second generator doesn't depend on
`x`, and so instead of requiring three different `f` expressions to be
evaluated, only `f 0` needs to be evaluated once and used three times.

In `[ (x, y) | x <- [1..3], y <- f 0 ]`, if `f 0` is an empty list, the entire
list comprehension is an empty list. This is true regardless of whether the
first generator reads from `[1..3]` or `[1..300000]`. The difference is that the
second causes the list comprehension to take measurably more time and memory to
compute the empty list than the first does. But there's no need, in principle,
to discover that `f 0` is empty 300000 times. Once will do.

Similarly, regardless of whether one writes `[ x | x <- [1..3], False ]` or
`[ x | x <- [1..300000], False ]`, the result will be empty. But again, the
second expression requires more time and memory to compute. (This situation is
easier to work around, since moving the guard leftward in the comprehension would
cause it to be checked first and have no other side effects. Contrast with
generators, which when rearranged can reorder the final result. But even so,
users may want to place a guard that is very expensive to compute even once
after a generator that is likely to be empty (or guarded by a cheaper guard),
even if there is no data dependency between the two. See [Example 2](#example-2)
for a concrete illustration of this situation.)

Note that as an extreme case, evaluating `[ () | _ <- repeat 0, _ <- [] ]` to
WHNF does not terminate. But `[ () | _ <- [], _ <- repeat 0 ]` swiftly evaluates
to `[]`. From a mathematical perspective, the two expressions should contain the
same elements, in a different order; so the first expression should evaluate to
`[]` as well. Only the list comprehension desugaring strategy prevents it from
doing so.

The relevant difference between `[ (x, y) | x <- [1..3], y <- f x ]` and
`[ (x, y) | x <- [1..3], y <- f 0 ]` is whether the second generator depends on
a variable bound by the first. More generally, an optimization is possible
whenever a generator or a guard in a list comprehension does not depend on any
variables bound by the previous generator. This optimization can help even if
the entire list comprehension is not empty! Consider the following:

```hs
[ (x, y, z) | x <- [1..3], y <- f x, z <- g x ]
```

This expression will traverse each of `f 1`, `f 2`, and `f 3`, any of which may
be long lists. Suppose `g 2` is empty. Then there is no need to check `g 2` for
emptiness for every element of `f 2`; after the first such check, the
comprehension should continue on to processing the elements of `f 3`. But even
so, the overall list comprehension will be non-empty if `f 1` and `g 1` are both
non-empty or if `f 3` and `g 3` are both non-empty.

Informally, the strategy is this: Imagine representing a list comprehension as a
series of nested loops in an imperative language that supports labelled `break`
statements. For this most recent example, something like the following:

```js
var result = [];
label0:
for (var x of [1..3]) {
  label1:
  for (var y of f(x)) {
    label2:
    for (var z of g(x)) {
      result.push((x, y, z))
    }
  }
}
```

And then for every loop where the iteratee doesn't depend on the variable bound
in the nearest enclosing loop, test the iteratee for emptiness. When empty, add
a `break` referring to the loop inside the nearest enclosing loop that *does*
bind a variable used by the iteratee (or `break` the outermost loop, if no
enclosing loop binds any variables used by the iteratee).

```js
var result = [];
label0:
for (var x of [1..3]) {
  label1:
  for (var y of f(x)) {
    var g_x = g(x);
    if (isEmpty(g_x)) {
      break label1; // g_x will remain empty until x changes
      // if instead of g(x), we used g(0), this would be label0
    }
    label2:
    for (var z of g_x) {
      result.push((x, y, z))
    }
  }
}
```

(The same logic applies, *mutatis mutandis*, for guards.)

Note: in the imperative language, this `isEmpty` check may require extra time to
perform; we'll see in the next section when this strategy is expressed in
Haskell that the optimization doesn't introduce any additional `case`
reductions.

The intended handling of `let`s deserves a quick note. Suppose we had:

```hs
[ (x, y, z) | x <- [1..3], y <- f x, let zs = g x, z <- zs ]
```

The definition of `zs` doesn't depend on `y`, and so one might reasonably
propose that this receives the same optimization as the previous example (which
is the same as this example with `zs` inlined). In order to keep the
implementation simple, however, I don't propose this. Rather, I propose treating
`zs` in this example as if it were also bound by the generator expression
preceding it. If the `let zs` is moved leftward as follows:

```hs
[ (x, y, z) | x <- [1..3], let zs = g x, y <- f x, z <- zs ]
```

then this example will see the same optimization as before, in which an empty
`zs` results in a jump to the next value of `x`.

## Proposed Change Specification

GHC uses two desugaring strategies for list comprehensions: a ‘normal’ strategy
and a ‘foldr/build’ strategy. I propose changing them both.

### Normal strategy

The existing strategy is described in the GHC source with the following comment:

```
TE << [ e | qs ] >>  =  TQ << [ e | qs ] ++ Nil (typeOf e) >>

(Rule C)
TQ << [ e | ] ++ L >> = Cons (typeOf e) TE <<e>> TE <<L>>

(Rule B)
TQ << [ e | b , qs ] ++ L >> =
    if TE << b >> then TQ << [ e | qs ] ++ L >> else TE << L >>

(Rule A')
TQ << [ e | p <- L1, qs ]  ++  L2 >> =
  letrec
    h = \ u1 ->
          case u1 of
            []        ->  TE << L2 >>
            (u2 : u3) ->
                  (( \ TE << p >> -> ( TQ << [e | qs]  ++  (h u3) >> )) u2)
                    [] (h u3)
  in
    h ( TE << L1 >> )

"h", "u1", "u2", and "u3" are new variables.
```

With all due respect to whichever honored elder originally transcribed this into
ASCII, it's sloppy notation. (I challenge the reader to describe the
metasyntactic conventions under which
```
                  (( \ TE << p >> -> ( TQ << [e | qs]  ++  (h u3) >> )) u2)
                    [] (h u3)
```
means what it was intended to mean.)

Below, I rewrite this strategy as Template Haskell, so that when I propose
modifying it, we all start on the same page for what the metasyntax means.

```hs
desugarListComp :: [Stmt] -> ExpQ
desugarListComp sts = tq sts [| [] |]
  where
  tq :: [Stmt] -> ExpQ -> ExpQ
  tq (NoBindS e : [])  l  = [| $(pure e) : $l |]
  tq (NoBindS b : qs)  l  = [| if $(pure b) then $(tq qs l) else $l |]
  tq (BindS p l1 : qs) l2 =
    [|
      let
        h [] = $l2
        h (x : xs) = case x of
          $(pure p) -> $(tq qs [| h xs |])
          _         -> h xs
      in h $(pure l1)
    |]
  -- Not in the rules, but for completeness:
  tq (LetS decs : qs)  l = letE (map pure decs) $ tq qs l
  -- Parallel comprehensions assumed to be handled outside of this translation;
  -- nothing proposed here should affect them.
```

The above function works in part by passing a ‘tail expression’ around in the
second argument to `tq`. The main thrust of this proposal is to replace the tail
expression with a ‘tail stack’, defined as follows (again using TH types; this
would of course be adapted to use GHC types in the implementation of this
proposal):

<details open>
<summary><code>TailStack</code> and API (click to hide)</summary>

```hs
-- | Stores a stack of in-scope variables: one at the bottom (representing the
-- end of the list comprehension), plus one for each generator, along with a
-- list of names bound by that generator. The variables reference suffixes of
-- the list comprehension, such that consing on one is like a ‘break’ out of
-- the iterations up to the corresponding generator (or out of the entire list
-- comprehension, in the case of the bottom variable).
data TailStack
  = Bottom Name
  | Push [Name] Name TailStack

-- | Take the top variable from the stack.
top :: TailStack -> Name
top (Bottom n) = n
top (Push _ n _) = n

-- | Push a new variable onto the stack, tagged with the list of names bound by
-- the given pattern.
push :: Pat -> Name -> TailStack -> TailStack
push = Push . namesBoundInPat -- as implemented in `th-extras`

-- | If the top element of the stack is a Push, prepend the names bound by a
-- list of Decs to it. This is used in desugaring let statements, to associate
-- let-bound names with their closest generator.
insertDecs :: [Dec] -> TailStack -> TailStack
insertDecs _ ts@Bottom{} = ts
insertDecs decs (Push boundHere n ts) = Push (foldr f boundHere decs) n ts
  where
  f = (++) . namesBoundInDec -- as implemented in `th-extras`

-- | For an expression `e`, get the variable from the stack corresponding to
-- the next point in the comprehension at which the names used in `e` may take
-- new values. The accompanying Bool is False if this variable is from the top
-- of the stack (in which case smaller code can be generated by skipping the
-- shortcut optimization).
breakLoop :: Exp -> TailStack -> (Bool, Name)
breakLoop e = either (False, ) ((True, ) . go) . step
  where
  step (Bottom n) = Left n
  step (Push boundHere n ts)
    | any (`elem` fvs) boundHere = Left n
    | otherwise = Right ts
  fvs = namesFreeInExp e -- as implemented in, uh, our imaginations?
  go = either id go . step
```

</details>

With that machinery in place, the new ‘normal’ desugaring strategy will be:

```hs
desugarListComp' :: [Stmt] -> ExpQ
desugarListComp' sts = [| let n = [] in $(tq sts $ Bottom 'n) |]
  where
  tq :: [Stmt] -> TailStack -> ExpQ
  tq (NoBindS e : []) ts = [| $(pure e) : $(varE $ top ts) |]
  tq (NoBindS b : qs) ts =
    [| if $(pure b) then $(tq qs ts) else $(varE $ snd $ breakLoop b ts) |]
  tq (BindS p l : qs) ts | (doShortcut, brk) <- breakLoop l ts =
    [|
      let
        h [] = $(varE $ top ts)
        h (x : xs) = h1 x xs
        h1 x xs = case x of
          $(pure p) -> let t = h xs in $(tq qs $ push p 't ts)
          _         -> h xs
      in $(
        if doShortcut then
          [|
            case $(pure l) of
              [] -> $(varE brk)
              (x : xs) -> h1 x xs
          |]
        else [| h $(pure l) |]
      )
    |]
  tq (LetS decs : qs) ts = letE (map pure decs) $ tq qs $ insertDecs decs ts
```

(I realize that writing what is more or less a full implementation, even if it's
a Template Haskell prototype, is perhaps contrary to the spirit of the
specification part of the GHC proposal process. But this is the best way I can
think of to communicate the proposed changes without ambiguity.)

Note that, as promised, there are no new `case` reductions inserted by the
optimization. The `case` expression that appears in the `doShortcut` branch
takes the place of the `case` expression that would be reduced in the first call
to `h`.

### foldr/build

Again starting from comments in the GHC source:

```
TE[ e | ]            c n = c e n
TE[ e | b , q ]      c n = if b then TE[ e | q ] c n else n
TE[ e | p <- l , q ] c n = let
                                f = \ x b -> case x of
                                                  p -> TE[ e | q ] c b
                                                  _ -> b
                           in
                           foldr f n l
```

This is more coherent than the other comment, but for comparison purposes I'm
translating it into TH anyway:

```hs
desugarListCompFB :: [Stmt] -> ExpQ
desugarListCompFB sts = [| build (\c n -> $(te sts [| c |] [| n |])) |]
  where
  te :: [Stmt] -> ExpQ -> ExpQ -> ExpQ
  te (NoBindS e : []) c n = [| $c $(pure e) $n |]
  te (NoBindS b : q)  c n = [| if $(pure b) then $(te q c n) else $n |]
  te (BindS p l : q)  c n =
    [|
      let
        f = \ x b -> case x of
          $(pure p) -> $(te q c [| b |])
          _         -> b
      in
      foldr f $n $(pure l)
    |]
  te (LetS decs : q)  c n = letE (map pure decs) $ te q c n
```

Again the thrust is to replace an argument with `TailStack`, with the same API
as before:

```hs
desugarListCompFB' :: [Stmt] -> ExpQ
desugarListCompFB' sts = [| build (\c n -> $(te sts [| c |] $ Bottom 'n)) |]
  where
  te :: [Stmt] -> ExpQ -> TailStack -> ExpQ
  te (NoBindS e : []) c ts = [| $c $(pure e) $(varE $ top ts) |]
  te (NoBindS b : q)  c ts =
    [| if $(pure b) then $(te q c ts) else $(varE $ snd $ breakLoop b ts) |]
  te (BindS p l : q)  c ts | (doShortcut, brk) <- breakLoop l ts =
    [|
      let
        f = \ x b -> case x of
          $(pure p) -> $(te q c $ push p 'b ts)
          _         -> b
      in $(
        if doShortcut then [| foldrOrElse f $(varE $ top ts) $(varE brk) |]
                      else [| foldr f $(varE $ top ts) |]
      ) $(pure l)
    |]
  te (LetS decs : q)  c ts = letE (map pure decs) $ te q c $ insertDecs decs ts
```

See the next section for the definition of `foldrOrElse`, a new function to be
added in userspace.

## Proposed Library Change Specification

`foldrOrElse` is to be added... somewhere... (`GHC.List`?) with the following
definition:

```hs
foldrOrElse :: (a -> b -> b) -> b -> b -> [a] -> b
foldrOrElse _ _ z0 [] = z0
foldrOrElse f z _ (x : xs) = f x (foldr f z xs)
```

It needs to be given a top-level definition so that it has a chance at
participating in list fusion.

Speaking of list fusion, the alert reader may have noticed that using
`foldrOrElse` instead of `foldr` means (I think?) that list comprehensions will
no longer always be ‘good consumers’ of their generators. This is the point of
the draft-proposal at which all of my confidence throws a martini in my face and
storms out of the building. I don't even know—not for lack of searching—what the
definition of a ‘good consumer’ is, but from what I think I understand, for list
comprehensions to remain good consumers I would need to write a rewrite rule
that will fuse `foldrOrElse f z z0 (build g)`. This is a problem because the
shape of `g` allows only for a uniform `z` to be provided. I've experimented
with rewrite rules of the form
``foldrOrElse f z z0 (build (\c n -> x `c` (g c n))) = x `f` g f z`` and
`foldrOrElse f z z0 (build (\c n -> n)) = z0`, but they don't seem to work very
well, due to either limitations in how GHC does rewrite pattern matching or
limitations in my ability to write rewrite rules (or both). But even those
rules, if they worked, would not handle the cases in which it isn't statically
known whether a `build`-encoded list is empty.

I wouldn't want to propose a new optimization that makes an existing widely-used
optimization less effective. But I can think of only one way to salvage this
scenario, and that would be adding and using a `build1` variant with a signature
that matches `foldrOrElse`:

```hs
build1 :: (forall b. (a -> b -> b) -> b -> b -> b) -> [a]
build1 g = g (:) [] []

{-# RULES
"foldrOrElse/build1" forall k z z0 (g :: forall b. (a -> b -> b) -> b -> b -> b) .
                     foldrOrElse f z z0 (build1 g) = g f z z0
"foldr/build1"       forall k z (g :: forall b. (a -> b -> b) -> b -> b -> b) .
                     foldr f z (build1 g) = g f z z
  #-}
```

And then going on a campaign to rewrite uses of `build`, which won't work with
list comprehensions that use `foldrOrElse`, to uses of `build1`, which will.
This is the bit that I think may want to be its own proposal.

I don't know if this scuttles the whole idea. I imagine there are other times in
which it would be nice to have fusion-friendly behavior conditional on a list
being empty (and thus other use cases for `build1` than this proposal), but
someone with actual experience with pain points in list fusion should do that
evaluation. The `z0` parameter certainly lacks a certain elegance, and one could
reasonably ask why stop at this number of `b`-typed arguments—if `build1` is
superior to `build`, is there going to be a later proposal calling for a
`build2`? I don't have a reasoned answer to that question. I feel like `build1`
should be enough, but that's just intuition.

## Examples

### Example 1

As a worked example (specifying in TH makes it easy to ask an unmodified GHC for
this!), here is a comparison of
`[ (x, y, z) | x <- [1..3], y <- f x, z <- g x ]` run through each strategy:

#### Normal (old)
```hs
    let
      h_a6bl [] = []
      h_a6bl (x_a6bm : xs_a6bn)
        = case x_a6bm of
            x_a6bi
              -> let
                   h_a6bo [] = h_a6bl xs_a6bn
                   h_a6bo (x_a6bp : xs_a6bq)
                     = case x_a6bp of
                         y_a6bj
                           -> let
                                h_a6br [] = h_a6bo xs_a6bq
                                h_a6br (x_a6bs : xs_a6bt)
                                  = case x_a6bs of
                                      z_a6bk -> ((x_a6bi, y_a6bj, z_a6bk) : h_a6br xs_a6bt)
                                      _ -> h_a6br xs_a6bt
                              in h_a6br (g x_a6bi)
                         _ -> h_a6bo xs_a6bq
                 in h_a6bo (f x_a6bi)
            _ -> h_a6bl xs_a6bn
    in h_a6bl [1 .. 3]
```

#### Normal (new)
```hs
    let n_a6bk = [] in
    let
      h_a6bl [] = n_a6bk
      h_a6bl (x_a6bn : xs_a6bo) = h1_a6bm x_a6bn xs_a6bo
      h1_a6bm x_a6bp xs_a6bq
        = case x_a6bp of
            x_a6bh
              -> let t_a6br = h_a6bl xs_a6bq in
                 let
                   h_a6bs [] = t_a6br
                   h_a6bs (x_a6bu : xs_a6bv) = h1_a6bt x_a6bu xs_a6bv
                   h1_a6bt x_a6bw xs_a6bx
                     = case x_a6bw of
                         y_a6bi
                           -> let t_a6by = h_a6bs xs_a6bx in
                              let
                                h_a6bz [] = t_a6by
                                h_a6bz (x_a6bB : xs_a6bC) = h1_a6bA x_a6bB xs_a6bC
                                h1_a6bA x_a6bD xs_a6bE
                                  = case x_a6bD of
                                      z_a6bj
                                        -> let t_a6bF = h_a6bz xs_a6bE
                                           in ((x_a6bh, y_a6bi, z_a6bj) : t_a6bF)
                                      _ -> h_a6bz xs_a6bE
                              in
                                -- This is the optimization:
                                case g x_a6bh of
                                  [] -> t_a6br -- skip processing the rest of xs_a6bv
                                  (x_a6bG : xs_a6bH) -> h1_a6bA x_a6bG xs_a6bH
                         _ -> h_a6bs xs_a6bx
                 in h_a6bs (f x_a6bh)
            _ -> h_a6bl xs_a6bq
    in h_a6bl [1 .. 3]
```

Most of the changes here are binding subexpressions to variables, which is done
fairly proactively in order to prevent subexpressions from being duplicated
(trusting that the compiler will inline anything that makes sense to inline
later).

Notice that in the region of code marked as the optimization, there are no
*additional* reductions done relative to the old desugaring. There is no extra
empty-list test; the shortcut is handled as part of examining the first element
of the list.

#### foldr/build (old)
```hs
    GHC.Base.build
      (\ c_a8rQ n_a8rR
         -> let
              f_a8rS
                = \ x_a8rT b_a8rU
                    -> case x_a8rT of
                         x_a8rN
                           -> let
                                f_a8rV
                                  = \ x_a8rW b_a8rX
                                      -> case x_a8rW of
                                           y_a8rO
                                             -> let
                                                  f_a8rY
                                                    = \ x_a8rZ b_a8s0
                                                        -> case x_a8rZ of
                                                             z_a8rP
                                                               -> c_a8rQ
                                                                    (x_a8rN, y_a8rO, z_a8rP) b_a8s0
                                                             _ -> b_a8s0
                                                in foldr f_a8rY b_a8rX (g x_a8rN)
                                           _ -> b_a8rX
                              in foldr f_a8rV b_a8rU (f x_a8rN)
                         _ -> b_a8rU
            in foldr f_a8rS n_a8rR [1 .. 3])
```

#### foldr/build (new)
```hs
    GHC.Base.build
      (\ c_a8rO n_a8rP
         -> let
              f_a8rQ
                = \ x_a8rR b_a8rS
                    -> case x_a8rR of
                         x_a8rL
                           -> let
                                f_a8rT
                                  = \ x_a8rU b_a8rV
                                      -> case x_a8rU of
                                           y_a8rM
                                             -> let
                                                  f_a8rW
                                                    = \ x_a8rX b_a8rY
                                                        -> case x_a8rX of
                                                             z_a8rN
                                                               -> c_a8rO
                                                                    (x_a8rL, y_a8rM, z_a8rN) b_a8rY
                                                             _ -> b_a8rY
                                                -- The only non-renaming change is this line:
                                                in foldrOrElse f_a8rW b_a8rV b_a8rS (g x_a8rL)
                                           _ -> b_a8rV
                              in foldr f_a8rT b_a8rS (f x_a8rL)
                         _ -> b_a8rS
            in foldr f_a8rQ n_a8rP [1 .. 3])
```

The fact that this desugaring does no additional reductions follows from the
definition of `foldrOrElse`.


### Example 2

This example demonstrates how guards and lets are processed.

```hs
[ (x, y)
| x <- [1..3]
, let cond = expensiveTest x
, y <- f x
, x > y
, cond
]
```

(Note that the difference between `x > y, cond` and `x > y && cond` is now
meaningful! The latter would not cause the generated code to change under this
optimization because the guard as a whole depends on both `x` and `y`. But when
`cond` is a separate guard, a shortcut to the next `x` is made available for
when `cond` is false. I hasten to reiterate that, assuming the list
comprehension terminates with both choices, there is of course no difference in
the final result, only in performance characteristics.)

#### Normal (old)
```hs
    let
      h_a8xk [] = []
      h_a8xk (x_a8xl : xs_a8xm)
        = case x_a8xl of
            x_a8xh
              -> let cond_a8xi = expensiveTest x_a8xh in
                 let
                   h_a8xn [] = h_a8xk xs_a8xm
                   h_a8xn (x_a8xo : xs_a8xp)
                     = case x_a8xo of
                         y_a8xj
                           -> if (x_a8xh > y_a8xj) then
                                  if cond_a8xi then
                                      ((x_a8xh, y_a8xj) : h_a8xn xs_a8xp)
                                  else
                                      h_a8xn xs_a8xp
                              else
                                  h_a8xn xs_a8xp
                         _ -> h_a8xn xs_a8xp
                 in h_a8xn (f x_a8xh)
            _ -> h_a8xk xs_a8xm
    in h_a8xk [1 .. 3]
```

#### Normal (new)
```hs
    let n_a8FK = [] in
    let
      h_a8FL [] = n_a8FK
      h_a8FL (x_a8FN : xs_a8FO) = h1_a8FM x_a8FN xs_a8FO
      h1_a8FM x_a8FP xs_a8FQ
        = case x_a8FP of
            x_a8FH
              -> let t_a8FR = h_a8FL xs_a8FQ in
                 let cond_a8FI = expensiveTest x_a8FH in
                 let
                   h_a8FS [] = t_a8FR
                   h_a8FS (x_a8FU : xs_a8FV) = h1_a8FT x_a8FU xs_a8FV
                   h1_a8FT x_a8FW xs_a8FX
                     = case x_a8FW of
                         y_a8FJ
                           -> let t_a8FY = h_a8FS xs_a8FX
                              in
                                if (x_a8FH > y_a8FJ) then
                                    -- The optimization is using t_a8FR here
                                    -- instead of t_a8FY
                                    if cond_a8FI then ((x_a8FH, y_a8FJ) : t_a8FY) else t_a8FR
                                else
                                    t_a8FY
                         _ -> h_a8FS xs_a8FX
                 in h_a8FS (f x_a8FH)
            _ -> h_a8FL xs_a8FQ
    in h_a8FL [1 .. 3]
```

#### foldr/build (old)
```hs
    GHC.Base.build
      (\ c_a8FK n_a8FL
         -> let
              f_a8FM
                = \ x_a8FN b_a8FO
                    -> case x_a8FN of
                         x_a8FH
                           -> let cond_a8FI = expensiveTest x_a8FH in
                              let
                                f_a8FP
                                  = \ x_a8FQ b_a8FR
                                      -> case x_a8FQ of
                                           y_a8FJ
                                             -> if (x_a8FH > y_a8FJ) then
                                                    if cond_a8FI then
                                                        c_a8FK (x_a8FH, y_a8FJ) b_a8FR
                                                    else
                                                        b_a8FR
                                                else
                                                    b_a8FR
                                           _ -> b_a8FR
                              in foldr f_a8FP b_a8FO (f x_a8FH)
                         _ -> b_a8FO
            in foldr f_a8FM n_a8FL [1 .. 3])
```

#### foldr/build (new)
```hs
    GHC.Base.build
      (\ c_a8FH n_a8FI
         -> let
              f_a8FJ
                = \ x_a8FK b_a8FL
                    -> case x_a8FK of
                         x_a8FE
                           -> let cond_a8FF = expensiveTest x_a8FE in
                              let
                                f_a8FM
                                  = \ x_a8FN b_a8FO
                                      -> case x_a8FN of
                                           y_a8FG
                                             -> if (x_a8FE > y_a8FG) then
                                                    if cond_a8FF then
                                                        c_a8FH (x_a8FE, y_a8FG) b_a8FO
                                                    else
                                                        b_a8FL -- not b_a8FO
                                                else
                                                    b_a8FO
                                           _ -> b_a8FO
                              in foldr f_a8FM b_a8FL (f x_a8FE)
                         _ -> b_a8FL
            in foldr f_a8FJ n_a8FI [1 .. 3])
```


## Effect and Interactions

* As discussed above, this change will make list fusion less effective in some
  cases, which might be mitigated by work to replace `build` with `build1`.

* The `Applicative []` and `Monad []` instances are defined using list
  comprehensions, so unless those instances are changed, this optimization will
  apply to some members of those instances. This has some potentially remarkable
  consequences, because, for example, `repeat 0 >>= const []` doesn't terminate,
  and neither does `repeat 0 >> []` or `repeat 0 *> []` right now. But if this
  proposal is implemented and the aforementioned instances aren't changed,
  `repeat 0 >> []` and `repeat 0 *> []` will terminate (evaluating to `[]`). But
  the documentation for `(>>)` states that `as >> bs` should be ‘understood as’
  `as >>= const bs`. Is it a problem if one terminates and the other does not?

  I argue that it is not. When there are two expressions that should be
  equivalent and one can't terminate, we should not feel obligated to prohibit
  the other from terminating as well. Doing so doesn't make any program more
  correct; it only makes fewer programs terminate.

  Given that `[1..n] >> []` is `[]` for any `n`, there is no question that for
  an infinite `xs`, the correct behavior for `xs >> []` is either nontermination
  or `[]`—to specify otherwise would be to postulate a decision procedure for
  the finitude of a list. Arriving at this answer in finite rather than infinite
  time is therefore a valid optimization.

* There are three list comprehension extensions with which this proposal could
  interact: `ParallelListComp`, `TransformListComp`, and `MonadComprehensions`.
  The interaction with `ParallelListComp` is trivial; parallel comprehensions
  desugar to regular comprehensions, to which this transformation can apply
  unmodified.

  `TransformListComp` is dauntingly underspecified, and the paper cited in the
  documentation uses its own desugaring scheme distinct from either of the two
  described in the GHC source, but from reading the source I gather that the
  strategy actually used by this extension is to bundle up everything preceeding
  a `then` statement into an inner list comprehension, transform that inner
  list, and then use that as the first generator in an outer list comprehension.
  This proposal can apply within that desugaring, with partial success:

  ```hs
  [ (x, y, z) | x <- [1..3], y <- [1..3], then take n, z <- f x ]
  -- gets optimized like
  [ (x, y, z) | (x, y) <- take n [ (x', y') | x' <- [1..3], y' <- [1..3] ], z <- f x ]
  -- which is not optimal relative to how
  [ (x, y, z) | x <- [1..3], y <- [1..3], z <- f x ]
  -- is optimized (the former shortcuts to the next y if f x is empty,
  -- the latter to the next x).
  ```

  Basically, for empty-list-shortcut optimization purposes, all generators
  preceding a `then` statement are treated as a single generator, and skipped
  all together or not at all. I don't propose attempting to do better than this;
  I'm skeptical it's even possible to do so.

  `MonadComprehensions` have an entirely separate desugaring and this proposal
  does not attempt to modify that. (To improve the state of the art here, see
  [ghc#14700](https://gitlab.haskell.org/ghc/ghc/-/issues/14700), a request for
  `ApplicativeDo` to apply to `MonadComprehensions`, which assuming a
  suitably-written `Applicative` instance would have a similar effect.)

## Costs and Drawbacks

Excepting what-to-do-about-list-fusion, the desugaring changes seem likely to be
very local and not difficult to maintain. Accurately and completely explaining
the desugaring procedure is likely to be more involved, which may burden the
next person to try to learn about how GHC does these things. Most language users
are unlikely to be aware of the change at all.

The what-to-do-about-list-fusion bit could be a different story altogether; I
simply don't know.

## Backward Compatibility

This is somewhere between 0 and 1 on the scale:

0. No breakage
1. Breakage only in extremely rare cases (e.g. for specifically-constructed
   examples, but probably no packages published in the Hackage package repository)

The sort of extremely rare case I'm imagining is someone relying on an affected
list comprehension not terminating before a timeout kicks in, or catching an
expected `NonTermination` exception. I mention this only for completeness's
sake; in practice, I expect no breakage, as this only affects performance
characteristics of compiled programs.

## Alternatives

* Doing nothing.

* Leaning on [ghc#14700](https://gitlab.haskell.org/ghc/ghc/-/issues/14700) and
  stronger advocacy for `ApplicativeDo` and `MonadComprehensions`. This is
  tempting, but for the fact that list comprehensions are used to define the
  `Applicative []` instance, and I would like that instance to benefit from this
  optimization. Inlining the list comprehension desugaring in `Applicative []`
  is not a good idea because there are two different desugarings that GHC
  switches between based on compiler flags. (See
  `Note: [List comprehensions and inlining]`.)

* It would be possible to optimize `let`-statements more aggressively. Instead
  of associating a `let`-bound name with the preceding generator, we could
  associate it with the nearest preceding generator that binds a variable that
  the statement references, directly or via other `let`s.

  It's always possible for the user to move lazy `let`s around to get the
  advantage of the more aggressive optimization. But there could be a use case
  for having a strict `let` later in a list comprehension than it needs to be
  from a data-dependency perspective, and this alternative could be useful in
  optimizing such cases.

  This seems sufficiently niche that I think it probably isn't worth the
  complexity it would add to the desugaring, but I could be persuaded otherwise.

## Unresolved Questions

Please help me figure out what to do about list fusion!

## Implementation Plan

TODO, though I'm probably competent to do the desugaring part of this.

## Endorsements

TODO
