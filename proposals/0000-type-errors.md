---
author: Sandy Maguire
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/278).

# Better Custom Type Errors

The `GHC.TypeLits.TypeError` machinery is much too eager to emit `TypeError`s.
This proposal loosens them up significantly so that they:

* can produce better error messages
* don't get in the way of more important type errors
* aren't emitted at definition sites

This proposal is motivated by [a lot][lots] of experience with `TypeError`s
going wrong.

[lots]: https://github.com/polysemy-research/polysemy/issues?utf8=%E2%9C%93&q=is%3Aissue+label%3A%22custom+errors%22+


## Motivation

`TypeErrors` are littered with non-compositionality, and seem to explode at
every chance they get. Here are a few aggressively use-cases:

### TypeErrors are emitted at their definition sites, not their use sites

The following module will not compile:

```haskell
module Example where

type MyError = TypeError ('Text "This module doesn't compile")
```

(instead you'll see the error `This module doesn't compile`)

This is not just a contrived example; even [boot libraries run into this
problem.][containers]

[containers]: https://github.com/haskell/containers/blob/403cf3cb7ef365961269cebe9d4eeb7221edd927/containers/src/Utils/Containers/Internal/TypeError.hs#L17-L52

The eagerness of `TypeError` means we can't easily provide good error messages
for default implementations of classes (reported as a [GHC bug][bug]):

[bug]: https://gitlab.haskell.org/ghc/ghc/issues/16906

```haskell
class Foo a where
    foo :: a
    default foo :: TypeError
        (Text "Please define foo manually. Suggested definitions:" :$$:
         Text "  foo = something complicated" :$$:
         Text "  foo = something else complicated")
        => a
    foo = error "unreachable"
```

The behavior of `TypeError` today prevents this *class* from compiling, rather
than triggering on *instances* which forget to implement the `foo` method. Many
libraries provide several different generic means of implementing their class
methods --- because there is a choice, `DeriveAnyClass` can't be used, but it
would be helpful to inform the user of their options. [Early versions of
`polysemy`, for example, had multiple defaults.][polysemy]

[polysemy]: https://github.com/polysemy-research/polysemy/blob/b23f7d2561e2be7440f124c65218247f3afe849e/src/Polysemy/Internal/Effect.hs#L85-L124

There is a workaround for all of the above, but it depends on an undocumented
implementation detail of how type families expand.


### TypeErrors get in the way of good, useful errors

When used in value-level code, `TypeError`s will often *prevent legitimate type
errors* from being emitted. Consider the following encoding of a row type:

```haskell
type family Lookup (needle :: k) (haystack :: [k]) where
  ...

type Member effect row = Lookup effect row
```

We can write some code that uses the type level list:

```haskell
get :: Member (State s) row => Effect row s
get = ...
```

and attempt to use it:

```haskell
wrong1 :: Effect row Int
wrong1 = get
```

Here we'd like to get a custom error message saying `You should try adding
a (Member (State Int) row) constraint`, but instead we'll see something about a
stuck `Lookup effect row` constraint.  There is [a trick][detecting] for
producing a type error iff some type is stuck, which we can do here:

[detecting]: https://kcsongor.github.io/report-stuck-families/

```haskell
type MyUsefulTypeError needle haystack =
  ... something about `'ShowType haystack`

type Member needle haystack =
  ErrorIfStuck (MyUsefulTypeError needle haystack) (Lookup needle haystack)
```

and this appears to work, in the sense that it will give the right error message
for `wrong1`. But it has a dreadful implication --- it will completely suppress
real type errors. Consider `wrong2`:

```haskell
wrong2 :: Member (State (Maybe Int)) r => Effect row Bool
wrong2 = isJust $ get @(Maybe Int)  -- should be `fmap isJust get`
```

Unfortunately `wrong2` will also emit `MyUsefulTypeError` rather than producing
the usual `Could not match 'Maybe Int' with 'Effect row (Maybe Int)'` type
error! Not only this, but `MyUsefulTypeError` will also be emitted instead of
typed holes, completely destroying hole-driven development workflows!

**There is no workaround for this problem.** We are stuck choosing between
terrible type errors that leak implementation details, and nice type errors that
completely squelch GHC's usual errors.



## Proposed Change Specification

There are three separate pieces to the proposed change. The first two correspond
to the issues raised above, and the third is for quality of life improvements
since we're here anyway.

### Part 1) Don't Emit User-Written Type Errors

`TypeError`s will no longer be emitted when appearing in user-written types.
Instead, they will be emitted whenever they appear in *inferred* types.

#### Part 1.5) A note on backwards compatibility

The original use case for `TypeError` was as a genuine error case in a `type
family` match. For example:

```haskell
type family Pred (n :: Nat) :: Nat where
  Pred 0 = TypeError ...
  Pred n = n - 1
```

When a user writes `Pred 0` monomorphically directly in a type, we *probably
still want to emit it,* despite the proposed changes above. To that end, we will
also add two more rules for when to emit type errors:

1. Whenever a type family reduces to a `TypeError`
2. Whenever a type family scrutinizes a `TypeError`

Rule 1 gives us today's behavior for eagerly reporting `TypeError`s that result
as genuine errors in type family expansion.

I don't have a compelling argument for Rule 2, but it *feels right.*



### Part 2) Add Finer Grained Error Reporting

We will make the following changes to the `GHC.TypeLits` library:

```haskell
data ErrorPriority = HighPriority | LowPriority

type family
  TypeErrorPriority (priority :: ErrorPriority) (msg :: ErrorMessage) where

type TypeError msg = TypeErrorPriority 'HighPriority msg
```

The `HighPriority` and `LowPriority` constructors correspond to the existent
`report1` and `report2` error reports in GHC's reporting machinery.
`report1` contains error messages directly from the compiler, and will suppress
other errors it sees corresponding to the same types. Today, `TypeErrors` are
emitted in `report1`.

`report2` is all the extra stuff GHC will tell you about if there are no real
problems. `report2` is emitted iff `report1` was empty, meaning this is a good
place to put error messages such as `MyUsefulTypeError` above.

The names `HighPriority` and `LowPriority` are deliberately left ambiguous to
allow for finer grain control in the future.


## Part 3) New Constructors for ErrorMessage

The following constructors will be added to `ErrorMessage`:

```haskell
ShowTypePrec :: Nat -> k -> ErrorMessage
```

`ShowTypePrec n k` will show the type `k` with precedence `n`, corresponding to
Haskell's everyday precedence numbers. That is to say, `'ShowTypePrec 0 (Either
Int Bool) ~ 'Text "Either Int Bool"`, but `'ShowTypePrec 10 (Either Int Bool) ~
'Text "(Either Int Bool)"`. This is helpful when you want to produce an error
where part of the type is built in a `'Text`.


## Examples

All of the following examples will now compile as a result of **Part 1**:

```haskell
type MyError = TypeError ...

class Foo where
  default foo :: TypeError ... => ()
  foo :: ()

instance TypeError ... => Show (a -> b)

crazy :: TypeError ... => ()
crazy = ()

alsoCrazy :: TypeError ... => ()  -- for the same ... as 'crazy'
alsoCrazy = crazy
```

But, correspondingly, each of the following examples will emit a `TypeError`:

```haskell
-- A
instance Foo

-- A
badShow :: String
badShow = show $ id @(Int -> Int)

-- A
blah :: ()
blah = crazy

-- B
bad :: String -> TypeError ...
bad _ = ()
```

The examples annotated `A` each try to discharge a `TypeError` constraint, while
the `B` example attempts to unify `()` with `TypeError`.

---

For **Part 2**, defining `ErrorIfStuck` in terms of `TypeErrorPriority
'LowPriority` will now solve the error with `Member` marked via "**There is no
workaround to this problem.**"""

---

For **Part 3**, consider this real error message from [`polysemy`][err]:

[err]: https://github.com/polysemy-research/polysemy/blob/ac6d7b312114863987f103871df54b2a5d1fe7d8/src/Polysemy/Internal/CustomErrors.hs#L101-L107

```haskell
  TypeError
      ( 'Text "Could not deduce: (Member "
  ':<>: 'ShowType effect
  ':<>: 'Text " "
  ':<>: 'ShowType row
  ':<>: 'Text ") "
    )
```

It's crucial to put the right parentheses around `effect` and `row` in order for
`Member` to kind-check. See
[polysemy#74](https://github.com/polysemy-research/polysemy/issues/74) for an
example of the inscrutable errors you might get otherwise.

Now we could just always put in the parens, but GHC omits them if they're
unnecessary, and not having this feature means we're a second class citizen in
this regard. It can [sort of be worked around for monomorphic types][sorta]

[sorta]: https://github.com/polysemy-research/polysemy/blob/master/src/Polysemy/Internal/CustomErrors.hs#L40-L62

Instead, we can just write:

```haskell
  TypeError
      ( 'Text "Could not deduce: (Member "
  ':<>: 'ShowTypePrec 10 effect
  ':<>: 'Text " "
  ':<>: 'ShowTypePrec 10 row
  ':<>: 'Text ") "
    )
```

and GHC will do the right thing for us.



## Effect and Interactions

Besides the potential backwards compatibility issues described in **Part 1.5**,
I can think of no interactions.


## Costs and Drawbacks

Maintenance costs should be ridiculously small; custom type errors are hidden
off in their own little part of the compiler. The *emitting upon type family
expansion* stuff might have a maintenance burden --- I'm not particularly familiar
with that part of GHC.


## Alternatives

We could just do nothing, but as the amount of links above shows, there is a lot
of frustration with the current implementation of `TypeError`s.


## Unresolved Questions

1. Do we really need to worry about the backwards compatibility changes in
   **Part 1.5**? Things would be drastically simpler without them.
2. Are `HighPriority` and `LowPriority` good enough names? Should we provide
   better semantics here? Possibly use `Nat`s instead?


## Implementation Plan

I (Sandy Maguire) have [already][mr1730] [implemented][mr1739] most of this.

[mr1730]: https://gitlab.haskell.org/ghc/ghc/merge_requests/1730
[mr1739]: https://gitlab.haskell.org/ghc/ghc/merge_requests/1739

