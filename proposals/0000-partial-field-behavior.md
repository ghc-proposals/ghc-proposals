---
author: Matt Parsons
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/535).

# Partial Field Behavior

Haskell allows you to define records:

```haskell
data Rec = Rec { a :: Int, b :: Char }
```

Haskell allows you to define sum types:

```haskell
data Maybe a = Nothing | Just a
```

Haskell *also* allows you to define sum types, where the constructors are records:

```haskell
data OptionRec 
    = None
    | Some { fromSome :: a }
```

The `fromSome` field is *partial* - it does not exist on every constructor.
As a result, some of the uses this field may fail at runtime if the constructor isn't right.

This proposal introduces a few extensions which can control how GHC handles partial fields. 


## Motivation

The desire for partial fields comes up all the time.
[This StackOverflow question](https://stackoverflow.com/questions/37652243/record-syntax-and-sum-types) makes the request several years ago.
[This issue comment](https://github.com/input-output-hk/plutus/commit/cce0f0999f138fe7bf1937080087452bb4d61b8b#r81608004) links to a PR with a datatype that carries field labels to describe components of an error.
A convenient shape for JSON API types is a sum-type of records.
The [`optparse-generic`](https://hackage.haskell.org/package/optparse-generic-1.4.8) library generates sub-command parsers with named arguments using partial fields.

Currently, these are all *bad*, because they generate partial functions.
Even `NoFieldSelectors` is small help, since `OverloadedRecordDot` allows you to project from them in a way that isn't safe.

Rational people can disagree on the *best* way to handle this.
Rational people can also agree that "throwing a runtime error with very little diagnostic information" is *not* a particularly nice solution. 

The current solution is to *not do this*, and instead use a specific record type instead of a record.

```haskell
-- Bad:
data Message 
    = SendChat { message :: Text, user :: UserId }
    | Signup { eventId :: EventId, user :: UserId }

-- Work-around:
data Message 
    = SendChat SendChatContents
    | Signup SignupContents

data SendChatContents = SendChatContents
    { message :: Text
    , user :: UserId
    }

data Signup = SignupContents
    { eventId :: EventId
    , user :: UserId
    }
```

However, this has a few notable disadvantages:

1. It's three declarations instead of one - boilerplate
2. You need to write two constructors at every use (pattern matching or construction)
3. An extra pointer indirection (though bang patterns and `UNPACK` pragmas can usually alleviate this)
4. Tools that reflect on the shape of data will treat them differently (ie JSON parsing and subcommand parsing)


### Other Languages

How do other languages handle this?

[TypeScript](https://www.typescriptlang.org/docs/handbook/unions-and-intersections.html) provides a compiler error if you use a partial field on a union type.
However, if you refine the type using a `switch` statement, then you can use the fields, and this is guaranteed to be safe.
Additionally, the compiler does not complain if you use a field that is present on every type.

[Rust](https://doc.rust-lang.org/reference/items/enumerations.html) simply does not allow you to use dot notation on an enum with record fields, even if the field is shared among all types.
To access named fields on an enum, you must use pattern matching.

[F#](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/discriminated-unions) also does not treat named fields on a union as accessors, and only uses them for construction and pattern matching.

[OCaml](https://stackoverflow.com/questions/22367396/does-ocaml-have-record-syntax-in-disjoint-unions) does not allow you to define named fields in a union at all.

[PureScript](https://jordanmartinez.github.io/purescript-jordans-reference-site/content/21-Hello-World/01-Prelude-ish/01-Basic-FP-Data-Types/01-Sum-and-Product-Types.html)'s sums-with-records are different - they contain a single value that is an anonymous record, instead of the constructor carrying a number of fields.

Every language that has field labels in sum types allows you to use them in record construction and pattern matching.
Contrasting these languages, then, we have this table:

| Language   | Sum Labels?  | Field Access? | Runtime errors? |
|------------|--------------|---------------|-----------------|
| Haskell    |     Y        |       Y       |      Y          |
| Rust       |     Y        |               |                 |      
| F#         |     Y        |               |                 |
| OCaml      |              |               |                 |
| TypeScript |     Y        |       Y       |                 |
| PureScript |              |               |                 |

I don't know about you, but I don't like being the only language in the table with runtime errors about this.


## Proposed Change Specification

I propose we introduce a few new language extensions that allow us to customize the behavior of partial field selectors.
The following are *options*, and are not intended to be a complete set that must remain unbroken.

### `NoPartialFieldSelectors` + `PartialFieldSelectors`

`PartialFieldSelectors` is the current behavior of GHC.
So let's talk about `NoPartialFieldSelectors`:

#### No selector function

Under `NoPartialFieldSelectors`, we wouldn't get a partial function.

```haskell
data OptionRec a = None | Some { fromSome :: a }

main = 
    print (fromSome None)
```

The above could should give an error stating that `fromSome` is a partial field, and no selector function is generated due to `NoPartialFieldSelectors`.

#### No record update

Under `NoPartialFieldSelectors`, we wouldn't be able to use record update syntax with the label.

```haskell
main = do
    let some = None
    print (some { fromSome = 2 })
```

This program would give an error stating that `fromSome` is a partial field, and cannot be used in record update.

#### No `HasField` instance

Under `NoPartialFieldSelectors`, we wouldn't be able to use `getField` or `OverloadedRecordDot`:

```haskell
main = do
    let some = None
    print (getField @"fromSome" some)
```

This should give an error stating that `fromSome` is a partial field, and cannot be used for field access.
This can take the form of a `TypeError` instance for `HasField`:

```haskell
instance 
    TypeError 
        ( Text "The field 'fromSome' is partial, and was not generated because of 
        :<>: Text "NoPartialFieldSelectors."
        )
  =>
    HasField "fromSome" (OptionRec a) a
  where
    getField _ = error "impossible"
```

#### Record Construction + Pattern Matching

Record  construction continues to work as usual, since these are completely safe already.

```haskell
main = do
    run (Some { fromSome = 2 })

run (Some { fromSome = val }) = 
    print val

run (Some { fromSome }) = 
    print fromSome

run (Some { .. }) = 
    print fromSome
```


### `MaybeFieldSelectors`

This extension would change the behavior of partial fields such that *accessor function* and `HasField` instance return a `Maybe`.

#### Maybe Selector Function

```haskell
data Message 
    = SendChat { message :: Text, user :: UserId }
    | Signup { eventId :: EventId, user :: UserId }

>>> :t user
user :: Message -> UserId
>>> :t eventId
eventId :: Message -> Maybe EventId
>>> :t message
message :: Message -> Maybe Text
```

Since `user :: UserId` is shared among both constructors, it can't fail, and we can return the `UserId`.
But since `message` and `eventId` are not shared among all constructors, they are partial.
As a result, their selector function is `Maybe typ` instead of `typ`.

#### Maybe `HasField` instance

```haskell
main = do
    let msg = SendChat { message = "hello", user = 3 }

    case msg.message of
        Just txt ->
            print txt
        Nothing ->
            pure ()

    print msg.user
```

Instead of generating the instance of HasField with the `typ`, we provide `Maybe typ`:

```haskell
instance HasField "user" Message UserId where
    getField = \case
        SendChat { user } -> user
        Signup { user } -> user

instance HasField "eventId" Message (Maybe EventId) where
    getField = \case
        SendChat {} -> Nothing
        Signup { eventId } -> Just eventId
```


#### Record Update

This extension would not change the behavior of partial record fields used in an update.

The status quo is to throw an exception:

```haskell
>>> data T = A { x :: Int } | B { y :: Int }
>>> deriving instance Show T
>>> (A 3) { y = 2 }
*** Exception: Non-exhaustive patterns in record update
```

With `-Wincomplete-record-updates` (part of `-Wall`), this issues a warning:

```haskell
λ> (A 3) { y = 2 }

interactive:10:1: warning: [-Wincomplete-record-updates]
    Pattern match(es) are non-exhaustive
    In a record-update construct: Patterns of type ‘T’ not matched: A 3
*** Exception: interactive:10:1-15: Non-exhaustive patterns in record update
```

Users interested in safety can set `-Werror=incomplete-record-updates` while still being able to use partial fields safely in other contexts.

#### Record Construction + Pattern Matching

There's no reason to change record construction or pattern matching, since these are safe.
Field labels would retain their original type.

### Per Type and Per Field

The [proposal to enable `NoFieldSelectors` on a per-type and per-field basis](https://github.com/ghc-proposals/ghc-proposals/pull/512), while not currently accepted, seems to be positively received by the community.
In the spirit of anticipating obvious extensions, these extensions should *also* be applicable on a per-datatype and per-field basis, instead of only on the per-module basis.

## Examples

The above sections have numerous examples, so I won't be repeating them here.

## Effect and Interactions

This proposal can introduce potentially tricky behavior with the partial field warnings currently in place.
Were this proposal implemented, [this PR about warning on partial field uses](https://github.com/ghc-proposals/ghc-proposals/pull/516) would need to take it into account.
I suspect that a field can simply be marked `partial`, and if it is partial, then it is warned on (or turned into a `Maybe`, etc).

A partial field generated with the `MaybeFieldSelectors` would silence the warning provided by `-Wpartial-fields` on the definition site:

```
λ> data T = A { x :: Int } | B { y :: Int } deriving stock Show

<interactive>:19:14: warning: [-Wpartial-fields]
    Use of partial record field selector: ‘x’

<interactive>:19:31: warning: [-Wpartial-fields]
    Use of partial record field selector: ‘y’
```

With #516's `-Wincomplete-field-selectors`, a warning will occur whenever a *partial* field is used.
This proposal intends to turn partial fields into "safe" fields using `Maybe`.
A datatype or field that is marked as a `MaybeFieldSelector` would not trigger a warning for solving `HasField sym rec (Maybe typ)` nor for using the field as an accessor function.

## Costs and Drawbacks

- I'm unfamiliar with how difficult this might be to implement.
- I'm sure there are others!

## Alternatives

The current workaround is to define specific types for the contents of a sum:

```haskell
-- Bad:
data Message 
    = SendChat { message :: Text, user :: UserId }
    | Signup { eventId :: EventId, user :: UserId }

-- Work-around:
data Message 
    = SendChat SendChatContents
    | Signup SignupContents

data SendChatContents = SendChatContents
    { message :: Text
    , user :: UserId
    }

data Signup = SignupContents
    { eventId :: EventId
    , user :: UserId
    }
```

Programmers must be aware of the problems with the `Bad` approach and be taught the `Good` approach.
Furthermore, they must exercise the discipline to actually do this.

Another alternative is to use [`-Werror=incomplete-record-selectors`](https://github.com/ghc-proposals/ghc-proposals/pull/516) when that is implemented.
That will warn on unsafe use of partial field accessors, but won't warn on pattern match or construction.
However, that simply *warns* about the bad behavior, while this proposal allows for good behavior.

## Unresolved Questions

## Implementation Plan

(Optional) If accepted who will implement the change? Which other resources
and prerequisites are required for implementation?

## Endorsements

(Optional) This section provides an opportunity for any third parties to express their
support for the proposal, and to say why they would like to see it adopted.
It is not mandatory for have any endorsements at all, but the more substantial
the proposal is, the more desirable it is to offer evidence that there is
significant demand from the community.  This section is one way to provide
such evidence.
