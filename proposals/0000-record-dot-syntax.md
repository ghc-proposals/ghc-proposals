---
author: Neil Mitchell and Shayne Fletcher
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/282).

# Record Dot Syntax

Records in Haskell are [widely recognised](https://www.yesodweb.com/blog/2011/09/limitations-of-haskell) as being under-powered, with duplicate field names being particularly troublesome. We propose a new language extension `RecordDotSyntax` that provides syntactic sugar to make the features introduced in [the `HasField` proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst) more accessible, improving the user experience.

## 1. Motivation

In almost every programming language we write `a.b` to mean the `b` field of the `a` record expression. In Haskell that becomes `b a`, and even then, only works if there is only one `b` in scope. Haskell programmers have struggled with this weakness, variously putting each record in a separate module and using qualified imports, or prefixing record fields with the type name. We propose bringing `a.b` to Haskell, which works regardless of how many `b` fields are in scope. Here's a simple example of what is on offer:

```haskell
{-# LANGUAGE RecordDotSyntax #-}

data Company = Company {name :: String, owner :: Person}
data Person = Person {name :: String, age :: Int}

display :: Company -> String
display c = c.name ++ " is run by " ++ c.owner.name

nameAfterOwner :: Company -> Company
nameAfterOwner c = c{name = c.owner.name ++ "'s Company"}
```

We declare two records both having `name` as a field label. The user may then write `c.name` and `c.owner.name` to access those fields. We can also write `c{name = x}` as a record update, which works even though `name` is no longer unique. Under the hood, we make use of `getField` and `setField` from [the `HasField` proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst).

An implementation of this proposal has been battle tested and hardened over two years in the enterprise environment as part of [Digital Asset](https://digitalasset.com/)'s [DAML](https://daml.com/) smart contract language (a Haskell derivative utilizing GHC in its implementation), and also in a [Haskell preprocessor and a GHC plugin](https://github.com/ndmitchell/record-dot-preprocessor/). When initially considering Haskell as a basis for DAML, the inadequacy of records was considered the most severe problem, and without devising the scheme presented here, wouldn't be using Haskell. The feature enjoys universal popularity with users.

## 2. Proposed Change Specification
For the specification we focus on the changes to the parsing rules, and the desugaring, with the belief the type checking and renamer changes required are an unambiguous consequences of those.

### 2.1 `RecordDotSyntax` language extension
This change adds a new language extension `RecordDotSyntax`.

#### 2.1.1 Syntax
In the event the language extension is enabled:

| Expression            | Equivalent                       |
| --                    | --                               |
| `(.lbl)`              | `(\e -> e.lbl)`                  |
| `(.lbl₁.lbl₂)`        | `(\e -> e.lbl₁.lbl₂)`            |
| `e.lbl`               | `getField @"lbl" e`              |
| `e.lbl₁.lbl₂`         | `(e.lbl₁).lbl₂`                  |
| `e{lbl = val}`        | `setField @"lbl" e val`          |
| `e{lbl₁.lbl₂ = val}`  | `e{lbl₁ = (e.lbl₁){lbl₂ = val}}` |
| `e.lbl₁{lbl₂ = val}`  | `(e.lbl₁){lbl₂ = val}`           |
| `e{lbl₁ = val₁}.val₂` | `(e{lbl₁ = val₁}).val₂`          |

*[Note: `e{lbl = val}` is the syntax of a standard H98 record update. It's the nested form introduced by this proposal that is new : `e{lbl1.lbl2 = val}`. However, in the event `RecordDotSyntax` is in effect, note that we propose that `e{lbl = val}` desugar to `setField @"lbl" e val`]*.

#### 2.1.2 Precedence
We propose that '`.`' "bind more tightly" than function application thus, `f r.a.b` parses as `f (r.a.b)`.

| Expression   | Interpretation    |
| --           | --                |
| `f r.x`      | means `f (r.x)`   |
| `f M.n.x`    | means `f (M.n.x)` |
| `f M.N.x`    | means `f (M.N.x)` |
| `f r .x`     | is illegal        |
| `f (g r).x`  | `f ((g r).x)`     |
| `f (g r) .x` | is illegal        |

#### 2.1.3 Fields whose names are operator symbols
We propose that dot notation isn't available for fields whose names are operator symbols (for example, `+`, `.+.` and so on).

*[Note : For fields whose names are operator symbols, one can still write `getField` expressions (e.g. `getField @".+." r`)]*.

### 2.2 Definitions
For what follows, we use these informal definitions:
* A **field selector** is an expression like `.a` or `.a.b`;
* A **field selection** is an expression like `r.a` or `(f x).a.b`;
* A **field update** is an expression like `r{a = 12}` or `r{a.b = "foo"}`;
* A **punned field update** is an expression like `r{a}` or `r{a.b}` (here it is understood that `b` is a variable bound in the environment of the expression and only valid syntax if the `NamedFieldPuns` language extension is in effect).

### 2.3 Lexing and Parsing

The prototype implements the parsing scheme presented here. More information about the prototype is available in [this section](#91-prototype).

#### 2.3.1 Lexer
A new token case `ITproj Bool` is introduced. When the extension is enabled occurences of operator `.` are classified using the whitespace sensitive operator mechanism from [this (accepted) GHC proposal](https://github.com/ghc-proposals/ghc-proposals/pull/229). The rules are:

| Occurence   | Token          | Means                | Example |
| --          | --             | --                   | --      |
| prefix      | `ITproj True`  | field selector       | `.x`    |
| tight infix | `ITproj False` | field selection      | `r.x`   |
| suffix      | `ITdot`        | function composition | `f. g`  |
| loose infix | `ITdot`        | function composition | `f . g` |

No `ITproj` tokens will ever be issued if `RecordDotSyntax` is not enabled.

#### 2.3.2 Parsing
The Haskell grammar is extended with the following productions. We use these notations:

| Symbol | Occurence   |
|--------|-------------|
| *.ᴾ*   | prefix      |
| *.ᵀ*   | tight-infix |

###### 2.3.2.1
[Field]
<br/>
&nbsp;&nbsp;&nbsp;&nbsp; *field* &nbsp;&nbsp;→&nbsp;&nbsp; *varid*  &nbsp;&nbsp;|&nbsp;&nbsp; *qvarid*
<br/>

###### 2.3.2.2
[Field to update]
<br/>
&nbsp;&nbsp;&nbsp;&nbsp; *fieldToUpdate* &nbsp;&nbsp;→&nbsp;&nbsp; *fieldToUpdate* *.ᵀ* *field* &nbsp;&nbsp;|&nbsp;&nbsp; *field*
<br/>

###### 2.3.2.3
[Field selectors]
<br/>
&nbsp;&nbsp;&nbsp;&nbsp; *aexp* &nbsp;&nbsp;→&nbsp;&nbsp; *( projection )*
<br/>
&nbsp;&nbsp;&nbsp;&nbsp; *projection* &nbsp;&nbsp;→&nbsp;&nbsp; *.ᴾ* *field* &nbsp;&nbsp;|&nbsp;&nbsp; *projection* *.ᵀ* *field*
<br/>

###### 2.3.2.4
[Field selection]
<br/>
&nbsp;&nbsp;&nbsp;&nbsp; *fexp* &nbsp;&nbsp;→&nbsp;&nbsp; *fexp* *.ᵀ* *field*
<br/>

###### 2.3.2.5
[Field update]
<br/>
&nbsp;&nbsp;&nbsp;&nbsp; *fbind* &nbsp;&nbsp;→&nbsp;&nbsp;  *field* *.ᵀ* *fieldToUpdate* *=* *exp*
<br/>
&nbsp;&nbsp;&nbsp;&nbsp; *fbind* &nbsp;&nbsp;→&nbsp;&nbsp; *field* *.ᵀ* *fieldToUpdate*
<br/>

## 3. Examples

This is a record type with functions describing a study `Class` (*Oh! Pascal, 2nd ed. Cooper & Clancy, 1985*).

```haskell
data Grade = A | B | C | D | E | F
data Quarter = Fall | Winter | Spring
data Status = Passed | Failed | Incomplete | Withdrawn

data Taken =
  Taken { year :: Int
        , term :: Quarter
        }

data Class =
  Class { hours :: Int
        , units :: Int
        , grade :: Grade
        , result :: Status
        , taken :: Taken
        }

getResult :: Class -> Status
getResult c = c.result -- get

setResult :: Class -> Status -> Class
setResult c r = c{result = r} -- update

setYearTaken :: Class -> Int -> Class
setYearTaken c y = c{taken.year = y} -- nested update

getResults :: [Class] -> [Status]
getResults = map (.result) -- selector

getTerms :: [Class]  -> [Quarter]
getTerms = map (.taken.term) -- nested selector
```

Further examples [accompany the prototype](https://gitlab.haskell.org/shayne-fletcher-da/ghc/-/blob/f74bb04d850c53e4b35eeba53052dd4b407fd60b/record-dot-syntax-tests/Test.hs) and yet more (as tests) are available in the examples directory of [this repository](https://github.com/ndmitchell/record-dot-preprocessor). Those tests include infix applications, polymorphic data types, interoperation with other extensions and more.

## 4. Effect and Interactions

**Polymorphic updates:** When enabled, this extension takes the `a{b=c}` syntax and uses it to mean `setField`. The biggest difference a user is likely to experience is that the resulting type of `a{b=c}` is the same as the type `a` - you _cannot_ change the type of the record by updating its fields. The removal of polymorphism is considered essential to preserve decent type inference, and is the only option supported by [the `HasField` proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst). Anyone wishing to use polymorphic updates can write `let Foo{..} = a in Foo{polyField=[], ..}` instead.

**Higher-rank fields:** It is impossible to express `HasField` instances for data types such as `data T = MkT { foo :: forall a . a -> a}`, which means they can't have this syntax available. Users can still write their own selector functions using record puns if required.  There is a possibility that with future types of impredicativity such `getField` expressions could be solved specially by the compiler.

**Lenses and a.b syntax:** The `a.b` syntax is commonly used in conjunction with the `lens` library, e.g. `expr^.field1.field2`. Treating `a.b` without spaces as a record projection would break such code. The alternatives would be to use a library with a different lens composition operator (e.g. `optics`), introduce an alias in `lens` for `.` (perhaps `%`), write such expressions with spaces, or not enable this extension when also using lenses. While unfortunate, we consider that people who are heavy users of lens don't feel the problems of inadequate records as strongly, so the problems are lessened. In addition, it has been discussed (e.g. [here](https://github.com/ghc-proposals/ghc-proposals/pull/282#issuecomment-546159561)), that this proposal is  complimentary to lens and can actually benefit lens users (as with `NoFieldSelectors` one can use the same field names for everything: dot notation, lens-y getting, lens-y modification, record updates, `Show/Generic`).

**Rebindable syntax:** When `RebindableSyntax` is enabled the `getField` and `setField` functions are those in scope, rather than those in `GHC.Records`. The `.` function (as used in the `a.b.c` desugaring) remains the `Prelude` version (we see the `.` as a syntactic shortcut for an explicit lambda, and believe that whether the implementation uses literal `.` or a lambda is an internal detail).

**Enabled extensions:** The `RecordDotSyntax` extension does not imply enabling/disabling any other extensions. It is often likely to be used in conjunction with either the `NoFieldSelectors` extension or`DuplicateRecordFields`.

## 5. Costs and Drawbacks

The implementation of this proposal adds code to the compiler, but not a huge amount. Our [prototype](#91-prototype) shows the essence of the parsing changes, which is the most complex part.

If this proposal becomes widely used then it is likely that all Haskell users would have to learn that `a.b` is a record field selection. Fortunately, given how popular this syntax is elsewhere, that is unlikely to surprise new users.

This proposal advocates a different style of writing Haskell records, which is distinct from the existing style. As such, it may lead to the bifurcation of Haskell styles, with some people preferring the lens approach, and some people preferring the syntax presented here. That is no doubt unfortunate, but hard to avoid - `a.b` really is ubiquitous in programming languages. We consider that any solution to the records problem _must_ cause some level of divergence, but note that this mechanism (as distinct from some proposals) localises that divergence in the implementation of a module - users of the module will not know whether its internals used this extension or not.

The use of `a.b` with no spaces on either side can make it harder to write expressions that span multiple lines. To split over two lines it is possible to use the `&` function from `Base` or do either of:
```
(myexpression.field1.field2.field3
    ).field4.field5

let temp = myexpression.field1.field2.field3
in temp.field4.field5
```

## 6. Alternatives to this proposal

Instead of this proposal, we could do any of the following:

* Using the [`lens` library](https://hackage.haskell.org/package/lens). While lenses help both with accessors and overloaded names (e.g. `makeFields`), one still needs to use one of the techniques mentioned below (or similar) to work around the problem of duplicate name selectors. In addition, lens-based syntax is more verbose, e.g. `f $ record ^. field` instead of possible `f record.field`.
More importantly, while the concept of lenses is very powerful, that power can be [complex to use](https://twitter.com/fylwind/status/549342595940237312?lang=en), and for many projects that complexity is undesirable. In many ways lenses let you abstract over record fields, but Haskell has neglected the "unabstracted" case of concrete fields. Moreover, as it has been [previously mentioned](#Effect-and-Interactions), this proposal is orthogonal to lens and can actually benefit lens users.
* The [`DuplicateRecordFields` extension](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#duplicate-record-fields) is designed to solve similar problems. We evaluated this extension as the basis for DAML, but found it lacking. The rules about what types must be inferred by what point are cumbersome and tricky to work with, requiring a clear understanding of at what stage a type is inferred by the compiler.
* Some style guidelines mandate that each record should be in a separate module. That works, but then requires qualified modules to access fields - e.g. `Person.name (Company.owner c)`. Forcing the structure of the module system to follow the records also makes circular dependencies vastly more likely, leading to complications such as boot files that are ideally avoided.
* Some style guidelines suggest prefixing each record field with the type name, e.g. `personName (companyOwner c)`. While it works, it isn't pleasant, and many libraries then abbreviate the types to lead to code such as `prsnName (coOwner c)`, which can increase confusion.
* There is a [GHC plugin and preprocessor](https://github.com/ndmitchell/record-dot-preprocessor) that both implement much of this proposal. While both have seen light use, their ergonomics are not ideal. The preprocessor struggles to give good location information given the necessary expansion of substrings. The plugin cannot support the full proposal and leads to error messages mentioning `getField`. Suggesting either a preprocessor or plugin to beginners is not an adequate answer. One of the huge benefits to the `a.b` style in other languages is support for completion in IDE's, which is quite hard to give for something not actually in the language.
* Continue to [vent](https://www.reddit.com/r/haskell/comments/vdg55/haskells_record_system_is_a_cruel_joke/) [about](https://bitcheese.net/haskell-sucks) [records](https://medium.com/@snoyjerk/least-favorite-thing-about-haskal-ef8f80f30733) [on](https://www.quora.com/What-are-the-worst-parts-about-using-Haskell) [social](http://www.stephendiehl.com/posts/production.html) [media](https://www.drmaciver.com/2008/02/tell-us-why-your-language-sucks/).

All these approaches are currently used, and represent the "status quo", where Haskell records are considered not fit for purpose.

## 7. Alternatives within this proposal

### 7.1 Should `RecordDotSyntax` imply `NoFieldSelectors` or another extension?

Typically `RecordDotSyntax` will be used in conjunction with `NoFieldSelectors`, but `DuplicateRecordFields` would work too. Of those two, `DuplicateRecordFields` complicates GHC, while `NoFieldSelectors` conceptually simplifies it, so we prefer to bias the eventual outcome. However, there are lots of balls in the air, and enabling `RecordDotSyntax` should ideally not break normal code, so we leave everything distinct (after [being convinced](https://github.com/ghc-proposals/ghc-proposals/pull/282#issuecomment-547641588)).

### 7.2 Should a syntax be provided for modification?

Earlier versions of this proposal contained a modify field sytnax of the form `a{field * 2}`. While appealing, there is a lot of syntactic debate, with variously `a{field <- (*2)}`, `a{field * = 2}` and others being proposed. None of these syntax variations are immediately clear to someone not familiar with this proposal. To be conservative, we leave this feature out.

### 7.3 Should there be update sections?

There are no update sections. Should `({a=})`, `({a=b})` or `(.lbl=)` be an update section? While nice, we leave this feature out.

### 7.4 Should pattern matching be extended?

We do not extend pattern matching, although it would be possible for `P{foo.bar=Just x}` to be defined.

### 7.5 Will whitespace sensitivity become worse?

We're not aware of qualified modules giving any problems, but it's adding whitespace sensitivity in one more place.

### 7.6 Should a new update syntax be added?

One suggestion is that record updates remain as normal, but `a { .foo = 1 }` be used to indicate the new forms of updates. While possible, we believe that option leads to a confusing result, with two forms of update both of which fail in different corner cases. Instead, we recommend use of `C{foo}` as a pattern (with `-XNamedFieldPuns`) to extract fields if necessary.

## 8. Unresolved issues

None.

## 9. Implementation Plan

### 9.1 Prototype

To gain confidence these changes integrate as expected [a prototype](https://gitlab.haskell.org/shayne-fletcher-da/ghc/-/tree/record-dot-syntax-4.1) was produced that parses and desugars forms directly in the parser. For confirmation, we _do not_ view desugaring in the parser as the correct implementation choice, but it provides a simple mechanism to pin down the changes without going as far as adding additional AST nodes or type checker rules. The prototype is sufficiently rich enough to "do the right thing" with [this test file](https://gitlab.haskell.org/shayne-fletcher-da/ghc/-/blob/f74bb04d850c53e4b35eeba53052dd4b407fd60b/record-dot-syntax-tests/Test.hs).

### 9.2 Who will provide an implementation?

If accepted, the proposal authors would be delighted to provide an implementation. Implementation depends on the implementation of [the `HasField` proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst) and [the `NoFieldSelectors` proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0160-no-toplevel-field-selectors.rst).
