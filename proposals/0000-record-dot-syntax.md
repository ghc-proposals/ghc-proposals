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

An implementation of this proposal has been battle tested and hardened over 18 months in the enterprise environment as part of [Digital Asset](https://digitalasset.com/)'s [DAML](https://daml.com/) smart contract language (a Haskell derivative utilizing GHC in its implementation), and also in a [Haskell preprocessor and a GHC plugin](https://github.com/ndmitchell/record-dot-preprocessor/). When initially considering Haskell as a basis for DAML, the inadequacy of records was considered the most severe problem, and without devising the scheme presented here, we wouldn't be using Haskell. The feature enjoys universal popularity with users.

## 2. Proposed Change Specification

For the specification we focus on the changes to the parsing rules, and the desugaring, with the belief the type checking and renamer changes required are an unambiguous consequences of those.

### 2.1 `RecordDotSyntax` language extension
This change adds a new language extension `RecordDotSyntax`.

#### 2.1.1 Syntax
In the event the language extension is enabled:

| Expression | Equivalent |
| -- | -- |
| `.lbl` | `(\x -> x.lbl)` the `.` cannot have whitespace after |
| `.lbl1.lbl2` | `(\x -> x.lbl1.lbl2)` |
| `e.lbl` | `getField @"lbl" e` the `.` cannot have whitespace either before or after |
| `e.lbl1.lbl2` | `(e.lbl1).lbl2` |
| `e{lbl = val}` | `setField @"lbl" e val` |
| `e{lbl1.lbl2 = val}` | `e{lbl1 = (e.lbl1){lbl2 = val}}` performing a nested update |
| `e.lbl1{lbl2 = val}` | `(e.lbl1){lbl2 = val}` |
| `e{lbl1 = val}.lbl2` | `(e{lbl1 = val}).lbl2` |
| `e{lbl1 = val1, lbl2 = val2}` | `(e{lbl1 = val1}){lbl2 = val2}` |

*[Note: `e{lbl=val}` is the syntax of a standard H98 record update. It's the nested form introduced by this proposal that is new : `e{lbl1.lbl2 = val}`. However, in the event `RecordDotSyntax` is in effect, we propose that `e{lbl = val}` desugar to `setField @"lbl" e a`]*.

#### 2.1.2 Precedence

Regarding precedence, we propose that '`.`' should "bind more tightly" than function application thus, `f r.a.b` should parse as `f (r.a.b)`.

### 2.2 Definitions

For clarity of terminology in what follows, we make the following informal definitions:
* A **field selector** is an expression like `.a` or `.a.b` preceded by white-space;
* A **field selection** is an expression like `r.a` or `(f x).a.b`, where the first dot is preceded by a close paren or a varid;
* A **field update** is an expression like `r{a = 12}` or `r{a.b = "foo"}`;
* A **punned field update** is an expression like `r{a}` or `r{a.b}` (where it is understood that `b` is a variable bound in the environment of the expression and only valid syntax if the `NamedFieldPuns` language extension is in effect).

### 2.3 Lexing and Parsing

The intent of this section is **not** to recommend a particular parsing implementation. Rather, we aim only to show that lexing and parsing is feasible. We are open to learning of better strategies for the implementation of the lexing and parsing but in their absence, the scheme presented here appears to have the required properties.

#### 2.3.1 Lexer

A new lexeme *fieldid* is introduced.
<br/>
<br/>*lexeme* → *qvarid* | *qconid* | *qvarsym* | *qconsym*
| *literal* | *special* | *reservedop* | *reservedid* | *fieldid*
<br/>*fieldid* → *.varid{.varid}*

In terms of changes to GHC's `Lexer.x` we write the following.

```haskell
-- Regular expressions
@fieldid = (\. @varid)+
...
<0,option_prags> {
  ...
  @fieldid / {ifExtension RecordDotSyntaxBit} { idtoken fieldid }
}
...

-- Token type
data Token
  = ITas
  | ...
  | ITfieldid [FastString]
  ...

-- Lexer actions
fieldid :: StringBuffer -> Int -> Token
fieldid buf len = ITfieldid $! splitFields buf len

-- Split a buffer with contents like '.foo.bar.baz' into components.
splitFields :: StringBuffer -> Int -> [FastString]
splitFields buf len = ...
```

Note that tokens of case `ITfieldid`  will never be issued if `RecordDotSyntax` is not enabled.

In terms of changes to GHC's `Parser.y`, the new token is incorporated into the parser like so.
```haskell
%token
  ...
  FIELDID { L _ (ITfieldid _) }
  ...

{
...
getFIELDID      (dL->L _ (ITfieldid  x)) = x
...
}
```

#### 2.3.2 Parsing

##### 2.3.2.1 Parsing of field selectors

Supporting field selectors is achieved by extending the set of `aexp` productions.
<br/>
<br/>*aexp* → *fieldid*

```haskell
aexp    :: { ECP }
       ...
       | FIELDID {
           ...
         } -- <- here

       ...
```

##### 2.3.2.2 Parsing of field selections

Supporting field selections does not require any new productions. The production *fexp -> fexp aexp* is sufficient, only, its semantic action needs to be updated to take `RecordDotSyntax` into account.
```haskell
fexp    :: { ECP }
        : fexp aexp
          {%do
             {
               ; recordDotSyntax <- getBit RecordDotSyntaxBit
               ; if not recordDotSyntax
                   then
                     ... do as we do today
                   else do {
                     ; lhs <- runECP_P $1 :: P (Located (HsExpr GhcPs))
                     ; rhs <- runECP_P $2 :: P (Located (HsExpr GhcPs))
                     ; if not (isFieldSelector rhs)
                        then
                         .... do as we do today
                        else
                           if (adjacent lhs rhs)
                             then
                               ... handle field selection (e.g. 'a.foo.bar')
                             else
                               ... handle an application on a field selector (e.g. 'f .foo.bar')
                    }
         ...
```
*[No doubt a real implementation can express this logic more elegantly - we present it in this way here to elucidate.]*

The key point to note is the disambiguation of a field selection from the application of a term to a field selector. That is, looking at white-space to distinguish between `f.x` and `f .x`. This is handled by the function `adjacent` which can be defined simply as:
```haskell
adjacent :: Located a -> Located b -> Bool
adjacent (L a _) (L b _) = isGoodSrcSpan a && srcSpanEnd a == srcSpanStart b
```

*[One thing to look out for in the implementation at this point is to carefully respect the precedence rule i.e. `f a.b` parse as `f (a.b)`.]*

##### 2.3.2.3 Parsing of field updates

Field updates and punned field updates are achieved by generalizing the `aexp` productions.
<br/>
<br> *aexp* → *aexp⟨qcon⟩* { *pbind* , … , *pbind* }
<br/>*pbind* -> *qvar*=*exp* | *var* *aexp*=*exp*

The existing rule is
```haskell
aexp1   :: { ECP }
        : aexp1 '{' fbinds '}' { ...}
```
It's easy enough to extend `aexp1` to handle simple cases of nested field updates and punned field updates like so:
```haskell
aexp1   :: { ECP }
        : aexp1 '{' fbinds '}' { ... as we do today... }
        | aexp1 '{' VARID FIELDID '=' texp '}' { ... } <- nested field update here
        | aexp1 '{' VARID FIELDID '}' { ... } <- punned field update here here
       ...
```
*[As written, this of course means that `r{a = ...}` doesn't result in a `setField` expression whereas `r{a.b = ...}` does. Further, `r{a.b = ..., c = ...}` (multiple updates) aren't handled. We are not endorsing either of those things, rather we are just demonstrating that implementation of this proposal will be achieved by careful generalization of `fbinds`.]*

The prototype implements the parsing scheme present here. More information about the prototype is available in [this section](#91-prototype).

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

addYears :: Class -> Int -> Class
addYears c n = c{taken.year = c.taken.year + n} -- update via op

squareUnits :: Class -> Class
squareUnits c = c{units = (\x -> x * x) c.units} -- update via function

getResults :: [Class] -> [Status]
getResults = map .result -- selector

getTerms :: [Class]  -> [Quarter]
getTerms = map .taken.term -- nested selector
```

A full, rigorous set of examples (as tests) are available in the examples directory of [this repository](https://github.com/ndmitchell/record-dot-preprocessor). Those tests include infix applications, polymorphic data types, interoperation with other extensions and more.

## 4. Effect and Interactions

**Polymorphic updates:** When enabled, this extension takes the `a{b=c}` syntax and uses it to mean `setField`. The biggest difference a user is likely to experience is that the resulting type of `a{b=c}` is the same as the type `a` - you _cannot_ change the type of the record by updating its fields. The removal of polymorphism is considered essential to preserve decent type inference, and is the only option supported by [the `HasField` proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst). Anyone wishing to use polymorphic updates can write `let Foo{..} = Foo{polyField=[], ..}` instead.

**Higher-rank fields:** It is impossible to express `HasField` instances for data types such as `data T = MkT { foo :: forall a . a -> a}`, which means they can't have this syntax available. Users can still write their own selector functions using record puns if required.  There is a possibility that with future types of impredicativity such `getField` expressions could be solved specially by the compiler.

**Lenses and a.b syntax:** The `a.b` syntax is commonly used in conjunction with the `lens` library, e.g. `expr^.field1.field2`. Treating `a.b` without spaces as a record projection would break such code. The alternatives would be to use a library with a different lens composition operator (e.g. `optics`), introduce an alias in `lens` for `.` (perhaps `%`), write such expressions with spaces, or not enable this extension when also using lenses. While unfortunate, we consider that people who are heavy users of lens don't feel the problems of inadequate records as strongly, so the problems are lessened. In addition, it has been discussed (e.g. [here](https://github.com/ghc-proposals/ghc-proposals/pull/282#issuecomment-546159561)), that this proposal is  complimentary to lens and can actually benefit lens users (as with `NoFieldSelectors` one can use the same field names for everything: dot notation, lens-y getting, lens-y modification, record updates, `Show/Generic`).

**Rebindable syntax:** When `RebindableSyntax` is enabled the `getField` and `setField` functions are those in scope, rather than those in `GHC.Records`. The `.` function (as used in the `a.b.c` desugaring) remains the `Prelude` version (we see the `.` as a syntactic shortcut for an explicit lambda, and believe that whether the implementation uses literal `.` or a lambda is an internal detail).

**Enabled extensions:** When `RecordDotSyntax` is a distinct extension, implying no other extensions off or on. It is often likely to be used in conjunction with either the `NoFieldSelectors` extension or`DuplicateRecordFields`.

## 5. Costs and Drawbacks

The implementation of this proposal adds code to the compiler, but not a huge amount. Our [prototype](https://gitlab.haskell.org/shayne-fletcher-da/ghc/tree/record-dot-syntax-alt) shows the essence of the parsing changes, which is the most complex part.

If this proposal becomes widely used then it is likely that all Haskell users would have to learn that `a.b` is a record field selection. Fortunately, given how popular this syntax is elsewhere, that is unlikely to surprise new users.

This proposal advocates a different style of writing Haskell records, which is distinct from the existing style. As such, it may lead to the bifurcation of Haskell styles, with some people preferring the lens approach, and some people preferring the syntax presented here. That is no doubt unfortunate, but hard to avoid - `a.b` really is ubiquitous in programming languages. We consider that any solution to the records problem _must_ cause some level of divergence, but note that this mechanism (as distinct from some proposals) localises that divergence in the implementation of a module - users of the module will not know whether its internals used this extension or not.

The use of `a.b` with no spaces on either side can make it harder to write expressions that span multiple lines. To split over two lines it is possible to do either of:

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

Below are some possible variations on this plan, but we advocate the choices made above:

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

One suggestion is that record updates remain as normal, but `a { .foo = 1 }` be used to indicate the new forms of updates. While possible, we believe that option leads to a confusing result, with two forms of update both of which fail in different corner cases. Instead, we recommend use of `C{foo}` as a pattern to extract fields if necessary.

### 7.7 Which syntax should be chosen for selector functions?

Three syntax options have bee proposed for selector functions: `.foo`, `(.foo)`, and `_.foo`.  This aspect is the most debated of the entire proposal (following [Wadler's law](https://wiki.haskell.org/Wadler's_Law)).  We have opted for `.foo`.

We consider `_.foo` to not be very Haskelly, as it is similar to very different uses of underscore.  Therefore, we reject it.

The decision between `.foo` and `(.foo)` partially comes from a significant difference of perspective:

* On the one hand, `x.foo` can be seen, as described above, as a new syntax that desugars to `getField @"foo" x`, and `(.foo)` and `(.foo.bar)` as sections of that syntax, which themselves desugar to `\x -> x.foo` and `\x -> x.foo.bar`.  Note that this is NOT a section of `.` as a binary operator, but rather a section in the more general sense that it elides the one and only subexpression and adds an implicit lambda.
* On the other hand, `.foo` can be seen as the more fundamental construct here, reminiscent of `#foo` from `OverloadedLabels`, and it can desugar directly to `getField @"foo"`.  Then one can recover the rest of the syntax above by desugaring: `x.foo` to `.foo x`, and `.foo.bar` to `.bar . .foo`.

Thus, it has been discussed at length whether field selection can be seen as just another section, or should be seen as something else that is not section-like.  The `SignatureSections` and `TupleSections` extensions (especially for 3-tuples and larger) have already established that that section can be formed by various kinds of elided expressions, not just the operands of a binary operator.  However, some would resist spreading this generalization further, and argue that `SignatureSections` and `TupleSections` are justified by looking "operator-like".  That is, even though a single `,` in a 3-tuple and the `::` in a type annotation are not real operators, some feel that they at least look a little more like it because there is non-trivial grammar on both sides.

Independent of this difference, there are pragmatic concerns on both sides:

* Some consider the parentheses to be too verbose, and the extra level of parentheses a problem for readability.  Even if one agrees that this is conceptually a section, this is the first type of section where parentheses are not actually needed for parsing, so omitting parentheses is still possible even if it loses a bit of consistency in favor of brevity.
* Some consider it acceptable (if unfortunate) that `a . b` and `a.b` have different meanings in this proposal, but believe that assigning three distinct meanings to `a . b`, `a .b`, and `a.b` is just too confusing.
* Looking at that existing implementation of GHC, supporting `(.b)` is less changes that supporting `.b` alone. While the implementation complexity is not a reason for picking one over the other, the existing grammar of the compiler can give hints about what logically follows.

### 7.8 Should punning be extended to updates?

Originally this proposal included `a{foo.bar}` to mean `a{foo.bar = bar}`, but that seemed to confuse everyone, so has been removed.

## 8. Unresolved issues

In this proposal we pick `.field` to be the syntax for selector functions, however, there are also good reasons (listed [in this proposal](#77-which-syntax-should-be-chosen-for-selector-functions)) to require brackets, namely `(.field)`. While resolved, we consider it worth the committee's deliberation as to which is preferable. Neither author is opposed to either outcome.

## 9. Implementation Plan

### 9.1 Prototype

To gain confidence these changes integrate as expected [a prototype](https://gitlab.haskell.org/shayne-fletcher-da/ghc/tree/record-dot-syntax-alt) was produced that parses and desugars forms directly in the parser. For confirmation, we _do not_ view desugaring in the parser as the correct implementation choice, but it provides a simple mechanism to pin down the changes without going as far as adding additional AST nodes or type checker rules. The prototype is sufficiently rich enough to "do the right thing" with [this test file](https://gitlab.haskell.org/shayne-fletcher-da/ghc/raw/record-dot-syntax-alt/record-dot-syntax-tests/Test.hs).

*[An earlier version of this proposal came with a different [prototype](https://gitlab.haskell.org/shayne-fletcher-da/ghc/commits/record-dot-syntax). That prototype differs from the current state of this proposal in that "naked field selectors" are deemed illegal and field selections with white-space are legal e.g. `f .x .y` is `f.x.y`. These differences lead to a somewhat different parsing scheme than the one presented here]*

### 9.2 Who will provide an implementation?

If accepted, the proposal authors would be delighted to provide an implementation. Implementation depends on the implementation of [the `HasField` proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst) and [the `NoFieldSelectors` proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0160-no-toplevel-field-selectors.rst).
