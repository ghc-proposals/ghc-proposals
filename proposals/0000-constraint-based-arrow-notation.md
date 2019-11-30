---
author: Alexis King
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/303).

# Constraint based arrow notation

Since GHC 7.8, the desugaring of custom commands (aka `(|` banana brackets `|)`) in [arrow notation][] has differed from that used in both [the original paper][Paterson01] and [the original implementation in GHC][Paterson04]. The current desugaring is easier to typecheck and theoretically provides the same expressive power as the original desugaring, but in practice, it is incompatible with the types programmers assign to control operators. For example, consider the type of `handle` given in the paper and [the `arrows` library][hackage:arrows]:

```haskell
handle :: ArrowError e arr => arr a b -> arr (a, e) b -> arr a b
```

The original desugaring supported the use of `handle` as a control operator, but under GHC’s current desugaring, it fails to typecheck. This proposal presents an alternate, constraint-based approach that is more faithful to the original notation, allowing many more operations to be used directly as control operators.

## Motivation

As mentioned above, arrow notation provides special syntax to support so-called “user defined commands,” which allows user-defined control operators to take advantage of the automatic environment passing afforded by `proc`. A motivating example given in Paterson’s original paper, [A New Notation for Arrows][Paterson01], is the aforementioned `handle` operator, intended for use in arrow notation with the following syntax:

```haskell
c1 `handle` \e -> c2
```

The key feature of this syntax is that arguments to `handle` are specified as *commands*, not expressions, and those commands may refer to arrow-local variables currently in scope in the enclosing `proc` expression. GHC’s original implementation of arrow notation accepted the above use of `handle`, as documented in Paterson’s `arrows` library, but on modern versions of GHC, it is rejected with an error like the following:

```
error:
  • Occurs check: cannot construct the infinite type: a1 ~ (a1, ())
    Expected type: arr (a1, ()) b
                   -> arr (a1, (e, b0)) b -> arr (a1, ()) b
      Actual type: arr (a1, ()) b
                   -> arr ((a1, ()), (e, b0)) b -> arr (a1, ()) b
  • In the expression: handle
```

The discrepancy comes from a change made in GHC 7.8 to the way the current “argument stack” is passed to commands. In earlier versions of GHC, each argument to a control operator was expected to have the following shape:

```haskell
((... ((e, s1), s2), ...), sn)
```

The first value, `e`, is the current *command environment*, a compiler-defined representation of the current lexical environment used to thread the values of variables in scope through the generated arrow expression. The other values, `s1` through `sn`, form the initial *argument stack* passed to the subcommand. In the simplest case, the stack is empty, such as in the first argument to `handle`, but a non-empty stack can be used to pass values to subcommands, in this case the value of a caught exception. The `\e -> c2` term is not actually a lambda expression, but a lambda *command*, which binds the topmost element of the argument stack to a variable in the local environment.

Modern versions of GHC also use an environment and argument stack, but they use a different representation:

```haskell
(e, (s1, (s2, ... (sn, ()) ...)))
```

At first blush, this is quite similar—it uses the same strategy of nesting tuples—but the key difference is the use of `()` as a terminating element. This is easier for GHC’s constraint-based typechecker to understand, but it causes trouble for programmers. The expected type of `handle` becomes

```haskell
handle :: ArrowError e arr => arr (a, ()) b -> arr (a, (e, ())) b -> arr (a, ()) b
```

which is both much more complicated to read and a pretty stupid type to write if one is not specifically cooperating with arrow notation. This is a problem, as not *all* uses of operations such as `handle` are in arrow notation, and the original version with the simpler type is much more natural to work with in those situations. Programmers are forced to choose between three options: provide the simpler type and forgo arrow notation, provide the complicated one and require arrow notation, or provide both and write an adapter from one to the other by hand. None of these is ideal!

Some operations, such as `(<+>)`, are sufficiently polymorphic that they work under both desugaring schemes. However, `handle` is not alone in its incompatibility with GHC’s current approach:

  1. The `newReader` operation from the `ArrowReader` class (which is the analog to `local` from `MonadReader`) fails in a similar way to `handle`, but with the extra stack argument appearing in the return type rather than the argument type.

  2. The examples given in GHC’s own User’s Guide include several example operations that were required to change under the new approach, including `bracketA`, `runReader`, `runState`, `bind`, and `cond`.

  3. Various types that appear in the wild are dramatically more likely to work with the old approach than the new one. For example, the codebase I work on for my day job includes the following function:

     ```haskell
     keyed :: (Eq k, Applicative m) => Rule m (e, k, a) b -> Rule m (e, Map k a) (Map k b)
     ```

     Making this work with the old notation only requires adding an extra level of nesting in the first argument, but making it work with the new notation requires a dramatically more complicated type. Under this proposal, it would work in arrow notation as-written with no changes at all.

In other words, the original rules were carefully designed to align with the types programmers *naturally* write, allowing ordinary arrow combinators to be used as control operators. The current desugaring used by GHC falls flat precisely because it does *not* align with those types, making it significantly less useful (so much so that users seem confused about the notation’s purpose).

## Proposed Change Specification

To restore the spirit of the original system, I propose the following modifications to GHC’s implementation of arrow notation.

### Typechecking

Both the original and current system [are specified][Paterson04] using judgments of the following form:

```
G;D |-a c :: stk --> t
```

These judgments should be read as follows: under contexts `G` and `D` containing the types of non-local and local variables, respectively, the command `c` accepts a stack of type `stk` and returns a value of type `t` in some arrow `a`. The special `-->` arrow is *not* a Haskell type, but part of the syntax of the `|-a` judgment itself. The `stk` term is a list of (mono-)types, and it is an “input” to the judgment for the purposes of an algorithmic implementation.

Here is an example of an existing typechecking rule, this one for command application:

```
G;ys |-a cmd :: (t1, stk) --> t2
G,xs |- exp :: t1
ys \subseteq xs
--------------------------------
G;xs |-a cmd exp :: stk --> t2
```

I do not propose any radical changes to the structure of the `|-a` judgment or its rules, but I suggest the following key modifications:

  1. Eliminate the requirement that `stk` be strictly an input to `|-a`. Instead, allow it to be an arbitrary type of kind `[Type]`. This allows `stk` to be a (potentially unsolved) metavariable rather than a concrete list, which allows GHC’s typechecker to propagate information bidirectionally using the ordinary type inference mechanism.

  2. Introduce two wired-in type families:

     1. `ArrowStack :: [Type] -> Type`, which converts a stack type to a tuple. Morally, it is a closed type family with the following infinitely-long definition:

        ```haskell
        type family ArrowStack stk where
          ArrowStack '[a]       = a
          ArrowStack '[a, b]    = (a, b)
          ArrowStack '[a, b, c] = (a, b, c)
          ...
        ```

        This type family is non-injective, as the RHS of the first case overlaps with all other cases.

     2. `ArrowEnv :: Type -> [Type] -> Type`, which accepts an environment and a stack and produces a tuple. This is quite similar to `ArrowStack`, and it has a similar definition:

         ```haskell
         data Env env -- an opaque type that represents the local environment

         type family ArrowEnv env stk = arg | arg -> env stk where
           ArrowEnv env '[]     = Env env
           ArrowEnv env '[a]    = (Env env, a)
           ArrowEnv env '[a, b] = (Env env, a, b)
           ...
         ```

         Crucially, this type family *is* injective, which preserves important type inference properties.

  3. Change the `|-a` rules for `f -< e`, `f -<< e`, `c e`, `\p -> c`, and `(| e c ... |)` to use the above type families in the relevant places. In the context of GHC’s implementation, this means emitting equality constraints between applications of those type families rather than solving everything up front.

Because the ASCII-art versions of the modified rules are more difficult to read than properly typeset versions, I’ve created readable renderings of the key rules (where Σ(θ) is used in place of `ArrowStack` and Σ(Δ,θ) is used in place of `ArrowEnv`):

<img src="0000-typechecking-rules.png" height="600" />

The rule for `f -<< e` follows naturally from the one for `f -< e`, and the other rules remain essentially unchanged.

### Desugaring

To specify how `proc` notation is translated to ordinary Haskell code, the `|-a` judgment given by Paterson is further extended with desugaring rules:

```
G;D |-a c :: stk --> t    --->  e
```

The added `--->  e` clause specifies that the command `c` desugars to the expression `e`. Here is the rule for command application extended with desugaring rules:

```
G;ys |-a cmd :: (t1, stk) --> t2    --->  cmd'
G,xs |- exp :: t1
ys \subseteq xs
--------------------------------
G;xs |-a cmd exp :: stk --> t2      --->  premap (\((xs), stk) -> ((ys), (exp, stk))) cmd'
```

In GHC, the desugaring rules can remain essentially unchanged under this proposal, since the desugarer runs after typechecking, and it has access to fully-solved type information. However, *in principle* that is not necessary, as it is possible to define arrow notation as a type-agnostic translation into applications of special typeclass methods. I have worked out a set of desugaring rules that use that approach, but I have omitted them here because I believe them to be unnecessary; please let me know if you feel otherwise, and I can provide them.

### Other details

#### Error reporting

In practice, it is unlikely that the generated equality constraints on `ArrowStack` and `ArrowEnv` will ever fail to solve. The strict type required by the `(| e c ... |)` rule is almost always enough to propagate information downwards, and the other rules cannot introduce any ambiguity about the shape of the stack. However, while unlikely, it is not impossible, so it likely makes sense for GHC to generate custom type error messages upon failure to solve `ArrowStack` and `ArrowEnv` equalities to avoid implementation details leaking out to the programmer.

#### The `Env` type

The `ArrowEnv` rule wraps the environment type in an opaque, compiler-defined `Env` type, unlike the current implementation, which simply uses a large tuple. The separate `Env` type is necessary primarily to ensure the `ArrowEnv` type family is injective: if it were a tuple, the RHS of the empty stack case would overlap with other cases. Additionally, the `Env` wrapper allows the `(| e c ... |)` rule to check `e` against a type with a skolem variable in place of `env` without destroying injectivity.

Fortunately, the wrapper itself is only necessary at the type level, so the actual representation can be identical to the current one. A logical implementation of `Env` would be

```haskell
newtype Env env = Env env
```

so the current representation can be passed as the `env` parameter directly.

#### The stack and large tuples

This proposal eliminates the use of a nested tuple for the argument stack completely. Instead, a stack of *n* arguments is represented directly be a tuple of size *n* (or *n*+1 when combined with an environment). Theoretically, this could cause trouble, as GHC does not support tuples of arbitrary size.

I don’t think this is actually a problem—61 arguments really ought to be enough for anyone! The only situation where the tuple limit could possibly be relevant is the environment, but the existing technique of nesting tuples works fine there, since the representation of the environment is opaque. It’s probably a good idea to make `ArrowStack` and `ArrowEnv` report meaningful error messages if that limit is ever somehow exceeded, but otherwise, I don’t expect this to be an issue.

## Examples

With this proposal, all of the examples given in the motivation section simply work. Here is a list of control operators and their types, all of which are accommodated by this proposal:

```haskell
(<+>) :: ArrowPlus a => a e c -> a e c -> a e c
untilA :: ArrowChoice a => a e () -> a e Bool -> a e ()
handleA :: ArrowError ex a => a e c -> a (e, ex) c -> a e c
bracketA :: ArrowMask a => a e b -> a (e, b) c -> a (e, c) d -> a e d
localA :: ArrowReader r a => a e b -> a (e, r) b
runReaderA :: Arrow a => ReaderA r a e b -> a (e, r) b
runStateA :: Arrow a => StateA s a e b -> a (e, s) (b, s)
bind :: Arrow a => a e b -> a (e, b) c -> a e c
bind_ :: Arrow a => a e b -> a e c -> a e c
cond :: ArrowChoice a => a e b -> a e b -> a (e, Bool) b
```

Of the above list, only `(<+>)`, `untilA`, and `bind_` are accepted as control operators in the current implementation.

## Effect and Interactions

Aside from the direct, intended effect on this particular feature of `proc` notation, the impact of the proposed change should be small to nonexistent. Programs that do not use `proc` notation will not be impacted at all, and programs that use `proc` notation but do not use custom control operators are unlikely to be impacted, either. The scope of this change is small.

## Costs and Drawbacks

There are two primary drawbacks to this change:

  1. It is backwards-incompatible.

     Ordinarily, a change that is *this* backwards-incompatible would need to be handled with great care, and it would likely need to be hidden behind a separate language extension. However, in this case, my recommendation is to simply make the change.

     The number of users of `proc` notation is small to begin with, and the number of users using custom control operators is vanishingly tiny. Consider that GHC 7.8 made an equally backwards-incompatible change to this same feature, and as far as I can tell, precisely zero users complained (and possibly roughly as many noticed). The change was considered so inconsequential that despite being very backwards incompatible, it was not even mentioned in the release notes!

  2. It adds new functionality into the constraint solver (indirectly, via the wired-in type families) specifically to support arrows, which seem somewhat maligned in the Haskell community. Arguably, this moves in the wrong direction: some people might prefer `proc` notation be removed, not further cemented.

     I think, however, such fear would be unfounded. The proposed change doesn’t make `proc` notation any more difficult to remove should someone wish to do it in the future, nor does it introduce any meaningful maintenance burden on any other part of the compiler. As long as `proc` notation is here (and some programmers, myself included, do find it useful!), it might as well work properly.

## Alternatives

This proposal has the following alternatives:

  1. **Implement this change, but gate it behind a separate language extension.**

     I outlined in the previous section why I think this isn’t worth doing, and it would complicate the implementation but provide relatively little value. Still, it’s always an option.

  2. **Implement this change, but use the nested tuple representation of the original paper instead of the flat representation of this proposal.**

     This is basically just a subjective choice. Arguably, the original implementation worked the way it did because it wasn’t clear how to make it work differently at the time. I think the flat representation is more useful to programmers, and I don’t think it has any significant downsides, but I’m not completely attached to it either.

  3. **Do nothing.**

     Arrows are a pretty low-priority feature in GHC right now, so not doing this isn’t going to leave too many people upset. Still, I think it’s a small enough change to be worth the effort, assuming someone (me) is willing to volunteer their time to implement it.

## Unresolved Questions

I do not have any unresolved questions about the approach in the proposal at this time.

## Implementation Plan

I, Alexis King, volunteer to implement this change if this proposal is accepted. I have examined the implementation of `TcArrows` and `DsArrows`, and neither of them seem particularly complicated to change. I am no expert in GHC’s internals, and in fact I’ve never actually submitted a patch before, but this seems like as good a reason as any to try.

Concretely, I expect the change will mostly involve implementing `ArrowStack` and `ArrowEnv` as new `BuiltInSynFamily`s and plumbing them through `TcArrows`.

## Endorsements

None so far.

[arrow notation]: https://downloads.haskell.org/ghc/8.8.1/docs/html/users_guide/glasgow_exts.html#arrow-notation
[hackage:arrows]: https://hackage.haskell.org/package/arrows
[Paterson01]: http://www.staff.city.ac.uk/~ross/papers/notation.html
[Paterson04]: https://downloads.haskell.org/~ghc/papers/arrow-rules.pdf
