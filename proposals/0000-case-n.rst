.. proposal-number:: 

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

Extend `LambdaCase` with `\case2`, `\case3`
==============

`LambdaCase` lets you write less code or have fewer variable to worry
about by using `\case`. It allows a convenient notation for anonymous
functions that pattern match, I would like those benefits extended to
functions of two or more arguments with `\case2`, `\case3`.

Motivation
----------

```haskell
transpose :: [[a]] -> [[a]]
transpose []             = []
transpose ([]   : xss)   = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])
```

I use `LambdaCase` a lot. The code is terser with more focus on the
structure of the program, I don't have to repeat a function's name and
sometimes it lets me omit parentheses:

```haskell
transpose :: [[a]] -> [[a]]
transpose = \case
  []         -> []
  []    :xss -> transpose xss
  (x:xs):xss -> (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])
```

You avoid another variable name: I will call the argument `xs` and
shadow it in the last pattern which can lead to confusion:

```haskell
transpose :: [[a]] -> [[a]]
transpose ys = case ys of
  []         -> []
  []    :xss -> transpose xss
  (x:xs):xss -> (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])
```

It is also useful for passing one-off functions as arguments:

```
> unfoldr (\case 0 -> Nothing; n -> Just (n, n-1)) 10
[10,9,8,7,6,5,4,3,2,1]
```

It would be nice to get the same benefits for more than one argument.

Proposed Change
---------------

Using `\case2` it would desugar into a lambda of two arguments
followed by a case analysis of their product:

```haskell
-- foo = \x y -> case (x, y) of
--   (Nothing, y) -> y
--   (Just x,  y) -> x + y

foo = \case2
  (Nothing, y) -> y
  (Just x,  y) -> x + y
```

and a `\case3` desugars into a lambda of 3 arguments and scrutinises
their 3-product:

```haskell
-- foo = \x y z -> case (x, y, z) of
--   (Nothing, y, z) -> y + z
--   (Just x,  y, _) -> x + y

foo = \case3
  (Nothing, y, z) -> y + z
  (Just x,  y, _) -> x + y
```

A large number of Haskell functions pattern match on their two last
arguments simultaneously:

```haskell
-- isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
-- isPrefixOf [] _         =  True
-- isPrefixOf _  []        =  False
-- isPrefixOf (x:xs) (y:ys)=  x == y && isPrefixOf xs ys

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf = \case2
  ([],   _)    -> True
  (_,    [])   -> False
  (x:xs, y:ys) -> x == y && isPrefixOf xs ys
```

Using `\case2` and some arranging we can rewrite

```haskell
instance (Eq1 f, Eq1 g) => Eq1 (Sum f g) where
  liftEq :: (a -> b -> Bool) -> ((Sum f g) a -> (Sum f g) b -> Bool)
  liftEq eq (InL x1) (InL x2) = liftEq eq x1 x2
  liftEq _  (InL _)  (InR _)  = False
  liftEq _  (InR _)  (InL _)  = False
  liftEq eq (InR y1) (InR y2) = liftEq eq y1 y2
```

to

```haskell
instance (Eq1 f, Eq1 g) => Eq1 (Sum f g) where
  liftEq :: (a -> b -> Bool) -> ((Sum f g) a -> (Sum f g) b -> Bool)
  liftEq eq = \case2
    (InL x1, InL x2) -> liftEq eq x1 x2
    (InR y1, InR y2) -> liftEq eq y1 y2
    _                -> False
```

Usecase for record syntax from [Trac
#12376](https://ghc.haskell.org/trac/ghc/ticket/12376#comment:3) where
the user cannot use a multi-equation definition:

```haskell
eqList a = MkEq
  { (==) = \case2
      ([],   [])   -> True
      (x:xs, y:ys) -> ...
      (_,    _)    -> False
  , (/=) = neqDef (eqList a)
  }
```

Let's say we defined our own version of `Bool` and wanted to use
`foldBy` to crush some structure like we would with `All`:

```haskell
data B = F | T

foldB :: Foldable f => f B -> B
foldB = foldBy (\case2 (T, T) -> T; _ -> F) T
```

Compare this to the alternatives:

```haskell
foldB' :: Foldable f => f B -> B
foldB' = foldBy (\a b -> case (a, b) of (T, T) -> T; _ -> F) T

-- Wouldn't work in GHCi
foldB'' :: Foldable f => f B -> B
foldB'' = foldBy and T where
  and T T = T
  and _ _ = F

foldB''' :: Foldable f => f B -> B
foldB''' = foldBy (let and T T = T; and _ _ = F in and) T
```
* discuss how the change addresses the points raised in the Motivation section
* discuss how the proposed approach might interact with existing features  

Note, however, that this section need not describe details of the
implementation of the feature. The proposal is merely supposed to give a
conceptual specification of the new feature and its behavior.

The more equations, the more we save.

Drawbacks
---------

This shouldn't steal any syntax, the syntax still doesn't look ideal:
I will make a separate proposal to allow omitting parentheses when it
is non-ambiguous:

```haskell
isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf = \case2
  [],   _    -> True
  _,    []   -> False
  x:xs, y:ys -> x == y && isPrefixOf xs ys

eqList a = MkEq
  { (==) = \case2
      [],   []   -> True
      x:xs, y:ys -> ...
      _,    _    -> False
  , (/=) = neqDef (eqList a)
  }

instance (Eq1 f, Eq1 g) => Eq1 (Sum f g) where
  liftEq :: (a -> b -> Bool) -> ((Sum f g) a -> (Sum f g) b -> Bool)
  liftEq eq = \case2
    InL x1, InL x2 -> liftEq eq x1 x2
    InR y1, InR y2 -> liftEq eq y1 y2
    _              -> False

foldB :: Foldable f => f B -> B
foldB = foldBy (\case2 T, T -> T; _ -> F) T
```

This is a very minimal change but it would improve my life.

Alternatives
------------

Just use a lambda and a `case`, come on.

Unresolved Questions
--------------------
