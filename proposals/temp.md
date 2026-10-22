```hs
-- ========================== New proposal API ==========================

newtype CHollow# (lctor :: k) a = CHollow# (# Compact#, a #)

chollowUnsafeCoerce# :: forall k (lctor :: k) a. CHollow# lctor a -> a
chollowUnsafeCoerce# (CHollow# (# _, a #)) = a

chollowCompact# :: forall k (lctor :: k) a. CHollow# lctor a -> Compact#
chollowCompact# (CHollow# (# compact, _ #)) = compact

class FillCHollowField (lctor :: k) a (n :: Nat) (f :: TYPE r) (boxed :: Bool) | lctor a n -> f, lctor a n -> boxed where
  -- compile-time primop
  -- expects that f lives in the same region as the CHollow#
  fillCHollowField#
    :: CHollow# lctor a
    -> f
    -> State# RealWorld
    -> State# RealWorld

-- RTS primop (not exposed)
allocHollowFromItp# :: Compact# -> Addr# -> State# RealWorld -> (# State# RealWorld, a #)
-- Compile-time primop (not exposed)
reifyCtorItp# :: forall k (lctor :: k). Proxy# lctor -> Addr#

type LCtorToType :: forall k. k -> Type -- I don't have an idea at the moment to get rid of that

compactAddCHollow#
  :: forall k (lctor :: k)
  .  Compact#
  -> Proxy# lctor
  -> State# RealWorld
  -> (# State# RealWorld, CHollow# lctor (LCtorToType lctor) #)
compactAddCHollow# region prox s = case allocHollowFromItp# region (reifyCtorItp# prox) s of
  (# s', hollow #) -> (# s', CHollow# (# region, hollow #) #)

-- ========================== Dest-based API (userland, not part of proposal) ==========================

newtype Dest# (boxed :: Bool) (f :: TYPE r) = Dest# (# CHollow# lctor a, Addr# #)

getDest#
  :: forall k r (lctor :: k) a (n :: Nat) (f :: TYPE r) (boxed :: Bool)
  .  FillCHollowField lctor a n f boxed
  => CHollow# lctor a
  -> Proxy# n
  -> State# RealWorld
  -> (# State# RealWorld, Dest# boxed f #)
getDest# chollow _ s =
  case anyToAddr# (fillCHollowField# @lctor @a @n @f @boxed) s of
    (# s', funcPtr #) -> (# s', Dest# (# chollow, funcPtr #) #)

-- Need to ensure that the argument x already lives in the same region as chollow
fillLeaf#
  :: forall r (f :: TYPE r) (boxed :: Bool)
  .  Dest# boxed f
  -> f
  -> State# RealWorld
  -> State# RealWorld
fillLeaf# (Dest# (# chollow :: CHollow# lctor a, funcPtr #)) x s =
  let fillFunc :: CHollow# lctor a -> f -> State# RealWorld -> State# RealWorld
      fillFunc = addrToAny# funcPtr
   in fillFunc chollow x s

-- Need to ensure that the argument chollow' already lives in the same region as chollow
fillComp#
  :: forall (f :: Type) k' (lctor' :: k')
  .  Dest# 'True f
  -> CHollow# lctor' f
  -> State# RealWorld
  -> State# RealWorld
fillComp# (Dest# (# chollow :: CHollow# lctor a, funcPtr #)) chollow' s =
  let fillFunc :: CHollow# lctor a -> f -> State# RealWorld -> State# RealWorld
      fillFunc = addrToAny# funcPtr
   in fillFunc chollow (chollowUnsafeCoerce# chollow') s
```
