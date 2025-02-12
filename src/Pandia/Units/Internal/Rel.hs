{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ExistentialQuantification #-}
module Pandia.Units.Internal.Rel
  ( module Pandia.Units.Internal.Rel
  ) where

import GHC.TypeLits
import Data.Type.Ord

import Data.Proxy

-- | Type level sign that allow to encode type level integers
data Signed a = Pos a | Neg a

type Rel = Signed Nat

type family AbsRel (a :: Rel) :: Nat where
  AbsRel (Pos a) = a
  AbsRel (Neg a) = a

type family NegateRel (a :: Rel) :: Rel where
  NegateRel (Pos a) = Neg a
  NegateRel (Neg a) = Pos a

type family SumRelBool (a :: Rel) (b :: Rel) (cmp :: Bool) where
  SumRelBool (Pos a) (Pos b) _ = Pos (a + b)
  SumRelBool (Pos a) (Neg b) True = Pos (a - b)
  SumRelBool (Pos a) (Neg b) False = Neg (b - a)
  SumRelBool (Neg a) (Pos b) True = Neg (a - b)
  SumRelBool (Neg a) (Pos b) False = Pos (b - a)
  SumRelBool (Neg a) (Neg b) _ = Neg (a + b)

type family SumRel (a :: Rel) (b :: Rel) :: Rel where
  SumRel a b = SumRelBool a b (AbsRel a >=? AbsRel b)

type family SubRel (a :: Rel) (b :: Rel) :: Rel where
  SubRel a b = SumRel a (NegateRel b)


type family NormalizeRel (a :: Rel) :: Rel where
  NormalizeRel (Neg 0) = Pos 0
  NormalizeRel a = a

type family MulRel (a :: Rel) (b :: Rel) :: Rel where
  MulRel (Pos a) (Pos b) = Pos (a * b)
  MulRel (Pos a) (Neg b) = Neg (a * b)
  MulRel (Neg a) (Pos b) = Neg (a * b)
  MulRel (Neg a) (Neg b) = Pos (a * b)

type family PowRel (a :: Rel) (n :: Nat) :: Rel where
  PowRel (Pos a) n = Pos (a ^ n)
  PowRel (Neg a) n = NegPow a n (Compare (Mod n 2) 0)

type family NegPow (a :: Nat) (n :: Nat) (mod2 :: Ordering) :: Rel where
  NegPow a n EQ = Pos (a ^ n)
  NegPow a n _ = Neg (a ^ n)


class KnownRel (r :: Rel) where
  relVal :: proxy r -> Integer

instance KnownNat n => KnownRel (Pos n) where
  relVal _  = natVal (Proxy :: Proxy n)

instance KnownNat n => KnownRel (Neg n) where
  relVal _  = -natVal (Proxy :: Proxy n)


pos0 :: Proxy (Pos 0)
pos0 = Proxy

pos1 :: Proxy (Pos 1)
pos1 = Proxy

pos2 :: Proxy (Pos 2)
pos2 = Proxy

pos3 :: Proxy (Pos 3)
pos3 = Proxy

pos4 :: Proxy (Pos 4)
pos4 = Proxy

neg1 :: Proxy (Neg 1)
neg1 = Proxy

neg2 :: Proxy (Neg 2)
neg2 = Proxy

neg3 :: Proxy (Neg 3)
neg3 = Proxy

neg4 :: Proxy (Neg 4)
neg4 = Proxy




