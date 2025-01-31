{-# LANGUAGE NoStarIsType #-}
module Pandia.Units.Rel
  ( module Pandia.Units.Rel
  ) where

import GHC.TypeLits
import Data.Type.Ord

import Data.Proxy

import Control.Exception

type Rel = Relative Nat

data Relative a = Pos a | Neg a

type family SumRelBool (a :: Relative Nat) (b :: Relative Nat) (cmp :: Bool) where
  SumRelBool (Pos a) (Pos b) _ = Pos (a + b)
  SumRelBool (Pos a) (Neg b) True = Pos (a - b)
  SumRelBool (Pos a) (Neg b) False = Neg (b - a)
  SumRelBool (Neg a) (Pos b) True = Neg (a - b)
  SumRelBool (Neg a) (Pos b) False = Pos (b - a)
  SumRelBool (Neg a) (Neg b) _ = Neg (a + b)

-- type CmpRel (a :: Relative Nat) (b :: Relative Nat):: Bool where
--   CmpRel (Pos a) (Pos b) = a >=? b
--   CmpRel (Pos a) (Neg b) = True


type family SumRel (a :: Relative Nat) (b :: Relative Nat) :: Relative Nat where
  SumRel a b = SumRelBool a b (a >=? b)

type family NegateRel (a :: Relative Nat) :: Relative Nat where
  NegateRel (Pos a) = Neg a
  NegateRel (Neg a) = Pos a

type family SubRel (a :: Relative Nat) (b :: Relative Nat) :: Relative Nat where
  SubRel a b = SumRel a (NegateRel b)


x:: Proxy (SumRel (Neg 2) (Pos 2))
x = Proxy

type family NormalizeRel (a :: Relative Nat) :: Relative Nat where
  NormalizeRel (Neg 0) = Pos 0
  NormalizeRel a = a

type family MulRel (a :: Relative Nat) (b :: Relative Nat) :: Relative Nat where
  MulRel (Pos a) (Pos b) = Pos (a * b)
  MulRel (Pos a) (Neg b) = Neg (a * b)
  MulRel (Neg a) (Pos b) = Neg (a * b)
  MulRel (Neg a) (Neg b) = Pos (a * b)

type family PowRel (a :: Relative Nat) (n :: Nat) :: Relative Nat where
  PowRel (Pos a) n = Pos (a ^ n)
  PowRel (Neg a) n = NegPow a n (Compare (Mod a 2) 0)

type family NegPow (a :: Nat) (n :: Nat) (mod2 :: Ordering) :: Relative Nat where
  NegPow a n EQ = Pos (a ^ n)
  NegPow a n _ = Neg (a ^ n)


class KnownRel (r :: Relative Nat)

instance KnownNat n => KnownRel (Pos n)
instance KnownNat n => KnownRel (Neg n)

class RelVal (r :: Relative Nat) where
  relVal :: proxy r -> Integer

instance KnownNat n => RelVal (Pos n) where
  relVal _  = natVal (Proxy :: Proxy n)

instance KnownNat n => RelVal (Neg n) where
  relVal _  = -natVal (Proxy :: Proxy n)