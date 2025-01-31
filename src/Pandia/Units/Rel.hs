{-# LANGUAGE NoStarIsType #-}
module Pandia.Units.Rel
  ( module Pandia.Units.Rel
  ) where

import GHC.TypeLits
import Data.Type.Ord

import Data.Proxy


data Rel a = Pos a | Neg a

type family SumRelBool (a :: Rel Nat) (b :: Rel Nat) (cmp :: Bool) where
  SumRelBool (Pos a) (Pos b) _ = Pos (a + b)
  SumRelBool (Pos a) (Neg b) True = Pos (a - b)
  SumRelBool (Pos a) (Neg b) False = Neg (b - a)
  SumRelBool (Neg a) (Pos b) True = Neg (a - b)
  SumRelBool (Neg a) (Pos b) False = Pos (b - a)
  SumRelBool (Neg a) (Neg b) _ = Neg (a + b)

type family SumRel (a :: Rel Nat) (b :: Rel Nat) :: Rel Nat where
  SumRel a b = SumRelBool a b (a >=? b)

type family NegateRel (a :: Rel Nat) :: Rel Nat where
  NegateRel (Pos a) = Neg a
  NegateRel (Neg a) = Pos a

type family NormalizeRel (a :: Rel Nat) :: Rel Nat where
  NormalizeRel (Neg 0) = Pos 0
  NormalizeRel a = a

type family MulRel (a :: Rel Nat) (b :: Rel Nat) :: Rel Nat where
  MulRel (Pos a) (Pos b) = Pos (a * b)
  MulRel (Pos a) (Neg b) = Neg (a * b)
  MulRel (Neg a) (Pos b) = Neg (a * b)
  MulRel (Neg a) (Neg b) = Pos (a * b)

type family PowRel (a :: Rel Nat) (n :: Nat) :: Rel Nat where
  PowRel (Pos a) n = Pos (a ^ n)
  PowRel (Neg a) n = NegPow a n (Compare (Mod a 2) 0)

type family NegPow (a :: Nat) (n :: Nat) (mod2 :: Ordering) :: Rel Nat where
  NegPow a n EQ = Pos (a ^ n)
  NegPow a n _ = Neg (a ^ n)




-- x :: Proxy (SumRel (Pos 1) (Neg 2))
-- x = Proxy
