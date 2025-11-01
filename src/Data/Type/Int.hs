{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs#-}
{-# LANGUAGE RankNTypes #-}

--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Type.Int
-- Description :  Type-level integers
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- Type level integers.
--
--------------------------------------------------------------------------------


module Data.Type.Int
  ( module Data.Type.Int
  ) where

import GHC.TypeLits
import Data.Type.Ord
import Data.Type.Equality
import Data.Type.Bool

import Data.Proxy

-- | Add a sign to any type
--
data Signed a = Pos a | Neg a | Zero

-- | Type integers
--
-- ZZ represents the mathematical font for the set of integers
type ZZ = Signed Nat



type instance Compare (a :: Signed k) (b :: Signed k) = CmpSigned a b

type family IsPos (a :: ZZ) :: Bool where
  IsPos (Pos a) = 'True
  IsPos b       = 'False

-- | Compare Signed kinds when those kinds are comparable.
type family CmpSigned a b where
  CmpSigned (Neg a) (Neg b) = FlipOrdering (Compare a b)
  CmpSigned (Neg a) b = 'LT
  CmpSigned Zero (Neg a) = 'GT
  CmpSigned Zero Zero = 'EQ
  CmpSigned Zero (Pos a) = 'GT
  CmpSigned (Pos a) (Pos b) = Compare a b
  CmpSigned (Pos a) b = 'GT

-- | Always use @Zero@ instead of @Pos 0@ or @Neg 0@.
type family NormalizeInt (a :: ZZ) :: ZZ where
  NormalizeInt (Pos 0) = Zero
  NormalizeInt (Neg 0) = Zero
  NormalizeInt n = n


-- | Reverse the order of an Ordering
--
-- This should be declared in to Data.Type.Ord in base
type family FlipOrdering (o :: Ordering) :: Ordering where
  FlipOrdering 'LT = 'GT
  FlipOrdering 'EQ = 'EQ
  FlipOrdering 'GT = 'LT


-- | Absolute value
--
type family Abs (a :: ZZ) :: Nat where
  Abs (Pos a) = a
  Abs (Neg a) = a
  Abs Zero = 0

-- | Unary negation
--
type family Negate (a :: ZZ) :: ZZ where
  Negate (Pos a) = Neg a
  Negate (Neg a) = Pos a
  Negate Zero = Zero


-- | Utility family for Add
--
type family AddCmp (cmp :: Ordering) (a :: ZZ) (b :: ZZ) where
  AddCmp _ a Zero = a
  AddCmp _ Zero b = b
  AddCmp _ (Pos a) (Pos b) = Pos (a + b)
  AddCmp _ (Neg a) (Neg b) = Neg (a + b)
  AddCmp EQ _ _ = Zero
  AddCmp LT (Pos a) (Neg b) = Neg (b - a)
  AddCmp GT (Pos a) (Neg b) = Pos (a - b)
  AddCmp LT (Neg a) (Pos b) = Pos (b - a)
  AddCmp GT (Neg a) (Pos b) = Neg (a - b)

-- | Addition
type family Add (a :: ZZ) (b :: ZZ) :: ZZ where
  Add a b = AddCmp (Compare (Abs a) (Abs b)) a b

-- | Subtraction
type family Sub (a :: ZZ) (b :: ZZ) :: ZZ where
  Sub a b = Add a (Negate b)

-- | Multiplication
type family Mul (a :: ZZ) (b :: ZZ) :: ZZ where
  Mul a Zero = a
  Mul Zero b = b
  Mul (Pos a) (Pos b) = Pos (a * b)
  Mul (Pos a) (Neg b) = Neg (a * b)
  Mul (Neg a) (Pos b) = Neg (a * b)
  Mul (Neg a) (Neg b) = Pos (a * b)


-- | Exponentiation
type family Pow (a :: ZZ) (n :: Nat) :: ZZ where
  Pow Zero 0 = Pos 1 -- Following Nat from Base : 0^0 :: Natural = 1
  Pow Zero n = Zero
  Pow (Pos a) n = Pos (a ^ n)
  Pow (Neg a) n = If (Mod n 2 == 0) (Pos (a^n)) (Neg (a^n))


-- | Gives the integer associated to a type-level integer.
class KnownInt (r :: ZZ) where
  -- | Reify a type integer to an integer.
  intVal :: proxy r -> Integer

instance KnownInt Zero where
  intVal _  = 0

instance KnownNat n => KnownInt (Pos n) where
  intVal _  = natVal (Proxy :: Proxy n)

instance KnownNat n => KnownInt (Neg n) where
  intVal _  = -natVal (Proxy :: Proxy n)

-- | Singleton type for type-level integers.
data SZZ (z :: ZZ) where
  SPos :: KnownNat n => SNat n -> SZZ ('Pos n)
  SNeg :: KnownNat n => SNat n -> SZZ ('Neg n)
  SZero :: SZZ 'Zero

-- | Singleton for zero.
zero :: SZZ 'Zero
zero = SZero

-- | Singleton for positive integers.
--
-- >>> :t pos @3
--  pos @3 :: KnownNat 3 => SZZ ('Pos 3)
--
pos :: KnownNat n => SZZ ('Pos n)
pos = SPos SNat

-- | Integer singleton for 1.
pos1 :: SZZ ('Pos 1)
pos1 = pos @1

-- | Integer singleton for 2.
pos2 :: SZZ ('Pos 2)
pos2 = pos @2

-- | Integer singleton for 3.
pos3 :: SZZ ('Pos 3)
pos3 = pos @3

-- | Integer singleton for 4.
pos4 :: SZZ ('Pos 4)
pos4 = pos @4

-- | Singleton for negative integers.
--
-- >>> :t neg @3
-- neg @3 :: KnownNat 3 => SZZ ('Neg 3)
--
neg :: KnownNat n => SZZ ('Neg n)
neg = SNeg SNat

-- | Integer singleton for -1.
neg1 :: SZZ ('Neg 1)
neg1 = neg @1

-- | Integer singleton for -2.
neg2 :: SZZ ('Neg 2)
neg2 = neg @2

-- | Integer singleton for -3.
neg3 :: SZZ ('Neg 3)
neg3 = neg @3

-- | Integer singleton for -4.
neg4 :: SZZ ('Neg 4)
neg4 = neg @4


-- | Return the 'Integer' corresponding to @n@ in an @SZZ n@ value.
fromSZZ :: SZZ n -> Integer
fromSZZ SZero      = 0
fromSZZ (SPos sn)  = natVal sn
fromSZZ (SNeg sn)  = -natVal sn

-- | Convert an 'Integer' into an 'SZZ n' value, where 'n' is a fresh type-level
-- symbol.
withSomeSZZ :: Integer -> (forall (n :: ZZ). SZZ n -> r) -> r
withSomeSZZ 0 f = f SZero
withSomeSZZ i f
  | i == 0    = f SZero
  | i > 0     = withSomeSNat i (fpos f)
  | otherwise = withSomeSNat (negate i) (fneg f)
  where
    fpos f' (Just (SNat :: SNat n)) = f' (SPos (SNat @n))
    fpos _ Nothing = error "withSomeSZZ: This should never happen.\
      \ A bug report would be appreciated."

    fneg f' (Just (SNat :: SNat n)) = f' (SNeg (SNat @n))
    fneg _ Nothing = error "withSomeSZZ: This should never happen.\
      \ A bug report would be appreciated."

-- | Convert an explicit `SZZ n` value into an implicit `KnownInt n` constraint.
withKnownInt :: SZZ n -> (KnownInt n => r) -> r
withKnownInt SZero      r = r
withKnownInt (SPos sn)  r = withKnownNat sn r
withKnownInt (SNeg sn)  r = withKnownNat sn r
