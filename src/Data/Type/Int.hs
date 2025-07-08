{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ExistentialQuantification #-}
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

type family CmpSigned a b where
  CmpSigned (Neg a) (Neg b) = FlipOrdering (Compare a b)
  CmpSigned (Neg a) b = 'LT
  CmpSigned Zero (Neg a) = 'GT
  CmpSigned Zero Zero = 'EQ
  CmpSigned Zero (Pos a) = 'GT
  CmpSigned (Pos a) (Pos b) = Compare a b
  CmpSigned (Pos a) b = 'GT

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


-- | To the power of a natural number
type family Pow (a :: ZZ) (n :: Nat) :: ZZ where
  Pow Zero 0 = Pos 1 -- Following Nat from Base : 0^0 :: Natural = 1
  Pow Zero n = Zero
  Pow (Pos a) n = Pos (a ^ n)
  Pow (Neg a) n = If (Mod n 2 == 0) (Pos (a^n)) (Neg (a^n))

-- | Lifting of singleton type for Nat to type
type family SInt (a :: ZZ) where
  SInt (Neg a) = 'Neg (SNat a)
  SInt Zero = 'Zero
  SInt (Pos a) = 'Pos (SNat a)

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




