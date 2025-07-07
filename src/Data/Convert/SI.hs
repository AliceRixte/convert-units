module Data.Convert.SI
  ( module Data.Convert.SI
  ) where

import Control.Newtype

import Data.Convert.FromTo
import Data.Convert.Unit


-- | A quantity in meters
--
-- This is the base unit of the length dimension in the SI system.
newtype Metre a = Metre a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)


type instance Standard (Metre a) = Metre a
type instance Dimension Metre = "1"

instance Num a => Dimensional (Metre a) a where
  factor = 1

-- This is the base unit of the length dimension in the SI system.
newtype Sec a = Sec a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance Newtype (Sec a) a

type instance Standard (Sec a) = Sec a

-- instance Fractional a => Dimensional (Sec a) a where
type instance Dimension Sec = "2"
  -- factor = 1