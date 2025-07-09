module Data.Units.SI
  ( module Data.Units.SI
  ) where

import Data.Proxy
import GHC.TypeLits

import Data.Units.Base

newtype Length a = Length a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance DimId Length = 300
type instance DimName Length = "L"

-- | A quantity in meters
--
-- This is the base unit of the length dimension in the SI system.
newtype Metre a = Metre a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)
  deriving Show via StdUnit Metre a

instance Fractional a => IsQuantity (Metre a) a where
  convFactor = 1

instance IsUnit Metre where
  type StdUnitOf Metre = Metre

instance ShowUnit Metre where
  showUnit = "m"


type instance DimOf Metre = Length


newtype Time a = Time a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance DimId Time = 400
type instance DimName Time = "T"

-- This is the base unit of the length dimension in the SI system.
newtype Sec a = Sec a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)


instance Fractional a => IsQuantity (Sec a) a where
  convFactor = 1

instance ShowUnit Sec where
  showUnit = "s"

instance IsUnit Sec where
  type StdUnitOf Sec = Sec


type instance DimOf Sec = Time

-- instance Fractional a => Dimensional (Sec a) a where
type instance DimId Sec = 400
  -- factor = 1