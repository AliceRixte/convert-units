module Data.Units.SI
  ( module Data.Units.SI
  ) where

import GHC.TypeLits

import Data.Units.Base

newtype Length a = Length a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance DimId Length = 300
type instance ShowDim Length = Text "L"

-- | A quantity in meters
--
-- This is the base unit of the length dimension in the SI system.
newtype Meter a = Meter a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)
  deriving Show via StdUnit Meter a

instance Fractional a => ConvFactor Meter a where
  factorFrom = 1

instance IsUnit Meter where
  type StdUnitOf Meter = Meter

instance ShowUnit Meter where
  type ShowUnitType Meter = Text "m"
  showUnit = "m"


type instance DimOf Meter = Length


newtype Time a = Time a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance DimId Time = 400
type instance ShowDim Time = Text "T"

-- This is the base unit of the length dimension in the SI system.
newtype Second a = Second a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)


instance Fractional a => ConvFactor Second a where
  factorFrom = 1

instance ShowUnit Second where
  type ShowUnitType Second = Text "s"
  showUnit = "s"

instance IsUnit Second where
  type StdUnitOf Second = Second


type instance DimOf Second = Time

-- instance Fractional a => Dimensional (Second a) a where
type instance DimId Second = 400
  -- factor = 1