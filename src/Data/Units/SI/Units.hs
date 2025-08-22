module Data.Units.SI.Units
  ( module Data.Units.SI.Units
  ) where

import GHC.TypeLits

import Data.Units.Base
import Data.Units.SI.Dimensions



-- | A quantity in meters, denotated @m@ in SI.
--
-- This is the base unit of the length dimension in the SI system.
--
newtype Meter a = Meter a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via MetaUnit Meter a

instance Fractional a => ConvFactor Meter a where
  factorFrom = 1

instance IsUnit Meter where
  type DimOf Meter = Length
  type StdUnitOf Meter = Meter

instance ShowUnit Meter where
  type ShowUnitType Meter = Text "m"
  prettyUnit = "m"
  showUnit = "Meter"


-- | A quantity in seconds, denotated @s@ in SI.
--
-- This is the base unit of the time dimension in the SI system.
--
newtype Second a = Second a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via MetaUnit Second a

instance Fractional a => ConvFactor Second a where
  factorFrom = 1

instance IsUnit Second where
  type DimOf Second = Time
  type StdUnitOf Second = Second

instance ShowUnit Second where
  type ShowUnitType Second = Text "s"
  prettyUnit = "s"
  showUnit = "Second"

-- | A quantity in Kelvin, denotated @K@ in SI.
--
-- This is the base unit of the thermodynamic temperature dimension in the SI
-- system.
--
newtype Kelvin a = Kelvin a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via MetaUnit Kelvin a


instance Fractional a => ConvFactor Kelvin a where
  factorFrom = 1

instance IsUnit Kelvin where
  type DimOf Kelvin = Temperature
  type StdUnitOf Kelvin = Kelvin

instance ShowUnit Kelvin where
  type ShowUnitType Kelvin = Text "K"
  showUnit = "K"