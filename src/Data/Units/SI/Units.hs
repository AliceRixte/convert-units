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

type instance DimOf Meter = Length

instance IsUnit Meter where
  type StdUnitOf Meter = Meter

instance ShowUnit Meter where
  type ShowUnitType Meter = Text "m"
  showUnit = "m"


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

type instance DimOf Second = Time

instance IsUnit Second where
  type StdUnitOf Second = Second

instance ShowUnit Second where
  type ShowUnitType Second = Text "s"
  showUnit = "s"

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

type instance DimOf Kelvin = Temperature

instance IsUnit Kelvin where
  type StdUnitOf Kelvin = Kelvin

instance ShowUnit Kelvin where
  type ShowUnitType Kelvin = Text "K"
  showUnit = "K"