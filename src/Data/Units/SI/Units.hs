module Data.Units.SI.Units
  ( module Data.Units.SI.Units
  ) where

import GHC.TypeLits

import Data.Units.Base

-- | The length dimension, denotated @L@ in SI.
--
--  This may contain a length quantity with unspecified unit.
--
newtype Length a = Length a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)

type instance DimId Length = 300
type instance ShowDim Length = Text "L"

instance IsDim Length where
  type StdUnitOf' Length = Meter


-- | A quantity in meters, denotated @m@ in SI.
--
-- This is the base unit of the length dimension in the SI system.
--
newtype Meter a = Meter a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)

instance Fractional a => ConvFactor Meter a where
  factorFrom = 1

instance IsUnit Meter where
  type DimOf Meter = Length

instance ShowUnit Meter where
  type ShowUnitType Meter = Text "m"
  prettyUnit = "m"
  showUnit = "Meter"

--------------------------------------------------------------------------------

-- | The time dimension, denotated @T@ in SI.
--
--  This may contain a length quantity with unspecified unit.
--
newtype Time a = Time a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)

type instance DimId Time = 400

type instance ShowDim Time = Text "T"

instance IsDim Time where
  type StdUnitOf' Time = Second

-- | A quantity in seconds, denotated @s@ in SI.
--
-- This is the base unit of the time dimension in the SI system.
--
newtype Second a = Second a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)

instance Fractional a => ConvFactor Second a where
  factorFrom = 1

instance IsUnit Second where
  type DimOf Second = Time

instance ShowUnit Second where
  type ShowUnitType Second = Text "s"
  prettyUnit = "s"
  showUnit = "Second"

--------------------------------------------------------------------------------

-- | The thermodynamic temperature dimension, denotated @Θ@ in SI.
--
--  This may contain a temperature quantity with unspecified unit.
--
newtype Temperature a = Temperature a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)

type instance DimId Temperature = 500
type instance ShowDim Temperature = Text "Θ"

instance IsDim Temperature where
  type StdUnitOf' Temperature = Kelvin

-- | A quantity in Kelvin, denotated @K@ in SI.
--
-- This is the base unit of the thermodynamic temperature dimension in the SI
-- system.
--
newtype Kelvin a = Kelvin a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)


instance Fractional a => ConvFactor Kelvin a where
  factorFrom = 1

instance IsUnit Kelvin where
  type DimOf Kelvin = Temperature

instance ShowUnit Kelvin where
  type ShowUnitType Kelvin = Text "K"
  showUnit = "K"