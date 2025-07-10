module Data.Units.SI
  ( module Data.Units.SI
  ) where

import GHC.TypeLits

import Data.Units.Base



------------------------------------ Length ------------------------------------

-- | The length dimension, denotated @L@ in SI.
--
--  This may contain a length quantity with unspecified unit.
--
newtype Length a = Length a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)

type instance DimId Length = 300
type instance ShowDim Length = Text "L"

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

------------------------------------- Time -------------------------------------

-- | The time dimension, denotated @T@ in SI.
--
--  This may contain a length quantity with unspecified unit.
--
newtype Time a = Time a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)

type instance DimId Time = 400

type instance ShowDim Time = Text "T"

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

-------------------------- Thermodynamic Temperature ---------------------------

-- | The thermodynamic temperature dimension, denotated @Θ@ in SI.
--
--  This may contain a temperature quantity with unspecified unit.
--
newtype Temperature a = Temperature a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)

type instance DimId Temperature = 500
type instance ShowDim Temperature = Text "Θ"

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