module Data.Units.NonStd.Temperature
  ( module Data.Units.NonStd.Temperature
  ) where

import Data.Units.Base
import Data.Units.SI

newtype Celsius a = Celsius a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via (MetaUnit Celsius a)

instance IsUnit Celsius where
  type StdUnitOf Celsius = Kelvin
  type DimOf Celsius = Temperature

instance ShowUnit Celsius where
  type ShowUnitType Celsius = Text "°C"
  showUnit = "Celsius"
  prettyUnit = "°C"

instance Fractional a => From Celsius a where
  from (Celsius x) = Kelvin (x + 273.15)

instance Fractional a => To Celsius a where
  to (Kelvin x) = Celsius (x - 273.15)

instance Fractional a => ConvFactor Celsius a where
  factorFrom = 1





