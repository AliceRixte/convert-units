module Data.Units.NonSI.Temperature
  ( Data.Units.SI.Derived.Celsius (..)
  , Fahrenheit(..)
  ) where

import Data.Units.Core
import Data.Units.SI
import Data.Units.SI.Derived


-- | Thermodynamic temperature in Fahrenheit degrees.
$(mkUnitNoFactor "Fahrenheit" "°F" ''Temperature)

instance Fractional a => ConversionFactor Fahrenheit a where
  factor = 5 / 9
  {-# INLINE factor #-}

instance Fractional a => ConvertibleUnit Fahrenheit a where
  toBaseUnit (Qty x) = Qty ((x + 459.67) * 5 / 9)
  {-# INLINE toBaseUnit #-}

  fromBaseUnit (Qty x) = Qty (x * 9 / 5 - 459.67)
  {-# INLINE fromBaseUnit #-}
