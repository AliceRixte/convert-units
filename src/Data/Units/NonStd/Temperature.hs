{-# LANGUAGE TemplateHaskell #-}
module Data.Units.NonStd.Temperature
  ( module Data.Units.NonStd.Temperature
  ) where

import Data.Units.Base
import Data.Units.SI


-- | Thermodynamic temperature in Celsius degrees
--
$(mkUnitNoFactor "Celsius" "°C" ''Temperature)

instance Fractional a => ConversionFactor Celsius a where
  factor = 1
  {-# INLINE factor #-}

instance Fractional a => ConvertibleUnit Celsius a where
  toNormalUnit (Celsius x) = Kelvin (x + 273.15)
  {-# INLINE toNormalUnit #-}

  fromNormalUnit (Kelvin x) = Celsius (x - 273.15)
  {-# INLINE fromNormalUnit #-}

-- | Thermodynamic temperature in Fahrenheit degrees
--
$(mkUnitNoFactor "Fahrenheit" "°F" ''Temperature)

instance Fractional a => ConversionFactor Fahrenheit a where
  factor = 5 / 9
  {-# INLINE factor #-}

instance Fractional a => ConvertibleUnit Fahrenheit a where
  toNormalUnit (Fahrenheit x) = Kelvin ((x + 459.67) * 5 / 9)
  {-# INLINE toNormalUnit #-}

  fromNormalUnit (Kelvin x) = Fahrenheit (x * 9 / 5 - 459.67)
  {-# INLINE fromNormalUnit #-}