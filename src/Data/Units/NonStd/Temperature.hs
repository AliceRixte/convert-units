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
  factorFrom = 1
  {-# INLINE factorFrom #-}

instance Fractional a => ConvertibleUnit Celsius a where
  toNormalUnit (Celsius x) = Kelvin (x + 273.15)
  {-# INLINE toNormalUnit #-}

  fromNormalUnit (Kelvin x) = Celsius (x - 273.15)
  {-# INLINE fromNormalUnit #-}
