{-# LANGUAGE TemplateHaskell #-}
module Data.Units.NonStd.Temperature
  ( module Data.Units.NonStd.Temperature
  ) where

import Data.Units.Base
import Data.Units.SI


-- | Thermodynamic temperature in Celsius degrees
--
$(mkUnit "Celsius" "°C" ''Temperature 1)

instance Fractional a => From Celsius a where
  from (Celsius x) = Kelvin (x + 273.15)

instance Fractional a => To Celsius a where
  to (Kelvin x) = Celsius (x - 273.15)
