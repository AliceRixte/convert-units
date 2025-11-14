module Data.Units.NonSI.TemperatureSpec where

import Test.Hspec

import Data.Units.NonSI.Temperature
import Data.Units.SI

import Data.Units.Core.ConvertProp

spec :: Spec
spec = do
  describe "Temperature" $ do
    toFromSpec @Celsius    @Double
    toFromSpec @Fahrenheit @Double
    toFromSpec @Kelvin     @Double
    fromToAssert @Double (Celsius 0)    (Kelvin 273.15)
    fromToAssert @Double (Fahrenheit 32) (Celsius 0)
    fromToAssert @Double (Fahrenheit 212) (Kelvin 373.15)
