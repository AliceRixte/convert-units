module Data.Units.SI.SystemSpec (spec) where

import Test.Hspec

import Data.Units

import Data.Units.BaseProp

spec :: Spec
spec = do
  describe "Units" $ do
    sameDimSpec @Meter @Double
    toFromSpec @Gram @Double
    toFromSpec @Meter @Double
    toFromSpec @Second @Double
    toFromSpec @Ampere @Double
    toFromSpec @Kelvin @Double
    toFromSpec @Mole @Double
    toFromSpec @Candela @Double


