module Data.Units.NonStd.LengthSpec where

import Test.Hspec

import Data.Units.NonStd.Length
import Data.Units.SI

import Data.Units.Base.ConvertProp

spec :: Spec
spec = do
  describe "Length" $ do
    toFromSpec @Foot    @Double
    fromToAssert @Double (Meter 1) (Foot 3.2808333333333333)
    fromToAssert @Double (Foot 1) (Meter 0.3048006096012192)
