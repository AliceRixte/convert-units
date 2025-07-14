module Data.Units.NonStd.TimeSpec where

import Test.Hspec

import Data.Units.NonStd.Time
import Data.Units.Base.ConvertProp

spec :: Spec
spec = do
  describe "Angle" $ do
    toFromSpec @Minute @Double
    toFromSpec @Hour   @Double
    fromToAssert @Double (Hour 1) (Minute 60)
