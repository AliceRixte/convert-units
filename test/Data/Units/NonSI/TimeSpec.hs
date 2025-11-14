module Data.Units.NonSI.TimeSpec where

import Test.Hspec

import Data.Units.NonSI.Time
import Data.Units.Core.ConvertProp

spec :: Spec
spec = do
  describe "Angle" $ do
    toFromSpec @Minute @Double
    toFromSpec @Hour   @Double
    fromToAssert @Double (Hour 1) (Minute 60)
