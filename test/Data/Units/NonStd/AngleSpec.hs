module Data.Units.NonStd.AngleSpec where

import Test.Hspec

import Data.Units.Base
import qualified Data.Units.SI.Angle as SI
import qualified Data.Units.SI.NonStd.Angle as SI
import qualified Data.Units.AngleSI.Angle as A
import qualified Data.Units.AngleSI.NonStd.Angle as A

import Data.Units.Base.ConvertProp

spec :: Spec
spec = do
  describe "Angle" $ do
    toFromSpec @SI.Radian   @Double
    toFromSpec @SI.Degree   @Double
    toFromSpec @SI.Turn     @Double
    toFromSpec @SI.Gradian  @Double
    toFromSpec @A.Radian    @Double
    toFromSpec @A.Degree    @Double
    toFromSpec @A.Turn      @Double
    toFromSpec @A.Gradian   @Double
    fromToAssert @Double (SI.Radian pi)     (SI.Degree 180)
    fromToAssert @Double (SI.Degree 90)     (SI.Gradian 100)
    fromToAssert @Double (SI.Turn (1 / 4))  (SI.Degree 90)
    fromToAssert @Double (SI.Radian (pi/4)) (SI.Turn (1 / 8))
    fromToAssert @Double (A.Radian pi)     (A.Degree 180)
    fromToAssert @Double (A.Degree 90)     (A.Gradian 100)
    fromToAssert @Double (A.Turn (1 / 4))  (A.Degree 90)
    fromToAssert @Double (A.Radian (pi/4)) (A.Turn (1 / 8))
