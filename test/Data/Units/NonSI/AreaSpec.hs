module Data.Units.NonSI.AreaSpec where

import Test.Hspec

import Data.Units.Core.ConvertProp
import Data.Units.Core.System
import Data.Units.Imperial.Area
import Data.Units.SI

type SquareMeter = (Meter .^+ 2) Double

spec :: Spec
spec = do
  describe "Area" $ do
    toFromSpec @Acre    @Double
    fromToAssert @Double (1 :: SquareMeter) (Acre $ 1 / ((1200 / 3937)^2 * 43560))
    fromToAssert @Double (Acre 1) ((1200 / 3937)^2 * 43560 :: SquareMeter)

    toFromSpec @Perch    @Double
    fromToAssert @Double (1 :: SquareMeter) (Perch $ 1 / ((1200 / 3937)^2 * 1089 / 4))
    fromToAssert @Double (Perch 1) ((1200 / 3937)^2 * 1089 / 4 :: SquareMeter)

    toFromSpec @Rood    @Double
    fromToAssert @Double (1 :: SquareMeter) (Rood $ 1 / ((1200 / 3937)^2 * 10890))
    fromToAssert @Double (Rood 1) ((1200 / 3937)^2 * 10890 :: SquareMeter)
