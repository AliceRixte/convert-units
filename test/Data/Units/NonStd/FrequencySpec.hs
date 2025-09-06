module Data.Units.NonStd.FrequencySpec where

import Test.Hspec

import Data.Units.SI
import Data.Units.NonStd.Frequency

import Data.Units.Base.ConvertProp

spec :: Spec
spec = do
  describe "Frequency" $ do
    fromToAssert @Double (69 :: MidiPitch Double) (Hertz 440)
    it "decomposePitchCents, detune < 1" $ do

      decomposePitchCents (123.5 :: MidiPitch Double) `shouldBe` (123, 0.5)

