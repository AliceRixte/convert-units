module Data.Units.PrefixSpec (spec) where

import Test.Hspec

import Data.Units

import Data.Units.BaseProp

spec :: Spec
spec = do
  describe "Units" $ do
    sameDimSpec @Meter @Double
  describe "Prefix" $ do
    describe "Kilo" $ do
      fromToSpec @(Kilo Meter) @(Milli Meter) @Double
      fromToSpec' @(Kilo Meter) @(Milli Meter) @Double
      toFromSpec @(Kilo Meter) @Double
      toFromSpec @(Kilo Meter .^- 1) @Double
      -- mulDiffDimSpec @(Kilo Meter .^- 1) @(Milli Second) @Double
      -- divDiffDimSpec @(Kilo Meter) @(Milli Second) @Double