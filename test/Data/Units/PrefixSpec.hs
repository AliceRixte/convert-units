module Data.Units.PrefixSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Units.Common

import Data.Units.Prefix
import Data.Units

spec :: Spec
spec =
  describe "Prefix" $ do
    describe "Kilo" $ do
      fromToSpec @(Kilo Meter) @(Milli Meter) @Double
      fromToSpec' @(Kilo Meter) @(Milli Meter) @Double
      toFromSpec @(Kilo Meter) @Double
      toFromSpec @(Kilo Meter -^~ 1) @Double
      mulDiffDimSpec @(Kilo Meter -^~ 1) @(Milli Second) @Double
      divDiffDimSpec @(Kilo Meter) @(Milli Second) @Double