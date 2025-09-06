module Data.Units.SI.PrefixSpec (spec) where

import Test.Hspec

import Data.Units

import Data.Units.BaseProp

spec :: Spec
spec = do
  describe "Units" $ do
    sameDimSpec @Meter @Double
  describe "Prefix" $ do
    describe "Quecto" $ do
      toFromSpec @(Quecto Meter) @Double
      toFromSpec @(Quecto Meter .^- 1) @Double
    describe "Ronto" $ do
      toFromSpec @(Ronto Meter) @Double
      toFromSpec @(Ronto Meter .^- 1) @Double
    describe "Yocto" $ do
      toFromSpec @(Yocto Meter) @Double
      toFromSpec @(Yocto Meter .^- 1) @Double
    describe "Zepto" $ do
      toFromSpec @(Zepto Meter) @Double
      toFromSpec @(Zepto Meter .^- 1) @Double
    describe "Atto" $ do
      toFromSpec @(Atto Meter) @Double
      toFromSpec @(Atto Meter .^- 1) @Double
    describe "Femto" $ do
      toFromSpec @(Femto Meter) @Double
      toFromSpec @(Femto Meter .^- 1) @Double
    describe "Pico" $ do
      toFromSpec @(Pico Meter) @Double
      toFromSpec @(Pico Meter .^- 1) @Double
    describe "Nano" $ do
      toFromSpec @(Nano Meter) @Double
      toFromSpec @(Nano Meter .^- 1) @Double
    describe "Micro" $ do
      toFromSpec @(Micro Meter) @Double
      toFromSpec @(Micro Meter .^- 1) @Double
    describe "Milli" $ do
      toFromSpec @(Milli Meter) @Double
      toFromSpec @(Milli Meter .^- 1) @Double
    describe "Centi" $ do
      toFromSpec @(Centi Meter) @Double
      toFromSpec @(Centi Meter .^- 1) @Double
    describe "Deci" $ do
      toFromSpec @(Deci Meter) @Double
      toFromSpec @(Deci Meter .^- 1) @Double
    describe "Deca" $ do
      toFromSpec @(Deca Meter) @Double
      toFromSpec @(Deca Meter .^- 1) @Double
    describe "Hecto" $ do
      toFromSpec @(Hecto Meter) @Double
      toFromSpec @(Hecto Meter .^- 1) @Double
    describe "Kilo" $ do
      fromToSpec @(Kilo Meter) @(Milli Meter) @Double
      fromToSpec' @(Kilo Meter) @(Milli Meter) @Double
      toFromSpec @(Kilo Meter) @Double
      toFromSpec @(Kilo Meter .^- 1) @Double
    describe "Mega" $ do
      toFromSpec @(Mega Meter) @Double
      toFromSpec @(Mega Meter .^- 1) @Double
    describe "Giga" $ do
      toFromSpec @(Giga Meter) @Double
      toFromSpec @(Giga Meter .^- 1) @Double
    describe "Tera" $ do
      toFromSpec @(Tera Meter) @Double
      toFromSpec @(Tera Meter .^- 1) @Double
    describe "Peta" $ do
      toFromSpec @(Peta Meter) @Double
      toFromSpec @(Peta Meter .^- 1) @Double
    describe "Exa" $ do
      toFromSpec @(Exa Meter) @Double
      toFromSpec @(Exa Meter .^- 1) @Double
    describe "Zetta" $ do
      toFromSpec @(Zetta Meter) @Double
      toFromSpec @(Zetta Meter .^- 1) @Double
    describe "Yotta" $ do
      toFromSpec @(Yotta Meter) @Double
      toFromSpec @(Yotta Meter .^- 1) @Double
    describe "Ronna" $ do
      toFromSpec @(Ronna Meter) @Double
      toFromSpec @(Ronna Meter .^- 1) @Double
    describe "Quecca" $ do
      toFromSpec @(Quecca Meter) @Double
      toFromSpec @(Quecca Meter .^- 1) @Double
