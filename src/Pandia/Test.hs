{-# LANGUAGE TypeOperators #-}

module Pandia.Test
  ( module Pandia.Test
  ) where

import Test.Hspec
import Test.QuickCheck


import Pandia.Units

approxEq :: (Ord a, Fractional a) => a -> a -> Bool
approxEq a b = abs (a - b) < 1e-12



fromToSelf1 :: Convertor u cd p a -> a -> a
fromToSelf1 f = fromSys' (coerceFrom f) . toSys' (coerceTo f)

fromToSelf2 :: Convertor u cd p a -> a -> a
fromToSelf2 f =  toSys' (coerceTo f) . fromSys' (coerceFrom f)

fromToSelf :: (Ord a, Fractional a) => Convertor u 'FromDimSys 'False a -> a -> Bool
fromToSelf f a = fromToSelf1 f a `approxEq` a && fromToSelf2 f a `approxEq` a

fromToSelf' :: Convertor u 'FromDimSys 'False Double -> Double -> Bool
fromToSelf' = fromToSelf




sameFunc :: (Ord a, Fractional a) => (a -> a) -> (a -> a) -> a ->  Bool
sameFunc f g a = f a `approxEq` g a

propConvSpec :: (Ord a, Fractional a, SameDim SI u v) =>
  Convertor u 'ToDimSys 'False a -> Convertor v 'FromDimSys 'False a
    -> (a -> a) -> (a -> a) -> a -> Bool
propConvSpec f g specfg specgf a =
  sameFunc (coerceTo f  ~~> coerceFrom g) specfg a
  -- && sameFunc (fromToNoCheck' (coerceTo g) (coerceFrom f)) specgf a


propConvSpec' :: SameDim SI u v =>
  Convertor u 'ToDimSys 'False Double -> Convertor v 'FromDimSys 'False Double
  -> (Double -> Double) -> (Double -> Double) -> Double -> Bool
propConvSpec'  = propConvSpec


type Kmmph = Kilo Meter -/- Hour
type Mps = Meter -/- Second


kmphTomps :: (Ord a, Fractional a) =>  a -> a
kmphTomps = (/3.6)

mpsTokmph :: (Ord a, Fractional a) => a -> a
mpsTokmph = (*3.6)

kcToKk :: (Ord a, Fractional a) => a -> a
kcToKk a =  a + 273.15 / 1000

kkToKc :: (Ord a, Fractional a) => a -> a
kkToKc a = a - 273.15 / 1000

main :: IO ()
main = hspec $ do
  describe "~~>" $ do
    it "km/h <~> m/s" $ property $
      propConvSpec' (kilo meter -/- hour) (meter -/- second) kmphTomps mpsTokmph
    it "k°K <~> k°C" $ property $
      propConvSpec' (kilo kelvin)  (kilo celsius) kkToKc kcToKk

  describe "angles" $ do
   it "self radians" $ property $
    fromToSelf' radian