module Main
  ( main
  ) where

import Test.Hspec
import Test.QuickCheck


import Pandia.Units

approxEq :: Double -> Double -> Bool
approxEq a b = abs (a - b) < 1e-12

fromToSelf1 :: Convertor f a -> a -> a
fromToSelf1 f = fromSI' (coerceTo f). toSI' (coerceFrom f)

fromToSelf2 :: Convertor f a -> a -> a
fromToSelf2 f =  toSI' (coerceFrom f) . fromSI' (coerceTo f)

fromToSelf :: Convertor f Double -> Double -> Bool
fromToSelf f a = fromToSelf1 f a `approxEq` a && fromToSelf2 f a `approxEq` a


propConvSpec :: (Double -> Double) -> (Double -> Double) -> Double ->  Bool
propConvSpec f g a = f a `approxEq` g a


kmphTomps :: Double -> Double
kmphTomps = (/3.6)

mpsTokmph :: Double -> Double
mpsTokmph = (*3.6)


main :: IO ()
main = hspec $ do
  describe "~~>" $ do
    it "km/h ~~> m/s" $ property $
      propConvSpec kmphTomps (kilo meter -/- hour ~~> meter -/- second)
    it "m/s  ~~> km/h" $ property $
      propConvSpec mpsTokmph (meter -/- second ~~> kilo meter -/- hour)

  describe "angles" $ do
   it "self radians" $ property $
    fromToSelf radian