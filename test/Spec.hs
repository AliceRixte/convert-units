{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main
  ( main
  ) where


import Test.Hspec
import Test.QuickCheck

import Pandia.Units

approxEq :: (Ord a, Fractional a) => a -> a -> Bool
approxEq a b = abs (a - b) < 1e-12



fromToSelf1 :: Convertor u cd p a -> a -> a
fromToSelf1 f = fromSI' (coerceFrom f) . toSI' (coerceTo f)

fromToSelf2 :: Convertor u cd p a -> a -> a
fromToSelf2 f =  toSI' (coerceTo f) . fromSI' (coerceFrom f)

fromToSelf :: (Ord a, Fractional a) => Convertor u 'FromSI 'False a -> a -> Bool
fromToSelf f a = fromToSelf1 f a `approxEq` a && fromToSelf2 f a `approxEq` a

fromToSelf' :: Convertor u 'FromSI 'False Double -> Double -> Bool
fromToSelf' = fromToSelf




sameFunc :: (Ord a, Fractional a) => (a -> a) -> (a -> a) -> a ->  Bool
sameFunc f g a = f a `approxEq` g a

propConvSpec :: (Ord a, Fractional a, SameDim u v) =>
  Convertor u 'ToSI 'False a -> Convertor v 'FromSI 'False a
    -> (a -> a) -> (a -> a) -> a -> Bool
propConvSpec f g specfg specgf a =
  sameFunc (coerceTo f  ~~> coerceFrom g) specfg a
  -- && sameFunc (fromToNoCheck' (coerceTo g) (coerceFrom f)) specgf a


propConvSpec' :: SameDim u v =>
  Convertor u 'ToSI 'False Double -> Convertor v 'FromSI 'False Double
  -> (Double -> Double) -> (Double -> Double) -> Double -> Bool
propConvSpec'  = propConvSpec


type Kmmph = Kilo Meter -/- Hour
type Mps = Meter -/- Second


kmphTomps :: (Ord a, Fractional a) =>  a -> a
kmphTomps = (/3.6)

mpsTokmph :: (Ord a, Fractional a) => a -> a
mpsTokmph = (*3.6)


main :: IO ()
main = hspec $ do
  describe "~~>" $ do
    it "km/h <~> m/s" $ property $
      propConvSpec' (kilo meter -/- hour)  (meter -/- second) kmphTomps mpsTokmph

  describe "angles" $ do
   it "self radians" $ property $
    fromToSelf' radian