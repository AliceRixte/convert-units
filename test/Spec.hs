{-# LANGUAGE TypeOperators #-}

module Main
  ( main
  ) where

import Test.Hspec
import Test.QuickCheck


import Pandia.Units

approxEq :: a -> a -> Bool
approxEq a b = abs (a - b) < 1e-12

fromToSelf1 :: Convertor f a -> a -> a
fromToSelf1 f = fromSI' (coerceTo f). toSI' (coerceFrom f)

fromToSelf2 :: Convertor f a -> a -> a
fromToSelf2 f =  toSI' (coerceFrom f) . fromSI' (coerceTo f)

fromToSelf :: Convertor f a -> a -> Bool
fromToSelf f a = fromToSelf1 f a `approxEq` a && fromToSelf2 f a `approxEq` a



sameFunc :: (a -> a) -> (a -> a) -> a ->  Bool
sameFunc f g a = f a `approxEq` g a

-- propConvSpec ::
--   Convertor f a -> Convertor g a -> (a -> a) -> (a -> a) -> Bool
-- propConvSpec f g specfg specgf =
--   sameFunc (coerceFrom f ~~> coerceTo g) specfg
--   && sameFunc (coerceFrom g ~~> coerceTo f) specgf


type Kmmph = Kilo Meter -/- Hour
type Mps = Meter -/- Second


kmphTomps :: a -> a
kmphTomps = (/3.6)

mpsTokmph :: a -> a
mpsTokmph = (*3.6)


main :: IO ()
main = hspec $ do
  describe "~~>" $ do
    it "km/h <~> m/s" $ property $
      propConvSpec (kilo meter -/- hour)  (meter -/- second) kmphTomps mpsTokmph

  describe "angles" $ do
   it "self radians" $ property $
    fromToSelf radian