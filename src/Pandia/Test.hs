module Pandia.Test
  ( module Pandia.Test
  ) where

import Test.Hspec
import Test.QuickCheck


import Pandia.Units

-- approxEq :: Double -> Double -> Bool
-- approxEq a b = abs (a - b) < 1e-12

-- fromToSelf1 :: Convertor f a -> a -> a
-- fromToSelf1 f = fromSI' (coerceTo f). toSI' (coerceFrom f)

-- fromToSelf2 :: Convertor f a -> a -> a
-- fromToSelf2 f =  toSI' (coerceFrom f) . fromSI' (coerceTo f)

-- fromToSelf :: Convertor f Double -> Double -> Bool
-- fromToSelf f a = fromToSelf1 f a `approxEq` a && fromToSelf2 f a `approxEq` a

-- main' :: IO ()
-- main' = hspec $ do
--   describe "angles" $ do
--    it "self radians" $ property $
--     fromToSelf radian