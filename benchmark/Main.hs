{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds#-}
{-# LANGUAGE TypeOperators#-}

{-# OPTIONS_GHC -ddump-to-file #-}

module Main (main) where

import Criterion.Main

import Data.Units
import Data.Units.NonStd.Time

-- mkBench f n = bench (show n) $ nf f n

-- iterations :: [Int]
-- iterations = [10, 100, 1000]


-- kmphTomps :: Int -> Double
-- kmphTomps n =  iterate (fromToCoerce @(Kilo Meter ./. Hour) @(Meter ./. Second)) 5 !! n

-- multConv :: Int -> Double
-- multConv n =  iterate (*3.6) (5 :: Double) !! n


u :: (.^-) Second 1 Double
u = fromTo (1415 :: Hertz Double)

main :: IO()
main = print u
-- main = do
--   defaultMain [
--     bgroup "~>" [
--         bgroup "kmph ~> mps" (fmap (mkBench kmphTomps) iterations)
--       , bgroup "mult" (fmap (mkBench multConv) iterations)
--       ]
--     ]