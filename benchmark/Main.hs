{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds#-}
{-# LANGUAGE TypeOperators#-}
module Main (main) where

import Criterion.Main

import Pandia.Units

mkBench f n = bench (show n) $ nf f n

iterations :: [Int]
iterations = [10, 100, 1000]


kmphTomps :: Int -> Double
kmphTomps n =  iterate (kilo meters -/- hour ~> meters -/- second) (5 :: Double) !! n

fasterConv :: Int -> Double
fasterConv n =  iterate (*3.6) (5 :: Double) !! n

main :: IO()
main = do
  print  "xouxou"
  defaultMain [
    bgroup "~>" [
        bgroup "kmph ~> mps" (fmap (mkBench kmphTomps) iterations)
      , bgroup "mult" (fmap (mkBench fasterConv) iterations)
      ]
    ]