module Pandia.Units.International
  ( module Pandia.Units.International
  ) where

import Pandia.Units.Convertor
import Pandia.Units.Prefix

newtype Meter a = Meter a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ConvertorClass Meter a

meter :: Convertor Meter a
meter = convertor
{-# INLINE meter #-}


newtype Second a = Second a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ConvertorClass Second a

second :: Convertor Second a
second = convertor
{-# INLINE second #-}



newtype Frame a = Frame a deriving (Show, Eq, Num)
type FPS = Frame -/- Second
instance ConvertorClass Frame a
frame = convertor :: Convertor Frame a
fps = frame `per` second
decaRate = 2 :: (Deca Frame -/- Second) Int

bolub = (deca frame `per` second ~> fps) decaRate