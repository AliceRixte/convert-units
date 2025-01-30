module Pandia.Units.Length
  ( module Pandia.Units.Length
  ) where

import Pandia.Units.Convertor


newtype Meter a = Meter a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

meter :: Convertor Meter a
meter = convertor

meters :: Convertor Meter a
meters = convertor

instance ConvertorClass Meter a where
  convertor _ = id
  {-# INLINE convertor #-}