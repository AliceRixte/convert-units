module Pandia.Units.Length
  ( module Pandia.Units.Length
  ) where

import Pandia.Units.Convert

-- meter :: a -> a
-- meter = id

-- meters :: a -> a
-- meters = id

newtype Meter a = Meter a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

meter :: Convertor Meter a
meter = convertor

meters :: Convertor Meter a
meters = convertor

instance ConvertType Meter a where
  convertor _ = id
  {-# INLINE convertor #-}