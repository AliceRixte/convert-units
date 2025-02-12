module Pandia.Units.NonStd.Temperature
  ( module Pandia.Units.NonStd.Temperature
  ) where

import Pandia.Units.Core

import Data.Coerce

newtype Celsius a = Celsius a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance HasDim syst Celsius where
  type DimOf syst Celsius = DimTh syst (Pos 1)

celsius :: ConvertorClass Celsius cd p a => Convertor Celsius cd p a
celsius = convertor
{-# INLINE celsius #-}

instance Fractional a => ConvertorClass Celsius 'ToDimSys 'False a where
  convertor _ x = x + 273.15
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Celsius 'ToDimSys 'True a where
  convertor _ _ = 1
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Celsius 'FromDimSys 'False a where
  convertor _ x = x - 273.15
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Celsius 'FromDimSys 'True a where
  convertor _ _ = 1
  {-# INLINE convertor #-}




