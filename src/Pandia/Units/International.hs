module Pandia.Units.International
  ( module Pandia.Units.International
  ) where

import Pandia.Units.Convertor
import Pandia.Units.Dimension

newtype Meter a = Meter a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ToDimension Meter where
  type ToDim Meter = 'Dimension 1 0 0 0 0 0 0

instance ConvertorClass Meter a

meter :: Convertor Meter a
meter = convertor
{-# INLINE meter #-}


newtype Second a = Second a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ToDimension Second where
  type ToDim Second = 'Dimension 0 1 0 0 0 0 0

instance ConvertorClass Second a

second :: Convertor Second a
second = convertor
{-# INLINE second #-}


newtype Newton a = Newton a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ConvertorClass Newton a

newton :: Convertor Newton a
newton = convertor
{-# INLINE newton #-}

newtype Kelvin a = Kelvin a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ConvertorClass Kelvin a

kelvin :: Convertor Kelvin a
kelvin = convertor
{-# INLINE kelvin #-}

newtype Celsius a = Celsius a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance Fractional a => ConvertorClass Celsius (From a) where
  convertor _ x = x + 273.15
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Celsius (To a) where
  convertor _ x = x - 273.15
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Celsius (Per a) where
  convertor _ = id
  {-# INLINE convertor #-}

celsius :: ConvertorClass Celsius a => Convertor Celsius a
celsius = convertor
{-# INLINE celsius #-}


