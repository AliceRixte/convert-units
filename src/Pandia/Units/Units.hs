module Pandia.Units.Units
  ( module Pandia.Units.Units
  ) where

import Pandia.Units.Convertor
import Pandia.Units.Dimension

----------------------------------- Length -----------------------------------

newtype Meter a = Meter a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ToDimension Meter where
  type ToDim Meter = 'Dimension 1 0 0 0 0 0 0

instance ConvertorClass Meter a

meter :: Convertor Meter a
meter = convertor
{-# INLINE meter #-}

------------------------------------ Mass ------------------------------------

newtype Gram a = Gram a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ToDimension Gram where
  type ToDim Gram = 'Dimension 0 1 0 0 0 0 0

gram :: ConvertorClass Gram a => Convertor Gram a
gram = convertor
{-# INLINE gram #-}

--  This is not the identity, as SI uses kilograms as the base unit for mass.
instance Fractional a => ConvertorClass Gram (From a) where
  convertor _ x = x / 1000
  {-# INLINE convertor #-}

instance Num a => ConvertorClass Gram (Per (From a)) where
  convertor _ x = x * 1000
  {-# INLINE convertor #-}

instance Num a => ConvertorClass Gram (To a) where
  convertor _ x = x * 1000
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Gram (Per (To a)) where
  convertor _ x = x / 1000
  {-# INLINE convertor #-}

newtype Ton a = Ton a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

ton :: ConvertorClass Ton a => Convertor Ton a
ton = convertor
{-# INLINE ton #-}

instance Num a => ConvertorClass Ton (From a) where
  convertor _ x = x * 1000
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Ton (Per (From a)) where
  convertor _ x = x / 1000
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Ton (To a) where
  convertor _ x = x / 1000
  {-# INLINE convertor #-}

instance Num a => ConvertorClass Ton (Per (To a)) where
  convertor _ x = x * 1000
  {-# INLINE convertor #-}


------------------------------------ Time ------------------------------------

newtype Second a = Second a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ToDimension Second where
  type ToDim Second = 'Dimension 0 0 1 0 0 0 0

instance ConvertorClass Second a

second :: Convertor Second a
second = convertor
{-# INLINE second #-}

newtype Hour a = Hour a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ToDimension Hour where
  type ToDim Hour = 'Dimension 0 0 1 0 0 0 0

hour :: ConvertorClass Hour a => Convertor Hour a
hour = convertor
{-# INLINE hour #-}

instance Num a => ConvertorClass Hour (From a) where
  convertor _ x = x * 3600
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Hour (Per (From a)) where
  convertor _ x = x / 3600
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Hour (To a) where
  convertor _ x = x / 3600
  {-# INLINE convertor #-}

instance Num a => ConvertorClass Hour (Per (To a)) where
  convertor _ x = x * 3600
  {-# INLINE convertor #-}

------------------------------ Electric current ------------------------------

------------------------- Thermodynamic temperature --------------------------


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


---------------------------- Amount of substance -----------------------------

----------------------------- Luminous intensity -----------------------------


------------------------------- Compound units -------------------------------

newtype Newton a = Newton a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ConvertorClass Newton a

newton :: Convertor Newton a
newton = convertor
{-# INLINE newton #-}


