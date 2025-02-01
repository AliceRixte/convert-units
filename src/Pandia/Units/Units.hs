module Pandia.Units.Units
  ( module Pandia.Units.Units
  ) where

import Pandia.Units.Convertor
import Pandia.Units.Dimension

----------------------------------- Length -----------------------------------


------------------------------------ Mass ------------------------------------

newtype Ton a = Ton a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ToDimension Ton where
  type ToDim Ton = DimMass

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

newtype Hour a = Hour a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ToDimension Hour where
  type ToDim Hour = DimTime

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



