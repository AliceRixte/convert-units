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

instance Num a => ConvertorClass Ton (ToSI a) where
  convertor _ x = x * 1000
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Ton (Per (ToSI a)) where
  convertor _ x = x / 1000
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Ton (FromSI a) where
  convertor _ x = x / 1000
  {-# INLINE convertor #-}

instance Num a => ConvertorClass Ton (Per (FromSI a)) where
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

instance Num a => ConvertorClass Hour (ToSI a) where
  convertor _ x = x * 3600
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Hour (Per (ToSI a)) where
  convertor _ x = x / 3600
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Hour (FromSI a) where
  convertor _ x = x / 3600
  {-# INLINE convertor #-}

instance Num a => ConvertorClass Hour (Per (FromSI a)) where
  convertor _ x = x * 3600
  {-# INLINE convertor #-}

------------------------------ Electric current ------------------------------

------------------------- Thermodynamic temperature --------------------------

newtype Celsius a = Celsius a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance Fractional a => ConvertorClass Celsius (ToSI a) where
  convertor _ x = x + 273.15
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Celsius (FromSI a) where
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



