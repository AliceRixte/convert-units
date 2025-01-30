
module Pandia.Units.Time
  ( module Pandia.Units.Time
  ) where

import Data.Kind


import Pandia.Units.Convert


newtype Second a = Second a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ConvertType Second a where
  convertType _ = second
  {-# INLINE convertType #-}

second :: a -> a
second = id

seconds :: a -> a
seconds = id


newtype Hour a = Hour a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ConvHour a => ConvertType Hour a where
  convertType _ = hour

class ConvHour a where
  hour :: a -> a

hours :: ConvHour a => a -> a
hours = hour

instance Num a => ConvHour (From a) where
  hour x = x * 3600
  {-# INLINE hour #-}

instance Fractional a => ConvHour (To a) where
  hour x = x / 3600
  {-# INLINE hour #-}

instance Fractional a => ConvHour (Per (From a)) where
  hour x = x / 3600
  {-# INLINE hour #-}

instance Num a => ConvHour (Per (To a)) where
  hour x = x * 3600
  {-# INLINE hour #-}



newtype Day a = Day a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ConvDay a => ConvertType Day a where
  convertType _ = day

class ConvDay a where
  day :: a -> a

days :: ConvDay a => a -> a
days = day

instance Num a => ConvDay (From a) where
  day x = x * 86400
  {-# INLINE day #-}

instance Fractional a => ConvDay (To a) where
  day x = x / 86400
  {-# INLINE day #-}

instance Fractional a => ConvDay (Per (From a)) where
  day x = x / 86400
  {-# INLINE day #-}

instance Num a => ConvDay (Per (To a)) where
  day x = x * 86400
  {-# INLINE day #-}



