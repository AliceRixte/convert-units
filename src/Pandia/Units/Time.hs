
module Pandia.Units.Time
  ( module Pandia.Units.Time
  ) where

import Pandia.Units.Convertor


newtype Second a = Second a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

-- instance ConvertType Second a where
--   convertType _ = second
--   {-# INLINE convertType #-}

second :: Convertor Second a
second = convertor

seconds :: Convertor Second a
seconds = convertor

instance ConvertType Second a where
  convertor _ = id
  {-# INLINE convertor #-}

newtype Hour a = Hour a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

hour :: ConvertType Hour a => Convertor Hour a
hour = convertor

hours :: ConvertType Hour a => Convertor Hour a
hours = convertor

instance Num a => ConvertType Hour (From a) where
  convertor _ x = x * 3600
  {-# INLINE convertor #-}

instance Fractional a => ConvertType Hour (To a) where
  convertor _ x = x / 3600
  {-# INLINE convertor #-}

instance Fractional a => ConvertType Hour (Per (From a)) where
  convertor _ x = x / 3600
  {-# INLINE convertor #-}

instance Num a => ConvertType Hour (Per (To a)) where
  convertor _ x = x * 3600
  {-# INLINE convertor #-}







-- -- instance ConvHour a => ConvertType Hour a where
-- --   convertType _ = hour

-- class ConvHour a where
--   hour :: a -> a

-- hours :: ConvHour a => a -> a
-- hours = hour

-- instance Num a => ConvHour (From a) where
--   hour x = x * 3600
--   {-# INLINE hour #-}

-- instance Fractional a => ConvHour (To a) where
--   hour x = x / 3600
--   {-# INLINE hour #-}

-- instance Fractional a => ConvHour (Per (From a)) where
--   hour x = x / 3600
--   {-# INLINE hour #-}

-- instance Num a => ConvHour (Per (To a)) where
--   hour x = x * 3600
--   {-# INLINE hour #-}



-- newtype Day a = Day a
--   deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
--           , Bounded, Enum, Semigroup, Monoid)

-- -- instance ConvDay a => ConvertType Day a where
-- --   convertType _ = day

-- class ConvDay a where
--   day :: a -> a

-- days :: ConvDay a => a -> a
-- days = day

-- instance Num a => ConvDay (From a) where
--   day x = x * 86400
--   {-# INLINE day #-}

-- instance Fractional a => ConvDay (To a) where
--   day x = x / 86400
--   {-# INLINE day #-}

-- instance Fractional a => ConvDay (Per (From a)) where
--   day x = x / 86400
--   {-# INLINE day #-}

-- instance Num a => ConvDay (Per (To a)) where
--   day x = x * 86400
--   {-# INLINE day #-}



