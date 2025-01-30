
module Pandia.Units.Time
  ( module Pandia.Units.Time
  ) where

import Pandia.Units.Convertor


-- newtype Second a = Second a
--   deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
--           , Bounded, Enum, Semigroup, Monoid)

-- second :: Convertor Second a
-- second = convertor

-- seconds :: Convertor Second a
-- seconds = convertor

-- instance ConvertorClass Second a where
--   convertor _ = id
--   {-# INLINE convertor #-}


-- type Hertz = NoUnit -/- Second

-- hertz :: Num a => Convertor Hertz a
-- hertz = nounit -/- second

-- newtype Hour a = Hour a
--   deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
--           , Bounded, Enum, Semigroup, Monoid)

-- hour :: ConvertorClass Hour a => Convertor Hour a
-- hour = convertor

-- hours :: ConvertorClass Hour a => Convertor Hour a
-- hours = convertor

-- instance Num a => ConvertorClass Hour (From a) where
--   convertor _ x = x * 3600
--   {-# INLINE convertor #-}

-- instance Fractional a => ConvertorClass Hour (To a) where
--   convertor _ x = x / 3600
--   {-# INLINE convertor #-}

-- instance Fractional a => ConvertorClass Hour (Per (From a)) where
--   convertor _ x = x / 3600
--   {-# INLINE convertor #-}

-- instance Num a => ConvertorClass Hour (Per (To a)) where
--   convertor _ x = x * 3600
--   {-# INLINE convertor #-}







-- -- instance ConvHour a => ConvertorClass Hour a where
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

-- -- instance ConvDay a => ConvertorClass Day a where
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



