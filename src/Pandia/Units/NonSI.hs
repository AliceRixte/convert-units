module Pandia.Units.NonSI
  ( module Pandia.Units.NonSI
  ) where

import Pandia.Units.Convertor
import Pandia.Units.Dimension
import Pandia.Units.SI
import Pandia.Units.Rel
import Pandia.Units.Unit

import Data.Coerce

----------------------------------- Length -----------------------------------


------------------------------------ Mass ------------------------------------


newtype Ton a = Ton a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance HasDim syst Ton where
  type DimOf syst Ton = DimM syst (Pos 1)

ton :: ConvertorClass Ton cd p a => Convertor Ton cd p a
ton = convertor
{-# INLINE ton #-}

-- instance Num a => ConvertorClass Ton 'ToDimSys 'False a where
--   convertor _ x = x * 1000
--   {-# INLINE convertor #-}

-- instance Fractional a => ConvertorClass Ton 'ToDimSys 'True a where
--   convertor _ x = x / 1000
--   {-# INLINE convertor #-}

-- instance Fractional a => ConvertorClass Ton 'FromDimSys 'False a where
--   convertor _ x = x / 1000
--   {-# INLINE convertor #-}

-- instance Num a => ConvertorClass Ton 'FromDimSys 'True a where
--   convertor _ x = x * 1000
--   {-# INLINE convertor #-}



------------------------------------ Time ------------------------------------





newtype Minute a = Minute a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance HasDim syst Minute where
  type DimOf syst Minute = DimT syst (Pos 1)

minute :: ConvertorClass Minute cd p a => Convertor Minute cd p a
minute = convertor
{-# INLINE minute #-}

instance Num a => ConvertorClass Minute 'ToDimSys 'False a where
  convertor _ x = x * 60
  {-# INLINE convertor #-}

instance Num a => ConvertorClass Minute 'ToDimSys 'True a where
  convertor _ _ = 60
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Minute 'FromDimSys 'False a where
  convertor _ x = x / 60
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Minute 'FromDimSys 'True a where
  convertor _ _ = 1 / 60
  {-# INLINE convertor #-}


newtype Hour a = Hour a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance HasDim syst Hour where
  type DimOf syst Hour = DimT syst (Pos 1)

hour :: ConvertorClass Hour cd p a => Convertor Hour cd p a
hour = convertor
{-# INLINE hour #-}

instance Num a => ConvertorClass Hour 'ToDimSys 'False a where
  convertor _ x = x * 3600
  {-# INLINE convertor #-}

instance Num a => ConvertorClass Hour 'ToDimSys 'True a where
  convertor _ _ = 3600
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Hour 'FromDimSys 'False a where
  convertor _ x = x / 3600
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Hour 'FromDimSys 'True a where
  convertor _ _ = 1 / 3600
  {-# INLINE convertor #-}

----------------------------------- Beats ------------------------------------

newtype Beat a = Beat a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance HasDim syst Beat where
  type DimOf syst Beat = DimT syst (Pos 1)

newtype Bpm a = Bpm a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

class BeatConvertor cd p a where
  beat :: Bpm a -> Convertor Beat cd p a

instance Fractional a => BeatConvertor 'ToDimSys 'False a where
  beat bpm  _ x = 60 * x / coerce bpm
  {-# INLINE beat #-}

instance Fractional a => BeatConvertor 'ToDimSys 'True a where
  beat bpm _ _ = 60 / coerce bpm
  {-# INLINE beat #-}

instance Fractional a => BeatConvertor 'FromDimSys 'False a where
  beat bpm _ x = coerce bpm * x / 60
  {-# INLINE beat #-}

instance Fractional a => BeatConvertor 'FromDimSys 'True a where
  beat bpm _ _ = coerce bpm / 60
  {-# INLINE beat #-}



------------------------------ Electric current ------------------------------

------------------------- Thermodynamic temperature --------------------------

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


-- instance Fractional a => ConvertorClass Celsius 'ToDimSys 'False a where
--   convertor _ x = x + 273.15
--   {-# INLINE convertor #-}

-- instance Fractional a => ConvertorClass Celsius 'FromDimSys 'False a where
--   convertor _ x = x - 273.15
--   {-# INLINE convertor #-}

-- instance Fractional a => ConvertorClass Celsius cd 'True a where
--   convertor _ = id
--   {-# INLINE convertor #-}


---------------------------- Amount of substance -----------------------------

----------------------------- Luminous intensity -----------------------------


------------------------------- Compound units -------------------------------



