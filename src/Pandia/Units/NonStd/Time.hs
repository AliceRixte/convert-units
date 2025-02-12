module Pandia.Units.NonStd.Time
  ( module Pandia.Units.NonStd.Time
  ) where

import Pandia.Units.Core

import Data.Coerce

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
  beat bpm  _ x = x * 60 / coerce bpm
  {-# INLINE beat #-}

instance Fractional a => BeatConvertor 'ToDimSys 'True a where
  beat bpm _ _ = 60 / coerce bpm
  {-# INLINE beat #-}

instance Fractional a => BeatConvertor 'FromDimSys 'False a where
  beat bpm _ x = x * coerce bpm / 60
  {-# INLINE beat #-}

instance Fractional a => BeatConvertor 'FromDimSys 'True a where
  beat bpm _ _ = coerce bpm / 60
  {-# INLINE beat #-}



