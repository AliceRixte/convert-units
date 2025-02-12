module Pandia.Units.NonStd.NonSI
  ( module Pandia.Units.NonStd.NonSI
  ) where

import Pandia.Units.Convertor
import Pandia.Units.Dimension
import Pandia.Units.System
import Pandia.Units.Rel

import Data.Coerce

----------------------------------- Angle ------------------------------------

-- | Angle in degrees
--
newtype Degree a = Degree a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance HasDim syst Degree where
  type DimOf syst Degree = DimA syst (Pos 1)

degree :: ConvertorClass Degree cd p a => Convertor Degree cd p a
degree = convertor
{-# INLINE degree #-}

instance Floating a => ConvertorClass Degree 'ToDimSys 'False a where
  convertor _ x = x * pi / 180
  {-# INLINE convertor #-}

instance Floating a => ConvertorClass Degree 'ToDimSys 'True a where
  convertor _ _ = pi / 180
  {-# INLINE convertor #-}

instance Floating a => ConvertorClass Degree 'FromDimSys 'False a where
  convertor _ x = x * 180 / pi
  {-# INLINE convertor #-}

instance Floating a => ConvertorClass Degree 'FromDimSys 'True a where
  convertor _ _ = 180 / pi
  {-# INLINE convertor #-}


-- | Angle in complete turns (also called cycles or revolutions)
--
-- See https://en.wikipedia.org/wiki/Turn_(angle)
newtype Turn a = Turn a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance HasDim syst Turn where
  type DimOf syst Turn = DimA syst (Pos 1)

turn :: ConvertorClass Turn cd p a => Convertor Turn cd p a
turn = convertor
{-# INLINE turn #-}

instance Floating a => ConvertorClass Turn 'ToDimSys 'False a where
  convertor _ x = x * 2 * pi
  {-# INLINE convertor #-}

instance Floating a => ConvertorClass Turn 'ToDimSys 'True a where
  convertor _ _ = 2 * pi
  {-# INLINE convertor #-}

instance Floating a => ConvertorClass Turn 'FromDimSys 'False a where
  convertor _ x = x / (2 * pi)
  {-# INLINE convertor #-}

instance Floating a => ConvertorClass Turn 'FromDimSys 'True a where
  convertor _ _ = 1 / (2 * pi)
  {-# INLINE convertor #-}


-- | Angle in gradians
--
-- See https://en.wikipedia.org/wiki/Gradian
newtype Gradian a = Gradian a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance HasDim syst Gradian where
  type DimOf syst Gradian = DimA syst (Pos 1)

gradian :: ConvertorClass Gradian cd p a => Convertor Gradian cd p a
gradian = convertor
{-# INLINE gradian #-}

instance Floating a => ConvertorClass Gradian 'ToDimSys 'False a where
  convertor _ x = x * pi / 200
  {-# INLINE convertor #-}

instance Floating a => ConvertorClass Gradian 'ToDimSys 'True a where
  convertor _ _ = pi / 200
  {-# INLINE convertor #-}

instance Floating a => ConvertorClass Gradian 'FromDimSys 'False a where
  convertor _ x = x * 200 / pi
  {-# INLINE convertor #-}

instance Floating a => ConvertorClass Gradian 'FromDimSys 'True a where
  convertor _ _ = 200 / pi
  {-# INLINE convertor #-}


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



