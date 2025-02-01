module Pandia.Units.SI
  ( module Pandia.Units.SI
  ) where

import Pandia.Units.Convertor
import Pandia.Units.Dimension
import Pandia.Units.Prefix
import Pandia.Units.Rel

import GHC.TypeLits
----------------------------------- Length -----------------------------------

newtype Meter a = Meter a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance HasDim syst Meter where
  type DimOf syst Meter = DimL syst (Pos 1)

instance ConvertorClass Meter cd p a

meter :: Convertor Meter cd p a
meter = convertor
{-# INLINE meter #-}

------------------------------------ Mass ------------------------------------

newtype Gram a = Gram a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance HasDim syst Gram where
  type DimOf syst Gram = DimM syst (Pos 1)

gram :: ConvertorClass Gram cd p a => Convertor Gram cd p a
gram = convertor
{-# INLINE gram #-}

instance Fractional a => ConvertorClass Gram 'ToSI 'False a where
  convertor _ x = x / 1000
  {-# INLINE convertor #-}

instance Num a => ConvertorClass Gram 'ToSI 'True a where
  convertor _ x = x * 1000
  {-# INLINE convertor #-}

instance Num a => ConvertorClass Gram 'FromSI 'False a where
  convertor _ x = x * 1000
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Gram 'FromSI 'True a where
  convertor _ x = x / 1000
  {-# INLINE convertor #-}


--  This is not the identity, as SI uses kilograms as the base unit for mass.
-- instance Fractional a => ConvertorClass Gram (ToSI a) where
--   convertor _ x = x / 1000
--   {-# INLINE convertor #-}

-- instance Num a => ConvertorClass Gram (Per (ToSI a)) where
--   convertor _ x = x * 1000
--   {-# INLINE convertor #-}

-- instance Num a => ConvertorClass Gram (FromSI a) where
--   convertor _ x = x * 1000
--   {-# INLINE convertor #-}

-- instance Fractional a => ConvertorClass Gram (Per (FromSI a)) where
--   convertor _ x = x / 1000
--   {-# INLINE convertor #-}

------------------------------------ Time ------------------------------------

newtype Second a = Second a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance HasDim syst Second where
  type DimOf syst Second = DimT syst (Pos 1)

instance ConvertorClass Second cd p a

second :: Convertor Second cd p a
second = convertor
{-# INLINE second #-}


------------------------------ Electric current ------------------------------

newtype Ampere a = Ampere a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance HasDim syst Ampere where
  type DimOf syst Ampere = DimI syst (Pos 1)


instance ConvertorClass Ampere cd p a

ampere :: Convertor Ampere cd p a
ampere = convertor
{-# INLINE ampere #-}

------------------------- Thermodynamic temperature --------------------------


newtype Kelvin a = Kelvin a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance HasDim syst Kelvin where
  type DimOf syst Kelvin = DimTh syst (Pos 1)

instance ConvertorClass Kelvin cd p a

kelvin :: Convertor Kelvin cd p a
kelvin = convertor
{-# INLINE kelvin #-}


---------------------------- Amount of substance -----------------------------

newtype Mole a = Mole a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance HasDim syst Mole where
  type DimOf syst Mole = DimN syst (Pos 1)

instance ConvertorClass Mole cd p a

mole :: Convertor Mole cd p a
mole = convertor
{-# INLINE mole #-}

----------------------------- Luminous intensity -----------------------------

newtype Candela a = Candela a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance HasDim syst Candela where
  type DimOf syst Candela = DimJ syst (Pos 1)

instance ConvertorClass Candela cd p a

candela :: Convertor Candela cd p a
candela = convertor
{-# INLINE candela #-}


------------------------------- Derived units -------------------------------

type Radian = Meter -/- Meter

radian :: Num a => Convertor Radian cd p a
radian = convertor
{-# INLINE radian #-}



type Newton = Kilo Gram -*- Meter -/- Second -^- Pos 2

