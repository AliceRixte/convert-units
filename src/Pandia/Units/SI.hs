module Pandia.Units.SI
  ( module Pandia.Units.SI
  ) where

import Pandia.Units.Convertor
import Pandia.Units.Dimension
import Pandia.Units.Prefix
import Pandia.Units.Rel

----------------------------------- Length -----------------------------------

newtype Meter a = Meter a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ToDimension Meter where
  type ToDim Meter = DimLength

instance ConvertorClass Meter a

meter :: Convertor Meter a
meter = convertor
{-# INLINE meter #-}

------------------------------------ Mass ------------------------------------

newtype Gram a = Gram a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ToDimension Gram where
  type ToDim Gram = DimMass

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

------------------------------------ Time ------------------------------------

newtype Second a = Second a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ToDimension Second where
  type ToDim Second = DimTime

instance ConvertorClass Second a

second :: Convertor Second a
second = convertor
{-# INLINE second #-}


------------------------------ Electric current ------------------------------

newtype Ampere a = Ampere a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ToDimension Ampere where
  type ToDim Ampere = DimCurrent

instance ConvertorClass Ampere a

ampere :: Convertor Ampere a
ampere = convertor
{-# INLINE ampere #-}

------------------------- Thermodynamic temperature --------------------------


newtype Kelvin a = Kelvin a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ToDimension Kelvin where
  type ToDim Kelvin = DimTemperature

instance ConvertorClass Kelvin a

kelvin :: Convertor Kelvin a
kelvin = convertor
{-# INLINE kelvin #-}


---------------------------- Amount of substance -----------------------------

newtype Mole a = Mole a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ToDimension Mole where
  type ToDim Mole = DimAmount

instance ConvertorClass Mole a

mole :: Convertor Mole a
mole = convertor
{-# INLINE mole #-}

----------------------------- Luminous intensity -----------------------------

newtype Candela a = Candela a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance ToDimension Candela where
  type ToDim Candela = DimLuminousIntensity

instance ConvertorClass Candela a

candela :: Convertor Candela a
candela = convertor
{-# INLINE candela #-}


------------------------------- Compound units -------------------------------

type Newton = Kilo Gram -*- Meter -/- Second -^- Pos 2

