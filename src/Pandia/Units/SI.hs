module Pandia.Units.SI
  ( module Pandia.Units.SI
  ) where

import Pandia.Units.Convertor
import Pandia.Units.Dimension
import Pandia.Units.Prefix
import Pandia.Units.Rel
import Pandia.Units.Unit

import GHC.TypeLits

type SI = '[
    'Dim "L"
  , 'Dim "M"
  , 'Dim "T"
  , 'Dim "I"
  , 'Dim "Th"
  , 'Dim "N"
  , 'Dim "J"
  ]

type AngleSI = 'Dim "A" ': SI

type family DimL (syst :: DimSystem Symbol) (n :: Rel) :: [Dim Symbol] where
  DimL syst n = MonoDim syst "L" n

type family DimM (syst :: DimSystem Symbol) (n :: Rel) :: [Dim Symbol] where
  DimM syst n = MonoDim syst "M" n

type family DimT (syst :: DimSystem Symbol) (n :: Rel) :: [Dim Symbol] where
  DimT syst n = MonoDim syst "T" n

type family DimI (syst :: DimSystem Symbol) (n :: Rel) :: [Dim Symbol] where
  DimI syst n = MonoDim syst "I" n

type family DimTh (syst :: DimSystem Symbol) (n :: Rel) :: [Dim Symbol] where
  DimTh syst n = MonoDim syst "Th" n

type family DimN (syst :: DimSystem Symbol) (n :: Rel) :: [Dim Symbol] where
  DimN syst n = MonoDim syst "N" n

type family DimJ (syst :: DimSystem Symbol) (n :: Rel) :: [Dim Symbol] where
  DimJ syst n = MonoDim syst "J" n

type family DimA (syst :: DimSystem Symbol) (n :: Rel) :: [Dim Symbol] where
  DimA syst n = MonoDim syst "A" n


----------------------------------- Angle ------------------------------------

newtype Angle a = Angle a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance FlexQuantity "A" = Angle

newtype Radian a = Radian a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance BaseUnit "A" = Radian

instance HasDim syst Radian where
  type DimOf syst Radian = DimA syst (Pos 1)

instance ConvertorClass Radian cd p a

radian :: Convertor Radian cd p a
radian = convertor
{-# INLINE radian #-}


----------------------------------- Length -----------------------------------



-- | Quantity of the length dimension whose unit is not specified.
newtype Length a = Length a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance FlexQuantity "L" = Length

instance HasDim syst Length where
  type DimOf syst Length = DimL syst (Pos 1)

-- | A quantity in meters
--
-- This is the base unit of the length dimension in the SI system.
newtype Meter a = Meter a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance BaseUnit "L" = Meter

instance HasDim syst Meter where
  type DimOf syst Meter = DimL syst (Pos 1)

instance ConvertorClass Meter cd p a

meter :: Convertor Meter cd p a
meter = convertor
{-# INLINE meter #-}

------------------------------------ Mass ------------------------------------

newtype Mass a = Mass a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance FlexQuantity "M" = Mass

newtype Gram a = Gram a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance BaseUnit "M" = Kilo Gram

instance HasDim syst Gram where
  type DimOf syst Gram = DimM syst (Pos 1)

gram :: ConvertorClass Gram cd p a => Convertor Gram cd p a
gram = convertor
{-# INLINE gram #-}

-- instance Fractional a => ConvertorClass Gram 'ToDimSys 'False a where
--   convertor _ x = x / 1000
--   {-# INLINE convertor #-}

-- instance Num a => ConvertorClass Gram 'ToDimSys 'True a where
--   convertor _ x = x * 1000
--   {-# INLINE convertor #-}

-- instance Num a => ConvertorClass Gram 'FromDimSys 'False a where
--   convertor _ x = x * 1000
--   {-# INLINE convertor #-}

-- instance Fractional a => ConvertorClass Gram 'FromDimSys 'True a where
--   convertor _ x = x / 1000
--   {-# INLINE convertor #-}


------------------------------------ Time ------------------------------------

newtype Time a = Time a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance FlexQuantity "T" = Time

newtype Second a = Second a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance BaseUnit "T" = Second

instance HasDim syst Second where
  type DimOf syst Second = DimT syst (Pos 1)

instance ConvertorClass Second cd p a

second :: Convertor Second cd p a
second = convertor
{-# INLINE second #-}


------------------------------ Electric current ------------------------------

newtype ElectricCurrent a = ElectricCurrent a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance FlexQuantity "I" = ElectricCurrent

newtype Ampere a = Ampere a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance BaseUnit "I" = Ampere

instance HasDim syst Ampere where
  type DimOf syst Ampere = DimI syst (Pos 1)


instance ConvertorClass Ampere cd p a

ampere :: Convertor Ampere cd p a
ampere = convertor
{-# INLINE ampere #-}

------------------------- Thermodynamic temperature --------------------------

newtype Temperature a = Temperature a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance FlexQuantity "Th" = Temperature

newtype Kelvin a = Kelvin a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance BaseUnit "Th" = Kelvin

instance HasDim syst Kelvin where
  type DimOf syst Kelvin = DimTh syst (Pos 1)

instance ConvertorClass Kelvin cd p a

kelvin :: Convertor Kelvin cd p a
kelvin = convertor
{-# INLINE kelvin #-}


---------------------------- Amount of substance -----------------------------

newtype SubstanceAmount a = SubstanceAmount a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance FlexQuantity "N" = SubstanceAmount

newtype Mole a = Mole a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance BaseUnit "N" = Mole

instance HasDim syst Mole where
  type DimOf syst Mole = DimN syst (Pos 1)

instance ConvertorClass Mole cd p a

mole :: Convertor Mole cd p a
mole = convertor
{-# INLINE mole #-}

----------------------------- Luminous intensity -----------------------------

newtype LuminousIntensity a = LuminousIntensity a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance FlexQuantity "J" = LuminousIntensity

newtype Candela a = Candela a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance BaseUnit "J" = Candela

instance HasDim syst Candela where
  type DimOf syst Candela = DimJ syst (Pos 1)


instance ConvertorClass Candela cd p a

candela :: Convertor Candela cd p a
candela = convertor
{-# INLINE candela #-}


------------------------------- Derived units -------------------------------



type Newton = Meter -*- Kilo Gram  -*- Second -^- Neg 2

