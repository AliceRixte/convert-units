module Pandia.Units.Core.System
  ( module Pandia.Units.Core.System
  ) where

import Data.Fixed

import Pandia.Units.Core.Convertor
import Pandia.Units.Core.Dimension
import Pandia.Units.Core.Prefix
import Pandia.Units.Core.Rel
import Pandia.Units.Core.Unit

import Data.Proxy

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

type family NoDim (sys :: DimSystem Symbol) :: [Dim Symbol] where
  NoDim ('Dim k ': ds) = 'Dim k ('Pos 0) ': NoDim ds

type family DimA (sys :: DimSystem Symbol) (n :: Rel) :: [Dim Symbol] where
  DimA SI n = NoDim SI
  DimA sys n = MonoDim sys "A" n

type family DimL (sys :: DimSystem Symbol) (n :: Rel) :: [Dim Symbol] where
  DimL sys n = MonoDim sys "L" n

type family DimM (sys :: DimSystem Symbol) (n :: Rel) :: [Dim Symbol] where
  DimM sys n = MonoDim sys "M" n

type family DimT (sys :: DimSystem Symbol) (n :: Rel) :: [Dim Symbol] where
  DimT sys n = MonoDim sys "T" n

type family DimI (sys :: DimSystem Symbol) (n :: Rel) :: [Dim Symbol] where
  DimI sys n = MonoDim sys "I" n

type family DimTh (sys :: DimSystem Symbol) (n :: Rel) :: [Dim Symbol] where
  DimTh sys n = MonoDim sys "Th" n

type family DimN (sys :: DimSystem Symbol) (n :: Rel) :: [Dim Symbol] where
  DimN sys n = MonoDim sys "N" n

type family DimJ (sys :: DimSystem Symbol) (n :: Rel) :: [Dim Symbol] where
  DimJ sys n = MonoDim sys "J" n

----------------------------------- Angle ------------------------------------

newtype Angle a = Angle a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance FlexQuantity "A" = Angle

newtype Radian a = Radian a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance BaseUnit "A" = Radian

instance HasDim sys Radian where
  type DimOf sys Radian = DimA sys (Pos 1)

instance ConvertorClass Radian cd p a

-- | Convertor for angle in radians
radian :: Convertor Radian cd p a
radian = convertor
{-# INLINE radian #-}

-- | Normalize an angle to the range ]-pi, pi]
normalizeRadians :: (RealFrac a, Floating a) => Radian a -> Radian a
normalizeRadians x = if xmod > pi then xmod - twoPi else xmod
  where
    twoPi = 2 * pi
    xmod = x `mod'` twoPi


----------------------------------- Length -----------------------------------



-- | Quantity of the length dimension whose unit is not specified.
newtype Length a = Length a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance FlexQuantity "L" = Length

instance HasDim sys Length where
  type DimOf sys Length = DimL sys (Pos 1)

-- | A quantity in meters
--
-- This is the base unit of the length dimension in the SI system.
newtype Meter a = Meter a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance BaseUnit "L" = Meter

instance HasDim sys Meter where
  type DimOf sys Meter = DimL sys (Pos 1)

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

instance HasDim sys Gram where
  type DimOf sys Gram = DimM sys (Pos 1)

gram :: ConvertorClass Gram cd p a => Convertor Gram cd p a
gram = convertor
{-# INLINE gram #-}

instance Fractional a => ConvertorClass Gram 'ToDimSys 'False a where
  convertor _ x = x / 1000
  {-# INLINE convertor #-}

instance Fractional a => ConvertorClass Gram 'ToDimSys 'True a where
  convertor _ _ = 1 / 1000
  {-# INLINE convertor #-}

instance Num a => ConvertorClass Gram 'FromDimSys 'False a where
  convertor _ x = x * 1000
  {-# INLINE convertor #-}

instance Num a => ConvertorClass Gram 'FromDimSys 'True a where
  convertor _ _ = 1000
  {-# INLINE convertor #-}


------------------------------------ Time ------------------------------------

newtype Time a = Time a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance FlexQuantity "T" = Time

newtype Second a = Second a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

type instance BaseUnit "T" = Second

instance HasDim sys Second where
  type DimOf sys Second = DimT sys (Pos 1)

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

instance HasDim sys Ampere where
  type DimOf sys Ampere = DimI sys (Pos 1)


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

instance HasDim sys Kelvin where
  type DimOf sys Kelvin = DimTh sys (Pos 1)

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

instance HasDim sys Mole where
  type DimOf sys Mole = DimN sys (Pos 1)

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

instance HasDim sys Candela where
  type DimOf sys Candela = DimJ sys (Pos 1)


instance ConvertorClass Candela cd p a

candela :: Convertor Candela cd p a
candela = convertor
{-# INLINE candela #-}


------------------------------- Derived units -------------------------------


type Joule = Meter -*- Kilo Gram -^- Pos 2 -*- Second -^- Neg 2

joule :: Convertor Joule cd p a
joule _ = id
{-# INLINE joule #-}

type Newton = Meter -*- Kilo Gram  -*- Second -^- Neg 2

newton :: Convertor Newton cd p a
newton _ = id
{-# INLINE newton #-}


dimSI :: Convertor u cd p a -> Proxy (DimOf SI u)
dimSI _ = Proxy
{-# INLINE dimSI #-}

dimUnitSI :: u a -> Proxy (DimOf SI u)
dimUnitSI _ = Proxy
{-# INLINE dimUnitSI #-}