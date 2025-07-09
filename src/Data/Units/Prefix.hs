{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Units.Prefix where

import Data.Kind

import Data.Convert.FromTo
import Data.Units.Base


showsPrecPrefix :: forall u. ShowUnit u => String -> Int -> ShowS
showsPrecPrefix s d = showParen (d > 10) $
    showString s . showsUnit @u


newtype Milli (u :: Unit) a = Milli (u a)
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
            , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

instance IsUnit u => IsUnit (Milli u) where
  type StdUnitOf (Milli u) = u

instance (Num a, ConvFactor u a)
  => ConvFactor (Milli u) a where
  factorTo = 1000 * factorTo @u

instance (ShowUnit u, IsUnit u) => ShowUnit (Milli u) where
  type ShowUnitType (Milli u) = Text "k" :<>: ShowUnitType u
  showsPrecUnit = showsPrecPrefix @u "k"



instance (ShowUnit u, Show a) =>  Show (Milli u a) where
  showsPrec = showsPrecQuantity @(Milli u)

--------------------------------------------------------------------------------


newtype Hecto (u :: Unit) a = Hecto (u a)
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
            , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

instance IsUnit u => IsUnit (Hecto u) where
  type StdUnitOf (Hecto u) = u

instance (Num a, ConvFactor u a)
  => ConvFactor (Hecto u) a where
  factorFrom = 10 * factorFrom @u

instance (ShowUnit u, IsUnit u) => ShowUnit (Hecto u) where
  type ShowUnitType (Hecto u) = Text "k" :<>: ShowUnitType u
  showsPrecUnit = showsPrecPrefix @u "k"

instance (ShowUnit u, Show a) =>  Show (Hecto u a) where
  showsPrec = showsPrecQuantity @(Hecto u)

--------------------------------------------------------------------------------
newtype Kilo (u :: Unit) a = Kilo (u a)
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
            , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

instance IsUnit u => IsUnit (Kilo u) where
  type StdUnitOf (Kilo u) = u

instance (Num a, ConvFactor u a)
  => ConvFactor (Kilo u) a where
  factorFrom = 1000 * factorFrom @u

instance (ShowUnit u, IsUnit u) => ShowUnit (Kilo u) where
  type ShowUnitType (Kilo u) = Text "k" :<>: ShowUnitType u
  showsPrecUnit = showsPrecPrefix @u "k"


instance (ShowUnit u, Show a) =>  Show (Kilo u a) where
  showsPrec = showsPrecQuantity @(Kilo u)


