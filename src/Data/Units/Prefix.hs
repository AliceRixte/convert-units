{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Units.Prefix where

import Data.Kind

import Data.Convert.FromTo
import Data.Units.Base



newtype Kilo (u :: Unit) a = Kilo (u a)
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
            , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

instance IsUnit u => IsUnit (Kilo u) where
  type StdUnitOf (Kilo u) = u

showsPrecPrefix :: forall u. ShowUnit u => String -> Int -> ShowS
showsPrecPrefix s d = showParen (d > 10) $
    showString s . showsUnit @u

instance (ShowUnit u, IsUnit u) => ShowUnit (Kilo u) where
  type ShowUnitType (Kilo u) = Text "k" :<>: ShowUnitType u
  showsPrecUnit = showsPrecPrefix @u "k"

instance (ShowUnit u, Show a) =>  Show (Kilo u a) where
  showsPrec = showsPrecQuantity @(Kilo u)

instance (Num a, ConvFactor u a)
  => ConvFactor (Kilo u) a where
  factorFrom = 1000 * factorFrom @u
