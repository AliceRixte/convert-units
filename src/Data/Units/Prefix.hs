{-# LANGUAGE TypeApplications #-}

module Data.Units.Prefix where

import Data.Convert.FromTo
import Data.Units.Base

newtype Kilo (u :: Unit) a = Kilo (u a)
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
            , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

instance IsUnit u => IsUnit (Kilo u) where
  type StdUnitOf (Kilo u) = u

instance (Num a, ConvFactor u a)
  => ConvFactor (Kilo u) a where
  factorFrom = 1000
