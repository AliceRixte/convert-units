{-# LANGUAGE TypeApplications #-}

module Data.Units.Prefix where

import Data.Convert.FromTo
import Data.Units.Base

newtype Kilo (u :: Unit) a = Kilo (u a)
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
            , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)


instance (Num a, IsQuantity (u a) a)
  => IsQuantity (Kilo u a) a where
  convFactor = 1000
