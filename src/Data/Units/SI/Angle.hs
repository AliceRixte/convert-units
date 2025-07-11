--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.AngleSI.Angle
-- Description :  Dimensionless angles
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- This module defines radians as the standard unit for the angle (`@A@`)
-- dimension.
--
-- See "Data.Units.SI.Angle" for dimensionless radians and steradians.
--
--------------------------------------------------------------------------------

module Data.Units.SI.Angle where

import Data.Fixed

import Data.Units.Base

-- | An angle in radians.
--
newtype Radian a = Radian a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via MetaUnit Radian a

instance Fractional a => ConvFactor Radian a where
  factorFrom = 1

type instance DimOf Radian = NoDim

instance IsUnit Radian where
  type StdUnitOf Radian = NoUnit

instance ShowUnit Radian where
  type ShowUnitType Radian = Text "rad"
  showUnit = "rad"

-- | Normalize an angle to the range ]-pi, pi]
normalizeRadians :: (RealFrac a, Floating a) => Radian a -> Radian a
normalizeRadians x = if xmod > pi then xmod - twoPi else xmod
  where
    twoPi = 2 * pi
    xmod = x `mod'` twoPi

-- | A solid angle in steradians.
--
newtype Steradian a = Steradian a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via MetaUnit Steradian a

instance Fractional a => ConvFactor Steradian a where
  factorFrom = 1

type instance DimOf Steradian = NoDim

instance IsUnit Steradian where
  type StdUnitOf Steradian = NoUnit

instance ShowUnit Steradian where
  type ShowUnitType Steradian = Text "sr"
  showUnit = "sr"

