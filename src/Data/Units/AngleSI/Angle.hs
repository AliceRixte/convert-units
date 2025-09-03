
--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.AngleSI.Angle
-- Description :  Angles with a dimension A
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


module Data.Units.AngleSI.Angle where

import Data.Fixed

import Data.Units.Base

-- | The angle dimension, denotated @A@.
--
newtype Angle a = Angle a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)

type instance DimId Angle = 100
type instance ShowDim Angle = Text "A"

instance IsDim Angle where
  type DimToUnit Angle = Radian

-- | An angle in radians.
--
newtype Radian a = Radian a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)

instance Fractional a => ConvFactor Radian a where
  factorFrom = 1

instance IsUnit Radian where
  type DimOf Radian = Angle

instance ShowUnit Radian where
  type ShowUnitType Radian = Text "rad"
  showUnit = "Radian"
  prettyUnit = "rad"

-- | Normalize an angle to the range ]-pi, pi]
normalizeRadians :: (RealFrac a, Floating a) => Radian a -> Radian a
normalizeRadians x = if xmod > pi then xmod - twoPi else xmod
  where
    twoPi = 2 * pi
    xmod = x `mod'` twoPi


-- | A solid angle in steradians.
--
type Steradian = Radian .^+ 2


