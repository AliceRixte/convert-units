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
import Data.Coerce

import Data.Units.Base

-- | An angle in radians.
--
$(mkUnit "Radian" "sr" ''NoDim 1)

-- | A solid angle in steradians.
--
$(mkUnit "Steradian" "sr" ''NoDim 1)

-- | Normalize an angle to the range ]-pi, pi]
normalizeRadians :: (RealFrac a, Floating a) => Radian a -> Radian a
normalizeRadians x = if xmod > pi then xmod - twoPi else xmod
  where
    twoPi = 2 * pi
    xmod = x `mod'` twoPi

