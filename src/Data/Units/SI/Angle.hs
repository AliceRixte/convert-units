--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.SI.Angle
-- Description :  Dimensionless angles
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- This module defines radians and steradians as dimension less units. as the
-- standard unit for the angle (`@A@`)
--
-- See "Data.Units.AngleSI.Angle" for radians and steradians in an angle
-- dimension `@A@`.
--
--------------------------------------------------------------------------------

module Data.Units.SI.Angle where

import Data.Fixed
import Data.Coerce

import Data.Units.Base

-- | An angle in radians.
--
$(mkUnitFrom "Radian" "rad" ''NoDim 1)

-- | A solid angle in steradians.
--
$(mkUnitFrom "Steradian" "sr" ''NoDim 1)

-- | Normalize an angle to the range ]-pi, pi]
normalizeRadians :: (RealFrac a, Floating a) => Radian a -> Radian a
normalizeRadians x = if xmod > pi then xmod - twoPi else xmod
  where
    twoPi = 2 * pi
    xmod = x `mod'` twoPi

