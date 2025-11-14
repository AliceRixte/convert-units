
--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.AngleSI.System
-- Description :  SI unit system with dimensional angles of dimension A
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


module Data.Units.AngleSI.System
  ( module Data.Units.SI.System
  , Angle (..)
  , Radian (..)
  , normalizeRadians
  )
  where

import Data.Fixed
import Data.Coerce

import Data.Units.Core
import Data.Units.SI.System

-- | The angle dimension, denotated @A@.
--
$(mkDim "Angle" "A" 1000)


-- | An angle in radians.
--
-- This is the base unit of the angle dimension.
--
$(mkBaseUnit "Radian" "rad" ''Angle)

-- | Normalize an angle to the range ]-pi, pi]
normalizeRadians :: (RealFrac a, Floating a) => Radian a -> Radian a
normalizeRadians x = if xmod > pi then xmod - twoPi else xmod
  where
    twoPi = 2 * pi
    xmod = x `mod'` twoPi

