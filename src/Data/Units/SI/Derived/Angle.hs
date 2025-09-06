--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.SI.Derived.Angle
-- Description :  Dimensionless angles
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- This module defines radians and steradians as derived dimensionless units.
--
-- See "Data.Units.AngleSI" for radians and steradians in an angle
-- dimension `@A@`.
--
--------------------------------------------------------------------------------

module Data.Units.SI.Derived.Angle
  ( Angle
  , Radian (..)
  , normalizeRadians
  , SolidAngle
  , Steradian (..)
  , LuminousFlux
  , Lumen (..)
  , Illuminance
  , Lux (..)
  )
  where

import Data.Fixed
import Data.Coerce

import Data.Units.Base
import Data.Units.SI.System
import Data.Units.SI.Derived.NonAngle

-- | The angle derived dimension in SI. Equal to
--
-- @'NoDim'@
--
type Angle = NormalizeDim (Length ./. Length)

-- | An angle in radians.
--
$(mkUnit "Radian" "rad" ''Angle 1)

-- | The solid angle derived dimension in SI. Equal to
--
-- @'NoDim'@
--
type SolidAngle = NormalizeDim (Angle .^+ 2)

-- | A solid angle in steradians.
--
$(mkUnit "Steradian" "sr" ''SolidAngle 1)

-- | Normalize an angle to the range ]-pi, pi]
normalizeRadians :: (RealFrac a, Floating a) => Radian a -> Radian a
normalizeRadians x = if xmod > pi then xmod - twoPi else xmod
  where
    twoPi = 2 * pi
    xmod = x `mod'` twoPi

-- | Luminous flux quantity. Equal to
--
-- @ 'LuminousIntensity'@
--
type LuminousFlux = NormalizeDim (LuminousIntensity .*. SolidAngle)

-- | Luminous flux in lumens
--
$(mkUnit "Lumen" "lm" ''LuminousFlux 1)

-- | Illuminance quantity. Equal to
--
-- @ 'Length'.^-2 .*. 'LuminousIntensity' @
--
type Illuminance = NormalizeDim (LuminousFlux ./. Area)

-- | Illuminance in lux
--
$(mkUnit "Lux" "lx" ''Illuminance 1)