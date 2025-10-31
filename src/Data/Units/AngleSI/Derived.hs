--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.AngleSI.Derived
-- Description :  Dimensionless angles
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- This module defines SI derived units that use angles, where angles have a
-- dimension @`A`@.
--
-- See "Data.Units.SI.Derived.Angle" for an equivalent of this where angles are
-- dimensionless.
--
--------------------------------------------------------------------------------

module Data.Units.AngleSI.Derived
  ( module Data.Units.SI.Derived.NonAngle
  , SolidAngle
  , Steradian (..)
  , LuminousFlux
  , Lumen (..)
  , Illuminance
  , Lux (..)
  )
  where

import Data.Coerce

import Data.Units.Base
import Data.Units.AngleSI.System
import Data.Units.SI.Derived.NonAngle


-- | Solid angle quantity.
type SolidAngle = Angle .^+ 2

-- | A solid angle in steradians.
--
$(mkUnit "Steradian" "sr" ''SolidAngle 1)

-- | Luminous flux quantity. Equal to
--
-- @ 'Angle'.^+2 .*. 'LuminousIntensity'@
--
type LuminousFlux = NormalizeDim (LuminousIntensity .*. SolidAngle)

-- | Luminous flux in lumens.
$(mkUnit "Lumen" "lm" ''LuminousFlux 1)

-- | Illuminance quantity. Equal to
--
-- @ 'Angle'.^+2 .*. 'Length'.^-2 .*. 'LuminousIntensity' @
--
type Illuminance = NormalizeDim (LuminousFlux ./. Area)

-- | Illuminance in lux.
$(mkUnit "Lux" "lx" ''Illuminance 1)