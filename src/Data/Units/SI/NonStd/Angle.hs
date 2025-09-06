--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.SI.NonStd.Angle
-- Description :  Dimensionless non standard angle units
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- Dimensionless non standard angle units.
--
--------------------------------------------------------------------------------


module Data.Units.SI.NonStd.Angle where

import Data.Units.Base

-- | Angle in degrees.
--
$(mkUnitNoFactor "Degree" "°" ''NoDim)

-- | Angle in complete turns (also called cycles or revolutions)
--
-- See https://en.wikipedia.org/wiki/Turn_(angle)
--
$(mkUnitNoFactor "Turn" "tr" ''NoDim)

-- | Angle in gradians
--
-- See https://en.wikipedia.org/wiki/Gradian
--
$(mkUnitNoFactor "Gradian" "grad" ''NoDim)

instance Floating a => ConvertibleUnit Degree a

instance Floating a => ConversionFactor Degree a where
  factor = pi / 180
  {-# INLINE factor #-}

instance Floating a => ConvertibleUnit Turn a

instance Floating a => ConversionFactor Turn a where
  factor = 2 * pi
  {-# INLINE factor #-}

instance Floating a => ConvertibleUnit Gradian a

instance Floating a => ConversionFactor Gradian a where
  factor = pi / 200
  {-# INLINE factor #-}

