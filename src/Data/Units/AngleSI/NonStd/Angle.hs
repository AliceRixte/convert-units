
--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.AngleSI.NonStd.Angle
-- Description :  Non standard angle units with a dimension A
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- Non standard angle units with a dimension A.
--
--------------------------------------------------------------------------------


module Data.Units.AngleSI.NonStd.Angle where

import Data.Units.Base

import Data.Units.AngleSI.Angle



-- | Angle in degrees.
--
$(mkUnitNoFactor "Degree" "°" ''Angle)

-- | Angle in complete turns (also called cycles or revolutions)
--
-- See https://en.wikipedia.org/wiki/Turn_(angle)
--
$(mkUnitNoFactor "Turn" "tr" ''Angle)

-- | Angle in gradians
--
-- See https://en.wikipedia.org/wiki/Gradian
--
$(mkUnitNoFactor "Gradian" "grad" ''Angle)


instance Floating a => ConvertibleUnit Degree a

instance Floating a => ConversionFactor Degree a where
  factorFrom = pi / 180
  {-# INLINE factorFrom #-}

instance Floating a => ConvertibleUnit Turn a

instance Floating a => ConversionFactor Turn a where
  factorFrom = 2 * pi
  {-# INLINE factorFrom #-}

instance Floating a => ConvertibleUnit Gradian a

instance Floating a => ConversionFactor Gradian a where
  factorFrom = pi / 200
  {-# INLINE factorFrom #-}

