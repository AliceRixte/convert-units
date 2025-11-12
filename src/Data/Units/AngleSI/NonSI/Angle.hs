
--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.AngleSI.NonSI.Angle
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


module Data.Units.AngleSI.NonSI.Angle
  ( Degree (..)
  , Turn (..)
  , Gradian (..)
  )
where

import Data.Units.Base

import Data.Units.AngleSI.System



-- | Angle in degrees.
$(mkUnitNoFactor "Degree" "°" ''Angle)

-- | Angle in complete turns (also called cycles or revolutions).
$(mkUnitNoFactor "Turn" "tr" ''Angle)

-- | Angle in gradians.
$(mkUnitNoFactor "Gradian" "grad" ''Angle)


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

