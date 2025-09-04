--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.Base.System
-- Description :  System of units
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Describe a system of units and their dimensions.
--
--------------------------------------------------------------------------------

module Data.Units.SI.System
  ( Mass (..)
  , Gram (..)
  , Length (..)
  , Meter (..)
  , Time (..)
  , Second (..)
  , Temperature (..)
  , Kelvin (..)
  , AmountOfSubstance (..)
  , Mole (..)
  , Current (..)
  , Ampere (..)
  , LuminousIntensity (..)
  , Candela (..)
  ) where

import Data.Coerce
import Data.Units.Base
import Data.Units.SI.Prefixes

-- | The mass dimension, denotated @M@ in SI.
--
--  This may contain a mass quantity with unspecified unit.
--
$(mkDim "Mass" "M" 200)


-- | A quantity in grams, denotated @g@ in SI.
--
-- Notice that the base unit for the mass dimension is @'Kilo' 'Gram'@, not
-- @'Gram'@.
--
-- This is correctly taken into account by this library:
--
-- >>> from (Gram 8)
-- quantity @(Kilo Gram) 8.0e-3
-- >>> :kind! BaseUnitOf (Kilo Gram)
-- Not in scope: type constructor or class `BaseUnitOf'
$(mkUnitNoFactor "Gram" "g" ''Mass)

instance Fractional a => ConvertibleUnit Gram a

instance Fractional a => ConvFactor Gram a where
  factorTo = 1000
  {-# INLINE factorTo #-}

instance IsDim Mass where
  type DimToUnit Mass = Kilo Gram

-- | The length dimension, denotated @L@ in SI.
--
--  This may contain a length quantity with unspecified unit.
--
$(mkDim "Length" "L" 300)


-- | A quantity in meters, denotated @m@ in SI.
--
-- This is the base unit of the length dimension in the SI system.
--
$(mkBaseUnit "Meter" "m" ''Length)



-- | The time dimension, denotated @T@ in SI.
--
--  This may contain a length quantity with unspecified unit.
--
$(mkDim "Time" "T" 400)


-- | A quantity in seconds, denotated @s@ in SI.
--
-- This is the base unit of the time dimension in the SI system.
--
$(mkBaseUnit "Second" "s" ''Time)



-- | The thermodynamic temperature dimension, denotated @Θ@ in SI.
--
--  This may contain a temperature quantity with unspecified unit.
--
$(mkDim "Temperature" "Θ" 500)


-- | A quantity in Kelvin, denotated @K@ in SI.
--
-- This is the base unit of the thermodynamic temperature dimension in the SI
-- system.
--
$(mkBaseUnit "Kelvin" "K" ''Temperature)

-- | The amount of substance dimension, denotated @N@ in SI.
--
--  This may contain an amount of substance quantity with unspecified unit.
--
$(mkDim "AmountOfSubstance" "N" 600)


-- | A quantity in moles, denotated @mol@ in SI.
--
-- This is the base unit of the amount of substance dimension in the SI
-- system.
--
$(mkBaseUnit "Mole" "mol" ''AmountOfSubstance)

-- | The electric current dimension, denotated @I@ in SI.
--
--  This may contain an electric current quantity with unspecified unit.
--
$(mkDim "Current" "I" 700)


-- | A quantity in amperes, denotated @A@ in SI.
--
-- This is the base unit of the electric current dimension in the SI
-- system.
--
$(mkBaseUnit "Ampere" "A" ''Current)

-- | The luminous intensity dimension, denotated @J@ in SI.
--
--  This may contain a luminous intensity quantity with unspecified unit.
--
$(mkDim "LuminousIntensity" "J" 800)

-- | A quantity in candelas, denotated @cd@ in SI.
--
-- This is the base unit of the luminous intensity dimension in the SI
-- system.
--
$(mkBaseUnit "Candela" "cd" ''LuminousIntensity)

