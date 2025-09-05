--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.SI.System
-- Description :  Base units and dimensions of the International System of Units
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Base units and dimensions of the International System of Units (abbreviated
-- SI).
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
$(mkDim "Mass" "M" 0x08080402)


-- | A quantity in grams, denotated @g@ in SI.
--
-- Notice that the base unit for the mass dimension is @'Kilo' 'Gram'@, not
-- @'Gram'@.
--
-- This is correctly taken into account by this library:
--
-- >>> from (Gram 8)
-- quantity @(Kilo Gram) 8.0e-3
-- >>> :kind! BaseUnitOf Gram
-- BaseUnitOf Gram :: * -> *
-- = Kilo Gram
--
$(mkUnitNoFactor "Gram" "g" ''Mass)

instance Fractional a => ConvertibleUnit Gram a

instance Fractional a => ConversionFactor Gram a where
  factorTo = 1000
  {-# INLINE factorTo #-}

instance IsDim Mass where
  type DimToUnit Mass = Kilo Gram

-- | The length dimension, denotated @L@ in SI.
--
--  This may contain a length quantity with unspecified unit.
--
$(mkDim "Length" "L" 0x08080404)


-- | A quantity in meters, denotated @m@ in SI.
--
-- This is the base unit of the length dimension in the SI system.
--
$(mkBaseUnit "Meter" "m" ''Length)



-- | The time dimension, denotated @T@ in SI.
--
--  This may contain a length quantity with unspecified unit.
--
$(mkDim "Time" "T" 0x08080408)


-- | A quantity in seconds, denotated @s@ in SI.
--
-- This is the base unit of the time dimension in the SI system.
--
$(mkBaseUnit "Second" "s" ''Time)

-- | The electric current dimension, denotated @I@ in SI.
--
--  This may contain an electric current quantity with unspecified unit.
--
$(mkDim "Current" "I" 0x08080801)


-- | A quantity in amperes, denotated @A@ in SI.
--
-- This is the base unit of the electric current dimension in the SI
-- system.
--
$(mkBaseUnit "Ampere" "A" ''Current)

-- | The thermodynamic temperature dimension, denotated @Θ@ in SI.
--
--  This may contain a temperature quantity with unspecified unit.
--
$(mkDim "Temperature" "Θ" 0x08080802)





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
$(mkDim "AmountOfSubstance" "N" 0x08080804)


-- | A quantity in moles, denotated @mol@ in SI.
--
-- This is the base unit of the amount of substance dimension in the SI
-- system.
--
$(mkBaseUnit "Mole" "mol" ''AmountOfSubstance)


-- | The luminous intensity dimension, denotated @J@ in SI.
--
--  This may contain a luminous intensity quantity with unspecified unit.
--
$(mkDim "LuminousIntensity" "J" 0x08080808)

-- | A quantity in candelas, denotated @cd@ in SI.
--
-- This is the base unit of the luminous intensity dimension in the SI
-- system.
--
$(mkBaseUnit "Candela" "cd" ''LuminousIntensity)

