--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.SI.System
-- Description :  Derived units and dimensions for the International System of Units
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Derived units and dimensions for the International System of Units.
--
--------------------------------------------------------------------------------


module Data.Units.SI.Derived where

import Data.Units.Base
import Data.Units.SI.System

import Data.Coerce

type Frequency = Time .^- 1

-- | Frequency in hertz
--
-- @1 Hz = 1 s⁻¹@
--
$(mkUnitFrom "Hertz" "Hz" ''Frequency 1)
