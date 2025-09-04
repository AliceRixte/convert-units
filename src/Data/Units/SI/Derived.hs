{-# LANGUAGE PatternSynonyms #-}

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


module Data.Units.SI.Derived
  ( -- * Official derived units from SI
    Frequency
  , Hertz (..)
  , Speed
  , Acceleration
  , Force
  , Newton
  , pattern Newton
  )
  where

import Data.Coerce

import Data.Units.Base
import Data.Units.SI.System
import Data.Units.SI.Angle
import Data.Units.SI.Prefixes

import Data.Coerce

type Frequency = Time .^- 1

-- | Frequency in hertz
--
$(mkUnitFrom "Hertz" "Hz" ''Frequency 1)

type Speed = Length ./. Time

type Acceleration = Length ./. (Time .^+ 2)

type Force = Mass .*. Acceleration

-- | Force in newtons
--
$(mkUnitFrom "Newton" "N" ''Force 1)

type Pressure = StandardizeDim (Force ./. (Length .^+ 2))

-- | Pressure in pascals
--
$(mkUnitFrom "Pascal" "Pa" ''Pressure 1)

















