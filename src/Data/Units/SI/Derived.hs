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
  , Pressure
  , Newton (..)
  , Pascal (..)
  , Joule (..)
  , Watt (..)
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
$(mkUnit "Hertz" "Hz" ''Frequency 1)

type Speed = NormalizeDim (Length ./. Time)

type Acceleration = NormalizeDim (Length ./. Time .^+ 2)

type Force = NormalizeDim (Mass .*. Acceleration)

-- | Force in newtons
--
$(mkUnit "Newton" "N" ''Force 1)

type Pressure = NormalizeDim (Force ./. (Length .^+ 2))
type Stress = NormalizeDim (Force ./. (Length .^+ 2))

-- | Pressure in pascals
--
$(mkUnit "Pascal" "Pa" ''Pressure 1)

-- | Energy quantity. Corresponds to (Mass .*. Length.^2 ./. Time.^2).
--
type Energy = NormalizeDim (Length .*. Force)
type Work = Energy
type Heat = Energy

-- | Energy in joules
--
$(mkUnit "Joule" "J" ''Energy 1)

type Power = NormalizeDim (Energy ./. Time)
type RadiantFlux = Power

$(mkUnit "Watt" "W" ''Power 1)

type ElectricCharge = NormalizeDim (Time .*. Current)
type QuantityOfElectricity = ElectricCharge

$(mkUnit "Coulomb" "C" ''ElectricCharge 1)

type Voltage = NormalizeDim (Power ./. Current)
type ElectricPotential = Voltage
type ElectromotiveForce = Voltage

$(mkUnit "Volt" "V" ''Voltage 1)

type Capacitance = NormalizeDim (ElectricCharge ./. Voltage)

$(mkUnit "Farad" "F" ''Capacitance 1)

type Resistance = NormalizeDim (Voltage ./. Current)
type Impedance = Resistance
type Reactance = Resistance

$(mkUnit "Ohm" "Ω" ''Resistance 1)

type Conductance = NormalizeDim (Current ./. Voltage)

$(mkUnit "Siemens" "S" ''Conductance 1)

type MagneticFlux = NormalizeDim (Voltage .*. Time)

$(mkUnit "Weber" "Wb" ''MagneticFlux 1)

type MagneticInduction = NormalizeDim (MagneticFlux ./. (Length .^+ 2))
type MagneticFluxDensity = MagneticInduction

$(mkUnit "Tesla" "T" ''MagneticInduction 1)

type Inductance = NormalizeDim (MagneticFlux ./. Current)

-- | Thermodynamic temperature in Celsius degrees
--
$(mkUnitNoFactor "Celsius" "°C" ''Temperature)

instance Fractional a => ConversionFactor Celsius a where
  factor = 1
  {-# INLINE factor #-}

instance Fractional a => ConvertibleUnit Celsius a where
  toNormalUnit (Celsius x) = Kelvin (x + 273.15)
  {-# INLINE toNormalUnit #-}

  fromNormalUnit (Kelvin x) = Celsius (x - 273.15)
  {-# INLINE fromNormalUnit #-}

