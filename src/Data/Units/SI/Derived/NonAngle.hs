--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.SI.System
-- Description :  SI derived units not containing angles.
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Derived units and dimensions for the International System of Units that do
-- not contain angles.
--
--------------------------------------------------------------------------------


module Data.Units.SI.Derived.NonAngle
  ( -- * Official derived units from SI
    module Data.Units.SI.Derived.NonAngle
  )
  where

import Data.Units.Base
import Data.Units.SI.System

type Area = Length .^+ 2
type Volume = Length .^+ 3

type Frequency = Time .^- 1

-- | Frequency in hertz
--
$(mkUnit "Hertz" "Hz" ''Frequency 1)

-- | Speed quantity. Equal to
--
-- @'Length' .*. 'Time'.^-1@
--
type Speed = NormalizeDim (Length ./. Time)

type Acceleration = NormalizeDim (Length ./. Time .^+ 2)

-- | Acceleration quantity. Equal to
--
-- @'Length' .*. 'Time'.^-2@
--
type Force = NormalizeDim (Mass .*. Acceleration)

-- | Force in newtons
--
$(mkUnit "Newton" "N" ''Force 1)

-- | Pressure quantity. Equal to
--
-- @'Mass' .*. 'Length'.^-1 .*. 'Time'.^-2@
--
type Pressure = NormalizeDim (Force ./. (Length .^+ 2))
type Stress = Pressure

-- | Pressure in pascals
--
$(mkUnit "Pascal" "Pa" ''Pressure 1)

-- | Energy quantity. Equal to
--
-- @ 'Mass' .*. 'Length'.^+2 .*. 'Time'.^-2@
--
type Energy = NormalizeDim (Length .*. Force)
type Work = Energy
type Heat = Energy

-- | Energy in joules
--
$(mkUnit "Joule" "J" ''Energy 1)

-- | Power quantity. Equal to
--
-- @ 'Mass' .*. 'Length'.^+2 .*. 'Time'.^-3 @
--


type Power = NormalizeDim (Energy ./. Time)
type RadiantFlux = Power

-- | Power in watts
--
$(mkUnit "Watt" "W" ''Power 1)

-- | Electric charge quantity. Equal to
--
-- @ 'Time' .*. 'Current'@
--
type ElectricCharge = NormalizeDim (Time .*. Current)
type QuantityOfElectricity = ElectricCharge


$(mkUnit "Coulomb" "C" ''ElectricCharge 1)

-- | Electric voltage quantity. Equal to
--
-- @ 'Mass' .*. 'Length'.^+2 .*. 'Time'.^-3 .*. 'Current'.^-1@
--
type Voltage = NormalizeDim (Power ./. Current)
type ElectricPotential = Voltage
type ElectromotiveForce = Voltage

$(mkUnit "Volt" "V" ''Voltage 1)

-- | Electric capacitance quantity. Equal to
--
-- @'Mass'.^-1 .*. 'Length'.^-2 .*. 'Time'.^+4 .*. 'Current'.^+2@.
--
type Capacitance = NormalizeDim (ElectricCharge ./. Voltage)

$(mkUnit "Farad" "F" ''Capacitance 1)

-- | Electric resistance quantity. Equal to
--
-- @ 'Mass' .*. 'Length'.^+2 .*. 'Time'.^-3 .*. 'Current'.^-2@
--
type Resistance = NormalizeDim (Voltage ./. Current)
type Impedance = Resistance
type Reactance = Resistance

$(mkUnit "Ohm" "Ω" ''Resistance 1)

-- | Electric conductance quantity. Equal to
--
-- @ Mass.^-1 .*. Length.^-2 .*. Time.^+3 .*. Current.^+2 @
--
type Conductance = NormalizeDim (Current ./. Voltage)

$(mkUnit "Siemens" "S" ''Conductance 1)

-- | Magnetic flux quantity. Equal to
--
-- @ 'Mass' .*. 'Length'.^+2 .*. 'Time'.^-2 .*. 'Current'.^-1@
--
type MagneticFlux = NormalizeDim (Voltage .*. Time)

$(mkUnit "Weber" "Wb" ''MagneticFlux 1)

-- | Magnetic induction quantity. Equal to
--
-- @ 'Mass' .*. 'Time'.^-2 .*. 'Current'.^-1@
--
type MagneticInduction = NormalizeDim (MagneticFlux ./. (Length .^+ 2))
type MagneticFluxDensity = MagneticInduction

$(mkUnit "Tesla" "T" ''MagneticInduction 1)

-- | Inductance quantity. Equal to
--
-- @ 'Mass' .*. 'Length'.^+2 .*. 'Time'.^-2 .*. 'Current'.^-2@

type Inductance = NormalizeDim (MagneticFlux ./. Current)

$(mkUnit "Henry" "H" ''Inductance 1)

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

