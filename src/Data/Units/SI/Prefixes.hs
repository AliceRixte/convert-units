{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.SI.Prefixes
-- Description :  Prefixes of the International System of Units
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Prefixes for the International System of Units (abbreviated SI).
--
--------------------------------------------------------------------------------

module Data.Units.SI.Prefixes
  ( Quecto (..)
  , Ronto (..)
  , Yocto (..)
  , Zepto (..)
  , Atto (..)
  , Femto (..)
  , Pico (..)
  , Nano (..)
  , Micro (..)
  , Milli (..)
  , Centi (..)
  , Deci (..)
  , Deca (..)
  , Hecto (..)
  , Kilo (..)
  , Mega (..)
  , Giga (..)
  , Tera (..)
  , Peta (..)
  , Exa (..)
  , Zetta (..)
  , Yotta (..)
  , Ronna (..)
  , Quecca (..)
  ) where

import Data.Units.Base

-- | SI prefix for 10⁻³⁰
--
$(mkPrefix "Quecto" "q" 1e-30)

-- | SI prefix for 10⁻²⁷
--
$(mkPrefix "Ronto" "r" 1e-27)

-- | SI prefix for 10⁻²⁴
--
$(mkPrefix "Yocto" "y" 1e-24)

-- | SI prefix for 10⁻²¹
--
$(mkPrefix "Zepto" "z" 1e-21)

-- | SI prefix for 10⁻¹⁸
--
$(mkPrefix "Atto" "a" 1e-18)

-- | SI prefix for 10⁻¹⁵
--
$(mkPrefix "Femto" "f" 1e-15)

-- | SI prefix for 10⁻¹²
--
$(mkPrefix "Pico" "p" 1e-12)

-- | SI prefix for 10⁻⁹
--
$(mkPrefix "Nano" "n" 1e-9)

-- | SI prefix for 10⁻⁶
--
$(mkPrefix "Micro" "µ" 1e-6)

-- | SI prefix for 10⁻³
--
$(mkPrefix "Milli" "m" 1e-3)

-- | SI prefix for 10⁻²
--
$(mkPrefix "Centi" "c" 1e-2)

-- | SI prefix for 10⁻¹
--
$(mkPrefix "Deci" "d" 1e-1)

-- | SI prefix for 10¹
--
$(mkPrefix "Deca" "da" 1e1)

-- | SI prefix for 10²
--
$(mkPrefix "Hecto" "h" 1e2)

-- | SI prefix for 10³
--
$(mkPrefix "Kilo" "k" 1e3)

-- | SI prefix for 10⁶
--
$(mkPrefix "Mega" "M" 1e6)

-- | SI prefix for 10⁹
--
$(mkPrefix "Giga" "G" 1e9)

-- | SI prefix for 10¹²
--
$(mkPrefix "Tera" "T" 1e12)

-- | SI prefix for 10¹⁵
--
$(mkPrefix "Peta" "P" 1e15)

-- | SI prefix for 10¹⁸
--
$(mkPrefix "Exa" "E" 1e18)

-- | SI prefix for 10²¹
--
$(mkPrefix "Zetta" "Z" 1e21)

-- | SI prefix for 10²⁴
--
$(mkPrefix "Yotta" "Y" 1e24)

-- | SI prefix for 10²⁷
--
$(mkPrefix "Ronna" "R" 1e27)

-- | SI prefix for 10³⁰
--
$(mkPrefix "Quecca" "Q" 1e30)
