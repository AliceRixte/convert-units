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
$(mkPrefixFrom "Quecto" "q" 1e-30)

-- | SI prefix for 10⁻²⁷
--
$(mkPrefixFrom "Ronto" "r" 1e-27)

-- | SI prefix for 10⁻²⁴
--
$(mkPrefixFrom "Yocto" "y" 1e-24)

-- | SI prefix for 10⁻²¹
--
$(mkPrefixFrom "Zepto" "z" 1e-21)

-- | SI prefix for 10⁻¹⁸
--
$(mkPrefixFrom "Atto" "a" 1e-18)

-- | SI prefix for 10⁻¹⁵
--
$(mkPrefixFrom "Femto" "f" 1e-15)

-- | SI prefix for 10⁻¹²
--
$(mkPrefixFrom "Pico" "p" 1e-12)

-- | SI prefix for 10⁻⁹
--
$(mkPrefixFrom "Nano" "n" 1e-9)

-- | SI prefix for 10⁻⁶
--
$(mkPrefixFrom "Micro" "µ" 1e-6)

-- | SI prefix for 10⁻³
--
$(mkPrefixFrom "Milli" "m" 1e-3)

-- | SI prefix for 10⁻²
--
$(mkPrefixFrom "Centi" "c" 1e-2)

-- | SI prefix for 10⁻¹
--
$(mkPrefixFrom "Deci" "d" 1e-1)

-- | SI prefix for 10¹
--
$(mkPrefixFrom "Deca" "da" 1e1)

-- | SI prefix for 10²
--
$(mkPrefixFrom "Hecto" "h" 1e2)

-- | SI prefix for 10³
--
$(mkPrefixFrom "Kilo" "k" 1e3)

-- | SI prefix for 10⁶
--
$(mkPrefixFrom "Mega" "M" 1e6)

-- | SI prefix for 10⁹
--
$(mkPrefixFrom "Giga" "G" 1e9)

-- | SI prefix for 10¹²
--
$(mkPrefixFrom "Tera" "T" 1e12)

-- | SI prefix for 10¹⁵
--
$(mkPrefixFrom "Peta" "P" 1e15)

-- | SI prefix for 10¹⁸
--
$(mkPrefixFrom "Exa" "E" 1e18)

-- | SI prefix for 10²¹
--
$(mkPrefixFrom "Zetta" "Z" 1e21)

-- | SI prefix for 10²⁴
--
$(mkPrefixFrom "Yotta" "Y" 1e24)

-- | SI prefix for 10²⁷
--
$(mkPrefixFrom "Ronna" "R" 1e27)

-- | SI prefix for 10³⁰
--
$(mkPrefixFrom "Quecca" "Q" 1e30)
