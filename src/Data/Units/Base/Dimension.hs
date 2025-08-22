{-# LANGUAGE DeriveFunctor #-}

--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.Base.Dimension
-- Description :  Dimensions of a unit system
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- This module provides an extensible way to define dimensions for any unit
-- system.
--
-- Dimensions multiplication @'(-*-)'@ and exponentiation @'(-^-)'@ are
-- implemented the same way as unit multiplication and exponentiation, and
-- therefore implemented in @Data.Units.Base.Unit@.
--
--------------------------------------------------------------------------------

module Data.Units.Base.Dimension
  ( Dim
  , ShowDim
  , DimId
  , NoDim
  )
where

import Data.Kind
import GHC.TypeLits

-- | A unit dimension.
--
--  Modeled as a newtype constructor, just like @'Unit'@.
--
-- >>> type Speed = Length -/- Time
--
type Dim = Type -> Type

-- | A dimension identifier.
--
-- This identifiers allow to sort the units when computing the standard unit.
--
-- >>> type instance DimId Length = 300
--
-- >>> :kind! StdUnitOf (Second -^~ 1 -*- Meter)
-- Meter -*- (Second -^- Neg 1)
--
--
-- Two different dimensions must have different identifiers. To make sure this
-- remains true, we maintain here an //exhaustive// list of dimensions declared
-- in this package //and// any package that depends on it. Please raise an issue
-- if you added a new dimension.
--
-- [This package:]
--
--  +--------------------------------------+-----+
--  | Dimension                            | Id  |
--  +======================================+=====+
--  | @'NoDim'@                            | 000 |
--  +--------------------------------------+-----+
--  | @'Angle'@                            | 100 |
--  +--------------------------------------+-----+
--  | @'Data.Units.SI.Mass'@               | 200 |
--  +--------------------------------------+-----+
--  | @'Data.Units.SI.Length'@             | 300 |
--  +--------------------------------------+-----+
--  | @'Data.Units.SI.Time'@               | 400 |
--  +--------------------------------------+-----+
--  | @'Data.Units.SI.ElectricCurrent'@    | 500 |
--  +--------------------------------------+-----+
--  | @'Data.Units.SI.Temperature'@        | 600 |
--  +--------------------------------------+-----+
--  | @'Data.Units.SI.SubstanceAmount'@    | 700 |
--  +--------------------------------------+-----+
--  | @'Data.Units.SI.LuminousIntensity'@  | 800 |
--  +--------------------------------------+-----+
--
type family DimId (d:: Dim) :: Nat

-- | Pretty print a dimension in error messages
--
-- >>> type instance ShowDim Length = Text "L"
--
-- Using the following in a @'TypeError'@
--
-- @
-- ShowDim (Length -*- Time -^~ 1)
-- @
--
-- will show @L.T⁻¹@
--
type family ShowDim (d :: Dim) :: ErrorMessage


-- | The dimension of non dimensional quantities
--
newtype NoDim a = NoDim a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)


type instance DimId NoDim = 0
type instance ShowDim NoDim = Text "NoDim"



--------------------------------------------------------------------------------


