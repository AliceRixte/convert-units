
--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.AngleSI.NonStd.Angle
-- Description :  Non standard angle units
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- Non standard angle units with a dimension A
--
--------------------------------------------------------------------------------


module Data.Units.AngleSI.NonStd.Angle where

import Data.Units.Base

import Data.Units.AngleSI.Angle

-- | Angle in degrees.
--
newtype Degree a = Degree a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via MetaUnit Degree a

instance Floating a => ConvFactor Degree a where
  factorFrom = pi / 180

instance IsUnit Degree where
  type DimOf Degree = Angle
  type StdUnitOf Degree = Radian

instance ShowUnit Degree where
  type ShowUnitType Degree = Text "°"
  showUnit = "Degree"
  prettyUnit = "°"


-- | Angle in complete turns (also called cycles or revolutions)
--
-- See https://en.wikipedia.org/wiki/Turn_(angle)
--
newtype Turn a = Turn a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via MetaUnit Turn a

instance Floating a => ConvFactor Turn a where
  factorFrom = 2 * pi

instance IsUnit Turn where
  type DimOf Turn = Angle
  type StdUnitOf Turn = Radian

instance ShowUnit Turn where
  type ShowUnitType Turn = Text "tr"
  showUnit = "Turn"
  prettyUnit = "tr"



-- | Angle in gradians
--
-- See https://en.wikipedia.org/wiki/Gradian
--
newtype Gradian a = Gradian a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via MetaUnit Gradian a

instance Floating a => ConvFactor Gradian a where
  factorFrom = pi / 200

instance IsUnit Gradian where
  type DimOf Gradian = Angle
  type StdUnitOf Gradian = Radian

instance ShowUnit Gradian where
  type ShowUnitType Gradian = Text "grad"
  showUnit = "Gradian"
  prettyUnit = "grad"
