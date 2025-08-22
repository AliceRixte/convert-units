--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.NonStd.Time
-- Description :  Non standard time units
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- Non standard time units.
--
--------------------------------------------------------------------------------


module Data.Units.NonStd.Time
  ( module Data.Units.NonStd.Time
  ) where

import Data.Units.Base
import Data.Units.SI

-- | Time quantity in minutes
--
newtype Minute a = Minute a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via (MetaUnit Minute a)

instance IsUnit Minute where
  type DimOf Minute = Time
  type StdUnitOf Minute = Second

instance ShowUnit Minute where
  type ShowUnitType Minute = Text "min"
  showUnit = "Minute"
  prettyUnit = "min"


instance Fractional a => ConvFactor Minute a where
  factorFrom = 60

-- | Time quantity in hours
--
newtype Hour a = Hour a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via (MetaUnit Hour a)

instance IsUnit Hour where
  type DimOf Hour = Time
  type StdUnitOf Hour = Second

instance ShowUnit Hour where
  type ShowUnitType Hour = Text "hr"
  showUnit = "Hour"
  prettyUnit = "hr"

instance Fractional a => ConvFactor Hour a where
  factorFrom = 3600

