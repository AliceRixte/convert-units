{-# LANGUAGE TemplateHaskell #-}



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

import Data.Units.Base.TH

import Data.Units.Base
import Data.Units.SI

$(mkUnit "Minuteee" "min" ''Time 60)

-- | Time quantity in minutes
--
newtype Minute a = Minute a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)

instance IsUnit Minute where
  type DimOf Minute = Time

instance ShowUnit Minute where
  type ShowUnitType Minute = Text "min"
  showUnit = "Minute"
  prettyUnit = "min"


instance Fractional a => ConvFactor Minute a where
  factorFrom = 60

-- | Time quantity in hours
--
newtype Hour a = Hour a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)

instance IsUnit Hour where
  type DimOf Hour = Time

instance ShowUnit Hour where
  type ShowUnitType Hour = Text "hr"
  showUnit = "Hour"
  prettyUnit = "hr"

instance Fractional a => ConvFactor Hour a where
  factorFrom = 3600

