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
import Data.Units.SI

-- | Time quantity in minutes
--
$(mkUnitFrom "Minute" "min" ''Time 60)

-- | Time quantity in hours
--
$(mkUnitFrom "Hour" "hr" ''Time 3600)


