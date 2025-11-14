--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.NonSI.Time
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


module Data.Units.NonSI.Time
  ( module Data.Units.NonSI.Time
  ) where

import Data.Units.Core
import Data.Units.SI

-- | Time in minutes.
$(mkUnit "Minute" "min" ''Time 60)

-- | Time in hours.
$(mkUnit "Hour" "hr" ''Time 3600)


