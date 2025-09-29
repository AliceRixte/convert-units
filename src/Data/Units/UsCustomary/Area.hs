module Data.Units.UsCustomary.Area
  ( Acre(..)
  ) where

import Data.Units.Base
import Data.Units.SI

-- | Area in acre
--
$(mkUnit "Acre" "ac" ''Area $ (1200 / 3937)^2 * 43560)
