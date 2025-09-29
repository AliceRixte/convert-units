module Data.Units.Imperial.Area
  ( Acre(..)
  , Perch(..)
  , Rood(..)
  ) where

import Data.Units.Base
import Data.Units.SI

-- | Area in acres.
--
$(mkUnit "Acre" "ac" ''Area $ (1200 / 3937)^2 * 43560)

-- | Area in perches.
--
$(mkUnit "Perch" "perch" ''Area $ (1200 / 3937)^2 * 1089 / 4)

-- | Area in roods.
--
$(mkUnit "Rood" "ro" ''Area $ (1200 / 3937)^2 * 10890)
