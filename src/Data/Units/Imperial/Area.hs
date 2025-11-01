module Data.Units.Imperial.Area
  ( Perch(..)
  , Rood(..)
  ) where

import Data.Units.Base
import Data.Units.SI

-- | Area in perch
--
$(mkUnit "Perch" "perch" ''Area $ (1200 / 3937)^2 * 1089 / 4)

-- | Area in rood
--
$(mkUnit "Rood" "ro" ''Area $ (1200 / 3937)^2 * 10890)
