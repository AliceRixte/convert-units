{-# LANGUAGE TemplateHaskell #-}
module Data.Units.UsCustomary.Length
-- Reexport the imperial units that are also used in US customary within the US
-- customary modules
  ( Mil
  , Point
  , Pica
  , Data.Units.Imperial.Length.Twip
  , Data.Units.Imperial.Length.Inch
  , Data.Units.Imperial.Length.Foot
  , Data.Units.Imperial.Length.Yard
  , Data.Units.Imperial.Length.Mile
  , Data.Units.Imperial.Length.League
  , Data.Units.Imperial.Length.Fathom
  , Data.Units.Imperial.Length.Cable
  , Data.Units.Imperial.Length.NauticalMile
  ) where

import Data.Units.Core
import Data.Units.SI
import Data.Units.Imperial.Length


-- | Length in mils.
$(mkUnit "Mil" "mil" ''Length $ 1200 / 3937 / 12 / 1000)

-- | Length in points.
$(mkUnit "Point" "p" ''Length $ 1200 / 3937 / 12 / 72)

-- | Length in picas.
$(mkUnit "Pica" "P" ''Length $ 1200 / 3937 / 12 / 72 * 12)
