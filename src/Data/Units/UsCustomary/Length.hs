{-# LANGUAGE TemplateHaskell #-}
module Data.Units.UsCustomary.Length
  ( Mil
  , Point
  , Pica
  ) where

import Data.Units.Base
import Data.Units.SI

-- XXX I found the 3 below at https://en.wikipedia.org/wiki/United_States_customary_units
-- but not at https://en.wikipedia.org/wiki/Imperial_units
$(mkUnit "Mil" "mil" ''Length $ 1200 / 3937 / 12 / 1000)
$(mkUnit "Point" "p" ''Length $ 1200 / 3937 / 12 / 72)
$(mkUnit "Pica" "P" ''Length $ 1200 / 3937 / 12 / 72 * 12)
