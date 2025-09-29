{-# LANGUAGE TemplateHaskell #-}
module Data.Units.NonStd.Length
  ( Twip(..)
  , Thou(..)
  , Barleycorn(..)
  , Inch(..)
  , Hand(..)
  , Foot(..)
  , Yard(..)
  , Chain(..)
  , Furlong(..)
  , Mile(..)
  , League(..)
  , Fathom(..)
  , Cable(..)
  , NauticalMile(..)
  , Link(..)
  , Rod(..)
  ) where

import Data.Units.Base
import Data.Units.SI

-- | Length in Feet
--

-- TODO: add doc for each

$(mkUnit "Foot" "ft" ''Length $ 1200 / 3937) -- XXX is this meant ot use
                                             -- mkBaseUnit and somehow the other
                                             -- are not meant to use the 1200/3937 factor?

-- XXX: what about those without abbreviation or symbol?
-- XXX: Do we just lowercase the name of the unit, e.g. Barleycorn->barleycorn?
$(mkUnit "Twip" "" ''Length $ 1200 / 3937 / 17280)
$(mkUnit "Thou" "th" ''Length $ 1200 / 3937 / 12000) -- XXX in reality this is a different name and different symbol for Mil
$(mkUnit "Barleycorn" "" ''Length $ 1200 / 3937 / 36)
$(mkUnit "Inch" "in" ''Length $ 1200 / 3937 / 12)
$(mkUnit "Hand" "hh" ''Length $ 1200 / 3937 / 3)
$(mkUnit "Yard" "yd" ''Length $ 1200 / 3937 * 3)
$(mkUnit "Chain" "ch" ''Length $ 1200 / 3937 * 66)
$(mkUnit "Furlong" "fur" ''Length $ 1200 / 3937 * 660)
$(mkUnit "Mile" "mi" ''Length $ 1200 / 3937 * 5280)
$(mkUnit "League" "lea" ''Length $ 1200 / 3937 * 15840)
$(mkUnit "Fathom" "ftm" ''Length $ 1200 / 3937 * 6.0761)
$(mkUnit "Cable" "" ''Length $ 1200 / 3937 * 607.61)
$(mkUnit "NauticalMile" "nmi" ''Length $ 1200 / 3937 * 60761 / 10)
$(mkUnit "Link" "" ''Length $ 1200 / 3937 * 66 / 100)
$(mkUnit "Rod" "" ''Length $ 1200 / 3937 * 66 / 4)
