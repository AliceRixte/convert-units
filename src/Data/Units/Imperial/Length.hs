{-# LANGUAGE TemplateHaskell #-}
module Data.Units.Imperial.Length
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


-- | Length in foot XXX Sounds ugly, I'd write feet, but do I pluralise all
-- the others too? Many seem ok, but... `thous`?????
$(mkUnit "Foot" "ft" ''Length $ 1200 / 3937)

-- | Length in twip
$(mkUnit "Twip" "twip" ''Length $ 1200 / 3937 / 17280)

-- | Length in thou
$(mkUnit "Thou" "th" ''Length $ 1200 / 3937 / 12000)

-- | Length in barleycorn
$(mkUnit "Barleycorn" "barleycorn" ''Length $ 1200 / 3937 / 36)

-- | Length in inch
$(mkUnit "Inch" "in" ''Length $ 1200 / 3937 / 12)

-- | Length in hand
$(mkUnit "Hand" "hh" ''Length $ 1200 / 3937 / 3)

-- | Length in yard
$(mkUnit "Yard" "yd" ''Length $ 1200 / 3937 * 3)

-- | Length in chain
$(mkUnit "Chain" "ch" ''Length $ 1200 / 3937 * 66)

-- | Length in furlong
$(mkUnit "Furlong" "fur" ''Length $ 1200 / 3937 * 660)

-- | Length in mile
$(mkUnit "Mile" "mi" ''Length $ 1200 / 3937 * 5280)

-- | Length in league
$(mkUnit "League" "lea" ''Length $ 1200 / 3937 * 15840)

-- | Length in fathom
$(mkUnit "Fathom" "ftm" ''Length $ 1200 / 3937 * 6.0761)

-- | Length in cable
$(mkUnit "Cable" "cable" ''Length $ 1200 / 3937 * 607.61)

-- | Length in nauticalMile
$(mkUnit "NauticalMile" "nmi" ''Length $ 1200 / 3937 * 60761 / 10)

-- | Length in link
$(mkUnit "Link" "lnk" ''Length $ 1200 / 3937 * 66 / 100)

-- | Length in rod
$(mkUnit "Rod" "rod" ''Length $ 1200 / 3937 * 66 / 4)
