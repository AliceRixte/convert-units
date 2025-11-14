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

import Data.Units.Core
import Data.Units.SI


-- | Length in feet.
$(mkUnit "Foot" "ft" ''Length $ 1200 / 3937)

-- | Length in twips.
$(mkUnit "Twip" "twip" ''Length $ 1200 / 3937 / 17280)

-- | Length in thous.
$(mkUnit "Thou" "th" ''Length $ 1200 / 3937 / 12000)

-- | Length in barleycorns.
$(mkUnit "Barleycorn" "barleycorn" ''Length $ 1200 / 3937 / 36)

-- | Length in inches.
$(mkUnit "Inch" "in" ''Length $ 1200 / 3937 / 12)

-- | Length in hands.
$(mkUnit "Hand" "hh" ''Length $ 1200 / 3937 / 3)

-- | Length in yards.
$(mkUnit "Yard" "yd" ''Length $ 1200 / 3937 * 3)

-- | Length in chains.
$(mkUnit "Chain" "ch" ''Length $ 1200 / 3937 * 66)

-- | Length in furlongs.
$(mkUnit "Furlong" "fur" ''Length $ 1200 / 3937 * 660)

-- | Length in miles.
$(mkUnit "Mile" "mi" ''Length $ 1200 / 3937 * 5280)

-- | Length in leagues.
$(mkUnit "League" "lea" ''Length $ 1200 / 3937 * 15840)

-- | Length in fathoms.
$(mkUnit "Fathom" "ftm" ''Length $ 1200 / 3937 * 6.0761)

-- | Length in cables.
$(mkUnit "Cable" "cable" ''Length $ 1200 / 3937 * 607.61)

-- | Length in nautical miles.
$(mkUnit "NauticalMile" "nmi" ''Length $ 1200 / 3937 * 60761 / 10)

-- | Length in links.
$(mkUnit "Link" "lnk" ''Length $ 1200 / 3937 * 66 / 100)

-- | Length in rods.
$(mkUnit "Rod" "rod" ''Length $ 1200 / 3937 * 66 / 4)
