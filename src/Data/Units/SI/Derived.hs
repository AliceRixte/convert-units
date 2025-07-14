module Data.Units.SI.Derived where

import Data.Units.Base
import Data.Units.SI.Units

-- | Frequency in Hertz
--
newtype Hertz a = Hertz a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via MetaUnit Hertz a

instance Fractional a => ConvFactor Hertz a where
  factorFrom = 1

instance IsUnit Hertz where
  type StdUnitOf Hertz = Second -^~ 1

instance ShowUnit Hertz where
  type ShowUnitType Hertz = Text "Hz"
  showUnit = "Hertz"
  prettyUnit = "Hz"