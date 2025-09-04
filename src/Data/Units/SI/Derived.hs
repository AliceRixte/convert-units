module Data.Units.SI.Derived where

import Data.Units.Base
import Data.Units.SI.System

import Data.Coerce

-- type Hertz = Second .^- 1
-- | Frequency in Hertz
--  .*. .+. .-.
newtype Hertz a = Hertz a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via MetaUnit Hertz a

instance Fractional a => ConvFactor Hertz a where
  factorFrom = 1

instance Fractional a => ConvertibleUnit Hertz a where
  from = coerce
  {-# INLINE from #-}

  to = coerce
  {-# INLINE to #-}

instance IsUnit Hertz where
  type DimOf Hertz = Time .^- 1

instance ShowUnit Hertz where
  type ShowUnitType Hertz = Text "Hz"
  showUnit = "Hertz"
  prettyUnit = "Hz"