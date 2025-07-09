{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Units.Prefix where

import Data.Kind

import Data.Convert.FromTo
import Data.Units.Base


type Prefix = Unit -> Type -> Type

class ShowPrefix (p :: Prefix) where
  showPrefix :: String

newtype MetaPrefix (p :: Prefix) a = MetaPrefix a

-- instance (ShowUnit u, ShowPrefix p) => IsUnit (MetaPrefix (p u)) where
--   type StdUnitOf (MetaPrefix (p u)) = u



instance ShowUnit u => IsUnit (MetaPrefix (p u)) where
  type StdUnitOf (MetaPrefix (p u)) = u

newtype Kilo (u :: Unit) a = Kilo (u a)
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
            , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)


instance IsUnit u => IsUnit (Kilo u) where
  type StdUnitOf (Kilo u) = u

instance (Num a, ConvFactor u a)
  => ConvFactor (Kilo u) a where
  factorFrom = 1000
