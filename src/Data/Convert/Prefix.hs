module Data.Convert.Prefix where

import Data.Convert.FromTo
import Data.Convert.Unit

newtype Kilo (u :: Unit) a = Kilo (u a)
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
            , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

type instance Standard (Kilo u a) = u a

instance (Num a, Dimensional (u a) a)
  => Dimensional (Kilo u a) a where
  factor = 1000

