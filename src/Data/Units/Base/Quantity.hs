{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Data.Units.Base.Quantity where

import Data.Kind

import Data.Units.Base.Unit

type Quantity = Type

class IsQuantity (q :: Quantity)  where
  type StandardQuantity q :: Quantity
  type UnitOf q :: Unit

instance IsUnit u => IsQuantity (u a) where
  type StandardQuantity (u a) = (StdUnitOf u) a
  type UnitOf (u a) = u

