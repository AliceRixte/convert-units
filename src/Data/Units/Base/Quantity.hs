{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Data.Units.Base.Quantity where

import Data.Kind
import Data.Coerce
import Data.Proxy

import Data.Type.Int

import Data.Units.Base.Unit



type Quantity = Type

class (Coercible q a, Fractional a) => IsQuantity q a | q -> a where
  convFactor :: a

instance Fractional a => IsQuantity (StdUnit u a) a where
  convFactor = 1

instance Fractional a => IsQuantity (NoUnit a) a where
  convFactor = 1

instance (Num a, IsQuantity (u a) a, IsQuantity (v a) a)
  =>  IsQuantity ((u -*- v) a) a where
  convFactor = convFactor @(u a) * convFactor @(v a)

instance (IsQuantity (u a) a, KnownInt n)
  =>  IsQuantity ((u -^- n) a) a where
  convFactor = convFactor @(u a) ^^ intVal (Proxy :: Proxy n)


forgetUnit :: IsQuantity q a => q -> a
forgetUnit = coerce

toQuantity :: forall q a. IsQuantity q a => a -> q
toQuantity = coerce



