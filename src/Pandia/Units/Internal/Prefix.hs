{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuantifiedConstraints #-}

--------------------------------------------------------------------------------
-- |
--
-- Module      :  Pandia.Units.Internal.Prefix
-- Description :  Metric prefixes from the International System of Units
-- Copyright   :  (c) Alice Rixte 2024
-- License:  LGPL 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- This modules provides support for metric prefixes from the International
-- System of Units.Internal.
--
------------------------------------------------------------------------------

module Pandia.Units.Internal.Prefix
  ( module Pandia.Units.Internal.Prefix
  ) where

import Pandia.Units.Internal.Convertor
import Pandia.Units.Internal.Dimension

import Data.Kind

------------------------------------ Nano ------------------------------------

newtype Nano (u :: Type -> Type) a = Nano (u a)
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
            , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

instance HasDim syst u => HasDim syst (Nano u) where
  type DimOf syst (Nano u) = DimOf syst u

instance (Num a, ConvertorClass u cd p a, NanoClass u cd p a)
  => ConvertorClass (Nano u) cd p a where
  convertor = nano convertor
  {-# INLINE convertor #-}

class NanoClass u cd p a where
  nano :: Convertor u cd p a -> Convertor (Nano u) cd p a

instance Fractional a => NanoClass u 'ToDimSys 'False a where
  nano u _ = runConvertor u . (/ 1000000000)
  {-# INLINE nano #-}

instance Fractional a => NanoClass u 'ToDimSys 'True a where
  nano u _ _ = unitMultiplier u / 1000000000
  {-# INLINE nano #-}

instance Num a => NanoClass u 'FromDimSys 'False a where
  nano u _ = (* 1000000000) . runConvertor u
  {-# INLINE nano #-}

instance Num a => NanoClass u 'FromDimSys 'True a where
  nano u _ _ = unitMultiplier u * 1000000000
  {-# INLINE nano #-}

----------------------------------- Micro ------------------------------------

newtype Micro (u :: Type -> Type) a = Micro (u a)
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
            , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

instance HasDim syst u => HasDim syst (Micro u) where
  type DimOf syst (Micro u) = DimOf syst u

instance (Num a, ConvertorClass u cd p a, MicroClass u cd p a)
  => ConvertorClass (Micro u) cd p a where
  convertor = micro convertor
  {-# INLINE convertor #-}

class MicroClass u cd p a where
  micro :: Convertor u cd p a -> Convertor (Micro u) cd p a

instance Fractional a => MicroClass u 'ToDimSys 'False a where
  micro u _ = runConvertor u . (/ 1000000)
  {-# INLINE micro #-}

instance Fractional a => MicroClass u 'ToDimSys 'True a where
  micro u _ _ = unitMultiplier u / 1000000
  {-# INLINE micro #-}

instance Num a => MicroClass u 'FromDimSys 'False a where
  micro u _ = (* 1000000) . runConvertor u
  {-# INLINE micro #-}

instance Num a => MicroClass u 'FromDimSys 'True a where
  micro u _ _ = unitMultiplier u * 1000000
  {-# INLINE micro #-}



----------------------------------- Milli ------------------------------------

newtype Milli (u :: Type -> Type) a = Milli (u a)
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
            , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

instance HasDim syst u => HasDim syst (Milli u) where
  type DimOf syst (Milli u) = DimOf syst u

instance (Num a, ConvertorClass u cd p a, MilliClass u cd p a)
  => ConvertorClass (Milli u) cd p a where
  convertor = milli convertor
  {-# INLINE convertor #-}

class MilliClass u cd p a where
  milli :: Convertor u cd p a -> Convertor (Milli u) cd p a

instance Fractional a => MilliClass u 'ToDimSys 'False a where
  milli u _ = runConvertor u . (/ 1000)
  {-# INLINE milli #-}

instance Fractional a => MilliClass u 'ToDimSys 'True a where
  milli u _ _ = unitMultiplier u / 1000
  {-# INLINE milli #-}

instance Num a => MilliClass u 'FromDimSys 'False a where
  milli u _ = (* 1000) . runConvertor u
  {-# INLINE milli #-}

instance Num a => MilliClass u 'FromDimSys 'True a where
  milli u _ _ = unitMultiplier u * 1000
  {-# INLINE milli #-}



----------------------------------- Deca ------------------------------------

-- newtype Deca (u :: Type -> Type) a = Deca (u a)
--   deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
--              , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

-- instance ToDimension u => ToDimension (Deca u) where
--   type ToDim (Deca u) = ToDim u

-- deca :: forall u a. DecaClass a => Convertor u a -> Convertor (Deca u) a
-- deca u _ = decaFun (u (Proxy :: Proxy u) :: a -> a)
-- {-# INLINE deca #-}

-- instance (Num a, ConvertorClass u a, DecaClass a)
--   => ConvertorClass (Deca u) a where
--   convertor _ = decaFun (convertor (Proxy :: Proxy u))
--   {-# INLINE convertor #-}

-- class DecaClass a where
--   decaFun :: (a -> a) -> a -> a

-- instance Num a => DecaClass (ToSI a) where
--   decaFun u = (* 10) . u
--   {-# INLINE decaFun #-}

-- instance Fractional a => DecaClass (Per (ToSI a)) where
--   decaFun u = (/ 10) . u
--   {-# INLINE decaFun #-}

-- instance Fractional a => DecaClass (FromSI a) where
--   decaFun u = (/ 10) . u
--   {-# INLINE decaFun #-}

-- instance Num a => DecaClass (Per (FromSI a)) where
--   decaFun u = (* 10) . u
--   {-# INLINE decaFun #-}

------------------------------------ Kilo ------------------------------------

newtype Kilo (u :: Type -> Type) a = Kilo (u a)
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
            , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

instance HasDim syst u => HasDim syst (Kilo u) where
  type DimOf syst (Kilo u) = DimOf syst u

instance (Num a, ConvertorClass u cd p a, KiloClass u cd p a)
  => ConvertorClass (Kilo u) cd p a where
  convertor = kilo convertor
  {-# INLINE convertor #-}

class KiloClass u cd p a where
  kilo :: Convertor u cd p a -> Convertor (Kilo u) cd p a

instance Num a => KiloClass u 'ToDimSys 'False a where
  kilo u _ =  runConvertor u . (*1000)
  {-# INLINE kilo #-}

instance Num a => KiloClass u 'ToDimSys 'True a where
  kilo u _ _  = unitMultiplier u * 1000
  {-# INLINE kilo #-}

instance Fractional a => KiloClass u 'FromDimSys 'False a where
  kilo u _  =  (/1000) . runConvertor u
  {-# INLINE kilo #-}

instance Fractional a => KiloClass u 'FromDimSys 'True a where
  kilo u _ _ =  unitMultiplier u / 1000
  {-# INLINE kilo #-}


