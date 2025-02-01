{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

--------------------------------------------------------------------------------
-- |
--
-- Module      :  Pandia.Units.Prefix
-- Description :  Metric prefixes from the International System of Units
-- Copyright   :  (c) Alice Rixte 2024
-- License:  LGPL 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- This modules provides support for metric prefixes from the International
-- System of Units.
--
-- * Usage
--
-- ** Simple conversion
--
-- To convert between to prefixes, use composition:
--
-- @
-- >>> ('milli' . 'toMicro') 1
-- 1000.0
-- >>> ('centi' . 'toKilo') 1
-- 1.0e-5
-- @
--
-- ** Newtypes for precision
--
-- Use newtypes to specify the unit prefix, and use @'coerce'@ to convert
-- between them.
--
-- @
-- >>> a = 1 :: 'Milli' Double
-- >>> ('coerce' . 'milli' . 'toMicro') a :: 'Micro' Double
-- Micro 1000.0
-- @
-- Thanks to @'coerce'@, there is no runtime overhead from using these newtypes
-- (see @'Data.Coerce'@ in base).
--
-- However, be careful with coerce, as the type system will not prevent you from
-- doing this :
--
-- @
-- >>> a = 1 :: 'Milli' Double
-- >>> ('coerce' . 'milli' . 'toKilo') a :: 'Micro' Double
-- Micro 1.0e-6
-- @
--
-- If you need such a conversion, you should probably define a function like
--
-- @
-- milliToMicro :: 'Milli' a -> 'Micro' a
-- milliToMicro = 'coerce' . 'milli' . 'toMicro'
-- @
------------------------------------------------------------------------------

module Pandia.Units.Prefix
  ( module Pandia.Units.Prefix
  ) where

import Pandia.Units.Convertor
import Pandia.Units.Dimension

import Data.Proxy
import Data.Kind

----------------------------------- Milli ------------------------------------

newtype Milli (u :: Type -> Type) a = Milli (u a)
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
            , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

instance ToDimension u => ToDimension (Milli u) where
  type ToDim (Milli u) = ToDim u

instance (Num a, ConvertorClass u cd p a, MilliClass u cd p a)
  => ConvertorClass (Milli u) cd p a where
  convertor = milli convertor
  {-# INLINE convertor #-}

class MilliClass u cd p a where
  milli :: Convertor u cd p a -> Convertor (Milli u) cd p a

instance Fractional a => MilliClass u 'ToSI 'False a where
  milli u _ = (/ 1000) . runConvertor u
  {-# INLINE milli #-}

instance Num a => MilliClass u 'ToSI 'True a where
  milli u _ = (* 1000) . runConvertor u
  {-# INLINE milli #-}

instance Num a => MilliClass u 'FromSI 'False a where
  milli u _ = (* 1000) . runConvertor u
  {-# INLINE milli #-}

instance Fractional a => MilliClass u 'FromSI 'True a where
  milli u _ = (/ 1000) . runConvertor u
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

instance ToDimension u => ToDimension (Kilo u) where
  type ToDim (Kilo u) = ToDim u

instance (Num a, ConvertorClass u cd p a, KiloClass u cd p a)
  => ConvertorClass (Kilo u) cd p a where
  convertor = kilo convertor
  {-# INLINE convertor #-}

class KiloClass u cd p a where
  kilo :: Convertor u cd p a -> Convertor (Kilo u) cd p a

instance Num a => KiloClass u 'ToSI 'False a where
  kilo u _ = (* 1000) . runConvertor u
  {-# INLINE kilo #-}

instance Fractional a => KiloClass u 'ToSI 'True a where
  kilo u _ = (/ 1000) . runConvertor u
  {-# INLINE kilo #-}

instance Fractional a => KiloClass u 'FromSI 'False a where
  kilo u _ = (/ 1000) . runConvertor u
  {-# INLINE kilo #-}

instance Num a => KiloClass u 'FromSI 'True a where
  kilo u _ = (* 1000) . runConvertor u
  {-# INLINE kilo #-}

-- kilo :: forall u a. KiloClass a => Convertor u a -> Convertor (Kilo u) a
-- kilo u _ = kiloFun (u (Proxy :: Proxy u) :: a -> a)
-- {-# INLINE kilo #-}

-- instance (Num a, ConvertorClass u a, KiloClass a)
--   => ConvertorClass (Kilo u) a where
--   convertor _ = kiloFun (convertor (Proxy :: Proxy u))
--   {-# INLINE convertor #-}

-- class KiloClass a where
--   kiloFun :: (a -> a) -> a -> a

-- instance Num a => KiloClass (ToSI a) where
--   kiloFun u = u . (* 1000)
--   {-# INLINE kiloFun #-}

-- instance Fractional a => KiloClass (Per (ToSI a)) where
--   kiloFun u = u . (/ 1000)
--   {-# INLINE kiloFun #-}

-- instance Fractional a => KiloClass (FromSI a) where
--   kiloFun u = (/ 1000) . u
--   {-# INLINE kiloFun #-}

-- instance Num a => KiloClass (Per (FromSI a)) where
--   kiloFun u = (* 1000) . u
--   {-# INLINE kiloFun #-}

