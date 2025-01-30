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

import Data.Proxy
import Data.Kind

----------------------------------- Milli ------------------------------------

newtype Milli (f :: Type -> Type) a = Milli (f a)
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
            , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)
milli :: forall f a. MilliClass a => Convertor f a -> Convertor (Milli f) a
milli f _ = milliFun (f (Proxy :: Proxy f) :: a -> a)
{-# INLINE milli #-}

instance (Num a, ConvertorClass f a, MilliClass a)
  => ConvertorClass (Milli f) a where
  convertor _ = milliFun (convertor (Proxy :: Proxy f))
  {-# INLINE convertor #-}

class MilliClass a where
  milliFun :: (a -> a) -> a -> a

instance Fractional a => MilliClass (From a) where
  milliFun f = (/ 1000) . f
  {-# INLINE milliFun #-}

instance Num a => MilliClass (Per (From a)) where
  milliFun f = (* 1000) . f
  {-# INLINE milliFun #-}

instance Num a => MilliClass (To a) where
  milliFun f = (* 1000) . f
  {-# INLINE milliFun #-}

instance Fractional a => MilliClass (Per (To a)) where
  milliFun f = (/ 1000) . f
  {-# INLINE milliFun #-}

----------------------------------- Deca ------------------------------------

newtype Deca (f :: Type -> Type) a = Deca (f a)
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
             , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

deca :: forall f a. DecaClass a => Convertor f a -> Convertor (Deca f) a
deca f _ = decaFun (f (Proxy :: Proxy f) :: a -> a)
{-# INLINE deca #-}

instance (Num a, ConvertorClass f a, DecaClass a)
  => ConvertorClass (Deca f) a where
  convertor _ = decaFun (convertor (Proxy :: Proxy f))
  {-# INLINE convertor #-}

class DecaClass a where
  decaFun :: (a -> a) -> a -> a

instance Num a => DecaClass (From a) where
  decaFun f = (* 10) . f
  {-# INLINE decaFun #-}

instance Fractional a => DecaClass (Per (From a)) where
  decaFun f = (/ 10) . f
  {-# INLINE decaFun #-}

instance Fractional a => DecaClass (To a) where
  decaFun f = (/ 10) . f
  {-# INLINE decaFun #-}

instance Num a => DecaClass (Per (To a)) where
  decaFun f = (* 10) . f
  {-# INLINE decaFun #-}

------------------------------------ Kilo ------------------------------------

newtype Kilo (f :: Type -> Type) a = Kilo (f a)
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
            , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

kilo :: forall f a. KiloClass a => Convertor f a -> Convertor (Kilo f) a
kilo f _ = kiloFun (f (Proxy :: Proxy f) :: a -> a)
{-# INLINE kilo #-}

instance (Num a, ConvertorClass f a, KiloClass a)
  => ConvertorClass (Kilo f) a where
  convertor _ = kiloFun (convertor (Proxy :: Proxy f))
  {-# INLINE convertor #-}

class KiloClass a where
  kiloFun :: (a -> a) -> a -> a

instance Num a => KiloClass (From a) where
  kiloFun f = (* 1000) . f
  {-# INLINE kiloFun #-}

instance Fractional a => KiloClass (Per (From a)) where
  kiloFun f = (/ 1000) . f
  {-# INLINE kiloFun #-}

instance Fractional a => KiloClass (To a) where
  kiloFun f = (/ 1000) . f
  {-# INLINE kiloFun #-}

instance Num a => KiloClass (Per (To a)) where
  kiloFun f = (* 1000) . f
  {-# INLINE kiloFun #-}

