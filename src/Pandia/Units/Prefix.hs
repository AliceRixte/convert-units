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

import Pandia.Units.Convert

import Data.Proxy
import Data.Kind

newtype Milli (f :: Type -> Type) a = Milli (f a)
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat, Functor)

newtype Kilo (f :: Type -> Type) a = Kilo (f a)
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat, Functor)




-- newtype Meter a = Meter a
--   deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat, Functor)


-- class ConvertPrefixType f a | f -> a where
--   convertPrefix :: f -> a -> a

-- class ConvertUnitType f a | f -> a where
--   convertUnit :: f -> a -> a

-- instance ConvertUnitType a => ConvertPrefixType Milli a  where
--   convertPrefix (Milli f) = milli (convertUnit f)

-- instance ConvertUnitType Meter a where
--   convertUnit (Meter _) = id




-- instance (PrefixConv a, ConvertType f a)
--   => ConvertType (Milli f) a where
--   convertType _ = milli (convertType (Proxy :: Proxy f))

-- instance (KiloConv a, ConvertType f a)
--   => ConvertType (Kilo f) a where
--   convertType _ = kilo (convertType (Proxy :: Proxy f))

-- instance ConvertType Meter a where
--   convertType _ = id





class PrefixConv a where
  milli ::  (a -> a) -> a -> a

instance Fractional a => PrefixConv (From a) where
  milli f = (/ 1000) . f
  {-# INLINE milli #-}

instance Num a => PrefixConv (Per (From a)) where
  milli f = (* 1000) . f
  {-# INLINE milli #-}

instance Num a => PrefixConv (To a) where
  milli f =  (* 1000) . f
  {-# INLINE milli #-}

instance Fractional a => PrefixConv (Per (To a)) where
  milli f = (/ 1000) . f
  {-# INLINE milli #-}


kilo :: forall f a. KiloConv a => Convertor f a -> Convertor (Kilo f) a
kilo f _ = kiloFast (f (Proxy :: Proxy f) :: a -> a)

instance (Num a, ConvertType f (From a)) => ConvertType (Kilo f) (From a) where
  convertor _ = kiloFast (convertor (Proxy :: Proxy f))
  {-# INLINE convertor #-}


class KiloConv a where
  kiloFast :: (a -> a) -> a -> a

instance Num a => KiloConv (From a) where
  kiloFast f = (* 1000) . f
  {-# INLINE kiloFast #-}

instance Fractional a => KiloConv (Per (From a)) where
  kiloFast f = (/ 1000) . f
  {-# INLINE kiloFast #-}

instance Fractional a => KiloConv (To a) where
  kiloFast f = (/ 1000) . f
  {-# INLINE kiloFast #-}

instance Num a => KiloConv (Per (To a)) where
  kiloFast f = (* 1000) . f
  {-# INLINE kiloFast #-}

