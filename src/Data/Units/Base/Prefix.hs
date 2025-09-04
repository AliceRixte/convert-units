{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}

--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.Base.Prefix
-- Description :  Unit prefix for a system of units
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- Provides way to define prefixes for any system of units.
--
--------------------------------------------------------------------------------


module Data.Units.Base.Prefix where

import GHC.TypeError

import Data.Units.Base.System
import Data.Units.Base.Convert

-- | A unit prefix, like Kilo, Milli, etc.
type Prefix = Unit -> Unit

-- | The application of a prefix to a unit must always be a unit.
class (forall (u :: Unit). IsUnit u => IsUnit (p u))
  => IsPrefix (p :: Prefix)

instance (forall (u :: Unit). IsUnit u => IsUnit (p u))
  => IsPrefix (p :: Prefix)

-- | A prefix that has a conversion factor.
class (Fractional a, IsPrefix p) => PrefixFactor (p :: Prefix) a where
  {-# MINIMAL prefixFactorFrom | prefixFactorTo #-}
  -- | Prefix conversion factor from the prefixed unit to the corresponding
  -- standard unit
  --
  -- >>> prefixFactorFrom @Kilo
  -- 1000.0
  --
  prefixFactorFrom :: a
  prefixFactorFrom = 1 / prefixFactorTo @p
  {-# INLINE prefixFactorFrom #-}

  -- | Prefix conversion factor fromthe standard unit to the prefixed unit
  --
  -- >>> prefixFactorTo @Kilo
  -- 1000.0
  --
  prefixFactorTo :: a
  prefixFactorTo = 1 / prefixFactorFrom @p
  {-# INLINE prefixFactorTo #-}

-- | Prefixes that can be shown as a string, or as a type error message.
class IsPrefix p => ShowPrefix (p :: Prefix) where
  {-# MINIMAL showPrefix |  showsPrefixPrec #-}

  -- | Allows to print units in conversion error messages
  --
  -- >>> type ShowPrefix Kilo = "k"
  --
  type ShowPrefixType p :: ErrorMessage

  -- | TODO
  showsPrefixPrec :: Int -> ShowS
  showsPrefixPrec _ = (showPrefix @p ++)

  -- | A string representing a prefix.
  showPrefix :: String
  showPrefix = showsPrefix @p ""

  prettysPrefixPrec :: Int -> ShowS
  prettysPrefixPrec _ = (prettyPrefix @p ++)

  -- | A string representing a prefix.
  prettyPrefix :: String
  prettyPrefix = prettysPrefix @p ""

-- | TODO
showsPrefix :: forall p. ShowPrefix p => ShowS
showsPrefix = showsPrefixPrec @p 0

prettysPrefix :: forall p. ShowPrefix p => ShowS
prettysPrefix = prettysPrefixPrec @p 0

-- showsPrefixCons :: forall p. ShowPrefix p => ShowS
-- showsPrefixCons = showsPrecPrefix @p 0


-- | A prefix that can represent any prefix.
--
-- This can be used with the `deriving via` mechanism to derive some of the
-- prefix instances.
newtype MetaPrefix (p :: Prefix) (u :: Unit) a = MetaPrefix (p u a)
  deriving Show via (MetaUnit (p u) a)

instance PrefixFactor p a => PrefixFactor (MetaPrefix p) a where
  prefixFactorFrom = prefixFactorFrom @p
  {-# INLINE prefixFactorFrom #-}
  prefixFactorTo = prefixFactorTo @p
  {-# INLINE prefixFactorTo #-}

instance (PrefixFactor p a, ConvFactor u a, BaseUnitOf (p u) ~ BaseUnitOf u)
  => ConvFactor (MetaPrefix p u) a where
  factorFrom = prefixFactorFrom @p * factorFrom @u
  {-# INLINE factorFrom #-}
  factorTo= prefixFactorTo @p * factorTo @u
  {-# INLINE factorTo #-}

instance
  (PrefixFactor p a, ConvertibleUnit u a, BaseUnitOf (p u) ~ BaseUnitOf u)
  => ConvertibleUnit (MetaPrefix p u) a where
  toBaseUnit (MetaPrefix a) = prefixFrom @p @u a
  {-# INLINE toBaseUnit #-}
  fromBaseUnit a = MetaPrefix $ prefixTo @p @u a
  {-# INLINE fromBaseUnit #-}

prefixFrom :: forall (p :: Prefix) (u :: Unit) a.
  (PrefixFactor p a, ConvertibleUnit u a, BaseUnitOf (p u) ~ BaseUnitOf u)
  => p u a -> BaseUnitOf u a
prefixFrom u = toBaseUnit @u $ quantity @u (prefixFactorFrom @p * unQuantity u)
{-# INLINE prefixFrom #-}

prefixTo :: forall (p :: Prefix) (u :: Unit) a.
  (PrefixFactor p a, ConvertibleUnit u a, BaseUnitOf (p u) a ~ BaseUnitOf u a)
  => BaseUnitOf (p u) a -> p u a
prefixTo a = quantity  $ unQuantity (fromBaseUnit @u a) * prefixFactorTo @p
{-# INLINE prefixTo #-}


instance ShowPrefix p => ShowPrefix (MetaPrefix p) where
  type ShowPrefixType (MetaPrefix p)  = ShowPrefixType p
  showsPrefixPrec = showsPrefixPrec @p
  showPrefix = showPrefix @p
  prettysPrefixPrec = prettysPrefixPrec @p
  prettyPrefix = prettyPrefix @p

instance (IsPrefix p, IsUnit u)
  => IsUnit (MetaPrefix p u) where
  type DimOf (MetaPrefix p u) = DimOf u

instance (ShowPrefix p, ShowUnit u)
  => ShowUnit (MetaPrefix p u) where
  type ShowUnitType (MetaPrefix p u) = ShowPrefixType p :<>: ShowUnitType u
  showsUnitPrec d = showParen (d > 10) $
    showsPrefix @p . showString " " .  showsUnitPrec @u 11
  prettysUnitPrec d = showParen (d > 10) $
    prettysPrefix @p  . prettysUnit @u

