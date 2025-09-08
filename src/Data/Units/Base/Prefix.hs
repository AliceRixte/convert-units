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
-- Provides a way to define prefixes for any system of units.
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
--
class (Fractional a, IsPrefix p) => PrefixFactor (p :: Prefix) a where
  -- | Prefix conversion factor from the prefixed unit to the corresponding
  -- standard unit
  --
  -- >>> prefixFactor @Kilo
  -- 1000.0
  --
  prefixFactor :: a

-- | Prefixes that can be shown as a string, or as a type error message.
class IsPrefix p => ShowPrefix (p :: Prefix) where
  {-# MINIMAL showPrefix |  showsPrefixPrec #-}

  -- | Allows to print units in conversion error messages
  --
  -- >>> type ShowPrefix Kilo = "k"
  --
  type ShowPrefixType p :: ErrorMessage

  -- | Convert a prefix to a readable string
  --
  -- @'showsPrefixPrec'@ should satisfy the law :
  --
  -- @showsPrefixPrec d x r ++ s  ==  showsPrec d x (r ++ s)@
  --
  showsPrefixPrec :: Int -> ShowS
  showsPrefixPrec _ = (showPrefix @p ++)

  -- | Convert a prefix to a string representing its type.
  --
  -- >>> showPrefix @Kilo
  -- "Kilo"
  --
  showPrefix :: String
  showPrefix = showsPrefix @p ""

  -- | Same as @'showsPrefixPrec'@ but for pretty printing.
  --
  -- @'prettysPrefixPrec'@ should satisfy the law :
  --
  -- @prettysPrefixPrec d x r ++ s  ==  prettysPrec d x (r ++ s)@
  --
  prettysPrefixPrec :: Int -> ShowS
  prettysPrefixPrec _ = (prettyPrefix @p ++)

  -- | Same as @'showPrefix'@ but for pretty printing
  --
  -- >>> prettyPrefix @Kilo
  -- "k"
  prettyPrefix :: String
  prettyPrefix = prettysPrefix @p ""

-- | Equivalent to 'showsPrefixPrec' with a precedence of 0.
showsPrefix :: forall p. ShowPrefix p => ShowS
showsPrefix = showsPrefixPrec @p 0

-- | Equivalent to 'prettysPrefixPrec' with a precedence of 0.
prettysPrefix :: forall p. ShowPrefix p => ShowS
prettysPrefix = prettysPrefixPrec @p 0


-- | A prefix that can represent any prefix.
--
-- This can be used with the `deriving via` mechanism to derive some of the
-- prefix instances.
--
newtype MetaPrefix (p :: Prefix) (u :: Unit) a = MetaPrefix (p u a)
  deriving Show via (MetaUnit (p u) a)

instance PrefixFactor p a => PrefixFactor (MetaPrefix p) a where
  prefixFactor = prefixFactor @p
  {-# INLINE prefixFactor #-}

instance
  (PrefixFactor p a, ConversionFactor u a, NormalizeUnit (p u) ~ NormalizeUnit u)
  => ConversionFactor (MetaPrefix p u) a where
  factor = prefixFactor @p * factor @u
  {-# INLINE factor #-}

instance
  (PrefixFactor p a, ConvertibleUnit u a, NormalizeUnit (p u) ~ NormalizeUnit u)
  => ConvertibleUnit (MetaPrefix p u) a where
  toBaseUnit (MetaPrefix a) = prefixToNormalUnit @p @u a
  {-# INLINE toBaseUnit #-}
  fromBaseUnit a = MetaPrefix $ prefixFromBaseUnit @p @u a
  {-# INLINE fromBaseUnit #-}

-- | Convert a prefixed unit to the corresponding standard unit.
--
prefixToNormalUnit :: forall (p :: Prefix) (u :: Unit) a.
  (PrefixFactor p a, ConvertibleUnit u a, NormalizeUnit (p u) ~ NormalizeUnit u)
  => p u a -> NormalizeUnit u a
prefixToNormalUnit u =
    toBaseUnit @u $ quantity @u (prefixFactor @p * unQuantity u)
{-# INLINE prefixToNormalUnit #-}

-- | Convert a standard unit to the corresponding prefixed unit.
--
prefixFromBaseUnit :: forall (p :: Prefix) (u :: Unit) a.
  (PrefixFactor p a, ConvertibleUnit u a
  , NormalizeUnit (p u) a ~ NormalizeUnit u a)
  => NormalizeUnit (p u) a -> p u a
prefixFromBaseUnit a = quantity  $ unQuantity (fromBaseUnit @u a) / prefixFactor @p
{-# INLINE prefixFromBaseUnit #-}


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

