{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Units.Base.Prefix where

import Data.Coerce

import Data.Units.Base.Unit
import Data.Units.Base.Convert

-- | A unit prefix, like Kilo, Milli, etc.
type Prefix = Unit -> Unit

class (forall (u :: Unit). IsUnit u => IsUnit (p u))
  => IsPrefix (p :: Prefix)

instance (forall (u :: Unit). IsUnit u => IsUnit (p u))
  => IsPrefix (p :: Prefix)

class (Fractional a, IsPrefix p) => PrefixFactor (p :: Prefix) a where
  {-# MINIMAL prefixFactorFrom | prefixFactorTo #-}
  prefixFactorFrom :: a
  prefixFactorFrom = 1 / prefixFactorTo @p
  {-# INLINE prefixFactorFrom #-}

  prefixFactorTo :: a
  prefixFactorTo = 1 / prefixFactorFrom @p
  {-# INLINE prefixFactorTo #-}

class IsPrefix p => ShowPrefix (p :: Prefix) where
  {-# MINIMAL showPrefix |  showsPrecPrefix #-}
  type ShowPrefixType p :: ErrorMessage
  showsPrecPrefix :: Int -> ShowS
  showsPrecPrefix _ = (showPrefix @p ++)

  showPrefix :: String
  showPrefix = showsPrefix @p ""

showsPrefix :: forall p. ShowPrefix p => ShowS
showsPrefix = showsPrecPrefix @p 0



newtype MetaPrefix (p :: Prefix) (u :: Unit) a = MetaPrefix (p u a)
  deriving Show via (MetaUnit (p u) a)

instance PrefixFactor p a => PrefixFactor (MetaPrefix p) a where
  prefixFactorFrom = prefixFactorFrom @p
  {-# INLINE prefixFactorFrom #-}
  prefixFactorTo = prefixFactorTo @p
  {-# INLINE prefixFactorTo #-}

instance (PrefixFactor p a, ConvFactor u a, StdUnitOf (p u) ~ StdUnitOf u)
  => ConvFactor (MetaPrefix p u) a where
  factorFrom = prefixFactorFrom @p * factorFrom @u
  {-# INLINE factorFrom #-}
  factorTo= prefixFactorTo @p * factorTo @u
  {-# INLINE factorTo #-}

instance (PrefixFactor p a, From u a, StdUnitOf (p u) ~ StdUnitOf u)
  => From (MetaPrefix p u) a where

  from (MetaPrefix a) = prefixFrom @p @u a
  {-# INLINE from #-}

prefixFrom :: forall (p :: Prefix) (u :: Unit) a.
  (PrefixFactor p a, From u a, StdUnitOf (p u) ~ StdUnitOf u)
  => p u a -> StdUnitOf u a
prefixFrom a = from @u (coerce (prefixFactorFrom @p * coerce a :: a) :: u a)
{-# INLINE prefixFrom #-}

prefixTo :: forall (p :: Prefix) (u :: Unit) a.
  (PrefixFactor p a, To u a, StdUnitOf (p u) a ~ StdUnitOf u a)
  => StdUnitOf (p u) a -> p u a
prefixTo a = coerce (coerce (to a :: u a) * prefixFactorTo @p :: a)
{-# INLINE prefixTo #-}


instance ShowPrefix p => ShowPrefix (MetaPrefix p) where
  type ShowPrefixType (MetaPrefix p)  = ShowPrefixType p
  showsPrecPrefix = showsPrecPrefix @p
  showPrefix = showPrefix @p

instance (IsPrefix p, IsUnit u)
  => IsUnit (MetaPrefix p u) where
  type StdUnitOf (MetaPrefix p u) = StdUnitOf u

instance (ShowPrefix p, ShowUnit u)
  => ShowUnit (MetaPrefix p u) where
  type ShowUnitType (MetaPrefix p u) = ShowPrefixType p :<>: ShowUnitType u
  showsPrecUnit d = showParen (d > 10) $
    showsPrefix @p . showsUnit @u

