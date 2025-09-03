{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Units.SI.Prefixes where


import Data.Units.Base


$(mkPrefix "Milli" "m" (1/1000))


-- newtype Milli (u :: Unit) a = Milli (u a)
--   deriving ( Eq, Ord, Num, Fractional, Floating, Real
--            , RealFrac, RealFloat, Functor)
--   deriving Show via MetaPrefix Milli u a
--   deriving ShowUnit via MetaPrefix Milli u

-- deriving via MetaPrefix Milli u instance IsUnit u => IsUnit (Milli u )

-- instance ShowPrefix Milli where
--   type ShowPrefixType Milli = Text "m"
--   showPrefix = "Milli"
--   prettyPrefix = "m"


-- instance Fractional a => PrefixFactor Milli a where
--   prefixFactorTo = 1000
--   {-# INLINE prefixFactorTo #-}

instance (To u a, Fractional a) => To (Milli u) a where
  to = prefixTo
  {-# INLINE to #-}

instance (From u a, Fractional a) => From (Milli u) a where
  from = prefixFrom
  {-# INLINE from #-}

-- instance ConvFactor u a => ConvFactor (Milli u) a where
--   factorFrom = factorFrom @(MetaPrefix Milli u)
--   {-# INLINE factorFrom #-}


--------------------------------------------------------------------------------

newtype Kilo (u :: Unit) a = Kilo (u a)
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via MetaPrefix Kilo u a
  deriving ShowUnit via MetaPrefix Kilo u

deriving via MetaPrefix Kilo u instance IsUnit u => IsUnit (Kilo u )

instance ShowPrefix Kilo where
  type ShowPrefixType Kilo = Text "k"
  prettyPrefix = "k"
  showPrefix = "Kilo"

instance Fractional a => PrefixFactor Kilo a where
  prefixFactorFrom = 1000
  {-# INLINE prefixFactorFrom #-}

instance (To u a, Fractional a) => To (Kilo u) a where
  to = prefixTo
  {-# INLINE to #-}

instance (From u a, Fractional a) => From (Kilo u) a where
  from = prefixFrom
  {-# INLINE from #-}

instance ConvFactor u a => ConvFactor (Kilo u) a where
  factorFrom = factorFrom @(MetaPrefix Kilo u)
  {-# INLINE factorFrom #-}


