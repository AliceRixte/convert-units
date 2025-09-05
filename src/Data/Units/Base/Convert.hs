{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.Base.Convert
-- Description :  Conversion between units
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- Conversion between units. Use @'fromTo'@ to convert between two units of the
-- same dimension.
--
-- = Implementing conversions for custom units
--
-- Depending on how the custom unit is converted to its standard unit, there are
-- three ways to implement its conversion summarized in the following table and
-- described with further details afterwards:
--
--  +--------------------------+------------------------+----------------------+
--  |                          | Which instances        | Note                 |
--  |                          | to declare             |                      |
--  +==========================+========================+======================+
--  | Conversion factor        | Only @'ConversionFactor'@    | @'from' == 'from''@, |
--  |                          | (@'From'@ and @'To'@   | @'to' == 'to''@,     |
--  |                          | have a default         | @'fromTo' == @       |
--  |                          | overlappable instance) | @'fromTo''@          |
--  +--------------------------+------------------------+----------------------+
--  | Affine conversion        | @'ConversionFactor'@,        | @'from' /= 'from''@, |
--  |                          | @'From'@ and @'To'@    | @'to' /= 'to''@,     |
--  |                          |                        | @'fromTo' /= @       |
--  |                          |                        | @'fromTo''@          |
--  +--------------------------+------------------------+----------------------+
--  | Non linear conversion    |                        | @'from''@, @'to''@   |
--  |                          | @'From'@ and @'To'@    | and @'fromTo''@      |
--  |                          |                        | cannot be used       |
--  +--------------------------+------------------------+----------------------+
--
--
-- === Multiplication by a conversion factor
--
-- For units that can be converted to and from their corresponding standard
-- units by multiplication of a converion factor, you only need to declare an
-- instance of @'ConversionFactor'@, like
--
-- @
-- instance Fractional a => ConversionFactor Hour a where f
--   actorFrom = 3600
-- @
--
-- You will get instances for @'From'@ and @'To'@ for free.
--
-- >>> toNormalUnit (Hour 1)
-- Second 3600.0
-- >>> toNormalUnit' (Hour 1)
-- Second 3600.0
--
-- === Affine conversion (with an offset)
--
-- Some units cannot be conversed by a simple multiplication. For instance, the
-- conversion between Celsius degrees and Kelvin degrees involves addition
-- @x °C = x + 273.15 K@.
--
-- However, when considered as /differences/ of temperatures, Celsius degrees
-- are converted to Kelvin degrees by a multiplication of @1@.
--
-- This can be expressed by the following instances:
--
-- @
-- instance Fractional a => ConversionFactor Celsius a where
--   factorFrom = 1
--
-- instance Fractional a => From Celsius a where
--   from (Celsius x) = Kelvin (x - 273.15)
--
-- instance Fractional a => To Celsius a where
--   to (Kelvin x) = Celsius (x + 273.15)
-- @
--
-- >>> from (Celsius 0)
-- Kelvin 273.15
-- >>> from' (Celsius 0)
-- Kelvin 0.0
--
-- === Other conversions
--
-- Any other conversion can be implemented, like for instance logarithmic units.
-- In this case, you should only give an instance for @'From'@ and @'To'@, and
-- no instance for @'ConversionFactor'@. See for instance linear picth
-- @'Data.Unit.NonStd.Frequency.Tet'@.
--
--------------------------------------------------------------------------------

module Data.Units.Base.Convert
  ( ConvertibleUnit (..)
  , FromTo
  , fromTo
  , from
  , to
  , ConversionFactor (..)
  , toNormalUnit'
  , fromNormalUnit'
  , FromTo'
  , fromTo'
  , from'
  , to'
  )
  where

import Data.Proxy

import Data.Type.Int

import Data.Units.Base.System



-- | A unit whose quantities are convertible from that unit to its corresponding
-- standard unit.
--
-- Instances must satisfy the following law :
--
-- * @'toNormalUnit' . 'fromNormalUnit' == 'id'@
--
class (IsUnit u, IsUnit (NormalizeUnit u)) => ConvertibleUnit u a where
  -- | Convert a quantity to its standard unit.
  --
  -- >>> import Data.Units.NonStd.Time
  -- >>> toNormalUnit @Hour 1
  -- Second 3600.0
  -- >>> toNormalUnit (Hour 1)
  -- Second 3600.0
  -- >>> toNormalUnit @(Kilo Meter ./. Hour) 36
  -- quantity @(Meter .*. Second .^- 1) 10.0
  -- >>> toNormalUnit (Celsius 0)
  -- Kelvin 273.15
  toNormalUnit :: u a -> NormalizeUnit u a
  default toNormalUnit :: ConversionFactor u a => u a -> NormalizeUnit u a
  toNormalUnit = toNormalUnit'
  {-# INLINE toNormalUnit #-}

  -- | Convert a quantity to its standard unit.
  --
  -- >>> fromNormalUnit @Hour 1800
  -- Hour 0.5
  -- >>> fromNormalUnit 1800 :: Hour Double
  -- Hour 0.5
  -- >>> fromNormalUnit @(Kilo Meter ./. Hour) 10
  -- quantity @(Kilo Meter .*. Hour .^- 1) 36.0
  -- >>> fromNormalUnit @Celsius 0
  -- Celsius (-273.15)
  --
  fromNormalUnit :: NormalizeUnit u a -> u a
  default fromNormalUnit :: ConversionFactor u a => NormalizeUnit u a -> u a
  fromNormalUnit = fromNormalUnit'
  {-# INLINE fromNormalUnit #-}


-- | A constraint that is satisfied when both units have the same dimension and
-- are such that @u@ can be converted to @v@.
--
type FromTo u v a = (DimEq u v, ConvertibleUnit u a, ConvertibleUnit v a)

-- | Conversion between two quantities with the same dimension.
--
--  >>> fromTo @Celsius @Kelvin 0
--  Kelvin 273.15
-- >>> fromTo @(Milli Second) @Hour 1
-- Hour 2.7777777777777776e-7
-- >>> fromTo (Milli (Second 1)) :: Hour Double
-- Hour 2.7777777777777776e-7
-- >>> fromTo @Turn @Degree (1/4) -- angle conversion
-- Degree 90.0
-- >>> fromTo @(Kilo Meter ./. Hour) @(Milli Meter ./. Milli Second) 36
-- quantity @(Milli Meter .*. Milli Second .^- 1) 10.0
--
fromTo :: FromTo u v a => u a -> v a
fromTo = fromNormalUnit . toNormalUnit
{-# INLINE fromTo #-}

-- | A mere synonym of @'fromTo'@ where it is more intuitive to use only one
-- type application.
--
-- >>> from @Celsius 0 :: Kelvin Double
-- Kelvin 273.15
--
from :: FromTo u v a => u a -> v a
from = fromTo
{-# INLINE from #-}

-- | Same as @'fromTo'@ but the type applications are reversed
--
-- >>> to @Kelvin (Celsius 0)
-- Kelvin 273.15
--
to :: forall v u a. FromTo u v a => u a -> v a
to = fromTo
{-# INLINE to #-}


--------------------------------------------------------------------------------


-- | Unit that can be converted to their corresponding standard unit by
-- multiplication with a conversion factor.
--
-- Instances must satisfy the following laws:
--
-- * @'factorFrom' == 1 / 'factorTo'@
-- * @'toNormalUnit' == (* 'factorFrom')@
-- * @'fromNormalUnit' == (* 'factorTo')@
--
class (ConvertibleUnit u a, Fractional a) => ConversionFactor u a where
  {-# MINIMAL factorFrom | factorTo #-}
  -- | Multiplying a quantity of type @u a@ with @'factorFrom'@ will convert it
  -- to its corresponding standard unit @NormalizeUnit u a@
  --
  -- >>> factorFrom @Hour :: Double
  -- 3600.0
  -- >>> factorFrom @Celsius :: Double
  -- 1.0
  -- >>> factorFrom @(Kilo Meter ./. Hour) :: Double
  -- 0.2777777777777778
  factorFrom :: a
  factorFrom = 1 / factorTo @u
  {-# INLINE factorFrom #-}

  -- | Multiplying a quantity of type @NormalizeUnit u a@ with @'factorTo'@
  -- will convert it to the unit @u a@
  --
  -- >>> factorTo @Hour :: Double
  -- 2.777777777777778e-4
  -- >>> factorTo @Celsius :: Double
  -- 1.0
  -- >>> factorTo @(Kilo Meter ./. Hour) :: Double
  -- 3.5999999999999996
  --
  -- Notice the small error due to the fact that the factor computed is @1 /
  -- (1000 / 3600)@. This does not mean however that the conversion will be less
  -- efficient, because this is likely to be done at compile time by GHC and
  -- inlined.
  --
  factorTo :: a
  factorTo = 1 / factorFrom @u
  {-# INLINE factorTo #-}

instance (IsUnit u, IsUnit (DimToUnit (DimOf u)), Fractional a)
  => ConvertibleUnit (MetaUnit u) a where

instance (IsUnit u, IsUnit (DimToUnit (DimOf u)), Fractional a)
  => ConversionFactor (MetaUnit u) a where
  factorFrom = 1
  {-# INLINE factorFrom #-}


instance Fractional a => ConvertibleUnit NoUnit a

instance Fractional a => ConversionFactor NoUnit a where
  factorFrom = 1
  {-# INLINE factorFrom #-}

instance (Num a, ConversionFactor u a, ConversionFactor v a, IsUnit (NormalizeUnit (u .*. v)))
  => ConvertibleUnit (u .*. v) a

instance (Num a, ConversionFactor u a, ConversionFactor v a, IsUnit (NormalizeUnit (u .*. v)))
  =>  ConversionFactor (u .*. v) a where
  factorFrom = factorFrom @u * factorFrom @v
  {-# INLINE factorFrom #-}

instance (ConversionFactor u a, IsUnit (NormalizeUnit (u .^. n)),  KnownInt n)
  => ConvertibleUnit (u .^. n) a

instance (ConversionFactor u a, IsUnit (NormalizeUnit (u .^. n)),  KnownInt n)
  =>  ConversionFactor (u .^. n) a where
  factorFrom = factorFrom @u ^^ intVal (Proxy :: Proxy n)
  {-# INLINE factorFrom #-}

-- | Convert a quantity to its corresponding standard unit by multiplying it
-- by  @'fromFactor'@.
--
-- >>> toNormalUnit' @Hour 1
-- Second 3600.0
-- >>> toNormalUnit' (Hour 1)
-- Second 3600.0
-- >>> toNormalUnit' @(Kilo Meter ./. Hour) 36
-- quantity @(Meter .*. Second .^- 1) 10.0
-- >>> toNormalUnit' (Celsius 0)
-- Kelvin 0.0
--
toNormalUnit' :: forall u a. ConversionFactor u a
  => u a -> NormalizeUnit u a
toNormalUnit' q = quantity (unQuantity q * factorFrom @u)
{-# INLINE toNormalUnit' #-}

-- | Convert a standard quantity to a unit @u@ by multiplying it by
-- by  @'toFactor'@.
--
-- >>> fromNormalUnit' @Hour 1800
-- Hour 0.5
-- >>> fromNormalUnit' 1800 :: Hour Double
-- Hour 0.5
-- >>> fromNormalUnit' @(Kilo Meter ./. Hour) 10
-- quantity @(Kilo Meter .*. Hour .^- 1) 36.0
-- >>> fromNormalUnit' @Celsius 0
-- Celsius 0.0
--
fromNormalUnit' :: forall u a. ConversionFactor u a
  => NormalizeUnit u a -> u a
fromNormalUnit' q = quantity (unQuantity q * factorTo @u)
{-# INLINE fromNormalUnit' #-}

-- | A constraint that is satisfied when both units have the same dimension and
-- are such that @u@ can be converted to @v@ by using a conversion factor.
--
type FromTo' u v a = (DimEq u v, ConversionFactor u a, ConversionFactor v a)

-- | Conversion, using conversion factors, between two quantities with the same
-- dimension
--
-- >>> fromTo' @Celsius @Kelvin 0
-- Kelvin 0.0
-- >>> fromTo' @(Milli Second) @Hour 1
-- Hour 2.7777777777777776e-7
-- >>> fromTo' (Milli (Second 1)) :: Hour Double
-- Hour 2.7777777777777776e-7
-- >>> fromTo' @Turn @Degree (1/4) -- angle conversion
-- Degree 90.0
-- >>> fromTo' @(Kilo Meter ./. Hour) @(Milli Meter ./. Milli Second) 36
-- quantity @(Milli Meter .*. Milli Second .^- 1) 10.0
--
fromTo' :: forall u v a.
  FromTo' u v a
  => u a -> v a
fromTo' q = quantity (unQuantity q * (factorFrom @u * factorTo @v))
{-# INLINE fromTo' #-}



-- | A mere synonym of @'fromTo''@ where it is more intuitive to use only one
-- type application.
--
-- >>> from' @Celsius 0 :: Kelvin Double
-- Kelvin 0.0
--
from' :: FromTo' u v a => u a -> v a
from' = fromTo'
{-# INLINE from' #-}

-- | Same as @'fromTo''@ but the type applications are reversed
--
-- >>> to' @Kelvin (Celsius 0)
-- Kelvin 0.0
--
to' :: forall v u a. FromTo' u v a => u a -> v a
to' = fromTo'
{-# INLINE to' #-}


