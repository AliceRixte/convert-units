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
--  | Conversion factor        | Only @'ConvFactor'@    | @'from' == 'from''@, |
--  |                          | (@'From'@ and @'To'@   | @'to' == 'to''@,     |
--  |                          | have a default         | @'fromTo' == @       |
--  |                          | overlappable instance) | @'fromTo''@          |
--  +--------------------------+------------------------+----------------------+
--  | Affine conversion        | @'ConvFactor'@,        | @'from' /= 'from''@, |
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
-- instance of @'ConvFactor'@, like
--
-- @
-- instance Fractional a => ConvFactor Hour a where f
--   actorFrom = 3600
-- @
--
-- You will get instances for @'From'@ and @'To'@ for free.
--
-- >>> from (Hour 1)
-- ofUnit 3600.0 "s"
-- >>> from' (Hour 1)
-- ofUnit 3600.0 "s"
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
-- instance Fractional a => ConvFactor Celsius a where
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
-- ofUnit -273.15 "K"
-- >>> from' (Celsius 0)
-- ofUnit 0.0 "K"
--
-- === Other conversions
--
-- Any other conversion can be implemented, like for instance logarithmic units.
-- In this case, you should only give an instance for @'From'@ and @'To'@, and
-- no instance for @'ConvFactor'@. See for instance linear picth
-- @'Data.Unit.NonStd.Frequency.Tet'@.
--
--------------------------------------------------------------------------------

module Data.Units.Base.Convert
  ( ConvertibleUnit (..)
  , FromTo
  , fromTo
  , ConvFactor(..)
  , from'
  , to'
  , FromTo'
  , fromTo'

   -- Remove
  , fromCoerce
  , toCoerce
  , fromToCoerce
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
-- * @'from' . 'to' == 'id'@
--
class (IsUnit u, IsUnit (BaseUnitOf u)) => ConvertibleUnit u a where
  -- | Convert a quantity to its standard unit.
  --
  -- >>> import Data.Units.NonStd.Time
  -- >>> from @Hour 1
  -- ofUnit 3600.0 "s"
  -- >>> from (Hour 1)
  -- ofUnit 3600.0 "s"
  -- >>> from @(Kilo Meter -/- Hour) 36
  -- ofUnit 10.0 "m.s⁻¹"
  -- >>> from (Celsius 0)
  -- ofUnit 273.15 "K"
  from :: u a -> BaseUnitOf u a
  default from :: ConvFactor u a => u a -> BaseUnitOf u a
  from = from'
  {-# INLINE from #-}

  -- | Convert a quantity to its standard unit.
  --
  -- >>> to @Hour 1800
  -- ofUnit 0.5 "hr"
  -- >>> to 1800 :: Hour Double
  -- ofUnit 0.5 "hr"
  -- >>> to @(Kilo Meter -/- Hour) 10
  -- ofUnit 36.0 "km.hr⁻¹"
  -- >>> to @Celsius 0
  -- ofUnit (-273.15) "°C"
  --
  to :: BaseUnitOf u a -> u a
  default to :: ConvFactor u a => BaseUnitOf u a -> u a
  to = to'
  {-# INLINE to #-}

fromCoerce :: forall u a. ConvertibleUnit u a => a -> a
fromCoerce = unQuantity @(BaseUnitOf u) . from . quantity @u
{-# INLINE fromCoerce #-}

-- instance {-# OVERLAPPABLE #-}
--   (ConvFactor u a, IsUnit (BaseUnitOf u), IsUnit u)
--     => ConvertibleUnit u a where
--   from = from'
--   {-# INLINE from #-}

--   to = to'
--   {-# INLINE to #-}


toCoerce :: forall u a. ConvertibleUnit u a => a -> a
toCoerce = unQuantity @u . to . quantity @(BaseUnitOf u)
{-# INLINE toCoerce #-}

-- | A constraint that is satisfied when both units have the same dimension and
-- are such that @u@ can be converted to @v@.
--
type FromTo u v a = (DimEq u v, ConvertibleUnit u a, ConvertibleUnit v a)

-- | Conversion between two quantities with the same dimension.
--
--  >>> fromTo @Celsius @Kelvin 0
--  ofUnit 273.15 "K"
-- >>> fromTo @(Milli Second) @Hour 1
-- ofUnit 2.7777777777777776e-7 "hr"
-- >>> fromTo (Milli (Second 1)) :: Hour Double
-- ofUnit 2.7777777777777776e-7 "hr"
-- >>> fromTo @Turn @Degree (1/4) -- angle conversion
-- ofUnit 90.0 "°"
-- >>> fromTo @(Kilo Meter -/- Hour) @(Milli Meter -/- Milli Second) 36
-- ofUnit 10.0 "mm.ms⁻¹"
--
fromTo :: FromTo u v a => u a -> v a
fromTo = to . from
{-# INLINE fromTo #-}

fromToCoerce :: forall u v a. FromTo u v a => a -> a
fromToCoerce = unQuantity @v . fromTo . quantity @u
{-# INLINE fromToCoerce #-}

--------------------------------------------------------------------------------


-- | Unit that can be converted to their corresponding standard unit by
-- multiplication with a conversion factor.
--
-- Instances must satisfy the following laws:
--
-- * @'factorFrom' == 1 / 'factorTo'@
-- * @'from' == (* 'factorFrom')@
-- * @'to' == (* 'factorTo')@
--
class (ConvertibleUnit u a, Fractional a) => ConvFactor u a where
  {-# MINIMAL factorFrom | factorTo #-}
  -- | Multiplying a quantity of type @u a@ with @'factorFrom'@ will convert it
  -- to its corresponding standard unit @BaseUnitOf u a@
  --
  -- >>> factorFrom @Hour :: Double
  -- 3600.0
  -- >>> factorFrom @Celsius :: Double
  -- 1.0
  -- >>> factorFrom @(Kilo Meter -/- Hour) :: Double
  -- 0.2777777777777778
  factorFrom :: a
  factorFrom = 1 / factorTo @u
  {-# INLINE factorFrom #-}

  -- | Multiplying a quantity of type @BaseUnitOf u a@ with @'factorTo'@
  -- will convert it to the unit @u a@
  --
  -- >>> factorTo @Hour :: Double
  -- 2.777777777777778e-4
  -- >>> factorTo @Celsius :: Double
  -- 1.0
  -- >>> factorTo @(Kilo Meter -/- Hour) :: Double
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
  => ConvFactor (MetaUnit u) a where
  factorFrom = 1
  {-# INLINE factorFrom #-}


instance Fractional a => ConvertibleUnit NoUnit a

instance Fractional a => ConvFactor NoUnit a where
  factorFrom = 1
  {-# INLINE factorFrom #-}

instance (Num a, ConvFactor u a, ConvFactor v a, IsUnit (BaseUnitOf (u .*. v)))
  => ConvertibleUnit (u .*. v) a

instance (Num a, ConvFactor u a, ConvFactor v a, IsUnit (BaseUnitOf (u .*. v)))
  =>  ConvFactor (u .*. v) a where
  factorFrom = factorFrom @u * factorFrom @v
  {-# INLINE factorFrom #-}

instance (ConvFactor u a, IsUnit (BaseUnitOf (u .^. n)),  KnownInt n)
  => ConvertibleUnit (u .^. n) a

instance (ConvFactor u a, IsUnit (BaseUnitOf (u .^. n)),  KnownInt n)
  =>  ConvFactor (u .^. n) a where
  factorFrom = factorFrom @u ^^ intVal (Proxy :: Proxy n)
  {-# INLINE factorFrom #-}

-- | Convert a quantity to its corresponding standard unit by multiplying it
-- by  @'fromFactor'@.
--
-- >>> from' @Hour 1
-- ofUnit 3600.0 "s"
-- >>> from' (Hour 1)
-- ofUnit 3600.0 "s"
-- >>> from' @(Kilo Meter -/- Hour) 36
-- ofUnit 10.0 "m.s⁻¹"
-- >>> from' (Celsius 0)
-- ofUnit 0.0 "K"
--
from' :: forall u a. ConvFactor u a
  => u a -> BaseUnitOf u a
from' q = quantity (unQuantity q * factorFrom @u)
{-# INLINE from' #-}

-- | Convert a standard quantity to a unit @u@ by multiplying it by
-- by  @'toFactor'@.
--
-- >>> to' @Hour 1800
-- ofUnit 0.5 "hr"
-- >>> to' 1800 :: Hour Double
-- ofUnit 0.5 "hr"
-- >>> to' @(Kilo Meter -/- Hour) 10
-- ofUnit 36.0 "km.hr⁻¹"
-- >>> to' @Celsius 0
-- ofUnit (-273.15) "°C"
--
to' :: forall u a. ConvFactor u a
  => BaseUnitOf u a -> u a
to' q = quantity (unQuantity q * factorTo @u)
{-# INLINE to' #-}

-- | A constraint that is satisfied when both units have the same dimension and
-- are such that @u@ can be converted to @v@ by using a conversion factor.
--
type FromTo' u v a = (DimEq u v, ConvFactor u a, ConvFactor v a)

-- | Conversion, using conversion factors, between two quantities with the same dimension
--
-- >>> fromTo' @Celsius @Kelvin 0
--  ofUnit 0.0 "K"
-- >>> fromTo' @(Milli Second) @Hour 1
-- ofUnit 2.7777777777777776e-7 "hr"
-- >>> fromTo' (Milli (Second 1)) :: Hour Double
-- ofUnit 2.7777777777777776e-7 "hr"
-- >>> fromTo' @Turn @Degree (1/4) -- angle conversion
-- ofUnit 90.0 "°"
-- >>> fromTo' @(Kilo Meter -/- Hour) @(Milli Meter -/- Milli Second) 36
-- ofUnit 10.0 "mm.ms⁻¹"
--
fromTo' :: forall u v a.
  FromTo' u v a
  => u a -> v a
fromTo' q = quantity (unQuantity q * (factorFrom @u * factorTo @v))
{-# INLINE fromTo' #-}
