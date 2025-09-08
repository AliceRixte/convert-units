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
-- Conversion between units. Use @'from'@, @'to'@, or  @'fromTo'@ to convert
-- between two units of the same dimension.
--
-- = Implementing conversions for custom units
--
-- Depending on how the custom unit is converted to its standard unit, there are
-- three ways to implement its conversion summarized in the following table and
-- described with further details afterwards:
--
--  +-----------------------+---------------------------+----------------------+
--  |                       | Which instances           | Note                 |
--  |                       | to declare                |                      |
--  +=======================+===========================+======================+
--  | Conversion factor     | @'ConversionFactor'@ and  |                      |
--  |                       | @'ConvertibleUnit'@ using |                      |
--  |                       | the default               | @'fromTo' == @       |
--  |                       | implementations           | @'fromTo''@          |
--  |                       | for 'fromBaseUnit' and  |                      |
--  |                       | 'toBaseUnit'            |                      |
--  +-----------------------+---------------------------+----------------------+
--  | Affine conversion     | @'ConversionFactor'@ and  |                      |
--  |                       | @'ConvertibleUnit'@       |                      |
--  |                       |                           | @'fromTo' /= @       |
--  |                       |                           | @'fromTo''@          |
--  +-----------------------+---------------------------+----------------------+
--  | Non linear conversion |                           | @'from''@, @'to''@   |
--  |                       | @'ConvertibleUnit'@       | and @'fromTo''@      |
--  |                       |                           | cannot be used       |
--  +-----------------------+---------------------------+----------------------+
--
--
-- === Multiplication by a conversion factor
--
-- For units that can be converted to and from their corresponding standard
-- units by multiplication of a converion factor, you only need to declare an
-- instance of @'ConversionFactor'@, like
--
-- @
-- instance Fractional a => ConversionFactor Hour a where
--   factor = 3600
--
-- instance Fractional a => ConvertibleUnit Hour a
--    -- uses default implementations for 'fromBaseUnit' and 'toBaseUnit'
-- @
--
-- >>> fromTo @Hour @Second 1
-- Second 3600.0
-- >>> fromTo' @Hour @Second 1
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
-- instance Num a => ConversionFactor Celsius a where
--   factor = 1
--
-- instance Fractional a => ConvertibleUnit Celsius a where
--   toBaseUnit (Celsius x) = Kelvin (x - 273.15)
--   fromBaseUnit (Kelvin x) = Celsius (x + 273.15)
-- @
--
-- >>> fromTo @Celsius @Kelvin 0
-- Kelvin 273.15
-- >>> fromTo' @Celsius @Kelvin 0
-- Kelvin 0.0
--
-- === Other conversions
--
-- Any other conversion can be implemented, like for instance logarithmic units.
-- In this case, you should only give an instance for @'ConvertibleUnit'@, and
-- no instance for @'ConversionFactor'@. See for instance linear picth
-- @'Data.Unit.NonStd.Frequency.Tet'@.
--
--------------------------------------------------------------------------------

module Data.Units.Base.Convert
  ( DimEq
  -- * Generic conversion between units
  , ConvertibleUnit (..)
  , FromTo
  , fromTo
  , from
  , to
  , ($~)
  , (~&)
  -- * Conversion using conversion factors
  , ConversionFactor (..)
  , toBaseUnit'
  , fromBaseUnit'
  , FromTo'
  , fromTo'
  , from'
  , to'
  )
  where

import Data.Proxy
import Data.Kind
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeError

import Data.Type.Int

import Data.Units.Base.System

-- | A constraint to test whether two units have
type family DimEq (u :: Unit) (v :: Unit) :: Constraint where
  DimEq u v = DimEqStd u v (DimOf u) (DimOf v)

type family DimEqStd (u :: Unit) (v :: Unit) (du :: Dim) (dv :: Dim)
  :: Constraint where
  DimEqStd u v du dv =
    ( IsUnit u
    , IsUnit v
    , du ~ dv
    , If (du == dv) (() :: Constraint)
      (TypeError (
            Text "Cannot convert unit ‘"
            :<>: ShowUnitType u
            :<>: Text "’ to unit ‘"
            :<>: ShowUnitType v
            :<>: Text "’ because their dimensions do not match."
            :$$: Text "Dimension of ‘"
            :<>: ShowUnitType u
            :<>: Text "’ is: "
            :<>: ShowDimType du
            :$$: Text "Dimension of ‘"
            :<>: ShowUnitType v
            :<>: Text "’ is: "
            :<>: ShowDimType dv
    )))



-- | A unit whose quantities are convertible from that unit to its corresponding
-- standard unit.
--
-- Instances must satisfy the following law :
--
-- * @'toBaseUnit' . 'fromBaseUnit' == 'id'@
--
class (IsUnit u, IsUnit (NormalizeUnit u)) => ConvertibleUnit u a where
  -- | Convert a quantity to its standard unit.
  --
  -- >>> import Data.Units.NonStd.Time
  -- >>> toBaseUnit @Hour 1
  -- Second 3600.0
  -- >>> toBaseUnit (Hour 1)
  -- Second 3600.0
  -- >>> toBaseUnit @(Kilo Meter ./. Hour) 36
  -- quantity @(Meter .*. Second .^- 1) 10.0
  -- >>> toBaseUnit (Celsius 0)
  -- Kelvin 273.15
  toBaseUnit :: u a -> NormalizeUnit u a
  default toBaseUnit :: ConversionFactor u a => u a -> NormalizeUnit u a
  toBaseUnit = toBaseUnit'
  {-# INLINE toBaseUnit #-}

  -- | Convert a quantity to its standard unit.
  --
  -- >>> fromBaseUnit @Hour 1800
  -- Hour 0.5
  -- >>> fromBaseUnit 1800 :: Hour Double
  -- Hour 0.5
  -- >>> fromBaseUnit @(Kilo Meter ./. Hour) 10
  -- quantity @(Kilo Meter .*. Hour .^- 1) 36.0
  -- >>> fromBaseUnit @Celsius 0
  -- Celsius (-273.15)
  --
  fromBaseUnit :: NormalizeUnit u a -> u a
  default fromBaseUnit :: ConversionFactor u a => NormalizeUnit u a -> u a
  fromBaseUnit = fromBaseUnit'
  {-# INLINE fromBaseUnit #-}



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
fromTo = fromBaseUnit . toBaseUnit
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

-- | A convenient operator for converting a unit before feeding it to a
-- function.
--
-- >>> import Linear
-- >>> rotation (Radian th) = V2 (V2 (cos th) (- sin th)) (V2 (sin th)  (cos th))
-- >>> rotation $~ Degree 90
-- V2 (V2 6.123031769111886e-17 (-1.0)) (V2 1.0 6.123031769111886e-17)
--
($~) :: FromTo u v a => (v a -> b) -> u a -> b
f $~ x = f (fromTo x)
{-# INLINE ($~) #-}

infixr 0 $~

-- | Same as @'($~)'@ but with arguments flipped.
--
(~&) :: FromTo u v a => u a -> (v a -> b) -> b
(~&) = flip ($~)
{-# INLINE (~&) #-}

infixl 1 ~&

--------------------------------------------------------------------------------

-- | Unit that can be converted to their corresponding standard unit by
-- multiplication with a conversion factor.
--
-- Instances must satisfy the following laws:
--
-- * @'factor' == 1 / 'factorTo'@
-- * @'toBaseUnit' == (* 'factor')@
-- * @'fromBaseUnit' == (* 'factorTo')@
--
class (ConvertibleUnit u a, Fractional a) => ConversionFactor u a where
  {-# MINIMAL factor #-}

  -- | Multiplying a quantity of type @u a@ with @'factor'@ will convert it
  -- to its corresponding standard unit @NormalizeUnit u a@
  --
  -- >>> factor @Hour :: Double
  -- 3600.0
  -- >>> factor @Celsius :: Double
  -- 1.0
  -- >>> factor @(Kilo Meter ./. Hour) :: Double
  -- 0.2777777777777778
  factor :: a

instance Fractional a => ConvertibleUnit NoUnit a

instance Fractional a => ConversionFactor NoUnit a where
  factor = 1
  {-# INLINE factor #-}

instance (Num a, ConversionFactor u a, ConversionFactor v a, IsUnit (NormalizeUnit (u .*. v)))
  => ConvertibleUnit (u .*. v) a

instance (Num a, ConversionFactor u a, ConversionFactor v a, IsUnit (NormalizeUnit (u .*. v)))
  =>  ConversionFactor (u .*. v) a where
  factor = factor @u * factor @v
  {-# INLINE factor #-}

instance (ConversionFactor u a, IsUnit (NormalizeUnit (u .^. n)),  KnownInt n)
  => ConvertibleUnit (u .^. n) a

instance (ConversionFactor u a, IsUnit (NormalizeUnit (u .^. n)),  KnownInt n)
  =>  ConversionFactor (u .^. n) a where
  factor = factor @u ^^ intVal (Proxy :: Proxy n)
  {-# INLINE factor #-}

-- | Convert a quantity to its corresponding standard unit by multiplying it
-- by  @'fromFactor'@.
--
-- >>> toBaseUnit' @Hour 1
-- Second 3600.0
-- >>> toBaseUnit' (Hour 1)
-- Second 3600.0
-- >>> toBaseUnit' @(Kilo Meter ./. Hour) 36
-- quantity @(Meter .*. Second .^- 1) 10.0
-- >>> toBaseUnit' (Celsius 0)
-- Kelvin 0.0
--
toBaseUnit' :: forall u a. ConversionFactor u a
  => u a -> NormalizeUnit u a
toBaseUnit' q = quantity (unQuantity q * factor @u)
{-# INLINE toBaseUnit' #-}

-- | Convert a standard quantity to a unit @u@ by multiplying it by
-- by  @'toFactor'@.
--
-- >>> fromBaseUnit' @Hour 1800
-- Hour 0.5
-- >>> fromBaseUnit' 1800 :: Hour Double
-- Hour 0.5
-- >>> fromBaseUnit' @(Kilo Meter ./. Hour) 10
-- quantity @(Kilo Meter .*. Hour .^- 1) 36.0
-- >>> fromBaseUnit' @Celsius 0
-- Celsius 0.0
--
fromBaseUnit' :: forall u a. ConversionFactor u a
  => NormalizeUnit u a -> u a
fromBaseUnit' q = quantity (unQuantity q / factor @u)
{-# INLINE fromBaseUnit' #-}

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
fromTo' q = quantity (unQuantity q * (factor @u / factor @v))
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


