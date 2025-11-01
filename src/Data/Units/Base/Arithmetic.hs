--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.Base.Arithmetic
-- Description :  Addition, multiplication and exponentiation of quantities
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- Addition, multiplication and exponentiation of quantities. Dimension analysis is done statically via the type system.
--
-- == Addition and multiplication of a quantity by a scalar
--
-- To add, multiply, divide, and so on, a quantity with a scalar , use its @'Fractional'@ instance :
--
-- >>> a = Milli (Second 5)
-- >>> 3 * a
-- quantity @(Milli Second) 15
--
-- [Warning] These instances are provided because they are convenient, but be careful !  This means that you can write:
--
-- >>> Second 2 * Second 3
-- Second 6
--
-- which does not respect dimension analysis: the multiplication of two time
-- quantities should be of dimension @T²@ and here it has dimension @T@.
--
-- Some of the operators proposed here solve this problem:
--
-- >>> Second 2 .*~ Second 3
-- quantity @(Second .^+ 2) 6
--
--------------------------------------------------------------------------------

module Data.Units.Base.Arithmetic
  (
  -- ** Addition
    (.+~)
  , (~+.)
  , (~+~)
  -- ** Subtraction
  , (.-~)
  , (~-.)
  , (~-~)
  -- ** Multiplication
  , (.*.)
  , (.*~)
  , (~*.)
  , (~*~)
  -- ** Division
  , (./.)
  , (./~)
  , (~/.)
  , (~/~)
  -- ** Exponentiation
  , (.^.)
  , (~^.)
  , (.^~)
  , (~^~)
  ) where

import Data.Type.Int

import Data.Units.Base.System
import Data.Units.Base.Convert

----------------------------------- Addition -----------------------------------

-- | Add two quantities of same dimension. The unit of the right operand is
-- converted to the unit of the left operand
--
-- >>> Kilo (Meter 5) .+~ Meter 80
-- quantity @(Kilo Meter) 5.08
--
-- >>> Meter 2 .+~ Second 3
--  • Cannot convert unit ‘s’ of dimension ‘T’
--    to unit ‘m’ of dimension ‘L’.
--
(.+~) :: forall u v a. FromTo' v u a => u a -> v a -> u a
u .+~ v = quantity (unQuantity u + unQuantity (fromTo' v :: u a))
{-# INLINE (.+~) #-}

infixr 5 .+~

-- | Same as @'(.+~)'@ but it is the left operand that is converted.
--
-- >>> Kilo (Meter 5) ~+. Meter 80
-- quantity @(Kilo Meter) 5080.0
--
(~+.) :: FromTo' u v a => u a -> v a -> v a
(~+.) = flip (.+~)
{-# INLINE (~+.) #-}

infixr 5 ~+.

-- | Add two quantities of same dimension and convert to the standard unit.
--
-- >>> Kilo (Meter 1) ~+~ Milli (Meter 150)
-- Meter 1000.15
--
(~+~) ::
  ( DimEq u v
  , ConversionFactor u a, ConversionFactor v a
  )
 => u a -> v a -> (BaseUnitOf u) a
u ~+~ v = quantity (unQuantity (toBaseUnit' u) + unQuantity (toBaseUnit' v))
{-# INLINE (~+~) #-}

infixr 5 ~+~

--------------------------------- Subtraction ----------------------------------

-- | Subtract two quantities of same dimension. The unit of the right operand is converted to the unit of the left operand
--
-- >>> Kilo (Meter 5) .-~ Meter 80
-- quantity @(Kilo Meter) 4.92
--
(.-~) :: forall u v a. FromTo' v u a => u a -> v a -> u a
u .-~ v = quantity (unQuantity u - unQuantity (fromTo' v :: u a))
{-# INLINE (.-~) #-}

infixr 5 .-~

-- | Same as @'(.-~)'@ but it is the left operand that is converted.
--
-- >>> Kilo (Meter 5) ~-. Meter 80
-- Meter 4920.0
--
(~-.) :: forall u v a. FromTo' u v a => u a -> v a -> v a
u ~-. v = quantity (unQuantity (fromTo' u :: v a) - unQuantity v)
-- {-# INLINE (~-.) #-}

infixr 5 ~-.

-- | Subtract two quantities of same dimension and convert to the standard unit.
--
-- >>> Kilo (Meter 1) ~-~ Milli (Meter 150)
-- Meter 999.85
--
(~-~) ::
  ( DimEq u v
  , ConversionFactor v a, ConversionFactor u a
  )
 => u a -> v a -> (BaseUnitOf u) a
u ~-~ v = quantity $ unQuantity (toBaseUnit' u) - unQuantity (toBaseUnit' v)
{-# INLINE (~-~) #-}

infixr 5 ~-~

-------------------------------- Multiplication --------------------------------



-- | Multiply two quantities.
--
-- Usage is not recommended, as this will result non standard units.
--
-- For instance:
--
-- >>> Kilo (Meter 2) .*. Milli (Meter 4)
-- quantity @(Kilo Meter .*. Milli Meter) 8
--
(.*.) ::
  ( IsUnit u, IsUnit v
  , Num a
  )
 => u a -> v a -> (u .*. v) a
u .*. v = quantity $ unQuantity u * unQuantity v
{-# INLINE (.*.) #-}

infixr 7 .*.

-- | Multiply two quantities, and tries to normalize the resulting unit, without
-- converting to base units.
--
-- >>> Meter 2 .*~ Meter 3 .*~ Meter 4
-- quantity @(Meter.^+3) 24
--
-- When two multiplied units have the same dimension, the right most unit is
-- converted to left most unit:
--
-- >>> Milli (Meter 2) .*~ Micro (Meter 3)
-- quantity @(Milli Meter.^+2) 6.0e-3
--
-- Derived units are not unfolded:
--
-- >>> Kilo Watt 3 .*~ Hour 5
-- quantity @(Kilo Watt .*. Hour) 14.999999999999998
--
-- Units are ordered, so that the result unit do not depend on the order of the
-- computations.
--
-- >>> Meter 2 .*~ Newton 2 .*~ Kilo (Meter 2) .*~ Kilo (Gram 1)
-- quantity @(Newton .*. Kilo Gram .*. Meter.^+2) 8000.0
--
(.*~) :: forall u v a uv.
  ( uv ~ u .*~ v
  , FromTo' (u .*. v) uv a
  , IsUnit u, IsUnit v, IsUnit uv
  , Num a
  )
  => u a -> v a -> uv a
u .*~ v = to' @uv (u .*. v)
{-# INLINE (.*~) #-}

infixr 7 .*~

-- | Same as '(.*~)' but with right priority
--
-- >>> Meter 2 ~*. Meter 3 ~*. Meter 4
-- quantity @(Meter.^+3) 24
--
-- >>> Milli (Meter 2) ~*. Micro (Meter 3)
-- quantity @(Micro Meter.^+2) 6000.000000000001
--
(~*.) :: forall u v a uv.
  ( uv ~ u ~*. v
  , FromTo' (u .*. v) uv a
  , IsUnit u, IsUnit v, IsUnit uv
  , Num a
  )
  => u a -> v a -> uv a
u ~*. v = to' @uv (u .*. v)
{-# INLINE (~*.) #-}

infixr 7 ~*.

-- | Multiply two quantities of the same dimension and convert both of them to the corresponding standard unity.
--
-- >>> Milli (Meter 2) ~*~ Kilo (Meter 3)
-- quantity @(Meter .^+ 2) 6.0
--
-- >>> Meter 2 ~*~ Second 5
-- • Cannot convert unit ‘m’ to unit ‘s’ because their dimensions do not match.
--   Dimension of ‘m’ is: L
--   Dimension of ‘s’ is: T
--
(~*~) ::
  ( u2 ~ BaseUnitOf u .^+ 2, IsUnit u2
  , DimEq u v
  , ConversionFactor u a, ConversionFactor v a
  )
 => u a -> v a -> u2 a
u ~*~ v = quantity $ unQuantity (toBaseUnit' u) * unQuantity (toBaseUnit' v)
{-# INLINE (~*~) #-}

infix 7 ~*~


----------------------------------- Division -----------------------------------

-- | Multiply two quantities.
--
-- Usage is not recommended, as this will result non standard units.
--
-- For instance:
--
-- >>> Kilo (Meter 2) ./. Milli (Meter 4)
-- quantity @(Kilo Meter .*. Milli Meter.^-1) 0.5
--
(./.) ::
  ( IsUnit u, IsUnit v, IsUnit (u ./. v)
  , Fractional a
  )
  => u a -> v a -> (u ./. v) a
u ./. v = quantity (unQuantity u / unQuantity v)
{-# INLINE (./.) #-}

infix 7 ./.

-- | Same '(.*~)' but for division.
--
-- >>> Milli (Meter 3) ./~ quantity @(Meter .^+ 2) 2
-- quantity @(Milli Meter.^-1) 1.5e-6
--
(./~) :: forall u v a uv.
  ( uv ~ u ./~ v
  , FromTo' (u ./. v) uv a
  , IsUnit u, IsUnit v, IsUnit uv
  , Num a
  )
  => u a -> v a -> uv a
u ./~ v = to' @uv (u ./. v)
{-# INLINE (./~) #-}

infix 7 ./~

-- | Same '(~/.)' but with right priority
--
-- >>> Milli (Meter 3) ~/. quantity @(Meter .^+ 2) 2
-- quantity @(Meter.^-1) 1.5e-3
--
(~/.) :: forall u v a uv.
  ( uv ~ u ~/. v
  , FromTo' (u ./. v) uv a
  , IsUnit u, IsUnit v, IsUnit uv
  , Num a
  )
  => u a -> v a -> uv a
u ~/. v = to' @uv (u ./. v)
{-# INLINE (~/.) #-}

infix 7 ~/.

-- | Divide two quantities of same dimensions. The numerator will be converted
-- to the denominator
--
-- Units of the same dimension are authorized only when the units are equal.
--
-- >>> Meter 4 ~/~ Kilo (Meter 1)
-- NoUnit 4.0e-3
--
(~/~) ::
  ( DimEq u v
  , ConversionFactor u a, ConversionFactor v a
  )
  => u a -> v a -> NoUnit a
u ~/~ v = quantity $ unQuantity (toBaseUnit' u)  / unQuantity (toBaseUnit' v)
{-# INLINE (~/~) #-}

infix 6 ~/~

-------------------------------- Exponentiation --------------------------------

-- | Raise a quantity to a power.
--
-- >>> Meter 2 .^. pos2
-- quantity @(Meter.^+2) 4.0
--
-- Usage is not recommended, as this will result non standard units.
--
-- For instance:
--
-- >>> (Meter 2 .*. Centi (Meter 30)) .^. pos2
-- quantity @((Meter .*. Centi Meter).^+2) 3600.0
--
(.^.) :: forall (n :: ZZ) proxy u a. (IsUnit u, KnownInt n, Fractional a)
  => u a -> proxy n -> (u .^. n) a
u .^. p = quantity $ unQuantity u ^^ intVal p
{-# INLINE (.^.) #-}

infix 8 .^.

-- | Raise a quantity to a power and tries to normalize the resulting unit,
-- without converting to base units.
--
-- This is meant to be used with @'SZZ'@ singletons, like 'pos', 'neg'
-- or 'zero', and there other flavors like 'pos2', 'neg3', etc.
--
-- >>> Meter 2 ~^. pos2
-- quantity @(Meter.^+2) 4.0
--
-- >>> Meter 2 ~^. pos @10
-- quantity @(Meter.^+10) 1024.0
--
-- >>> (Meter 2 .*. Centi (Meter 30)) ~^. pos2
-- quantity @(Centi Meter.^+4) 3.6e7
(~^.) :: forall (n :: ZZ) proxy u a un.
  (un ~ u ~^. n, FromTo' (u .^. n) un a, IsUnit u, KnownInt n, Fractional a)
  => u a -> proxy n -> un a
u ~^. p = to' @un (u .^. p)
{-# INLINE (~^.) #-}

infix 8 ~^.

-- | Same as @'(.^~)'@ but with priority to rightmost units.
--
-- >>> Meter 2 .^~ pos2
-- quantity @(Meter.^+2) 4.0
--
-- >>> (Meter 2 .*. Centi (Meter 30)) ~^. pos2
-- quantity @(Meter.^+4) 0.36000000000000004
--
(.^~ ) :: forall (n :: ZZ) proxy u a un.
  (un ~ u .^~  n, FromTo' (u .^. n) un a, IsUnit u, KnownInt n, Fractional a)
  => u a -> proxy n -> un a
u .^~  p = to' @un (u .^. p)
{-# INLINE (.^~ ) #-}

infix 8 .^~

-- | Raise a quantity to a power and convert to the standard unit.
--
-- >>> Kilo (Meter 2) ~^~ neg1
-- quantity @(Meter .^- 1) 5.0e-4
--
(~^~) :: forall (n :: ZZ) proxy u un a.
  (KnownInt n, ConversionFactor u a, un ~ BaseUnitOf u .^. n )
  => u a -> proxy n -> un a
u ~^~ p = quantity @un $ unQuantity (toBaseUnit' u) ^^ intVal p
{-# INLINE (~^~) #-}

infix 8 ~^~


