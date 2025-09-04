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
-- [Warning] These instances are provided because they are convenient, but be careful !  This means that you can write
--
-- >>> Second 2 * Second 3
-- Second 6
--
-- Which does not respect dimension analysis: the multiplication of two time
-- quantities should be of dimension @T²@ and here it has dimension @T@.
--
-- All the operators proposed here solve this problem:
--
-- >>> Second 2 .*. Second 3
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
  , (~/~)
  -- ** Exponentiation
  , (.^.)
  , (~^.)
  ) where

import Data.Type.Int

import Data.Units.Base.System
import Data.Units.Base.Convert

----------------------------------- Addition -----------------------------------

-- | Add two quantities of same dimension. The unit of the right operand is converted to the unit of the left operand
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
  , ConvFactor u a, ConvFactor v a
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
  , ConvFactor v a, ConvFactor u a
  )
 => u a -> v a -> (BaseUnitOf u) a
u ~-~ v = quantity $ unQuantity (toBaseUnit' u) - unQuantity (toBaseUnit' v)
{-# INLINE (~-~) #-}

infixr 5 ~-~

-------------------------------- Multiplication --------------------------------


-- | Multiply two quantities of different dimensions.
--
-- Units of the same dimension are authorized only when the units are equal.
--
-- >>> Meter 2 .*. Second 3
-- quantity @(Meter .*. Second) 6
--
-- >>> Second 5 .*. Meter 2
-- quantity @(Meter .*. Second) 10
--
-- >>> Meter 5 .*. Meter 8
-- quantity @(Meter .^+ 2) 40
--
-- >>> Meter 2 .*. Kilo (Meter 3)
--  • Failed to multiply two different units ‘m’ and ‘km’ with the same dimension ‘L’.
--   Hint : Did you try to multiply via (.*.) or divide (./.)
--          two quantities with the same dimension but different
--          units ?
--   If so, you might want to use (~*.), (.*~), (~*~), or (~/~) instead.
(.*.) ::
  ( uv ~ NormalizeUnit (u .*. v)
  , IsUnit u, IsUnit v, IsUnit uv
  , Num a
  )
 => u a -> v a -> uv a
u .*. v = quantity $ unQuantity u * unQuantity v
{-# INLINE (.*.) #-}

infixr 7 .*.

-- | Multiply two quantities of the same dimension. The unit of the right
-- will be converted to the unit of the left operand.
--
-- >>> Meter 2 .*~ Kilo (Meter 3)
-- quantity @(Meter .^+ 2) 6000.0
--
-- >>> Kilo (Meter 3) .*~ Meter 2
-- quantity @(Kilo Meter .^+ 2) 6.0e-3
--
-- >>> Meter 2 .*~ Second 5
-- • Cannot convert unit ‘m’ of dimension ‘L’
--       to unit ‘s’ of dimension ‘T’.
--
(.*~) :: forall u v u2 a.
  ( u2 ~ NormalizeUnit (u .^+ 2) , IsUnit u2
  , FromTo' v u a
  )
 => u a -> v a -> u2 a
u .*~ v = quantity $ unQuantity u * unQuantity (fromTo' v :: u a)
{-# INLINE (.*~) #-}

infix 7 .*~

-- | Same as @'(.*~)'@ but it is the left operand that is converted.
--
-- >>> Milli (Meter 2) ~*. Kilo (Meter 3)
-- quantity @(Kilo Meter .^+ 2) 6.0e-6
--
-- >>> Kilo (Meter 3) ~*. Meter 2
-- quantity @(Meter .^+ 2) 6000.0
--
(~*.) ::
  ( v2 ~ NormalizeUnit (v .^+ 2), IsUnit v2
  , FromTo' u v a
  )
 => u a -> v a -> v2 a
(~*.) = flip (.*~)
{-# INLINE (~*.) #-}

infix 7 ~*.

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
  , ConvFactor u a, ConvFactor v a
  )
 => u a -> v a -> u2 a
u ~*~ v = quantity $ unQuantity (toBaseUnit' u) * unQuantity (toBaseUnit' v)
{-# INLINE (~*~) #-}

infix 7 ~*~

----------------------------------- Division -----------------------------------


-- | Divide two quantities of different dimensions.
--
-- Units of the same dimension are authorized only when the units are equal.
--
-- >>> Meter 4 ./. Second 2
-- quantity @(Meter .*. Second .^- 1) 2.0
--
--
(./.) ::
  (uv ~ NormalizeUnit (u ./. v)
  , IsUnit u, IsUnit v, IsUnit uv
  , Fractional a
  )
  => u a -> v a -> uv a
u ./. v = quantity (unQuantity u / unQuantity v)
{-# INLINE (./.) #-}

infix 6 ./.

-- | Divide two quantities of same dimensions. The numerator will be converted
-- to the denominator
--
-- Units of the same dimension are authorized only when the units are equal.
--
-- >>> Meter 4 ~/~ Kilo (Meter 1)
-- NoUnit 4.0e-3
--
--
(~/~) ::
  ( DimEq u v
  , ConvFactor u a, ConvFactor v a
  )
  => u a -> v a -> NoUnit a
u ~/~ v = quantity $ unQuantity (toBaseUnit' u)  / unQuantity (toBaseUnit' v)
{-# INLINE (~/~) #-}

infix 6 ~/~

-------------------------------- Exponentiation --------------------------------

-- | Raise a quantity to a power.
--
-- This is meant to be used with @'Data.Type.Int.Proxy'@
--
-- >>> Kilo (Meter 2) .^. pos2
-- quantity @(Kilo Meter .^+ 2) 4.0
--
(.^.) :: forall (n :: ZZ) proxy u a. (IsUnit u, KnownInt n, Fractional a)
  => u a -> proxy n -> (u .^. n) a
u .^. p = quantity $ unQuantity u ^^ intVal p
{-# INLINE (.^.) #-}

infix 8 .^.


-- | Raise a quantity to a power and convert to the standard unit.
--
-- >>> Kilo (Meter 2) ~^. neg1
-- quantity @(Meter .^- 1) 5.0e-4
--
(~^.) :: forall (n :: ZZ) proxy u un a.
  (KnownInt n, ConvFactor u a, un ~ BaseUnitOf u .^. n )
  => u a -> proxy n -> un a
u ~^. p = quantity @un $ unQuantity (toBaseUnit' u) ^^ intVal p
{-# INLINE (~^.) #-}

infix 8 ~^.


