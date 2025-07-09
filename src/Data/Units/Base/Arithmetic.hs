
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}



module Data.Units.Base.Arithmetic
  (
  -- ** Addition
    (-+~)
  , (~+-)
  , (~+~)
  -- ** Subtraction
  , (--~)
  , (~--)
  , (~-~)
  -- ** Multiplication
  , (-*-)
  , (-*~)
  , (~*-)
  , (~*~)
  -- ** Division
  , (-/-)
  , (~/~)
  -- ** Exponentiation
  , (-^-)
  , (~^-)

  ) where


import Data.Coerce

import Data.Type.Int

import Data.Units.Base.Unit
import Data.Units.Base.Convert

----------------------------------- Addition -----------------------------------

-- | Add two quantities of same dimension. The unit of the right operand is converted to the unit of the left operand
--
-- >>> Kilo (Meter 5) -+~ Meter 80
-- ofUnit 5.08 "km"
--
--
-- >>> Meter 2 -+~ Second 3
--     • Failed to multiply two different units ‘m’ and ‘km’ with the same dimension ‘L’.
--      Hint : Did you try to multiply via (-*-) two quantities with
--             the same dimension but different units ?
--      If so, you might want to use (~*-), (-*~) or (~*~) instead.
--
(-+~) :: forall u v a.
  ( DimEq u v
  , ConvFactor v a, ConvFactor u a
  , Num a
  )
 => u a -> v a -> u a
u -+~ v = coerce (coerce u + coerce v * factorFrom @v * factorTo @u :: a)
{-# INLINE (-+~) #-}

infixr 5 -+~

-- | Same as @'(-+~)'@ but it is the left operand that is converted.
--
-- >>> Kilo (Meter 5) ~+- Meter 80
-- ofUnit 5080.0 "m"
--
(~+-) :: forall u v a.
  ( DimEq u v
  , ConvFactor v a, ConvFactor u a
  , Num a
  )
 => u a -> v a -> v a
u ~+- v = coerce (coerce u * factorFrom @u * factorTo @v + coerce v :: a)
{-# INLINE (~+-) #-}

infixr 5 ~+-

-- | Add two quantities of same dimension and convert to the standard unit.
--
-- >>> Kilo (Meter 1) ~+~ Milli (Meter 150)
-- ofUnit 1000.15 "m"
--
(~+~) :: forall u v a.
  ( DimEq u v
  , IsUnit (StandardizeUnit u)
  , ConvFactor v a, ConvFactor u a
  , Num a
  )
 => u a -> v a -> (StandardizeUnit u) a
u ~+~ v = coerce (coerce u * factorFrom @u + coerce v * factorFrom @v :: a)
{-# INLINE (~+~) #-}

infixr 5 ~+~

--------------------------------- Subtraction ----------------------------------

-- | Subtract two quantities of same dimension. The unit of the right operand is converted to the unit of the left operand
--
-- >>> Kilo (Meter 5) --~ Meter 80
-- ofUnit 4.92 "km"
--
(--~) :: forall u v a.
  ( DimEq u v
  , ConvFactor v a, ConvFactor u a
  , Num a
  )
 => u a -> v a -> u a
u --~ v = coerce (coerce u - coerce v * factorFrom @v * factorTo @u :: a)
{-# INLINE (--~) #-}

infixr 5 --~

-- | Same as @'(--~)'@ but it is the left operand that is converted.
--
-- >>> Kilo (Meter 5) ~-- Meter 80
-- ofUnit 4920.0 "m"
--
(~--) :: forall u v a.
  ( DimEq u v
  , ConvFactor v a, ConvFactor u a
  , Num a
  )
 => u a -> v a -> v a
u ~-- v = coerce (coerce u * factorFrom @u * factorTo @v - coerce v :: a)
-- {-# INLINE (~--) #-}

infixr 5 ~--

-- | Subtract two quantities of same dimension and convert to the standard unit.
--
-- >>> Kilo (Meter 1) ~-~ Milli (Meter 150)
-- ofUnit 999.85 "m"
--
(~-~) :: forall u v a.
  ( DimEq u v
  , IsUnit (StandardizeUnit u)
  , ConvFactor v a, ConvFactor u a
  , Num a
  )
 => u a -> v a -> (StandardizeUnit u) a
u ~-~ v = coerce (coerce u * factorFrom @u - coerce v * factorFrom @v :: a)
{-# INLINE (~-~) #-}

infixr 5 ~-~

-------------------------------- Multiplication --------------------------------


-- | Multiply two quantities of different dimensions.
--
-- Units of the same dimension are authorized only when the units are equal.
--
-- >>> Meter 2 -*- Second 3
-- ofUnit 6 "m.s"
--
-- >>> Second 5 -*- Meter 2
-- ofUnit 10 "m.s"
--
-- >>> Meter 5 -*- Meter 8
-- ofUnit 40 "m²"
--
-- >>> Meter 2 -*- Kilo (Meter 3)
--     • Failed to multiply two different units ‘m’ and ‘km’ with the same dimension ‘L’.
--      Hint : Did you try to multiply via (-*-) two quantities with
--             the same dimension but different units ?
--      If so, you might want to use (~*-), (-*~) or (~*~) instead.
--
(-*-) :: forall u v uv a.
  ( uv ~ NormalizeUnit (u -*- v)
  , IsUnit u, IsUnit v, IsUnit uv
  , Num a
  )
 => u a -> v a -> uv a
u -*- v = coerce (coerce u * coerce v :: a)
{-# INLINE (-*-) #-}

infixr 7 -*-

-- | Multiply two quantities of the same dimension. The unit of the right
-- will be converted to the unit of the left operand.
--
-- >>> Meter 2 -*~ Kilo (Meter 3)
-- ofUnit 6000.0 "m²"
--
-- >>> Kilo (Meter 3) -*~ Meter 2
-- ofUnit 6.0e-3 "km²"
--
-- >>> Meter 2 -*~ Second 5
-- • Cannot convert unit ‘m’ to unit ‘s’ because their dimensions do not match.
--   Dimension of ‘m’ is: L
--   Dimension of ‘s’ is: T
--
(-*~) :: forall u v u2 a.
  ( u2 ~ NormalizeUnit (u -^+ 2) , IsUnit u2
  , DimEq u v
  , ConvFactor u a, ConvFactor v a
  , Num a
  )
 => u a -> v a -> u2 a
u -*~ v = coerce (coerce u * coerce v * factorFrom @v * factorTo @u :: a)
{-# INLINE (-*~) #-}

infix 7 -*~

-- | Same as @'(-*~)'@ but it is the left operand that is converted.
--
-- >>> Milli (Meter 2) ~*~ Kilo (Meter 3)
-- ofUnit 6.0e-3 "km²"
--
-- >>> Kilo (Meter 3) ~*- Meter 2
-- ofUnit 6000.0 "m²"
--
(~*-) :: forall u v u2 a.
  ( u2 ~ NormalizeUnit (u -^+ 2), IsUnit u2
  , DimEq u v
  , ConvFactor u a, ConvFactor v a
  , Num a
  )
 => u a -> v a -> u2 a
u ~*- v = coerce (coerce u * coerce v * factorFrom @u * factorTo @v :: a)
{-# INLINE (~*-) #-}

infix 7 ~*-

-- | Multiply two quantities of the same dimension and convert both of them to the corresponding standard unity.
--
-- >>> Milli (Meter 2) ~*~ Kilo (Meter 3)
-- ofUnit 6.0 "m²"
--
-- >>> Meter 2 ~*~ Second 5
-- • Cannot convert unit ‘m’ to unit ‘s’ because their dimensions do not match.
--   Dimension of ‘m’ is: L
--   Dimension of ‘s’ is: T
--
(~*~) :: forall u v u2 a.
  ( u2 ~ StandardizeUnit u -^+ 2, IsUnit u2
  , DimEq u v
  , ConvFactor u a, ConvFactor v a
  , Num a
  )
 => u a -> v a -> u2 a
u ~*~ v = coerce (coerce u * coerce v * factorFrom @u * factorFrom @v :: a)
{-# INLINE (~*~) #-}

infix 7 ~*~

----------------------------------- Division -----------------------------------


-- | Divide two quantities of different dimensions.
--
-- Units of the same dimension are authorized only when the units are equal.
--
-- >>> Meter 4 -/- Second 2
-- ofUnit 2.0 "m.s⁻¹"
--
--
(-/-) :: forall u v a. (IsUnit u, IsUnit v, Fractional a)
  => u a -> v a -> (u -/- v) a
u -/- v = coerce (coerce u / coerce v :: a)
{-# INLINE (-/-) #-}

infix 6 -/-

-- | Divide two quantities of same dimensions. The numerator will be converted
-- to the denominator
--
-- Units of the same dimension are authorized only when the units are equal.
--
-- >>> Meter 4 ~/~ Kilo (Meter 1)
-- ofUnit 2.0 "m.s⁻¹"
--
--
(~/~) :: forall u v a.
  ( DimEq u v
  , ConvFactor u a, ConvFactor v a
  , Fractional a)
  => u a -> v a -> NoUnit a
u ~/~ v = coerce (coerce u * factorFrom @u / (coerce v * factorFrom @v) :: a)
{-# INLINE (~/~) #-}

infix 6 ~/~

-------------------------------- Exponentiation --------------------------------

-- | Raise a quantity to a power.
--
-- This is meant to be used with @'Data.Type.Int.Proxy'@
--
-- >>> Kilo (Meter 2) ~^- pos2
--
(-^-) :: forall (n :: ZZ) proxy u a. (IsUnit u, KnownInt n, Fractional a)
  => u a -> proxy n -> (u -^- n) a
u -^- p = coerce $ (coerce u :: a) ^^ intVal p
{-# INLINE (-^-) #-}

infix 8 -^-


-- | Raise a quantity to a power and convert to the standard unit.
--
-- >>> Kilo (Meter 2) ~^- neg1
-- ofUnit 5.0e-4 "m⁻¹"
--
(~^-) :: forall (n :: ZZ) proxy u a.
  (IsUnit u, KnownInt n, ConvFactor u a, Fractional a)
  => u a -> proxy n -> (StandardizeUnit u -^- n) a
u ~^- p = coerce $ (coerce u * factorFrom @u :: a ) ^^ intVal p
{-# INLINE (~^-) #-}

infix 8 ~^-


