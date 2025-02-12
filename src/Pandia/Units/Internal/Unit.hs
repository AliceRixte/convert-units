{-# LANGUAGE DeriveFunctor #-}

module Pandia.Units.Internal.Unit
  ( module Pandia.Units.Internal.Unit
  ) where

import Data.Kind
import Data.Coerce
import Data.Proxy

import Pandia.Units.Internal.Rel

----------------------------- Unit construction ------------------------------

-- | A unit is represented by a newtype constructor. A quantity of some newtype
-- @u@ is of type @u a@.
type Unit = Type -> Type


-- | A unit that has no dimension.
--
-- @
-- type MyHertz = NoUnit -/- Second
-- @
--
newtype NoUnit a = NoUnit a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)


-- | Multiplication of two units.
--
-- @
-- type MyForceMoment a = (Newton -*- Meter) a
-- @
--
newtype ((u :: Unit) -*- (v :: Unit)) a = MulUnit a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)
infixl 7 -*-

-- | Multiply two quantities
--
(-*-) :: forall u v a. (Coercible a (u a), Coercible a (v a), Num a)
 => u a -> v a -> (u -*- v) a
u -*- v = coerce (coerce u * coerce v :: a)
{-# INLINE (-*-) #-}


-- | Division of two units.
--
-- @
-- type MySpeed a = (Meter -/- Second) a
-- type MyMolarEntropy a = (Joule -/- Mole -*- Kelvin) a
-- @
--
-- Notice that division has priority over division.
--
newtype ((u :: Unit) -/- (v :: Unit)) a = PerUnit a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)
infix 6 -/-

-- | Divide two quantities
--
(-/-) :: forall u v a. (Coercible a (u a), Coercible a (v a), Fractional a)
  => u a -> v a -> (u -/- v) a
u -/- v = coerce (coerce u / coerce v :: a)
{-# INLINE (-/-) #-}



-- | Unit to the power of a positive natural number
--
-- Negative exponents are not supported. Use division and @'NoUnit'@ if you need
-- them.
--
-- @
-- type MyAcceleration a = (Meter -/- Second -^- 2) a
-- @
--
newtype ((u :: Unit) -^- (n :: Rel)) a = PowUnit a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)
infix 8 -^-

-- | Raise a quantity to a power
--
(-^-) :: forall u n a. (Coercible a (u a), Num a)
  => u a -> Proxy n -> (u -^- n) a
u -^- _ = coerce u
{-# INLINE (-^-) #-}



type family ElimDiv (u :: Unit) :: Unit where
    ElimDiv (u -*- v) = ElimDiv u -*- ElimDiv v
    ElimDiv (u -^- n) = ElimDiv u -^- n
    ElimDiv (u -/- u) = NoUnit
    ElimDiv (u -/- v) = ElimDiv u -*- ElimDiv v -^- Neg 1
    ElimDiv u = u

type family ElimPow (u :: Unit) :: Unit where
  ElimPow (u -/- v) = ElimPow u -/- ElimPow v
  ElimPow (u -*- v) = ElimPow u -*- ElimPow v
  ElimPow (u -^- Pos 0) = NoUnit
  ElimPow (u -^- Neg 0) = NoUnit
  ElimPow (u -^- Pos 1) = ElimPow u
  ElimPow ((u -^- n) -^- m) = ElimPow (u -^- (n `MulRel` m))
  ElimPow ((u -*- v) -^- n) = ElimPow (u -^- n -*- v -^- n)
  ElimPow u = u

type family ElimNoUnit (u :: Unit) :: Unit where
  ElimNoUnit (NoUnit -*- u) = ElimNoUnit u
  ElimNoUnit (u -*- NoUnit) = ElimNoUnit u
  ElimNoUnit (u -*- v) = ElimNoUnit u -*- ElimNoUnit v
  ElimNoUnit u = u

type family SimplifyUnit (u :: Unit) :: Unit where
  SimplifyUnit u =  ElimNoUnit (ElimNoUnit (ElimPow (ElimDiv u)))
  -- we need to apply ElimNoUnit twice because there can be one trailing at the end
