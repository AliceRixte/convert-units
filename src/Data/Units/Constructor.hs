{-# LANGUAGE DeriveFunctor #-}

module Data.Units.Constructor
  ( module Data.Units.Constructor
  ) where

import Data.Kind
import Data.Coerce
import Data.Proxy
import Data.Type.Ord
import GHC.TypeLits


import Pandia.Units.Core.Rel

import Data.Convert.FromTo

type family Dimension (u :: Unit) :: Symbol

type instance Dimension (u -^- n) = Dimension u

type family ApplyStandard u :: Unit where
  ApplyStandard ((u -*- v) a) = ApplyStandard (u a) -*- ApplyStandard (v a)
  ApplyStandard ((u -^- n) a) = ApplyStandard (u a) -^- n
  ApplyStandard (u a) = GetUnitCons (Standard (u a))
  ApplyStandard u = TypeError (
    Text "The type family ApplyStandard should be called with a unit 'u a'"
    :$$: Text "but '"
    :<>: ShowType u
    :<>: Text "' is not of the form 'u a'."
    )

type family GetUnitCons u :: Unit where
  GetUnitCons (u a) = u
  GetUnitCons u = TypeError (
    Text "The type family GetUnitCons should be called with a unit 'u a'"
    :$$: Text "but '"
    :<>: ShowType u
    :<>: Text "' is not of the form 'u a'."
    )


--------------------------------------------------------------------------------

type family StandardizeUnit u where
  StandardizeUnit (u -*- NoUnit) = StandardizeUnit u
  StandardizeUnit (NoUnit -*- v) = StandardizeUnit v
  StandardizeUnit (u -*- v) = Insert (StandardizeUnit u) (StandardizeUnit v)
  StandardizeUnit (NoUnit -^- n) = NoUnit
  StandardizeUnit ((u -*- v) -^- n) = StandardizeUnit (u -^- n -*- v -^- n)
  StandardizeUnit ((u -^- n) -^- m) = StandardizeUnit (u -^- MulRel n m)
  StandardizeUnit (u -^- n) = NormalExp (u -^- n)
  StandardizeUnit u = u

type family Insert u v where
  Insert NoUnit v = v
  Insert u NoUnit = u
  Insert u (v -*- w) =
    InsertCmp (Compare (Dimension u) (Dimension v)) u (v -*- w)
  Insert u v =
    CombineCmp (Compare (Dimension u) (Dimension v)) u v

type family InsertCmp cmp u v where
  InsertCmp 'LT u (v -*- w) = u -*- v -*- w
  InsertCmp 'GT u (v -*- w) = v -*- Insert u w
  InsertCmp 'EQ u (v -*- w) = SameDim u v -*- w
  InsertCmp c u v = TypeError (
        Text  "InsertCmp must be called with arguments of"
   :<>: Text "the form InsertCmp cmp u (v -*- w)"
   :$$: Text "  instead, it was called with InsertCmp "
   :<>: ShowType c
   :<>: Text " ("
   :<>: ShowType u
   :<>: Text ") ("
   :<>: ShowType v
   :<>: Text ")"
   )

type family CombineCmp cmp u v where
  CombineCmp 'LT u v = u -*- v
  CombineCmp 'GT u v = v -*- u
  CombineCmp 'EQ u v = SameDim u v

type family SameDim u v where
  SameDim (u -^- n) (u -^- m) = NormalExp (u -^- SumRel n m)
  SameDim u (u -^- m) = NormalExp (u -^- SumRel (Pos 1) m)
  SameDim (u -^- n) u = NormalExp (u -^- SumRel n (Pos 1))
  SameDim u u = NormalExp (u -^- Pos 2)
  SameDim u v = TypeError (
         Text "Two standard units must have different dimensions"
    :$$: Text "  but here, both "
    :<>: ShowType u
    :<>: Text " and "
    :<>: ShowType v
    :<>: Text " have the same dimension"
    :<>: Text (Dimension u)
    :<>: Text "."
    )

type family NormalExp u where
  NormalExp (u -^- Pos 1) = u
  NormalExp (u -^- Pos 0) = NoUnit
  NormalExp (u -^- Neg 0) = NoUnit
  NormalExp u = u

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

type instance Dimension NoUnit = ""
type instance Standard (NoUnit a) = NoUnit a

-- | Multiplication of two units.
--
-- @
-- type MyForceMoment = Newton -*- Meter
-- @
--
newtype ((u :: Unit) -*- (v :: Unit)) a = MulUnit a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)
infixr 7 -*-

type instance Standard ((u -*- v) a) =
  StandardizeUnit (ApplyStandard ((u -*- v) a)) a


-- | Multiply two quantities
--
(-*-) :: forall u v a. (Coercible a (u a), Coercible a (v a), Num a)
 => u a -> v a -> (u -*- v) a
u -*- v = coerce (coerce u * coerce v :: a)
{-# INLINE (-*-) #-}

type family InverseUnit u where
  InverseUnit (u -*- v) = InverseUnit u -*- InverseUnit v
  InverseUnit (u -^- n) = NormalExp (u -^- NegateRel n)
  InverseUnit NoUnit = NoUnit
  InverseUnit u = u -^- Neg 1


-- | Division of two units.
--
-- @
-- type MySpeed a = (Meter -/- Second) a
-- type MyMolarEntropy a = (Joule -/- Mole -*- Kelvin) a
-- @
--
-- Notice that multiplication has priority over division.
--
type family u -/- v where
  u -/- v = u -*- InverseUnit v

infix 6 -/-

-- | Divide two quantities
--
(-/-) :: forall u v a. (Coercible a (u a), Coercible a (v a), Fractional a)
  => u a -> v a -> (u -/- v) a
u -/- v = coerce (coerce u / coerce v :: a)
{-# INLINE (-/-) #-}



-- | Unit to the power of a positive natural number
--
-- @
-- type MyAcceleration a = (Meter -*- Second -^- Neg 2) a
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
