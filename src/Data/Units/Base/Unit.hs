{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Data.Units.Base.Unit
  ( module Data.Units.Base.Unit
  , module GHC.TypeError
  )
  where

import Data.Coerce
import Data.Kind
import Data.Proxy
import Data.Type.Ord
import Data.Type.Bool
import GHC.TypeError
import GHC.TypeLits

import Data.Type.Int

import Data.Units.Base.Dimension

-- | A unit is represented by a newtype constructor. A quantity of some unit
-- @u@ is of type @u a@.
--
type Unit = Type -> Type
type StandardUnit = Type -> Type


type family DimOf (u :: StandardUnit) :: Dim

class (forall a. Coercible (u a) a) => IsUnit (u :: Unit) where
  type StdUnitOf u :: StandardUnit

forgetUnit :: IsUnit u => u a -> a
forgetUnit = coerce

toUnit :: forall u a. IsUnit u => a -> u a
toUnit = coerce


class IsUnit u => ShowUnit (u :: Unit) where
  type ShowUnitType u :: ErrorMessage

  showsPrecUnit :: Int -> ShowS
  showsPrecUnit _ = (showUnit @u ++)

  showUnit :: String
  showUnit = showsUnit @u ""


showsUnit :: forall u. ShowUnit u => ShowS
showsUnit = showsPrecUnit @u 0

showsPrecQuantity :: forall u a. (ShowUnit u, Show a) => Int -> u a -> ShowS
showsPrecQuantity d u = showParen (d > 10) $
    showString "checkUnit " . shows (coerce u :: a) . showString " \""
      . showString (showUnit @u) . showString "\""

showsQuantity :: (ShowUnit u, Show a) => u a -> ShowS
showsQuantity  = showsPrecQuantity 0

showQuantity :: (ShowUnit u, Show a) => u a -> String
showQuantity u = showsQuantity u ""



--------------------------------------------------------------------------------



newtype StdUnit (u :: StandardUnit) a = StdUnit a

instance ShowUnit u => ShowUnit (StdUnit u) where
  type ShowUnitType (StdUnit u) = ShowUnitType u
  showsPrecUnit = showsPrecUnit @u

instance (Show a, ShowUnit u) => Show (StdUnit u a) where
  showsPrec = showsPrecQuantity

-- | This is just the constant function. It allows to pretty print the unit using Show
checkUnit :: forall u a. ShowUnit u => u a -> String -> u a
checkUnit u s =
  if s == showUnit @u then
    u
  else
    error $ "Current unit \"" ++ showUnit @u
          ++  "\" does not match expected unit \""
          ++ s ++ "\""

instance IsUnit (StdUnit u) where
  type StdUnitOf (StdUnit u) = u

newtype NonStdUnit (u :: Unit) a = NonStdUnit a
  deriving Show via StdUnit u a


--------------------------------------------------------------------------------


-- | A unit that has no dimension.
--
-- @
-- type MyHertz = NoUnit -/- Second
-- @
--
newtype NoUnit a = NoUnit a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

type instance DimOf NoUnit = NoDim

instance IsUnit NoUnit where
  type StdUnitOf NoUnit = NoUnit

-- | Multiplication of two units.
--
-- @
-- type MyForceMoment = Newton -*- Meter
-- @
--
newtype ((u :: Unit) -*- (v :: Unit)) a = MulUnit a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)
  deriving Show via StdUnit (u -*- v) a
infixr 7 -*-

type instance DimOf (u -*- v) = DimOf u -*- DimOf v
type instance ShowDim (u -*- v) = ShowDim u :<>: Text "." :<>: ShowDim v

instance (ShowUnit u, ShowUnit v) => ShowUnit (u -*- v) where
  type ShowUnitType (u -*- v) =
         Text "(" :<>: ShowUnitType u
    :<>: Text "." :<>: ShowUnitType v
    :<>: Text ")"
  showsPrecUnit d = showParen (d > 7) $
    showsPrecUnit @u 7 . showString "." .  showsPrecUnit @v 7


instance (IsUnit u, IsUnit v) => IsUnit (u -*- v) where
  type StdUnitOf (u -*- v) = StandardizeUnit (u -*- v)



--------------------------------------------------------------------------------


-- | Unit to the power of a positive natural number
--
-- @
-- type MyAcceleration a = (Meter -*- Second -^- Neg 2) a
-- @
--
newtype ((u :: Unit) -^- (n :: ZZ)) a = PowUnit a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)
  deriving Show via StdUnit (u -^- n) a
infix 8 -^-

type instance DimOf (u -^- n) = DimOf u -^- n
type instance DimId (d -^- n) = DimId d
type instance ShowDim (d -^- n) = ShowDim d :<>: ShowIntExponent n

type family ShowIntExponent (n :: ZZ) :: ErrorMessage where
  ShowIntExponent (Pos n) =
      If (n <=? 9) (ShowDigitExponent n) (Text "^" :<>: ShowType n)
  ShowIntExponent Zero = Text "⁰"
  ShowIntExponent (Neg n) =
      If (n <=? 9) (Text "⁻" :<>: ShowDigitExponent n)
                   (Text "^-" :<>: ShowType n)

type family ShowDigitExponent (n :: Nat) :: ErrorMessage where
  ShowDigitExponent 0 = Text "⁰"
  ShowDigitExponent 1 = Text "¹"
  ShowDigitExponent 2 = Text "²"
  ShowDigitExponent 3 = Text "³"
  ShowDigitExponent 4 = Text "⁴"
  ShowDigitExponent 5 = Text "⁵"
  ShowDigitExponent 6 = Text "⁶"
  ShowDigitExponent 7 = Text "⁷"
  ShowDigitExponent 8 = Text "⁸"
  ShowDigitExponent 9 = Text "⁹"

instance IsUnit u => IsUnit (u -^- n) where
  type StdUnitOf (u -^- n) = StandardizeUnit (u -^- n)


instance (ShowUnit u, KnownInt n) => ShowUnit (u -^- n) where
  type ShowUnitType (u -^- n) =
         ShowUnitType u :<>: ShowIntExponent n
  showsPrecUnit d = showParen (d >= 8) $
    showsPrecUnit @u 8 .  showString (toSuperscript <$> show (intVal (Proxy :: Proxy n)))

toSuperscript :: Char -> Char
toSuperscript '0' = '⁰'
toSuperscript '1' = '¹'
toSuperscript '2' = '²'
toSuperscript '3' = '³'
toSuperscript '4' = '⁴'
toSuperscript '5' = '⁵'
toSuperscript '6' = '⁶'
toSuperscript '7' = '⁷'
toSuperscript '8' = '⁸'
toSuperscript '9' = '⁹'
toSuperscript '+' = '⁺'
toSuperscript '-' = '⁻'
toSuperscript ')' = '⁾'
toSuperscript '(' = '⁽'
toSuperscript '=' = '⁼'
toSuperscript a = a


--------------------------------------------------------------------------------


type family StandardizeUnit u where
  StandardizeUnit (u -*- NoUnit) = StandardizeUnit u
  StandardizeUnit (NoUnit -*- v) = StandardizeUnit v
  StandardizeUnit ((u -*- v) -*- w) =
    Insert (StandardizeUnit u) (StandardizeUnit (v -*- w))
  StandardizeUnit (u -*- v) = Insert (StandardizeUnit u) (StandardizeUnit v)
  StandardizeUnit (NoUnit -^- n) = NoUnit
  StandardizeUnit ((u -*- v) -^- n) = StandardizeUnit (u -^- n -*- v -^- n)
  StandardizeUnit ((u -^- n) -^- m) = StandardizeUnit (u -^- Mul n m)
  StandardizeUnit (u -^- n) = NormalizeExp (u -^- n)
  StandardizeUnit u = StdUnitOf u

type family Insert u v where
  Insert NoUnit v = v
  Insert u NoUnit = u
  Insert u (v -*- w) =
    InsertCmp (Compare (DimId (DimOf u)) (DimId (DimOf v))) u (v -*- w)
  Insert u v =
    SwapCmp (Compare (DimId (DimOf u)) (DimId (DimOf v))) u v

type family InsertCmp cmp u v where
  InsertCmp 'LT u (v -*- w) = u -*- v -*- w
  InsertCmp 'GT u (v -*- w) = v -*- Insert u w
  InsertCmp 'EQ u (v -*- w) = MulSameDim u v -*- w
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

type family SwapCmp cmp u v where
  SwapCmp 'LT u v = u -*- v
  SwapCmp 'GT u v = v -*- u
  SwapCmp 'EQ u v = MulSameDim u v


type family MulSameDim u v where
  MulSameDim (u -^- n) (u -^- m) = NormalizeExp (u -^- Add n m)
  MulSameDim u (u -^- m) = NormalizeExp (u -^- Add (Pos 1) m)
  MulSameDim (u -^- n) u = NormalizeExp (u -^- Add n (Pos 1))
  MulSameDim u u = u -^- Pos 2
  MulSameDim u v = TypeError (
         Text "Two standard units must have different dimensions"
    :$$: Text "  but here, both "
    :<>: ShowType u
    :<>: Text " and "
    :<>: ShowType v
    :<>: Text " have the same dimension"
    :<>: ShowType (DimOf u)
    :<>: Text "."
    )

type family NormalizeExp u where
  NormalizeExp (u -^- Pos 1) = u
  NormalizeExp (u -^- Zero) = NoUnit
  NormalizeExp u = u




--------------------------------------------------------------------------------

type family InverseUnit u where
  InverseUnit (u -*- v) = InverseUnit u -*- InverseUnit v
  InverseUnit (u -^- n) = NormalizeExp (u -^- Negate n)
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

