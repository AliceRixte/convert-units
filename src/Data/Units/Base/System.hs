{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuantifiedConstraints #-}

--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.Base.System
-- Description :  System of units
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- Describe a system of units and their dimensions.
--
--------------------------------------------------------------------------------

module Data.Units.Base.System
  (
  -- * Dimensions
    Dim
  , DimId
  , ShowDim
  , NoDim (..)
  , IsDim (..)
  , DimEq
  , NormalizeDim
  -- * Units
  , Unit
  , NormalizeUnit
  , NormalizeUnit'
  , ShowUnit (..)
  , prettysUnit
  , IsUnit (..)
  , NoUnit (..)
  , MetaUnit (..)
  -- * Quantity
  , quantity
  , unQuantity
  , showsQuantity
  , showQuantity
  , prettyQuantity
  , printQuantity
  -- * Unit and dimension constructors
  , type (.*.) (..)
  , type (./.)
  , type (.^.) (..)
  , type (.^+)
  , type (.^-)
  )
  where

import Data.Coerce
import Data.Kind
import Data.Proxy
import Data.Type.Ord
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeError
import GHC.TypeLits

import Data.Type.Int


---------------------------------- Dimension -----------------------------------

-- | A unit dimension.
--
--  Modeled as a newtype constructor, just like @'Unit'@.
--
-- >>> type Speed = Length -/- Time
--
type Dim = Type -> Type

-- | A dimension identifier.
--
-- This identifiers allow to sort the units when computing the standard unit.
--
-- >>> type instance DimId Length = 300
--
-- >>> :kind! NormalizeUnit (Second .^- 1 .*. Meter)
-- Meter .*. (Second .^. Neg 1)
--
--
-- Two different dimensions must have different identifiers. To make sure this
-- remains true, we maintain here an /exhaustive/ list of dimensions declared
-- in this package /and/ any package that depends on it. Please raise an issue
-- if you added a new dimension.
--
-- [This package:]
--
--  +--------------------------------------+-----+
--  | Dimension                            | Id  |
--  +======================================+=====+
--  | @'NoDim'@                            | 000 |
--  +--------------------------------------+-----+
--  | @'Data.Units.AngleSI.Angle.Angle'@   | 100 |
--  +--------------------------------------+-----+
--  | @'Data.Units.SI.Mass'@               | 200 |
--  +--------------------------------------+-----+
--  | @'Data.Units.SI.Length'@             | 300 |
--  +--------------------------------------+-----+
--  | @'Data.Units.SI.Time'@               | 400 |
--  +--------------------------------------+-----+
--  | @'Data.Units.SI.ElectricCurrent'@    | 500 |
--  +--------------------------------------+-----+
--  | @'Data.Units.SI.Temperature'@        | 600 |
--  +--------------------------------------+-----+
--  | @'Data.Units.SI.SubstanceAmount'@    | 700 |
--  +--------------------------------------+-----+
--  | @'Data.Units.SI.LuminousIntensity'@  | 800 |
--  +--------------------------------------+-----+
--
type family DimId (d:: Dim) :: Nat

-- | Pretty print a dimension in error messages
--
-- >>> type instance ShowDim Length = Text "L"
--
-- Using the following in a @'TypeError'@
--
-- @
-- ShowDim (Length .*. Time .^- 1)
-- @
--
-- will show @L.T⁻¹@
--
type family ShowDim (d :: Dim) :: ErrorMessage


-- | The dimension of non dimensional quantities
--
newtype NoDim a = NoDim a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)


type instance DimId NoDim = 0
type instance ShowDim NoDim = Text "NoDim"


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
          :<>: Text "’ of dimension ‘"
          :<>: ShowDim du
          :<>: Text "’"
          :$$: Text "to unit ‘"
          :<>: ShowUnitType v
          :<>: Text "’ of dimension ‘"
          :<>: ShowDim dv
          :<>: Text "’."
    )))

-------------------------- Dimension standardization ---------------------------

-- | Helper type family for defining DimOf for .*. and .^.
--
type family DimOf' (u :: Unit) :: Dim where
  DimOf' (u .*. NoUnit) = DimOf' u
  DimOf' (NoUnit .*. v) = DimOf' v
  DimOf' ((u .*. v) .*. w) = DimOf' (u .*. (v .*. w))
  DimOf' (u .*. v) = InsertDim (DimOf' u) (DimOf' v)
  DimOf' (NoUnit .^. n) = NoDim
  DimOf' ((u .*. v) .^. n) = DimOf' (u .^. n .*. v .^. n)
  DimOf' ((u .^. n) .^. m) = DimOf' (u .^. Mul n m)
  DimOf' (u .^. n) = NormalizeExpDim (DimOf u .^. n)
  DimOf' u = DimOf u


type family NormalizeDim d where
  NormalizeDim (d .*. NoDim) = NormalizeDim d
  NormalizeDim (NoDim .*. e) = NormalizeDim e
  NormalizeDim ((d .*. e) .*. f) = NormalizeDim (d .*. (e .*. f))
  NormalizeDim (d .*. e) = InsertDim (NormalizeDim d) (NormalizeDim e)
  NormalizeDim (NoDim .^. n) = NoDim
  NormalizeDim ((d .*. e) .^. n) = NormalizeDim (d .^. n .*. e .^. n)
  NormalizeDim ((d .^. n) .^. m) = NormalizeDim (d .^. Mul n m)
  NormalizeDim (d .^. n) = NormalizeExpDim (d .^. n)
  NormalizeDim d = d

type family InsertDim d e where
  InsertDim NoDim e = e
  InsertDim d NoDim = d
  InsertDim d (e .*. f) =
    InsertCmpDim (Compare (DimId d) (DimId e)) d (e .*. f)
  InsertDim d e =
    InsertCmpDim (Compare (DimId d) (DimId e)) d e

type family InsertCmpDim cmp d v where
  InsertCmpDim 'LT d (e .*. f) = d .*. e .*. f
  InsertCmpDim 'GT d (e .*. f) = e .*. InsertDim d f
  InsertCmpDim 'EQ d (e .*. f) = MulNoDim (MulPowDim d e) f
  InsertCmpDim 'LT d e = d .*. e
  InsertCmpDim 'GT d e = e .*. d
  InsertCmpDim 'EQ d e = MulPowDim d e

type family MulNoDim d e where
  MulNoDim NoDim e = e
  MulNoDim d NoDim = d
  MulNoDim d e = d .*. e

type family MulPowDim d e where
  MulPowDim (d .^. n) (d .^. m) = NormalizeExpDim (d .^. Add n m)
  MulPowDim d (d .^. m) = NormalizeExpDim (d .^. Add (Pos 1) m)
  MulPowDim (d .^. n) d = NormalizeExpDim (d .^. Add n (Pos 1))
  MulPowDim d d = d .^. Pos 2
  MulPowDim d e = TypeError (
         Text "Failed to multiply two different units ‘"
    :<>: ShowUnitType d
    :<>: Text "’ and ‘"
    :<>: ShowUnitType e
    :<>: Text "’ with the same dimension ‘"
    :<>: ShowDim (DimOf d)
    :<>: Text "’."
    :$$: Text "Hint : Did you try to multiply via (.*.) two quantities with"
    :$$: Text "       the same dimension but different units ?"
    :$$: Text "If so, you might want to use (~*-), (-*~) or (~*~) instead. "
    )

type family NormalizeExpDim u where
  NormalizeExpDim (u .^. Pos 1) = u
  NormalizeExpDim (u .^. Zero) = NoDim
  NormalizeExpDim u = u


------------------------------------ Units -------------------------------------



-- | A unit is represented by a newtype constructor. A quantity of some unit
-- @u@ is of type @u a@.
--
type Unit = Type -> Type

type NormalizeUnit u = DimToUnit (DimOf u)

class (IsUnit (DimToUnit d), forall a. Coercible (d a) a)
  => IsDim (d :: Dim) where
  type DimToUnit d :: Unit



--------------------------------------------------------------------------------

-- | Any unit must have a corresponding standard unit. Additionally, a unit is a
-- newtype constructor : a quantity @u a@ can always be coerced to its magnitude
-- @a@.
class (forall a. Coercible (u a) a) => IsUnit (u :: Unit) where
  type DimOf u :: Dim

-- | Make a quantity out of any numerical value (called the /magnitude/ of that
-- quantity)
--
-- >>> quantity @(Meter ./. Second) 1
-- quantity @(Meter .*. Second .^- 1) 1
quantity :: forall u a. IsUnit u => a -> u a
quantity = coerce
{-# INLINE quantity #-}

-- | Get the magnitude of a quantity.
--
--  @unQuantity (quantity @u a) === a @
--
unQuantity :: IsUnit u => u a -> a
unQuantity = coerce
{-# INLINE unQuantity #-}

-- | Units that can be shown as a string, or as a type error message.
--
class IsUnit u => ShowUnit (u :: Unit) where
  {-# MINIMAL showUnit |  showsUnitPrec #-}

  -- | Allows to print units in conversion error messages
  --
  -- >>> type ShowUnit Meter = "m"
  --
  type ShowUnitType u :: ErrorMessage

  showsUnitPrec :: Int -> ShowS
  showsUnitPrec _ = (showUnit @u ++)

  showUnit :: String
  showUnit = showsUnit @u ""

  -- | Same as @'showsUnitPrec'@ but for pretty printing.
  --
  prettysUnitPrec :: Int -> ShowS
  prettysUnitPrec _ = (prettyUnit @u ++)

  -- | Same as @'showUnit'@ but for pretty printing
  --
  -- >>> putStrLn $ prettyUnit @(Kilo Meter ./. Second)
  -- km.s⁻¹
  --
  prettyUnit :: String
  prettyUnit = prettysUnit @u ""


showsUnit :: forall u. ShowUnit u => ShowS
showsUnit = showsUnitPrec @u 0

prettysUnit :: forall u. ShowUnit u => ShowS
prettysUnit = prettysUnitPrec @u 0


showsQuantityPrec :: forall u a. (ShowUnit u, Show a) => Int -> u a -> ShowS
showsQuantityPrec d u = showParen (d > 10) $
    showString "quantity @" . showsUnitPrec @u 11 . showString " " .
      showsPrec 11 (unQuantity u)

showsQuantity :: (ShowUnit u, Show a) => u a -> ShowS
showsQuantity  = showsQuantityPrec 0

showQuantity :: (ShowUnit u, Show a) => u a -> String
showQuantity u = showsQuantity u ""


prettyQuantity :: forall u a. (ShowUnit u, Show a) => u a -> String
prettyQuantity u  = show (unQuantity u) ++ " " ++  prettyUnit @u

printQuantity :: (Show a, ShowUnit u) => u a -> IO ()
printQuantity = putStr . showQuantity


--------------------------------------------------------------------------------



newtype MetaUnit (u :: Unit) a = MetaUnit a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

instance ShowUnit u => ShowUnit (MetaUnit u) where
  type ShowUnitType (MetaUnit u) = ShowUnitType u
  prettysUnitPrec = prettysUnitPrec @u
  showsUnitPrec = showsUnitPrec @u

instance (Show a, ShowUnit u) => Show (MetaUnit u a) where
  showsPrec = showsQuantityPrec

instance IsUnit u => IsUnit (MetaUnit u) where
  type DimOf (MetaUnit u) = DimOf u

--------------------------------------------------------------------------------

-- | A unit that has no dimension.
--
-- @
-- type MyHertz = NoUnit ./. Second
-- @
--
newtype NoUnit a = NoUnit a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)

instance IsUnit NoUnit where
  type DimOf NoUnit = NoDim

instance IsDim NoDim where
  type DimToUnit NoDim = NoUnit


-- | Multiplication of two units.
--
-- @
-- type MyForceMoment = Newton .*. Meter
-- @
--
newtype ((u :: Unit) .*. (v :: Unit)) a = MulUnit a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via MetaUnit (u .*. v) a

infixr 7 .*.

type instance ShowDim (u .*. v) = ShowDim u :<>: Text "⋅" :<>: ShowDim v

instance (ShowUnit u, ShowUnit v) => ShowUnit (u .*. v) where
  type ShowUnitType (u .*. v) =
    ShowUnitType u
    :<>: Text "⋅" :<>: ShowUnitType v
  prettysUnitPrec d = showParen (d > 7) $
    prettysUnitPrec @u 7 . showString "⋅" .  prettysUnitPrec @v 7
  showsUnitPrec d = showParen (d > 7) $
    showsUnitPrec @u 7 . showString " .*. " .  showsUnitPrec @v 7


instance (IsUnit u, IsUnit v) => IsUnit (u .*. v) where
  type DimOf (u .*. v) = DimOf' (u .*. v)

instance (IsDim d, IsDim e) => IsDim (d .*. e) where
  type DimToUnit (d .*. e) = DimToUnit d .*. DimToUnit e



-------------------------------- Unit division ---------------------------------

type family InverseUnit u where
  InverseUnit (u .*. v) = InverseUnit u .*. InverseUnit v
  InverseUnit (u .^. n) = NormalizeExp (u .^. Negate n)
  InverseUnit NoUnit = NoUnit
  InverseUnit u = u .^. Neg 1

-- | Division of two units.
--
-- @
-- type MySpeed a = (Meter ./. Second) a
-- type MyMolarEntropy a = (Joule ./. Mole .*. Kelvin) a
-- @
--
-- Notice that multiplication has priority over division.
--
type family (u :: Unit) ./. (v :: Unit) :: Unit where
  u ./. v = u .*. InverseUnit v

infix 6 ./.


----------------------------- Unit exponentiation ------------------------------


-- | Exponentiation of a unit
--
-- @
-- type Acceleration = Meter .*. Second .^. Neg 2
-- @
--
newtype ((u :: Unit) .^. (n :: ZZ)) a = PowUnit a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via MetaUnit (u .^. n) a
infix 8 .^.

-- | Positive exponentiation of a unit
--
-- @
-- type Area = Meter .^+ 2
-- @
--
type a .^+ b = a .^. Pos b
infix 8 .^+

-- | Negative exponentiation of a unit
--
-- @
-- type Hertz = Second .^- 1
-- @
--
type a .^- b = a .^. Neg b
infix 8 .^-

type instance DimId (d .^. n) = DimId d
type instance ShowDim (d .^. n) = ShowDim d :<>: ShowIntExponent n

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

instance IsUnit u => IsUnit (u .^. n) where
  type DimOf (u .^. n) = DimOf' (u .^. n)

instance IsDim d  => IsDim (d .^. n) where
  type DimToUnit (d .^. n) = DimToUnit d .^. n


instance (ShowUnit u, KnownInt n) => ShowUnit (u .^. n) where
  type ShowUnitType (u .^. n) =
         ShowUnitType u :<>: ShowIntExponent n
  prettysUnitPrec d = showParen (d >= 8) $
    prettysUnitPrec @u 8 .  showString (toSuperscript <$> show (intVal (Proxy :: Proxy n)))
  showsUnitPrec d = showParen (d >= 8) $
    if n >= 0 then
      showsUnitPrec @u 8 . showString " .^+ " . shows n
    else
      showsUnitPrec @u 8 . showString " .^- " . shows (-n)
    where
      n = intVal (Proxy :: Proxy n)


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

------------------------------ Unit normalization ------------------------------

-- | Sort a unit by dimension, and try to collapse them into exponents without
-- converting it to standard units.
--
-- >>> :kind! NormalizeUnit' (Minute .*. Minute)
-- Minute .^. Pos 2
--
-- >>> 1 :: NormalizeUnit' (Minute .*. Second) Double
-- <interactive>:39:1: error: [GHC-47403]
-- • Failed to multiply two different units ‘min’ and ‘s’ with the same
--   dimension ‘T’.
--   Hint : Did you try to multiply via (.*.) two quantities with
--          the same dimension but different units ?
--
type family NormalizeUnit' u where
  NormalizeUnit' (u .*. NoUnit) = NormalizeUnit' u
  NormalizeUnit' (NoUnit .*. v) = NormalizeUnit' v
  NormalizeUnit' ((u .*. v) .*. w) = NormalizeUnit' (u .*. (v .*. w))
  NormalizeUnit' (u .*. v) = InsertForNormalize (NormalizeUnit' u) (NormalizeUnit' v)
  NormalizeUnit' (NoUnit .^. n) = NoUnit
  NormalizeUnit' ((u .*. v) .^. n) = NormalizeUnit' (u .^. n .*. v .^. n)
  NormalizeUnit' ((u .^. n) .^. m) = NormalizeUnit' (u .^. Mul n m)
  NormalizeUnit' (u .^. n) = NormalizeExp (u .^. n)
  NormalizeUnit' u = u -- ^ This is the only difference with StandardizeUnit

type family Insert u v where
  Insert NoUnit v = v
  Insert u NoUnit = u
  Insert u (v .*. w) =
    InsertCmp (Compare (DimId (DimOf u)) (DimId (DimOf v))) u (v .*. w)
  Insert u v =
    SwapCmp (Compare (DimId (DimOf u)) (DimId (DimOf v))) u v

type family NormalizeExp u where
  NormalizeExp (u .^. Pos 1) = u
  NormalizeExp (u .^. Zero) = NoUnit
  NormalizeExp u = u


type family InsertForNormalize u v where
  InsertForNormalize NoUnit v = v
  InsertForNormalize u NoUnit = u
  InsertForNormalize u (v .*. w) =
    InsertCmp (Compare
                  -- We need to standardize to get the dimension (not necessary
                  -- in StandardizeUnit becaus units are already standardized)
                  (DimId (DimOf u))
                  (DimId (DimOf v))
              ) u (v .*. w)
  InsertForNormalize u v =
    SwapCmp (Compare
                  -- We need to standardize to get the dimension (not necessary
                  -- in StandardizeUnit becaus units are already standardized)
                  (DimId (DimOf u))
                  (DimId (DimOf v))
            ) u v

type family InsertCmp cmp u v where
  InsertCmp 'LT u (v .*. w) = u .*. v .*. w
  InsertCmp 'GT u (v .*. w) = v .*. Insert u w
  InsertCmp 'EQ u (v .*. w) = MulNoUnit (MulSameDim u v) w
  InsertCmp c u v = TypeError (
        Text  "InsertCmp must be called with arguments of"
   :<>: Text "the form InsertCmp cmp u (v .*. w)"
   :$$: Text "  instead, it was called with InsertCmp "
   :<>: ShowType c
   :<>: Text " ("
   :<>: ShowType u
   :<>: Text ") ("
   :<>: ShowType v
   :<>: Text ")"
   )

type family MulNoUnit d e where
  MulNoUnit NoUnit e = e
  MulNoUnit d NoUnit = d
  MulNoUnit d e = d .*. e

type family SwapCmp cmp u v where
  SwapCmp 'LT u v = u .*. v
  SwapCmp 'GT u v = v .*. u
  SwapCmp 'EQ u v = MulSameDim u v

type family MulSameDim u v where
  MulSameDim (u .^. n) (u .^. m) = NormalizeExp (u .^. Add n m)
  MulSameDim u (u .^. m) = NormalizeExp (u .^. Add (Pos 1) m)
  MulSameDim (u .^. n) u = NormalizeExp (u .^. Add n (Pos 1))
  MulSameDim u u = u .^. Pos 2
  MulSameDim u v = TypeError (
         Text "Failed to multiply two different units ‘"
    :<>: ShowUnitType u
    :<>: Text "’ and ‘"
    :<>: ShowUnitType v
    :<>: Text "’ with the same dimension ‘"
    :<>: ShowDim (DimOf u)
    :<>: Text "’."
    :$$: Text "Hint : Did you try to multiply via (.*.) or divide (./.) "
    :$$: Text "       two quantities with the same dimension but different"
    :$$: Text "       units ?"
    :$$: Text "If so, you might want to use (~*.), (.*~), (~*~), or (~/~) instead."
    )
