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
import Data.Type.Equality
import GHC.TypeError
import GHC.TypeLits

import Data.Type.Int

import Data.Units.Base.Dimension

-- | A unit is represented by a newtype constructor. A quantity of some unit
-- @u@ is of type @u a@.
--
type Unit = Type -> Type

type StdUnitOf u = StdUnitOf' (DimOf u)

class IsUnit (StdUnitOf' d) => IsDim (d :: Dim) where
  type StdUnitOf' d :: Unit

type family DimOf' (u :: Unit) :: Dim where
  DimOf' (u -*- NoUnit) = DimOf' u
  DimOf' (NoUnit -*- v) = DimOf' v
  DimOf' ((u -*- v) -*- w) =
    InsertDim (DimOf' u) (DimOf' (v -*- w))
  DimOf' (u -*- v) = InsertDim (DimOf' u) (DimOf' v)
  DimOf' (NoUnit -^- n) = NoDim
  DimOf' ((u -*- v) -^- n) = DimOf' (u -^- n -*- v -^- n)
  DimOf' ((u -^- n) -^- m) = DimOf' (u -^- Mul n m)
  DimOf' (u -^- n) = NormalizeExpDim (DimOf u -^- n)
  DimOf' u = DimOf u

type family StandardizeDim d where
  StandardizeDim (d -*- NoDim) = StandardizeDim d
  StandardizeDim (NoDim -*- e) = StandardizeDim e
  StandardizeDim ((d -*- e) -*- f) =
    InsertDim (StandardizeDim d) (StandardizeDim (e -*- f))
  StandardizeDim (d -*- e) = InsertDim (StandardizeDim d) (StandardizeDim e)
  StandardizeDim (NoDim -^- n) = NoDim
  StandardizeDim ((d -*- e) -^- n) = StandardizeDim (d -^- n -*- e -^- n)
  StandardizeDim ((d -^- n) -^- m) = StandardizeDim (d -^- Mul n m)
  StandardizeDim (d -^- n) = NormalizeExpDim (d -^- n)
  StandardizeDim d = d

type family MulDim (d :: Dim) (e :: Dim) where
  MulDim d e = StandardizeDim (d -*- e)

type family InsertDim d e where
  InsertDim NoDim e = e
  InsertDim d NoDim = d
  InsertDim d (e -*- f) =
    InsertCmpDim (Compare (DimId d) (DimId e)) d (e -*- f)
  InsertDim d e =
    InsertCmpDim (Compare (DimId d) (DimId e)) d e

type family InsertCmpDim cmp d v where
  InsertCmpDim 'LT d (e -*- f) = d -*- e -*- f
  InsertCmpDim 'GT d (e -*- f) = e -*- InsertDim d f
  InsertCmpDim 'EQ d (e -*- f) = MulNoDim (MulPowDim d e) f
  InsertCmpDim 'LT d e = d -*- e
  InsertCmpDim 'GT d e = e -*- d
  InsertCmpDim 'EQ d e = MulPowDim d e

type family MulNoDim d e where
  MulNoDim NoDim e = e
  MulNoDim d NoDim = d
  MulNoDim d e = d -*- e

type family MulPowDim d e where
  MulPowDim (d -^- n) (d -^- m) = NormalizeExpDim (d -^- Add n m)
  MulPowDim d (d -^- m) = NormalizeExpDim (d -^- Add (Pos 1) m)
  MulPowDim (d -^- n) d = NormalizeExpDim (d -^- Add n (Pos 1))
  MulPowDim d d = d -^- Pos 2
  MulPowDim d e = TypeError (
         Text "Failed to multiply two different units ‘"
    :<>: ShowUnitType d
    :<>: Text "’ and ‘"
    :<>: ShowUnitType e
    :<>: Text "’ with the same dimension ‘"
    :<>: ShowDim (DimOf d)
    :<>: Text "’."
    :$$: Text "Hint : Did you try to multiply via (-*-) two quantities with"
    :$$: Text "       the same dimension but different units ?"
    :$$: Text "If so, you might want to use (~*-), (-*~) or (~*~) instead. "
    )

type family NormalizeExpDim u where
  NormalizeExpDim (u -^- Pos 1) = u
  NormalizeExpDim (u -^- Zero) = NoDim
  NormalizeExpDim u = u



--------------------------------------------------------------------------------

-- | Any unit must have a corresponding standard unit. Additionally, a unit is a
-- newtype constructor : a quantity @u a@ can always be coerced to its magnitude
-- @a@.
class (forall a. Coercible (u a) a) => IsUnit (u :: Unit) where
  type DimOf u :: Dim

-- | Make a quantity out of any numerical value (called the /magnitude/ of that
-- quantity)
--
-- >>> quantity @(Meter -/- Second) 1
-- ofUnit 1 "m.s⁻¹"
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
  -- >>> putStrLn $ prettyUnit @(Kilo Meter -/- Second)
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

-- | Check whether a unit has the same unit as the one represented by a String.
--
-- This will fail if the units do not match and is the constant function if the
-- units match.
--
-- This is mainly useful for pretty printing units.
--
ofUnit :: forall u a. ShowUnit u => u a -> String -> u a
ofUnit u s =
  if s == showUnit @u then
    u
  else
    error $ "Current unit \"" ++ showUnit @u
          ++  "\" does not match expected unit \""
          ++ s ++ "\""





  -- type StdUnitOf (MetaUnit u) = u



--------------------------------------------------------------------------------


-- | A unit that has no dimension.
--
-- @
-- type MyHertz = NoUnit -/- Second
-- @
--
newtype NoUnit a = NoUnit a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)

instance IsUnit NoUnit where
  type DimOf NoUnit = NoDim

instance IsDim NoDim where
  type StdUnitOf' NoDim = NoUnit


-- | Multiplication of two units.
--
-- @
-- type MyForceMoment = Newton -*- Meter
-- @
--
newtype ((u :: Unit) -*- (v :: Unit)) a = MulUnit a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via MetaUnit (u -*- v) a

infixr 7 -*-

type instance ShowDim (u -*- v) = ShowDim u :<>: Text "." :<>: ShowDim v

instance (ShowUnit u, ShowUnit v) => ShowUnit (u -*- v) where
  type ShowUnitType (u -*- v) =
         Text "(" :<>: ShowUnitType u
    :<>: Text "." :<>: ShowUnitType v
    :<>: Text ")"
  prettysUnitPrec d = showParen (d > 7) $
    prettysUnitPrec @u 7 . showString "." .  prettysUnitPrec @v 7
  showsUnitPrec d = showParen (d > 7) $
    showsUnitPrec @u 7 . showString " -*- " .  showsUnitPrec @v 7


instance (IsUnit u, IsUnit v) => IsUnit (u -*- v) where
  type DimOf (u -*- v) = DimOf' (u -*- v)

instance (IsDim d, IsDim e) => IsDim (d -*- e) where
  type StdUnitOf' (d -*- e) = StdUnitOf' d -*- StdUnitOf' e



--------------------------------------------------------------------------------


-- | Exponentiation of a unit
--
-- @
-- type MyAcceleration a = (Meter -*- Second -^- Neg 2) a
-- @
--
newtype ((u :: Unit) -^- (n :: ZZ)) a = PowUnit a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via MetaUnit (u -^- n) a
infix 8 -^-

type a -^+ b = a -^- Pos b
infix 8 -^+

type a -^~ b = a -^- Neg b
infix 8 -^~

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
  type DimOf (u -^- n) = DimOf' (u -^- n)

instance IsDim d  => IsDim (d -^- n) where
  type StdUnitOf' (d -^- n) = StdUnitOf' d -^- n


instance (ShowUnit u, KnownInt n) => ShowUnit (u -^- n) where
  type ShowUnitType (u -^- n) =
         ShowUnitType u :<>: ShowIntExponent n
  prettysUnitPrec d = showParen (d >= 8) $
    prettysUnitPrec @u 8 .  showString (toSuperscript <$> show (intVal (Proxy :: Proxy n)))
  showsUnitPrec d = showParen (d >= 8) $
    if n >= 0 then
      showsUnitPrec @u 8 . showString " -^+ " . shows n
    else
      showsUnitPrec @u 8 . showString " -^~ " . shows (-n)
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
  StandardizeUnit (u -^- n) = NormalizeExp (StdUnitOf u -^- n)
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
  InsertCmp 'EQ u (v -*- w) = MulNoUnit (MulSameDim u v) w
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

type family MulNoUnit d e where
  MulNoUnit NoUnit e = e
  MulNoUnit d NoUnit = d
  MulNoUnit d e = d -*- e

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
         Text "Failed to multiply two different units ‘"
    :<>: ShowUnitType u
    :<>: Text "’ and ‘"
    :<>: ShowUnitType v
    :<>: Text "’ with the same dimension ‘"
    :<>: ShowDim (DimOf u)
    :<>: Text "’."
    :$$: Text "Hint : Did you try to multiply via (-*-) two quantities with"
    :$$: Text "       the same dimension but different units ?"
    :$$: Text "If so, you might want to use (~*-), (-*~) or (~*~) instead. "
    )

type family NormalizeExp u where
  NormalizeExp (u -^- Pos 1) = u
  NormalizeExp (u -^- Zero) = NoUnit
  NormalizeExp u = u

--------------------------------------------------------------------------------

-- | Same as StandardizeUnit but does not convert to standard units. This can
-- result in rectangle units.
type family NormalizeUnit u where
  NormalizeUnit (u -*- NoUnit) = NormalizeUnit u
  NormalizeUnit (NoUnit -*- v) = NormalizeUnit v
  NormalizeUnit ((u -*- v) -*- w) =
    InsertForNormalize (NormalizeUnit u) (NormalizeUnit (v -*- w))
  NormalizeUnit (u -*- v) = InsertForNormalize (NormalizeUnit u) (NormalizeUnit v)
  NormalizeUnit (NoUnit -^- n) = NoUnit
  NormalizeUnit ((u -*- v) -^- n) = NormalizeUnit (u -^- n -*- v -^- n)
  NormalizeUnit ((u -^- n) -^- m) = NormalizeUnit (u -^- Mul n m)
  NormalizeUnit (u -^- n) = NormalizeExp (u -^- n)
  NormalizeUnit u = u -- ^ This is the only difference with StandardizeUnit

-- | Same as StandardizeUnit but does not convert to standard units. This can
-- result in rectangle units.
type family InsertForNormalize u v where
  InsertForNormalize NoUnit v = v
  InsertForNormalize u NoUnit = u
  InsertForNormalize u (v -*- w) =
    InsertCmp (Compare
                  -- We need to standardize to get the dimension (not necessary
                  -- in StandardizeUnit becaus units are already standardized)
                  (DimId (DimOf (StandardizeUnit u)))
                  (DimId (DimOf (StandardizeUnit v)))
              ) u (v -*- w)
  InsertForNormalize u v =
    SwapCmp (Compare
                  -- We need to standardize to get the dimension (not necessary
                  -- in StandardizeUnit becaus units are already standardized)
                  (DimId (DimOf (StandardizeUnit u)))
                  (DimId (DimOf (StandardizeUnit v)))
            ) u v


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


--------------------------------------------------------------------------------

type family DimEq (u :: Unit) (v :: Unit) :: Constraint where
  DimEq u v = DimEqStd u v (StdUnitOf u) (StdUnitOf v)

-- Avoid computing too many times StdUnitOf ? (I don't know if GHC would
-- optimize it)
type family DimEqStd (u :: Unit) (v :: Unit) (stdu :: Unit) (stdv :: Unit)
  :: Constraint where
  DimEqStd u v stdu stdv =
    ( IsUnit u
    , IsUnit v
    , stdu ~ stdv
    , IsUnit stdu
    , If (stdu == stdv) (() :: Constraint)
      (TypeError (
          Text "Cannot convert unit ‘"
          :<>: ShowUnitType u
          :<>: Text "’ to unit ‘"
          :<>: ShowUnitType v
          :<>: Text "’ because their dimensions do not match."
          :$$: Text "Dimension of ‘"
          :<>: ShowUnitType u
          :<>: Text "’ is: "
          :<>: ShowDim (DimOf stdu)
          :$$: Text "Dimension of ‘"
          :<>: ShowUnitType v
          :<>: Text "’ is: "
          :<>: ShowDim (DimOf stdv)
    )))