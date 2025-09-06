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
  , IsDim (..)
  , ShowDim (..)
  , prettysDim
  , showsDim
  , showDimOf
  , prettyDimOf
  , putDimOf
  , NormalizeDim

  -- * Units
  , Unit
  , ShowUnit (..)
  , prettysUnit
  , showsUnit
  , IsUnit (..)

  -- * Quantity
  , quantity
  , unQuantity
  , showsQuantity
  , showQuantity
  , prettyQuantity
  , putQuantity

  -- * Unit and dimension constructors
  , NoDim (..)
  , NoUnit (..)
  , MetaUnit (..)
  , type (.*.) (..)
  , type (.^.) (..)
  , type (.^+)
  , type (.^-)


  -- * Unit normalization
  , NormalizeUnit
  , NormalizeUnitL
  , NormalizeUnitR
  , type (.*~)
  , type (~*.)
  , type (~*~)
  , type (./.)
  , type (./~)
  , type (~/.)
  , type (~/~)
  , type (~^.)
  , type (.^~)
  , type (~^~)
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

class (IsUnit (DimToUnit d), forall a. Coercible (d a) a)
  => IsDim (d :: Dim) where
  type DimToUnit d :: Unit

-- | A dimension identifier.
--
-- This identifiers allow to sort the units when computing the standard unit.
--
-- >>> type instance DimId Length = 300
--
-- >>> :kind! BaseUnitOf (Second .^- 1 .*. Meter)
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
--  +--------------------------------------+-----------------+
--  | Dimension                            | Id              |
--  +======================================+=================+
--  | Reserved                             |   0             |
--  +--------------------------------------+-----------------+
--  | @'NoDim'@                            |   1             |
--  +--------------------------------------+-----------------+
--  | @'Data.Units.AngleSI.Angle.Angle'@   | 1000            |
--  +--------------------------------------+-----------------+
--  | @'Data.Units.SI.Mass'@               | 2000            |
--  +--------------------------------------+-----------------+
--  | @'Data.Units.SI.Length'@             | 3000            |
--  +--------------------------------------+-----------------+
--  | @'Data.Units.SI.Time'@               | 4000            |
--  +--------------------------------------+-----------------+
--  | @'Data.Units.SI.ElectricCurrent'@    | 5000            |
--  +--------------------------------------+-----------------+
--  | @'Data.Units.SI.Temperature'@        | 6000            |
--  +--------------------------------------+-----------------+
--  | @'Data.Units.SI.AmountOfSubstance'@  | 7000            |
--  +--------------------------------------+-----------------+
--  | @'Data.Units.SI.LuminousIntensity'@  | 8000            |
--  +--------------------------------------+-----------------+
--
type family DimId (d:: Dim) :: ZZ

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

-- | Dimensions that can be shown as a string, or as a type error message.
--
class ShowDim (d :: Dim) where
  {-# MINIMAL showDim |  showsDimPrec #-}

  -- | Allows to print dimensions in conversion error messages
  --
  -- >>> type ShowDimType Length = "L"
  --
  type ShowDimType d :: ErrorMessage

  showsDimPrec :: Int -> ShowS
  showsDimPrec _ = (showDim @d ++)

  showDim :: String
  showDim = showsDim @d ""

  -- | Same as @'showsDimPrec'@ but for pretty printing.
  --
  prettysDimPrec :: Int -> ShowS
  prettysDimPrec _ = (prettyDim @d ++)

  -- | Same as @'showDim'@ but for pretty printing
  --
  -- >>> putStrLn $ prettyDim @(Kilo Meter ./. Second)
  -- km.s⁻¹
  --
  prettyDim :: String
  prettyDim = prettysDim @d ""


showsDim :: forall d. ShowDim d => ShowS
showsDim = showsDimPrec @d 0

prettysDim :: forall d. ShowDim d => ShowS
prettysDim = prettysDimPrec @d 0

showDimOf :: forall u a. (IsUnit u, ShowDim (DimOf u)) => u a -> String
showDimOf _ = showDim @(DimOf u)

prettyDimOf :: forall u a. (IsUnit u, ShowDim (DimOf u)) => u a -> String
prettyDimOf _ = prettyDim @(DimOf u)

putDimOf :: forall u a. (IsUnit u, ShowDim (DimOf u)) => u a -> IO ()
putDimOf = putStrLn . prettyDimOf



-- | The dimension of non dimensional quantities
--
newtype NoDim a = NoDim a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)


type instance DimId NoDim = Pos 1

instance ShowDim NoDim where
  type ShowDimType NoDim = Text "NoDim"
  showDim = "NoDim"
  prettyDim = "NoDim"


type family CmpDim (d :: Dim) (e :: Dim) :: Ordering where
  CmpDim (d .*. d') (e .*. e') =
    If (CmpDim d e == 'EQ) (CmpDim d' e') (CmpDim d e)
  CmpDim (d .*. d') e ='LT
  CmpDim d (e .*. e') = 'GT
  CmpDim (d .^. dn) (e .^. en) =
    If (CmpDim d e == 'EQ) (CmpSigned dn en) (CmpDim d e)
  CmpDim (d .^. dn) e =
    If (CmpDim d e == 'EQ) (CmpSigned dn (Pos 1)) (CmpDim d e)
  CmpDim d (e .^. en) =
    If (CmpDim d e == 'EQ) (CmpSigned (Pos 1) en) (CmpDim d e)
  CmpDim d e = CmpSigned (DimId d) (DimId e)


-------------------------- Dimension normalization ---------------------------


-- | Helper type family for defining DimOf for .*. and .^.
--
type family DimOf' (u :: Unit) :: Dim where
  DimOf' u = NormalizeDim (UnitToDim u)

type family UnitToDim (u :: Unit) :: Dim where
  UnitToDim (u .*. v) = UnitToDim u .*. UnitToDim v
  UnitToDim (u .^. n) = UnitToDim u .^. n
  UnitToDim u = DimOf u

type NormalizeDim d = NormalizeFlatDim (Flatten d)

type family Flatten u where
  Flatten (u .*. v) = Flatten u .*. Flatten v
  Flatten ((u .*. v) .^. n) = Flatten (u .^. n) .*. Flatten (v .^. n)
  Flatten ((u .^. n) .^. m) = Flatten (u .^. Mul n m)
  Flatten (u .^. n) = u .^. n
  Flatten u = u

type family NormalizeFlatDim d where
  NormalizeFlatDim (d .*. NoDim) = NormalizeFlatDim d
  NormalizeFlatDim (NoDim .*. e) = NormalizeFlatDim e
  NormalizeFlatDim ((d .*. e) .*. f) = NormalizeFlatDim (d .*. (e .*. f))
  NormalizeFlatDim (d .*. e) =
    InsertDim (NormalizeFlatDim d) (NormalizeFlatDim e)
  NormalizeFlatDim (NoDim .^. n) = NoDim
  NormalizeFlatDim (d .^. n) = NormalizeExpDim (d .^. n)
  NormalizeFlatDim d = d

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
    :<>: ShowDimType (DimOf d)
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




-- | Any unit must have a dimension. Additionally, a unit is a newtype
-- constructor : a quantity @u a@ can always be coerced to its magnitude @a@.
--
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

putQuantity :: (Show a, ShowUnit u) => u a -> IO ()
putQuantity = putStrLn . prettyQuantity


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

instance (ShowUnit u, ShowUnit v) => ShowUnit (u .*. v) where
  type ShowUnitType (u .*. v) =
    ShowUnitType u
    :<>: Text "⋅" :<>: ShowUnitType v
  prettysUnitPrec d = showParen (d > 7) $
    prettysUnitPrec @u 7 . showString "⋅" .  prettysUnitPrec @v 7
  showsUnitPrec d = showParen (d > 7) $
    showsUnitPrec @u 7 . showString " .*. " .  showsUnitPrec @v 7

instance (ShowDim u, ShowDim v) => ShowDim (u .*. v) where
  type ShowDimType (u .*. v) =
    ShowDimType u
    :<>: Text "⋅" :<>: ShowDimType v
  prettysDimPrec d = showParen (d > 7) $
    prettysDimPrec @u 7 . showString "⋅" .  prettysDimPrec @v 7
  showsDimPrec d = showParen (d > 7) $
    showsDimPrec @u 7 . showString " .*. " .  showsDimPrec @v 7


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
      showsUnitPrec @u 8 . showString ".^+" . shows n
    else
      showsUnitPrec @u 8 . showString ".^-" . shows (-n)
    where
      n = intVal (Proxy :: Proxy n)

instance (ShowDim u, KnownInt n) => ShowDim (u .^. n) where
  type ShowDimType (u .^. n) =
         ShowDimType u :<>: ShowIntExponent n
  prettysDimPrec d = showParen (d >= 8) $
    prettysDimPrec @u 8 .  showString (toSuperscript <$> show (intVal (Proxy :: Proxy n)))
  showsDimPrec d = showParen (d >= 8) $
    if n >= 0 then
      showsDimPrec @u 8 . showString ".^+" . shows n
    else
      showsDimPrec @u 8 . showString ".^-" . shows (-n)
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

type NormalizeUnit u = DimToUnit (DimOf u)

type (u :: Unit) ~*~ (v :: Unit) = NormalizeUnit (u .*. v)

infixr 7 ~*~

type (u :: Unit) ~/~ (v :: Unit) = NormalizeUnit (u ./. v)

infixr 7 ~/~

type (u :: Unit) ~^~ (n :: ZZ) = NormalizeUnit (u .^. n)

infixr 8 ~^~


type family MulNoUnit d e where
  MulNoUnit NoUnit e = e
  MulNoUnit d NoUnit = d
  MulNoUnit d e = d .*. e

type family NormalizeExp u where
  NormalizeExp (u .^. Pos 1) = u
  NormalizeExp (u .^. Zero) = NoUnit
  NormalizeExp u = u


--------------------- Unit normalization left priority ----------------------

type (u :: Unit) .*~ (v :: Unit) = NormalizeUnitL (u .*. v)

infixr 7 .*~

type (u :: Unit) ./~ (v :: Unit) = NormalizeUnitL (u ./. v)

infixr 7 ./~

type (u :: Unit) .^~ (n :: ZZ) = NormalizeUnitL (u .^. n)

infixr 8 .^~

-- | Tries to normalize a unit without converting to base units.
--
-- >>> :kind! NormalizeUnitR (Minute .*. Second)
-- Minute .^. Pos 2
--
type NormalizeUnitL u = NormalizeFlatUnitL (Flatten u)

type family NormalizeFlatUnitL u where
  NormalizeFlatUnitL (u .*. NoUnit) = NormalizeFlatUnitL u
  NormalizeFlatUnitL (NoUnit .*. v) = NormalizeFlatUnitL v
  NormalizeFlatUnitL ((u .*. v) .*. w) = NormalizeFlatUnitL (u .*. (v .*. w))
  NormalizeFlatUnitL (u .*. v) =
    InsertUnitL (NormalizeFlatUnitL u) (NormalizeFlatUnitL v)
  NormalizeFlatUnitL (NoUnit .^. n) = NoUnit
  NormalizeFlatUnitL (u .^. n) = NormalizeExp (u .^. n)
  NormalizeFlatUnitL u = u

type family InsertUnitL u v where
  InsertUnitL NoUnit v = v
  InsertUnitL u NoUnit = u
  InsertUnitL (u .*. v) w =
    TypeError (Text
      "Insert unit : Removing left association failed in NormalizeFlatUnitL")
  InsertUnitL (u .^. n) (v .^. m .*. w) =
      InsertCmpL (CmpDim (DimOf u) (DimOf v)) (u .^. n) (v .^. m .*. w)
  InsertUnitL (u .^. n) (v .*. w) =
      InsertCmpL (CmpDim (DimOf u) (DimOf v)) (u .^. n) (v .*. w)
  InsertUnitL (u .^. n) (v .^. m) =
      InsertCmpL (CmpDim (DimOf u) (DimOf v)) (u .^. n) (v .^. m)
  InsertUnitL (u .^. n) v =
      InsertCmpL (CmpDim (DimOf u) (DimOf v)) (u .^. n) v
  InsertUnitL u (v .^. m .*. w) =
      InsertCmpL (CmpDim (DimOf u) (DimOf v)) u (v .^. m .*. w)
  InsertUnitL u (v .*. w) =
      InsertCmpL (CmpDim (DimOf u) (DimOf v)) u (v .*. w)
  InsertUnitL u (v .^. m) =
      InsertCmpL (CmpDim (DimOf u) (DimOf v)) u (v .^. m)
  InsertUnitL u v =
      InsertCmpL (CmpDim (DimOf u) (DimOf v)) u v

type family InsertCmpL cmp u v where
  InsertCmpL 'LT u (v .*. w) = u .*. v .*. w
  InsertCmpL 'GT u (v .*. w) = v .*. InsertUnitL u w
  InsertCmpL 'EQ u (v .*. w) = MulNoUnit (MulSameDimL u v) w
  InsertCmpL 'LT u v = u .*. v
  InsertCmpL 'GT u v = v .*. u
  InsertCmpL 'EQ u v = MulSameDimL u v

type family MulSameDimL u v where
  MulSameDimL (u .^. n) (v .^. m) = NormalizeExp (u .^. Add n m)
  MulSameDimL u (v .^. m) = NormalizeExp (u .^. Add (Pos 1) m)
  MulSameDimL (u .^. n) v = NormalizeExp (u .^. Add n (Pos 1))
  MulSameDimL u v = u .^. Pos 2



---------------------- Unit normalization right priotiy -----------------------

-- The only difference with right is MulSameDim

-- | Same as @'(~*.)'@ but with priority to right most units
type (u :: Unit) ~*. (v :: Unit) = NormalizeUnitR (u .*. v)

infixr 7 ~*.

-- | Same as @'(~/.)'@ but with priority to right most units
type (u :: Unit) ~/. (v :: Unit) = NormalizeUnitR (u ./. v)

infixr 7 ~/.

-- | Same as @'(.^~)'@ but with priority to right most units
type (u :: Unit) ~^. (n :: ZZ) = NormalizeUnitR (u .^. n)

infix 8  ~^.

-- | Tries to normalize a unit without converting to base units. When two units
-- have the same dimension, they will be collapsed to an exponentiation right
-- most unit.
--
-- >>> :kind! NormalizeUnitR (Minute .*. Second)
-- Second .^. Pos 2
--
type NormalizeUnitR u = NormalizeFlatUnitR (Flatten u)

type family NormalizeFlatUnitR u where
  NormalizeFlatUnitR (u .*. NoUnit) = NormalizeFlatUnitR u
  NormalizeFlatUnitR (NoUnit .*. v) = NormalizeFlatUnitR v
  NormalizeFlatUnitR ((u .*. v) .*. w) = NormalizeFlatUnitR (u .*. (v .*. w))
  NormalizeFlatUnitR (u .*. v) =
    InsertUnitR (NormalizeFlatUnitR u) (NormalizeFlatUnitR v)
  NormalizeFlatUnitR (NoUnit .^. n) = NoUnit
  NormalizeFlatUnitR (u .^. n) = NormalizeExp (u .^. n)
  NormalizeFlatUnitR u = u

type family InsertUnitR u v where
  InsertUnitR NoUnit v = v
  InsertUnitR u NoUnit = u
  InsertUnitR (u .*. v) w =
    TypeError (Text
      "Insert unit : Removing left association failed in NormalizeFlatUnitR")
  InsertUnitR (u .^. n) (v .^. m .*. w) =
      InsertCmpR (CmpDim (DimOf u) (DimOf v)) (u .^. n) (v .^. m .*. w)
  InsertUnitR (u .^. n) (v .*. w) =
      InsertCmpR (CmpDim (DimOf u) (DimOf v)) (u .^. n) (v .*. w)
  InsertUnitR (u .^. n) (v .^. m) =
      InsertCmpR (CmpDim (DimOf u) (DimOf v)) (u .^. n) (v .^. m)
  InsertUnitR (u .^. n) v =
      InsertCmpR (CmpDim (DimOf u) (DimOf v)) (u .^. n) v
  InsertUnitR u (v .^. m .*. w) =
      InsertCmpR (CmpDim (DimOf u) (DimOf v)) u (v .^. m .*. w)
  InsertUnitR u (v .*. w) =
      InsertCmpR (CmpDim (DimOf u) (DimOf v)) u (v .*. w)
  InsertUnitR u (v .^. m) =
      InsertCmpR (CmpDim (DimOf u) (DimOf v)) u (v .^. m)
  InsertUnitR u v =
      InsertCmpR (CmpDim (DimOf u) (DimOf v)) u v

type family InsertCmpR cmp u v where
  InsertCmpR 'LT u (v .*. w) = u .*. v .*. w
  InsertCmpR 'GT u (v .*. w) = v .*. InsertUnitR u w
  InsertCmpR 'EQ u (v .*. w) = MulNoUnit (MulSameDimR u v) w
  InsertCmpR 'LT u v = u .*. v
  InsertCmpR 'GT u v = v .*. u
  InsertCmpR 'EQ u v = MulSameDimR u v

type family MulSameDimR u v where
  MulSameDimR (u .^. n) (v .^. m) = NormalizeExp (v .^. Add n m)
  MulSameDimR u (v .^. m) = NormalizeExp (v .^. Add (Pos 1) m)
  MulSameDimR (u .^. n) v = NormalizeExp (v .^. Add n (Pos 1))
  MulSameDimR u v = v .^. Pos 2
