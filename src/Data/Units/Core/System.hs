{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.Core.System
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

module Data.Units.Core.System
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
  , prettyUnitInfo
  , putInfoU
  , IsUnit (..)

  -- * Quantity
  , quantity
  , unQuantity
  , showQuantity
  , prettyQuantity
  , putQuantity
  , putInfoQ

  -- * Unit and dimension constructors
  , NoDim (..)
  , NoUnit (..)
  , MetaUnit (..)
  , type (.*.) (..)
  , type (.^.) (..)
  , type (.^+)
  , type (.^-)


  -- * Unit normalization
  , BaseUnitOf
  , NormalizeUnitL
  , NormalizeUnitR
  -- ** Normalization operators
  -- *** Multiplication
  , type (.*~)
  , type (~*.)
  , type (~*~)
  -- *** Division
  , type (./.)
  , type (./~)
  , type (~/.)
  , type (~/~)
  -- *** Exponentiation
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
--  +----------------------------------------------------+----------------+
--  | Dimension                                          | Id             |
--  +====================================================+================+
--  | Reserved                                           |   0            |
--  +----------------------------------------------------+----------------+
--  | @'NoDim'@                                          |   1            |
--  +----------------------------------------------------+----------------+
--  | @'Data.Units.AngleSI.System.Angle'@                | 1000           |
--  +----------------------------------------------------+----------------+
--  | @'Data.Units.SI.System.Mass'@                      | 2000           |
--  +----------------------------------------------------+----------------+
--  | @'Data.Units.SI.System.Length'@                    | 3000           |
--  +----------------------------------------------------+----------------+
--  | @'Data.Units.SI.System.Time'@                      | 4000           |
--  +----------------------------------------------------+----------------+
--  | @'Data.Units.SI.System.ElectricCurrent'@           | 5000           |
--  +----------------------------------------------------+----------------+
--  | @'Data.Units.SI.System.Temperature'@               | 6000           |
--  +----------------------------------------------------+----------------+
--  | @'Data.Units.SI.System.AmountOfSubstance'@         | 7000           |
--  +----------------------------------------------------+----------------+
--  | @'Data.Units.SI.System.LuminousIntensity'@         | 8000           |
--  +----------------------------------------------------+----------------+
--
type family DimId (d:: Dim) :: ZZ

-- | Dimensions that can be shown as a string, or as a type error message.
--
class ShowDim (d :: Dim) where
  {-# MINIMAL showDim |  showsDimPrec #-}

  -- | Allows to print dimensions in conversion error messages
  --
  -- >>> type ShowDimType Length = "L"
  --
  type ShowDimType d :: ErrorMessage

  -- | Convert a dimension to a readable string
  --
  -- @'showsDimPrec'@ should satisfy the law :
  --
  -- @showsDimPrec d x r ++ s  ==  showsPrec d x (r ++ s)@
  showsDimPrec :: Int -> ShowS
  showsDimPrec _ = (showDim @d ++)

  -- | Convert a dimension to a string representing its type.
  --
  -- >>> showDim  @(Length ./. Time)
  -- "Length .*. Time.^-1"
  showDim :: String
  showDim = showsDim @d ""

  -- | Same as @'showsDimPrec'@ but for pretty printing.
  --
  -- @'prettysDimPrec'@ should satisfy the law :
  --
  -- @prettysDimPrec d x r ++ s  ==  prettysPrec d x (r ++ s)@
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

-- | Equivalent to 'showsDimPrec' with a precedence of 0.
--
showsDim :: forall d. ShowDim d => ShowS
showsDim = showsDimPrec @d 0

-- | Equivalent to 'prettysDimPrec' with a precedence of 0.
--
prettysDim :: forall d. ShowDim d => ShowS
prettysDim = prettysDimPrec @d 0

-- | Show the dimension of a quantity.
--
-- >>>  showDimOf (quantity @(Kilo Meter ./. Second) 1)
-- "Length .*. Time.^-1"
--
showDimOf :: forall u a. (IsUnit u, ShowDim (DimOf u)) => u a -> String
showDimOf _ = showDim @(DimOf u)

-- | Same as 'showDimOf' but for pretty printing.
-- >>> putStrLn $ prettyDimOf (quantity @(Kilo Meter ./. Second) 1)
-- L.T⁻¹
prettyDimOf :: forall u a. (IsUnit u, ShowDim (DimOf u)) => u a -> String
prettyDimOf _ = prettyDim @(DimOf u)

-- | Print the dimension of a quantity.
--
-- >>> putDimOf (quantity @(Kilo Meter ./. Second) 1)
-- L.T⁻¹
--
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

  -- | Convert a unit to a readable string
  --
  -- @'showsUnitPrec'@ should satisfy the law :
  --
  -- @showsUnitPrec d x r ++ s  ==  showsPrec d x (r ++ s)@
  --
  showsUnitPrec :: Int -> ShowS
  showsUnitPrec _ = (showUnit @u ++)

  -- | Convert a unit to a string representing its type.
  --
  -- >>> showUnit @(Kilo Meter ./. Second)
  -- "Kilo Meter .*. Second.^-1"
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


-- | Equivalent to 'showsUnitPrec' with a precedence of 0.
--
showsUnit :: forall u. ShowUnit u => ShowS
showsUnit = showsUnitPrec @u 0

-- | Equivalent to 'prettysUnitPrec' with a precedence of 0.
--
prettysUnit :: forall u. ShowUnit u => ShowS
prettysUnit = prettysUnitPrec @u 0

-- | Pretty print information about a unit, its dimension and its normalized
-- form.
--
prettyUnitInfo :: forall u du nu.
  ( du ~ DimOf u
  , nu ~ BaseUnitOf u
  , ShowUnit u
  , ShowDim du
  , ShowUnit nu
  ) => String
prettyUnitInfo =
  "Unit:       " ++ showUnit @u  ++ "\n" ++
  " abbr:      " ++ prettyUnit @u  ++ "\n" ++
  "Dimension:  " ++ showDim @du  ++ "\n" ++
  " abbr:      " ++ prettyDim @du  ++ "\n" ++
  "Normalized: " ++ showUnit @nu ++ "\n" ++
  " abbr:      " ++ prettyUnit @nu ++ "\n"

-- | Print information about a unit, its dimension and its normalized form.
--
-- >>> putInfoU @Newton
-- Unit:       Newton
--  abbr:      N
-- Dimension:  Mass .*. Length .*. Time.^-2
--  abbr:      M⋅L⋅T⁻²
-- Normalized: Kilo Gram .*. Meter .*. Second.^-2
--  abbr:      kg⋅m⋅s⁻²
--
putInfoU :: forall u du nu.
  ( du ~ DimOf u
  , nu ~ BaseUnitOf u
  , ShowUnit u
  , ShowDim du
  , ShowUnit nu
  ) => IO ()
putInfoU = putStr $ prettyUnitInfo @u

-- | Same as 'prettyUnitInfo' but for quantities.
--
prettyQuantityInfo :: forall u a.
  ( ShowUnit u
  , ShowDim (DimOf u)
  , ShowUnit (BaseUnitOf u)
  , Show a
  ) => u a -> String
prettyQuantityInfo u = prettyUnitInfo @u ++
  "Magnitude:  " ++ show (unQuantity u) ++ "\n"

-- | Same as 'putInfoU' but for quantities.
--
-- >>> putInfoQ (Newton 4)
-- Unit:       Newton
--  abbr:      N
-- Dimension:  Mass .*. Length .*. Time.^-2
--  abbr:      M⋅L⋅T⁻²
-- Normalized: Kilo Gram .*. Meter .*. Second.^-2
--  abbr:      kg⋅m⋅s⁻²
-- Magnitude:  4
putInfoQ :: forall u a.
  ( ShowUnit u
  , ShowDim (DimOf u)
  , ShowUnit (BaseUnitOf u)
  , Show a
  ) => u a -> IO ()
putInfoQ u = putStr $ prettyQuantityInfo @u u


-- | Same as 'showsUnitPrec' but for quantities.
--
showsQuantityPrec :: forall u a. (ShowUnit u, Show a) => Int -> u a -> ShowS
showsQuantityPrec d u = showParen (d > 10) $
    showString "quantity @" . showsUnitPrec @u 11 . showString " " .
      showsPrec 11 (unQuantity u)

-- | Equivalent to 'showsQuantityPrec' with a precedence of 0.
--
showsQuantity :: (ShowUnit u, Show a) => u a -> ShowS
showsQuantity  = showsQuantityPrec 0

-- | Same as 'showUnit' but for quantities
--
-- >>> showQuantity (quantity @(Kilo Meter ./. Second) 1)
-- "quantity @(Kilo Meter .*. Second.^-1) 1.0"
--
showQuantity :: (ShowUnit u, Show a) => u a -> String
showQuantity u = showsQuantity u ""

-- | Same as 'prettyUnit' but for quantities
--
-- >>> putStrLn $ prettyQuantity (quantity @(Kilo Meter ./. Second) 1)
-- 1 km.s⁻¹
--
prettyQuantity :: forall u a. (ShowUnit u, Show a) => u a -> String
prettyQuantity u  = show (unQuantity u) ++ " " ++  prettyUnit @u

-- | Pretty print a quantity.
--
-- >>> putQuantity (quantity @(Kilo Meter ./. Second) 1)
-- 1 km.s⁻¹
putQuantity :: (Show a, ShowUnit u) => u a -> IO ()
putQuantity = putStrLn . prettyQuantity


--------------------------------------------------------------------------------


-- | A unit that can represent any unit.
--
-- This can be used with the `deriving via` mechanism to derive some of the
-- unit instances.
--
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

-- | Normalizes a unit by converting it to a product of  exponentiations of base
-- units.
type BaseUnitOf u = DimToUnit (DimOf u)

-- | Multiplies two units and normalizes the result.
--
type (u :: Unit) ~*~ (v :: Unit) = BaseUnitOf (u .*. v)

infixr 7 ~*~

-- | Divides two units and normalizes the result.
--
type (u :: Unit) ~/~ (v :: Unit) = BaseUnitOf (u ./. v)

infixr 6 ~/~

-- | Exponentiates a unit and normalizes the result.
--
type (u :: Unit) ~^~ (n :: ZZ) = BaseUnitOf (u .^. n)

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

-- | Tries to normalize a unit without converting to base units.
--
-- >>> :kind! NormalizeUnitR (Minute .*. Second)
-- Minute .^. Pos 2
--
type NormalizeUnitL u = NormalizeFlatUnitL (Flatten u)

-- | Multiplies two units and use left weak normalization.
type (u :: Unit) .*~ (v :: Unit) = NormalizeUnitL (u .*. v)

infixr 7 .*~

-- | Divides two units and use left weak normalization.
type (u :: Unit) ./~ (v :: Unit) = NormalizeUnitL (u ./. v)

infixr 6 ./~

-- | Exponentiates a unit and use left weak normalization.
type (u :: Unit) .^~ (n :: ZZ) = NormalizeUnitL (u .^. n)

infixr 8 .^~


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


-- | Tries to normalize a unit without converting to base units. When two units
-- have the same dimension, they will be collapsed to an exponentiation right
-- most unit.
--
-- >>> :kind! NormalizeUnitR (Minute .*. Second)
-- Second .^. Pos 2
--
type NormalizeUnitR u = NormalizeFlatUnitR (Flatten u)

-- | Same as @'(~*.)'@ but with priority to right most units
type (u :: Unit) ~*. (v :: Unit) = NormalizeUnitR (u .*. v)

infixr 7 ~*.

-- | Same as @'(~/.)'@ but with priority to right most units
type (u :: Unit) ~/. (v :: Unit) = NormalizeUnitR (u ./. v)

infixr 7 ~/.

-- | Same as @'(.^~)'@ but with priority to right most units
type (u :: Unit) ~^. (n :: ZZ) = NormalizeUnitR (u .^. n)

infix 8  ~^.


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
