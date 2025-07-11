module Data.Units.SI.Dimensions
  ( module Data.Units.SI.Dimensions
  ) where

import GHC.TypeLits

import Data.Units.Base

-- | The length dimension, denotated @L@ in SI.
--
--  This may contain a length quantity with unspecified unit.
--
newtype Length a = Length a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)

type instance DimId Length = 300
type instance ShowDim Length = Text "L"


-- | The time dimension, denotated @T@ in SI.
--
--  This may contain a length quantity with unspecified unit.
--
newtype Time a = Time a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)

type instance DimId Time = 400

type instance ShowDim Time = Text "T"


-- | The thermodynamic temperature dimension, denotated @Θ@ in SI.
--
--  This may contain a temperature quantity with unspecified unit.
--
newtype Temperature a = Temperature a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)

type instance DimId Temperature = 500
type instance ShowDim Temperature = Text "Θ"
